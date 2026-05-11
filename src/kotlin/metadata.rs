//! Decoder/encoder for the `@kotlin.Metadata` annotation.
//!
//! The annotation has the JVM shape (per `kotlin/Metadata.kt`):
//! ```text
//! @Retention(RUNTIME)
//! @Target(TYPE)
//! public @interface Metadata {
//!   int    k()  default 1;          // class kind (1..5)
//!   int[]  mv() default {};         // metadata format version
//!   String[] d1() default {};       // bit-packed protobuf payload
//!   String[] d2() default {};       // plain string pool referenced from d1
//!   String  xs() default "";        // multi-file class part: facade name
//!   String  pn() default "";        // Kotlin package FQN if it overrides JVM
//!   int    xi() default 0;          // bit flags (pre-release, IR, K2, ...)
//! }
//! ```
//! Plus a deprecated `bv` field which the loader ignores; we preserve it
//! verbatim if present so writes are byte-identical with reads.
//!
//! This module exposes the fields as a plain mutable Rust struct. It does
//! *not* provide any rename helpers or other policy — callers manipulate the
//! string pool (`d2`) and the raw protobuf byte stream (`d1`) themselves.

use crate::dex::error::DexError;
use crate::kotlin::bit_encoding::{decode_d1, encode_d1};
use crate::types::{
    AnnotationElement, AnnotationValue, AnnotationVisibility, ObjectIdentifier, SmaliAnnotation,
    SmaliClass, TypeSignature,
};

/// JNI descriptor of the annotation type we recognise.
pub const KOTLIN_METADATA_TYPE: &str = "Lkotlin/Metadata;";

/// Class kind values defined by `@kotlin.Metadata.k`.
///
/// Numeric values are part of the on-disk format and must not be reordered.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum KotlinMetadataKind {
    /// A regular Kotlin class (or interface, object, enum, annotation).
    Class = 1,
    /// Top-level declarations from a single `.kt` file ("file facade").
    FileFacade = 2,
    /// Compiler-generated synthetic class (lambda body, `$DefaultImpls`,
    /// `$WhenMappings`, callable reference implementation).
    SyntheticClass = 3,
    /// Multi-file class facade: a `@JvmMultifileClass`-annotated façade that
    /// lists the part class names but holds no declarations of its own.
    MultiFileClassFacade = 4,
    /// One part of a multi-file class (declarations live here).
    MultiFileClassPart = 5,
}

impl KotlinMetadataKind {
    pub fn from_int(v: i32) -> Option<Self> {
        match v {
            1 => Some(Self::Class),
            2 => Some(Self::FileFacade),
            3 => Some(Self::SyntheticClass),
            4 => Some(Self::MultiFileClassFacade),
            5 => Some(Self::MultiFileClassPart),
            _ => None,
        }
    }

    pub fn as_int(self) -> i32 {
        self as i32
    }
}

/// Decoded view of an `@kotlin.Metadata` annotation.
///
/// Mutate the fields directly to rewrite the metadata. The renameable
/// strings live in `string_pool` (the annotation's `d2`); the protobuf bytes
/// in `proto_bytes` (the decoded `d1`) reference those strings by index, so
/// substituting an entry of `string_pool` is enough to "rename" a Kotlin
/// declaration in the vast majority of cases.
#[derive(Debug, Clone)]
pub struct KotlinMetadata {
    /// `k`: which of the five class kinds this is. `None` if the value
    /// wasn't one of the documented kinds (preserved as `extra_int_kind`).
    pub kind: Option<KotlinMetadataKind>,
    /// `k`: raw integer value, preserved even when `kind` is `None`.
    pub kind_raw: i32,
    /// `mv`: metadata format version triple (e.g. `[1, 9, 0]`). Kotlin
    /// reflection refuses metadata whose version is newer than its own, so
    /// rewriters must not bump this.
    pub metadata_version: Vec<i32>,
    /// `d1`: the bit-unpacked protobuf payload. Treat as opaque bytes unless
    /// you are decoding the protobuf yourself.
    pub proto_bytes: Vec<u8>,
    /// `d2`: the plain-text string pool referenced from `proto_bytes`.
    /// Editing entries here is the standard way to rename Kotlin-level
    /// declarations.
    pub string_pool: Vec<String>,
    /// `xi`: bit flags (pre-release, IR, K2 frontend, etc.). Preserve
    /// verbatim unless you know what you're doing.
    pub extra_int: i32,
    /// `xs`: only set for multi-file class parts (`kind = MultiFileClassPart`),
    /// where it's the JVM internal name of the corresponding facade class.
    pub extra_string: String,
    /// `pn`: Kotlin package name when `@JvmPackageName` makes it differ from
    /// the JVM package. Empty otherwise.
    pub package_name: String,
    /// `bv`: deprecated bytecode-version field. Preserved if present so that
    /// callers can produce byte-identical output; ignored by the runtime.
    pub bytecode_version: Option<Vec<i32>>,
}

impl KotlinMetadata {
    /// Returns `true` if `annotation`'s type is `Lkotlin/Metadata;`.
    pub fn matches(annotation: &SmaliAnnotation) -> bool {
        annotation.annotation_type.to_jni() == KOTLIN_METADATA_TYPE
    }

    /// Decode a smali annotation into the structured `KotlinMetadata` view.
    ///
    /// Returns an error if the annotation's type isn't `Lkotlin/Metadata;`
    /// or if a required element is missing or has the wrong shape.
    pub fn from_annotation(annotation: &SmaliAnnotation) -> Result<Self, DexError> {
        if !Self::matches(annotation) {
            return Err(DexError::new(&format!(
                "annotation is {}, not {}",
                annotation.annotation_type.to_jni(),
                KOTLIN_METADATA_TYPE
            )));
        }

        let mut kind_raw: i32 = 1;
        let mut metadata_version: Vec<i32> = Vec::new();
        let mut d1: Vec<String> = Vec::new();
        let mut d2: Vec<String> = Vec::new();
        let mut extra_int: i32 = 0;
        let mut extra_string = String::new();
        let mut package_name = String::new();
        let mut bytecode_version: Option<Vec<i32>> = None;

        for element in &annotation.elements {
            match element.name.as_str() {
                "k" => kind_raw = parse_int(&element.value, "k")?,
                "mv" => metadata_version = parse_int_array(&element.value, "mv")?,
                "d1" => d1 = parse_string_array(&element.value, "d1")?,
                "d2" => d2 = parse_string_array(&element.value, "d2")?,
                "xi" => extra_int = parse_int(&element.value, "xi")?,
                "xs" => extra_string = parse_string(&element.value, "xs")?,
                "pn" => package_name = parse_string(&element.value, "pn")?,
                "bv" => bytecode_version = Some(parse_int_array(&element.value, "bv")?),
                other => {
                    return Err(DexError::new(&format!(
                        "unexpected element '{other}' in @kotlin.Metadata"
                    )));
                }
            }
        }

        Ok(KotlinMetadata {
            kind: KotlinMetadataKind::from_int(kind_raw),
            kind_raw,
            metadata_version,
            proto_bytes: decode_d1(&d1),
            string_pool: d2,
            extra_int,
            extra_string,
            package_name,
            bytecode_version,
        })
    }

    /// Re-encode this metadata back into a `SmaliAnnotation`.
    ///
    /// Element order matches what `kotlinc` emits (`k, mv, d1, d2, xs, pn,
    /// xi`), with `bv` placed last when present, so well-formed inputs
    /// round-trip without reordering.
    pub fn to_annotation(&self) -> SmaliAnnotation {
        let mut elements: Vec<AnnotationElement> = Vec::with_capacity(8);
        elements.push(AnnotationElement {
            name: "k".to_string(),
            value: AnnotationValue::Single(format_int(self.kind_raw)),
        });
        elements.push(AnnotationElement {
            name: "mv".to_string(),
            value: AnnotationValue::Array(
                self.metadata_version.iter().map(|v| format_int(*v)).collect(),
            ),
        });
        let d1 = encode_d1(&self.proto_bytes);
        elements.push(AnnotationElement {
            name: "d1".to_string(),
            value: AnnotationValue::Array(d1.iter().map(|s| quote_string(s)).collect()),
        });
        elements.push(AnnotationElement {
            name: "d2".to_string(),
            value: AnnotationValue::Array(
                self.string_pool.iter().map(|s| quote_string(s)).collect(),
            ),
        });
        if !self.extra_string.is_empty() {
            elements.push(AnnotationElement {
                name: "xs".to_string(),
                value: AnnotationValue::Single(quote_string(&self.extra_string)),
            });
        }
        if !self.package_name.is_empty() {
            elements.push(AnnotationElement {
                name: "pn".to_string(),
                value: AnnotationValue::Single(quote_string(&self.package_name)),
            });
        }
        if self.extra_int != 0 {
            elements.push(AnnotationElement {
                name: "xi".to_string(),
                value: AnnotationValue::Single(format_int(self.extra_int)),
            });
        }
        if let Some(bv) = &self.bytecode_version {
            elements.push(AnnotationElement {
                name: "bv".to_string(),
                value: AnnotationValue::Array(bv.iter().map(|v| format_int(*v)).collect()),
            });
        }

        SmaliAnnotation {
            // kotlin.Metadata is itself a runtime annotation.
            visibility: AnnotationVisibility::Runtime,
            annotation_type: TypeSignature::Object(ObjectIdentifier::from_jni_type(
                KOTLIN_METADATA_TYPE,
            )),
            elements,
        }
    }
}

impl SmaliClass {
    /// If this class carries a `@kotlin.Metadata` annotation, decode and
    /// return it. Returns `None` if the annotation isn't present, and an
    /// error if the annotation is present but malformed.
    pub fn kotlin_metadata(&self) -> Result<Option<KotlinMetadata>, DexError> {
        for ann in &self.annotations {
            if KotlinMetadata::matches(ann) {
                return Ok(Some(KotlinMetadata::from_annotation(ann)?));
            }
        }
        Ok(None)
    }

    /// Replace (or insert) the class's `@kotlin.Metadata` annotation with
    /// the supplied value. Existing kotlin metadata annotations are removed
    /// first, then the supplied one appended.
    pub fn set_kotlin_metadata(&mut self, metadata: &KotlinMetadata) {
        self.annotations.retain(|a| !KotlinMetadata::matches(a));
        self.annotations.push(metadata.to_annotation());
    }
}

// ---- AnnotationValue parse helpers ----------------------------------------

fn parse_int(value: &AnnotationValue, name: &str) -> Result<i32, DexError> {
    match value {
        AnnotationValue::Single(s) => parse_int_literal(s).ok_or_else(|| {
            DexError::new(&format!("@kotlin.Metadata element '{name}' is not an int: {s}"))
        }),
        _ => Err(DexError::new(&format!(
            "@kotlin.Metadata element '{name}' must be a single int"
        ))),
    }
}

fn parse_int_array(value: &AnnotationValue, name: &str) -> Result<Vec<i32>, DexError> {
    match value {
        AnnotationValue::Array(items) => items
            .iter()
            .map(|s| {
                parse_int_literal(s).ok_or_else(|| {
                    DexError::new(&format!(
                        "@kotlin.Metadata element '{name}' contains non-int '{s}'"
                    ))
                })
            })
            .collect(),
        _ => Err(DexError::new(&format!(
            "@kotlin.Metadata element '{name}' must be an int array"
        ))),
    }
}

fn parse_string_array(value: &AnnotationValue, name: &str) -> Result<Vec<String>, DexError> {
    match value {
        AnnotationValue::Array(items) => items.iter().map(|s| Ok(unquote_string(s))).collect(),
        _ => Err(DexError::new(&format!(
            "@kotlin.Metadata element '{name}' must be a string array"
        ))),
    }
}

fn parse_string(value: &AnnotationValue, name: &str) -> Result<String, DexError> {
    match value {
        AnnotationValue::Single(s) => Ok(unquote_string(s)),
        _ => Err(DexError::new(&format!(
            "@kotlin.Metadata element '{name}' must be a single string"
        ))),
    }
}

/// Parse a smali int literal (decimal or hex with `0x` prefix, optional sign).
fn parse_int_literal(s: &str) -> Option<i32> {
    let s = s.trim();
    let (sign, body) = if let Some(rest) = s.strip_prefix('-') {
        (-1i64, rest)
    } else if let Some(rest) = s.strip_prefix('+') {
        (1i64, rest)
    } else {
        (1i64, s)
    };
    let value: i64 = if let Some(hex) = body.strip_prefix("0x").or_else(|| body.strip_prefix("0X"))
    {
        i64::from_str_radix(hex, 16).ok()?
    } else {
        body.parse::<i64>().ok()?
    };
    let signed = sign * value;
    i32::try_from(signed).ok()
}

/// Strip surrounding `"` quotes and undo smali escapes if present. The smali
/// parser already does this for top-level annotation values, but a string
/// that came from `AnnotationValue::Single` may still carry the quotes if it
/// was passed through verbatim.
fn unquote_string(s: &str) -> String {
    let trimmed = s.trim();
    if trimmed.len() >= 2 && trimmed.starts_with('"') && trimmed.ends_with('"') {
        crate::smali_ops::unescape_smali_string(&trimmed[1..trimmed.len() - 1])
    } else {
        trimmed.to_string()
    }
}

/// Re-quote and escape a string for embedding in a smali annotation value.
fn quote_string(s: &str) -> String {
    let mut out = String::with_capacity(s.len() + 2);
    out.push('"');
    for ch in s.chars() {
        match ch {
            '\\' => out.push_str("\\\\"),
            '"' => out.push_str("\\\""),
            '\n' => out.push_str("\\n"),
            '\r' => out.push_str("\\r"),
            '\t' => out.push_str("\\t"),
            c if (c as u32) < 0x20 => out.push_str(&format!("\\u{:04x}", c as u32)),
            c if (0x7f..=0x9f).contains(&(c as u32)) => {
                out.push_str(&format!("\\u{:04x}", c as u32))
            }
            c => out.push(c),
        }
    }
    out.push('"');
    out
}

fn format_int(v: i32) -> String {
    // Match kotlinc's typical output: hex with `0x` prefix.
    if v < 0 {
        format!("-0x{:x}", -(v as i64))
    } else {
        format!("0x{:x}", v)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn make_test_annotation() -> SmaliAnnotation {
        // Minimal valid @kotlin.Metadata: kind=2 (file facade), version 1.5.1,
        // d1 with one byte (1), d2 with one string.
        SmaliAnnotation {
            visibility: AnnotationVisibility::Runtime,
            annotation_type: TypeSignature::Object(ObjectIdentifier::from_jni_type(
                KOTLIN_METADATA_TYPE,
            )),
            elements: vec![
                AnnotationElement {
                    name: "k".to_string(),
                    value: AnnotationValue::Single("0x2".to_string()),
                },
                AnnotationElement {
                    name: "mv".to_string(),
                    value: AnnotationValue::Array(vec![
                        "0x1".to_string(),
                        "0x5".to_string(),
                        "0x1".to_string(),
                    ]),
                },
                AnnotationElement {
                    name: "d1".to_string(),
                    // The smali parser would have already unescaped   → '\0'.
                    value: AnnotationValue::Array(vec!["\u{0000}\u{0001}".to_string()]),
                },
                AnnotationElement {
                    name: "d2".to_string(),
                    value: AnnotationValue::Array(vec!["foo".to_string(), "bar".to_string()]),
                },
                AnnotationElement {
                    name: "xi".to_string(),
                    value: AnnotationValue::Single("0x30".to_string()),
                },
            ],
        }
    }

    #[test]
    fn decodes_minimal_annotation() {
        let ann = make_test_annotation();
        let m = KotlinMetadata::from_annotation(&ann).expect("decode");
        assert_eq!(m.kind, Some(KotlinMetadataKind::FileFacade));
        assert_eq!(m.kind_raw, 2);
        assert_eq!(m.metadata_version, vec![1, 5, 1]);
        assert_eq!(m.string_pool, vec!["foo".to_string(), "bar".to_string()]);
        assert_eq!(m.extra_int, 0x30);
        assert_eq!(m.proto_bytes, vec![1u8]);
    }

    #[test]
    fn rejects_wrong_annotation_type() {
        let mut ann = make_test_annotation();
        ann.annotation_type =
            TypeSignature::Object(ObjectIdentifier::from_jni_type("Lcom/other/Annot;"));
        assert!(KotlinMetadata::from_annotation(&ann).is_err());
    }

    #[test]
    fn round_trip_preserves_payload() {
        let ann = make_test_annotation();
        let m = KotlinMetadata::from_annotation(&ann).expect("decode");
        let re_ann = m.to_annotation();
        let m2 = KotlinMetadata::from_annotation(&re_ann).expect("re-decode");
        assert_eq!(m.kind_raw, m2.kind_raw);
        assert_eq!(m.metadata_version, m2.metadata_version);
        assert_eq!(m.proto_bytes, m2.proto_bytes);
        assert_eq!(m.string_pool, m2.string_pool);
        assert_eq!(m.extra_int, m2.extra_int);
        assert_eq!(m.extra_string, m2.extra_string);
        assert_eq!(m.package_name, m2.package_name);
    }

    #[test]
    fn editing_string_pool_round_trips() {
        // The "rename use case": decode, mutate d2, re-encode, re-decode,
        // observe the change is visible.
        let ann = make_test_annotation();
        let mut m = KotlinMetadata::from_annotation(&ann).expect("decode");
        m.string_pool[0] = "renamed".to_string();
        let re_ann = m.to_annotation();
        let m2 = KotlinMetadata::from_annotation(&re_ann).expect("re-decode");
        assert_eq!(m2.string_pool, vec!["renamed".to_string(), "bar".to_string()]);
        // proto_bytes must not have changed (rename is a string-only edit).
        assert_eq!(m2.proto_bytes, m.proto_bytes);
    }

    #[test]
    fn smali_class_helper_round_trips() {
        let smali = r#"
.class public Lcom/example/K;
.super Ljava/lang/Object;

.annotation runtime Lkotlin/Metadata;
    k = 0x2
    mv = { 0x1, 0x5, 0x1 }
    d1 = {
        " "
    }
    d2 = {
        "alpha",
        "beta"
    }
    xi = 0x30
.end annotation
"#;
        let mut class = SmaliClass::from_smali(smali).expect("parse class");
        let meta = class
            .kotlin_metadata()
            .expect("decode")
            .expect("metadata present");
        assert_eq!(meta.kind, Some(KotlinMetadataKind::FileFacade));
        assert_eq!(meta.string_pool, vec!["alpha".to_string(), "beta".to_string()]);
        assert_eq!(meta.proto_bytes, vec![1u8]);

        // Mutate and write back.
        let mut meta = meta;
        meta.string_pool[0] = "renamed".to_string();
        class.set_kotlin_metadata(&meta);

        // Verify exactly one kotlin.Metadata remains and it carries the new
        // string in d2.
        let count = class
            .annotations
            .iter()
            .filter(|a| KotlinMetadata::matches(a))
            .count();
        assert_eq!(count, 1, "expected exactly one kotlin.Metadata annotation");
        let recovered = class.kotlin_metadata().expect("decode").expect("present");
        assert_eq!(recovered.string_pool[0], "renamed");
    }

    #[test]
    fn class_without_kotlin_metadata_returns_none() {
        let smali = r#"
.class public Lcom/example/Plain;
.super Ljava/lang/Object;
"#;
        let class = SmaliClass::from_smali(smali).expect("parse");
        assert!(class.kotlin_metadata().expect("ok").is_none());
    }
}
