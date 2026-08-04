//! Android App Bundle (`.aab`) support.
//!
//! An AAB is a zip like an APK, so [`ApkFile`](super::zip::ApkFile) reads and
//! writes one unchanged. What differs is the *encoding* of two things and the
//! *layout* of the entries:
//!
//! | | APK | AAB |
//! |---|---|---|
//! | Manifest encoding | binary XML (AXML) | protobuf (`aapt.pb.XmlNode`) |
//! | Manifest path | `AndroidManifest.xml` | `base/manifest/AndroidManifest.xml` |
//! | Resource table | `resources.arsc` | `base/resources.pb` |
//! | Code | `classes.dex` | `base/dex/classes.dex` |
//! | Native libs | `lib/<abi>/` | `base/lib/<abi>/` |
//! | Loose files | root of the zip | `base/root/` |
//!
//! This module provides the protobuf manifest codec, helpers for translating
//! between the two layouts, and encoders for the small targeting protos that
//! bundletool expects (`native.pb`, `assets.pb`).
//!
//! The manifest decodes into the same [`AndroidManifest`] DOM used for APKs, so
//! code that inspects or edits a manifest does not care which container it came
//! from — and a manifest read from one can be written to the other.
//!
//! ```no_run
//! use smali::android::aab;
//! use smali::android::zip::ApkFile;
//!
//! # fn main() -> Result<(), Box<dyn std::error::Error>> {
//! let mut bundle = ApkFile::from_file("app.aab")?;
//! let raw = &bundle.entry(aab::MANIFEST_PATH).unwrap().data;
//!
//! let mut manifest = aab::decode_manifest(raw)?;
//! manifest.set_version_name("2.0");
//!
//! bundle.replace_entry(aab::MANIFEST_PATH, aab::encode_manifest(&manifest)?)?;
//! bundle.write_to_file("out.aab")?;
//! # Ok(())
//! # }
//! ```
//!
//! `resources.pb` is left as opaque bytes: the `ResourceTable` schema is far
//! larger than the manifest's and repackaging a bundle does not require
//! decoding it.

use super::binary_xml::{
    AndroidManifest, BinaryXmlError, BinaryXmlResult, ManifestAttribute, ManifestElement,
    ManifestValue,
};
use super::pb::{PbReader, PbWriter, WIRE_LEN};

/// Path of the manifest inside a bundle's base module.
pub const MANIFEST_PATH: &str = "base/manifest/AndroidManifest.xml";
/// Path of the protobuf resource table inside a bundle's base module.
pub const RESOURCES_PATH: &str = "base/resources.pb";
/// Path of the bundle-wide configuration proto.
pub const BUNDLE_CONFIG_PATH: &str = "BundleConfig.pb";

// ---------------------------------------------------------------------------
// Field numbers from AOSP `frameworks/base/tools/aapt2/Resources.proto`.
// Named rather than inlined so the wire format is auditable against the schema.
// ---------------------------------------------------------------------------

// message XmlNode { oneof node { XmlElement element = 1; string text = 2; } }
const NODE_ELEMENT: u32 = 1;
const NODE_TEXT: u32 = 2;
const NODE_SOURCE: u32 = 3;

// message XmlElement {
//   repeated XmlNamespace namespace_declaration = 1;
//   string namespace_uri = 2;
//   string name = 3;
//   repeated XmlAttribute attribute = 4;
//   repeated XmlNode child = 5;
// }
const ELEMENT_NAMESPACE_DECL: u32 = 1;
const ELEMENT_NAMESPACE_URI: u32 = 2;
const ELEMENT_NAME: u32 = 3;
const ELEMENT_ATTRIBUTE: u32 = 4;
const ELEMENT_CHILD: u32 = 5;

// message XmlNamespace { string prefix = 1; string uri = 2; }
const NAMESPACE_PREFIX: u32 = 1;
const NAMESPACE_URI: u32 = 2;

// message XmlAttribute {
//   string namespace_uri = 1;
//   string name = 2;
//   string value = 3;
//   SourcePosition source = 4;
//   uint32 resource_id = 5;
//   Item compiled_item = 6;
// }
const ATTR_NAMESPACE_URI: u32 = 1;
const ATTR_NAME: u32 = 2;
const ATTR_VALUE: u32 = 3;
const ATTR_SOURCE: u32 = 4;
const ATTR_RESOURCE_ID: u32 = 5;
const ATTR_COMPILED_ITEM: u32 = 6;

// message Item {
//   oneof value { Reference ref = 1; String str = 2; RawString raw_str = 3;
//                 StyledString styled_str = 4; FileReference file = 5; Id id = 6;
//                 Primitive prim = 7; }
// }
const ITEM_REF: u32 = 1;
const ITEM_STR: u32 = 2;
const ITEM_PRIM: u32 = 7;

// message Reference { Type type = 1; uint32 id = 2; string name = 3; ... }
const REFERENCE_ID: u32 = 2;

// message String { string value = 1; }
const STRING_VALUE: u32 = 1;

// message Primitive {
//   oneof oneof_value { Empty null_value = 1; ... uint32 int_decimal_value = 6;
//                       uint32 int_hexadecimal_value = 7; bool boolean_value = 8; ... }
// }
const PRIM_INT_DECIMAL: u32 = 6;
const PRIM_INT_HEX: u32 = 7;
const PRIM_BOOLEAN: u32 = 8;

fn malformed(msg: impl Into<String>) -> BinaryXmlError {
    BinaryXmlError::MalformedDocument(msg.into())
}

// ---------------------------------------------------------------------------
// Manifest decoding
// ---------------------------------------------------------------------------

/// Decode a protobuf-encoded `AndroidManifest.xml` from an AAB.
///
/// The result is the same [`AndroidManifest`] type produced by
/// [`AndroidManifest::from_bytes`] for APKs, so downstream editing code is
/// shared between the two container formats.
pub fn decode_manifest(bytes: &[u8]) -> BinaryXmlResult<AndroidManifest> {
    let node = read_node(bytes, 0)?;
    match node {
        DecodedNode::Element(root) => Ok(AndroidManifest::from_root(root)),
        DecodedNode::Text(_) => Err(malformed(
            "bundle manifest root is a text node, expected an element",
        )),
        DecodedNode::Empty => Err(malformed("bundle manifest contains no root element")),
    }
}

enum DecodedNode {
    Element(ManifestElement),
    Text(String),
    Empty,
}

/// Bundles nest one `XmlNode` per element; a hostile file could nest without
/// bound, so recursion is capped well above anything a real manifest needs.
const MAX_DEPTH: usize = 256;

fn read_node(bytes: &[u8], depth: usize) -> BinaryXmlResult<DecodedNode> {
    if depth > MAX_DEPTH {
        return Err(malformed(format!(
            "bundle manifest nests deeper than {MAX_DEPTH} elements"
        )));
    }
    let mut reader = PbReader::new(bytes);
    let mut result = DecodedNode::Empty;
    let mut source: Option<Vec<u8>> = None;
    while !reader.is_empty() {
        let (field, wire) = reader.key()?;
        match (field, wire) {
            (NODE_ELEMENT, WIRE_LEN) => {
                let body = reader.bytes()?;
                result = DecodedNode::Element(read_element(body, depth)?);
            }
            (NODE_TEXT, WIRE_LEN) => {
                result = DecodedNode::Text(reader.string()?);
            }
            // Source positions are not modelled, but are kept verbatim so an
            // untouched manifest re-encodes byte-for-byte.
            (NODE_SOURCE, WIRE_LEN) => {
                source = Some(reader.bytes()?.to_vec());
            }
            _ => reader.skip(wire)?,
        }
    }
    if let (DecodedNode::Element(el), Some(src)) = (&mut result, source) {
        el.proto_source = Some(src);
    }
    Ok(result)
}

fn read_element(bytes: &[u8], depth: usize) -> BinaryXmlResult<ManifestElement> {
    let mut reader = PbReader::new(bytes);
    let mut element = ManifestElement::new(String::new());
    // Namespace declarations map prefix -> uri for this subtree. The DOM stores
    // the prefix on each node, so resolve as we go.
    let mut declarations: Vec<(String, String)> = Vec::new();
    let mut text = String::new();

    while !reader.is_empty() {
        let (field, wire) = reader.key()?;
        match (field, wire) {
            (ELEMENT_NAMESPACE_DECL, WIRE_LEN) => {
                let body = reader.bytes()?;
                declarations.push(read_namespace(body)?);
            }
            (ELEMENT_NAMESPACE_URI, WIRE_LEN) => {
                let uri = reader.string()?;
                if !uri.is_empty() {
                    element.namespace_uri = Some(uri);
                }
            }
            (ELEMENT_NAME, WIRE_LEN) => element.tag = reader.string()?,
            (ELEMENT_ATTRIBUTE, WIRE_LEN) => {
                let body = reader.bytes()?;
                element.attributes.push(read_attribute(body)?);
            }
            (ELEMENT_CHILD, WIRE_LEN) => {
                let body = reader.bytes()?;
                match read_node(body, depth + 1)? {
                    DecodedNode::Element(child) => element.children.push(child),
                    // aapt2 emits whitespace-only text nodes between elements;
                    // keep only meaningful text, matching the AXML reader.
                    DecodedNode::Text(t) => {
                        if !t.trim().is_empty() {
                            text.push_str(&t);
                        }
                    }
                    DecodedNode::Empty => {}
                }
            }
            _ => reader.skip(wire)?,
        }
    }

    if element.tag.is_empty() {
        return Err(malformed("bundle manifest element has no name"));
    }
    if !text.is_empty() {
        element.text = Some(text);
    }
    resolve_prefixes(&mut element, &declarations);
    Ok(element)
}

fn read_namespace(bytes: &[u8]) -> BinaryXmlResult<(String, String)> {
    let mut reader = PbReader::new(bytes);
    let (mut prefix, mut uri) = (String::new(), String::new());
    while !reader.is_empty() {
        let (field, wire) = reader.key()?;
        match (field, wire) {
            (NAMESPACE_PREFIX, WIRE_LEN) => prefix = reader.string()?,
            (NAMESPACE_URI, WIRE_LEN) => uri = reader.string()?,
            _ => reader.skip(wire)?,
        }
    }
    Ok((prefix, uri))
}

/// Attach the prefix matching each namespace URI, for this element and its
/// attributes. Declarations appear on the element that introduces them, which
/// for a manifest is almost always the root.
fn resolve_prefixes(element: &mut ManifestElement, declarations: &[(String, String)]) {
    if declarations.is_empty() {
        return;
    }
    let lookup = |uri: &Option<String>| -> Option<String> {
        let uri = uri.as_ref()?;
        declarations
            .iter()
            .find(|(_, u)| u == uri)
            .map(|(p, _)| p.clone())
    };
    if element.namespace_prefix.is_none() {
        element.namespace_prefix = lookup(&element.namespace_uri);
    }
    for attr in &mut element.attributes {
        if attr.namespace_prefix.is_none() {
            attr.namespace_prefix = lookup(&attr.namespace_uri);
        }
    }
    for child in &mut element.children {
        resolve_prefixes(child, declarations);
    }
}

fn read_attribute(bytes: &[u8]) -> BinaryXmlResult<ManifestAttribute> {
    let mut reader = PbReader::new(bytes);
    let mut attr = ManifestAttribute::new(String::new(), ManifestValue::String(String::new()));
    let mut raw_value = String::new();
    let mut compiled: Option<ManifestValue> = None;

    while !reader.is_empty() {
        let (field, wire) = reader.key()?;
        match (field, wire) {
            (ATTR_NAMESPACE_URI, WIRE_LEN) => {
                let uri = reader.string()?;
                if !uri.is_empty() {
                    attr.namespace_uri = Some(uri);
                }
            }
            (ATTR_NAME, WIRE_LEN) => attr.name = reader.string()?,
            (ATTR_VALUE, WIRE_LEN) => raw_value = reader.string()?,
            (ATTR_RESOURCE_ID, _) => {
                let id = reader.varint()? as u32;
                attr.resource_id = (id != 0).then_some(id);
            }
            (ATTR_SOURCE, WIRE_LEN) => {
                attr.proto_source = Some(reader.bytes()?.to_vec());
            }
            (ATTR_COMPILED_ITEM, WIRE_LEN) => {
                let body = reader.bytes()?;
                attr.proto_compiled_item = Some(body.to_vec());
                compiled = read_item(body)?;
            }
            _ => reader.skip(wire)?,
        }
    }

    if attr.name.is_empty() {
        return Err(malformed("bundle manifest attribute has no name"));
    }
    // A typed `compiled_item` is authoritative; `value` is the source text that
    // produced it and is kept so the original spelling survives a round trip.
    attr.value = compiled.unwrap_or(ManifestValue::String(raw_value.clone()));
    if !raw_value.is_empty() {
        attr.raw_value = Some(raw_value);
    }
    Ok(attr)
}

fn read_item(bytes: &[u8]) -> BinaryXmlResult<Option<ManifestValue>> {
    let mut reader = PbReader::new(bytes);
    let mut value = None;
    while !reader.is_empty() {
        let (field, wire) = reader.key()?;
        match (field, wire) {
            (ITEM_REF, WIRE_LEN) => {
                let body = reader.bytes()?;
                value = Some(ManifestValue::Reference(read_reference(body)?));
            }
            (ITEM_STR, WIRE_LEN) => {
                let body = reader.bytes()?;
                value = Some(ManifestValue::String(read_string_message(body)?));
            }
            (ITEM_PRIM, WIRE_LEN) => {
                let body = reader.bytes()?;
                value = read_primitive(body)?;
            }
            _ => reader.skip(wire)?,
        }
    }
    Ok(value)
}

fn read_reference(bytes: &[u8]) -> BinaryXmlResult<u32> {
    let mut reader = PbReader::new(bytes);
    let mut id = 0u32;
    while !reader.is_empty() {
        let (field, wire) = reader.key()?;
        match (field, wire) {
            (REFERENCE_ID, _) => id = reader.varint()? as u32,
            _ => reader.skip(wire)?,
        }
    }
    Ok(id)
}

fn read_string_message(bytes: &[u8]) -> BinaryXmlResult<String> {
    let mut reader = PbReader::new(bytes);
    let mut value = String::new();
    while !reader.is_empty() {
        let (field, wire) = reader.key()?;
        match (field, wire) {
            (STRING_VALUE, WIRE_LEN) => value = reader.string()?,
            _ => reader.skip(wire)?,
        }
    }
    Ok(value)
}

fn read_primitive(bytes: &[u8]) -> BinaryXmlResult<Option<ManifestValue>> {
    let mut reader = PbReader::new(bytes);
    let mut value = None;
    while !reader.is_empty() {
        let (field, wire) = reader.key()?;
        match field {
            PRIM_INT_DECIMAL => value = Some(ManifestValue::Integer(reader.varint()? as i64)),
            PRIM_INT_HEX => value = Some(ManifestValue::Hex(reader.varint()? as u32)),
            PRIM_BOOLEAN => value = Some(ManifestValue::Boolean(reader.varint()? != 0)),
            // Floats, dimensions, fractions and colours are not used by
            // manifest attributes; fall back to the source text for those.
            _ => reader.skip(wire)?,
        }
    }
    Ok(value)
}

// ---------------------------------------------------------------------------
// Manifest encoding
// ---------------------------------------------------------------------------

/// Encode an [`AndroidManifest`] as an AAB's protobuf `AndroidManifest.xml`.
///
/// Namespace declarations are emitted on the root element, which is where
/// aapt2 puts them and where Android's bundle tooling expects them.
pub fn encode_manifest(manifest: &AndroidManifest) -> BinaryXmlResult<Vec<u8>> {
    let mut declarations: Vec<(String, String)> = Vec::new();
    collect_declarations(manifest.root(), &mut declarations);

    let mut node = PbWriter::new();
    node.message_field(
        NODE_ELEMENT,
        write_element(manifest.root(), &declarations, true)?,
    );
    if let Some(src) = &manifest.root().proto_source {
        node.bytes_field(NODE_SOURCE, src);
    }
    Ok(node.into_bytes())
}

/// Gather every (prefix, uri) pair used anywhere in the tree, so all of them
/// can be declared once on the root.
fn collect_declarations(element: &ManifestElement, out: &mut Vec<(String, String)>) {
    let mut note = |prefix: &Option<String>, uri: &Option<String>| {
        if let (Some(p), Some(u)) = (prefix, uri)
            && !p.is_empty()
            && !u.is_empty()
            && !out.iter().any(|(_, existing)| existing == u)
        {
            out.push((p.clone(), u.clone()));
        }
    };
    note(&element.namespace_prefix, &element.namespace_uri);
    for attr in &element.attributes {
        note(&attr.namespace_prefix, &attr.namespace_uri);
    }
    for child in &element.children {
        collect_declarations(child, out);
    }
}

fn write_element(
    element: &ManifestElement,
    declarations: &[(String, String)],
    is_root: bool,
) -> BinaryXmlResult<PbWriter> {
    let mut w = PbWriter::new();
    if is_root {
        for (prefix, uri) in declarations {
            let mut ns = PbWriter::new();
            ns.string_field(NAMESPACE_PREFIX, prefix);
            ns.string_field(NAMESPACE_URI, uri);
            w.message_field(ELEMENT_NAMESPACE_DECL, ns);
        }
    }
    if let Some(uri) = &element.namespace_uri {
        w.string_field(ELEMENT_NAMESPACE_URI, uri);
    }
    w.string_field(ELEMENT_NAME, &element.tag);
    for attr in &element.attributes {
        w.message_field(ELEMENT_ATTRIBUTE, write_attribute(attr));
    }
    if let Some(text) = &element.text {
        let mut child = PbWriter::new();
        child.string_field(NODE_TEXT, text);
        w.message_field(ELEMENT_CHILD, child);
    }
    for child in &element.children {
        let mut node = PbWriter::new();
        node.message_field(NODE_ELEMENT, write_element(child, declarations, false)?);
        if let Some(src) = &child.proto_source {
            node.bytes_field(NODE_SOURCE, src);
        }
        w.message_field(ELEMENT_CHILD, node);
    }
    Ok(w)
}

/// Fields are emitted in ascending field-number order, matching aapt2, so an
/// unmodified attribute re-encodes to the same bytes it was read from.
fn write_attribute(attr: &ManifestAttribute) -> PbWriter {
    let mut w = PbWriter::new();
    if let Some(uri) = &attr.namespace_uri {
        w.string_field(ATTR_NAMESPACE_URI, uri);
    }
    w.string_field(ATTR_NAME, &attr.name);
    w.string_field(ATTR_VALUE, &attribute_source_text(attr));
    if let Some(src) = &attr.proto_source {
        w.bytes_field(ATTR_SOURCE, src);
    }
    if let Some(id) = attr.resource_id {
        w.varint_field(ATTR_RESOURCE_ID, id as u64);
    }
    // A preserved compiled_item is re-emitted untouched. Clearing it (after
    // changing `value`) makes the encoder rebuild one from the typed value;
    // aapt2 leaves plain strings uncompiled, so strings get none.
    match &attr.proto_compiled_item {
        Some(raw) => w.bytes_field(ATTR_COMPILED_ITEM, raw),
        None => {
            if let Some(item) = write_item(&attr.value) {
                w.message_field(ATTR_COMPILED_ITEM, item);
            }
        }
    }
    w
}

/// The `value` field holds the attribute as it appeared in source. Prefer the
/// preserved original where we have one so round trips are faithful.
fn attribute_source_text(attr: &ManifestAttribute) -> String {
    if let Some(raw) = &attr.raw_value {
        return raw.clone();
    }
    match &attr.value {
        ManifestValue::String(s) => s.clone(),
        ManifestValue::Boolean(b) => b.to_string(),
        ManifestValue::Integer(i) => i.to_string(),
        ManifestValue::Hex(h) => format!("0x{h:08x}"),
        ManifestValue::Reference(r) => format!("@0x{r:08x}"),
    }
}

/// Build the typed `compiled_item`. Plain strings are left uncompiled, which is
/// what aapt2 emits for attributes with no resource type.
fn write_item(value: &ManifestValue) -> Option<PbWriter> {
    let mut item = PbWriter::new();
    match value {
        // aapt2 leaves plain string attributes uncompiled — the `value` field
        // already carries the text, and emitting a redundant Item both bloats
        // the manifest and diverges from real bundle output.
        ManifestValue::String(_) => return None,
        ManifestValue::Reference(id) => {
            let mut msg = PbWriter::new();
            msg.varint_field(REFERENCE_ID, *id as u64);
            item.message_field(ITEM_REF, msg);
        }
        ManifestValue::Boolean(b) => {
            let mut msg = PbWriter::new();
            msg.varint_field(PRIM_BOOLEAN, u64::from(*b));
            item.message_field(ITEM_PRIM, msg);
        }
        ManifestValue::Integer(i) => {
            let mut msg = PbWriter::new();
            msg.varint_field(PRIM_INT_DECIMAL, *i as u64);
            item.message_field(ITEM_PRIM, msg);
        }
        ManifestValue::Hex(h) => {
            let mut msg = PbWriter::new();
            msg.varint_field(PRIM_INT_HEX, *h as u64);
            item.message_field(ITEM_PRIM, msg);
        }
    }
    Some(item)
}

// ---------------------------------------------------------------------------
// Container layout
// ---------------------------------------------------------------------------

/// Which container an archive is.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BundleFormat {
    /// A normal APK: manifest at the root, `classes.dex` at the root.
    Apk,
    /// An App Bundle: everything under module directories such as `base/`.
    Aab,
}

/// Identify the container from its entry names.
///
/// Detection keys on the bundle-only manifest path rather than on `base/`,
/// because an APK may legitimately contain a directory called `base/`.
pub fn detect_format<'a>(entries: impl IntoIterator<Item = &'a str>) -> BundleFormat {
    for name in entries {
        let name = name.trim_start_matches("./");
        if name == MANIFEST_PATH || name == BUNDLE_CONFIG_PATH || name == RESOURCES_PATH {
            return BundleFormat::Aab;
        }
    }
    BundleFormat::Apk
}

/// Module name for a bundle entry, e.g. `base` for `base/dex/classes.dex`.
///
/// Returns `None` for entries outside a module (`BUNDLE-METADATA/...`,
/// `BundleConfig.pb`, `META-INF/...`).
pub fn module_of(entry: &str) -> Option<&str> {
    let entry = entry.trim_start_matches("./");
    let (head, rest) = entry.split_once('/')?;
    if rest.is_empty() || head.is_empty() {
        return None;
    }
    match head {
        "BUNDLE-METADATA" | "META-INF" => None,
        module => Some(module),
    }
}

/// Translate a bundle entry to the path the same file would have in an APK.
///
/// `base/dex/classes.dex` becomes `classes.dex`, `base/root/foo.txt` becomes
/// `foo.txt`, `base/lib/arm64-v8a/libx.so` becomes `lib/arm64-v8a/libx.so`.
/// Entries with no APK equivalent — the targeting protos, bundle metadata —
/// return `None`.
pub fn to_apk_path(entry: &str) -> Option<String> {
    let entry = entry.trim_start_matches("./");
    let module = module_of(entry)?;
    let rest = &entry[module.len() + 1..];

    if rest == "resources.pb" || rest.ends_with(".pb") {
        return None;
    }
    if let Some(tail) = rest.strip_prefix("manifest/") {
        return Some(tail.to_string());
    }
    if let Some(tail) = rest.strip_prefix("dex/") {
        return Some(tail.to_string());
    }
    if let Some(tail) = rest.strip_prefix("root/") {
        return Some(tail.to_string());
    }
    // res/, lib/ and assets/ keep their directory in an APK.
    if rest.starts_with("res/") || rest.starts_with("lib/") || rest.starts_with("assets/") {
        return Some(rest.to_string());
    }
    None
}

/// Translate an APK path into the equivalent entry in a bundle module.
///
/// This is the inverse of [`to_apk_path`] for the paths that have an inverse.
pub fn to_bundle_path(module: &str, apk_path: &str) -> String {
    let path = apk_path.trim_start_matches("./");
    if path == "AndroidManifest.xml" {
        return format!("{module}/manifest/AndroidManifest.xml");
    }
    if is_dex_name(path) {
        return format!("{module}/dex/{path}");
    }
    if path.starts_with("res/") || path.starts_with("lib/") || path.starts_with("assets/") {
        return format!("{module}/{path}");
    }
    // Anything else sits loose at the root of an APK, which bundles keep
    // under `root/`.
    format!("{module}/root/{path}")
}

/// Is this the name of a top-level DEX (`classes.dex`, `classes2.dex`, ...)?
fn is_dex_name(name: &str) -> bool {
    super::zip::is_top_level_dex_name(name)
}

// ---------------------------------------------------------------------------
// Targeting protos
// ---------------------------------------------------------------------------

// message NativeLibraries { repeated TargetedNativeDirectory directory = 1; }
// message TargetedNativeDirectory { string path = 1; NativeDirectoryTargeting targeting = 2; }
// message NativeDirectoryTargeting { AbiTargeting abi = 1; ... }
// message AbiTargeting { repeated Abi value = 1; repeated Abi alternatives = 2; }
// message Abi { AbiAlias alias = 1; }
const NATIVE_DIRECTORY: u32 = 1;
const DIRECTORY_PATH: u32 = 1;
const DIRECTORY_TARGETING: u32 = 2;
const TARGETING_ABI: u32 = 1;
const ABI_VALUE: u32 = 1;
const ABI_ALIAS: u32 = 1;

/// `AbiAlias` enum values from `Targeting.proto`.
fn abi_alias(abi: &str) -> Option<u64> {
    Some(match abi {
        "armeabi" => 1,
        "armeabi-v7a" => 2,
        "arm64-v8a" => 3,
        "x86" => 4,
        "x86_64" => 5,
        "mips" => 6,
        "mips64" => 7,
        "riscv64" => 8,
        _ => return None,
    })
}

/// Build the `base/native.pb` targeting proto for a set of ABI directories.
///
/// `abis` are directory names as they appear under `lib/`, e.g. `arm64-v8a`.
/// Unrecognised names are skipped: bundletool rejects a directory whose
/// targeting names an alias it does not know.
pub fn encode_native_pb(abis: &[String]) -> Vec<u8> {
    let mut root = PbWriter::new();
    for abi in abis {
        let Some(alias) = abi_alias(abi) else { continue };

        let mut abi_msg = PbWriter::new();
        abi_msg.varint_field(ABI_ALIAS, alias);

        let mut abi_targeting = PbWriter::new();
        abi_targeting.message_field(ABI_VALUE, abi_msg);

        let mut targeting = PbWriter::new();
        targeting.message_field(TARGETING_ABI, abi_targeting);

        let mut dir = PbWriter::new();
        dir.string_field(DIRECTORY_PATH, &format!("lib/{abi}"));
        dir.message_field(DIRECTORY_TARGETING, targeting);

        root.message_field(NATIVE_DIRECTORY, dir);
    }
    root.into_bytes()
}

// message Assets { repeated TargetedAssetsDirectory directory = 1; }
// message TargetedAssetsDirectory { string path = 1; AssetsDirectoryTargeting targeting = 2; }
const ASSETS_DIRECTORY: u32 = 1;

/// Build the `base/assets.pb` targeting proto for a set of asset directories.
///
/// Directories are emitted with empty targeting, which means "applies to every
/// device" — the correct default for assets that are not split by density,
/// language or texture format.
pub fn encode_assets_pb(asset_dirs: &[String]) -> Vec<u8> {
    let mut root = PbWriter::new();
    for dir_path in asset_dirs {
        let mut dir = PbWriter::new();
        dir.string_field(DIRECTORY_PATH, dir_path);
        dir.message_field(DIRECTORY_TARGETING, PbWriter::new());
        root.message_field(ASSETS_DIRECTORY, dir);
    }
    root.into_bytes()
}

/// Collect the distinct ABI directory names under a module's `lib/`.
///
/// Order follows first appearance so output is deterministic for a given zip.
pub fn native_abis_from_entries<'a>(entries: impl IntoIterator<Item = &'a str>) -> Vec<String> {
    let mut out: Vec<String> = Vec::new();
    for name in entries {
        let name = name.trim_start_matches("./");
        let Some(module) = module_of(name) else {
            continue;
        };
        let rest = &name[module.len() + 1..];
        let Some(tail) = rest.strip_prefix("lib/") else {
            continue;
        };
        let Some((abi, file)) = tail.split_once('/') else {
            continue;
        };
        if file.is_empty() || abi.is_empty() {
            continue;
        }
        if !out.iter().any(|e| e == abi) {
            out.push(abi.to_string());
        }
    }
    out
}

/// Collect the distinct asset directories under a module's `assets/`.
pub fn asset_dirs_from_entries<'a>(entries: impl IntoIterator<Item = &'a str>) -> Vec<String> {
    let mut out: Vec<String> = Vec::new();
    for name in entries {
        let name = name.trim_start_matches("./");
        let Some(module) = module_of(name) else {
            continue;
        };
        let rest = &name[module.len() + 1..];
        if !rest.starts_with("assets/") {
            continue;
        }
        // The directory is everything up to the final path segment.
        let Some((dir, file)) = rest.rsplit_once('/') else {
            continue;
        };
        if file.is_empty() {
            continue;
        }
        if !out.iter().any(|e| e == dir) {
            out.push(dir.to_string());
        }
    }
    out
}
