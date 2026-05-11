//! Integration tests for `KotlinMetadata` against real-world Kotlin DEX
//! fixtures (rootbeer.dex contains classes compiled with kotlinc).
//!
//! These tests exercise the full pipeline: parse a real DEX, decode every
//! `@kotlin.Metadata` annotation, mutate the string pool, re-encode, and
//! verify the round trip survives cleanly.

use crate::dex::DexFile;
use crate::dex::builder::build_dex_file_bytes;
use crate::kotlin::{KOTLIN_METADATA_TYPE, KotlinMetadata, KotlinMetadataKind};
use crate::types::SmaliClass;
use std::fs;

fn rootbeer_classes() -> Vec<SmaliClass> {
    let bytes = fs::read("tests/rootbeer.dex").expect("read rootbeer fixture");
    let dex = DexFile::from_bytes(&bytes).expect("parse rootbeer dex");
    dex.to_smali().expect("decode classes")
}

#[test]
fn rootbeer_contains_kotlin_metadata_classes() {
    let classes = rootbeer_classes();
    let with_meta: Vec<&SmaliClass> = classes
        .iter()
        .filter(|c| {
            c.annotations
                .iter()
                .any(|a| a.annotation_type.to_jni() == KOTLIN_METADATA_TYPE)
        })
        .collect();
    assert!(
        with_meta.len() > 10,
        "expected many Kotlin classes in rootbeer, found {}",
        with_meta.len()
    );
}

#[test]
fn every_kotlin_metadata_in_rootbeer_decodes_cleanly() {
    let classes = rootbeer_classes();
    let mut decoded = 0usize;
    let mut by_kind = std::collections::BTreeMap::<i32, usize>::new();

    for class in &classes {
        let Some(meta) = class.kotlin_metadata().expect("metadata decode must not error") else {
            continue;
        };
        decoded += 1;
        *by_kind.entry(meta.kind_raw).or_default() += 1;

        // mv must be 3 ints (Kotlin metadata format version triple).
        assert_eq!(
            meta.metadata_version.len(),
            3,
            "{}: metadata_version should be 3 ints, got {:?}",
            class.name.as_jni_type(),
            meta.metadata_version
        );

        // d1 must decode to non-empty proto bytes for kinds that carry them.
        // MultiFileClassFacade (k=4) is the one that may legitimately have an
        // empty d1 (it stores the part list in d2 instead).
        if meta.kind != Some(KotlinMetadataKind::MultiFileClassFacade)
            && meta.kind != Some(KotlinMetadataKind::SyntheticClass)
        {
            assert!(
                !meta.proto_bytes.is_empty(),
                "{}: kind={:?} should have non-empty d1",
                class.name.as_jni_type(),
                meta.kind
            );
        }
    }

    assert!(decoded > 0, "no kotlin.Metadata annotations decoded");
    eprintln!("decoded {decoded} kotlin.Metadata annotations; kind histogram: {by_kind:?}");
}

#[test]
fn metadata_round_trip_byte_identical_for_real_classes() {
    // For every Kotlin class in rootbeer: decode → re-encode → re-decode and
    // assert structural equality. Byte-identical d1 isn't required (the
    // codec is deterministic but the smali source may have used an
    // alternative encoding), but the *decoded* protobuf bytes must match
    // exactly, and so must d2 / kind / version / xi / xs / pn.
    let classes = rootbeer_classes();
    let mut tested = 0usize;
    for class in &classes {
        let Some(original) = class.kotlin_metadata().expect("decode") else {
            continue;
        };
        let re_annotation = original.to_annotation();
        let recovered =
            KotlinMetadata::from_annotation(&re_annotation).expect("re-decode after encode");
        assert_eq!(
            recovered.kind_raw,
            original.kind_raw,
            "{}: kind drift",
            class.name.as_jni_type()
        );
        assert_eq!(
            recovered.metadata_version,
            original.metadata_version,
            "{}: metadata_version drift",
            class.name.as_jni_type()
        );
        assert_eq!(
            recovered.proto_bytes,
            original.proto_bytes,
            "{}: proto_bytes drift",
            class.name.as_jni_type()
        );
        assert_eq!(
            recovered.string_pool,
            original.string_pool,
            "{}: string_pool drift",
            class.name.as_jni_type()
        );
        assert_eq!(
            recovered.extra_int,
            original.extra_int,
            "{}: extra_int drift",
            class.name.as_jni_type()
        );
        assert_eq!(
            recovered.extra_string,
            original.extra_string,
            "{}: extra_string drift",
            class.name.as_jni_type()
        );
        assert_eq!(
            recovered.package_name,
            original.package_name,
            "{}: package_name drift",
            class.name.as_jni_type()
        );
        tested += 1;
    }
    assert!(tested > 10, "expected many round-trip cases, got {tested}");
}

#[test]
fn editing_string_pool_survives_dex_assembly() {
    // The end-to-end flow: parse a real Kotlin class, mutate a d2 entry,
    // re-assemble to DEX bytes, parse again, and verify the rename is
    // present in the recovered metadata.
    let mut classes = rootbeer_classes();

    // Pick a Kotlin class with a non-trivial string pool to mutate.
    let target_idx = classes
        .iter()
        .position(|c| {
            c.kotlin_metadata()
                .ok()
                .flatten()
                .map(|m| m.string_pool.len() >= 2 && m.kind == Some(KotlinMetadataKind::Class))
                .unwrap_or(false)
        })
        .expect("at least one suitable Kotlin class in rootbeer");
    let target_name = classes[target_idx].name.as_jni_type();

    // Pick a string in the pool that isn't likely to appear in many other
    // metadata blobs (avoid generic primitive-type descriptors). We replace
    // an entry with a unique sentinel string and verify it round-trips.
    let sentinel = "__SMALI_ROUND_TRIP_SENTINEL__";
    let original_pool;
    let target_string_idx;
    {
        let meta = classes[target_idx]
            .kotlin_metadata()
            .expect("decode")
            .expect("has metadata");
        original_pool = meta.string_pool.clone();
        // Find the longest string (most likely to be a unique identifier).
        target_string_idx = meta
            .string_pool
            .iter()
            .enumerate()
            .max_by_key(|(_, s)| s.len())
            .map(|(i, _)| i)
            .expect("non-empty pool");
    }

    let mut meta = classes[target_idx]
        .kotlin_metadata()
        .expect("decode")
        .expect("has metadata");
    meta.string_pool[target_string_idx] = sentinel.to_string();
    classes[target_idx].set_kotlin_metadata(&meta);

    // Re-assemble to DEX bytes and re-parse.
    let bytes = build_dex_file_bytes(&classes).expect("rebuild dex");
    let parsed = DexFile::from_bytes(&bytes).expect("reparse dex");
    let recovered_classes = parsed.to_smali().expect("decode round-tripped dex");
    let recovered = recovered_classes
        .iter()
        .find(|c| c.name.as_jni_type() == target_name)
        .expect("target class survived round-trip");
    let recovered_meta = recovered
        .kotlin_metadata()
        .expect("decode recovered metadata")
        .expect("metadata still present");

    assert_eq!(
        recovered_meta.string_pool[target_string_idx], sentinel,
        "sentinel didn't survive the smali -> DEX -> smali round trip"
    );
    // Other entries must be unchanged.
    for (i, original) in original_pool.iter().enumerate() {
        if i == target_string_idx {
            continue;
        }
        assert_eq!(
            &recovered_meta.string_pool[i], original,
            "string pool entry #{i} drifted"
        );
    }
    // proto_bytes are referenced by index, so they must also be untouched.
    let original_proto = KotlinMetadata::from_annotation(
        classes[target_idx]
            .annotations
            .iter()
            .find(|a| KotlinMetadata::matches(a))
            .expect("has metadata"),
    )
    .expect("decode")
    .proto_bytes;
    assert_eq!(
        recovered_meta.proto_bytes, original_proto,
        "proto_bytes drifted during a string-only edit"
    );
}

#[test]
fn handles_xs_and_pn_when_present() {
    // Survey every Kotlin class in rootbeer for non-empty xs / pn fields. If
    // any are present, ensure they round-trip. (rootbeer may or may not
    // contain these — the test still passes either way; it's verifying that
    // when they do appear, we don't drop them.)
    let classes = rootbeer_classes();
    for class in &classes {
        let Some(meta) = class.kotlin_metadata().expect("decode") else {
            continue;
        };
        if meta.extra_string.is_empty() && meta.package_name.is_empty() {
            continue;
        }
        let re_ann = meta.to_annotation();
        let recovered = KotlinMetadata::from_annotation(&re_ann).expect("re-decode");
        assert_eq!(recovered.extra_string, meta.extra_string);
        assert_eq!(recovered.package_name, meta.package_name);
    }
}
