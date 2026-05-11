use crate::android::zip::ApkFile;
use crate::dex::DexFile;
use crate::dex::builder::{
    DEX_REF_LIMIT, build_dex_file_bytes, build_multi_dex_bytes, build_multi_dex_bytes_with_limit,
};
use crate::types::SmaliClass;

fn make_simple_class(name: &str, super_class: &str) -> SmaliClass {
    let s = format!(
        ".class public L{};\n.super L{};\n\n.method public n()V\n    .registers 1\n    return-void\n.end method\n",
        name, super_class
    );
    SmaliClass::from_smali(&s).expect("parse generated class")
}

#[test]
fn small_input_produces_single_dex() {
    let classes: Vec<SmaliClass> = (0..3)
        .map(|i| make_simple_class(&format!("com/example/A{i}"), "java/lang/Object"))
        .collect();
    let dexes = build_multi_dex_bytes(&classes).expect("build multi-dex");
    assert_eq!(dexes.len(), 1, "small input should fit in one DEX");
    // Verify the produced DEX parses cleanly and contains all classes.
    let parsed = DexFile::from_bytes(&dexes[0]).expect("parse single dex");
    assert_eq!(parsed.class_defs.len(), 3);
}

#[test]
fn overflow_splits_across_multiple_dexes() {
    // With a moderately low artificial limit we can force the splitter to use
    // multiple DEXes without having to synthesize 65K+ classes. Each class
    // alone fits well under the limit, but several classes together don't.
    let classes: Vec<SmaliClass> = (0..8)
        .map(|i| make_simple_class(&format!("com/example/Big{i}"), "java/lang/Object"))
        .collect();
    let dexes = build_multi_dex_bytes_with_limit(&classes, 5).expect("build multi-dex");
    assert!(
        dexes.len() >= 2,
        "expected splitting with low limit, got {} dex(es)",
        dexes.len()
    );

    // Every produced DEX must parse cleanly and have at least one class.
    let mut total_classes = 0;
    for (i, dex_bytes) in dexes.iter().enumerate() {
        let parsed = DexFile::from_bytes(dex_bytes)
            .unwrap_or_else(|e| panic!("dex #{i} failed to parse: {e}"));
        assert!(
            !parsed.class_defs.is_empty(),
            "dex #{i} has no class_defs"
        );
        total_classes += parsed.class_defs.len();
    }
    assert_eq!(
        total_classes,
        classes.len(),
        "all input classes should appear across the produced DEXes"
    );
}

#[test]
fn family_stays_together_when_capacity_allows() {
    // Three-deep inheritance chain; with a generous limit the splitter should
    // keep them together so the loader doesn't have to cross DEX boundaries
    // for verification.
    let chain = vec![
        make_simple_class("com/example/Base", "java/lang/Object"),
        make_simple_class("com/example/Mid", "com/example/Base"),
        make_simple_class("com/example/Leaf", "com/example/Mid"),
    ];
    let dexes = build_multi_dex_bytes(&chain).expect("build multi-dex");
    assert_eq!(dexes.len(), 1, "small chain should fit in one DEX");

    let parsed = DexFile::from_bytes(&dexes[0]).expect("parse");
    let class_descriptors: Vec<String> = parsed
        .to_smali()
        .expect("decode")
        .iter()
        .map(|c| c.name.as_jni_type())
        .collect();
    for needle in [
        "Lcom/example/Base;",
        "Lcom/example/Mid;",
        "Lcom/example/Leaf;",
    ] {
        assert!(
            class_descriptors.iter().any(|d| d == needle),
            "{needle} missing"
        );
    }
}

#[test]
fn family_that_overflows_alone_errors_cleanly() {
    // Two classes united by inheritance; even a fresh DEX with limit=1 cannot
    // hold them, so we expect an error rather than a corrupted split.
    let classes = vec![
        make_simple_class("com/example/Root", "java/lang/Object"),
        make_simple_class("com/example/Child", "com/example/Root"),
    ];
    let err = build_multi_dex_bytes_with_limit(&classes, 1).expect_err("must error");
    let msg = err.to_string();
    assert!(
        msg.contains("exceeds") && msg.contains("inheritance"),
        "error message should mention oversize family: got {msg}"
    );
}

#[test]
fn empty_input_produces_one_empty_dex() {
    let dexes = build_multi_dex_bytes(&[]).expect("empty input ok");
    assert_eq!(dexes.len(), 1);
    // Parsing should succeed with zero class_defs.
    let parsed = DexFile::from_bytes(&dexes[0]).expect("parse empty dex");
    assert_eq!(parsed.class_defs.len(), 0);
}

#[test]
fn matches_single_dex_path_when_no_split_needed() {
    let classes: Vec<SmaliClass> = (0..3)
        .map(|i| make_simple_class(&format!("com/example/M{i}"), "java/lang/Object"))
        .collect();
    let single = build_dex_file_bytes(&classes).expect("single dex");
    let multi = build_multi_dex_bytes(&classes).expect("multi dex");
    assert_eq!(multi.len(), 1);
    // Bytes should be identical: the multi-dex path collapses to the same code
    // as the single-dex path when only one DEX is needed.
    assert_eq!(
        multi[0], single,
        "multi-dex should match single-dex bytes when one DEX suffices"
    );
}

#[test]
fn dex_ref_limit_is_64k() {
    // Sanity guard: the spec-defined limit must stay at 65,536 (the maximum
    // number of distinct refs addressable by a 16-bit BBBB index).
    assert_eq!(DEX_REF_LIMIT, 65_536);
}

#[test]
fn apk_set_dex_files_writes_classes_dex_series() {
    let mut apk = ApkFile::new();
    // Pre-existing dex entries that should be cleared.
    apk.replace_entry("classes.dex", b"old".to_vec()).unwrap();
    apk.replace_entry("classes2.dex", b"old2".to_vec()).unwrap();
    apk.replace_entry("classes3.dex", b"old3".to_vec()).unwrap();
    // A non-DEX entry that must survive untouched.
    apk.replace_entry("AndroidManifest.xml", b"<manifest/>".to_vec())
        .unwrap();
    // A nested file with "classes.dex" in the path that must not be touched.
    apk.replace_entry("res/raw/classes.dex", b"keepme".to_vec())
        .unwrap();

    apk.set_dex_files(vec![b"d1".to_vec(), b"d2".to_vec()])
        .expect("set dex files");

    let names: Vec<&str> = apk.entry_names().collect();
    assert!(names.contains(&"classes.dex"));
    assert!(names.contains(&"classes2.dex"));
    assert!(
        !names.contains(&"classes3.dex"),
        "stale classes3.dex should have been removed"
    );
    assert!(names.contains(&"AndroidManifest.xml"));
    assert!(
        names.contains(&"res/raw/classes.dex"),
        "non-top-level dex paths must not be removed"
    );

    // Contents reflect the new bytes.
    assert_eq!(apk.entry("classes.dex").unwrap().data.as_slice(), b"d1");
    assert_eq!(apk.entry("classes2.dex").unwrap().data.as_slice(), b"d2");
    // The nested entry was untouched.
    assert_eq!(
        apk.entry("res/raw/classes.dex").unwrap().data.as_slice(),
        b"keepme"
    );
}
