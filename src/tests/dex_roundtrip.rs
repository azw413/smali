use std::collections::BTreeMap;
use std::fs;

use crate::dex::DexFile;
use crate::types::{CatchDirective, SmaliClass, SmaliOp};

fn class_smali_map(classes: &[SmaliClass]) -> BTreeMap<String, String> {
    let mut map = BTreeMap::new();
    for class in classes {
        let desc = class.name.as_jni_type();
        map.insert(desc, class.to_smali());
    }
    map
}

#[test]
fn rebuilds_rootbeer_dex_roundtrip() {
    let bytes = fs::read("tests/rootbeer.dex").expect("read rootbeer fixture");
    let dex = DexFile::from_bytes(&bytes).expect("parse rootbeer dex");
    let classes = dex.to_smali().expect("convert to smali classes");

    let rebuilt_dex =
        DexFile::from_smali(&classes).expect("rebuild dex from smali classes and parse result");
    let rebuilt_classes = rebuilt_dex
        .to_smali()
        .expect("convert rebuilt dex to smali classes");

    let original_map = class_smali_map(&classes);
    let rebuilt_map = class_smali_map(&rebuilt_classes);

    let mut sample = Vec::new();
    let mut diff_count = 0usize;
    for (desc, original) in &original_map {
        match rebuilt_map.get(desc) {
            Some(rebuilt) if rebuilt == original => {}
            Some(rebuilt) => {
                diff_count += 1;
                if sample.len() < 5 {
                    sample.push(format!(
                        "{desc} (original {} bytes vs rebuilt {} bytes)",
                        original.len(),
                        rebuilt.len()
                    ));
                }
            }
            None => {
                diff_count += 1;
                if sample.len() < 5 {
                    sample.push(format!("{desc} missing from rebuilt output"));
                }
            }
        }
    }
    for desc in rebuilt_map.keys() {
        if !original_map.contains_key(desc) {
            diff_count += 1;
            if sample.len() < 5 {
                sample.push(format!("{desc} missing from original output"));
            }
        }
    }
    assert!(
        diff_count == 0,
        "smali text mismatch after rebuild: {diff_count} differing classes (examples: {})",
        sample.join(", ")
    );
}

const TRY_CATCH_CLASS: &str = r#"
.class public Lroundtrip/TryCatchSample;
.super Ljava/lang/Object;

.method public static safeLoad()V
    .registers 2
    const-string v0, "kotlinx/coroutines/test/internal/TestMainDispatcherFactory"
    :try_start
    invoke-static {v0}, Ljava/lang/Class;->forName(Ljava/lang/String;)Ljava/lang/Class;
    :try_end
    return-void
    .catch Ljava/lang/ClassNotFoundException; {:try_start .. :try_end} :handler

    :handler
    return-void
.end method

.method public constructor <init>()V
    .registers 1
    invoke-direct {p0}, Ljava/lang/Object;-><init>()V
    return-void
.end method
"#;

#[test]
fn preserves_try_catch_directives() {
    let original = SmaliClass::from_smali(TRY_CATCH_CLASS).expect("parse sample class");
    let assembled = SmaliClass::from_smali(TRY_CATCH_CLASS).expect("parse sample class");
    let dex = DexFile::from_smali(&[assembled]).expect("assemble sample class");
    let rebuilt = dex.to_smali().expect("roundtrip to smali");
    let rebuilt_class = rebuilt
        .iter()
        .find(|c| c.name == original.name)
        .expect("class present after roundtrip");

    let count_catches = |cls: &SmaliClass| {
        cls.methods
            .iter()
            .find(|m| m.name == "safeLoad")
            .expect("safeLoad present")
            .ops
            .iter()
            .filter(|op| matches!(op, SmaliOp::Catch(_)))
            .count()
    };

    let original = count_catches(&original);
    let roundtrip = count_catches(rebuilt_class);

    assert_eq!(
        original, roundtrip,
        "catch directives changed after roundtrip"
    );
}

#[test]
fn fast_service_loader_catches_present() {
    let bytes = fs::read("tests/rootbeer.dex").expect("read rootbeer fixture");
    let dex = DexFile::from_bytes(&bytes).expect("parse rootbeer dex");
    let classes = dex.to_smali().expect("convert dex to smali");
    let class = classes
        .iter()
        .find(|c| c.name.as_jni_type() == "Lkotlinx/coroutines/internal/FastServiceLoader;")
        .expect("FastServiceLoader present");
    let method = class
        .methods
        .iter()
        .find(|m| m.name == "loadMainDispatcherFactory$kotlinx_coroutines_core")
        .expect("loadMainDispatcherFactory present");
    let typed_exceptions: Vec<_> = method
        .ops
        .iter()
        .filter_map(|op| {
            if let SmaliOp::Catch(CatchDirective::Catch { exception, .. }) = op {
                Some(exception.clone())
            } else {
                None
            }
        })
        .collect();
    let class_not_found = typed_exceptions
        .iter()
        .filter(|exc| exc == &&"Ljava/lang/ClassNotFoundException;".to_string())
        .count();
    assert!(
        class_not_found >= 2,
        "expected at least two ClassNotFoundException catches, found {class_not_found}"
    );
}
