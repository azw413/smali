use std::collections::BTreeMap;
use std::fs;

use crate::dex::DexFile;
use crate::types::SmaliClass;

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

    assert_eq!(
        original_map, rebuilt_map,
        "smali text mismatch after rebuild"
    );
}
