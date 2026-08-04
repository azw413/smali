//! App Bundle format tests.
//!
//! The manifest codec is checked by building a manifest, encoding it, and
//! decoding it back, so these run without a bundle fixture. Tests against a
//! real `.aab` are gated behind `SMALI_TEST_AAB`, which should point at one:
//!
//! ```sh
//! SMALI_TEST_AAB=/path/to/app.aab cargo test aab
//! ```

use crate::android::aab::{
    self, BundleFormat, decode_manifest, detect_format, encode_manifest, module_of, to_apk_path,
    to_bundle_path,
};
use crate::android::binary_xml::{
    ANDROID_NAMESPACE_URI, AndroidManifest, ManifestAttribute, ManifestElement, ManifestValue,
};

fn sample_manifest() -> AndroidManifest {
    let mut root = ManifestElement::with_namespace_and_uri(
        "manifest",
        "android",
        ANDROID_NAMESPACE_URI,
        // `with_namespace_and_uri` takes the tag plus namespace metadata.
    );
    root.set_attribute(ManifestAttribute::new(
        "package",
        ManifestValue::String("com.example.app".into()),
    ));
    let mut version = ManifestAttribute::with_namespace_and_uri(
        "android",
        ANDROID_NAMESPACE_URI,
        "versionCode",
        ManifestValue::Integer(42),
    );
    version.resource_id = Some(0x0101021b);
    root.set_attribute(version);

    let mut app = ManifestElement::new("application");
    let mut debuggable = ManifestAttribute::with_namespace_and_uri(
        "android",
        ANDROID_NAMESPACE_URI,
        "debuggable",
        ManifestValue::Boolean(true),
    );
    debuggable.resource_id = Some(0x0101000f);
    app.set_attribute(debuggable);

    let mut activity = ManifestElement::new("activity");
    let mut name = ManifestAttribute::with_namespace_and_uri(
        "android",
        ANDROID_NAMESPACE_URI,
        "name",
        ManifestValue::String("com.example.app.Main".into()),
    );
    name.resource_id = Some(0x01010003);
    activity.set_attribute(name);
    app.append_child(activity);
    root.append_child(app);

    AndroidManifest::from_root(root)
}

#[test]
fn manifest_round_trips_through_protobuf() {
    let original = sample_manifest();
    let bytes = encode_manifest(&original).expect("encode");
    let decoded = decode_manifest(&bytes).expect("decode");

    assert_eq!(decoded.package_name(), Some("com.example.app"));
    assert_eq!(
        decoded.root().attribute_value("android:versionCode"),
        Some(&ManifestValue::Integer(42))
    );

    let app = decoded.root().find_child("application").expect("application");
    assert_eq!(
        app.attribute_value("android:debuggable"),
        Some(&ManifestValue::Boolean(true))
    );
    let activity = app.find_child("activity").expect("activity");
    assert_eq!(
        activity.attribute_value("android:name"),
        Some(&ManifestValue::String("com.example.app.Main".into()))
    );
}

#[test]
fn encoding_is_idempotent() {
    // Re-encoding a decoded manifest must be stable, otherwise repeated
    // read-modify-write cycles would keep changing the file.
    let bytes = encode_manifest(&sample_manifest()).expect("encode");
    let again = encode_manifest(&decode_manifest(&bytes).expect("decode")).expect("re-encode");
    assert_eq!(bytes, again, "encode(decode(x)) must be stable");
}

#[test]
fn resource_ids_survive_the_round_trip() {
    // An AAB carries the framework attribute id per attribute. Losing it makes
    // Android ignore the attribute entirely.
    let bytes = encode_manifest(&sample_manifest()).expect("encode");
    let decoded = decode_manifest(&bytes).expect("decode");
    let attr = decoded
        .root()
        .attributes
        .iter()
        .find(|a| a.name == "versionCode")
        .expect("versionCode present");
    assert_eq!(attr.resource_id, Some(0x0101021b));
}

#[test]
fn namespace_declarations_are_emitted_on_the_root() {
    let bytes = encode_manifest(&sample_manifest()).expect("encode");
    let decoded = decode_manifest(&bytes).expect("decode");
    // The prefix must be recoverable on a nested element's attribute, which
    // only works if the declaration was written at the root and inherited.
    let activity = decoded
        .root()
        .find_child("application")
        .and_then(|a| a.find_child("activity"))
        .expect("activity");
    let attr = &activity.attributes[0];
    assert_eq!(attr.namespace_prefix.as_deref(), Some("android"));
    assert_eq!(attr.namespace_uri.as_deref(), Some(ANDROID_NAMESPACE_URI));
}

#[test]
fn malformed_protobuf_errors_rather_than_panicking() {
    // Same contract as every other parser in the crate: bundles come from the
    // wild, so bad input must be an Err.
    let good = encode_manifest(&sample_manifest()).expect("encode");
    for cut in 0..good.len() {
        let slice = good[..cut].to_vec();
        let result = std::panic::catch_unwind(move || decode_manifest(&slice));
        assert!(result.is_ok(), "truncating to {cut} bytes panicked");
    }
    assert!(decode_manifest(&[0xff, 0xff, 0xff]).is_err());
    assert!(decode_manifest(&[]).is_err());
}

#[test]
fn format_detection_keys_on_bundle_only_paths() {
    assert_eq!(
        detect_format(["AndroidManifest.xml", "classes.dex"]),
        BundleFormat::Apk
    );
    assert_eq!(
        detect_format(["base/manifest/AndroidManifest.xml", "base/dex/classes.dex"]),
        BundleFormat::Aab
    );
    assert_eq!(detect_format(["BundleConfig.pb"]), BundleFormat::Aab);
    // An APK may legitimately contain a directory called base/.
    assert_eq!(
        detect_format(["AndroidManifest.xml", "assets/base/thing.txt"]),
        BundleFormat::Apk
    );
}

#[test]
fn paths_map_between_the_two_layouts() {
    let cases = [
        ("base/manifest/AndroidManifest.xml", "AndroidManifest.xml"),
        ("base/dex/classes.dex", "classes.dex"),
        ("base/dex/classes2.dex", "classes2.dex"),
        ("base/root/META-INF/services/x", "META-INF/services/x"),
        ("base/lib/arm64-v8a/libx.so", "lib/arm64-v8a/libx.so"),
        ("base/res/drawable/ic.png", "res/drawable/ic.png"),
        ("base/assets/data.bin", "assets/data.bin"),
    ];
    for (bundle, apk) in cases {
        assert_eq!(to_apk_path(bundle).as_deref(), Some(apk), "{bundle}");
        assert_eq!(to_bundle_path("base", apk), bundle, "reverse of {apk}");
    }

    // Bundle-only artefacts have no APK equivalent.
    for only in [
        "base/resources.pb",
        "base/native.pb",
        "BundleConfig.pb",
        "BUNDLE-METADATA/com.android.tools/x.pb",
    ] {
        assert_eq!(to_apk_path(only), None, "{only} should have no APK path");
    }
}

#[test]
fn module_detection_skips_non_module_directories() {
    assert_eq!(module_of("base/dex/classes.dex"), Some("base"));
    assert_eq!(module_of("feature1/dex/classes.dex"), Some("feature1"));
    assert_eq!(module_of("BUNDLE-METADATA/x/y.pb"), None);
    assert_eq!(module_of("META-INF/MANIFEST.MF"), None);
    assert_eq!(module_of("BundleConfig.pb"), None);
}

#[test]
fn targeting_scanners_collect_distinct_directories() {
    let entries = [
        "base/lib/arm64-v8a/libfoo.so",
        "base/lib/arm64-v8a/libbar.so",
        "base/lib/x86_64/libfoo.so",
        "base/lib/",
        "base/dex/classes.dex",
        "base/assets/level1/data.bin",
        "base/assets/level1/more.bin",
        "base/assets/level2/data.bin",
    ];
    assert_eq!(
        aab::native_abis_from_entries(entries),
        vec!["arm64-v8a".to_string(), "x86_64".to_string()]
    );
    assert_eq!(
        aab::asset_dirs_from_entries(entries),
        vec!["assets/level1".to_string(), "assets/level2".to_string()]
    );
}

#[test]
fn native_pb_encodes_known_abis_and_skips_unknown() {
    let bytes = aab::encode_native_pb(&["arm64-v8a".to_string()]);
    assert!(!bytes.is_empty());
    // directory(1) { path(1) = "lib/arm64-v8a", targeting(2) { abi(1) { value(1) { alias(1)=3 } } } }
    assert_eq!(bytes[0], 0x0a, "field 1, wire type 2");
    assert!(
        bytes.windows(13).any(|w| w == b"lib/arm64-v8a"),
        "path should be present"
    );

    // An unknown ABI directory must be skipped rather than emitted with a bogus
    // alias, which bundletool would reject.
    assert!(aab::encode_native_pb(&["sparc".to_string()]).is_empty());
}

#[test]
fn bundles_do_not_get_directory_zip_entries() {
    // bundletool rejects an .aab containing directory entries, and the zip
    // writer synthesises one per path prefix by default.
    use crate::android::zip::ApkFile;

    let mut apk = ApkFile::new();
    apk.replace_entry("base/dex/classes.dex", b"not really a dex".to_vec())
        .expect("add entry");
    assert!(apk.directory_entries(), "APKs keep directory entries");

    apk.set_directory_entries(false);
    assert!(!apk.directory_entries());
}

// ---------------------------------------------------------------------------
// Optional tests against a real bundle.
// ---------------------------------------------------------------------------

fn real_bundle() -> Option<Vec<u8>> {
    let path = std::env::var("SMALI_TEST_AAB").ok()?;
    std::fs::read(path).ok()
}

#[test]
fn real_bundle_manifest_round_trips() {
    let Some(bytes) = real_bundle() else {
        eprintln!("skipping: set SMALI_TEST_AAB to a .aab to run this test");
        return;
    };
    use crate::android::zip::ApkFile;
    let dir = std::env::temp_dir().join("smali_aab_test.aab");
    std::fs::write(&dir, &bytes).expect("write temp bundle");
    let bundle = ApkFile::from_file(&dir).expect("open bundle");

    let names: Vec<String> = bundle.entry_names().map(|s| s.to_string()).collect();
    assert_eq!(
        detect_format(names.iter().map(|s| s.as_str())),
        BundleFormat::Aab,
        "fixture should be a bundle"
    );
    assert!(
        !bundle.directory_entries(),
        "loading a bundle must disable directory entries"
    );

    let raw = &bundle
        .entry(aab::MANIFEST_PATH)
        .expect("bundle has a manifest")
        .data;
    let manifest = decode_manifest(raw).expect("decode real manifest");
    assert!(manifest.package_name().is_some(), "package name recovered");

    // Re-encoding must at minimum be stable and semantically equivalent.
    let re = encode_manifest(&manifest).expect("encode");
    let again = decode_manifest(&re).expect("re-decode");
    assert_eq!(again.package_name(), manifest.package_name());
    assert_eq!(
        encode_manifest(&again).expect("encode twice"),
        re,
        "encoding must be idempotent"
    );
}
