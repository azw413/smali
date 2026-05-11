# Smali Crate

A pure rust implementation of a parser, writer and set of types for the smali file format and a dex file reader and writer.

Smali is used in the Android reversing community to examine and modify Android APK files. It's a human readable text format that represents the complete structure of a Dex VM class including all of its instructions. 
The serialization and deserialization aim to be 100% compatible with Jesus Freke's Smali/Baksmali implementation.

The examples folder contains three examples :- 

1. rootbeer.rs which uses ApkFile to unpack any APK using Rootbeer, disassemble the dex files, find the rootbeer classes, patch the methods to disable the detection, recompile the dex and repackage the APK (it just needs re-signin afterwards). 
2. dex2smali.rs which takes a dex file and writes each contained class into the out directory in a package directory heirarchy.
3. smali2dex.rs which is the opposite, takes a directory of smali files and builds a working dex file. 

With this crate you can use it to disassemble, analyse and patch Android APK, manifest and DEX files. It is completely self-contained and does not rely on any Java based dependencies.

Here's the simple example from rootbeer.rs illustrating patching the APK file :-

```rust
fn process_apk(apk_path: &str) -> Result<(), Box<dyn Error>> {
    let mut apk = ApkFile::from_file(apk_path)?;
    let dex_entries: Vec<String> = apk
        .entry_names()
        .filter(|name| name.ends_with(".dex"))
        .map(|s| s.to_string())
        .collect();
    if dex_entries.is_empty() {
        return Err("No classes*.dex files found in APK".into());
    }

    let mut patched = false;
    for entry_name in dex_entries {
        if patch_dex_entry(&mut apk, &entry_name)? {
            println!("Patched {entry_name}");
            patched = true;
        }
    }

    if !patched {
        println!("No RootBeer detections found; writing original APK");
    }

    apk.write_to_file("out.apk")?;
    println!("Wrote patched APK to out.apk");
    Ok(())
}

fn patch_dex_entry(apk: &mut ApkFile, entry_name: &str) -> Result<bool, Box<dyn Error>> {
    let entry = apk
        .entry(entry_name)
        .ok_or_else(|| SmaliError::new(&format!("missing {entry_name}")))?;
    let dex = DexFile::from_bytes(&entry.data)?;
    let mut classes = dex.to_smali()?;
    let mut touched = false;

    for c in classes.iter_mut() {
        if is_rootbeer_class(c) {
            touched = true;
            for m in c.methods.iter_mut() {
                if m.signature.result == TypeSignature::Bool && m.signature.args.is_empty() {
                    let mut new_instructions = vec![
                        Op(DexOp::Const4 {
                            dest: v(0),
                            value: 0,
                        }),
                        Op(DexOp::Return { src: v(0) }),
                    ];
                    m.ops = new_instructions;
                    m.locals = 1;
                    println!(
                        "{} method {} successfully patched.",
                        c.name.as_java_type(),
                        &m.name
                    );
                }
            }
        }
    }

    if touched {
        let rebuilt = DexFile::from_smali(&classes)?;
        apk.replace_entry(entry_name, rebuilt.to_bytes().to_vec())?;
    }

    Ok(touched)
}
```

Take a look at the full examples for a better idea and also an example of how to update the Android manifest file.

## Working with large APKs (multi-DEX)

Modern Android apps frequently ship more than one DEX (`classes.dex`, `classes2.dex`, ...) because a single DEX can only address 65,536 distinct type / field / method / prototype references via its 16-bit instruction operands. When you load such an APK, modify enough classes, and write it back, the rebuilt set of classes may no longer fit in one DEX.

The `build_multi_dex_bytes` helper takes the same `&[SmaliClass]` as the single-DEX path but returns a `Vec<Vec<u8>>` — one byte buffer per output DEX. It topologically sorts classes and keeps inheritance families together so that a subclass and its superclass land in the same DEX whenever capacity allows. `ApkFile::set_dex_files` then writes those buffers as `classes.dex`, `classes2.dex`, ... and removes any stale `classesN.dex` entries left over from the original APK.

```rust
use smali::android::zip::{ApkFile, is_top_level_dex_name};
use smali::dex::DexFile;
use smali::dex::builder::build_multi_dex_bytes;
use smali::types::SmaliClass;

fn rebuild_apk(input: &str, output: &str) -> Result<(), Box<dyn std::error::Error>> {
    let mut apk = ApkFile::from_file(input)?;

    // Collect classes from every top-level DEX in the APK.
    let dex_names: Vec<String> = apk
        .entry_names()
        .filter(|n| is_top_level_dex_name(n))
        .map(|s| s.to_string())
        .collect();

    let mut classes: Vec<SmaliClass> = Vec::new();
    for name in &dex_names {
        let dex = DexFile::from_bytes(&apk.entry(name).unwrap().data)?;
        classes.extend(dex.to_smali()?);
    }

    // ... mutate `classes` here (patch methods, add/remove classes, ...) ...

    // Rebuild. If the result fits in one DEX you get a single-element Vec;
    // otherwise it auto-splits across as many DEXes as needed.
    let dex_files = build_multi_dex_bytes(&classes)?;
    apk.set_dex_files(dex_files)?;
    apk.write_to_file(output)?;
    Ok(())
}
```

`build_dex_file_bytes` (single-DEX) is still available and is the right choice when you know the output fits — it errors out at assembly time if any reference pool overflows, so it's also a good way to detect that you should switch to the multi-DEX path.

## Working with Kotlin metadata

Every class compiled by `kotlinc` carries an `@kotlin.Metadata` annotation that holds a Protobuf-encoded view of the original Kotlin source — class kind, generic type parameters, properties, extension functions, the JVM-vs-Kotlin name mapping, and so on. Kotlin reflection (`kotlin-reflect`), IDE tooling, `kotlinx.serialization`, DI frameworks like Koin/Hilt, and `@Composable` previews all consult this annotation *before* falling back to JVM signatures. If you rename a class or method at the DEX level without updating the metadata, those tools keep returning the pre-rewrite view and Kotlin reflection breaks at runtime.

The `smali::kotlin` module exposes the annotation as a plain mutable struct. The two interesting fields are:

- `proto_bytes` — the bit-unpacked Protobuf payload (`d1`). Treat as opaque unless you intend to decode the Kotlin metadata schema yourself.
- `string_pool` — the plain-text string pool (`d2`) referenced by index from `proto_bytes`. Almost every renameable Kotlin identifier (class names, function names, property names, type parameter names, JVM signatures) terminates in a slot here, so editing entries in `string_pool` is enough to "rename" Kotlin declarations without touching the protobuf at all.

```rust
use smali::dex::DexFile;
use smali::kotlin::{KotlinMetadata, KotlinMetadataKind};

fn dump_metadata(dex_path: &str) -> Result<(), Box<dyn std::error::Error>> {
    let bytes = std::fs::read(dex_path)?;
    let dex = DexFile::from_bytes(&bytes)?;
    let classes = dex.to_smali()?;

    for class in &classes {
        if let Some(meta) = class.kotlin_metadata()? {
            println!(
                "{}  kind={:?}  d2[{}]={:?}",
                class.name.as_jni_type(),
                meta.kind,
                meta.string_pool.len(),
                meta.string_pool.iter().take(3).collect::<Vec<_>>(),
            );
        }
    }
    Ok(())
}
```

Renaming a class requires updating its metadata *and* the metadata of every other class that references it (since they hold the old name in their own `string_pool`). The crate doesn't ship a rename helper — that's policy that belongs in your tool — but the pattern is short:

```rust
use smali::dex::DexFile;
use smali::dex::builder::build_multi_dex_bytes;

fn rename_in_metadata(
    classes: &mut [smali::types::SmaliClass],
    old: &str,
    new: &str,
) -> Result<usize, Box<dyn std::error::Error>> {
    let mut hits = 0;
    for class in classes.iter_mut() {
        let Some(mut meta) = class.kotlin_metadata()? else { continue };
        let mut touched = false;
        for slot in meta.string_pool.iter_mut() {
            if slot == old {
                *slot = new.to_string();
                hits += 1;
                touched = true;
            }
        }
        if touched {
            class.set_kotlin_metadata(&meta);
        }
    }
    Ok(hits)
}
```

A few things to leave alone when rewriting metadata:

- `metadata_version` (`mv`) — Kotlin reflection refuses anything newer than its own runtime knows. Don't bump it.
- `extra_int` (`xi`) — compiler bit flags (pre-release, IR backend, K2 frontend, strict semantics). Preserve verbatim.
- `proto_bytes` (`d1`) — only edit raw bytes if you've decoded the protobuf schema yourself (see `metadata.proto` and `jvm_metadata.proto` in the JetBrains/kotlin repository). For the common rename cases the string-pool approach above is enough; the protobuf doesn't need to change because it references strings by index.

For a low-level codec entry point — useful if you want to manipulate the `d1` byte stream directly — see `smali::kotlin::bit_encoding::{decode_d1, encode_d1}`.


