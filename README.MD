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
