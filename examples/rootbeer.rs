use smali::android::zip::ApkFile;
use smali::dex::DexFile;
use smali::smali_ops::{DexOp, v};
use smali::types::SmaliOp::Op;
use smali::types::*;
use std::env;
use std::error::Error;

// This demo reads an APK entirely in memory, searches for RootBeer and disables it by patching
// methods inside classes.dex, then writes a new APK without touching the filesystem for
// intermediate smali output.
// Try it on the RootBeer Sample app https://play.google.com/store/apps/details?id=com.scottyab.rootbeer.sample

//Usage: main <apk-file>
fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: {} <apk-file>", args[0]);
        std::process::exit(1);
    }

    match process_apk(&args[1]) {
        Ok(_) => println!("All done: written out.apk"),
        Err(e) => eprintln!("Aborted due to error: {e:?}"),
    }
}

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

// Matches the known field / method signatures for the RootBeer.class (remember this could be renamed)
fn is_rootbeer_class(c: &SmaliClass) -> bool {
    if c.fields.len() == 2 && c.methods.len() == 25 {
        if c.fields[0].signature == TypeSignature::Bool
            && c.methods[1].signature.result == TypeSignature::Bool
            && c.methods[4].signature.args.is_empty()
            && c.methods[4].signature.result == TypeSignature::Bool
        {
            println!("Detected RootBeer class: {}", c.name.as_java_type());
            return true;
        }
    }
    false
}
