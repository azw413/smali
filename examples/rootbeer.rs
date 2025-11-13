use smali::android::zip::{pack_apk, unpack_apk};
use smali::dex::DexFile;
use smali::smali_ops::{DexOp, v};
use smali::types::SmaliOp::Op;
use smali::types::*;
use std::env;
use std::error::Error;
use std::fs;
use std::path::{Path, PathBuf};

// This demo unpacks an APK, searches for RootBeer and disables it by patching methods inside classes.dex.
// Try it on the RootBeer Sample app https://play.google.com/store/apps/details?id=com.scottyab.rootbeer.sample

//Usage: main <apk-file>
fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: {} <apk-file>", args[0]);
        std::process::exit(1);
    }

    // Do everything else with the error trap
    match process_apk(&args[1]) {
        Ok(_) => {
            println!("All done: written out.apk");
        }
        Err(e) => {
            println!("Aborted due to error: {:?}", e);
        }
    }
}

// Matches the known field / method signatures for the RootBeer.class (remember this could be renamed)
fn is_rootbeer_class(c: &SmaliClass) -> bool {
    if c.fields.len() == 2 && c.methods.len() == 25 {
        // Probably unreliable if the ordering changes
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

/* This is where all the processing takes place, to make error handling easier */
fn process_apk(apk_file: &str) -> Result<(), Box<dyn Error>> {
    let work_dir = PathBuf::from("out_apk");
    if work_dir.exists() {
        fs::remove_dir_all(&work_dir)?;
    }
    unpack_apk(apk_file, &work_dir)?;

    let mut dex_paths = Vec::new();
    collect_dex_files(&work_dir, &mut dex_paths)?;
    if dex_paths.is_empty() {
        return Err("No classes*.dex files found in APK".into());
    }

    let mut patched = false;
    for dex_path in dex_paths {
        if patch_dex_file(&dex_path)? {
            println!("Patched {}", dex_path.display());
            patched = true;
        }
    }

    if !patched {
        println!("No RootBeer detections found; output APK will match the input.");
    }

    pack_apk(&work_dir, "out.apk")?;
    println!("Wrote patched APK to out.apk");

    Ok(())
}

fn collect_dex_files(dir: &Path, out: &mut Vec<PathBuf>) -> Result<(), Box<dyn Error>> {
    for entry in fs::read_dir(dir)? {
        let entry = entry?;
        let path = entry.path();
        if entry.file_type()?.is_dir() {
            collect_dex_files(&path, out)?;
            continue;
        }
        if path
            .file_name()
            .and_then(|n| n.to_str())
            .map(|name| name.starts_with("classes") && name.ends_with(".dex"))
            .unwrap_or(false)
        {
            out.push(path);
        }
    }
    Ok(())
}

fn patch_dex_file(path: &Path) -> Result<bool, Box<dyn Error>> {
    let dex = DexFile::from_file(path)?;
    let mut classes = dex.to_smali()?;
    let mut touched = false;

    for c in classes.iter_mut() {
        if is_rootbeer_class(c) {
            touched = true;
            // Patch all the methods returning a boolean
            for m in c.methods.iter_mut() {
                if m.signature.result == TypeSignature::Bool && m.signature.args.is_empty() {
                    let mut new_instructions = vec![];
                    new_instructions.push(Op(DexOp::Const4 {
                        dest: v(0),
                        value: 0,
                    })); // "const/4 v0, 0x0" - Set v0 to false
                    new_instructions.push(Op(DexOp::Return { src: v(0) })); //  "return v0" - return v0
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
        rebuilt.to_path(path)?;
    }

    Ok(touched)
}
