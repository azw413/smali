use smali::dex::DexFile;
use smali::find_smali_files;
use std::env;
use std::error::Error;
use std::path::PathBuf;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 3 {
        eprintln!("Usage: {} <smali_dir> <output.dex>", args[0]);
        std::process::exit(1);
    }

    match process_smali(&args[1], &args[2]) {
        Ok(_) => println!("Wrote DEX to {}", args[2]),
        Err(err) => {
            eprintln!("Failed: {err:?}");
            std::process::exit(1);
        }
    }
}

fn process_smali(input_dir: &str, output_path: &str) -> Result<(), Box<dyn Error>> {
    let dir = PathBuf::from(input_dir);
    let mut classes = find_smali_files(&dir)?;

    // Sort for deterministic ordering in the output file.
    classes.sort_by(|a, b| a.name.as_jni_type().cmp(&b.name.as_jni_type()));

    let dex = DexFile::from_smali(&classes)?;
    dex.to_path(output_path)?;
    Ok(())
}
