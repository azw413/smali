use smali::dex::DexFile;
use std::env;
use std::error::Error;
use std::fs;
use std::path::Path;

fn main() {
    let args: Vec<String> = env::args().collect();

    // Do everything else with the error trap
    match process_dex(&args[1]) {
        Ok(_) => {
            println!("All done: written smali to out/");
        }
        Err(e) => {
            println!("Aborted due to error: {:?}", e);
        }
    }
}

/* This is where all the processing takes place, to make error handling easier */
fn process_dex(dex_file: &str) -> Result<(), Box<dyn Error>> {
    // Parse the DEX
    let dex = DexFile::from_file(Path::new(dex_file))?;

    // Convert to Smali structures
    let classes = dex.to_smali()?;

    // Ensure the output root exists
    let out_root = Path::new("out");
    fs::create_dir_all(out_root)?;

    // Write each class to out/<pkg>/<Class>.smali, mirroring package structure
    for sc in classes {
        // SmaliClass.name.class_name is like "com/example/Foo$Bar"
        let class_path = &sc.name.as_java_type();

        // Build the output path
        let mut path = out_root.to_path_buf();
        for seg in class_path.split('.') {
            path.push(seg);
        }
        path.set_extension("smali");

        // Create parent dirs and write the file
        if let Some(parent) = path.parent() {
            fs::create_dir_all(parent)?;
        }
        let text = format!("{}", sc.to_smali());
        fs::write(&path, text)?;
    }

    Ok(())
}
