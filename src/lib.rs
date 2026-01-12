//! # Smali
//!
//! A library for reading and writing Android smali files
//!
use crate::types::{SmaliClass, SmaliError};
use std::path::PathBuf;

pub mod android;
pub mod dex;
pub mod smali_ops;
mod smali_parse;
mod smali_write;
mod tests;
pub mod types;

/// Recurses a base path, typically a 'smali' folder from apktool returning a Vector of all found smali classes
///
/// # Examples
///
/// ```no_run
///  use smali::find_smali_files;
///  use std::path::PathBuf;
///  use std::str::FromStr;
///
///  let mut p = PathBuf::from_str("smali").unwrap();
///  let mut classes = find_smali_files(&p).unwrap();
///  println!("{:} smali classes loaded.", classes.len());
/// ```
pub fn find_smali_files(dir: &PathBuf) -> Result<Vec<SmaliClass>, SmaliError> {
    let mut results = vec![];

    let entries = dir.read_dir().map_err(|e| {
        SmaliError::new(&format!(
            "Error reading directory {}: {e}",
            dir.to_string_lossy()
        ))
    })?;

    for entry in entries {
        let entry = entry.map_err(|e| {
            SmaliError::new(&format!(
                "Error reading directory entry in {}: {e}",
                dir.to_string_lossy()
            ))
        })?;
        let file_type = entry.file_type().map_err(|e| {
            SmaliError::new(&format!(
                "Error reading file type for {}: {e}",
                entry.path().to_string_lossy()
            ))
        })?;
        if file_type.is_dir() {
            let mut new_dir = dir.clone();
            new_dir.push(entry.file_name());
            let dir_hs = find_smali_files(&new_dir)?;
            results.extend(dir_hs);
        } else if let Some(name) = entry.file_name().to_str() {
            if name.ends_with(".smali") {
                let dex_file = SmaliClass::read_from_file(&entry.path())?;
                results.push(dex_file);
            }
        } else {
            return Err(SmaliError::new(&format!(
                "Non-UTF8 file name in {}",
                entry.path().to_string_lossy()
            )));
        }
    }

    Ok(results)
}
