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

    for p in dir.read_dir().unwrap() {
        if let Ok(p) = p {
            // Directory: recurse sub-directory
            if let Ok(f) = p.file_type() {
                if f.is_dir() {
                    let mut new_dir = dir.clone();
                    new_dir.push(p.file_name());
                    let dir_hs = find_smali_files(&new_dir)?;
                    results.extend(dir_hs);
                } else {
                    // It's a smali file
                    if p.file_name().to_str().unwrap().ends_with(".smali") {
                        let dex_file = SmaliClass::read_from_file(&p.path())?;
                        results.push(dex_file);
                    }
                }
            }
        }
    }

    Ok(results)
}
