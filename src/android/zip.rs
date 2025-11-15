use crc32fast::Hasher as Crc32;
use flate2::Compression;
use flate2::write::DeflateEncoder;
use std::collections::{BTreeMap, BTreeSet};
use std::fs::{self, File};
use std::io::{self, Read, Write};
use std::path::{Component, Path, PathBuf};
use zip::read::ZipArchive;

/// Result alias for APK (ZIP) operations.
pub type ApkZipResult<T> = Result<T, ApkZipError>;

/// Errors surfaced by the APK packing/unpacking helpers.
#[derive(Debug)]
pub enum ApkZipError {
    Io(io::Error),
    Zip(zip::result::ZipError),
    InvalidInput(String),
}

impl std::fmt::Display for ApkZipError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ApkZipError::Io(err) => write!(f, "I/O error: {err}"),
            ApkZipError::Zip(err) => write!(f, "ZIP error: {err}"),
            ApkZipError::InvalidInput(msg) => write!(f, "{msg}"),
        }
    }
}

impl std::error::Error for ApkZipError {}

impl From<io::Error> for ApkZipError {
    fn from(value: io::Error) -> Self {
        ApkZipError::Io(value)
    }
}

impl From<zip::result::ZipError> for ApkZipError {
    fn from(value: zip::result::ZipError) -> Self {
        ApkZipError::Zip(value)
    }
}

/// Compression preference for an APK entry.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ApkCompression {
    Stored,
    Deflated,
}

/// A single file entry stored in [`ApkFile`].
#[derive(Clone, Debug)]
pub struct ApkEntry {
    pub data: Vec<u8>,
    pub unix_mode: Option<u32>,
    pub compression: Option<ApkCompression>,
}

impl ApkEntry {
    pub fn new(data: Vec<u8>) -> Self {
        ApkEntry {
            data,
            unix_mode: None,
            compression: None,
        }
    }

    pub fn with_mode(mut self, mode: Option<u32>) -> Self {
        self.unix_mode = mode;
        self
    }

    pub fn with_compression(mut self, compression: Option<ApkCompression>) -> Self {
        self.compression = compression;
        self
    }
}

/// An in-memory representation of an APK (ZIP) file.
///
/// Entries are stored in a deterministic `BTreeMap`, so you can modify APK contents without
/// touching the host filesystem and without worrying about path collisions on case-insensitive
/// platforms.
pub struct ApkFile {
    entries: BTreeMap<String, ApkEntry>,
}

impl ApkFile {
    pub fn new() -> Self {
        ApkFile {
            entries: BTreeMap::new(),
        }
    }

    /// Load an APK from disk into memory.
    pub fn from_file(path: impl AsRef<Path>) -> ApkZipResult<Self> {
        let path = path.as_ref();
        let file = File::open(path)?;
        let mut archive = ZipArchive::new(file)?;
        let mut entries = BTreeMap::new();
        for idx in 0..archive.len() {
            let mut entry = archive.by_index(idx)?;
            if entry.name().ends_with('/') {
                continue;
            }
            let mut data = Vec::with_capacity(entry.size() as usize);
            entry.read_to_end(&mut data)?;
            #[allow(deprecated)]
            let sanitized = entry.sanitized_name();
            let name = path_to_entry_name(&sanitized)?;
            let compression = match entry.compression() {
                zip::CompressionMethod::Stored => Some(ApkCompression::Stored),
                zip::CompressionMethod::Deflated => Some(ApkCompression::Deflated),
                _ => None,
            };
            let apk_entry = ApkEntry::new(data)
                .with_mode(entry.unix_mode())
                .with_compression(compression);
            entries.insert(name, apk_entry);
        }
        Ok(ApkFile { entries })
    }

    /// Snapshot a directory tree into an in-memory APK.
    pub fn from_directory(dir: impl AsRef<Path>) -> ApkZipResult<Self> {
        let dir = dir.as_ref();
        if !dir.is_dir() {
            return Err(ApkZipError::InvalidInput(format!(
                "{} is not a directory",
                dir.display()
            )));
        }
        let mut entries = BTreeMap::new();
        gather_directory_entries(dir, dir, &mut entries)?;
        Ok(ApkFile { entries })
    }

    /// Serialize the in-memory APK back to disk as a ZIP/APK file.
    pub fn write_to_file(&self, path: impl AsRef<Path>) -> ApkZipResult<()> {
        let path = path.as_ref();
        if let Some(parent) = path.parent() {
            if !parent.as_os_str().is_empty() {
                fs::create_dir_all(parent)?;
            }
        }
        let mut file_names: Vec<_> = self.entries.keys().cloned().collect();
        file_names.sort();
        let mut buffer = Vec::new();
        let mut central_records = Vec::new();

        for name in &file_names {
            let entry = &self.entries[name];
            let plan = plan_entry(name, entry);
            let record = write_local_entry(&mut buffer, name, entry, &plan)?;
            central_records.push(record);
        }

        // Directory records (deterministic order)
        let directory_names = collect_directory_names(&file_names);
        for dir in directory_names {
            let record = write_directory_entry(&mut buffer, &dir)?;
            central_records.push(record);
        }

        let central_start = buffer.len() as u32;
        for record in &central_records {
            write_central_directory_entry(&mut buffer, record);
        }
        let central_size = buffer.len() as u32 - central_start;
        write_end_of_central_directory(
            &mut buffer,
            central_records.len(),
            central_size,
            central_start,
        );

        fs::write(path, buffer)?;
        Ok(())
    }

    /// Materialize the APK entries into a directory on disk (overwrites existing files).
    pub fn write_to_directory(&self, dir: impl AsRef<Path>) -> ApkZipResult<()> {
        let dir = dir.as_ref();
        fs::create_dir_all(dir)?;
        for (name, entry) in &self.entries {
            let path = dir.join(name);
            if let Some(parent) = path.parent() {
                fs::create_dir_all(parent)?;
            }
            fs::write(&path, &entry.data)?;
            #[cfg(unix)]
            if let Some(mode) = entry.unix_mode {
                use std::os::unix::fs::PermissionsExt;
                fs::set_permissions(&path, fs::Permissions::from_mode(mode))?;
            }
        }
        Ok(())
    }

    /// Iterate over entry names.
    pub fn entry_names(&self) -> impl Iterator<Item = &str> {
        self.entries.keys().map(|s| s.as_str())
    }

    /// Borrow an entry by name (e.g., `classes.dex`).
    pub fn entry(&self, name: &str) -> Option<&ApkEntry> {
        self.entries.get(name)
    }

    /// Replace the contents of an entry (or add a new entry).
    pub fn replace_entry(&mut self, name: impl AsRef<str>, data: Vec<u8>) -> ApkZipResult<()> {
        let normalized = normalize_entry_name(name.as_ref())?;
        self.entries.insert(normalized, ApkEntry::new(data));
        Ok(())
    }

    /// Remove an entry by name.
    pub fn remove_entry(&mut self, name: &str) -> bool {
        self.entries.remove(name).is_some()
    }
}

/// Convenience wrapper: unpack an APK onto disk.
pub fn unpack_apk(apk_path: impl AsRef<Path>, output_dir: impl AsRef<Path>) -> ApkZipResult<()> {
    let apk = ApkFile::from_file(apk_path)?;
    apk.write_to_directory(output_dir)
}

/// Convenience wrapper: repack a directory tree into an APK.
pub fn pack_apk(input_dir: impl AsRef<Path>, output_apk: impl AsRef<Path>) -> ApkZipResult<()> {
    let apk = ApkFile::from_directory(input_dir)?;
    apk.write_to_file(output_apk)
}

#[derive(Clone, Copy)]
struct EntryPlan {
    compression: ApkCompression,
    alignment: Option<u32>,
}

#[derive(Clone)]
struct CentralDirectoryRecord {
    file_name: Vec<u8>,
    compression: ApkCompression,
    crc32: u32,
    compressed_size: u32,
    uncompressed_size: u32,
    local_header_offset: u32,
    external_attrs: u32,
    is_directory: bool,
}

fn plan_entry(name: &str, entry: &ApkEntry) -> EntryPlan {
    let mut plan = classify_entry(name);
    if let Some(preferred) = entry.compression {
        plan.compression = preferred;
    }
    if plan.compression != ApkCompression::Stored {
        plan.alignment = None;
    }
    plan
}

fn write_local_entry(
    buf: &mut Vec<u8>,
    name: &str,
    entry: &ApkEntry,
    plan: &EntryPlan,
) -> ApkZipResult<CentralDirectoryRecord> {
    let offset = buf.len() as u32;
    let extra_len = if let Some(align) = plan.alignment {
        alignment_padding(offset, name.len(), align)
    } else {
        0
    };

    let (compressed_bytes, compression_method) = match plan.compression {
        ApkCompression::Stored => (entry.data.clone(), 0u16),
        ApkCompression::Deflated => (deflate_bytes(&entry.data)?, 8u16),
    };

    let mut crc = Crc32::new();
    crc.update(&entry.data);
    let crc32 = crc.finalize();

    write_u32(buf, 0x04034b50);
    write_u16(buf, 20);
    write_u16(buf, 0);
    write_u16(buf, compression_method);
    write_u16(buf, 0);
    write_u16(buf, 0);
    write_u32(buf, crc32);
    write_u32(buf, compressed_bytes.len() as u32);
    write_u32(buf, entry.data.len() as u32);
    write_u16(buf, name.as_bytes().len() as u16);
    write_u16(buf, extra_len as u16);
    buf.extend_from_slice(name.as_bytes());
    if extra_len > 0 {
        buf.extend(std::iter::repeat(0u8).take(extra_len as usize));
    }
    buf.extend_from_slice(&compressed_bytes);

    Ok(CentralDirectoryRecord {
        file_name: name.as_bytes().to_vec(),
        compression: plan.compression,
        crc32,
        compressed_size: compressed_bytes.len() as u32,
        uncompressed_size: entry.data.len() as u32,
        local_header_offset: offset,
        external_attrs: entry.unix_mode.unwrap_or(0o644) << 16,
        is_directory: false,
    })
}

fn write_directory_entry(buf: &mut Vec<u8>, name: &str) -> ApkZipResult<CentralDirectoryRecord> {
    let offset = buf.len() as u32;
    write_u32(buf, 0x04034b50);
    write_u16(buf, 10);
    write_u16(buf, 0);
    write_u16(buf, 0);
    write_u16(buf, 0);
    write_u16(buf, 0);
    write_u32(buf, 0);
    write_u32(buf, 0);
    write_u32(buf, 0);
    write_u16(buf, name.as_bytes().len() as u16);
    write_u16(buf, 0);
    buf.extend_from_slice(name.as_bytes());

    Ok(CentralDirectoryRecord {
        file_name: name.as_bytes().to_vec(),
        compression: ApkCompression::Stored,
        crc32: 0,
        compressed_size: 0,
        uncompressed_size: 0,
        local_header_offset: offset,
        external_attrs: (0o755u32 << 16) | 0x10,
        is_directory: true,
    })
}

fn write_central_directory_entry(buf: &mut Vec<u8>, record: &CentralDirectoryRecord) {
    write_u32(buf, 0x02014b50);
    write_u16(buf, 0x031E);
    write_u16(buf, 20);
    write_u16(buf, 0);
    let method = match record.compression {
        ApkCompression::Stored => 0u16,
        ApkCompression::Deflated => 8u16,
    };
    write_u16(buf, method);
    write_u16(buf, 0);
    write_u16(buf, 0);
    write_u32(buf, record.crc32);
    write_u32(buf, record.compressed_size);
    write_u32(buf, record.uncompressed_size);
    write_u16(buf, record.file_name.len() as u16);
    write_u16(buf, 0);
    write_u16(buf, 0);
    write_u16(buf, 0);
    write_u16(buf, if record.is_directory { 0x10 } else { 0 });
    write_u32(buf, record.external_attrs);
    write_u32(buf, record.local_header_offset);
    buf.extend_from_slice(&record.file_name);
}

fn write_end_of_central_directory(
    buf: &mut Vec<u8>,
    entry_count: usize,
    central_size: u32,
    central_offset: u32,
) {
    write_u32(buf, 0x06054b50);
    write_u16(buf, 0);
    write_u16(buf, 0);
    write_u16(buf, entry_count as u16);
    write_u16(buf, entry_count as u16);
    write_u32(buf, central_size);
    write_u32(buf, central_offset);
    write_u16(buf, 0);
}

fn deflate_bytes(data: &[u8]) -> ApkZipResult<Vec<u8>> {
    let mut encoder = DeflateEncoder::new(Vec::new(), Compression::default());
    encoder.write_all(data)?;
    Ok(encoder.finish()?)
}

fn gather_directory_entries(
    root: &Path,
    current: &Path,
    entries: &mut BTreeMap<String, ApkEntry>,
) -> ApkZipResult<()> {
    for entry in fs::read_dir(current)? {
        let entry = entry?;
        let path = entry.path();
        if entry.file_type()?.is_dir() {
            gather_directory_entries(root, &path, entries)?;
            continue;
        }
        if !entry.file_type()?.is_file() {
            continue;
        }
        let rel = relative_entry_path(root, &path)?;
        let mut data = Vec::new();
        File::open(&path)?.read_to_end(&mut data)?;
        let permissions = default_permissions(&entry);
        entries.insert(rel, ApkEntry::new(data).with_mode(Some(permissions)));
    }
    Ok(())
}

fn default_permissions(entry: &fs::DirEntry) -> u32 {
    #[cfg(unix)]
    {
        use std::os::unix::fs::MetadataExt;
        entry.metadata().map(|m| m.mode()).unwrap_or(0o644)
    }
    #[cfg(not(unix))]
    {
        entry
            .metadata()
            .map(|m| {
                if m.permissions().readonly() {
                    0o444
                } else {
                    0o644
                }
            })
            .unwrap_or(0o644)
    }
}

fn relative_entry_path(root: &Path, file: &Path) -> ApkZipResult<String> {
    let rel = file.strip_prefix(root).map_err(|_| {
        ApkZipError::InvalidInput(format!(
            "{} is not under {}",
            file.display(),
            root.display()
        ))
    })?;
    path_to_entry_name(rel)
}

fn path_to_entry_name(path: &Path) -> ApkZipResult<String> {
    let mut components = Vec::new();
    for comp in path.components() {
        match comp {
            Component::Normal(part) => components.push(part.to_string_lossy().replace('\\', "/")),
            Component::CurDir => {}
            Component::RootDir | Component::Prefix(_) => {
                return Err(ApkZipError::InvalidInput(format!(
                    "invalid entry path component in {}",
                    path.display()
                )));
            }
            Component::ParentDir => {
                return Err(ApkZipError::InvalidInput(
                    "entry paths may not contain parent components".to_string(),
                ));
            }
        }
    }
    if components.is_empty() {
        return Err(ApkZipError::InvalidInput(
            "entry name must not be empty".to_string(),
        ));
    }
    Ok(components.join("/"))
}

fn normalize_entry_name(name: &str) -> ApkZipResult<String> {
    path_to_entry_name(Path::new(name))
}

fn collect_directory_names(file_names: &[String]) -> Vec<String> {
    let mut dirs = BTreeSet::new();
    for name in file_names {
        let mut path = PathBuf::new();
        let components: Vec<_> = Path::new(name).components().collect();
        for (idx, component) in components.iter().enumerate() {
            if idx == components.len() - 1 {
                break;
            }
            if let Component::Normal(part) = component {
                path.push(part);
                dirs.insert(format!("{}/", path.to_string_lossy()));
            }
        }
    }
    dirs.into_iter().collect()
}

fn write_u16(buf: &mut Vec<u8>, value: u16) {
    buf.extend_from_slice(&value.to_le_bytes());
}

fn write_u32(buf: &mut Vec<u8>, value: u32) {
    buf.extend_from_slice(&value.to_le_bytes());
}

fn alignment_padding(offset: u32, name_len: usize, alignment: u32) -> u32 {
    if alignment <= 1 {
        return 0;
    }
    let base = offset as u64 + 30 + name_len as u64;
    let align = alignment as u64;
    ((align - (base % align)) % align) as u32
}

fn classify_entry(name: &str) -> EntryPlan {
    let lower = name.to_ascii_lowercase();
    let compression = if should_store_uncompressed(&lower) {
        ApkCompression::Stored
    } else {
        ApkCompression::Deflated
    };
    let alignment = if compression == ApkCompression::Stored {
        if lower.starts_with("lib/") && lower.ends_with(".so") {
            Some(16 * 1024)
        } else {
            Some(4)
        }
    } else {
        None
    };
    EntryPlan {
        compression,
        alignment,
    }
}

fn should_store_uncompressed(name: &str) -> bool {
    if name == "resources.arsc" {
        return true;
    }
    name.ends_with(".arsc")
        || name.ends_with(".dex")
        || name.ends_with(".so")
        || matches!(
            name.rsplit('.').next(),
            Some(ext)
                if matches!(
                    ext,
                    "png"
                        | "jpg"
                        | "jpeg"
                        | "gif"
                        | "webp"
                        | "heic"
                        | "avif"
                        | "mp3"
                        | "ogg"
                        | "wav"
                        | "aac"
                        | "flac"
                        | "m4a"
                        | "mp4"
                        | "webm"
                        | "mkv"
                        | "ico"
                )
        )
}
