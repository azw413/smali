use crc32fast::Hasher as Crc32;
use flate2::Compression;
use flate2::write::DeflateEncoder;
use std::collections::BTreeSet;
use std::fs::{self, File};
use std::io::{self, Read, Write};
use std::path::Path;
use zip::read::ZipArchive;

/// Convenience result alias for APK (de)packaging helpers.
pub type ApkZipResult<T> = Result<T, ApkZipError>;

/// Errors that can occur while packing or unpacking APK files.
#[derive(Debug)]
pub enum ApkZipError {
    Io(io::Error),
    Zip(zip::result::ZipError),
    InvalidInput(String),
}

impl std::fmt::Display for ApkZipError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ApkZipError::Io(err) => write!(f, "I/O error: {}", err),
            ApkZipError::Zip(err) => write!(f, "ZIP error: {}", err),
            ApkZipError::InvalidInput(msg) => write!(f, "{msg}"),
        }
    }
}

impl std::error::Error for ApkZipError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            ApkZipError::Io(err) => Some(err),
            ApkZipError::Zip(err) => Some(err),
            ApkZipError::InvalidInput(_) => None,
        }
    }
}

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

/// Extract an APK into the provided directory.
///
/// This uses `zip::ZipArchive` under the hood and preserves the directory structure of the
/// original APK. Permissions are best-effort on Unix (the `unix_mode` from the archive).
pub fn unpack_apk(apk_path: impl AsRef<Path>, output_dir: impl AsRef<Path>) -> ApkZipResult<()> {
    let apk_path = apk_path.as_ref();
    let output_dir = output_dir.as_ref();
    if !output_dir.exists() {
        fs::create_dir_all(output_dir)?;
    }

    let file = File::open(apk_path)?;
    let mut archive = ZipArchive::new(file)?;
    for idx in 0..archive.len() {
        let mut entry = archive.by_index(idx)?;
        #[allow(deprecated)]
        let name = entry.sanitized_name();
        let out_path = output_dir.join(name);

        if entry.name().ends_with('/') {
            fs::create_dir_all(&out_path)?;
            continue;
        }

        if let Some(parent) = out_path.parent() {
            fs::create_dir_all(parent)?;
        }

        let mut outfile = File::create(&out_path)?;
        io::copy(&mut entry, &mut outfile)?;

        #[cfg(unix)]
        if let Some(mode) = entry.unix_mode() {
            use std::os::unix::fs::PermissionsExt;
            fs::set_permissions(&out_path, fs::Permissions::from_mode(mode))?;
        }
    }

    Ok(())
}

/// Pack the contents of `input_dir` into an APK placed at `output_apk`.
///
/// The packer follows Android best practices:
/// - `resources.arsc`, `*.dex`, `lib/**/*.so`, and already-compressed media are stored
///   uncompressed so they can be mmapped by the runtime.
/// - Uncompressed entries are zip-aligned to 4 bytes (16 KiB for native libraries) by adjusting
///   the local header's extra field.
/// - Everything else is deflated using the default compression level.
pub fn pack_apk(input_dir: impl AsRef<Path>, output_apk: impl AsRef<Path>) -> ApkZipResult<()> {
    let input_dir = input_dir.as_ref().to_path_buf();
    let output_apk = output_apk.as_ref().to_path_buf();
    if !input_dir.is_dir() {
        return Err(ApkZipError::InvalidInput(format!(
            "input path {} is not a directory",
            input_dir.display()
        )));
    }

    let mut files = Vec::new();
    let mut dirs = BTreeSet::new();
    collect_entries(&input_dir, &input_dir, &mut files, &mut dirs)?;
    files.sort_by(|a, b| a.archive_path.cmp(&b.archive_path));

    let mut buffer = Vec::with_capacity(files.len() * 1024);
    let mut central = Vec::with_capacity(files.len());

    for entry in &files {
        central.push(write_local_entry(&mut buffer, entry)?);
    }

    // Optionally add empty directory entries for deterministic archives.
    for dir in dirs {
        let record = write_directory_entry(&mut buffer, &dir)?;
        central.push(record);
    }

    let central_start = buffer.len() as u32;
    for record in &central {
        write_central_directory_entry(&mut buffer, record);
    }
    let central_size = buffer.len() as u32 - central_start;
    write_end_of_central_directory(&mut buffer, central.len(), central_size, central_start);

    if let Some(parent) = output_apk.parent() {
        if !parent.as_os_str().is_empty() {
            fs::create_dir_all(parent)?;
        }
    }
    fs::write(output_apk, buffer)?;
    Ok(())
}

struct ApkInputEntry {
    archive_path: String,
    data: Vec<u8>,
    permissions: u32,
}

#[derive(Clone, Copy)]
enum ApkCompression {
    Stored,
    Deflated,
}

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
}

fn collect_entries(
    root: &Path,
    current: &Path,
    files: &mut Vec<ApkInputEntry>,
    dirs: &mut BTreeSet<String>,
) -> ApkZipResult<()> {
    for entry in fs::read_dir(current)? {
        let entry = entry?;
        let path = entry.path();
        if entry.file_type()?.is_dir() {
            let rel = relative_path(root, &path)?;
            if !rel.is_empty() {
                dirs.insert(format!("{rel}/"));
            }
            collect_entries(root, &path, files, dirs)?;
            continue;
        }

        if !entry.file_type()?.is_file() {
            continue;
        }

        let rel = relative_path(root, &path)?;
        let data = {
            let mut buf = Vec::new();
            File::open(&path)?.read_to_end(&mut buf)?;
            buf
        };

        let permissions = default_permissions(&entry);
        files.push(ApkInputEntry {
            archive_path: rel,
            data,
            permissions,
        });
    }
    Ok(())
}

fn relative_path(root: &Path, path: &Path) -> ApkZipResult<String> {
    let rel = path.strip_prefix(root).map_err(|_| {
        ApkZipError::InvalidInput(format!(
            "path {} is not under {}",
            path.display(),
            root.display()
        ))
    })?;
    let mut components = Vec::new();
    for part in rel.components() {
        use std::path::Component;
        match part {
            Component::Normal(os_str) => {
                components.push(
                    os_str
                        .to_str()
                        .ok_or_else(|| {
                            ApkZipError::InvalidInput(format!(
                                "non UTF-8 path component {:?}",
                                os_str
                            ))
                        })?
                        .to_string(),
                );
            }
            _ => {}
        }
    }
    Ok(components.join("/"))
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
            .and_then(|m| {
                if m.permissions().readonly() {
                    Ok(0o444)
                } else {
                    Ok(0o644)
                }
            })
            .unwrap_or(0o644)
    }
}

fn write_local_entry(
    buf: &mut Vec<u8>,
    entry: &ApkInputEntry,
) -> ApkZipResult<CentralDirectoryRecord> {
    let plan = classify_entry(&entry.archive_path);
    let file_name = entry.archive_path.as_bytes().to_vec();
    let offset = buf.len() as u32;

    let extra_len = if let Some(align) = plan.alignment {
        alignment_padding(offset, file_name.len(), align) as u16
    } else {
        0
    };

    let (compressed_data, compression_method) = match plan.compression {
        ApkCompression::Stored => (entry.data.clone(), 0u16),
        ApkCompression::Deflated => (
            deflate_bytes(&entry.data)?,
            8u16, // deflate
        ),
    };

    let mut crc = Crc32::new();
    crc.update(&entry.data);
    let crc32 = crc.finalize();

    write_u32(buf, 0x04034b50); // local header signature
    write_u16(buf, 20); // version needed
    write_u16(buf, 0); // flags
    write_u16(buf, compression_method);
    write_u16(buf, 0); // mod time
    write_u16(buf, 0); // mod date
    write_u32(buf, crc32);
    write_u32(
        buf,
        compressed_data.len().try_into().map_err(|_| {
            ApkZipError::InvalidInput(format!(
                "compressed data too large for {}",
                entry.archive_path
            ))
        })?,
    );
    write_u32(
        buf,
        entry.data.len().try_into().map_err(|_| {
            ApkZipError::InvalidInput(format!("file too large for {}", entry.archive_path))
        })?,
    );
    write_u16(buf, file_name.len() as u16);
    write_u16(buf, extra_len);
    buf.extend_from_slice(&file_name);
    if extra_len > 0 {
        buf.extend(std::iter::repeat(0u8).take(extra_len as usize));
    }
    buf.extend_from_slice(&compressed_data);

    Ok(CentralDirectoryRecord {
        file_name,
        compression: plan.compression,
        crc32,
        compressed_size: compressed_data.len() as u32,
        uncompressed_size: entry.data.len() as u32,
        local_header_offset: offset,
        external_attrs: (entry.permissions & 0o777) << 16,
    })
}

fn write_directory_entry(buf: &mut Vec<u8>, dir: &str) -> ApkZipResult<CentralDirectoryRecord> {
    let name = format!("{dir}").into_bytes();
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
    write_u16(buf, name.len() as u16);
    write_u16(buf, 0);
    buf.extend_from_slice(&name);

    Ok(CentralDirectoryRecord {
        file_name: name,
        compression: ApkCompression::Stored,
        crc32: 0,
        compressed_size: 0,
        uncompressed_size: 0,
        local_header_offset: offset,
        external_attrs: (0o755u32) << 16 | 0x10,
    })
}

fn write_central_directory_entry(buf: &mut Vec<u8>, record: &CentralDirectoryRecord) {
    write_u32(buf, 0x02014b50);
    write_u16(buf, 0x031E); // version made by (UNIX, 3.0)
    write_u16(buf, 20); // version needed
    write_u16(buf, 0); // flags
    let method = match record.compression {
        ApkCompression::Stored => 0u16,
        ApkCompression::Deflated => 8u16,
    };
    write_u16(buf, method);
    write_u16(buf, 0); // mod time
    write_u16(buf, 0); // mod date
    write_u32(buf, record.crc32);
    write_u32(buf, record.compressed_size);
    write_u32(buf, record.uncompressed_size);
    write_u16(buf, record.file_name.len() as u16);
    write_u16(buf, 0); // central extra length
    write_u16(buf, 0); // comment length
    write_u16(buf, 0); // disk number start
    write_u16(buf, 0); // internal attrs
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
    write_u16(buf, 0); // comment length
}

fn write_u16(buf: &mut Vec<u8>, value: u16) {
    buf.extend_from_slice(&value.to_le_bytes());
}

fn write_u32(buf: &mut Vec<u8>, value: u32) {
    buf.extend_from_slice(&value.to_le_bytes());
}

fn deflate_bytes(data: &[u8]) -> ApkZipResult<Vec<u8>> {
    let mut encoder = DeflateEncoder::new(Vec::new(), Compression::default());
    encoder.write_all(data)?;
    Ok(encoder.finish()?)
}

fn alignment_padding(offset: u32, name_len: usize, alignment: u32) -> u32 {
    if alignment <= 1 {
        return 0;
    }
    let base = offset as u64 + 30 + name_len as u64;
    let align = alignment as u64;
    ((align - (base % align)) % align) as u32
}

fn classify_entry(path: &str) -> EntryPlan {
    let lower = path.to_ascii_lowercase();
    let store = should_store_uncompressed(&lower);
    let compression = if store {
        ApkCompression::Stored
    } else {
        ApkCompression::Deflated
    };
    let alignment = if store {
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
            Some(ext) if matches!(
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
