#![forbid(unsafe_code)]

//! Authenticated, content-addressed artifact publication and retrieval.

use std::fs::{self, File, OpenOptions};
use std::io::{BufReader, Read, Write};
use std::path::{Component, Path, PathBuf};
use std::sync::atomic::{AtomicU64, Ordering};

use anyhow::{Context, Result, bail, ensure};
use sha2::{Digest, Sha256};

static TEMP_SEQUENCE: AtomicU64 = AtomicU64::new(0);

/// The authenticated identity and runtime locator of a published blob.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct PublishedBlob {
    /// The SHA-256 content identity, including its algorithm prefix.
    pub sha256: String,
    /// The exact byte length.
    pub bytes: u64,
    /// The path relative to the configured artifact-store root.
    pub locator: String,
}

fn digest(bytes: &[u8]) -> String {
    format!("{:x}", Sha256::digest(bytes))
}

fn reject_lexical_escape(path: &Path, label: &str) -> Result<()> {
    if path.is_absolute()
        || path
            .components()
            .any(|component| matches!(component, Component::ParentDir))
    {
        bail!("{label} escapes its configured root: {}", path.display());
    }
    Ok(())
}

fn destination_exists(path: &Path) -> Result<bool> {
    match fs::symlink_metadata(path) {
        Ok(_) => Ok(true),
        Err(error) if error.kind() == std::io::ErrorKind::NotFound => Ok(false),
        Err(error) => Err(error).with_context(|| format!("inspect {}", path.display())),
    }
}

fn secure_parent(root: &Path, relative: &Path) -> Result<PathBuf> {
    reject_lexical_escape(relative, "artifact locator")?;
    fs::create_dir_all(root)?;
    let trusted_root = fs::canonicalize(root)?;
    let parent = relative.parent().context("artifact path has no parent")?;
    let mut current = root.to_path_buf();
    for component in parent.components() {
        current.push(component.as_os_str());
        match fs::symlink_metadata(&current) {
            Ok(metadata) if metadata.file_type().is_symlink() => {
                bail!(
                    "artifact ancestor must not be a symlink: {}",
                    current.display()
                );
            }
            Ok(metadata) if !metadata.is_dir() => {
                bail!(
                    "artifact ancestor must be a directory: {}",
                    current.display()
                );
            }
            Ok(_) => {}
            Err(error) if error.kind() == std::io::ErrorKind::NotFound => {
                match fs::create_dir(&current) {
                    Ok(()) => {}
                    Err(error) if error.kind() == std::io::ErrorKind::AlreadyExists => {}
                    Err(error) => return Err(error.into()),
                }
            }
            Err(error) => return Err(error.into()),
        }
        if !fs::canonicalize(&current)?.starts_with(&trusted_root) {
            bail!(
                "artifact locator escapes trusted store root: {}",
                relative.display()
            );
        }
    }
    Ok(root.join(relative))
}

fn secure_existing_path(root: &Path, relative: &Path) -> Result<PathBuf> {
    reject_lexical_escape(relative, "artifact locator")?;
    let trusted_root = fs::canonicalize(root)?;
    let mut current = root.to_path_buf();
    let components = relative.components().collect::<Vec<_>>();
    for (index, component) in components.iter().enumerate() {
        current.push(component.as_os_str());
        let metadata = fs::symlink_metadata(&current)?;
        ensure!(
            !metadata.file_type().is_symlink(),
            "artifact path must not contain a symlink: {}",
            current.display()
        );
        if index + 1 == components.len() {
            ensure!(
                metadata.is_file(),
                "artifact destination is not a regular file: {}",
                current.display()
            );
        } else {
            ensure!(
                metadata.is_dir(),
                "artifact ancestor must be a directory: {}",
                current.display()
            );
        }
        ensure!(
            fs::canonicalize(&current)?.starts_with(&trusted_root),
            "artifact locator escapes trusted store root: {}",
            relative.display()
        );
    }
    Ok(current)
}

fn verify_destination(path: &Path, bytes: &[u8]) -> Result<()> {
    let metadata = fs::symlink_metadata(path)?;
    if metadata.file_type().is_symlink() || !metadata.is_file() {
        bail!(
            "artifact destination is not a regular file: {}",
            path.display()
        );
    }
    let actual = fs::read(path)?;
    if actual != bytes || digest(&actual) != digest(bytes) {
        bail!("content-address collision at {}", path.display());
    }
    Ok(())
}

/// Authenticates and returns a blob named by a root-relative runtime locator.
pub fn authenticate_blob(
    root: &Path,
    locator: &str,
    expected_sha256: &str,
    expected_bytes: u64,
) -> Result<Vec<u8>> {
    let relative = Path::new(locator);
    reject_lexical_escape(relative, "artifact locator")?;
    let path = secure_existing_path(root, relative)?;
    let mut reader = BufReader::new(File::open(&path)?);
    let mut hasher = Sha256::new();
    let mut bytes = Vec::new();
    let mut buffer = [0_u8; 64 * 1024];
    let mut length = 0_u64;
    loop {
        let read = reader.read(&mut buffer)?;
        if read == 0 {
            break;
        }
        hasher.update(&buffer[..read]);
        bytes.extend_from_slice(&buffer[..read]);
        length = length
            .checked_add(read as u64)
            .context("artifact byte length overflow")?;
    }
    ensure!(length == expected_bytes, "artifact byte length mismatch");
    ensure!(
        format!("sha256:{:x}", hasher.finalize()) == expected_sha256,
        "artifact hash mismatch"
    );
    Ok(bytes)
}

/// Publishes bytes once at their SHA-256-derived content address.
pub fn publish_blob(root: &Path, extension: &str, bytes: &[u8]) -> Result<PublishedBlob> {
    ensure!(
        !extension.is_empty() && extension.bytes().all(|byte| byte.is_ascii_alphanumeric()),
        "artifact extension must be non-empty ASCII alphanumeric"
    );
    let hash = digest(bytes);
    let locator = format!("sha256/{}/{}.{}", &hash[..2], hash, extension);
    let path = secure_parent(root, Path::new(&locator))?;
    if destination_exists(&path)? {
        verify_destination(&path, bytes)?;
    } else {
        let parent = path.parent().context("artifact path has no parent")?;
        let name = path
            .file_name()
            .context("artifact path has no file name")?
            .to_string_lossy();
        let temp = parent.join(format!(
            ".{name}.tmp-{}-{}",
            std::process::id(),
            TEMP_SEQUENCE.fetch_add(1, Ordering::Relaxed)
        ));
        let mut file = OpenOptions::new()
            .write(true)
            .create_new(true)
            .open(&temp)?;
        file.write_all(bytes)?;
        file.sync_all()?;
        drop(file);
        match fs::hard_link(&temp, &path) {
            Ok(()) => {}
            Err(error) if error.kind() == std::io::ErrorKind::AlreadyExists => {}
            Err(error) => {
                let _ = fs::remove_file(&temp);
                return Err(error).with_context(|| format!("publish {}", path.display()));
            }
        }
        fs::remove_file(&temp)?;
        verify_destination(&path, bytes)?;
    }
    Ok(PublishedBlob {
        sha256: format!("sha256:{hash}"),
        bytes: bytes.len() as u64,
        locator,
    })
}

/// Atomically replaces a summary file with complete bytes.
pub fn write_atomic_summary(path: &Path, bytes: &[u8]) -> Result<()> {
    let parent = path.parent().context("output has no parent directory")?;
    fs::create_dir_all(parent)?;
    let name = path
        .file_name()
        .context("output has no file name")?
        .to_string_lossy();
    let temp = parent.join(format!(
        ".{name}.tmp-{}-{}",
        std::process::id(),
        TEMP_SEQUENCE.fetch_add(1, Ordering::Relaxed)
    ));
    let mut file = OpenOptions::new()
        .write(true)
        .create_new(true)
        .open(&temp)?;
    file.write_all(bytes)?;
    file.sync_all()?;
    drop(file);
    if let Err(error) = fs::rename(&temp, path) {
        let _ = fs::remove_file(&temp);
        return Err(error).with_context(|| format!("replace {}", path.display()));
    }
    Ok(())
}
