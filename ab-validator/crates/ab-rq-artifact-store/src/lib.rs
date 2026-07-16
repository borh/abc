#![forbid(unsafe_code)]

//! Authenticated, content-addressed artifact publication and retrieval.
//!
//! The store is a single-owner, immutable-topology store: callers must prevent
//! other actors from replacing path components while an operation is in
//! progress. The component checks reject symlinks present when inspected, but
//! do not claim confinement against hostile concurrent directory mutation.
//!
//! Publication and summary replacement are atomically visible to concurrent
//! readers after success. They sync file contents, but not containing
//! directories, so success is not a power-loss durability guarantee. Summary
//! replacement installs a newly created file and therefore does not preserve
//! the previous file's mode or ownership.

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

/// A closed classification of blob-authentication failures.
///
/// Variants separate locator authority, byte retrieval, and content identity
/// so persisted evidence need not infer protocol meaning from error text.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum AuthenticateErrorKind {
    /// The locator is absolute or contains a parent-directory component.
    LocatorInvalid,
    /// The locator cannot be resolved to a regular, symlink-free store member.
    LocatorUnavailable,
    /// A resolved member could not be read completely.
    ReadFailed,
    /// Retrieved bytes disagree with the asserted length or SHA-256 identity.
    BlobMismatch,
}

/// A typed blob-authentication failure.
#[derive(Debug)]
pub struct AuthenticateError {
    kind: AuthenticateErrorKind,
    source: anyhow::Error,
}

impl AuthenticateError {
    /// Returns the stable protocol-level failure classification.
    pub fn kind(&self) -> AuthenticateErrorKind {
        self.kind
    }

    fn new(kind: AuthenticateErrorKind, source: impl Into<anyhow::Error>) -> Self {
        Self {
            kind,
            source: source.into(),
        }
    }
}

impl std::fmt::Display for AuthenticateError {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.source.fmt(formatter)
    }
}

impl std::error::Error for AuthenticateError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        self.source.source()
    }
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
    secure_parent_with_create_hook(root, relative, |_| {})
}

fn secure_parent_with_create_hook(
    root: &Path,
    relative: &Path,
    mut before_create: impl FnMut(&Path),
) -> Result<PathBuf> {
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
                before_create(&current);
                match fs::create_dir(&current) {
                    Ok(()) => {}
                    Err(error) if error.kind() == std::io::ErrorKind::AlreadyExists => {
                        let metadata = fs::symlink_metadata(&current)?;
                        ensure!(
                            !metadata.file_type().is_symlink() && metadata.is_dir(),
                            "artifact ancestor created concurrently must be a directory: {}",
                            current.display()
                        );
                    }
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
///
/// The store topology must remain under one trusted owner's control for the
/// duration of this call; see the module-level concurrency contract.
pub fn authenticate_blob(
    root: &Path,
    locator: &str,
    expected_sha256: &str,
    expected_bytes: u64,
) -> std::result::Result<Vec<u8>, AuthenticateError> {
    let relative = Path::new(locator);
    reject_lexical_escape(relative, "artifact locator")
        .map_err(|error| AuthenticateError::new(AuthenticateErrorKind::LocatorInvalid, error))?;
    let path = secure_existing_path(root, relative).map_err(|error| {
        AuthenticateError::new(AuthenticateErrorKind::LocatorUnavailable, error)
    })?;
    let mut reader = BufReader::new(
        File::open(&path)
            .map_err(|error| AuthenticateError::new(AuthenticateErrorKind::ReadFailed, error))?,
    );
    let mut hasher = Sha256::new();
    let mut bytes = Vec::new();
    let mut buffer = [0_u8; 64 * 1024];
    let mut length = 0_u64;
    loop {
        let read = reader
            .read(&mut buffer)
            .map_err(|error| AuthenticateError::new(AuthenticateErrorKind::ReadFailed, error))?;
        if read == 0 {
            break;
        }
        hasher.update(&buffer[..read]);
        bytes.extend_from_slice(&buffer[..read]);
        length = length.checked_add(read as u64).ok_or_else(|| {
            AuthenticateError::new(
                AuthenticateErrorKind::ReadFailed,
                anyhow::anyhow!("artifact byte length overflow"),
            )
        })?;
    }
    if length != expected_bytes {
        return Err(AuthenticateError::new(
            AuthenticateErrorKind::BlobMismatch,
            anyhow::anyhow!("artifact byte length mismatch"),
        ));
    }
    if format!("sha256:{:x}", hasher.finalize()) != expected_sha256 {
        return Err(AuthenticateError::new(
            AuthenticateErrorKind::BlobMismatch,
            anyhow::anyhow!("artifact hash mismatch"),
        ));
    }
    Ok(bytes)
}

/// Publishes bytes once at their SHA-256-derived content address.
///
/// The store topology must remain under one trusted owner's control for the
/// duration of this call. Success is visibility-atomic, not crash-durable.
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
///
/// Success is visibility-atomic, not crash-durable. Replacement uses a new
/// file whose permissions follow creation defaults rather than preserving the
/// replaced file's metadata.
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

#[cfg(all(test, unix))]
mod tests {
    use std::os::unix::fs::symlink;

    use tempfile::tempdir;

    use super::secure_parent_with_create_hook;

    #[test]
    fn concurrently_created_symlink_is_reinspected_after_already_exists() {
        let root = tempdir().unwrap();
        let outside = tempdir().unwrap();
        let mut injected = false;
        let result = secure_parent_with_create_hook(
            root.path(),
            std::path::Path::new("sha256/aa/blob.json"),
            |path| {
                if !injected && path == root.path().join("sha256") {
                    symlink(outside.path(), path).unwrap();
                    injected = true;
                }
            },
        );
        assert!(result.is_err());
        assert!(
            result
                .unwrap_err()
                .to_string()
                .contains("created concurrently must be a directory")
        );
    }
}
