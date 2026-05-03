//! Content-addressed parser-output cache.
//!
//! Layout: `<root>/<parser_id>/<adapter_sha>/<input_sha>.json`
//!
//! - `parser_id` identifies the adapter binary.
//! - `adapter_sha` is the deterministic hash of every regular file under the
//!   adapter source directory — recomputed once at startup.
//! - `input_sha` is the sha256 of the raw work source bytes as ingested.
//!
//! A code change in adapter sources moves entries into a new `<adapter_sha>`
//! directory, so stale results are unreachable by construction.

use std::{
    fs,
    path::{Path, PathBuf},
};

use anyhow::{Context, Result};
use sha2::{Digest, Sha256};
use walkdir::WalkDir;

/// Roots of source files that determine `adapter_sha`.
#[derive(Debug, Clone)]
pub struct AdapterFingerprintInputs {
    pub parser_id: String,
    pub source_roots: Vec<PathBuf>,
    pub exclude_globs: Vec<String>,
}

impl AdapterFingerprintInputs {
    pub fn for_parser(repo_root: &Path, parser_id: &str) -> Result<Self> {
        let adapter_dir = match parser_id {
            "aozora2" => repo_root.join("adapters/aozora2"),
            "aozora-rs" => repo_root.join("adapters/aozora-rs"),
            "aozora2html" => repo_root.join("adapters/aozora2html"),
            other => anyhow::bail!("unknown parser id: {other}"),
        };
        Ok(Self {
            parser_id: parser_id.to_string(),
            source_roots: vec![adapter_dir],
            exclude_globs: vec![
                "target".to_string(),
                "node_modules".to_string(),
                "__pycache__".to_string(),
                ".git".to_string(),
            ],
        })
    }
}

pub fn compute_adapter_sha(inputs: &AdapterFingerprintInputs) -> Result<String> {
    let mut entries: Vec<(PathBuf, Vec<u8>)> = Vec::new();
    for root in &inputs.source_roots {
        if !root.exists() {
            continue;
        }
        for entry in WalkDir::new(root).follow_links(false).into_iter() {
            let entry = entry?;
            if !entry.file_type().is_file() {
                continue;
            }
            let path = entry.path();
            let rel = match path.strip_prefix(root) {
                Ok(rel) => rel.to_path_buf(),
                Err(_) => continue,
            };
            if rel.components().any(|c| {
                inputs
                    .exclude_globs
                    .contains(&c.as_os_str().to_string_lossy().to_string())
            }) {
                continue;
            }
            let bytes = fs::read(path)
                .with_context(|| format!("read fingerprint input {}", path.display()))?;
            // Path component for hash uses POSIX separators and is rooted at the
            // source-root name so renames across roots invalidate.
            let mut keyed = String::new();
            keyed.push_str(
                &root
                    .file_name()
                    .map(|s| s.to_string_lossy().into_owned())
                    .unwrap_or_default(),
            );
            keyed.push('/');
            keyed.push_str(
                &rel.to_string_lossy()
                    .replace(std::path::MAIN_SEPARATOR, "/"),
            );
            entries.push((PathBuf::from(keyed), bytes));
        }
    }
    entries.sort_by(|a, b| a.0.cmp(&b.0));

    let mut outer = Sha256::new();
    for (rel, bytes) in &entries {
        let path_hash = Sha256::digest(rel.to_string_lossy().as_bytes());
        let content_hash = Sha256::digest(bytes);
        outer.update(path_hash);
        outer.update(content_hash);
    }
    Ok(hex(&outer.finalize()))
}

pub fn input_sha(bytes: &[u8]) -> String {
    hex(&Sha256::digest(bytes))
}

fn hex(bytes: &[u8]) -> String {
    let mut s = String::with_capacity(bytes.len() * 2);
    for b in bytes {
        use std::fmt::Write;
        let _ = write!(s, "{:02x}", b);
    }
    s
}

#[derive(Debug, Clone)]
pub struct ParserCache {
    root: PathBuf,
}

impl ParserCache {
    pub fn new(root: impl AsRef<Path>) -> Self {
        Self {
            root: root.as_ref().to_path_buf(),
        }
    }

    pub fn entry_path(&self, parser_id: &str, adapter_sha: &str, input_sha: &str) -> PathBuf {
        self.root
            .join(parser_id)
            .join(adapter_sha)
            .join(format!("{input_sha}.json"))
    }

    pub fn read(
        &self,
        parser_id: &str,
        adapter_sha: &str,
        input_sha: &str,
    ) -> Result<Option<Vec<u8>>> {
        let path = self.entry_path(parser_id, adapter_sha, input_sha);
        match fs::read(&path) {
            Ok(bytes) => Ok(Some(bytes)),
            Err(err) if err.kind() == std::io::ErrorKind::NotFound => Ok(None),
            Err(err) => Err(anyhow::Error::new(err))
                .with_context(|| format!("read cache entry {}", path.display())),
        }
    }

    pub fn write(
        &self,
        parser_id: &str,
        adapter_sha: &str,
        input_sha: &str,
        bytes: &[u8],
    ) -> Result<()> {
        let path = self.entry_path(parser_id, adapter_sha, input_sha);
        if let Some(parent) = path.parent() {
            fs::create_dir_all(parent).with_context(|| format!("mkdir {}", parent.display()))?;
        }
        fs::write(&path, bytes).with_context(|| format!("write {}", path.display()))?;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn deterministic_sha_two_files() {
        let dir = tempdir();
        std::fs::write(dir.join("a.txt"), b"hello").unwrap();
        std::fs::write(dir.join("b.txt"), b"world").unwrap();
        let inputs = AdapterFingerprintInputs {
            parser_id: "test".into(),
            source_roots: vec![dir.clone()],
            exclude_globs: vec![],
        };
        let s1 = compute_adapter_sha(&inputs).unwrap();
        let s2 = compute_adapter_sha(&inputs).unwrap();
        assert_eq!(s1, s2);
        assert_eq!(s1.len(), 64);
    }

    #[test]
    fn content_change_invalidates_sha() {
        let dir = tempdir();
        std::fs::write(dir.join("a.txt"), b"hello").unwrap();
        let inputs = AdapterFingerprintInputs {
            parser_id: "test".into(),
            source_roots: vec![dir.clone()],
            exclude_globs: vec![],
        };
        let s1 = compute_adapter_sha(&inputs).unwrap();
        std::fs::write(dir.join("a.txt"), b"hello!").unwrap();
        let s2 = compute_adapter_sha(&inputs).unwrap();
        assert_ne!(s1, s2);
    }

    #[test]
    fn parser_fingerprint_inputs_do_not_depend_on_reference_checkouts() {
        let repo = tempdir();

        for parser_id in ["aozora2", "aozora-rs", "aozora2html"] {
            let inputs = AdapterFingerprintInputs::for_parser(&repo, parser_id).unwrap();

            assert_eq!(inputs.source_roots.len(), 1);
            assert_eq!(
                inputs.source_roots[0],
                repo.join("adapters").join(parser_id)
            );
        }
    }

    fn tempdir() -> PathBuf {
        let p = std::env::temp_dir().join(format!(
            "ab-coverage-cache-test-{}-{}",
            std::process::id(),
            rand_suffix()
        ));
        let _ = std::fs::remove_dir_all(&p);
        std::fs::create_dir_all(&p).unwrap();
        p
    }

    fn rand_suffix() -> u64 {
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap()
            .as_nanos() as u64
    }
}
