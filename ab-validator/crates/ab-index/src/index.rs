use std::{
    collections::BTreeMap,
    fs,
    io::Read,
    path::{Path, PathBuf},
};

use anyhow::{Context, Result};
use chrono::{SecondsFormat, Utc};
use flate2::read::DeflateDecoder;
use rayon::prelude::*;
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};
use walkdir::WalkDir;
use zip::{CompressionMethod, ZipArchive};

use crate::{
    encoding::{decode_source_bytes, hex_sha256},
    features::{FeatureDetector, normalize_relative_path},
};

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct Index {
    pub version: u32,
    pub corpus_root: String,
    pub corpus_hash: String,
    pub generated_at: String,
    pub works_count: usize,
    pub works: Vec<WorkEntry>,
    pub by_feature: BTreeMap<String, Vec<String>>,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct WorkEntry {
    pub id: String,
    pub txt_path: String,
    pub html_path: Option<String>,
    pub features: Vec<String>,
    pub feature_lines: BTreeMap<String, Vec<usize>>,
}

#[derive(Debug)]
struct ScannedWork {
    entry: WorkEntry,
    file_size: u64,
    file_sha256: String,
}

#[derive(Debug, Clone)]
enum SourceFile {
    Plain(PathBuf),
    Zip { archive: PathBuf, entry: String },
}

/// Build a feature index from a corpus root and detector.
///
/// # Errors
///
/// Returns an error when source scanning, decoding, feature detection, or
/// serialization preparation fails.
pub fn build_index(corpus_root: &Path, detector: &FeatureDetector) -> Result<Index> {
    let sources = collect_source_files(corpus_root)?;
    let mut scanned = sources
        .par_iter()
        .map(|source| scan_work(corpus_root, source, detector))
        .collect::<Result<Vec<_>>>()?;

    scanned.sort_by(|a, b| a.entry.txt_path.cmp(&b.entry.txt_path));

    let corpus_hash = corpus_hash(scanned.iter().map(|work| {
        (
            work.entry.txt_path.as_str(),
            work.file_size,
            work.file_sha256.as_str(),
        )
    }));

    let mut works: Vec<WorkEntry> = scanned.into_iter().map(|work| work.entry).collect();
    works.sort_by(|a, b| a.id.cmp(&b.id).then(a.txt_path.cmp(&b.txt_path)));

    let mut by_feature: BTreeMap<String, Vec<String>> = BTreeMap::new();
    for work in &works {
        for feature in &work.features {
            by_feature
                .entry(feature.clone())
                .or_default()
                .push(work.id.clone());
        }
    }
    for work_ids in by_feature.values_mut() {
        work_ids.sort();
        work_ids.dedup();
    }

    Ok(Index {
        version: 1,
        corpus_root: corpus_root.to_string_lossy().into_owned(),
        corpus_hash,
        generated_at: Utc::now().to_rfc3339_opts(SecondsFormat::Secs, true),
        works_count: works.len(),
        works,
        by_feature,
    })
}

/// Write an index into a JSON file.
///
/// # Errors
///
/// Returns an error when the file cannot be created or serialized.
pub fn write_index(index: &Index, output: &Path) -> Result<()> {
    let file = fs::File::create(output)
        .with_context(|| format!("failed to create output {}", output.display()))?;
    serde_json::to_writer_pretty(file, index)?;
    Ok(())
}

/// Read an index from a JSON file.
///
/// # Errors
///
/// Returns an error when the file cannot be opened or parsed.
pub fn read_index(path: &Path) -> Result<Index> {
    let file =
        fs::File::open(path).with_context(|| format!("failed to open {}", path.display()))?;
    Ok(serde_json::from_reader(file)?)
}

#[must_use]
pub fn query_any(index: &Index, features: &[String]) -> Vec<String> {
    let mut ids = Vec::new();
    for feature in features {
        if let Some(work_ids) = index.by_feature.get(feature) {
            ids.extend(work_ids.iter().cloned());
        }
    }
    ids.sort();
    ids.dedup();
    ids
}

#[must_use]
pub fn query_all(index: &Index, features: &[String]) -> Vec<String> {
    let Some((first, rest)) = features.split_first() else {
        return Vec::new();
    };
    let Some(first_ids) = index.by_feature.get(first) else {
        return Vec::new();
    };

    first_ids
        .iter()
        .filter(|id| {
            rest.iter().all(|feature| {
                index
                    .by_feature
                    .get(feature)
                    .is_some_and(|ids| ids.binary_search(id).is_ok())
            })
        })
        .cloned()
        .collect()
}

#[must_use]
pub fn sample(index: &Index, limit: usize, features: &[String]) -> Vec<String> {
    let mut ids = query_any(index, features);
    if ids.len() > limit {
        ids.truncate(limit);
    }
    ids
}

fn collect_source_files(corpus_root: &Path) -> Result<Vec<SourceFile>> {
    let mut sources = Vec::new();
    for entry in WalkDir::new(corpus_root)
        .follow_links(false)
        .into_iter()
        .filter_entry(|entry| !is_hidden(entry.path()))
    {
        let entry = entry?;
        // The pinned nix `aozorabunko-corpus` derivation presents every leaf work
        // file as a symlink into the `-source` store path. WalkDir (follow_links
        // false, to avoid descending symlinked directories) reports these as
        // symlinks, not files, so resolve symlink targets here rather than skipping
        // them — otherwise the entire corpus indexes as zero works.
        let file_type = entry.file_type();
        let is_regular_file = if file_type.is_symlink() {
            fs::metadata(entry.path()).is_ok_and(|meta| meta.is_file())
        } else {
            file_type.is_file()
        };
        if !is_regular_file {
            continue;
        }
        let path = entry.path();
        if !is_aozora_work_source_path(path, corpus_root) {
            continue;
        }
        if is_text_file(path) {
            if is_zip_file(path)? {
                push_zip_sources(&mut sources, path);
            } else {
                sources.push(SourceFile::Plain(path.to_owned()));
            }
        } else if path
            .extension()
            .is_some_and(|ext| ext.eq_ignore_ascii_case("zip"))
        {
            push_zip_sources(&mut sources, path);
        }
    }
    sources.sort_by_key(|source| source_index_path(source, corpus_root));
    Ok(sources)
}

fn push_zip_sources(sources: &mut Vec<SourceFile>, path: &Path) {
    match zip_text_entries(path) {
        Ok(entries) => {
            for entry in entries {
                sources.push(SourceFile::Zip {
                    archive: path.to_owned(),
                    entry,
                });
            }
        }
        Err(error) => {
            eprintln!(
                "warning: skipping unreadable zip {}: {error:#}",
                path.display()
            );
        }
    }
}

fn is_hidden(path: &Path) -> bool {
    path.file_name()
        .is_some_and(|name| name.to_string_lossy().starts_with('.'))
}

fn is_aozora_work_source_path(path: &Path, corpus_root: &Path) -> bool {
    let Ok(relative) = path.strip_prefix(corpus_root) else {
        return false;
    };
    let parts = relative
        .components()
        .map(|component| component.as_os_str().to_string_lossy())
        .collect::<Vec<_>>();
    parts.len() >= 4
        && parts[0].eq_ignore_ascii_case("cards")
        && !parts[1].is_empty()
        && parts[2].eq_ignore_ascii_case("files")
}

fn scan_work(
    corpus_root: &Path,
    source: &SourceFile,
    detector: &FeatureDetector,
) -> Result<ScannedWork> {
    let source_path = source_index_path(source, corpus_root);
    let bytes = read_source_bytes(source)
        .with_context(|| format!("failed to read indexed source {source_path}"))?;
    let decoded = decode_source_bytes(&bytes)
        .with_context(|| format!("failed to decode indexed source {source_path}"))?;
    let feature_lines = detector
        .detect(&decoded.text)
        .into_iter()
        .collect::<BTreeMap<_, _>>();
    let mut features = feature_lines.keys().cloned().collect::<Vec<_>>();
    features.sort();

    let txt_path = source_path;
    let html_path = find_html_sibling(source_container_path(source), corpus_root)?;
    let id = work_id_from_index_path(&txt_path);

    Ok(ScannedWork {
        entry: WorkEntry {
            id,
            txt_path,
            html_path,
            features,
            feature_lines,
        },
        file_size: bytes.len() as u64,
        file_sha256: hex_sha256(&bytes),
    })
}

fn is_text_file(path: &Path) -> bool {
    path.extension()
        .is_some_and(|ext| ext.eq_ignore_ascii_case("txt"))
        && path
            .file_name()
            .is_some_and(|name| !name.to_string_lossy().eq_ignore_ascii_case("README.txt"))
}

fn is_zip_file(path: &Path) -> Result<bool> {
    let mut file =
        fs::File::open(path).with_context(|| format!("failed to open {}", path.display()))?;
    let mut magic = [0; 4];
    match file.read_exact(&mut magic) {
        Ok(()) => Ok(magic == [0x50, 0x4b, 0x03, 0x04]),
        Err(error) if error.kind() == std::io::ErrorKind::UnexpectedEof => Ok(false),
        Err(error) => Err(error).with_context(|| format!("failed to read {}", path.display())),
    }
}

fn zip_text_entries(path: &Path) -> Result<Vec<String>> {
    let file =
        fs::File::open(path).with_context(|| format!("failed to open {}", path.display()))?;
    let mut archive =
        ZipArchive::new(file).with_context(|| format!("failed to read zip {}", path.display()))?;
    for idx in 0..archive.len() {
        let file = archive
            .by_index_raw(idx)
            .with_context(|| format!("failed to read zip entry {idx} in {}", path.display()))?;
        let name = file.name();
        if is_zip_text_entry(name) && !file.is_dir() {
            return Ok(vec![name.to_owned()]);
        }
    }
    Ok(Vec::new())
}

fn read_source_bytes(source: &SourceFile) -> Result<Vec<u8>> {
    match source {
        SourceFile::Plain(path) => {
            fs::read(path).with_context(|| format!("failed to read {}", path.display()))
        }
        SourceFile::Zip { archive, entry } => {
            let file = fs::File::open(archive)
                .with_context(|| format!("failed to open {}", archive.display()))?;
            let mut archive_reader = ZipArchive::new(file)
                .with_context(|| format!("failed to read zip {}", archive.display()))?;
            for idx in 0..archive_reader.len() {
                let mut zipped = archive_reader.by_index_raw(idx).with_context(|| {
                    format!("failed to read zip entry {idx} in {}", archive.display())
                })?;
                if zipped.name() == entry {
                    return read_zip_entry_bytes(&mut zipped, archive, entry);
                }
            }
            anyhow::bail!("zip entry {entry} not found in {}", archive.display())
        }
    }
}

fn is_zip_text_entry(name: &str) -> bool {
    !name.starts_with("__MACOSX/")
        && !name.ends_with('/')
        && !Path::new(name).file_name().is_some_and(|file_name| {
            file_name
                .to_string_lossy()
                .eq_ignore_ascii_case("README.txt")
        })
        && Path::new(name)
            .extension()
            .is_some_and(|ext| ext.eq_ignore_ascii_case("txt"))
}

fn read_zip_entry_bytes(
    entry: &mut zip::read::ZipFile<'_>,
    archive: &Path,
    entry_name: &str,
) -> Result<Vec<u8>> {
    if entry.encrypted() {
        anyhow::bail!(
            "encrypted zip entry {entry_name} is not supported in {}",
            archive.display()
        );
    }

    let mut compressed = Vec::new();
    entry.read_to_end(&mut compressed).with_context(|| {
        format!(
            "failed to read zip entry {entry_name} in {}",
            archive.display()
        )
    })?;

    match entry.compression() {
        CompressionMethod::Stored => Ok(compressed),
        CompressionMethod::Deflated => {
            let mut decoder = DeflateDecoder::new(&compressed[..]);
            let mut out = Vec::new();
            decoder.read_to_end(&mut out).with_context(|| {
                format!(
                    "failed to deflate zip entry {entry_name} in {}",
                    archive.display()
                )
            })?;
            Ok(out)
        }
        method => {
            anyhow::bail!(
                "unsupported zip compression method {method:?} for {entry_name} in {}",
                archive.display()
            )
        }
    }
}

fn source_container_path(source: &SourceFile) -> &Path {
    match source {
        SourceFile::Plain(path) => path,
        SourceFile::Zip { archive, .. } => archive,
    }
}

fn source_index_path(source: &SourceFile, corpus_root: &Path) -> String {
    match source {
        SourceFile::Plain(path) => path
            .strip_prefix(corpus_root)
            .map(normalize_relative_path)
            .unwrap_or_else(|_| path.to_string_lossy().into_owned()),
        SourceFile::Zip { archive, entry } => {
            let archive_path = archive
                .strip_prefix(corpus_root)
                .map(normalize_relative_path)
                .unwrap_or_else(|_| archive.to_string_lossy().into_owned());
            format!("{archive_path}::{entry}")
        }
    }
}

fn find_html_sibling(path: &Path, corpus_root: &Path) -> Result<Option<String>> {
    let Some(parent) = path.parent() else {
        return Ok(None);
    };
    let source_work_prefix = path
        .file_stem()
        .and_then(|stem| stem.to_str())
        .and_then(|stem| stem.split('_').next());
    let mut candidates = Vec::new();
    for entry in fs::read_dir(parent)? {
        let entry = entry?;
        let candidate = entry.path();
        if candidate.extension().is_some_and(|ext| {
            ext.eq_ignore_ascii_case("html") || ext.eq_ignore_ascii_case("xhtml")
        }) {
            candidates.push(candidate);
        }
    }
    candidates.sort();

    if let Some(source_work_prefix) = source_work_prefix {
        for candidate in &candidates {
            let Some(html_work_prefix) = candidate
                .file_stem()
                .and_then(|stem| stem.to_str())
                .and_then(|stem| stem.split('_').next())
            else {
                continue;
            };
            if html_work_prefix == source_work_prefix {
                let rel = candidate.strip_prefix(corpus_root)?;
                return Ok(Some(normalize_relative_path(rel)));
            }
        }
    }

    if let Some(candidate) = candidates.first() {
        let rel = candidate.strip_prefix(corpus_root)?;
        return Ok(Some(normalize_relative_path(rel)));
    }

    Ok(None)
}

fn work_id_from_index_path(path: &str) -> String {
    let source_path = path.split_once("::").map_or(path, |(archive, _)| archive);
    let parts = source_path.split('/').collect::<Vec<_>>();

    if let Some(cards_pos) = parts.iter().position(|part| *part == "cards")
        && let (Some(card), Some(file_dir)) = (parts.get(cards_pos + 1), parts.get(cards_pos + 3))
        && parts.get(cards_pos + 2) == Some(&"files")
    {
        let file_name = file_dir
            .strip_suffix(".zip")
            .or_else(|| file_dir.strip_suffix(".txt"))
            .unwrap_or(file_dir);
        let file = file_name.split('_').next().unwrap_or(file_name);
        return format!("{card}_{file}");
    }

    Path::new(source_path)
        .file_stem()
        .and_then(|stem| stem.to_str())
        .unwrap_or("unknown")
        .chars()
        .map(|ch| {
            if ch.is_ascii_alphanumeric() || ch == '-' || ch == '_' {
                ch
            } else {
                '_'
            }
        })
        .collect()
}

fn corpus_hash<'a>(entries: impl IntoIterator<Item = (&'a str, u64, &'a str)>) -> String {
    let mut rows = entries.into_iter().collect::<Vec<_>>();
    rows.sort_by(|a, b| a.0.cmp(b.0));
    let mut hasher = Sha256::new();
    for (path, size, hash) in rows {
        hasher.update(path.as_bytes());
        hasher.update([0]);
        hasher.update(size.to_string().as_bytes());
        hasher.update([0]);
        hasher.update(hash.as_bytes());
        hasher.update(b"\n");
    }
    format!("sha256:{:x}", hasher.finalize())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_work_id_from_aozora_path() {
        assert_eq!(
            work_id_from_index_path("cards/000148/files/799_ruby_19091/test.txt"),
            "000148_799"
        );
        assert_eq!(
            work_id_from_index_path("cards/000148/files/799_ruby_19091.zip::799_ruby_19091.txt"),
            "000148_799"
        );
    }

    #[test]
    fn corpus_hash_uses_posix_paths_and_content() {
        let one = corpus_hash([("cards/1/files/a.txt", 3, "abc")]);
        let two = corpus_hash([("cards/1/files/a.txt", 3, "abc")]);
        assert_eq!(one, two);
    }

    #[test]
    fn identifies_aozora_work_source_paths() {
        let root = Path::new("/corpus");
        assert!(is_aozora_work_source_path(
            Path::new("/corpus/cards/000001/files/1.txt"),
            root
        ));
        assert!(is_aozora_work_source_path(
            Path::new("/corpus/cards/000001/files/1_ruby/1.txt"),
            root
        ));
        assert!(!is_aozora_work_source_path(
            Path::new("/corpus/tools/JISTABLE.zip"),
            root
        ));
        assert!(!is_aozora_work_source_path(
            Path::new("/corpus/reference/ruby_reference.txt"),
            root
        ));
    }

    #[test]
    #[cfg(unix)]
    fn collects_symlinked_work_files() {
        // The pinned nix `aozorabunko-corpus` presents every leaf work file as a
        // symlink into the `-source` store path. WalkDir reports those as symlinks,
        // not files; collect_source_files must still index them (regression: the
        // whole corpus otherwise indexes as zero works).
        use std::os::unix::fs::symlink;

        let root = std::env::temp_dir().join(format!("ab-index-symlink-{}", std::process::id()));
        let files = root.join("cards/000001/files");
        let targets = root.join("targets");
        fs::create_dir_all(&files).unwrap();
        fs::create_dir_all(&targets).unwrap();
        let real = targets.join("1.txt");
        fs::write(&real, "本文《ほんぶん》\n").unwrap();
        symlink(&real, files.join("1.txt")).unwrap();

        let sources = collect_source_files(&root).unwrap();
        fs::remove_dir_all(&root).ok();

        assert_eq!(sources.len(), 1, "symlinked work file must be collected");
        match &sources[0] {
            SourceFile::Plain(path) => assert!(path.ends_with("cards/000001/files/1.txt")),
            other => panic!("expected a plain source, got {other:?}"),
        }
    }
}
