use std::{
    collections::BTreeMap,
    fs,
    path::{Path, PathBuf},
};

use anyhow::{Context, Result};
use chrono::{SecondsFormat, Utc};
use rayon::prelude::*;
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};
use walkdir::WalkDir;

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

pub fn build_index(corpus_root: &Path, detector: &FeatureDetector) -> Result<Index> {
    let txt_files = collect_txt_files(corpus_root)?;
    let mut scanned = txt_files
        .par_iter()
        .map(|path| scan_work(corpus_root, path, detector))
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

pub fn write_index(index: &Index, output: &Path) -> Result<()> {
    let file = fs::File::create(output)
        .with_context(|| format!("failed to create output {}", output.display()))?;
    serde_json::to_writer_pretty(file, index)?;
    Ok(())
}

pub fn read_index(path: &Path) -> Result<Index> {
    let file =
        fs::File::open(path).with_context(|| format!("failed to open {}", path.display()))?;
    Ok(serde_json::from_reader(file)?)
}

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

pub fn sample(index: &Index, limit: usize, features: &[String]) -> Vec<String> {
    let mut ids = query_any(index, features);
    if ids.len() > limit {
        ids.truncate(limit);
    }
    ids
}

fn collect_txt_files(corpus_root: &Path) -> Result<Vec<PathBuf>> {
    let mut paths = Vec::new();
    for entry in WalkDir::new(corpus_root)
        .follow_links(false)
        .into_iter()
        .filter_entry(|entry| !is_hidden(entry.path()))
    {
        let entry = entry?;
        if !entry.file_type().is_file() {
            continue;
        }
        let path = entry.path();
        if path
            .extension()
            .is_some_and(|ext| ext.eq_ignore_ascii_case("txt"))
            && path
                .file_name()
                .is_some_and(|name| !name.to_string_lossy().eq_ignore_ascii_case("README.txt"))
        {
            paths.push(path.to_owned());
        }
    }
    paths.sort();
    Ok(paths)
}

fn is_hidden(path: &Path) -> bool {
    path.file_name()
        .is_some_and(|name| name.to_string_lossy().starts_with('.'))
}

fn scan_work(corpus_root: &Path, path: &Path, detector: &FeatureDetector) -> Result<ScannedWork> {
    let bytes = fs::read(path).with_context(|| format!("failed to read {}", path.display()))?;
    let decoded = decode_source_bytes(&bytes)?;
    let feature_lines = detector
        .detect(&decoded.text)
        .into_iter()
        .collect::<BTreeMap<_, _>>();
    let mut features = feature_lines.keys().cloned().collect::<Vec<_>>();
    features.sort();

    let rel = path
        .strip_prefix(corpus_root)
        .with_context(|| format!("{} is outside corpus root", path.display()))?;
    let txt_path = normalize_relative_path(rel);
    let html_path = find_html_sibling(path, corpus_root)?;
    let id = work_id_from_relative(rel);

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

fn find_html_sibling(path: &Path, corpus_root: &Path) -> Result<Option<String>> {
    let Some(parent) = path.parent() else {
        return Ok(None);
    };
    for entry in fs::read_dir(parent)? {
        let entry = entry?;
        let candidate = entry.path();
        if candidate.extension().is_some_and(|ext| {
            ext.eq_ignore_ascii_case("html") || ext.eq_ignore_ascii_case("xhtml")
        }) {
            let rel = candidate.strip_prefix(corpus_root)?;
            return Ok(Some(normalize_relative_path(rel)));
        }
    }
    Ok(None)
}

fn work_id_from_relative(path: &Path) -> String {
    let parts = path
        .components()
        .filter_map(|component| component.as_os_str().to_str())
        .collect::<Vec<_>>();

    if let Some(cards_pos) = parts.iter().position(|part| *part == "cards")
        && let (Some(card), Some(file_dir)) = (parts.get(cards_pos + 1), parts.get(cards_pos + 3))
        && parts.get(cards_pos + 2) == Some(&"files")
    {
        let file = file_dir.split('_').next().unwrap_or(file_dir);
        return format!("{card}_{file}");
    }

    path.file_stem()
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
            work_id_from_relative(Path::new("cards/000148/files/799_ruby_19091/test.txt")),
            "000148_799"
        );
    }

    #[test]
    fn corpus_hash_uses_posix_paths_and_content() {
        let one = corpus_hash([("cards/1/files/a.txt", 3, "abc")]);
        let two = corpus_hash([("cards/1/files/a.txt", 3, "abc")]);
        assert_eq!(one, two);
    }
}
