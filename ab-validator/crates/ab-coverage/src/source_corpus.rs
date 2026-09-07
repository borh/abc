use std::{
    collections::BTreeMap,
    fs,
    io::Read,
    path::{Path, PathBuf},
};

use ab_encoding::{DecodedSource, decode_source_bytes};
use anyhow::{Context, Result, bail};
use flate2::read::DeflateDecoder;
use serde_json::Value;
use zip::{CompressionMethod, ZipArchive};

#[derive(Debug, Clone)]
pub struct SourceWork {
    pub work_id: String,
    pub indexed_path: String,
    pub bytes: Vec<u8>,
    pub decoded: DecodedSource,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SourceIndexEntry {
    pub work_id: String,
    pub indexed_path: String,
}

pub fn load_index(path: &Path) -> Result<BTreeMap<String, String>> {
    Ok(load_index_entries(path)?
        .into_iter()
        .map(|entry| (entry.work_id, entry.indexed_path))
        .collect())
}

pub fn load_index_entries(path: &Path) -> Result<Vec<SourceIndexEntry>> {
    let index_bytes = fs::read(path).with_context(|| format!("read index {}", path.display()))?;
    let index_doc: Value = serde_json::from_slice(&index_bytes)
        .with_context(|| format!("parse index {}", path.display()))?;
    let works = index_doc
        .get("works")
        .and_then(Value::as_array)
        .ok_or_else(|| anyhow::anyhow!("index has no .works array"))?;

    let mut entries = Vec::new();
    for work in works {
        let id = work
            .get("id")
            .and_then(Value::as_str)
            .ok_or_else(|| anyhow::anyhow!("work missing id"))?;
        let indexed_path = work
            .get("txt_path")
            .or_else(|| work.get("indexed_path"))
            .or_else(|| work.get("source_path"))
            .and_then(Value::as_str)
            .ok_or_else(|| {
                anyhow::anyhow!("work {id} missing txt_path/indexed_path/source_path")
            })?;
        entries.push(SourceIndexEntry {
            work_id: id.to_owned(),
            indexed_path: indexed_path.to_owned(),
        });
    }
    Ok(entries)
}

pub fn read_source_work(
    corpus_root: &Path,
    work_id: &str,
    indexed_path: &str,
) -> Result<SourceWork> {
    let bytes = read_indexed_source_bytes(corpus_root, indexed_path)?;
    let decoded = decode_source_bytes(&bytes)
        .with_context(|| format!("decode source {work_id} at {indexed_path}"))?;
    Ok(SourceWork {
        work_id: work_id.to_owned(),
        indexed_path: indexed_path.to_owned(),
        bytes,
        decoded,
    })
}

pub fn read_indexed_source_bytes(corpus_root: &Path, indexed_path: &str) -> Result<Vec<u8>> {
    if let Some((archive, entry)) = indexed_path.split_once("::") {
        return read_zip_entry_bytes(&corpus_root.join(archive), entry);
    }
    let path = corpus_root.join(PathBuf::from(indexed_path));
    fs::read(&path).with_context(|| format!("read {}", path.display()))
}

fn read_zip_entry_bytes(archive: &Path, entry_name: &str) -> Result<Vec<u8>> {
    let file = fs::File::open(archive).with_context(|| format!("open {}", archive.display()))?;
    let mut zip =
        ZipArchive::new(file).with_context(|| format!("read zip {}", archive.display()))?;
    for idx in 0..zip.len() {
        let mut entry = zip
            .by_index_raw(idx)
            .with_context(|| format!("read entry {idx} in {}", archive.display()))?;
        if entry.name() != entry_name {
            continue;
        }
        if entry.encrypted() {
            bail!("encrypted zip entry {entry_name} not supported");
        }
        let mut compressed = Vec::new();
        entry.read_to_end(&mut compressed)?;
        return match entry.compression() {
            CompressionMethod::Stored => Ok(compressed),
            CompressionMethod::Deflated => {
                let mut decoder = DeflateDecoder::new(&compressed[..]);
                let mut out = Vec::new();
                decoder.read_to_end(&mut out)?;
                Ok(out)
            }
            method => bail!("unsupported zip method {method:?} for {entry_name}"),
        };
    }
    bail!("zip entry {entry_name} not found in {}", archive.display())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn load_index_accepts_txt_path_and_indexed_path() {
        let path = temp_file("source-index.json");
        std::fs::write(
            &path,
            r#"{
                "works": [
                    {"id": "w1", "txt_path": "cards/1/files/1.txt"},
                    {"id": "w2", "indexed_path": "cards/2/files/2.zip::2.txt"},
                    {"id": "w3", "source_path": "cards/3/files/3.txt"}
                ]
            }"#,
        )
        .unwrap();

        let index = load_index(&path).unwrap();

        assert_eq!(index["w1"], "cards/1/files/1.txt");
        assert_eq!(index["w2"], "cards/2/files/2.zip::2.txt");
        assert_eq!(index["w3"], "cards/3/files/3.txt");
    }

    #[test]
    fn load_index_entries_preserves_duplicate_work_ids() {
        let path = temp_file("source-index-duplicates.json");
        std::fs::write(
            &path,
            r#"{
                "works": [
                    {"id": "w1", "txt_path": "cards/1/files/1.txt"},
                    {"id": "w1", "txt_path": "cards/1/files/1_ruby.txt"}
                ]
            }"#,
        )
        .unwrap();

        let entries = load_index_entries(&path).unwrap();

        assert_eq!(entries.len(), 2);
        assert_eq!(entries[0].work_id, "w1");
        assert_eq!(entries[0].indexed_path, "cards/1/files/1.txt");
        assert_eq!(entries[1].work_id, "w1");
        assert_eq!(entries[1].indexed_path, "cards/1/files/1_ruby.txt");
    }

    #[test]
    fn read_source_work_decodes_with_canonical_decoder() {
        let root = temp_dir("source-work");
        let source = root.join("cards/1/files");
        std::fs::create_dir_all(&source).unwrap();
        std::fs::write(source.join("1.txt"), b"\xef\xbb\xbfabc").unwrap();

        let work = read_source_work(&root, "w1", "cards/1/files/1.txt").unwrap();

        assert_eq!(work.work_id, "w1");
        assert_eq!(work.indexed_path, "cards/1/files/1.txt");
        assert_eq!(work.bytes, b"\xef\xbb\xbfabc");
        assert_eq!(work.decoded.text, "abc");
        assert_eq!(work.decoded.encoding, "utf-8-bom");
    }

    fn temp_file(name: &str) -> std::path::PathBuf {
        let path = temp_dir(name).join(name);
        if let Some(parent) = path.parent() {
            std::fs::create_dir_all(parent).unwrap();
        }
        path
    }

    fn temp_dir(name: &str) -> std::path::PathBuf {
        let path = std::env::temp_dir().join(format!("ab-coverage-{name}-{}", std::process::id()));
        let _ = std::fs::remove_dir_all(&path);
        std::fs::create_dir_all(&path).unwrap();
        path
    }
}
