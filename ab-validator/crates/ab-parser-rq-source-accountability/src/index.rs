use std::collections::HashSet;
use std::fs;
use std::path::{Component, Path, PathBuf};

use anyhow::{Context, Result, bail};
use serde::Serialize;
use serde_json::Value;
use sha2::{Digest, Sha256};

use crate::{
    CorpusInput, JsonMediaType, RecordIndex, RecordIndexEntry, RecordIndexSchemaVersion, WorkInput,
    analyze_work,
};

fn digest(bytes: &[u8]) -> String {
    format!("{:x}", Sha256::digest(bytes))
}

fn render(value: &Value, out: &mut Vec<u8>) {
    match value {
        Value::Object(object) => {
            out.push(b'{');
            let mut entries = object.iter().collect::<Vec<_>>();
            entries.sort_unstable_by_key(|(key, _)| *key);
            for (index, (key, value)) in entries.into_iter().enumerate() {
                if index != 0 {
                    out.push(b',');
                }
                out.extend(serde_json::to_vec(key).expect("JSON key serializes"));
                out.push(b':');
                render(value, out);
            }
            out.push(b'}');
        }
        Value::Array(values) => {
            out.push(b'[');
            for (index, value) in values.iter().enumerate() {
                if index != 0 {
                    out.push(b',');
                }
                render(value, out);
            }
            out.push(b']');
        }
        _ => out.extend(serde_json::to_vec(value).expect("JSON scalar serializes")),
    }
}

pub fn canonical_json<T: Serialize>(value: &T) -> Result<String> {
    let value = serde_json::to_value(value)?;
    let mut bytes = Vec::new();
    render(&value, &mut bytes);
    Ok(String::from_utf8(bytes).expect("JSON is UTF-8"))
}

fn reject_lexical_escape(path: &Path, label: &str) -> Result<()> {
    if path.is_absolute() || path.components().any(|c| matches!(c, Component::ParentDir)) {
        bail!("{label} escapes its configured root: {}", path.display());
    }
    Ok(())
}

fn existing_below(root: &Path, relative: &Path, label: &str) -> Result<PathBuf> {
    reject_lexical_escape(relative, label)?;
    let root =
        fs::canonicalize(root).with_context(|| format!("canonicalize {}", root.display()))?;
    let path = fs::canonicalize(root.join(relative))
        .with_context(|| format!("resolve {label} {}", relative.display()))?;
    if !path.starts_with(&root) {
        bail!(
            "{label} escapes its configured root: {}",
            relative.display()
        );
    }
    Ok(path)
}

fn locator(digest: &str) -> String {
    format!("sha256/{}/{}.json", &digest[..2], digest)
}

fn write_atomic(path: &Path, bytes: &[u8]) -> Result<()> {
    let parent = path.parent().context("output has no parent directory")?;
    fs::create_dir_all(parent)?;
    let name = path
        .file_name()
        .context("output has no file name")?
        .to_string_lossy();
    let temp = parent.join(format!(".{name}.tmp-{}", std::process::id()));
    fs::write(&temp, bytes).with_context(|| format!("write {}", temp.display()))?;
    if let Err(error) = fs::rename(&temp, path) {
        let _ = fs::remove_file(&temp);
        return Err(error).with_context(|| format!("replace {}", path.display()));
    }
    Ok(())
}

fn store_blob(root: &Path, bytes: &[u8]) -> Result<(String, String)> {
    let hash = digest(bytes);
    let relative = locator(&hash);
    let path = root.join(&relative);
    if path.exists() {
        if fs::read(&path)? != bytes {
            bail!("content-address collision at {}", path.display());
        }
    } else {
        write_atomic(&path, bytes)?;
    }
    Ok((format!("sha256:{hash}"), relative))
}

pub fn analyze_corpus(mut input: CorpusInput) -> Result<RecordIndex> {
    let mut ids = HashSet::new();
    for entry in &input.entries {
        if entry.corpus_entry.work_id.is_empty() {
            bail!("empty work_id");
        }
        if !ids.insert(entry.corpus_entry.work_id.clone()) {
            bail!("duplicate work_id: {}", entry.corpus_entry.work_id);
        }
        reject_lexical_escape(&entry.source_path, "source path")?;
        reject_lexical_escape(
            Path::new(&format!("{}.json", entry.corpus_entry.work_id)),
            "work_id",
        )?;
    }
    input
        .entries
        .sort_by(|a, b| a.corpus_entry.work_id.cmp(&b.corpus_entry.work_id));

    let mut prepared = Vec::with_capacity(input.entries.len());
    for entry in input.entries {
        let source_path = existing_below(&input.source_root, &entry.source_path, "source path")?;
        let ir_relative = PathBuf::from(format!("{}.json", entry.corpus_entry.work_id));
        let parser_ir_path = existing_below(&input.parser_ir_root, &ir_relative, "Parser-IR path")?;
        prepared.push((entry, fs::read(source_path)?, fs::read(parser_ir_path)?));
    }

    let mut records = Vec::with_capacity(prepared.len());
    for (entry, original_bytes, parser_ir_bytes) in prepared {
        let placeholder = "content-addressed".to_owned();
        let mut analysis = analyze_work(WorkInput {
            original_bytes,
            parser_ir_bytes,
            corpus_entry: entry.corpus_entry,
            qualification_identity: input.qualification_identity.clone(),
            taxonomy: input.taxonomy.clone(),
            diagnostics_locator: placeholder,
        });
        if let Some(bytes) = analysis.diagnostics_bytes.as_deref() {
            let (_, diagnostic_locator) = store_blob(&input.store_root, bytes)?;
            analysis
                .record
                .diagnostics
                .as_mut()
                .expect("bytes imply reference")
                .locator = diagnostic_locator;
        }
        let record_bytes = canonical_json(&analysis.record)?.into_bytes();
        let (sha256, record_locator) = store_blob(&input.store_root, &record_bytes)?;
        records.push(RecordIndexEntry {
            work_id: analysis.record.work_id,
            sha256,
            bytes: record_bytes.len() as u64,
            media_type: JsonMediaType::ApplicationJson,
            locator: record_locator,
        });
    }
    let index = RecordIndex {
        schema_version: RecordIndexSchemaVersion::V1,
        records,
    };
    write_atomic(&input.index_out, canonical_json(&index)?.as_bytes())?;
    Ok(index)
}
