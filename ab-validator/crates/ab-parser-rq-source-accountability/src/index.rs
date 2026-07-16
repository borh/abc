use std::collections::HashSet;
use std::fs::{self, OpenOptions};
use std::io::Write;
use std::path::{Component, Path, PathBuf};
use std::sync::atomic::{AtomicU64, Ordering};

use anyhow::{Context, Result, bail};
use serde::Serialize;
use sha2::{Digest, Sha256};

use crate::{
    CoordinateSystem, CorpusInput, JsonMediaType, QualificationIdentity, RecordIndex,
    RecordIndexEntry, RecordIndexSchemaVersion, WorkInput, WorkStatus, analyze_work,
};

// `source_root`, `parser_ir_root`, and `store_root` are campaign-owned runtime
// configuration. Input containment is checked, but callers must keep these
// roots immutable during a run. Content blobs may remain orphaned after a
// publication failure; the index is the final atomic commit and sole summary.

static TEMP_SEQUENCE: AtomicU64 = AtomicU64::new(0);

fn digest(bytes: &[u8]) -> String {
    format!("{:x}", Sha256::digest(bytes))
}

/// Serializes using ABC's conservative canonical JSON discipline.
///
/// Object keys are ordered and floating-point numbers are rejected. This is
/// the repository helper used for hashable ABC values, whose keys are ASCII.
pub fn canonical_json<T: Serialize>(value: &T) -> Result<String> {
    let value = serde_json::to_value(value)?;
    ab_diff_utils::canonical_json_string(&value)
}

fn p0_string(value: &str) -> String {
    let mut encoded = String::from("\"");
    for character in value.chars() {
        match character {
            '/' => encoded.push_str("\\/"),
            character if character.is_ascii() => {
                let scalar =
                    serde_json::to_string(&character.to_string()).expect("string serializes");
                encoded.push_str(&scalar[1..scalar.len() - 1]);
            }
            character => {
                for unit in character.encode_utf16(&mut [0_u16; 2]) {
                    encoded.push_str(&format!("\\u{unit:04x}"));
                }
            }
        }
    }
    encoded.push('"');
    encoded
}

fn p0_canonical_json(value: &serde_json::Value, output: &mut String) {
    match value {
        serde_json::Value::Object(object) => {
            output.push('{');
            let mut entries = object.iter().collect::<Vec<_>>();
            entries.sort_unstable_by_key(|(key, _)| *key);
            for (index, (key, value)) in entries.into_iter().enumerate() {
                if index != 0 {
                    output.push(',');
                }
                output.push_str(&p0_string(key));
                output.push(':');
                p0_canonical_json(value, output);
            }
            output.push('}');
        }
        serde_json::Value::Array(values) => {
            output.push('[');
            for (index, value) in values.iter().enumerate() {
                if index != 0 {
                    output.push(',');
                }
                p0_canonical_json(value, output);
            }
            output.push(']');
        }
        serde_json::Value::String(value) => output.push_str(&p0_string(value)),
        value => output.push_str(&serde_json::to_string(value).expect("JSON scalar serializes")),
    }
}

pub fn qualification_identity_ref(identity: &QualificationIdentity) -> Result<String> {
    let value = serde_json::to_value(identity)?;
    let mut bytes = String::new();
    p0_canonical_json(&value, &mut bytes);
    Ok(format!("sha256:{}", digest(bytes.as_bytes())))
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

fn blob_identity(bytes: &[u8]) -> (String, String) {
    let hash = digest(bytes);
    let relative = locator(&hash);
    (format!("sha256:{hash}"), relative)
}

fn verify_destination(path: &Path, bytes: &[u8]) -> Result<()> {
    let metadata = fs::symlink_metadata(path)?;
    if metadata.file_type().is_symlink() || !metadata.is_file() {
        bail!("CAS destination is not a regular file: {}", path.display());
    }
    let actual = fs::read(path)?;
    if actual != bytes || digest(&actual) != digest(bytes) {
        bail!("content-address collision at {}", path.display());
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

fn preflight_destination(root: &Path, relative: &str, bytes: &[u8]) -> Result<()> {
    let path = root.join(relative);
    if destination_exists(&path)? {
        verify_destination(&path, bytes)?;
    }
    Ok(())
}

fn publish_blob(root: &Path, relative: &str, bytes: &[u8]) -> Result<()> {
    let path = root.join(relative);
    let parent = path.parent().context("CAS path has no parent")?;
    fs::create_dir_all(parent)?;
    if fs::symlink_metadata(parent)?.file_type().is_symlink() {
        bail!("CAS parent must not be a symlink: {}", parent.display());
    }
    if destination_exists(&path)? {
        return verify_destination(&path, bytes);
    }
    let name = path
        .file_name()
        .context("CAS path has no file name")?
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
    verify_destination(&path, bytes)
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

    let expected_work_count = prepared.len() as u64;
    let identity_ref = qualification_identity_ref(&input.qualification_identity)?;
    let mut analyses = Vec::with_capacity(prepared.len());
    for (entry, original_bytes, parser_ir_bytes) in prepared {
        let mut analysis = analyze_work(WorkInput {
            original_bytes,
            parser_ir_bytes,
            corpus_entry: entry.corpus_entry,
            qualification_identity: input.qualification_identity.clone(),
            taxonomy: input.taxonomy.clone(),
            diagnostics_locator: "content-addressed".to_owned(),
        });
        if let Some(bytes) = analysis.diagnostics_bytes.as_deref() {
            let (_, diagnostic_locator) = blob_identity(bytes);
            analysis
                .record
                .diagnostics
                .as_mut()
                .expect("bytes imply reference")
                .locator = diagnostic_locator;
        }
        analyses.push(analysis);
    }

    let mut publications: Vec<(String, Vec<u8>)> = Vec::new();
    let mut records = Vec::with_capacity(analyses.len());
    let mut errors = Vec::new();
    for analysis in analyses {
        if analysis.record.status == WorkStatus::Unavailable {
            errors.push(format!("work-unavailable:{}", analysis.record.work_id));
        }
        if let Some(bytes) = analysis.diagnostics_bytes {
            let (_, relative) = blob_identity(&bytes);
            publications.push((relative, bytes));
        }
        let record_bytes = canonical_json(&analysis.record)?.into_bytes();
        let (sha256, record_locator) = blob_identity(&record_bytes);
        records.push(RecordIndexEntry {
            work_id: analysis.record.work_id,
            sha256,
            bytes: record_bytes.len() as u64,
            media_type: JsonMediaType::ApplicationJson,
            locator: record_locator,
        });
        publications.push((
            records.last().expect("record just pushed").locator.clone(),
            record_bytes,
        ));
    }
    publications.sort_by(|a, b| a.0.cmp(&b.0));
    publications.dedup_by(|a, b| a.0 == b.0 && a.1 == b.1);
    for (relative, bytes) in &publications {
        preflight_destination(&input.store_root, relative, bytes)?;
    }
    for (relative, bytes) in &publications {
        publish_blob(&input.store_root, relative, bytes)?;
    }
    let index = RecordIndex {
        schema_version: RecordIndexSchemaVersion::V1,
        identity_ref,
        taxonomy_version: input.taxonomy.taxonomy_version,
        taxonomy_hash: input.taxonomy.taxonomy_hash,
        coordinate_system: CoordinateSystem::DecodedUtf8,
        status: if errors.is_empty() {
            WorkStatus::Ok
        } else {
            WorkStatus::Unavailable
        },
        expected_work_count,
        record_count: records.len() as u64,
        records,
        errors,
    };
    write_atomic(&input.index_out, canonical_json(&index)?.as_bytes())?;
    Ok(index)
}
