use std::{
    collections::BTreeMap,
    fs,
    io::{Read, Write},
    path::{Path, PathBuf},
    process::{Command, Stdio},
    sync::LazyLock,
    thread,
    time::{Duration, Instant},
};

use anyhow::{Context, Result, bail};
use flate2::read::DeflateDecoder;
use jsonschema::Validator;
use rayon::prelude::*;
use serde::{Deserialize, Serialize};
use serde_json::{Value, json};
use zip::{CompressionMethod, ZipArchive};

use crate::{
    encoding::{decode_source_bytes, hex_sha256},
    properties::{PropertyViolation, builtin_properties},
};

// The AAT schema is versioned (see `docs/aat-contract.md` "Versioning" and
// the sibling dispatch in `crates/ab-aat-to-parser-ir/src/schema.rs`).
// `ab-check` embeds both frozen versions and selects the compiled validator
// per AAT document by its own top-level `"version"` field, so a
// comparison-lane (v1) document keeps validating against the frozen v1
// schema even after `data/aat-schema.json` rotates to a newer version.
const AAT_SCHEMA_V1: &str = include_str!("../../../data/aat-schema-v1.json");
const AAT_SCHEMA_V2: &str = include_str!("../../../data/aat-schema.json");

fn compile_embedded_schema(source: &str, label: &str) -> Result<Validator, String> {
    let schema: Value = serde_json::from_str(source).map_err(|error| error.to_string())?;
    jsonschema::validator_for(&schema)
        .with_context(|| format!("failed to compile {label} AAT schema"))
        .map_err(|error| error.to_string())
}

static AAT_SCHEMA_VALIDATOR_V1: LazyLock<Result<Validator, String>> =
    LazyLock::new(|| compile_embedded_schema(AAT_SCHEMA_V1, "v1"));
static AAT_SCHEMA_VALIDATOR_V2: LazyLock<Result<Validator, String>> =
    LazyLock::new(|| compile_embedded_schema(AAT_SCHEMA_V2, "v2"));

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CheckReport {
    pub adapter: String,
    pub adapter_version: String,
    pub work_id: String,
    pub results: BTreeMap<String, CheckResult>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CheckResult {
    pub pass: bool,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub message: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub line: Option<usize>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub path: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub confidence: Option<String>,
}

#[derive(Debug, Deserialize)]
struct IndexFile {
    works: Vec<IndexWork>,
}

#[derive(Debug, Deserialize)]
struct IndexWork {
    id: String,
    txt_path: String,
    features: Vec<String>,
}

#[derive(Debug, Clone)]
pub struct BatchOptions<'a> {
    pub index_path: &'a Path,
    pub corpus_root: &'a Path,
    pub features: &'a [String],
    pub work_ids_path: Option<&'a Path>,
    pub adapter: &'a str,
    pub output_dir: &'a Path,
    pub aat_output_dir: Option<&'a Path>,
    pub jobs: usize,
    pub timeout: Duration,
}

#[derive(Debug)]
struct AdapterCheckOutput {
    report: CheckReport,
    aat: Option<Value>,
}

/// Return the compiled validator for a specific AAT schema `version`.
///
/// # Errors
///
/// Returns an error when `version` is not a known AAT schema version, or the
/// embedded schema for a known version fails to compile.
pub fn schema_validator_for_version(version: u64) -> Result<&'static Validator> {
    match version {
        1 => AAT_SCHEMA_VALIDATOR_V1
            .as_ref()
            .map_err(|message| anyhow::anyhow!(message.clone())),
        2 => AAT_SCHEMA_VALIDATOR_V2
            .as_ref()
            .map_err(|message| anyhow::anyhow!(message.clone())),
        other => bail!("unsupported AAT schema version {other} (known: 1, 2)"),
    }
}

/// Read an AAT document's top-level `"version"` field and return the
/// matching compiled validator. Fails closed: a missing or non-integer
/// `"version"` is rejected rather than defaulting to any particular schema.
///
/// # Errors
///
/// Returns an error when `"version"` is missing, is not an unsigned
/// integer, or is not a known AAT schema version.
pub fn schema_validator_for_document(aat: &Value) -> Result<&'static Validator> {
    let version = aat
        .get("version")
        .context("AAT document is missing the top-level \"version\" field")?
        .as_u64()
        .context("AAT document's top-level \"version\" field is not an unsigned integer")?;
    schema_validator_for_version(version)
}

/// Validate an AAT object against the JSON schema selected by its own
/// top-level `"version"` field.
///
/// # Errors
///
/// Returns an error when the document's version cannot be resolved to a
/// known schema, or when schema validation fails.
pub fn validate_aat_value(aat: &Value) -> Result<()> {
    let validator = schema_validator_for_document(aat)?;
    validator.validate(aat).map_err(|error| {
        anyhow::anyhow!(
            "AAT schema validation failed at {}: {error}",
            error.instance_path()
        )
    })
}

fn parse_json_value(bytes: &[u8]) -> Result<Value> {
    let mut deserializer = serde_json::Deserializer::from_slice(bytes);
    deserializer.disable_recursion_limit();
    let deserializer = serde_stacker::Deserializer::new(&mut deserializer);
    Value::deserialize(deserializer).map_err(Into::into)
}

/// Validate one text+aat pair and produce a report.
///
/// # Errors
///
/// Returns an error when input files cannot be read, AAT cannot be parsed,
/// or validation fails.
pub fn check_single(
    txt_path: &Path,
    aat_path: &Path,
    output: Option<&Path>,
) -> Result<CheckReport> {
    let txt_bytes =
        fs::read(txt_path).with_context(|| format!("failed to read {}", txt_path.display()))?;
    let decoded = decode_source_bytes(&txt_bytes)?;
    let aat = parse_json_value(
        &fs::read(aat_path).with_context(|| format!("failed to read {}", aat_path.display()))?,
    )
    .with_context(|| format!("failed to parse {}", aat_path.display()))?;
    let report = check_value(&decoded.text, &aat);
    write_report(&report, output)?;
    Ok(report)
}

pub fn check_value(txt: &str, aat: &Value) -> CheckReport {
    let adapter = aat
        .pointer("/meta/adapter")
        .and_then(Value::as_str)
        .unwrap_or("unknown")
        .to_owned();
    let adapter_version = aat
        .pointer("/meta/adapter_version")
        .and_then(Value::as_str)
        .unwrap_or("unknown")
        .to_owned();
    let work_id = aat
        .get("work_id")
        .and_then(Value::as_str)
        .unwrap_or("unknown")
        .to_owned();

    let mut results = BTreeMap::new();
    let schema_failure = match schema_validator_for_document(aat) {
        Err(error) => Some((error.to_string(), None)),
        Ok(validator) => validator
            .validate(aat)
            .err()
            .map(|error| (error.to_string(), Some(error.instance_path().to_string()))),
    };
    if let Some((message, path)) = schema_failure {
        results.insert(
            "schema_valid".to_owned(),
            CheckResult {
                pass: false,
                message: Some(message),
                line: None,
                path,
                confidence: Some("strict".to_owned()),
            },
        );
        return CheckReport {
            adapter,
            adapter_version,
            work_id,
            results,
        };
    }

    results.insert(
        "schema_valid".to_owned(),
        CheckResult {
            pass: true,
            message: None,
            line: None,
            path: None,
            confidence: Some("strict".to_owned()),
        },
    );

    for property in builtin_properties() {
        let name = property.name().to_owned();
        let result = match property.check(txt, aat) {
            Ok(()) => CheckResult {
                pass: true,
                message: None,
                line: None,
                path: None,
                confidence: Some(default_confidence(&name).to_owned()),
            },
            Err(PropertyViolation {
                message,
                line,
                path,
                confidence,
                ..
            }) => CheckResult {
                pass: false,
                message: Some(message),
                line,
                path,
                confidence: Some(confidence.to_owned()),
            },
        };
        results.insert(name, result);
    }

    CheckReport {
        adapter,
        adapter_version,
        work_id,
        results,
    }
}

/// Validate many works from an index and write reports.
///
/// # Errors
///
/// Returns an error when inputs cannot be read/discovered, adapter is missing, or
/// any execution/serialization step fails.
pub fn run_batch(options: BatchOptions<'_>) -> Result<()> {
    let index: IndexFile = serde_json::from_slice(&fs::read(options.index_path)?)?;
    let explicit_ids = if let Some(path) = options.work_ids_path {
        Some(serde_json::from_slice::<Vec<String>>(&fs::read(path)?)?)
    } else {
        None
    };
    let works = index
        .works
        .into_iter()
        .filter(|work| is_aozora_markup_corpus_source(&work.txt_path))
        .filter(|work| {
            if explicit_ids.is_none() && options.features.is_empty() {
                return true;
            }
            explicit_ids
                .as_ref()
                .is_some_and(|ids| ids.contains(&work.id))
                || (!options.features.is_empty()
                    && options
                        .features
                        .iter()
                        .any(|feature| work.features.binary_search(feature).is_ok()))
        })
        .collect::<Vec<_>>();

    let adapter_path = discover_adapter(options.adapter)?;
    let adapter_version = adapter_version(&adapter_path)?;
    let adapter_output_name = sanitize_filename(
        adapter_path
            .file_stem()
            .and_then(|name| name.to_str())
            .unwrap_or(options.adapter),
    );
    let pool = rayon::ThreadPoolBuilder::new()
        .num_threads(options.jobs)
        .build()?;

    pool.install(|| {
        works.par_iter().try_for_each(|work| -> Result<()> {
            let checked = invoke_and_check(
                &adapter_path,
                &adapter_version,
                options.corpus_root,
                &work.txt_path,
                &work.id,
                options.timeout,
            )?;
            let out = options
                .output_dir
                .join(&adapter_output_name)
                .join(report_filename(work));
            if let Some(parent) = out.parent() {
                fs::create_dir_all(parent)?;
            }
            write_report(&checked.report, Some(&out))?;
            if let (Some(aat_output_dir), Some(aat)) = (options.aat_output_dir, checked.aat) {
                let aat_path = aat_output_dir
                    .join(&adapter_output_name)
                    .join(report_filename(work));
                if let Some(parent) = aat_path.parent() {
                    fs::create_dir_all(parent)?;
                }
                let file = fs::File::create(aat_path)?;
                serde_json::to_writer_pretty(file, &aat)?;
            }
            Ok(())
        })
    })
}

/// Write a check report to file when requested.
///
/// # Errors
///
/// Returns an error when the output path cannot be created or JSON serialization fails.
pub fn write_report(report: &CheckReport, output: Option<&Path>) -> Result<()> {
    if let Some(path) = output {
        if let Some(parent) = path.parent() {
            fs::create_dir_all(parent)?;
        }
        let file = fs::File::create(path)?;
        serde_json::to_writer_pretty(file, report)?;
    } else {
        serde_json::to_writer_pretty(std::io::stdout(), report)?;
        println!();
    }
    Ok(())
}

fn invoke_and_check(
    adapter_path: &Path,
    adapter_version: &str,
    corpus_root: &Path,
    indexed_txt_path: &str,
    work_id: &str,
    timeout: Duration,
) -> Result<AdapterCheckOutput> {
    let txt_bytes = read_indexed_source_bytes(corpus_root, indexed_txt_path)?;
    let decoded = decode_source_bytes(&txt_bytes)?;
    let mut child = Command::new(adapter_path)
        .arg("--mode")
        .arg("aat")
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()?;
    let mut stdout = child
        .stdout
        .take()
        .context("adapter stdout was not piped")?;
    let mut stderr = child
        .stderr
        .take()
        .context("adapter stderr was not piped")?;
    let stdout_reader = thread::spawn(move || {
        let mut bytes = Vec::new();
        stdout.read_to_end(&mut bytes).map(|_| bytes)
    });
    let stderr_reader = thread::spawn(move || {
        let mut bytes = Vec::new();
        stderr.read_to_end(&mut bytes).map(|_| bytes)
    });

    {
        let mut stdin = child.stdin.take().context("adapter stdin was not piped")?;
        stdin.write_all(&txt_bytes)?;
    }

    let start = Instant::now();
    loop {
        if let Some(status) = child.try_wait()? {
            let stdout = join_reader(stdout_reader, "stdout")?;
            let stderr = join_reader(stderr_reader, "stderr")?;
            return match status.code() {
                Some(code) if code == 0 || code == 2 => {
                    let mut aat = parse_json_value(&stdout).with_context(|| {
                        format!("failed to parse adapter AAT JSON for {work_id}")
                    })?;
                    if let Some(root) = aat.as_object_mut() {
                        root.insert("work_id".to_owned(), Value::String(work_id.to_owned()));
                    }
                    if let Some(meta) = aat.get_mut("meta").and_then(Value::as_object_mut) {
                        meta.insert(
                            "adapter_version".to_owned(),
                            Value::String(adapter_version.to_owned()),
                        );
                    }
                    let report = check_value(&decoded.text, &aat);
                    Ok(AdapterCheckOutput {
                        report,
                        aat: Some(aat),
                    })
                }
                Some(1) => Ok(AdapterCheckOutput {
                    report: adapter_error_report(
                        adapter_path,
                        adapter_version,
                        work_id,
                        "fatal_error",
                        &String::from_utf8_lossy(&stderr),
                    ),
                    aat: None,
                }),
                code => Ok(AdapterCheckOutput {
                    report: adapter_error_report(
                        adapter_path,
                        adapter_version,
                        work_id,
                        "adapter_protocol_error",
                        &format!("unexpected exit code {code:?}"),
                    ),
                    aat: None,
                }),
            };
        }
        if start.elapsed() > timeout {
            let _ = child.kill();
            let _ = child.wait();
            let _ = join_reader(stdout_reader, "stdout");
            let _ = join_reader(stderr_reader, "stderr");
            return Ok(AdapterCheckOutput {
                report: adapter_error_report(
                    adapter_path,
                    adapter_version,
                    work_id,
                    "adapter_timeout",
                    "adapter timed out",
                ),
                aat: None,
            });
        }
        thread::sleep(Duration::from_millis(20));
    }
}

fn join_reader(
    reader: thread::JoinHandle<std::io::Result<Vec<u8>>>,
    stream_name: &str,
) -> Result<Vec<u8>> {
    reader
        .join()
        .map_err(|_| anyhow::anyhow!("adapter {stream_name} reader thread panicked"))?
        .with_context(|| format!("failed to read adapter {stream_name}"))
}

/// Read the exact plain-file or ZIP-member (`archive::member`) bytes an index row names.
pub fn read_indexed_source_bytes(corpus_root: &Path, indexed_path: &str) -> Result<Vec<u8>> {
    if let Some((archive_path, entry_name)) = indexed_path.split_once("::") {
        return read_zip_entry_bytes(&corpus_root.join(archive_path), entry_name);
    }
    let path = corpus_root.join(indexed_path);
    fs::read(&path).with_context(|| format!("failed to read {}", path.display()))
}

fn is_aozora_markup_corpus_source(indexed_path: &str) -> bool {
    let source_path = indexed_path
        .split_once("::")
        .map_or(indexed_path, |(archive, _)| archive);
    let parts = source_path.split('/').collect::<Vec<_>>();
    parts.len() >= 4
        && parts[0].eq_ignore_ascii_case("cards")
        && !parts[1].is_empty()
        && parts[2].eq_ignore_ascii_case("files")
}

fn read_zip_entry_bytes(archive: &Path, entry_name: &str) -> Result<Vec<u8>> {
    let file =
        fs::File::open(archive).with_context(|| format!("failed to open {}", archive.display()))?;
    let mut archive_reader = ZipArchive::new(file)
        .with_context(|| format!("failed to read zip {}", archive.display()))?;
    for idx in 0..archive_reader.len() {
        let mut entry = archive_reader
            .by_index_raw(idx)
            .with_context(|| format!("failed to read zip entry {idx} in {}", archive.display()))?;
        if entry.name() != entry_name {
            continue;
        }
        if entry.encrypted() {
            bail!(
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
        return match entry.compression() {
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
            method => bail!(
                "unsupported zip compression method {method:?} for {entry_name} in {}",
                archive.display()
            ),
        };
    }
    bail!("zip entry {entry_name} not found in {}", archive.display())
}

fn adapter_error_report(
    adapter_path: &Path,
    adapter_version: &str,
    work_id: &str,
    result_key: &str,
    message: &str,
) -> CheckReport {
    let mut results = BTreeMap::new();
    results.insert(
        result_key.to_owned(),
        CheckResult {
            pass: false,
            message: Some(message.to_owned()),
            line: None,
            path: None,
            confidence: Some("strict".to_owned()),
        },
    );
    CheckReport {
        adapter: adapter_path
            .file_name()
            .and_then(|name| name.to_str())
            .unwrap_or("adapter")
            .to_owned(),
        adapter_version: adapter_version.to_owned(),
        work_id: work_id.to_owned(),
        results,
    }
}

fn discover_adapter(adapter: &str) -> Result<PathBuf> {
    let binary = if adapter.ends_with("-adapter") {
        adapter.to_owned()
    } else {
        format!("{adapter}-adapter")
    };
    if let Ok(dir) = std::env::var("AB_ADAPTER_PATH") {
        let candidate = Path::new(&dir).join(&binary);
        if candidate.exists() {
            return Ok(candidate);
        }
    }
    if Path::new(adapter).exists() {
        return Ok(PathBuf::from(adapter));
    }
    for dir in std::env::var_os("PATH")
        .map(|paths| std::env::split_paths(&paths).collect::<Vec<_>>())
        .unwrap_or_default()
    {
        let candidate = dir.join(&binary);
        if candidate.exists() {
            return Ok(candidate);
        }
    }
    bail!("adapter {binary} not found in AB_ADAPTER_PATH or PATH")
}

fn adapter_version(adapter_path: &Path) -> Result<String> {
    let output = Command::new(adapter_path).arg("--version").output()?;
    if output.status.success() {
        Ok(String::from_utf8_lossy(&output.stdout).trim().to_owned())
    } else {
        Ok("unknown".to_owned())
    }
}

fn sanitize_filename(name: &str) -> String {
    name.chars()
        .map(|ch| {
            if ch.is_ascii_alphanumeric() || ch == '-' || ch == '_' {
                ch
            } else {
                '_'
            }
        })
        .collect()
}

fn report_filename(work: &IndexWork) -> String {
    let path_hash = hex_sha256(work.txt_path.as_bytes());
    format!("{}-{}.json", sanitize_filename(&work.id), &path_hash[..12])
}

fn default_confidence(name: &str) -> &'static str {
    match name {
        "parse_completeness" => "strict",
        _ => "heuristic",
    }
}

#[must_use]
pub fn report_to_value(report: &CheckReport) -> Value {
    json!(report)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn fixture_with_version(version: Value) -> Value {
        serde_json::json!({
            "version": version,
            "work_id": "fixture",
            "blocks": [{"kind": "paragraph", "content": [{"kind": "text", "value": "本文"}]}],
            "meta": {
                "adapter": "fixture",
                "adapter_version": "fixture",
                "source_encoding": "utf-8",
                "source_hash": "sha256:0000000000000000000000000000000000000000000000000000000000000000",
                "parse_complete": true,
                "warnings": []
            }
        })
    }

    #[test]
    fn validate_aat_value_reports_schema_status() {
        let valid = fixture_with_version(serde_json::json!(1));
        assert!(validate_aat_value(&valid).is_ok());

        let invalid = serde_json::json!({"version": 1});
        assert!(validate_aat_value(&invalid).is_err());
    }

    #[test]
    fn v1_document_validates_against_frozen_v1_schema() {
        let doc = fixture_with_version(serde_json::json!(1));
        assert!(validate_aat_value(&doc).is_ok());
    }

    #[test]
    fn v2_document_validates_against_current_v2_schema() {
        let doc = fixture_with_version(serde_json::json!(2));
        assert!(validate_aat_value(&doc).is_ok());
    }

    #[test]
    fn version_3_document_is_rejected_fail_closed() {
        let doc = fixture_with_version(serde_json::json!(3));
        let error = validate_aat_value(&doc).unwrap_err();
        assert!(
            error
                .to_string()
                .contains("unsupported AAT schema version 3"),
            "unexpected error message: {error}"
        );
    }

    #[test]
    fn missing_version_document_is_rejected_fail_closed() {
        let doc = serde_json::json!({
            "work_id": "fixture",
            "blocks": [],
            "meta": {
                "adapter": "fixture",
                "adapter_version": "fixture",
                "source_encoding": "utf-8",
                "source_hash": "sha256:0000000000000000000000000000000000000000000000000000000000000000",
                "parse_complete": true,
                "warnings": []
            }
        });
        let error = validate_aat_value(&doc).unwrap_err();
        assert!(
            error
                .to_string()
                .contains("missing the top-level \"version\" field"),
            "unexpected error message: {error}"
        );
    }

    #[test]
    fn schema_validator_for_version_rejects_unknown_version() {
        let error = schema_validator_for_version(99).unwrap_err();
        assert!(
            error
                .to_string()
                .contains("unsupported AAT schema version 99"),
            "unexpected error message: {error}"
        );
    }
}
