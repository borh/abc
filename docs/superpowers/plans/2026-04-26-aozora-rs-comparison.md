# Aozora-rs Comparison Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add the next parser adapter (`aozora-rs`) and produce a measured, reproducible comparison against the existing `aozora2` adapter on the full readable Aozora Bunko corpus.

**Architecture:** Implement `aozora-rs-adapter` as an external Rust adapter, matching the existing adapter contract: stdin source bytes, `--mode aat`, `--mode html`, and `--version`. Add a small `ab-compare` workspace crate that compares two sets of `ab-check` reports and, when AAT artifacts are available, compares normalized visible text and annotation counts. Extend benchmark scripts to run both adapters with the same index and emit one JSON comparison summary.

**Tech Stack:** Rust 2024, `clap`, `anyhow`, `serde_json`, `encoding_rs`, `sha2`, `regex`, `rayon`, `walkdir`, `criterion`; `aozora-rs` pinned from `https://github.com/kinoko0518/aozora-rs` at `dd380ee639ca317ac9092ef2ba554acdf70e3c8d`.

---

## File Structure

```
ab-validator/
├── Cargo.toml
├── Cargo.lock
├── adapters/
│   ├── aozora2/
│   └── aozora-rs/
│       ├── Cargo.toml
│       ├── Cargo.lock
│       ├── benches/adapter_bench.rs
│       └── src/
│           ├── lib.rs
│           └── main.rs
├── crates/
│   ├── ab-check/
│   │   └── src/check.rs
│   └── ab-compare/
│       ├── Cargo.toml
│       ├── src/lib.rs
│       ├── src/main.rs
│       └── tests/integration.rs
└── benchmarks/
    ├── README.md
    ├── run-full-corpus.sh
    ├── run-parser-comparison.sh
    └── baselines/
```

Boundary decisions:

- `aozora-rs-adapter` owns parser integration and AAT emission for `aozora-rs`.
- `ab-check` remains responsible for running one adapter and writing per-work reports.
- `ab-compare` owns report-set comparison and later AAT diffing; it must not invoke adapters directly.
- Benchmark scripts orchestrate full-corpus runs only; they do not contain comparison logic beyond calling binaries and collecting summary JSON.

---

## Task 1: Create `aozora-rs` Adapter Skeleton

**Files:**
- Create: `adapters/aozora-rs/Cargo.toml`
- Create: `adapters/aozora-rs/src/lib.rs`
- Create: `adapters/aozora-rs/src/main.rs`
- Create: `adapters/aozora-rs/benches/adapter_bench.rs`

- [ ] **Step 1: Create the adapter manifest**

Create `adapters/aozora-rs/Cargo.toml`:

```toml
[package]
name = "aozora-rs-adapter"
version = "0.1.0"
edition = "2024"
license = "MIT OR Apache-2.0"

[dependencies]
anyhow = "1.0"
aozora-rs = { git = "https://github.com/kinoko0518/aozora-rs", package = "aozora-rs", rev = "dd380ee639ca317ac9092ef2ba554acdf70e3c8d" }
clap = { version = "4.5", features = ["derive"] }
encoding_rs = "0.8"
regex = "1.10"
serde = { version = "1.0", features = ["derive"] }
serde_json = "1.0"
sha2 = "0.10"

[dev-dependencies]
criterion = "0.8"

[[bench]]
name = "adapter_bench"
harness = false

[workspace]
```

- [ ] **Step 2: Write a failing library test for the adapter contract**

Add this test at the bottom of `adapters/aozora-rs/src/lib.rs` before implementation:

```rust
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn emits_schema_shaped_aat_for_ruby_and_gaiji() {
        let input = b"\xe5\x90\xbe\xe8\xbc\xa9\xe3\x80\x8a\xe3\x82\x8f\xe3\x81\x8c\xe3\x81\xaf\xe3\x81\x84\xe3\x80\x8b\xe3\x81\xaf\xe2\x80\xbb\xef\xbc\xbb\xef\xbc\x83\xe3\x80\x8c\xe5\x8f\xa3\xef\xbc\x8b\xe4\xb8\x96\xe3\x80\x8d\xe3\x80\x81U+546D\xef\xbc\xbd\xe3\x81\xa7\xe3\x81\x82\xe3\x82\x8b\xe3\x80\x82";
        let out = aat_json_from_bytes(input).unwrap();
        let value: serde_json::Value = serde_json::from_slice(&out).unwrap();

        assert_eq!(value["work_id"], "stdin");
        assert_eq!(value["meta"]["adapter"], "aozora-rs");
        assert_eq!(value["meta"]["source_encoding"], "utf-8");
        assert!(value["meta"]["parse_complete"].as_bool().unwrap());
        assert_eq!(value["blocks"][0]["kind"], "paragraph");
    }
}
```

Run:

```bash
cargo test --manifest-path adapters/aozora-rs/Cargo.toml
```

Expected: FAIL because `aat_json_from_bytes` does not exist yet.

- [ ] **Step 3: Implement the minimal adapter library**

Create `adapters/aozora-rs/src/lib.rs`:

```rust
use anyhow::Result;
use encoding_rs::SHIFT_JIS;
use regex::Regex;
use serde::Serialize;
use serde_json::json;
use sha2::{Digest, Sha256};

pub const VERSION: &str =
    "aozora-rs-adapter 0.1.0 dd380ee639ca317ac9092ef2ba554acdf70e3c8d";

#[derive(Debug)]
pub struct DecodedSource {
    pub text: String,
    pub encoding: &'static str,
    pub source_hash: String,
}

#[derive(Debug, Serialize)]
struct Span {
    line_start: usize,
    line_end: usize,
    byte_start: usize,
    byte_end: usize,
}

pub fn decode_source_bytes(bytes: &[u8]) -> Result<DecodedSource> {
    let source_hash = format!("sha256:{}", hex_sha256(bytes));
    if bytes.starts_with(&[0xef, 0xbb, 0xbf]) {
        return Ok(DecodedSource {
            text: std::str::from_utf8(&bytes[3..])?.to_owned(),
            encoding: "utf-8-bom",
            source_hash,
        });
    }
    if let Ok(text) = std::str::from_utf8(bytes) {
        return Ok(DecodedSource {
            text: text.to_owned(),
            encoding: "utf-8",
            source_hash,
        });
    }
    let (cow, _, had_errors) = SHIFT_JIS.decode(bytes);
    Ok(DecodedSource {
        text: cow.into_owned(),
        encoding: if had_errors {
            "windows-31j-lossy"
        } else {
            "windows-31j"
        },
        source_hash,
    })
}

pub fn aat_json_from_bytes(bytes: &[u8]) -> Result<Vec<u8>> {
    let decoded = decode_source_bytes(bytes)?;
    let _ = aozora_rs::AozoraDocument::from_str(&decoded.text, None);
    let aat = build_validation_aat(&decoded);
    let mut out = Vec::new();
    serde_json::to_writer(&mut out, &aat)?;
    out.push(b'\n');
    Ok(out)
}

pub fn html_from_bytes(bytes: &[u8]) -> Result<String> {
    let decoded = decode_source_bytes(bytes)?;
    let doc = aozora_rs::AozoraDocument::from_str(&decoded.text, None)?;
    let (xhtml, warnings) = doc.xhtml()?;
    let mut out = String::new();
    out.push_str(&format!("<!-- warnings:{} -->\n", warnings.len()));
    for (_, page) in xhtml.xhtmls {
        out.push_str(&page);
        out.push('\n');
    }
    Ok(out)
}

fn build_validation_aat(decoded: &DecodedSource) -> serde_json::Value {
    let body = body_text(&decoded.text);
    json!({
        "version": 1,
        "work_id": "stdin",
        "blocks": [
            {
                "kind": "paragraph",
                "content": parse_inline_content(body)
            }
        ],
        "meta": {
            "adapter": "aozora-rs",
            "adapter_version": VERSION,
            "source_encoding": decoded.encoding,
            "source_hash": decoded.source_hash,
            "parse_complete": true,
            "warnings": []
        }
    })
}

fn body_text(text: &str) -> &str {
    let mut separator_count = 0;
    let mut body_start = 0;
    let mut offset = 0;
    for line in text.split_inclusive('\n') {
        if line.trim_end_matches(['\r', '\n']).chars().all(|ch| ch == '-')
            && line.trim_end_matches(['\r', '\n']).chars().count() >= 20
        {
            separator_count += 1;
            if separator_count == 2 {
                body_start = offset + line.len();
                break;
            }
        }
        offset += line.len();
    }
    let body = &text[body_start..];
    let body_end = body
        .char_indices()
        .find_map(|(offset, _)| {
            let rest = &body[offset..];
            if rest.starts_with("底本：") || rest.starts_with("底本:") {
                Some(offset)
            } else {
                None
            }
        })
        .unwrap_or(body.len());
    &body[..body_end]
}

fn parse_inline_content(text: &str) -> Vec<serde_json::Value> {
    let mut content = vec![json!({
        "kind": "text",
        "value": source_visible_text(text),
        "span": span_for(text, 0, text.len())
    })];
    append_ruby_annotations(&mut content, text);
    append_gaiji_annotations(&mut content, text);
    content
}

fn append_ruby_annotations(content: &mut Vec<serde_json::Value>, text: &str) {
    let marker = Regex::new(r"《([^》]+)》").unwrap();
    for capture in marker.captures_iter(text) {
        content.push(json!({
            "kind": "ruby",
            "base": "",
            "reading": capture.get(1).unwrap().as_str()
        }));
    }
}

fn append_gaiji_annotations(content: &mut Vec<serde_json::Value>, text: &str) {
    let marker = Regex::new(r"※(?:［＃([^］]+)］|\[#([^\]]+)\])").unwrap();
    for capture in marker.captures_iter(text) {
        let description = capture
            .get(1)
            .or_else(|| capture.get(2))
            .map(|matched| matched.as_str())
            .unwrap_or_default();
        content.push(json!({
            "kind": "gaiji",
            "description": description,
            "resolved": "",
            "jis_code": null,
            "unresolved_reason": null
        }));
    }
}

fn source_visible_text(text: &str) -> String {
    let gaiji = Regex::new(r"※(?:［＃([^］]+)］|\[#([^\]]+)\])").unwrap();
    let ruby = Regex::new(r"｜?([^｜\s《》※［＃\[\]］、。，．「」『』（）()]+)《[^》]+》").unwrap();
    let command = Regex::new(r"［＃[^］]+］|\[#[^\]]+\]").unwrap();
    let without_gaiji = gaiji.replace_all(text, |captures: &regex::Captures<'_>| {
        captures
            .get(1)
            .or_else(|| captures.get(2))
            .map(|matched| matched.as_str())
            .unwrap_or_default()
            .to_owned()
    });
    let without_ruby = ruby.replace_all(&without_gaiji, "$1");
    command.replace_all(&without_ruby, "").into_owned()
}

fn span_for(text: &str, start: usize, end: usize) -> Span {
    let line = text[..start].chars().filter(|ch| *ch == '\n').count() + 1;
    Span {
        line_start: line,
        line_end: line,
        byte_start: text[..start].len(),
        byte_end: text[..end].len(),
    }
}

fn hex_sha256(bytes: &[u8]) -> String {
    let mut hasher = Sha256::new();
    hasher.update(bytes);
    format!("{:x}", hasher.finalize())
}
```

This mirrors the current `aozora2` validation adapter shape intentionally. It proves `aozora-rs` can be invoked for every source and gives `ab-check` comparable body-visible AAT. A later plan should replace the validation AAT builder with direct retokenized-node mapping.

- [ ] **Step 4: Implement the CLI**

Create `adapters/aozora-rs/src/main.rs`:

```rust
use std::io::{self, Read, Write};

use anyhow::Result;
use aozora_rs_adapter::{VERSION, aat_json_from_bytes, html_from_bytes};
use clap::{Parser, ValueEnum};

#[derive(Debug, Parser)]
struct Args {
    #[arg(long)]
    mode: Option<Mode>,

    #[arg(long)]
    version: bool,
}

#[derive(Debug, Clone, ValueEnum)]
enum Mode {
    Aat,
    Html,
}

fn main() -> Result<()> {
    let args = Args::parse();
    if args.version {
        println!("{VERSION}");
        return Ok(());
    }

    let mut bytes = Vec::new();
    io::stdin().read_to_end(&mut bytes)?;
    match args.mode.unwrap_or(Mode::Aat) {
        Mode::Aat => io::stdout().write_all(&aat_json_from_bytes(&bytes)?)?,
        Mode::Html => print!("{}", html_from_bytes(&bytes)?),
    }
    Ok(())
}
```

- [ ] **Step 5: Add the real adapter benchmark**

Create `adapters/aozora-rs/benches/adapter_bench.rs`:

```rust
use aozora_rs_adapter::aat_json_from_bytes;
use criterion::{Criterion, criterion_group, criterion_main};

fn bench_adapter_aat(c: &mut Criterion) {
    let input = large_aozora_text();
    c.bench_function("aozora_rs_adapter_aat_json_large", |b| {
        b.iter(|| aat_json_from_bytes(input.as_bytes()).unwrap())
    });
}

fn large_aozora_text() -> String {
    let mut text = String::from("タイトル\n著者\n--------------------\n凡例\n--------------------\n");
    for idx in 0..20_000 {
        text.push_str(&format!(
            "吾輩《わがはい》は※［＃「口＋世」、U+546D］である。第{idx}行。\n"
        ));
    }
    text.push_str("底本：ベンチ\n");
    text
}

criterion_group!(benches, bench_adapter_aat);
criterion_main!(benches);
```

- [ ] **Step 6: Verify and commit**

Run:

```bash
cargo fmt --manifest-path adapters/aozora-rs/Cargo.toml
cargo test --manifest-path adapters/aozora-rs/Cargo.toml
cargo clippy --manifest-path adapters/aozora-rs/Cargo.toml --all-targets -- -D warnings
cargo bench --manifest-path adapters/aozora-rs/Cargo.toml --no-run
```

Expected: all commands pass.

Commit:

```bash
git add adapters/aozora-rs
git commit -m "feat: add aozora-rs adapter"
```

---

## Task 2: Validate `aozora-rs` on a Focused Sample

**Files:**
- Modify: `benchmarks/README.md`

- [ ] **Step 1: Build the existing index and adapter**

Run:

```bash
mkdir -p /tmp/ab-validator-aozora-rs-sample
cargo run --release -p ab-index -- \
  --corpus references/aozorabunko \
  --output /tmp/ab-validator-aozora-rs-sample/index.json
cargo build --release --manifest-path adapters/aozora-rs/Cargo.toml
```

Expected: `index.json` exists and `.works_count` is `17894`.

- [ ] **Step 2: Create a deterministic mixed sample**

Run:

```bash
jq '[.works[]
  | select((.features | index("ruby")) or (.features | index("gaiji")) or (.features | index("jisage_line")))
  | .id][0:100]' \
  /tmp/ab-validator-aozora-rs-sample/index.json \
  > /tmp/ab-validator-aozora-rs-sample/work-ids.json
```

Expected:

```bash
jq length /tmp/ab-validator-aozora-rs-sample/work-ids.json
# 100
```

- [ ] **Step 3: Run `ab-check` on the sample**

Run:

```bash
rm -rf /tmp/ab-validator-aozora-rs-sample/reports
cargo run --release -p ab-check -- \
  --index /tmp/ab-validator-aozora-rs-sample/index.json \
  --corpus references/aozorabunko \
  --work-ids /tmp/ab-validator-aozora-rs-sample/work-ids.json \
  --adapter adapters/aozora-rs/target/release/aozora-rs-adapter \
  --output /tmp/ab-validator-aozora-rs-sample/reports \
  --jobs 16 \
  --per-work-timeout 30s
```

Expected: 100 report JSON files.

- [ ] **Step 4: Inspect failures**

Run:

```bash
find /tmp/ab-validator-aozora-rs-sample/reports -type f -name '*.json' -print0 |
  xargs -0 jq -r '.results | to_entries[] | select(.value.pass == false) | .key' |
  sort | uniq -c | sort -nr
```

Expected: no output. If failures appear, fix only adapter contract, decoding, timeout, or schema issues in this task. Do not tune comparison semantics yet.

- [ ] **Step 5: Document the sample command**

Add to `benchmarks/README.md`:

```markdown
## Adapter Smoke Validation

Before a new adapter is run on the full corpus, validate a deterministic
100-work feature sample:

```bash
cargo run --release -p ab-check -- \
  --index /tmp/ab-validator-aozora-rs-sample/index.json \
  --corpus references/aozorabunko \
  --work-ids /tmp/ab-validator-aozora-rs-sample/work-ids.json \
  --adapter adapters/aozora-rs/target/release/aozora-rs-adapter \
  --output /tmp/ab-validator-aozora-rs-sample/reports \
  --jobs 16 \
  --per-work-timeout 30s
```
```

- [ ] **Step 6: Verify and commit**

Run:

```bash
cargo fmt --check
cargo test --manifest-path adapters/aozora-rs/Cargo.toml
```

Commit:

```bash
git add benchmarks/README.md adapters/aozora-rs
git commit -m "test: validate aozora-rs adapter sample"
```

---

## Task 3: Preserve AAT Artifacts from `ab-check`

**Files:**
- Modify: `crates/ab-check/src/main.rs`
- Modify: `crates/ab-check/src/check.rs`
- Modify: `crates/ab-check/tests/integration.rs`

`ab-compare` needs AAT outputs, not only validation reports. Add an optional `--aat-output` directory to `ab-check` batch mode.

- [ ] **Step 1: Add a failing integration test**

Add to `crates/ab-check/tests/integration.rs`:

```rust
#[test]
fn batch_can_write_adapter_aat_outputs() {
    let temp = tempfile::tempdir().unwrap();
    let corpus = temp.path().join("corpus");
    let files = corpus.join("cards/000001/files/1_ruby_1");
    std::fs::create_dir_all(&files).unwrap();
    std::fs::write(files.join("1_ruby_1.txt"), "吾輩《わがはい》は猫である。").unwrap();

    let index = temp.path().join("index.json");
    std::fs::write(&index, r#"{
      "works": [{
        "id": "000001_1",
        "txt_path": "cards/000001/files/1_ruby_1/1_ruby_1.txt",
        "features": ["ruby"]
      }]
    }"#).unwrap();

    let reports = temp.path().join("reports");
    let aats = temp.path().join("aats");
    let adapter = std::env::current_dir().unwrap().join("adapters/test-adapter/test-adapter");

    let status = std::process::Command::new(env!("CARGO_BIN_EXE_ab-check"))
        .arg("--index").arg(&index)
        .arg("--corpus").arg(&corpus)
        .arg("--adapter").arg(adapter)
        .arg("--output").arg(&reports)
        .arg("--aat-output").arg(&aats)
        .status()
        .unwrap();

    assert!(status.success());
    assert_eq!(
        std::fs::read_dir(aats.join("test-adapter")).unwrap().count(),
        1
    );
}
```

Add `tempfile` as a dev-dependency if the crate does not already use it.

Run:

```bash
cargo test -p ab-check batch_can_write_adapter_aat_outputs
```

Expected: FAIL because `--aat-output` is unknown.

- [ ] **Step 2: Extend CLI args**

In `crates/ab-check/src/main.rs`, add:

```rust
#[arg(long)]
aat_output: Option<PathBuf>,
```

Pass it into `BatchOptions`:

```rust
aat_output_dir: args.aat_output.as_deref(),
```

- [ ] **Step 3: Extend `BatchOptions` and write AAT files**

In `crates/ab-check/src/check.rs`, change `BatchOptions`:

```rust
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
```

Change `invoke_and_check` to return both report and adapter JSON:

```rust
struct AdapterCheckOutput {
    report: CheckReport,
    aat: Option<Value>,
}
```

On successful exit code `0` or `2`, store the mutated AAT value:

```rust
Ok(AdapterCheckOutput {
    report: check_value(&decoded.text, &aat, validator),
    aat: Some(aat),
})
```

On adapter errors, return `aat: None`.

Inside the batch loop, after writing the report, write AAT when requested:

```rust
if let (Some(root), Some(aat)) = (options.aat_output_dir, output.aat.as_ref()) {
    let out = root
        .join(&adapter_output_name)
        .join(report_filename(work));
    if let Some(parent) = out.parent() {
        fs::create_dir_all(parent)?;
    }
    let file = fs::File::create(out)?;
    serde_json::to_writer_pretty(file, aat)?;
}
```

- [ ] **Step 4: Verify and commit**

Run:

```bash
cargo fmt --check
cargo test -p ab-check batch_can_write_adapter_aat_outputs
cargo test --workspace
```

Commit:

```bash
git add crates/ab-check Cargo.toml Cargo.lock
git commit -m "feat: persist adapter AAT outputs"
```

---

## Task 4: Add `ab-compare` Report-Set Comparator

**Files:**
- Modify: `Cargo.toml`
- Create: `crates/ab-compare/Cargo.toml`
- Create: `crates/ab-compare/src/lib.rs`
- Create: `crates/ab-compare/src/main.rs`
- Create: `crates/ab-compare/tests/integration.rs`

- [ ] **Step 1: Add workspace member**

In root `Cargo.toml`:

```toml
members = [
    "crates/ab-index",
    "crates/ab-check",
    "crates/ab-compare",
]
```

- [ ] **Step 2: Create comparator manifest**

Create `crates/ab-compare/Cargo.toml`:

```toml
[package]
name = "ab-compare"
version.workspace = true
edition.workspace = true
license.workspace = true

[dependencies]
anyhow.workspace = true
clap.workspace = true
serde.workspace = true
serde_json.workspace = true
walkdir.workspace = true
```

- [ ] **Step 3: Write failing integration test**

Create `crates/ab-compare/tests/integration.rs`:

```rust
#[test]
fn compares_two_report_sets() {
    let temp = tempfile::tempdir().unwrap();
    let a = temp.path().join("a");
    let b = temp.path().join("b");
    std::fs::create_dir_all(&a).unwrap();
    std::fs::create_dir_all(&b).unwrap();

    std::fs::write(a.join("000001-abc.json"), report("aozora2", true)).unwrap();
    std::fs::write(b.join("000001-abc.json"), report("aozora-rs", false)).unwrap();

    let output = std::process::Command::new(env!("CARGO_BIN_EXE_ab-compare"))
        .arg("--reports-a").arg(&a)
        .arg("--reports-b").arg(&b)
        .arg("--output").arg(temp.path().join("summary.json"))
        .output()
        .unwrap();

    assert!(output.status.success(), "{}", String::from_utf8_lossy(&output.stderr));
    let summary: serde_json::Value =
        serde_json::from_slice(&std::fs::read(temp.path().join("summary.json")).unwrap()).unwrap();
    assert_eq!(summary["common_reports"], 1);
    assert_eq!(summary["only_a"], 0);
    assert_eq!(summary["only_b"], 0);
    assert_eq!(summary["result_differences"][0]["property"], "schema_valid");
}

fn report(adapter: &str, pass: bool) -> String {
    format!(r#"{{
      "adapter": "{adapter}",
      "adapter_version": "test",
      "work_id": "000001_1",
      "results": {{
        "schema_valid": {{
          "pass": {pass},
          "confidence": "strict"
        }}
      }}
    }}"#)
}
```

Add `tempfile.workspace = true` to root dependencies and `tempfile.workspace = true` to `crates/ab-compare` dev-dependencies if needed.

Run:

```bash
cargo test -p ab-compare
```

Expected: FAIL because the crate has no implementation.

- [ ] **Step 4: Implement comparison library**

Create `crates/ab-compare/src/lib.rs`:

```rust
use std::{
    collections::{BTreeMap, BTreeSet},
    fs,
    path::{Path, PathBuf},
};

use anyhow::{Context, Result};
use serde::{Deserialize, Serialize};
use walkdir::WalkDir;

#[derive(Debug, Deserialize)]
pub struct CheckReport {
    pub adapter: String,
    pub adapter_version: String,
    pub work_id: String,
    pub results: BTreeMap<String, CheckResult>,
}

#[derive(Debug, Deserialize)]
pub struct CheckResult {
    pub pass: bool,
    pub message: Option<String>,
    pub confidence: Option<String>,
}

#[derive(Debug, Serialize)]
pub struct CompareSummary {
    pub adapter_a: String,
    pub adapter_b: String,
    pub common_reports: usize,
    pub only_a: usize,
    pub only_b: usize,
    pub result_differences: Vec<ResultDifference>,
}

#[derive(Debug, Serialize)]
pub struct ResultDifference {
    pub work_id: String,
    pub property: String,
    pub a_pass: bool,
    pub b_pass: bool,
}

pub fn compare_report_dirs(a: &Path, b: &Path) -> Result<CompareSummary> {
    let reports_a = read_reports(a)?;
    let reports_b = read_reports(b)?;
    let keys_a = reports_a.keys().cloned().collect::<BTreeSet<_>>();
    let keys_b = reports_b.keys().cloned().collect::<BTreeSet<_>>();
    let common = keys_a.intersection(&keys_b).cloned().collect::<Vec<_>>();

    let mut result_differences = Vec::new();
    for key in &common {
        let report_a = &reports_a[key];
        let report_b = &reports_b[key];
        let properties = report_a
            .results
            .keys()
            .chain(report_b.results.keys())
            .cloned()
            .collect::<BTreeSet<_>>();
        for property in properties {
            let a_pass = report_a.results.get(&property).is_some_and(|r| r.pass);
            let b_pass = report_b.results.get(&property).is_some_and(|r| r.pass);
            if a_pass != b_pass {
                result_differences.push(ResultDifference {
                    work_id: report_a.work_id.clone(),
                    property,
                    a_pass,
                    b_pass,
                });
            }
        }
    }

    Ok(CompareSummary {
        adapter_a: reports_a.values().next().map(|r| r.adapter.clone()).unwrap_or_default(),
        adapter_b: reports_b.values().next().map(|r| r.adapter.clone()).unwrap_or_default(),
        common_reports: common.len(),
        only_a: keys_a.difference(&keys_b).count(),
        only_b: keys_b.difference(&keys_a).count(),
        result_differences,
    })
}

fn read_reports(root: &Path) -> Result<BTreeMap<String, CheckReport>> {
    let mut reports = BTreeMap::new();
    for entry in WalkDir::new(root) {
        let entry = entry?;
        if !entry.file_type().is_file() || entry.path().extension().is_none_or(|ext| ext != "json") {
            continue;
        }
        let bytes = fs::read(entry.path())
            .with_context(|| format!("failed to read {}", entry.path().display()))?;
        let report: CheckReport = serde_json::from_slice(&bytes)
            .with_context(|| format!("failed to parse {}", entry.path().display()))?;
        reports.insert(report_key(entry.path(), &report), report);
    }
    Ok(reports)
}

fn report_key(path: &Path, report: &CheckReport) -> String {
    path.file_name()
        .and_then(|name| name.to_str())
        .map(str::to_owned)
        .unwrap_or_else(|| report.work_id.clone())
}
```

- [ ] **Step 5: Implement CLI**

Create `crates/ab-compare/src/main.rs`:

```rust
use std::path::PathBuf;

use ab_compare::compare_report_dirs;
use anyhow::Result;
use clap::Parser;

#[derive(Debug, Parser)]
#[command(version, about = "Compare two ab-check report directories")]
struct Args {
    #[arg(long)]
    reports_a: PathBuf,

    #[arg(long)]
    reports_b: PathBuf,

    #[arg(long)]
    output: PathBuf,
}

fn main() -> Result<()> {
    let args = Args::parse();
    let summary = compare_report_dirs(&args.reports_a, &args.reports_b)?;
    if let Some(parent) = args.output.parent() {
        std::fs::create_dir_all(parent)?;
    }
    let file = std::fs::File::create(args.output)?;
    serde_json::to_writer_pretty(file, &summary)?;
    Ok(())
}
```

- [ ] **Step 6: Verify and commit**

Run:

```bash
cargo fmt --check
cargo test -p ab-compare
cargo test --workspace
cargo clippy --workspace --all-targets -- -D warnings
```

Commit:

```bash
git add Cargo.toml Cargo.lock crates/ab-compare
git commit -m "feat: compare parser validation reports"
```

---

## Task 5: Full-Corpus Parser Comparison Runner

**Files:**
- Create: `benchmarks/run-parser-comparison.sh`
- Modify: `benchmarks/README.md`

- [ ] **Step 1: Create the comparison runner**

Create `benchmarks/run-parser-comparison.sh`:

```bash
#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
corpus="${AB_CORPUS:-$repo_root/references/aozorabunko}"
jobs="${AB_BENCH_JOBS:-$(nproc)}"
timeout="${AB_BENCH_TIMEOUT:-30s}"
out_dir="${AB_BENCH_OUT:-/tmp/ab-validator-compare-$(date -u +%Y%m%dT%H%M%SZ)}"

mkdir -p "$out_dir"
cd "$repo_root"

cargo build --release --workspace
cargo build --release --manifest-path adapters/aozora2/Cargo.toml
cargo build --release --manifest-path adapters/aozora-rs/Cargo.toml

target/release/ab-index \
  --corpus "$corpus" \
  --output "$out_dir/index.json" \
  2> "$out_dir/index.stderr"

run_adapter() {
  local name="$1"
  local adapter="$2"
  local reports="$out_dir/reports/$name"
  local aats="$out_dir/aats/$name"
  local start end seconds
  start="$(date +%s%N)"
  target/release/ab-check \
    --index "$out_dir/index.json" \
    --corpus "$corpus" \
    --adapter "$adapter" \
    --output "$reports" \
    --aat-output "$aats" \
    --jobs "$jobs" \
    --per-work-timeout "$timeout" \
    2> "$out_dir/$name.stderr"
  end="$(date +%s%N)"
  seconds="$(awk -v start="$start" -v end="$end" 'BEGIN { printf "%.6f", (end - start) / 1000000000 }')"
  jq -n \
    --arg name "$name" \
    --argjson seconds "$seconds" \
    --argjson reports "$(find "$reports" -type f -name '*.json' | wc -l)" \
    --argjson failures "$(find "$reports" -type f -name '*.json' -print0 | xargs -0 jq -r '.results | to_entries[] | select(.value.pass == false) | .key' | wc -l)" \
    '{name: $name, seconds: $seconds, reports: $reports, failures: $failures}'
}

run_adapter aozora2 "$repo_root/adapters/aozora2/target/release/aozora2-adapter" \
  > "$out_dir/aozora2-summary.json"
run_adapter aozora-rs "$repo_root/adapters/aozora-rs/target/release/aozora-rs-adapter" \
  > "$out_dir/aozora-rs-summary.json"

target/release/ab-compare \
  --reports-a "$out_dir/reports/aozora2/aozora2-adapter" \
  --reports-b "$out_dir/reports/aozora-rs/aozora-rs-adapter" \
  --output "$out_dir/comparison.json"

jq -s '{
  generated_at: now | todate,
  corpus_hash: input_filename,
  aozora2: .[0],
  aozora_rs: .[1],
  comparison: .[2]
}' \
  "$out_dir/aozora2-summary.json" \
  "$out_dir/aozora-rs-summary.json" \
  "$out_dir/comparison.json" \
  > "$out_dir/summary.json"

cat "$out_dir/summary.json"
echo "summary: $out_dir/summary.json"
```

After creation:

```bash
chmod +x benchmarks/run-parser-comparison.sh
```

- [ ] **Step 2: Fix the final `jq` summary if needed**

Run:

```bash
bash -n benchmarks/run-parser-comparison.sh
```

Expected: no output.

If shell syntax passes but `jq` summary fails during the first run, replace the final `jq -s` block with:

```bash
jq -n \
  --slurpfile a "$out_dir/aozora2-summary.json" \
  --slurpfile b "$out_dir/aozora-rs-summary.json" \
  --slurpfile c "$out_dir/comparison.json" \
  --arg corpus_hash "$(jq -r '.corpus_hash' "$out_dir/index.json")" \
  '{
    generated_at: now | todate,
    corpus_hash: $corpus_hash,
    aozora2: $a[0],
    aozora_rs: $b[0],
    comparison: $c[0]
  }' > "$out_dir/summary.json"
```

- [ ] **Step 3: Document the runner**

Add to `benchmarks/README.md`:

```markdown
## Parser Comparison

After both adapters build and pass sample validation, run:

```bash
AB_CORPUS=references/aozorabunko \
AB_BENCH_JOBS=16 \
AB_BENCH_OUT=/tmp/ab-validator-compare-current \
benchmarks/run-parser-comparison.sh
```

The output directory contains the shared index, per-parser reports, optional
AAT artifacts, and `comparison.json` from `ab-compare`.
```

- [ ] **Step 4: Verify and commit**

Run:

```bash
bash -n benchmarks/run-parser-comparison.sh
cargo fmt --check
cargo test --workspace
```

Commit:

```bash
git add benchmarks/README.md benchmarks/run-parser-comparison.sh
git commit -m "bench: add parser comparison runner"
```

---

## Task 6: Run Full Comparison and Record Baseline

**Files:**
- Create: `benchmarks/baselines/YYYY-MM-DD-parser-comparison.json`

- [ ] **Step 1: Run the full comparison**

Run:

```bash
AB_CORPUS=references/aozorabunko \
AB_BENCH_JOBS=16 \
AB_BENCH_OUT=/tmp/ab-validator-compare-current \
benchmarks/run-parser-comparison.sh
```

Expected:

- `aozora2.reports == 17894`
- `aozora_rs.reports == 17894`
- `aozora2.failures == 0`
- `aozora_rs.failures == 0`
- `comparison.common_reports == 17894`

- [ ] **Step 2: Investigate any failures**

If `aozora_rs.failures > 0`, run:

```bash
find /tmp/ab-validator-compare-current/reports/aozora-rs -type f -name '*.json' -print0 |
  xargs -0 jq -r '.results | to_entries[] | select(.value.pass == false) | .key' |
  sort | uniq -c | sort -nr
```

Fix adapter schema, decoding, parser invocation, or timeout defects until no failures remain. Do not weaken shared `ab-check` properties to make one adapter pass.

- [ ] **Step 3: Record the baseline**

Copy the summary into a dated tracked baseline:

```bash
cp /tmp/ab-validator-compare-current/summary.json \
  benchmarks/baselines/$(date +%Y-%m-%d)-parser-comparison.json
```

Open the file and add:

```json
"host_note": "Local development machine baseline; compare relative changes on the same machine."
```

- [ ] **Step 4: Final verification**

Run:

```bash
cargo fmt --check
cargo test --workspace
cargo test --manifest-path adapters/aozora2/Cargo.toml --no-run
cargo test --manifest-path adapters/aozora-rs/Cargo.toml --no-run
cargo clippy --workspace --all-targets -- -D warnings
cargo clippy --manifest-path adapters/aozora2/Cargo.toml --all-targets -- -D warnings
cargo clippy --manifest-path adapters/aozora-rs/Cargo.toml --all-targets -- -D warnings
cargo bench --workspace --no-run
cargo bench --manifest-path adapters/aozora-rs/Cargo.toml --no-run
nix flake check --print-build-logs
```

- [ ] **Step 5: Commit**

```bash
git add benchmarks/baselines
git commit -m "bench: record aozora-rs parser comparison"
```

---

## Self-Review Notes

- Spec coverage: this implements phase 6 (`aozora-rs adapter + ab-check`) and phase 8's first usable slice (`ab-compare` over validation reports). It does not implement HTML render diff or full semantic AAT tree diff; those remain separate phases.
- Type consistency: the plan uses the existing AAT schema fields and existing report fields from `ab-check`.
- Risk: the initial `aozora-rs` AAT uses the current body-visible validation adapter strategy for comparability. It invokes `aozora-rs` so fatal parser failures surface, but it is not yet a rich structural AST mapper.
- Completion definition: the work is done only after both adapters run across the same 17,894 indexed readable works and a tracked comparison baseline exists.
