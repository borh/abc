# Aozora-rs Comparison Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add the next parser adapter (`aozora-rs`) and produce a measured, reproducible comparison against the existing `aozora2` adapter on the full readable Aozora Bunko corpus.

**Architecture:** Implement `aozora-rs-adapter` as an external Rust adapter, matching the existing adapter contract: stdin source bytes, `--mode aat`, `--mode html`, and `--version`. The adapter must derive AAT from `aozora-rs-core` tokenizer/scopenizer/retokenizer output, not from the shared regex projection used by the first validation adapter. Add a small `ab-compare` workspace crate that compares two sets of `ab-check` reports first; normalized AAT tree diff is explicitly out of scope for this increment and should be a follow-up once both adapters preserve structurally meaningful AAT artifacts.

**Tech Stack:** Rust 2024, `clap`, `anyhow`, `serde_json`, `encoding_rs`, `sha2`, `rayon`, `walkdir`, `criterion`; local path dependencies on `references/parsers/aozora-rs/aozora-rs/aozora-rs-core` and `references/parsers/aozora-rs/aozora-rs/aozora-rs-xhtml` at commit `dd380ee639ca317ac9092ef2ba554acdf70e3c8d`.

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
aozora-rs-core = { path = "../../references/parsers/aozora-rs/aozora-rs/aozora-rs-core" }
aozora-rs-xhtml = { path = "../../references/parsers/aozora-rs/aozora-rs/aozora-rs-xhtml" }
clap = { version = "4.5", features = ["derive"] }
encoding_rs = "0.8"
serde = { version = "1.0", features = ["derive"] }
serde_json = "1.0"
sha2 = "0.10"
winnow = "0.7"

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
        let input = "\
タイトル
著者
-------------------------------------------------------
凡例
-------------------------------------------------------
吾輩《わがはい》は※［＃「口＋世」、U+546D］である。"
            .as_bytes();
        let out = aat_json_from_bytes(input).unwrap();
        let value: serde_json::Value = serde_json::from_slice(&out).unwrap();

        assert_eq!(value["work_id"], "stdin");
        assert_eq!(value["meta"]["adapter"], "aozora-rs");
        assert_eq!(value["meta"]["source_encoding"], "utf-8");
        assert!(value["meta"]["parse_complete"].as_bool().unwrap());
        assert_eq!(value["blocks"][0]["kind"], "paragraph");
        assert!(value["blocks"][0]["content"]
            .as_array()
            .unwrap()
            .iter()
            .any(|node| node["kind"] == "ruby"));
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
use aozora_rs_core::{
    Break, Deco, Retokenized, parse_meta, retokenize, scopenize, tokenize,
};
use aozora_rs_xhtml::retokenized_to_xhtml;
use encoding_rs::SHIFT_JIS;
use serde::Serialize;
use serde_json::json;
use sha2::{Digest, Sha256};
use winnow::LocatingSlice;

pub const VERSION: &str =
    "aozora-rs-adapter 0.1.0 dd380ee639ca317ac9092ef2ba554acdf70e3c8d";

#[derive(Debug)]
pub struct DecodedSource {
    pub text: String,
    pub encoding: &'static str,
    pub source_hash: String,
}

#[derive(Debug)]
struct ParsedSource<'a> {
    body: &'a str,
    retokenized: Vec<Retokenized<'a>>,
    warnings: Vec<String>,
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
    let parsed = parse_with_aozora_rs(&decoded.text)?;
    let aat = build_aat(&decoded, &parsed);
    let mut out = Vec::new();
    serde_json::to_writer(&mut out, &aat)?;
    out.push(b'\n');
    Ok(out)
}

pub fn html_from_bytes(bytes: &[u8]) -> Result<String> {
    let decoded = decode_source_bytes(bytes)?;
    let parsed = parse_with_aozora_rs(&decoded.text)?;
    let warning_count = parsed.warnings.len();
    let xhtml = retokenized_to_xhtml(parsed.retokenized);
    let mut pages = xhtml.xhtmls.into_iter().collect::<Vec<_>>();
    pages.sort_by_key(|(page_id, _)| *page_id);
    let mut out = format!("<!-- warnings:{warning_count} -->\n");
    for (page_id, page) in pages {
        out.push_str(&format!("<!-- page:{page_id} -->\n"));
        out.push_str(&page);
        out.push('\n');
    }
    Ok(out)
}

fn parse_with_aozora_rs(text: &str) -> Result<ParsedSource<'_>> {
    let mut body = text;
    let mut warnings = Vec::new();
    if let Err(error) = parse_meta(&mut body) {
        warnings.push(format!("meta parse warning: {error}"));
    }

    let mut input = LocatingSlice::new(body);
    let tokenized = tokenize(&mut input)?;
    let ((scopenized, flat_tokens), scopenize_errors) = scopenize(tokenized).into_tuple();
    let (retokenized, retokenize_errors) = retokenize(flat_tokens, scopenized).into_tuple();
    warnings.extend(scopenize_errors.into_iter().map(|error| error.to_string()));
    warnings.extend(retokenize_errors.into_iter().map(|error| error.to_string()));

    Ok(ParsedSource {
        body,
        retokenized,
        warnings,
    })
}

fn build_aat(decoded: &DecodedSource, parsed: &ParsedSource<'_>) -> serde_json::Value {
    json!({
        "version": 1,
        "work_id": "stdin",
        "blocks": [
            {
                "kind": "paragraph",
                "content": retokenized_to_aat_content(&parsed.retokenized)
            }
        ],
        "meta": {
            "adapter": "aozora-rs",
            "adapter_version": VERSION,
            "source_encoding": decoded.encoding,
            "source_hash": decoded.source_hash,
            "parse_complete": parsed.warnings.is_empty(),
            "warnings": parsed.warnings.iter().map(|message| json!({ "message": message })).collect::<Vec<_>>()
        }
    })
}

fn retokenized_to_aat_content(tokens: &[Retokenized<'_>]) -> Vec<serde_json::Value> {
    let mut content = Vec::new();
    let mut idx = 0;
    while idx < tokens.len() {
        match &tokens[idx] {
            Retokenized::Text(text) => push_text(&mut content, text),
            Retokenized::Odoriji(odoriji) => push_text(&mut content, &odoriji.to_string()),
            Retokenized::Kunten(kunten) => push_text(&mut content, kunten),
            Retokenized::Okurigana(okurigana) => push_text(&mut content, okurigana),
            Retokenized::Break(Break::BreakLine) => push_text(&mut content, "\n"),
            Retokenized::Break(_) => push_text(&mut content, "\n"),
            Retokenized::Figure(figure) => content.push(json!({
                "kind": "gaiji",
                "description": figure.to_string(),
                "resolved": "",
                "jis_code": null,
                "unresolved_reason": null
            })),
            Retokenized::DecoBegin(Deco::Ruby(reading)) => {
                let (base, next_idx) = collect_decorated_visible_text(tokens, idx + 1, |deco| {
                    matches!(deco, Deco::Ruby(_))
                });
                content.push(json!({
                    "kind": "ruby",
                    "base": base,
                    "reading": reading
                }));
                idx = next_idx;
                continue;
            }
            Retokenized::DecoBegin(deco) => {
                let (value, next_idx) = collect_decorated_visible_text(tokens, idx + 1, |candidate| {
                    std::mem::discriminant(candidate) == std::mem::discriminant(deco)
                });
                content.push(json!({
                    "kind": "style",
                    "style_type": format!("{deco:?}"),
                    "content": [{"kind": "text", "value": value}]
                }));
                idx = next_idx;
                continue;
            }
            Retokenized::DecoEnd(_) => {}
        }
        idx += 1;
    }
    content
}

fn collect_decorated_visible_text(
    tokens: &[Retokenized<'_>],
    mut idx: usize,
    is_matching_end: impl Fn(&Deco<'_>) -> bool,
) -> (String, usize) {
    let mut value = String::new();
    let mut depth = 1;
    while idx < tokens.len() {
        match &tokens[idx] {
            Retokenized::Text(text) => value.push_str(text),
            Retokenized::Odoriji(odoriji) => value.push_str(&odoriji.to_string()),
            Retokenized::Kunten(kunten) => value.push_str(kunten),
            Retokenized::Okurigana(okurigana) => value.push_str(okurigana),
            Retokenized::Break(_) => value.push('\n'),
            Retokenized::Figure(figure) => value.push_str(&figure.to_string()),
            Retokenized::DecoBegin(_) => depth += 1,
            Retokenized::DecoEnd(deco) if depth == 1 && is_matching_end(deco) => {
                return (value, idx + 1);
            }
            Retokenized::DecoEnd(_) => depth -= 1,
        }
        idx += 1;
    }
    (value, idx)
}

fn push_text(content: &mut Vec<serde_json::Value>, value: &str) {
    if value.is_empty() {
        return;
    }
    content.push(json!({ "kind": "text", "value": value }));
}

fn hex_sha256(bytes: &[u8]) -> String {
    let mut hasher = Sha256::new();
    hasher.update(bytes);
    format!("{:x}", hasher.finalize())
}
```

This intentionally uses `aozora-rs-core` parser output for AAT generation. The mapping is still conservative, but a zero-difference result now means both parsers passed the same validation properties, not that two copies of the same regex projection agreed.

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

Expected: `index.json` exists and `.works_count` is greater than `17000`. Record the exact count because local `references/aozorabunko` mirrors can move.

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

Add `tempfile = "3"` to root `[workspace.dependencies]` and `tempfile.workspace = true` to `crates/ab-check` dev-dependencies if the crate does not already use it.

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

Replace the current batch loop body with this shape so the return type change is fully integrated:

```rust
pool.install(|| {
    works.par_iter().try_for_each(|work| -> Result<()> {
        let output = invoke_and_check(
            &adapter_path,
            &adapter_version,
            options.corpus_root,
            &work.txt_path,
            &work.id,
            options.timeout,
            &validator,
        )?;

        let report_out = options
            .output_dir
            .join(&adapter_output_name)
            .join(report_filename(work));
        if let Some(parent) = report_out.parent() {
            fs::create_dir_all(parent)?;
        }
        write_report(&output.report, Some(&report_out))?;

        if let (Some(root), Some(aat)) = (options.aat_output_dir, output.aat.as_ref()) {
            let aat_out = root
                .join(&adapter_output_name)
                .join(report_filename(work));
            if let Some(parent) = aat_out.parent() {
                fs::create_dir_all(parent)?;
            }
            let file = fs::File::create(aat_out)?;
            serde_json::to_writer_pretty(file, aat)?;
        }

        Ok(())
    })
})
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

[workspace.dependencies]
tempfile = "3"
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

[dev-dependencies]
tempfile.workspace = true
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

The manifest changes above add the required `tempfile` test dependency.

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
    path::Path,
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
        let key = duplicate_safe_report_key(&reports, entry.path(), &report);
        reports.insert(key, report);
    }
    Ok(reports)
}

fn duplicate_safe_report_key(
    reports: &BTreeMap<String, CheckReport>,
    path: &Path,
    report: &CheckReport,
) -> String {
    if !reports.contains_key(&report.work_id) {
        return report.work_id.clone();
    }
    let filename = path
        .file_name()
        .and_then(|name| name.to_str())
        .unwrap_or("duplicate");
    format!("{}::{filename}", report.work_id)
}
```

Use `work_id` as the primary identity. The filename suffix is only a duplicate
guard for corpus entries that share an Aozora card/work ID.

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

cat "$out_dir/summary.json"
echo "summary: $out_dir/summary.json"
```

After creation:

```bash
chmod +x benchmarks/run-parser-comparison.sh
```

- [ ] **Step 2: Check shell syntax**

Run:

```bash
bash -n benchmarks/run-parser-comparison.sh
```

Expected: no output.

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

- `aozora2.reports == jq '.works_count' /tmp/ab-validator-compare-current/index.json`
- `aozora_rs.reports == jq '.works_count' /tmp/ab-validator-compare-current/index.json`
- `aozora2.failures == 0`
- `aozora_rs.failures == 0`
- `comparison.common_reports == jq '.works_count' /tmp/ab-validator-compare-current/index.json`

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
- Risk: the initial `aozora-rs` AAT maps retokenized parser output into the current AAT schema, but it is still a conservative structural mapper. Full tree-diff semantics and HTML render diff remain later phases.
- Completion definition: the work is done only after both adapters run across the same indexed readable work count from the local corpus and a tracked comparison baseline exists.
