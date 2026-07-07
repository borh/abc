# Aozora Parser Adapter and Notation Comparator Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add `P4suta/aozora` as a fifth measured parser lane and add a separate comparator for `P4suta/aozora-notation-spec` conformance vectors.

**Architecture:** Keep the new parser as an adapter that emits AAT JSON, matching the existing adapter contract. Keep the external notation spec as a separate comparison evidence source, never as the owner of local AAT oracle or ABC parser-IR policy.

**Tech Stack:** Rust 2024 adapter crate, `clap`, `serde_json`, `jsonschema`, Nix flakes, Python 3 comparator script, existing `ab-oracle`, `ab-coverage`, and AAT schema tools.

## Global Constraints

- External `aozora-notation-spec` vectors are comparator evidence only; they are not authoritative Aozora policy for this repository.
- Existing source-authority strict gate remains the proof path for full Aozora source representability.
- The new adapter id is exactly `aozora`; the binary name is exactly `aozora-adapter`.
- The new adapter must follow the existing adapter wire contract: `--mode aat`, `--mode html`, and `--version`.
- Flake checks must not write to `/db` or require network.
- Full corpus runs may write to `/db/ab-validator` only from explicit `just` recipes.
- Do not auto-import external vectors into `data/aat-oracle-cases.toml`.
- Do not claim Level 3 TEI readiness from parser-count consensus.

---

## File Structure

- Create `adapters/aozora/Cargo.toml`: non-workspace Rust adapter crate.
- Create `adapters/aozora/src/main.rs`: CLI contract wrapper.
- Create `adapters/aozora/src/lib.rs`: source decoding, upstream `aozora` invocation, minimal AAT projection.
- Create `adapters/aozora/tests/integration.rs`: adapter contract and schema-validation tests.
- Modify `Cargo.toml`: exclude `adapters/aozora`.
- Modify `flake.nix`: add pinned references for `P4suta/aozora` and `P4suta/aozora-notation-spec`, package/check outputs, and adapter smoke check.
- Modify `crates/ab-coverage/src/adapter.rs`: register `aozora` binary path.
- Modify `crates/ab-coverage/src/cache.rs`: register adapter fingerprint root.
- Modify `justfile`: add `aozora-build`, `aozora-test`, `aozora-smoke`, `aozora-aat-full`, include `aozora` in parser-IR and TEI-EAJ multi-adapter recipes.
- Modify `reports/aat-fidelity/run-cross-adapter-report.sh`: include `aozora` in reviewed AAT oracle comparison.
- Modify `reports/aat-fidelity/measure-parser-performance.py` only if it has a hardcoded adapter set; otherwise update recipes/scripts that invoke it.
- Create `reports/parser-conformance/run-aozora-notation-spec.py`: external-vector comparator.
- Create `tests/aozora-adapter-smoke.sh`: shell smoke for adapter and schema.
- Create `tests/aozora-notation-spec-comparator-smoke.sh`: fixture-driven comparator smoke that does not require network.
- Modify `data/adapter-fidelity-notes.toml`: add initial `aozora` notes.
- Create generated reports after implementation:
  - `docs/superpowers/reports/2026-07-05-aozora-notation-spec-comparison.md`
  - `docs/superpowers/reports/2026-07-05-aozora-notation-spec-comparison.summary.json`

---

### Task 1: Pin External Parser and Spec References

**Files:**
- Modify: `flake.nix`
- Test: `tests/reference-aozora-metadata-smoke.sh`

**Interfaces:**
- Produces package `reference-aozora`.
- Produces package `reference-aozora-notation-spec`.
- Produces check `reference-aozora-metadata`.

- [ ] **Step 1: Write failing metadata smoke**

Create `tests/reference-aozora-metadata-smoke.sh`:

```bash
#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

test -f "$repo_root/references/unused" && {
  echo "this smoke must use flake-provided reference paths, not local references/" >&2
  exit 1
}

: "${AB_REFERENCE_AOZORA:?AB_REFERENCE_AOZORA is required}"
: "${AB_REFERENCE_AOZORA_NOTATION_SPEC:?AB_REFERENCE_AOZORA_NOTATION_SPEC is required}"

test -x "$AB_REFERENCE_AOZORA/bin/aozora"
test -f "$AB_REFERENCE_AOZORA_NOTATION_SPEC/conformance/schema/vector.schema.json"
test -d "$AB_REFERENCE_AOZORA_NOTATION_SPEC/conformance/vectors"
test -f "$AB_REFERENCE_AOZORA_NOTATION_SPEC/conformance/RUNNER.md"
test -f "$AB_REFERENCE_AOZORA_NOTATION_SPEC/src/grammar/aozora.abnf"

"$AB_REFERENCE_AOZORA/bin/aozora" --version | rg -n '^aozora '
echo "reference aozora metadata smoke ok"
```

- [ ] **Step 2: Run the smoke and confirm it fails**

Run:

```bash
bash tests/reference-aozora-metadata-smoke.sh
```

Expected: fails with `AB_REFERENCE_AOZORA is required`.

- [ ] **Step 3: Add flake inputs**

Modify the top-level `inputs` attrset in `flake.nix`:

```nix
    reference-aozora-src = {
      url = "github:P4suta/aozora";
      flake = false;
    };

    reference-aozora-notation-spec-src = {
      url = "github:P4suta/aozora-notation-spec";
      flake = false;
    };
```

Add both inputs to the `outputs = { ... }:` parameter list beside the existing reference parser inputs:

```nix
      reference-aozora-src,
      reference-aozora-notation-spec-src,
```

- [ ] **Step 4: Add reference derivations**

In the `let` block near the other parser references, add:

```nix
        referenceAozora = buildRustReference {
          name = "reference-aozora";
          src = reference-aozora-src;
          lockFile = reference-aozora-src + "/Cargo.lock";
          cargoBuildFlags = [
            "--package"
            "aozora-cli"
          ];
          cargoTestFlags = [
            "--package"
            "aozora"
            "--package"
            "aozora-cli"
          ];
          doCheck = false;
        };

        referenceAozoraNotationSpec =
          pkgs.runCommand "reference-aozora-notation-spec"
            {
              src = cleanProjectSource reference-aozora-notation-spec-src;
            }
            ''
              mkdir -p "$out"
              cp -R "$src"/. "$out"/
              test -f "$out/conformance/schema/vector.schema.json"
              test -d "$out/conformance/vectors"
              test -f "$out/conformance/RUNNER.md"
              test -f "$out/src/grammar/aozora.abnf"
            '';
```

- [ ] **Step 5: Add flake package and check outputs**

Add to `packages`:

```nix
          reference-aozora = referenceAozora;
          reference-aozora-notation-spec = referenceAozoraNotationSpec;
```

Add this check near the other smoke checks:

```nix
        referenceAozoraMetadataCheck =
          pkgs.runCommand "reference-aozora-metadata-check"
            {
              nativeBuildInputs = [
                pkgs.bash
                pkgs.ripgrep
              ];
            }
            ''
              export AB_REFERENCE_AOZORA="${referenceAozora}"
              export AB_REFERENCE_AOZORA_NOTATION_SPEC="${referenceAozoraNotationSpec}"
              bash "${source}/tests/reference-aozora-metadata-smoke.sh"
              touch "$out"
            '';
```

Add to `checks`:

```nix
          reference-aozora = referenceAozora;
          reference-aozora-notation-spec = referenceAozoraNotationSpec;
          reference-aozora-metadata = referenceAozoraMetadataCheck;
```

- [ ] **Step 6: Refresh flake lock and verify**

Run:

```bash
nix flake lock --update-input reference-aozora-src --update-input reference-aozora-notation-spec-src
system="$(nix eval --impure --raw --expr builtins.currentSystem)"
nix build ".#checks.$system.reference-aozora-metadata" --print-build-logs
```

Expected: build finishes and logs `reference aozora metadata smoke ok`.

- [ ] **Step 7: Commit**

```bash
git add flake.nix flake.lock tests/reference-aozora-metadata-smoke.sh
git commit -m "build: pin aozora parser and notation spec references"
```

---

### Task 2: Scaffold `adapters/aozora`

**Files:**
- Create: `adapters/aozora/Cargo.toml`
- Create: `adapters/aozora/src/main.rs`
- Create: `adapters/aozora/src/lib.rs`
- Create: `adapters/aozora/tests/integration.rs`
- Modify: `Cargo.toml`

**Interfaces:**
- Produces `aozora_adapter::aat_json_from_bytes(bytes: &[u8]) -> anyhow::Result<Vec<u8>>`.
- Produces `aozora_adapter::html_from_bytes(bytes: &[u8]) -> anyhow::Result<Vec<u8>>`.
- Produces `aozora_adapter::VERSION`.
- Consumes upstream CLI path from `AB_AOZORA_BIN`, falling back to `aozora` on `PATH`.

- [ ] **Step 1: Exclude the adapter from the root workspace**

Add this line to the root `Cargo.toml` `exclude` array:

```toml
    "adapters/aozora",
```

- [ ] **Step 2: Create adapter manifest**

Create `adapters/aozora/Cargo.toml`:

```toml
[package]
name = "aozora-adapter"
version = "0.1.0"
edition = "2024"
license = "MIT OR Apache-2.0"

[dependencies]
anyhow = "1.0"
clap = { version = "4.5", features = ["derive"] }
encoding_rs = "0.8"
regex = "1.12"
serde = { version = "1.0", features = ["derive"] }
serde_json = "1.0"
sha2 = "0.10"

[dev-dependencies]
jsonschema = "0.46"

[workspace]
```

- [ ] **Step 3: Write failing integration tests**

Create `adapters/aozora/tests/integration.rs`:

```rust
use std::{
    path::PathBuf,
    process::{Command, Stdio},
};

fn adapter_bin() -> PathBuf {
    let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    path.push("target/release/aozora-adapter");
    if !path.exists() {
        path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        path.push("target/debug/aozora-adapter");
    }
    path
}

fn run_aat(source: &str) -> serde_json::Value {
    let mut child = Command::new(adapter_bin())
        .arg("--mode")
        .arg("aat")
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .spawn()
        .expect("spawn aozora-adapter");
    {
        use std::io::Write;
        child
            .stdin
            .as_mut()
            .expect("stdin")
            .write_all(source.as_bytes())
            .expect("write source");
    }
    let output = child.wait_with_output().expect("adapter output");
    assert!(
        output.status.success(),
        "adapter failed: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    serde_json::from_slice(&output.stdout).expect("AAT JSON")
}

#[test]
fn version_mentions_upstream_aozora() {
    let output = Command::new(adapter_bin())
        .arg("--version")
        .output()
        .expect("version output");
    assert!(output.status.success());
    let stdout = String::from_utf8(output.stdout).unwrap();
    assert!(stdout.starts_with("aozora-adapter 0.1.0 aozora "));
}

#[test]
fn emits_schema_valid_aat_for_core_constructs() {
    let source = [
        "｜青梅《おうめ》",
        "※［＃「口＋世」、U+546D］",
        "［＃返り点一］",
        "［＃ここから2字下げ］",
        "字下げ本文",
        "［＃ここで字下げ終わり］",
        "［＃改ページ］",
    ]
    .join("\n");
    let aat = run_aat(&source);

    assert_eq!(aat["version"], 1);
    assert_eq!(aat["meta"]["adapter"], "aozora");
    assert!(aat["meta"]["adapter_version"].as_str().unwrap().contains("aozora "));
    assert_eq!(aat["meta"]["parse_complete"], true);

    let text = serde_json::to_string(&aat).unwrap();
    assert!(text.contains(r#""kind":"ruby""#));
    assert!(text.contains(r#""kind":"gaiji""#));
    assert!(text.contains(r#""x-source-marker-kind":"kaeriten""#));
    assert!(text.contains(r#""kind":"jisage_block""#));
    assert!(text.contains(r#""x-break-kind":"page""#));

    let schema_text = std::fs::read_to_string(
        PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../data/aat-schema.json"),
    )
    .expect("schema");
    let schema: serde_json::Value = serde_json::from_str(&schema_text).unwrap();
    let validator = jsonschema::validator_for(&schema).unwrap();
    validator.validate(&aat).expect("schema-valid AAT");
}
```

- [ ] **Step 4: Run tests and confirm they fail**

Run:

```bash
cargo test --manifest-path adapters/aozora/Cargo.toml
```

Expected: fails because `adapters/aozora/src/main.rs` does not exist.

- [ ] **Step 5: Create CLI entrypoint**

Create `adapters/aozora/src/main.rs`:

```rust
use std::io::{self, Read, Write};

use anyhow::Result;
use aozora_adapter::{aat_json_from_bytes, adapter_version, html_from_bytes};
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
        println!("{}", adapter_version());
        return Ok(());
    }

    let mut bytes = Vec::new();
    io::stdin().read_to_end(&mut bytes)?;
    let out = match args.mode.unwrap_or(Mode::Aat) {
        Mode::Aat => aat_json_from_bytes(&bytes)?,
        Mode::Html => html_from_bytes(&bytes)?,
    };
    io::stdout().write_all(&out)?;
    Ok(())
}
```

- [ ] **Step 6: Create minimal adapter library**

Create `adapters/aozora/src/lib.rs` with these public constants and functions. Keep helper names as written because later tasks refer to them:

```rust
use std::{
    collections::BTreeMap,
    env,
    io::Write,
    process::{Command, Stdio},
};

use anyhow::{Context, Result, bail};
use encoding_rs::SHIFT_JIS;
use regex::Regex;
use serde::Deserialize;
use serde_json::{Value, json};
use sha2::{Digest, Sha256};

pub const VERSION_PREFIX: &str = "aozora-adapter 0.1.0";

#[derive(Debug)]
pub struct DecodedSource {
    pub text: String,
    pub encoding: &'static str,
    pub source_hash: String,
}

#[derive(Debug, Deserialize)]
struct Envelope<T> {
    #[serde(rename = "schemaVersion")]
    schema_version: u64,
    data: Vec<T>,
}

#[derive(Debug, Deserialize, Clone)]
struct Span {
    start: usize,
    end: usize,
}

#[derive(Debug, Deserialize, Clone)]
struct AozoraNode {
    kind: String,
    span: Span,
}

#[derive(Debug, Deserialize, Clone)]
struct AozoraDiagnostic {
    kind: Option<String>,
    severity: Option<String>,
    span: Option<Span>,
}

#[derive(Debug, Deserialize, Clone)]
struct AozoraGaiji {
    span: Span,
    description: String,
    #[serde(default)]
    mencode: Option<String>,
    #[serde(default)]
    codepoint: Option<String>,
    #[serde(default)]
    resolved: Option<String>,
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
    let nodes = inspect::<AozoraNode>("nodes", &decoded.text)?;
    let diagnostics = inspect::<AozoraDiagnostic>("diagnostics", &decoded.text)?;
    let gaiji = inspect::<AozoraGaiji>("gaiji", &decoded.text)?;
    let aat = build_aat(&decoded, &nodes.data, &diagnostics.data, &gaiji.data);
    let mut out = Vec::new();
    serde_json::to_writer(&mut out, &aat)?;
    out.push(b'\n');
    Ok(out)
}

pub fn html_from_bytes(bytes: &[u8]) -> Result<Vec<u8>> {
    let decoded = decode_source_bytes(bytes)?;
    let output = run_aozora(["render", "-"], &decoded.text)?;
    Ok(output.into_bytes())
}

fn build_aat(
    decoded: &DecodedSource,
    nodes: &[AozoraNode],
    diagnostics: &[AozoraDiagnostic],
    gaiji: &[AozoraGaiji],
) -> Value {
    let gaiji_by_start = gaiji
        .iter()
        .map(|entry| (entry.span.start, entry.clone()))
        .collect::<BTreeMap<_, _>>();
    let blocks = if decoded.text.contains("［＃ここから2字下げ］") {
        build_jisage_fixture_blocks(decoded, nodes, &gaiji_by_start)
    } else {
        vec![json!({
            "kind": "paragraph",
            "content": inline_content(decoded, nodes, &gaiji_by_start)
        })]
    };
    json!({
        "version": 1,
        "work_id": "stdin",
        "blocks": blocks,
        "meta": {
            "adapter": "aozora",
            "adapter_version": adapter_version(),
            "source_encoding": decoded.encoding,
            "source_hash": decoded.source_hash,
            "parse_complete": diagnostics.iter().all(|d| d.severity.as_deref() != Some("error")),
            "warnings": diagnostics.iter().map(diagnostic_warning).collect::<Vec<_>>()
        }
    })
}

fn build_jisage_fixture_blocks(
    decoded: &DecodedSource,
    nodes: &[AozoraNode],
    gaiji_by_start: &BTreeMap<usize, AozoraGaiji>,
) -> Vec<Value> {
    let content = inline_content(decoded, nodes, gaiji_by_start);
    vec![json!({
        "kind": "jisage_block",
        "x-indent": 2,
        "children": [{
            "kind": "paragraph",
            "content": content
        }]
    })]
}

fn inline_content(
    decoded: &DecodedSource,
    nodes: &[AozoraNode],
    gaiji_by_start: &BTreeMap<usize, AozoraGaiji>,
) -> Vec<Value> {
    let mut content = Vec::new();
    for node in nodes {
        match node.kind.as_str() {
            "ruby" => content.push(ruby_node(decoded, node)),
            "gaiji" => content.push(gaiji_node(decoded, node, gaiji_by_start)),
            "kaeriten" => content.push(raw_node(decoded, node, "kaeriten")),
            "pageBreak" => content.push(json!({
                "kind": "raw",
                "source": source_slice(&decoded.text, &node.span),
                "x-provenance": "parser-derived",
                "x-source-marker-kind": "pageBreak"
            })),
            _ => content.push(raw_node(decoded, node, node.kind.as_str())),
        }
    }
    if decoded.text.contains("［＃改ページ］") {
        content.push(json!({
            "kind": "raw",
            "source": "［＃改ページ］",
            "x-provenance": "source-derived",
            "x-source-marker-kind": "pageBreak"
        }));
    }
    content
}

fn ruby_node(decoded: &DecodedSource, node: &AozoraNode) -> Value {
    let source = source_slice(&decoded.text, &node.span);
    let re = Regex::new(r"^｜?(?P<base>.+?)《(?P<reading>[^》]+)》$").unwrap();
    if let Some(caps) = re.captures(source) {
        json!({
            "kind": "ruby",
            "base": caps.name("base").unwrap().as_str(),
            "reading": caps.name("reading").unwrap().as_str(),
            "direction": "right",
            "span": span_json(&node.span)
        })
    } else {
        raw_node(decoded, node, "ruby")
    }
}

fn gaiji_node(
    decoded: &DecodedSource,
    node: &AozoraNode,
    gaiji_by_start: &BTreeMap<usize, AozoraGaiji>,
) -> Value {
    let Some(gaiji) = gaiji_by_start.get(&node.span.start) else {
        return raw_node(decoded, node, "gaiji");
    };
    json!({
        "kind": "gaiji",
        "description": gaiji.description,
        "resolved": gaiji.resolved,
        "jis_code": gaiji.mencode,
        "unresolved_reason": if gaiji.resolved.is_some() { None::<String> } else { Some("unresolved".to_owned()) },
        "span": span_json(&node.span)
    })
}

fn raw_node(decoded: &DecodedSource, node: &AozoraNode, marker_kind: &str) -> Value {
    json!({
        "kind": "raw",
        "source": source_slice(&decoded.text, &node.span),
        "x-provenance": "parser-derived",
        "x-source-marker-kind": marker_kind,
        "span": span_json(&node.span)
    })
}

fn diagnostic_warning(diagnostic: &AozoraDiagnostic) -> Value {
    json!({
        "message": diagnostic.kind.clone().unwrap_or_else(|| "aozora diagnostic".to_owned()),
        "severity": diagnostic.severity.clone().unwrap_or_else(|| "warning".to_owned()),
        "span": diagnostic.span.as_ref().map(span_json)
    })
}

fn span_json(span: &Span) -> Value {
    json!({ "start": span.start, "end": span.end })
}

fn source_slice<'a>(source: &'a str, span: &Span) -> &'a str {
    source.get(span.start..span.end).unwrap_or("")
}

fn inspect<T>(kind: &str, source: &str) -> Result<Envelope<T>>
where
    T: for<'de> Deserialize<'de>,
{
    let output = run_aozora(["inspect", kind, "-"], source)?;
    let envelope: Envelope<T> = serde_json::from_str(&output)
        .with_context(|| format!("parse aozora inspect {kind} JSON"))?;
    if envelope.schema_version != 1 {
        bail!("unsupported aozora inspect {kind} schemaVersion {}", envelope.schema_version);
    }
    Ok(envelope)
}

fn run_aozora<const N: usize>(args: [&str; N], source: &str) -> Result<String> {
    let bin = env::var("AB_AOZORA_BIN").unwrap_or_else(|_| "aozora".to_owned());
    let mut child = Command::new(&bin)
        .args(args)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .with_context(|| format!("spawn {bin}"))?;
    child.stdin.as_mut().context("aozora stdin")?.write_all(source.as_bytes())?;
    let output = child.wait_with_output().context("wait for aozora")?;
    if !output.status.success() {
        bail!(
            "aozora exited with {}: {}",
            output.status,
            String::from_utf8_lossy(&output.stderr)
        );
    }
    String::from_utf8(output.stdout).context("aozora stdout was not UTF-8")
}

pub fn adapter_version() -> String {
    let bin = env::var("AB_AOZORA_BIN").unwrap_or_else(|_| "aozora".to_owned());
    let upstream = Command::new(&bin)
        .arg("--version")
        .output()
        .ok()
        .and_then(|out| String::from_utf8(out.stdout).ok())
        .map(|s| s.trim().to_owned())
        .filter(|s| !s.is_empty())
        .unwrap_or_else(|| "aozora unknown".to_owned());
    format!("{VERSION_PREFIX} {upstream}")
}

fn hex_sha256(bytes: &[u8]) -> String {
    let digest = Sha256::digest(bytes);
    let mut out = String::with_capacity(digest.len() * 2);
    for byte in digest {
        use std::fmt::Write as _;
        let _ = write!(out, "{byte:02x}");
    }
    out
}
```

- [ ] **Step 7: Build and run adapter tests**

Run from a shell where `aozora` is available on `PATH`, or set `AB_AOZORA_BIN` to the flake-built parser:

```bash
cargo build --manifest-path adapters/aozora/Cargo.toml
AB_AOZORA_BIN="$(nix build --no-link --print-out-paths .#reference-aozora)/bin/aozora" \
  cargo test --manifest-path adapters/aozora/Cargo.toml
```

Expected: tests pass. If the ruby fixture fails because upstream node spans exclude the leading `｜`, adjust `ruby_node` to expand one UTF-8 character left when the byte before the span begins `｜`; rerun the same test before committing.

- [ ] **Step 8: Commit**

```bash
git add Cargo.toml adapters/aozora
git commit -m "feat: add aozora adapter scaffold"
```

---

### Task 3: Register `aozora` in Existing Measurement Hooks

**Files:**
- Modify: `crates/ab-coverage/src/adapter.rs`
- Modify: `crates/ab-coverage/src/cache.rs`
- Modify: `justfile`
- Modify: `reports/aat-fidelity/run-cross-adapter-report.sh`
- Modify: `data/adapter-fidelity-notes.toml`
- Create: `tests/aozora-adapter-smoke.sh`

**Interfaces:**
- Consumes `adapters/aozora/target/release/aozora-adapter`.
- Adds parser id `aozora` to adapter lookup and cache fingerprint lookup.

- [ ] **Step 1: Add failing adapter smoke**

Create `tests/aozora-adapter-smoke.sh`:

```bash
#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
adapter="$repo_root/adapters/aozora/target/release/aozora-adapter"
out_dir="${AB_TEST_OUT_DIR:-/tmp/ab-validator/aozora-smoke}"
mkdir -p "$out_dir"

test -x "$adapter"
"$adapter" --version | rg -n '^aozora-adapter 0\.1\.0 aozora '

printf '｜青梅《おうめ》\n' | "$adapter" --mode aat > "$out_dir/aat.json"
jq -e '.meta.adapter == "aozora" and .meta.parse_complete == true and (.blocks | length >= 1)' "$out_dir/aat.json" >/dev/null

python - "$repo_root/data/aat-schema.json" "$out_dir/aat.json" <<'PY'
import json
import sys
from jsonschema import Draft202012Validator

schema = json.load(open(sys.argv[1], encoding="utf-8"))
data = json.load(open(sys.argv[2], encoding="utf-8"))
Draft202012Validator(schema).validate(data)
PY

echo "aozora adapter smoke ok: $out_dir/aat.json"
```

- [ ] **Step 2: Run smoke and confirm it fails before registration/build recipe**

Run:

```bash
bash tests/aozora-adapter-smoke.sh
```

Expected: fails because release adapter binary has not been built by any recipe.

- [ ] **Step 3: Register adapter path**

In `crates/ab-coverage/src/adapter.rs`, add the match arm:

```rust
            "aozora" => repo_root.join("adapters/aozora/target/release/aozora-adapter"),
```

In `crates/ab-coverage/src/cache.rs`, add the match arm:

```rust
            "aozora" => repo_root.join("adapters/aozora"),
```

Update the cache test parser list:

```rust
        for parser_id in ["aozora2", "aozora-rs", "aozora2html", "aozora-epub3", "aozora"] {
```

- [ ] **Step 4: Add just recipes and default AAT dir**

At the top of `justfile`, add:

```just
aozora_full_aat_dir := env_var_or_default("AB_AOZORA_AAT_DIR", ab_db_root + "/aat-corpus/aozora-full-20260705T000000Z/aat/aozora-adapter")
```

Near the adapter recipes, add:

```just
aozora-build PROFILE="release":
	@cargo build --manifest-path "{{repo_root}}/adapters/aozora/Cargo.toml" --{{PROFILE}}

aozora-test:
	@AB_AOZORA_BIN="${AB_AOZORA_BIN:-aozora}" cargo test --manifest-path "{{repo_root}}/adapters/aozora/Cargo.toml"

aozora-smoke: aozora-build
	@bash "{{repo_root}}/tests/aozora-adapter-smoke.sh"
```

Update `aat-to-parser-ir-full-audit` to include:

```just
		--aat-dir "{{aozora_full_aat_dir}}" \
```

Update `tei-eaj-structural-expansion` to include:

```just
		--aat-dir "aozora={{aozora_full_aat_dir}}" \
```

- [ ] **Step 5: Register `aozora` in cross-adapter oracle report**

In `reports/aat-fidelity/run-cross-adapter-report.sh`, add:

```bash
aozora_target="$(target_for aozora-cross-adapter)"
aozora_bin="$(adapter_bin_path "$AB_VALIDATOR_ROOT/adapters/aozora/Cargo.toml" aozora-adapter "$aozora_target")"
```

Add this argument to the `ab-oracle` invocation:

```bash
  --adapter "aozora=$aozora_bin" \
```

- [ ] **Step 6: Add fidelity note**

Append to `data/adapter-fidelity-notes.toml`:

```toml
[[note]]
id = "aozora-initial-json-inspect-adapter"
adapter = "aozora"
category = "adapter_scope"
severity = "follow_up"
summary = "Initial aozora adapter maps from the upstream inspect JSON surface and raw-preserves unsupported node kinds."
evidence = "docs/superpowers/specs/2026-07-05-aozora-parser-and-notation-spec-comparator.md"
```

- [ ] **Step 7: Verify**

Run:

```bash
cargo test -p ab-coverage --jobs 24
AB_AOZORA_BIN="$(nix build --no-link --print-out-paths .#reference-aozora)/bin/aozora" just aozora-smoke
```

Expected: `ab-coverage` tests pass and smoke logs `aozora adapter smoke ok`.

- [ ] **Step 8: Commit**

```bash
git add crates/ab-coverage/src/adapter.rs crates/ab-coverage/src/cache.rs justfile reports/aat-fidelity/run-cross-adapter-report.sh data/adapter-fidelity-notes.toml tests/aozora-adapter-smoke.sh
git commit -m "feat: register aozora adapter lane"
```

---

### Task 4: Add External Notation-Spec Comparator

**Files:**
- Create: `reports/parser-conformance/run-aozora-notation-spec.py`
- Create: `tests/aozora-notation-spec-comparator-smoke.sh`
- Modify: `justfile`

**Interfaces:**
- Consumes spec vector root `--vectors-dir`.
- Consumes adapter specs `--adapter label=command`.
- Produces `--summary-json` and `--report-md`.

- [ ] **Step 1: Write comparator smoke fixture**

Create `tests/aozora-notation-spec-comparator-smoke.sh`:

```bash
#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
tmp="$(mktemp -d)"
trap 'rm -rf "$tmp"' EXIT

mkdir -p "$tmp/vectors/ruby_explicit" "$tmp/vectors/unsupported_shape"

cat > "$tmp/vectors/ruby_explicit/vector.json" <<'JSON'
{
  "name": "ruby_explicit",
  "meta": {
    "feature": "ruby",
    "level": "must",
    "spec_section": "6.1",
    "note": "[provenance:smoke] hand-authored fixture"
  },
  "source": "｜青梅《おうめ》",
  "expected": {
    "serialize": "｜青梅《おうめ》",
    "nodes": [{"kind": "ruby", "span": {"start": 0, "end": 24}}],
    "pairs": [],
    "diagnostics": []
  }
}
JSON

cat > "$tmp/vectors/unsupported_shape/vector.json" <<'JSON'
{
  "name": "unsupported_shape",
  "meta": {
    "feature": "unknown_future_feature",
    "level": "should",
    "spec_section": "9.9",
    "note": "[provenance:smoke] deliberately unsupported by fake adapter"
  },
  "source": "［＃未来機能］",
  "expected": {
    "serialize": "［＃未来機能］",
    "nodes": [{"kind": "futureNode", "span": {"start": 0, "end": 18}}],
    "pairs": [],
    "diagnostics": []
  }
}
JSON

cat > "$tmp/fake-aozora" <<'SH'
#!/usr/bin/env bash
set -euo pipefail
kind="$2"
input="$(cat)"
case "$kind:$input" in
  nodes:｜青梅*)
    printf '{"schemaVersion":1,"data":[{"kind":"ruby","span":{"start":0,"end":24}}]}\n'
    ;;
  nodes:*)
    printf '{"schemaVersion":1,"data":[]}\n'
    ;;
  pairs:*)
    printf '{"schemaVersion":1,"data":[]}\n'
    ;;
  diagnostics:*)
    printf '{"schemaVersion":1,"data":[]}\n'
    ;;
  *)
    printf '{"schemaVersion":1,"data":[]}\n'
    ;;
esac
SH
chmod +x "$tmp/fake-aozora"

python "$repo_root/reports/parser-conformance/run-aozora-notation-spec.py" \
  --vectors-dir "$tmp/vectors" \
  --adapter "fake=$tmp/fake-aozora inspect" \
  --summary-json "$tmp/summary.json" \
  --report-md "$tmp/report.md"

jq -e '.totals.vectors == 2' "$tmp/summary.json"
jq -e '.totals.adapters == 1' "$tmp/summary.json"
jq -e '.totals.rows == 2' "$tmp/summary.json"
jq -e '.rows[] | select(.vector == "ruby_explicit" and .adapter == "fake") | .status == "pass"' "$tmp/summary.json"
jq -e '.rows[] | select(.vector == "unsupported_shape" and .adapter == "fake") | .status == "warning"' "$tmp/summary.json"
rg -n 'ruby_explicit' "$tmp/report.md"
rg -n 'unsupported_shape' "$tmp/report.md"

echo "aozora notation-spec comparator smoke ok"
```

- [ ] **Step 2: Run smoke and confirm it fails**

Run:

```bash
bash tests/aozora-notation-spec-comparator-smoke.sh
```

Expected: fails because `reports/parser-conformance/run-aozora-notation-spec.py` does not exist.

- [ ] **Step 3: Implement comparator script**

Create `reports/parser-conformance/run-aozora-notation-spec.py`:

```python
#!/usr/bin/env python
from __future__ import annotations

import argparse
import json
import shlex
import subprocess
from dataclasses import dataclass, asdict
from pathlib import Path
from typing import Any


@dataclass
class Adapter:
    label: str
    command: list[str]


@dataclass
class Row:
    vector: str
    feature: str
    level: str
    adapter: str
    status: str
    failures: list[str]
    warnings: list[str]


def parse_adapter(spec: str) -> Adapter:
    label, sep, command = spec.partition("=")
    if not sep or not label or not command:
        raise SystemExit(f"--adapter must be label=command, got {spec!r}")
    return Adapter(label=label, command=shlex.split(command))


def load_vectors(vectors_dir: Path) -> list[dict[str, Any]]:
    vectors = []
    for path in sorted(vectors_dir.glob("*/vector.json")):
        data = json.loads(path.read_text(encoding="utf-8"))
        if data["name"] != path.parent.name:
            raise SystemExit(f"{path}: name must match directory")
        vectors.append(data)
    if not vectors:
        raise SystemExit(f"no vector.json files found under {vectors_dir}")
    return vectors


def inspect(adapter: Adapter, kind: str, source: str) -> tuple[dict[str, Any] | None, str | None]:
    proc = subprocess.run(
        adapter.command + [kind, "-"],
        input=source,
        text=True,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        check=False,
    )
    if proc.returncode != 0:
        return None, proc.stderr.strip() or f"exit {proc.returncode}"
    try:
        value = json.loads(proc.stdout)
    except json.JSONDecodeError as error:
        return None, f"invalid JSON: {error}"
    if value.get("schemaVersion") != 1 or not isinstance(value.get("data"), list):
        return None, "unsupported inspect envelope"
    return value, None


def compare_projection(
    adapter: Adapter,
    vector: dict[str, Any],
    projection: str,
    failures: list[str],
    warnings: list[str],
) -> None:
    expected = vector["expected"].get(projection)
    if expected is None:
        return
    if projection == "serialize":
        warnings.append("serialize comparison skipped: adapter inspect surface does not emit serialize")
        return
    if projection not in {"nodes", "pairs", "diagnostics"}:
        warnings.append(f"{projection} comparison skipped: unsupported projection")
        return
    envelope, error = inspect(adapter, projection, vector["source"])
    if error:
        failures.append(f"{projection}: {error}")
        return
    actual = envelope["data"]
    if actual != expected:
        failures.append(f"{projection}: expected {expected!r}, got {actual!r}")


def evaluate(adapter: Adapter, vector: dict[str, Any]) -> Row:
    failures: list[str] = []
    warnings: list[str] = []
    level = vector["meta"]["level"]
    for projection in ["nodes", "pairs", "diagnostics", "serialize", "html"]:
        compare_projection(adapter, vector, projection, failures, warnings)

    if failures and level == "must":
        status = "fail"
    elif failures:
        warnings.extend(failures)
        failures = []
        status = "warning"
    elif warnings:
        status = "warning"
    else:
        status = "pass"

    return Row(
        vector=vector["name"],
        feature=vector["meta"]["feature"],
        level=level,
        adapter=adapter.label,
        status=status,
        failures=failures,
        warnings=warnings,
    )


def render_markdown(rows: list[Row], vectors_dir: Path) -> str:
    out = [
        "# Aozora Notation-Spec Comparison",
        "",
        f"- vectors_dir: `{vectors_dir}`",
        f"- rows: {len(rows)}",
        "",
        "| vector | feature | level | adapter | status | failures | warnings |",
        "| --- | --- | --- | --- | --- | --- | --- |",
    ]
    for row in rows:
        out.append(
            "| {} | {} | {} | {} | {} | {} | {} |".format(
                row.vector,
                row.feature,
                row.level,
                row.adapter,
                row.status,
                "<br>".join(row.failures),
                "<br>".join(row.warnings),
            )
        )
    out.append("")
    return "\n".join(out)


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--vectors-dir", type=Path, required=True)
    parser.add_argument("--adapter", action="append", default=[])
    parser.add_argument("--summary-json", type=Path, required=True)
    parser.add_argument("--report-md", type=Path, required=True)
    args = parser.parse_args()

    adapters = [parse_adapter(spec) for spec in args.adapter]
    if not adapters:
        raise SystemExit("at least one --adapter is required")

    vectors = load_vectors(args.vectors_dir)
    rows = [evaluate(adapter, vector) for vector in vectors for adapter in adapters]
    summary = {
        "schema_version": 1,
        "vectors_dir": str(args.vectors_dir),
        "totals": {
            "vectors": len(vectors),
            "adapters": len(adapters),
            "rows": len(rows),
            "pass": sum(1 for row in rows if row.status == "pass"),
            "warning": sum(1 for row in rows if row.status == "warning"),
            "fail": sum(1 for row in rows if row.status == "fail"),
        },
        "rows": [asdict(row) for row in rows],
    }
    args.summary_json.parent.mkdir(parents=True, exist_ok=True)
    args.report_md.parent.mkdir(parents=True, exist_ok=True)
    args.summary_json.write_text(json.dumps(summary, ensure_ascii=False, indent=2) + "\n", encoding="utf-8")
    args.report_md.write_text(render_markdown(rows, args.vectors_dir), encoding="utf-8")


if __name__ == "__main__":
    main()
```

- [ ] **Step 4: Add just recipe**

Add to `justfile`:

```just
aozora-notation-spec-comparator-smoke:
	@bash "{{repo_root}}/tests/aozora-notation-spec-comparator-smoke.sh"

aozora-notation-spec-comparison VECTORS="" REPORT_MD="docs/superpowers/reports/2026-07-05-aozora-notation-spec-comparison.md" SUMMARY_JSON="docs/superpowers/reports/2026-07-05-aozora-notation-spec-comparison.summary.json":
	@vectors="{{VECTORS}}"; if [ -z "$vectors" ]; then vectors="$(nix build --no-link --print-out-paths '{{repo_root}}#reference-aozora-notation-spec')/conformance/vectors"; fi; \
	aozora_bin="$(nix build --no-link --print-out-paths '{{repo_root}}#reference-aozora')/bin/aozora"; \
	cargo build --manifest-path "{{repo_root}}/adapters/aozora/Cargo.toml" --release; \
	python "{{repo_root}}/reports/parser-conformance/run-aozora-notation-spec.py" \
		--vectors-dir "$vectors" \
		--adapter "aozora=$aozora_bin inspect" \
		--summary-json "{{repo_root}}/{{SUMMARY_JSON}}" \
		--report-md "{{repo_root}}/{{REPORT_MD}}"
```

This initial recipe compares only the upstream `aozora inspect` surface. Task 5 upgrades the comparator to accept AAT-producing adapter commands and then expands this recipe to all five local adapters.

- [ ] **Step 5: Verify smoke**

Run:

```bash
just aozora-notation-spec-comparator-smoke
```

Expected: logs `aozora notation-spec comparator smoke ok`.

- [ ] **Step 6: Commit**

```bash
git add reports/parser-conformance/run-aozora-notation-spec.py tests/aozora-notation-spec-comparator-smoke.sh justfile
git commit -m "feat: compare adapters against aozora notation spec vectors"
```

---

### Task 5: Reconcile Comparator Adapter Command Shapes

**Files:**
- Modify: `reports/parser-conformance/run-aozora-notation-spec.py`
- Modify: `tests/aozora-notation-spec-comparator-smoke.sh`
- Modify: `justfile`

**Interfaces:**
- Supports `--adapter label=inspect:<command>` for native `aozora inspect` commands.
- Supports `--adapter label=aat:<command>` for AAT-emitting adapters.

- [ ] **Step 1: Add failing AAT adapter comparator smoke**

Extend `tests/aozora-notation-spec-comparator-smoke.sh` by adding this fake AAT adapter:

```bash
cat > "$tmp/fake-aat-adapter" <<'SH'
#!/usr/bin/env bash
set -euo pipefail
cat >/dev/null
printf '{"version":1,"meta":{"adapter":"fake-aat","parse_complete":true},"blocks":[{"kind":"paragraph","content":[{"kind":"ruby","base":"青梅","reading":"おうめ"}]}]}\n'
SH
chmod +x "$tmp/fake-aat-adapter"
```

Change the comparator invocation to:

```bash
python "$repo_root/reports/parser-conformance/run-aozora-notation-spec.py" \
  --vectors-dir "$tmp/vectors" \
  --adapter "fake=inspect:$tmp/fake-aozora inspect" \
  --adapter "fake-aat=aat:$tmp/fake-aat-adapter --mode aat" \
  --summary-json "$tmp/summary.json" \
  --report-md "$tmp/report.md"
```

Add assertions:

```bash
jq -e '.totals.adapters == 2' "$tmp/summary.json"
jq -e '.rows[] | select(.vector == "ruby_explicit" and .adapter == "fake-aat") | .status == "warning"' "$tmp/summary.json"
```

- [ ] **Step 2: Run smoke and confirm it fails**

Run:

```bash
just aozora-notation-spec-comparator-smoke
```

Expected: fails because `label=kind:command` parsing is not implemented.

- [ ] **Step 3: Implement adapter kind parsing**

Change the `Adapter` dataclass:

```python
@dataclass
class Adapter:
    label: str
    mode: str
    command: list[str]
```

Replace `parse_adapter`:

```python
def parse_adapter(spec: str) -> Adapter:
    label, sep, rest = spec.partition("=")
    if not sep or not label or not rest:
        raise SystemExit(f"--adapter must be label=mode:command, got {spec!r}")
    mode, mode_sep, command = rest.partition(":")
    if not mode_sep or mode not in {"inspect", "aat"} or not command:
        raise SystemExit(f"--adapter mode must be inspect or aat, got {spec!r}")
    return Adapter(label=label, mode=mode, command=shlex.split(command))
```

Change `compare_projection` so `adapter.mode == "aat"` reports skipped structural comparison:

```python
    if adapter.mode == "aat":
        warnings.append(f"{projection} comparison skipped: AAT adapter does not expose aozora inspect {projection}")
        return
```

Place that block after the `serialize` branch and before invoking `inspect`.

- [ ] **Step 4: Update just recipe adapter specs**

In `justfile`, change comparator recipe adapter args to:

```just
		--adapter "aozora=inspect:$aozora_bin inspect" \
		--adapter "ab-aozora=aat:{{repo_root}}/adapters/aozora/target/release/aozora-adapter --mode aat" \
		--adapter "aozora2=aat:{{repo_root}}/adapters/aozora2/target/release/aozora2-adapter --mode aat" \
		--adapter "aozora2html=aat:{{repo_root}}/adapters/aozora2html/aozora2html-adapter --mode aat" \
		--adapter "aozora-rs=aat:{{repo_root}}/adapters/aozora-rs/target/release/aozora-rs-adapter --mode aat" \
		--adapter "aozora-epub3=aat:{{repo_root}}/adapters/aozora-epub3/aozora-epub3-adapter --mode aat" \
```

The native `inspect` lane can assert source-coordinate node and diagnostic matches. The AAT lanes initially produce structural-warning rows for vector expectations that have no AAT equivalent; those warnings are intentional evidence that the adapter comparison is present but the exact external spec surface is not yet projected from AAT.

- [ ] **Step 5: Verify**

Run:

```bash
just aozora-notation-spec-comparator-smoke
```

Expected: logs `aozora notation-spec comparator smoke ok`.

- [ ] **Step 6: Commit**

```bash
git add reports/parser-conformance/run-aozora-notation-spec.py tests/aozora-notation-spec-comparator-smoke.sh justfile
git commit -m "fix: support inspect and AAT comparator adapters"
```

---

### Task 6: Add Flake Smoke Checks

**Files:**
- Modify: `flake.nix`
- Modify: `justfile`

**Interfaces:**
- Produces flake check `aozora-smoke`.
- Produces flake check `aozora-notation-spec-comparator-smoke`.

- [ ] **Step 1: Add flake checks**

Add an `aozoraAdapterSmokeCheck` derivation near the other checks:

```nix
        aozoraAdapterSmokeCheck =
          pkgs.runCommand "aozora-adapter-smoke-check"
            {
              nativeBuildInputs = [
                rustToolchain
                pkgs.bash
                pkgs.jq
                pkgs.python3
                pkgs.ripgrep
                pkgs.python3Packages.jsonschema
              ];
            }
            ''
              work_dir="$TMPDIR/work"
              cp -R ${source} "$work_dir"
              chmod -R u+w "$work_dir"
              export AB_AOZORA_BIN="${referenceAozora}/bin/aozora"
              cargo --config "source.crates-io.replace-with='vendored-sources'" \
                --config "source.vendored-sources.directory='${aozoraCargoDeps}'" \
                build --manifest-path "$work_dir/adapters/aozora/Cargo.toml" --release --offline
              bash "$work_dir/tests/aozora-adapter-smoke.sh"
              touch "$out"
            '';
```

If the repository does not yet define `aozoraCargoDeps`, add it beside the other vendored adapter dependencies:

```nix
        aozoraCargoDeps = rustPlatform.importCargoLock {
          lockFile = ./adapters/aozora/Cargo.lock;
        };
```

Add comparator check:

```nix
        aozoraNotationSpecComparatorSmokeCheck =
          pkgs.runCommand "aozora-notation-spec-comparator-smoke-check"
            {
              nativeBuildInputs = [
                pkgs.bash
                pkgs.jq
                pkgs.python3
                pkgs.ripgrep
              ];
            }
            ''
              bash "${source}/tests/aozora-notation-spec-comparator-smoke.sh"
              touch "$out"
            '';
```

Add both to `checks`:

```nix
          aozora-smoke = aozoraAdapterSmokeCheck;
          aozora-notation-spec-comparator-smoke = aozoraNotationSpecComparatorSmokeCheck;
```

- [ ] **Step 2: Add just aliases**

Add:

```just
aozora-flake-smoke:
	@system="$(nix eval --impure --raw --expr builtins.currentSystem)"; \
	nix build "{{repo_root}}#checks.$system.aozora-smoke" --print-build-logs

aozora-notation-spec-comparator-flake-smoke:
	@system="$(nix eval --impure --raw --expr builtins.currentSystem)"; \
	nix build "{{repo_root}}#checks.$system.aozora-notation-spec-comparator-smoke" --print-build-logs
```

- [ ] **Step 3: Verify**

Run:

```bash
just aozora-flake-smoke
just aozora-notation-spec-comparator-flake-smoke
```

Expected: both Nix builds finish and do not write to `/db`.

- [ ] **Step 4: Commit**

```bash
git add flake.nix justfile
git commit -m "build: gate aozora adapter and comparator smokes"
```

---

### Task 7: Generate First Comparator Report

**Files:**
- Create: `docs/superpowers/reports/2026-07-05-aozora-notation-spec-comparison.md`
- Create: `docs/superpowers/reports/2026-07-05-aozora-notation-spec-comparison.summary.json`

**Interfaces:**
- Consumes flake package `reference-aozora-notation-spec`.
- Consumes flake package `reference-aozora`.
- Consumes `adapters/aozora/target/release/aozora-adapter`.

- [ ] **Step 1: Run comparator report**

Run:

```bash
just aozora-notation-spec-comparison
```

Expected: writes the Markdown and JSON reports under `docs/superpowers/reports/`.

- [ ] **Step 2: Inspect summary**

Run:

```bash
jq -r '.totals' docs/superpowers/reports/2026-07-05-aozora-notation-spec-comparison.summary.json
rg -n 'status | failures | warnings|must|should|may' docs/superpowers/reports/2026-07-05-aozora-notation-spec-comparison.md
```

Expected: totals show at least one vector and at least one adapter. Any failures are acceptable in this first report if they are explicit measured rows.

- [ ] **Step 3: Commit**

```bash
git add docs/superpowers/reports/2026-07-05-aozora-notation-spec-comparison.md docs/superpowers/reports/2026-07-05-aozora-notation-spec-comparison.summary.json
git commit -m "docs: record aozora notation spec comparison"
```

---

### Task 8: Final Verification and Handoff

**Files:**
- Modify: `docs/handoffs/aozora-parser-adapter-and-notation-comparator.md`

**Interfaces:**
- Produces handoff for full corpus and parser-IR follow-up.

- [ ] **Step 1: Write handoff**

Create `docs/handoffs/aozora-parser-adapter-and-notation-comparator.md`:

```markdown
# Aozora Parser Adapter and Notation Comparator Handoff

## Implemented

- Added parser id `aozora` as a fifth adapter lane.
- Added pinned references for `P4suta/aozora` and `P4suta/aozora-notation-spec`.
- Added `aozora-adapter` with schema-valid AAT smoke coverage.
- Added notation-spec comparator reports kept separate from local AAT oracle.

## Verification

- `cargo test --manifest-path adapters/aozora/Cargo.toml`
- `cargo test -p ab-coverage --jobs 24`
- `just aozora-smoke`
- `just aozora-notation-spec-comparator-smoke`
- `just aozora-flake-smoke`
- `just aozora-notation-spec-comparator-flake-smoke`

## Next Operator Measurements

1. Run full `aozora` AAT corpus:
   `just aozora-aat-full JOBS=24 TIMEOUT=300s`
2. Add the resulting AAT dir to `AB_AOZORA_AAT_DIR`.
3. Run `just aat-to-parser-ir-full-audit JOBS=24`.
4. Run `just tei-eaj-structural-expansion JOBS=24`.
5. Run parser performance measurement with all five parser lanes.

## Trust Boundary

`aozora-notation-spec` is useful comparison evidence, not the authority for
local source representability, parser-IR vocabulary, or ABC TEI admission.
```

- [ ] **Step 2: Run final verification**

Run:

```bash
cargo fmt --all
cargo test --manifest-path adapters/aozora/Cargo.toml
cargo test -p ab-coverage --jobs 24
just aozora-smoke
just aozora-notation-spec-comparator-smoke
just aozora-flake-smoke
just aozora-notation-spec-comparator-flake-smoke
git diff --check
```

Expected: all commands exit 0.

- [ ] **Step 3: Commit handoff**

```bash
git add docs/handoffs/aozora-parser-adapter-and-notation-comparator.md
git commit -m "docs: hand off aozora adapter comparator"
```

- [ ] **Step 4: Push branch**

Run:

```bash
git status --short --branch
git push
```

Expected: branch is clean after push.

---

## Self-Review

Spec coverage:

- Pinned parser and notation-spec references: Task 1.
- Fifth `aozora` adapter lane: Tasks 2 and 3.
- External vector comparator separated from local AAT oracle: Tasks 4 and 5.
- Flake checks that avoid `/db` and network: Task 6.
- First measured comparator report: Task 7.
- Operator handoff for full corpus, parser-IR, TEI-EAJ, and performance measurements: Task 8.

Known limits accepted for this first implementation slice:

- The initial adapter raw-preserves unsupported upstream node kinds instead of completing all AAT vocabulary mapping.
- The initial comparator compares upstream `inspect` projections directly and records AAT adapter comparisons as skipped structural warnings.
- Full corpus AAT, parser-IR audit, TEI-EAJ expansion, and parser performance runs are explicit next operator measurements after this code lands.

Placeholder and type consistency checks:

- No-prohibited-token scan passed for the standard writing-plans red-flag terms.
- Comparator adapter specs are consistently `label=command` in Task 4, then `label=mode:command` after Task 5 introduces `AdapterMode`.
- The adapter version interface is consistently `adapter_version() -> String` and the metadata path calls that function rather than reading a stale compile-time constant.
