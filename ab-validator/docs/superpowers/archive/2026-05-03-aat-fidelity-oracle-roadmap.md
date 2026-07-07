# AAT Fidelity and Oracle Roadmap Implementation Plan

> **Superseded:** Use `docs/superpowers/plans/2026-05-03-aat-fidelity-oracle-roadmap-v3.md` instead. The v3 plan composes with `ab-ir` and `ab-check`, separates oracle expectations from upstream observations, and incorporates the review-driven selector, schema, and span-contract corrections.

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Build a durable fidelity framework that separates AAT contract correctness, adapter faithfulness to upstream parsers, and independent linguistic/oracle correctness.

**Architecture:** The work is split into four independent layers. The AAT contract defines the target JSON shapes. The oracle data layer defines expected results for selected Aozora features without depending on any adapter. The fidelity runner compares each adapter output against both the contract and the oracle. Adapter fixes then use that runner and shared fixtures to improve behavior one feature family at a time.

**Tech Stack:** Rust 2024 workspace crates, JSON Schema 2020-12, TOML fixture data, existing Aozora adapters, `cargo test`, `nix develop`, and `/db/ab-validator` for temporary targets and generated comparison output.

---

## Current State

The project already has:

- `data/aat-schema.json`: the current AAT v1 schema.
- `data/aozora-syntax-coverage.toml`: syntax feature matrix with parser and adapter cells.
- `docs/adapter-fidelity.md`: current adapter-fidelity matrix.
- `adapters/aozora-rs`, `adapters/aozora2`, and `adapters/aozora2html`: the active parser adapters.
- `crates/ab-check`: AAT schema/property validation.
- `crates/ab-coverage`: feature coverage and syntax matrix tooling.

The main missing pieces are:

- a written AAT contract that explains semantics, not only schema shape;
- an independent oracle for expected linguistic/literary results;
- a shared fixture runner that applies the same cases to every adapter;
- block reconstruction and source-span work for richer AAT inspection.

## File Structure

- Create `docs/aat-contract.md` to define semantic meaning of every first-class AAT node.
- Modify `data/aat-schema.json` only when the contract exposes a missing schema shape.
- Create `data/aat-oracle-cases.schema.json` for oracle fixture validation.
- Create `data/aat-oracle-cases.toml` for selected source snippets and expected outcomes.
- Create `crates/ab-oracle/` as a small Rust library and CLI for loading oracle cases and evaluating AAT JSON.
- Modify root `Cargo.toml` to include `crates/ab-oracle` if the workspace currently includes all crates explicitly.
- Modify `crates/ab-check` only if shared validation helpers should move out of command code.
- Modify `crates/ab-coverage` to merge oracle/fidelity result summaries into coverage reports.
- Modify adapter tests under `adapters/*/tests/` to use shared oracle cases where feasible.
- Write generated or large comparison artifacts under `/db/ab-validator/aat-fidelity/`, not the repo.

---

### Task 1: Write the AAT Semantic Contract

**Files:**
- Create: `docs/aat-contract.md`
- Modify: `docs/adapter-fidelity.md`

- [ ] **Step 1: Create the contract document**

Create `docs/aat-contract.md` with this initial structure:

```markdown
# AAT Contract

The Aozora Adapter Tree (AAT) is the normalized JSON representation emitted by parser adapters. AAT records adapter output and provenance. It does not by itself assert that an adapter resolved every Aozora feature correctly.

## Judgment Layers

| Layer | Question | Example |
| --- | --- | --- |
| Schema validity | Is the JSON structurally valid AAT? | A `ruby` node has `base` and `reading`. |
| Adapter faithfulness | Did the adapter preserve what upstream emitted? | `aozora-rs` reports a JIS gaiji unresolved if `aozora-rs-gaiji` leaves it unresolved. |
| Oracle correctness | Does the output match the expected Aozora/literary result? | `第4水準2-13-47` should resolve to `撑` even if an adapter misses it. |

## First-Class Nodes

| Aozora feature | AAT shape | Required fields | Plaintext projection | Notes |
| --- | --- | --- | --- | --- |
| plain text | `{"kind":"text"}` | `value` | `value` | Adjacent text may be coalesced. |
| ruby | `{"kind":"ruby"}` | `base`, `reading` | `base` | `base_content` and `reading_content` preserve nested gaiji/style when available. |
| gaiji | `{"kind":"gaiji"}` | `description`, `resolved`, `unresolved_reason` | `resolved` when present, otherwise empty | Oracle correctness is evaluated separately. |
| accent | `{"kind":"accent"}` | `code`, `name`, `resolved` | `resolved` when present, otherwise `name` | Used for Latin accent notation. |
| figure | `{"kind":"figure"}` | `filename`, `alt`, `css_class` | empty | Captions may be represented separately or attached when an adapter can do so faithfully. |
| style | `{"kind":"style"}` | `style_type`, `content` | child projection | Includes emphasis and inline heading preservation. |
| font size | `{"kind":"font_size"}` | `size_type`, `level`, `content` | child projection | `size_type` is `dai` or `sho` when sourced from `aozora2`. |
| tcy | `{"kind":"tcy"}` | `content` | child projection | Vertical-in-horizontal text. |
| yokogumi | `{"kind":"yokogumi"}` | `content` | child projection | Inline horizontal composition. |
| keigakomi | `{"kind":"keigakomi"}` | `content` | child projection | Inline ruled enclosure. |
| caption | `{"kind":"caption"}` | `content` | child projection | Inline caption representation. |
| warigaki | `{"kind":"warigaki"}` | `upper`, `lower` | upper then lower projection | Split semantics should be preserved when upstream exposes them. |
| raw | `{"kind":"raw"}` | `source` | empty unless explicitly configured | Escape hatch for faithful preservation of unsupported parser events. |
| paragraph | `{"kind":"paragraph"}` | `content` | child projection | Default block container for inline content. |
| heading | `{"kind":"heading"}` | `level`, `style`, `content` | child projection | Preferred block-level heading shape. |
| block container | `*_block` | `children` | child projection | Used for block ranges such as jisage, quote, keigakomi, yokogumi, caption. |

## Source Spans

`span` is optional in AAT v1. When present, it refers to the decoded source text byte offsets and line numbers before adapter projection.

## Compatibility Rule

Adapters may emit `raw` for faithfully preserved upstream events that the AAT contract does not yet model. Dropping parser events requires an explicit adapter-fidelity note.
```

- [ ] **Step 2: Link the contract from the fidelity matrix**

Append this sentence to `docs/adapter-fidelity.md`:

```markdown
For node semantics and the distinction between schema validity, adapter faithfulness, and oracle correctness, see `docs/aat-contract.md`.
```

- [ ] **Step 3: Verify the contract exists**

Run:

```bash
test -s docs/aat-contract.md
rg -n "Oracle correctness|First-Class Nodes|Compatibility Rule" docs/aat-contract.md
```

Expected: both commands exit 0 and `rg` prints the three headings/phrases.

- [ ] **Step 4: Commit**

Run:

```bash
git add docs/aat-contract.md docs/adapter-fidelity.md
git commit -m "docs: define aat fidelity contract"
```

Expected: commit succeeds unless the worktree contains unrelated staged changes. If unrelated staged changes exist, unstage them first with `git restore --staged <path>` for those unrelated paths only.

---

### Task 2: Add Oracle Fixture Schema and Seed Cases

**Files:**
- Create: `data/aat-oracle-cases.schema.json`
- Create: `data/aat-oracle-cases.toml`

- [ ] **Step 1: Add a failing schema validation test script**

Create `tests/aat-oracle-cases-schema-smoke.sh`:

```bash
#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

nix develop "${repo_root}#aozora2html" --command python - "${repo_root}" <<'PY'
import json
from pathlib import Path
import sys
import tomli
import jsonschema

root = Path(sys.argv[1])
schema = json.loads((root / 'data' / 'aat-oracle-cases.schema.json').read_text())
cases = tomli.loads((root / 'data' / 'aat-oracle-cases.toml').read_text())
jsonschema.validate(cases, schema)
print(f\"validated {len(cases['case'])} oracle cases\")
PY
```

Run:

```bash
bash tests/aat-oracle-cases-schema-smoke.sh
```

Expected: FAIL because `data/aat-oracle-cases.schema.json` and `data/aat-oracle-cases.toml` do not exist yet.

- [ ] **Step 2: Create oracle schema**

Create `data/aat-oracle-cases.schema.json`:

```json
{
  "$schema": "https://json-schema.org/draft/2020-12/schema",
  "$id": "https://example.com/ab-validator/aat-oracle-cases.schema.json",
  "title": "AAT Oracle Cases",
  "type": "object",
  "required": ["case"],
  "additionalProperties": false,
  "properties": {
    "case": {
      "type": "array",
      "minItems": 1,
      "items": { "$ref": "#/$defs/Case" }
    }
  },
  "$defs": {
    "Case": {
      "type": "object",
      "required": ["id", "category", "source", "expect"],
      "additionalProperties": false,
      "properties": {
        "id": { "type": "string", "minLength": 1 },
        "category": { "type": "string", "minLength": 1 },
        "source": { "type": "string", "minLength": 1 },
        "notes": { "type": "string" },
        "expect": { "$ref": "#/$defs/Expect" }
      }
    },
    "Expect": {
      "type": "object",
      "additionalProperties": false,
      "properties": {
        "visible_text": { "type": "string" },
        "nodes": {
          "type": "array",
          "items": { "$ref": "#/$defs/ExpectedNode" }
        },
        "gaiji": {
          "type": "array",
          "items": { "$ref": "#/$defs/ExpectedGaiji" }
        }
      }
    },
    "ExpectedNode": {
      "type": "object",
      "required": ["kind"],
      "additionalProperties": false,
      "properties": {
        "kind": { "type": "string", "minLength": 1 },
        "count": { "type": "integer", "minimum": 0 },
        "min_count": { "type": "integer", "minimum": 0 },
        "field": { "type": "string" },
        "equals": {}
      }
    },
    "ExpectedGaiji": {
      "type": "object",
      "required": ["description", "correct"],
      "additionalProperties": false,
      "properties": {
        "description": { "type": "string", "minLength": 1 },
        "correct": { "type": ["string", "null"] },
        "jis_code": { "type": ["string", "null"] },
        "source": { "type": "string" }
      }
    }
  }
}
```

- [ ] **Step 3: Add seed oracle cases**

Create `data/aat-oracle-cases.toml`:

```toml
[[case]]
id = "gaiji.jis.2-13-47"
category = "gaiji"
source = "耳朶を※［＃「てへん＋掌」、第4水準2-13-47］えて"
notes = "JIS X 0213 plane 2 row 13 cell 47 should resolve to 撑."

[case.expect]
visible_text = "耳朶を撑えて"

[[case.expect.gaiji]]
description = "「てへん＋掌」、第4水準2-13-47"
correct = "撑"
jis_code = "2-13-47"
source = "JIS X 0213"

[[case]]
id = "gaiji.unicode.u546d"
category = "gaiji"
source = "※［＃「口＋世」、U+546D］は珍しい字。"

[case.expect]
visible_text = "呭は珍しい字。"

[[case.expect.gaiji]]
description = "「口＋世」、U+546D"
correct = "呭"
jis_code = ""
source = "Unicode code point"

[[case]]
id = "ruby.gaiji.inline_base"
category = "ruby"
source = "※［＃「口＋愛」、第3水準1-15-23］《おくび》が出た。"
notes = "The ruby base is a gaiji marker; adapters should preserve the gaiji as base content when upstream exposes it."

[case.expect]
visible_text = "が出た。"

[[case.expect.nodes]]
kind = "ruby"
min_count = 1

[[case.expect.nodes]]
kind = "gaiji"
min_count = 1

[[case]]
id = "accent.basic"
category = "accent"
source = "［＃「e`」はアクセント分解された欧文］"

[case.expect]
visible_text = ""

[[case.expect.nodes]]
kind = "accent"
min_count = 1

[[case]]
id = "figure.basic"
category = "figure"
source = "［＃挿絵（fig01.png、横４００×縦３００）入る］"

[case.expect]
visible_text = ""

[[case.expect.nodes]]
kind = "figure"
min_count = 1
```

- [ ] **Step 4: Run schema smoke test**

Run:

```bash
bash tests/aat-oracle-cases-schema-smoke.sh
```

Expected: PASS and print `validated 5 oracle cases`.

- [ ] **Step 5: Commit**

Run:

```bash
chmod +x tests/aat-oracle-cases-schema-smoke.sh
git add data/aat-oracle-cases.schema.json data/aat-oracle-cases.toml tests/aat-oracle-cases-schema-smoke.sh
git commit -m "test: seed aat oracle cases"
```

Expected: commit succeeds.

---

### Task 3: Create `ab-oracle` Loader Crate

**Files:**
- Modify: `Cargo.toml`
- Create: `crates/ab-oracle/Cargo.toml`
- Create: `crates/ab-oracle/src/lib.rs`
- Create: `crates/ab-oracle/src/main.rs`

- [ ] **Step 1: Add failing loader tests**

Create `crates/ab-oracle/src/lib.rs`:

```rust
use anyhow::Result;
use serde::Deserialize;
use std::{fs, path::Path};

#[derive(Debug, Deserialize)]
pub struct OracleFile {
    pub case: Vec<OracleCase>,
}

#[derive(Debug, Deserialize)]
pub struct OracleCase {
    pub id: String,
    pub category: String,
    pub source: String,
    #[serde(default)]
    pub notes: String,
    pub expect: Expected,
}

#[derive(Debug, Deserialize, Default)]
pub struct Expected {
    #[serde(default)]
    pub visible_text: Option<String>,
    #[serde(default)]
    pub nodes: Vec<ExpectedNode>,
    #[serde(default)]
    pub gaiji: Vec<ExpectedGaiji>,
}

#[derive(Debug, Deserialize)]
pub struct ExpectedNode {
    pub kind: String,
    #[serde(default)]
    pub count: Option<usize>,
    #[serde(default)]
    pub min_count: Option<usize>,
    #[serde(default)]
    pub field: Option<String>,
    #[serde(default)]
    pub equals: Option<toml::Value>,
}

#[derive(Debug, Deserialize)]
pub struct ExpectedGaiji {
    pub description: String,
    pub correct: Option<String>,
    #[serde(default)]
    pub jis_code: Option<String>,
    #[serde(default)]
    pub source: String,
}

pub fn load_oracle_file(path: &Path) -> Result<OracleFile> {
    let raw = fs::read_to_string(path)?;
    Ok(toml::from_str(&raw)?)
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::Path;

    #[test]
    fn loads_seed_oracle_cases() {
        let oracle = load_oracle_file(Path::new("../../data/aat-oracle-cases.toml")).unwrap();
        assert!(oracle.case.iter().any(|case| case.id == "gaiji.jis.2-13-47"));
        assert!(oracle.case.iter().any(|case| case.id == "ruby.gaiji.inline_base"));
    }
}
```

Run:

```bash
CARGO_TARGET_DIR=/db/ab-validator/target-ab-oracle \
  cargo test --manifest-path crates/ab-oracle/Cargo.toml -- --nocapture
```

Expected: FAIL because `crates/ab-oracle/Cargo.toml` does not exist yet.

- [ ] **Step 2: Add crate manifest**

Create `crates/ab-oracle/Cargo.toml`:

```toml
[package]
name = "ab-oracle"
version = "0.1.0"
edition = "2024"
license = "MIT OR Apache-2.0"

[dependencies]
anyhow = "1.0"
clap = { version = "4.5", features = ["derive"] }
serde = { version = "1.0", features = ["derive"] }
serde_json = "1.0"
toml = "0.9"
```

If the root workspace has an explicit `members` list, add `"crates/ab-oracle"` to it.

- [ ] **Step 3: Add CLI skeleton**

Create `crates/ab-oracle/src/main.rs`:

```rust
use std::path::PathBuf;

use ab_oracle::load_oracle_file;
use anyhow::Result;
use clap::Parser;

#[derive(Debug, Parser)]
struct Args {
    #[arg(long, default_value = "data/aat-oracle-cases.toml")]
    oracle: PathBuf,
}

fn main() -> Result<()> {
    let args = Args::parse();
    let oracle = load_oracle_file(&args.oracle)?;
    println!("loaded {} oracle cases", oracle.case.len());
    Ok(())
}
```

- [ ] **Step 4: Run loader tests and CLI**

Run:

```bash
CARGO_TARGET_DIR=/db/ab-validator/target-ab-oracle \
  cargo test --manifest-path crates/ab-oracle/Cargo.toml -- --nocapture

CARGO_TARGET_DIR=/db/ab-validator/target-ab-oracle \
  cargo run --manifest-path crates/ab-oracle/Cargo.toml -- --oracle data/aat-oracle-cases.toml
```

Expected: tests pass and CLI prints `loaded 5 oracle cases`.

- [ ] **Step 5: Commit**

Run:

```bash
git add Cargo.toml crates/ab-oracle
git commit -m "feat: add aat oracle loader"
```

Expected: commit succeeds.

---

### Task 4: Implement Oracle Evaluation Against AAT JSON

**Files:**
- Modify: `crates/ab-oracle/src/lib.rs`
- Modify: `crates/ab-oracle/src/main.rs`

- [ ] **Step 1: Add failing evaluator tests**

Append these tests to `crates/ab-oracle/src/lib.rs`:

```rust
#[cfg(test)]
mod evaluator_tests {
    use super::*;
    use serde_json::json;

    #[test]
    fn evaluates_visible_text_and_gaiji_correctness() {
        let case = OracleCase {
            id: "gaiji.jis.2-13-47".to_owned(),
            category: "gaiji".to_owned(),
            source: String::new(),
            notes: String::new(),
            expect: Expected {
                visible_text: Some("耳朶を撑えて".to_owned()),
                nodes: vec![],
                gaiji: vec![ExpectedGaiji {
                    description: "「てへん＋掌」、第4水準2-13-47".to_owned(),
                    correct: Some("撑".to_owned()),
                    jis_code: Some("2-13-47".to_owned()),
                    source: "JIS X 0213".to_owned(),
                }],
            },
        };
        let aat = json!({
            "blocks": [{
                "kind": "paragraph",
                "content": [
                    {"kind": "text", "value": "耳朶を"},
                    {"kind": "gaiji", "description": "「てへん＋掌」、第4水準2-13-47", "resolved": "撑", "jis_code": "2-13-47", "unresolved_reason": null},
                    {"kind": "text", "value": "えて"}
                ]
            }]
        });

        let result = evaluate_case(&case, &aat);
        assert!(result.passed, "{result:#?}");
    }

    #[test]
    fn reports_wrong_gaiji_resolution() {
        let case = OracleCase {
            id: "gaiji.jis.2-13-47".to_owned(),
            category: "gaiji".to_owned(),
            source: String::new(),
            notes: String::new(),
            expect: Expected {
                visible_text: None,
                nodes: vec![],
                gaiji: vec![ExpectedGaiji {
                    description: "「てへん＋掌」、第4水準2-13-47".to_owned(),
                    correct: Some("撑".to_owned()),
                    jis_code: Some("2-13-47".to_owned()),
                    source: "JIS X 0213".to_owned(),
                }],
            },
        };
        let aat = json!({
            "blocks": [{
                "kind": "paragraph",
                "content": [
                    {"kind": "gaiji", "description": "「てへん＋掌」、第4水準2-13-47", "resolved": null, "jis_code": "2-13-47", "unresolved_reason": "unresolved"}
                ]
            }]
        });

        let result = evaluate_case(&case, &aat);
        assert!(!result.passed);
        assert!(result.failures.iter().any(|failure| failure.contains("expected gaiji")));
    }
}
```

Run:

```bash
CARGO_TARGET_DIR=/db/ab-validator/target-ab-oracle \
  cargo test --manifest-path crates/ab-oracle/Cargo.toml evaluator_tests -- --nocapture
```

Expected: FAIL because `evaluate_case` is not defined.

- [ ] **Step 2: Implement evaluation types**

Add to `crates/ab-oracle/src/lib.rs`:

```rust
#[derive(Debug, Default)]
pub struct EvaluationResult {
    pub case_id: String,
    pub passed: bool,
    pub failures: Vec<String>,
}

pub fn evaluate_case(case: &OracleCase, aat: &serde_json::Value) -> EvaluationResult {
    let mut failures = Vec::new();

    if let Some(expected_visible) = &case.expect.visible_text {
        let actual_visible = visible_text(aat);
        if &actual_visible != expected_visible {
            failures.push(format!(
                "visible_text mismatch: expected {expected_visible:?}, got {actual_visible:?}"
            ));
        }
    }

    for expected_node in &case.expect.nodes {
        let count = count_kind(aat, &expected_node.kind);
        if let Some(expected_count) = expected_node.count {
            if count != expected_count {
                failures.push(format!(
                    "kind {:?} count mismatch: expected {expected_count}, got {count}",
                    expected_node.kind
                ));
            }
        }
        if let Some(min_count) = expected_node.min_count {
            if count < min_count {
                failures.push(format!(
                    "kind {:?} count below minimum: expected at least {min_count}, got {count}",
                    expected_node.kind
                ));
            }
        }
    }

    for expected_gaiji in &case.expect.gaiji {
        let matched = find_gaiji(aat, &expected_gaiji.description);
        match matched {
            Some(actual) => {
                let actual_resolved = actual.get("resolved").and_then(serde_json::Value::as_str);
                if actual_resolved != expected_gaiji.correct.as_deref() {
                    failures.push(format!(
                        "expected gaiji {:?} to resolve to {:?}, got {:?}",
                        expected_gaiji.description,
                        expected_gaiji.correct,
                        actual.get("resolved")
                    ));
                }
            }
            None => failures.push(format!(
                "expected gaiji {:?} was not present",
                expected_gaiji.description
            )),
        }
    }

    EvaluationResult {
        case_id: case.id.clone(),
        passed: failures.is_empty(),
        failures,
    }
}
```

- [ ] **Step 3: Implement traversal helpers**

Add to `crates/ab-oracle/src/lib.rs`:

```rust
fn visible_text(value: &serde_json::Value) -> String {
    let mut out = String::new();
    append_visible_text(value, &mut out);
    out
}

fn append_visible_text(value: &serde_json::Value, out: &mut String) {
    match value {
        serde_json::Value::Object(object) => match object.get("kind").and_then(serde_json::Value::as_str) {
            Some("text") => {
                if let Some(text) = object.get("value").and_then(serde_json::Value::as_str) {
                    out.push_str(text);
                }
            }
            Some("ruby") => {
                if let Some(base) = object.get("base").and_then(serde_json::Value::as_str) {
                    out.push_str(base);
                }
            }
            Some("gaiji") | Some("accent") => {
                if let Some(resolved) = object.get("resolved").and_then(serde_json::Value::as_str) {
                    out.push_str(resolved);
                }
            }
            Some("warigaki") => {
                append_visible_text(object.get("upper").unwrap_or(&serde_json::Value::Null), out);
                append_visible_text(object.get("lower").unwrap_or(&serde_json::Value::Null), out);
            }
            _ => {
                for value in object.values() {
                    append_visible_text(value, out);
                }
            }
        },
        serde_json::Value::Array(values) => {
            for value in values {
                append_visible_text(value, out);
            }
        }
        _ => {}
    }
}

fn count_kind(value: &serde_json::Value, kind: &str) -> usize {
    match value {
        serde_json::Value::Object(object) => {
            let here = usize::from(object.get("kind").and_then(serde_json::Value::as_str) == Some(kind));
            here + object.values().map(|value| count_kind(value, kind)).sum::<usize>()
        }
        serde_json::Value::Array(values) => values.iter().map(|value| count_kind(value, kind)).sum(),
        _ => 0,
    }
}

fn find_gaiji<'a>(
    value: &'a serde_json::Value,
    description: &str,
) -> Option<&'a serde_json::Map<String, serde_json::Value>> {
    match value {
        serde_json::Value::Object(object) => {
            if object.get("kind").and_then(serde_json::Value::as_str) == Some("gaiji")
                && object.get("description").and_then(serde_json::Value::as_str) == Some(description)
            {
                return Some(object);
            }
            object.values().find_map(|value| find_gaiji(value, description))
        }
        serde_json::Value::Array(values) => values.iter().find_map(|value| find_gaiji(value, description)),
        _ => None,
    }
}
```

- [ ] **Step 4: Extend CLI to evaluate an AAT file**

Update `crates/ab-oracle/src/main.rs`:

```rust
use std::{fs, path::PathBuf};

use ab_oracle::{evaluate_case, load_oracle_file};
use anyhow::Result;
use clap::Parser;

#[derive(Debug, Parser)]
struct Args {
    #[arg(long, default_value = "data/aat-oracle-cases.toml")]
    oracle: PathBuf,

    #[arg(long)]
    case_id: Option<String>,

    #[arg(long)]
    aat: Option<PathBuf>,
}

fn main() -> Result<()> {
    let args = Args::parse();
    let oracle = load_oracle_file(&args.oracle)?;
    if let Some(aat_path) = args.aat {
        let raw = fs::read_to_string(aat_path)?;
        let aat: serde_json::Value = serde_json::from_str(&raw)?;
        let mut failures = 0usize;
        for case in oracle.case.iter().filter(|case| {
            args.case_id.as_ref().is_none_or(|case_id| case_id == &case.id)
        }) {
            let result = evaluate_case(case, &aat);
            if result.passed {
                println!("PASS {}", result.case_id);
            } else {
                failures += 1;
                println!("FAIL {}", result.case_id);
                for failure in result.failures {
                    println!("  - {failure}");
                }
            }
        }
        if failures > 0 {
            anyhow::bail!("{failures} oracle case(s) failed");
        }
    } else {
        println!("loaded {} oracle cases", oracle.case.len());
    }
    Ok(())
}
```

- [ ] **Step 5: Run evaluator tests**

Run:

```bash
CARGO_TARGET_DIR=/db/ab-validator/target-ab-oracle \
  cargo test --manifest-path crates/ab-oracle/Cargo.toml -- --nocapture
```

Expected: PASS.

- [ ] **Step 6: Commit**

Run:

```bash
git add crates/ab-oracle
git commit -m "feat: evaluate aat oracle cases"
```

Expected: commit succeeds.

---

### Task 5: Add Shared Adapter Fidelity Runner

**Files:**
- Create: `tests/adapter-fidelity-smoke.sh`
- Modify: `crates/ab-oracle/src/main.rs`
- Modify: `docs/adapter-fidelity.md`

- [ ] **Step 1: Add failing smoke script**

Create `tests/adapter-fidelity-smoke.sh`:

```bash
#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
out_dir="${AB_FIDELITY_OUT_DIR:-/db/ab-validator/aat-fidelity/smoke}"
mkdir -p "$out_dir"

case_source='耳朶を※［＃「てへん＋掌」、第4水準2-13-47］えて'

printf '%s' "$case_source" \
  | cargo run --quiet --manifest-path "$repo_root/adapters/aozora2/Cargo.toml" -- --mode aat \
  > "$out_dir/aozora2-gaiji-jis.aat.json"

cargo run --quiet --manifest-path "$repo_root/crates/ab-oracle/Cargo.toml" -- \
  --oracle "$repo_root/data/aat-oracle-cases.toml" \
  --case-id gaiji.jis.2-13-47 \
  --aat "$out_dir/aozora2-gaiji-jis.aat.json"
```

Run:

```bash
bash tests/adapter-fidelity-smoke.sh
```

Expected: initially FAIL if `ab-oracle` CLI cannot filter by `--case-id` or if the adapter does not match the oracle.

- [ ] **Step 2: Make the smoke script runnable with `/db` targets**

Update the script to include:

```bash
export CARGO_TARGET_DIR="${CARGO_TARGET_DIR:-/db/ab-validator/target-adapter-fidelity}"
export TMPDIR="${TMPDIR:-/db/ab-validator/tmp}"
export TMP="${TMP:-/db/ab-validator/tmp}"
export TEMP="${TEMP:-/db/ab-validator/tmp}"
mkdir -p "$TMPDIR"
```

- [ ] **Step 3: Extend docs with runner usage**

Append to `docs/adapter-fidelity.md`:

```markdown
## Local Fidelity Smoke Run

Run:

```bash
bash tests/adapter-fidelity-smoke.sh
```

The script writes intermediate adapter AAT JSON under `/db/ab-validator/aat-fidelity/smoke`.
```
```

- [ ] **Step 4: Run smoke script**

Run:

```bash
bash tests/adapter-fidelity-smoke.sh
```

Expected: PASS and print `PASS gaiji.jis.2-13-47`.

- [ ] **Step 5: Commit**

Run:

```bash
chmod +x tests/adapter-fidelity-smoke.sh
git add tests/adapter-fidelity-smoke.sh docs/adapter-fidelity.md crates/ab-oracle/src/main.rs
git commit -m "test: add adapter fidelity smoke runner"
```

Expected: commit succeeds.

---

### Task 6: Generate Adapter-by-Adapter Oracle Reports

**Files:**
- Modify: `crates/ab-oracle/src/main.rs`
- Create: `crates/ab-oracle/src/adapter_run.rs`
- Create: `tests/adapter-oracle-report-smoke.sh`
- Modify: `docs/adapter-fidelity.md`

- [ ] **Step 1: Add failing report smoke test**

Create `tests/adapter-oracle-report-smoke.sh`:

```bash
#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
out_dir="${AB_FIDELITY_OUT_DIR:-/db/ab-validator/aat-fidelity/report-smoke}"
mkdir -p "$out_dir"

export CARGO_TARGET_DIR="${CARGO_TARGET_DIR:-/db/ab-validator/target-adapter-fidelity}"
export TMPDIR="${TMPDIR:-/db/ab-validator/tmp}"
export TMP="${TMP:-/db/ab-validator/tmp}"
export TEMP="${TEMP:-/db/ab-validator/tmp}"
mkdir -p "$TMPDIR"

cargo run --quiet --manifest-path "$repo_root/crates/ab-oracle/Cargo.toml" -- \
  --oracle "$repo_root/data/aat-oracle-cases.toml" \
  --adapter aozora2="$repo_root/target/debug/aozora2-adapter" \
  --report-json "$out_dir/report.json" \
  --report-md "$out_dir/report.md"

test -s "$out_dir/report.json"
test -s "$out_dir/report.md"
rg -n "aozora2|gaiji.jis.2-13-47" "$out_dir/report.md"
```

Run:

```bash
bash tests/adapter-oracle-report-smoke.sh
```

Expected: FAIL because `--adapter`, `--report-json`, and `--report-md` do not exist.

- [ ] **Step 2: Implement adapter command model**

Create `crates/ab-oracle/src/adapter_run.rs`:

```rust
use std::{io::Write, path::PathBuf, process::{Command, Stdio}};

use anyhow::{Context, Result};

#[derive(Debug, Clone)]
pub struct AdapterSpec {
    pub id: String,
    pub path: PathBuf,
}

impl AdapterSpec {
    pub fn parse(raw: &str) -> Result<Self> {
        let (id, path) = raw
            .split_once('=')
            .with_context(|| format!("adapter spec must be id=path, got {raw:?}"))?;
        Ok(Self {
            id: id.to_owned(),
            path: PathBuf::from(path),
        })
    }
}

pub fn run_adapter(spec: &AdapterSpec, source: &str) -> Result<serde_json::Value> {
    let mut child = Command::new(&spec.path)
        .arg("--mode")
        .arg("aat")
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .spawn()
        .with_context(|| format!("failed to spawn adapter {}", spec.id))?;

    child
        .stdin
        .as_mut()
        .context("adapter stdin missing")?
        .write_all(source.as_bytes())?;

    let output = child.wait_with_output()?;
    if !output.status.success() {
        anyhow::bail!(
            "adapter {} failed: {}",
            spec.id,
            String::from_utf8_lossy(&output.stderr)
        );
    }
    Ok(serde_json::from_slice(&output.stdout)?)
}
```

- [ ] **Step 3: Add JSON and markdown report output**

Extend `crates/ab-oracle/src/main.rs` so it accepts:

```rust
#[arg(long = "adapter")]
adapters: Vec<String>,

#[arg(long)]
report_json: Option<PathBuf>,

#[arg(long)]
report_md: Option<PathBuf>,
```

For each oracle case and adapter, run the adapter, evaluate the AAT, and write rows:

```json
{
  "adapter": "aozora2",
  "case_id": "gaiji.jis.2-13-47",
  "passed": true,
  "failures": []
}
```

Markdown table columns:

```markdown
| Adapter | Case | Result | Failures |
| --- | --- | --- | --- |
```

- [ ] **Step 4: Run report smoke test**

Run:

```bash
cargo build --manifest-path adapters/aozora2/Cargo.toml
bash tests/adapter-oracle-report-smoke.sh
```

Expected: PASS and report files exist under `/db/ab-validator/aat-fidelity/report-smoke`.

- [ ] **Step 5: Commit**

Run:

```bash
chmod +x tests/adapter-oracle-report-smoke.sh
git add crates/ab-oracle tests/adapter-oracle-report-smoke.sh docs/adapter-fidelity.md
git commit -m "feat: report adapter oracle fidelity"
```

Expected: commit succeeds.

---

### Task 7: Backfill Coverage Matrix From Oracle Results

**Files:**
- Modify: `data/aozora-syntax-coverage.schema.json`
- Modify: `data/aozora-syntax-coverage.toml`
- Modify: `crates/ab-coverage/src/matrix.rs`
- Modify: `crates/ab-coverage/src/merge.rs`
- Modify: `docs/coverage-report.md`

- [ ] **Step 1: Add failing schema/model test for oracle fields**

Add to `crates/ab-coverage/tests/schema_matrix.rs`:

```rust
#[test]
fn matrix_accepts_oracle_case_references() {
    let matrix = ab_coverage::matrix::CoverageMatrix::from_toml(
        std::path::Path::new("data/aozora-syntax-coverage.toml"),
    )
    .unwrap();

    let gaiji = matrix
        .rows()
        .iter()
        .find(|row| row.id == "gaiji.jis_code")
        .expect("gaiji.jis_code row");

    assert!(gaiji.oracle_cases.iter().any(|case| case == "gaiji.jis.2-13-47"));
}
```

Run:

```bash
CARGO_TARGET_DIR=/db/ab-validator/target-ab-coverage \
  cargo test --manifest-path crates/ab-coverage/Cargo.toml matrix_accepts_oracle_case_references -- --nocapture
```

Expected: FAIL because `oracle_cases` does not exist.

- [ ] **Step 2: Extend schema and Rust row model**

Add optional `oracle_cases` to `data/aozora-syntax-coverage.schema.json`:

```json
"oracle_cases": {
  "type": "array",
  "items": { "type": "string", "minLength": 1 }
}
```

Add to `crates/ab-coverage/src/matrix.rs::Row`:

```rust
#[serde(default)]
pub oracle_cases: Vec<String>,
```

- [ ] **Step 3: Link seed oracle cases to matrix rows**

Update `data/aozora-syntax-coverage.toml` rows:

```toml
oracle_cases = ["gaiji.jis.2-13-47"]
```

for `gaiji.jis_code`, and:

```toml
oracle_cases = ["gaiji.unicode.u546d"]
```

for `gaiji.unicode_codepoint`.

Use:

```bash
rg -n 'id = "gaiji.jis_code"|id = "gaiji.unicode_codepoint"' data/aozora-syntax-coverage.toml
```

to locate the exact rows.

- [ ] **Step 4: Run coverage tests**

Run:

```bash
CARGO_TARGET_DIR=/db/ab-validator/target-ab-coverage \
  cargo test --manifest-path crates/ab-coverage/Cargo.toml -- --nocapture
```

Expected: PASS.

- [ ] **Step 5: Commit**

Run:

```bash
git add data/aozora-syntax-coverage.schema.json data/aozora-syntax-coverage.toml crates/ab-coverage/src/matrix.rs crates/ab-coverage/tests/schema_matrix.rs
git commit -m "feat: link syntax rows to oracle cases"
```

Expected: commit succeeds.

---

### Task 8: Reconstruct `aozora2` Block Containers

**Files:**
- Modify: `adapters/aozora2/src/lib.rs`
- Modify: `data/aat-schema.json` only if contract gaps are discovered
- Modify: `docs/adapter-fidelity.md`

- [ ] **Step 1: Add failing block reconstruction tests**

Add to `adapters/aozora2/src/lib.rs` tests:

```rust
#[test]
fn projection_reconstructs_jisage_block_from_block_markers() {
    let nodes = vec![
        Node::BlockStart {
            block_type: BlockType::Jisage,
            params: BlockParams {
                width: Some(2),
                is_block: true,
                ..BlockParams::default()
            },
        },
        Node::Text("字下げされた段落。".to_owned()),
        Node::BlockEnd {
            block_type: BlockType::Jisage,
            params: BlockParams::default(),
        },
    ];

    let blocks = aozora_nodes_to_aat_blocks(&nodes);
    assert_eq!(blocks[0]["kind"], "jisage_block");
    assert_eq!(blocks[0]["x-indent"], 2);
    assert_eq!(blocks[0]["children"][0]["kind"], "paragraph");
}
```

Run:

```bash
CARGO_TARGET_DIR=/db/ab-validator/target-aozora2 \
  cargo test --manifest-path adapters/aozora2/Cargo.toml projection_reconstructs_jisage_block_from_block_markers -- --nocapture
```

Expected: FAIL because `aozora_nodes_to_aat_blocks` does not exist.

- [ ] **Step 2: Introduce block builder**

Implement a small block builder in `adapters/aozora2/src/lib.rs`:

```rust
fn aozora_nodes_to_aat_blocks(nodes: &[Node]) -> Vec<serde_json::Value> {
    let mut blocks = Vec::new();
    let mut current_inline = Vec::new();
    let mut stack: Vec<BlockFrame> = Vec::new();
    for node in nodes {
        match node {
            Node::BlockStart { block_type, params } if params.is_block => {
                flush_paragraph(&mut blocks, &mut current_inline);
                stack.push(BlockFrame::new(*block_type, params.clone()));
            }
            Node::BlockEnd { block_type, .. } if stack.last().is_some_and(|frame| frame.block_type == *block_type) => {
                flush_paragraph(&mut blocks, &mut current_inline);
                let frame = stack.pop().expect("frame checked above");
                push_block_frame(&mut blocks, frame);
            }
            other => append_aozora_node(&mut current_inline, other),
        }
    }
    flush_paragraph(&mut blocks, &mut current_inline);
    blocks
}
```

Use helper functions to keep `append_aozora_node` focused on inline projection.

- [ ] **Step 3: Route `build_aat` through block builder**

Change:

```rust
let content = parse_inline_content(body_text(&decoded.text));
```

to parse nodes once:

```rust
let tokens = aozora_core::tokenize(body_text(&decoded.text));
let nodes = aozora_core::parse(&tokens);
let blocks = aozora_nodes_to_aat_blocks(&nodes);
```

Then use `blocks` directly in the AAT envelope.

- [ ] **Step 4: Preserve raw fallback for unbalanced blocks**

If a `BlockEnd` has no matching start, emit the current raw node behavior:

```json
{"kind":"raw","source":"BlockEnd(Jisage)"}
```

Add a test:

```rust
#[test]
fn unmatched_block_end_remains_raw() {
    let nodes = vec![Node::BlockEnd {
        block_type: BlockType::Jisage,
        params: BlockParams::default(),
    }];
    let blocks = aozora_nodes_to_aat_blocks(&nodes);
    assert_eq!(blocks[0]["content"][0]["kind"], "raw");
}
```

- [ ] **Step 5: Run adapter and schema tests**

Run:

```bash
CARGO_TARGET_DIR=/db/ab-validator/target-aozora2 \
  cargo test --manifest-path adapters/aozora2/Cargo.toml -- --nocapture

CARGO_TARGET_DIR=/db/ab-validator/target-ab-check \
  cargo test --manifest-path crates/ab-check/Cargo.toml -- --nocapture
```

Expected: PASS.

- [ ] **Step 6: Commit**

Run:

```bash
git add adapters/aozora2/src/lib.rs data/aat-schema.json docs/adapter-fidelity.md
git commit -m "feat: reconstruct aozora2 block containers"
```

Expected: commit succeeds.

---

### Task 9: Add Source Span Support to the Contract and One Adapter

**Files:**
- Modify: `docs/aat-contract.md`
- Modify: `adapters/aozora-rs/src/aat.rs`
- Modify: `adapters/aozora-rs/src/source.rs`
- Modify: `adapters/aozora-rs/src/parser.rs`
- Modify: `adapters/aozora-rs/src/lib.rs`

- [ ] **Step 1: Decide span scope for first implementation**

Use `aozora-rs` first because it already has richer parser/source pipeline metrics. The first span scope is:

- source byte start/end on source-derived gaiji supplements;
- line start/end where already cheaply available;
- no guarantee that every parser-normalized node has a span.

Document this in `docs/aat-contract.md`:

```markdown
## Span Coverage Levels

| Level | Meaning |
| --- | --- |
| none | Adapter does not emit spans. |
| source-derived | Spans exist for nodes recovered directly from source scanning. |
| parser-derived | Spans exist for parser-emitted nodes when upstream exposes offsets. |
| complete | Every AAT node has a span. |
```

- [ ] **Step 2: Add failing test for source-derived gaiji span**

Add to `adapters/aozora-rs/src/aat.rs` tests:

```rust
#[test]
fn source_annotation_supplements_include_source_span_for_gaiji() {
    let source = "耳朶を※［＃「てへん＋掌」、第4水準2-13-47］えて";
    let aat = build_aat_for_test(source);
    let gaiji = find_first_kind(&aat, "gaiji").expect("gaiji node");
    assert_eq!(gaiji["span"]["byte_start"], 9);
    assert!(gaiji["span"]["byte_end"].as_u64().unwrap() > 9);
}
```

Run:

```bash
nix develop .# --command cargo test --manifest-path adapters/aozora-rs/Cargo.toml source_annotation_supplements_include_source_span_for_gaiji -- --nocapture
```

Expected: FAIL because the supplement does not include `span`.

- [ ] **Step 3: Thread source span through source annotation supplements**

Add span fields to the internal source supplement structs and serialize them using the existing AAT schema `span` object:

```json
{
  "line_start": 1,
  "line_end": 1,
  "byte_start": 9,
  "byte_end": 58
}
```

Use byte offsets into the decoded source text.

- [ ] **Step 4: Run aozora-rs tests**

Run:

```bash
nix develop .# --command cargo test --manifest-path adapters/aozora-rs/Cargo.toml -- --nocapture
```

Expected: PASS.

- [ ] **Step 5: Commit**

Run:

```bash
git add docs/aat-contract.md adapters/aozora-rs/src/aat.rs adapters/aozora-rs/src/source.rs adapters/aozora-rs/src/parser.rs adapters/aozora-rs/src/lib.rs
git commit -m "feat: add source-derived spans to aozora-rs aat"
```

Expected: commit succeeds.

---

### Task 10: Add Marimo/Report Drilldown Hooks for Fidelity Results

**Files:**
- Modify: `reports/morph-warehouse/warehouse_explorer.py`
- Modify: `reports/morph-warehouse/README.md` if present
- Modify: `crates/ab-morph-run/sql/schema.sql` only if fidelity result tables are persisted into the warehouse

- [ ] **Step 1: Choose non-invasive first integration**

Do not modify the warehouse schema first. Read adapter oracle report JSON files from `/db/ab-validator/aat-fidelity/` in the marimo notebook and display:

- adapter id,
- oracle case id,
- pass/fail,
- failure messages,
- AAT JSON preview for the selected case.

- [ ] **Step 2: Add failing notebook smoke assertion**

Extend `tests/morph-warehouse-marimo-notebook-smoke.sh` to assert that `warehouse_explorer.py` contains:

```bash
rg -n "adapter oracle|aat-fidelity|oracle case" reports/morph-warehouse/warehouse_explorer.py
```

Run:

```bash
bash tests/morph-warehouse-marimo-notebook-smoke.sh
```

Expected: FAIL until the notebook includes the fidelity panel.

- [ ] **Step 3: Add notebook controls**

In `reports/morph-warehouse/warehouse_explorer.py`, add a view that:

- loads `/db/ab-validator/aat-fidelity/report-smoke/report.json` when present;
- uses `mo.ui.dropdown` for adapter and case filters;
- uses `mo.ui.table` for result rows;
- uses `mo.json` for selected AAT preview when an `aat_path` field is present.

Keep controls local to the fidelity view so unrelated search views do not show irrelevant controls.

- [ ] **Step 4: Run notebook smoke test**

Run:

```bash
bash tests/morph-warehouse-marimo-notebook-smoke.sh
```

Expected: PASS.

- [ ] **Step 5: Commit**

Run:

```bash
git add reports/morph-warehouse/warehouse_explorer.py tests/morph-warehouse-marimo-notebook-smoke.sh reports/morph-warehouse/README.md
git commit -m "feat: expose adapter fidelity in warehouse explorer"
```

Expected: commit succeeds.

---

## Execution Order

1. Task 1: AAT contract.
2. Task 2: Oracle fixture schema and seed cases.
3. Task 3: Oracle loader crate.
4. Task 4: Oracle evaluator.
5. Task 5: Single-adapter fidelity smoke runner.
6. Task 6: Multi-adapter oracle reports.
7. Task 7: Link oracle cases to syntax coverage matrix.
8. Task 8: `aozora2` block reconstruction.
9. Task 9: source spans in `aozora-rs`.
10. Task 10: marimo/report drilldown.

This order produces useful checkpoints early and avoids doing a full corpus run before the semantics and oracle are stable.

## Verification Bundle

After all tasks in a development slice, run:

```bash
CARGO_TARGET_DIR=/db/ab-validator/target-aat-fidelity \
  cargo test --manifest-path crates/ab-oracle/Cargo.toml -- --nocapture

CARGO_TARGET_DIR=/db/ab-validator/target-aat-fidelity \
  cargo test --manifest-path crates/ab-check/Cargo.toml -- --nocapture

CARGO_TARGET_DIR=/db/ab-validator/target-aat-fidelity \
  cargo test --manifest-path crates/ab-coverage/Cargo.toml -- --nocapture

CARGO_TARGET_DIR=/db/ab-validator/target-aat-fidelity \
  cargo test --manifest-path adapters/aozora2/Cargo.toml -- --nocapture

nix develop .# --command cargo test --manifest-path adapters/aozora-rs/Cargo.toml -- --nocapture

nix develop "${repo_root}#aozora2html" --command python -m pytest adapters/aozora2html/tests/ -v

bash tests/aat-oracle-cases-schema-smoke.sh
bash tests/adapter-fidelity-smoke.sh
bash tests/adapter-oracle-report-smoke.sh
```

Expected: every command exits 0. Large generated outputs remain under `/db/ab-validator`.

## Self-Review

- Spec coverage: the plan covers AAT contract, oracle correctness, shared fixtures, adapter fidelity reports, `aozora2` block reconstruction, source spans, and marimo/report drilldown.
- Placeholder scan: no unresolved placeholders are present.
- Type consistency: `OracleCase`, `Expected`, `ExpectedNode`, and `ExpectedGaiji` names are used consistently across loader, evaluator, and CLI tasks.
- Scope check: each task is independently testable and can be committed separately.
