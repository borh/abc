# AAT Fidelity and Oracle Roadmap v2 Implementation Plan

> **Superseded:** Use `docs/superpowers/plans/2026-05-03-aat-fidelity-oracle-roadmap-v3.md` instead. The v3 plan keeps the three result axes but composes with existing `ab-ir`/`ab-check` semantics and splits oracle expectations from upstream observations.

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Build a durable AAT fidelity framework that separately reports schema validity, adapter faithfulness to upstream parser behavior, and independent oracle correctness.

**Architecture:** The framework has five deliberately separate parts: the AAT semantic contract, structured adapter-fidelity notes, upstream parser observations, oracle expectations, and report/UI surfaces. `ab-oracle` evaluates AAT output against oracle expectations and upstream observations, but it must not infer upstream faithfulness from oracle correctness alone. Generated outputs and intermediate AAT JSON stay under `/db/ab-validator`.

**Tech Stack:** Rust 2024 workspace crates, JSON Schema 2020-12, TOML fixture data, existing Aozora adapters, Nix-managed test environments, per-crate Cargo target directories under `/db/ab-validator`, and optional marimo reporting.

---

## Review-Driven Corrections

This v2 plan fixes the architectural issues in the previous roadmap:

- Adapter faithfulness and oracle correctness are separate result axes.
- Oracle assertions include path-scoped structural assertions, sequence assertions, negative assertions, and field absence checks.
- Node counting never defaults to an unrestricted recursive scan.
- `aozora2` block reconstruction starts with parser-output discovery tests before implementation.
- Spans use an explicit decoded-text coordinate system and do not claim original file byte offsets.
- Smoke scripts use a shared Nix/preflight layer and per-crate target directories.
- Fidelity UI work moves to a standalone `reports/aat-fidelity` notebook before any warehouse integration.
- Schema versioning is explicit: AAT uses top-level `version`; oracle cases declare `aat_version`.
- Oracle cases map to syntax coverage rows when they are created, not later.
- Adapter fidelity notes become structured TOML, not an unvalidated markdown table.

## Result Axes

Every adapter/case report row must expose these independent statuses:

| Axis | Values | Meaning |
| --- | --- | --- |
| `schema_status` | `pass`, `fail` | Whether adapter output validates against `data/aat-schema.json`. |
| `upstream_status` | `faithful`, `wrapper_mismatch`, `no_observation`, `not_applicable` | Whether adapter output preserves known upstream parser behavior for this case. |
| `oracle_status` | `pass`, `fail`, `skipped` | Whether adapter output matches independent Aozora correctness expectations. |

Example: if upstream `aozora-rs-gaiji` leaves `第4水準2-13-47` unresolved and the adapter reports it unresolved, then `upstream_status = "faithful"` and `oracle_status = "fail"`.

## File Structure

- Create `docs/aat-contract.md` for semantic node meanings, result axes, schema versioning, and span coordinates.
- Create `data/adapter-fidelity-notes.schema.json` and `data/adapter-fidelity-notes.toml` for structured adapter limitations.
- Create `data/aat-oracle-cases.schema.json` and `data/aat-oracle-cases.toml` for oracle cases, upstream observations, and syntax row links.
- Create `crates/ab-oracle/` for oracle loading, path-scoped assertions, upstream-faithfulness classification, adapter runs, and JSON/Markdown reports.
- Create shared shell helpers under `tests/lib/aat-fidelity-env.sh`.
- Create smoke scripts under `tests/`.
- Create `reports/aat-fidelity/` for standalone fidelity exploration.
- Only modify `reports/morph-warehouse/` after fidelity reports have a stable persisted schema.

---

### Task 1: Define the AAT Contract, Versioning, and Span Coordinates

**Files:**
- Create: `docs/aat-contract.md`
- Modify: `docs/adapter-fidelity.md`

- [ ] **Step 1: Write the contract**

Create `docs/aat-contract.md`:

```markdown
# AAT Contract

The Aozora Adapter Tree (AAT) is the normalized JSON emitted by parser adapters. It records adapter output and provenance. It does not by itself assert linguistic correctness.

## Versioning

AAT documents use top-level `version`. Current AAT is `version = 1`. Oracle cases use `aat_version = 1` to declare which AAT contract they target.

Schema-compatible additions to AAT v1 may add optional fields. Required field changes or changed node semantics require AAT v2.

## Result Axes

| Axis | Question |
| --- | --- |
| Schema validity | Is this structurally valid AAT for the declared version? |
| Adapter faithfulness | Did the adapter preserve what its upstream parser emitted? |
| Oracle correctness | Does the output match independent Aozora/literary expectations? |

These axes are independent. A faithful adapter can fail oracle correctness when upstream is wrong or incomplete.

## First-Class Nodes

| Feature | AAT shape | Required fields | Plaintext projection | Notes |
| --- | --- | --- | --- | --- |
| plain text | `text` | `value` | `value` | Adjacent text may be coalesced. |
| ruby | `ruby` | `base`, `reading` | `base` | `base_content` and `reading_content` preserve nested nodes when available. |
| gaiji | `gaiji` | `description`, `resolved`, `unresolved_reason` | `resolved` if present, otherwise empty | Correct resolution is an oracle judgment. |
| accent | `accent` | `code`, `name`, `resolved` | `resolved` if present, otherwise `name` | Used for Latin accent notation. |
| figure | `figure` | `filename`, `alt`, `css_class` | empty | Captions are attached or adjacent only when upstream exposes that relationship. |
| style | `style` | `style_type`, `content` | child projection | Emphasis, inline heading preservation, and related inline scopes. |
| font size | `font_size` | `size_type`, `level`, `content` | child projection | `size_type` is adapter-normalized. |
| tcy | `tcy` | `content` | child projection | Vertical-in-horizontal text. |
| yokogumi | `yokogumi` | `content` | child projection | Inline horizontal composition. |
| keigakomi | `keigakomi` | `content` | child projection | Inline ruled enclosure. |
| caption | `caption` | `content` | child projection | Inline caption representation. |
| warigaki | `warigaki` | `upper`, `lower` | upper then lower projection | Split semantics should be preserved when upstream exposes them. |
| raw | `raw` | `source` | empty | Faithful escape hatch for unsupported upstream events. |
| paragraph | `paragraph` | `content` | child projection | Default block. |
| heading | `heading` | `level`, `style`, `content` | child projection | Preferred block-level heading shape. |
| block container | `*_block` | `children` | child projection | Block ranges such as jisage, quote, keigakomi, yokogumi, caption. |

## Span Coordinate System

AAT v1 spans are decoded-text coordinates, not original file byte offsets.

- `byte_start` and `byte_end` are UTF-8 byte offsets into the decoded source string used by the adapter.
- `line_start` and `line_end` are one-based decoded-source line numbers.
- Optional `char_start` and `char_end` are Unicode scalar-value offsets into the decoded source string.
- Optional `column_start` and `column_end` are one-based decoded-source columns.
- Original encoded byte offsets require separate optional fields such as `raw_byte_start` and `raw_byte_end`; adapters must not imply decoded offsets are raw-file offsets.

Tests must compute expected offsets from the fixture string, for example with `source.find(marker).unwrap()`, rather than using unexplained numeric literals.

## Compatibility Rule

Adapters may emit `raw` for faithfully preserved upstream events that AAT does not yet model. Dropped parser events require a structured entry in `data/adapter-fidelity-notes.toml`.
```

- [ ] **Step 2: Link from adapter fidelity docs**

Append to `docs/adapter-fidelity.md`:

```markdown
For node semantics, result axes, schema versioning, and span coordinates, see `docs/aat-contract.md`.
```

- [ ] **Step 3: Verify**

Run:

```bash
test -s docs/aat-contract.md
rg -n "Result Axes|Span Coordinate System|Compatibility Rule" docs/aat-contract.md
```

Expected: exit 0.

- [ ] **Step 4: Commit**

Run:

```bash
git add docs/aat-contract.md docs/adapter-fidelity.md
git commit -m "docs: define aat contract and fidelity axes"
```

Expected: commit succeeds with only these paths staged.

---

### Task 2: Add Structured Adapter Fidelity Notes

**Files:**
- Create: `data/adapter-fidelity-notes.schema.json`
- Create: `data/adapter-fidelity-notes.toml`
- Modify: `docs/adapter-fidelity.md`
- Create: `tests/adapter-fidelity-notes-schema-smoke.sh`

- [ ] **Step 1: Add failing schema smoke test**

Create `tests/adapter-fidelity-notes-schema-smoke.sh`:

```bash
#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

nix-shell -p 'python3.withPackages(ps: [ ps.jsonschema ps.tomli ])' --run "
python3 - <<'PY'
import json
from pathlib import Path
import tomli
import jsonschema

root = Path('$repo_root')
schema = json.loads((root / 'data' / 'adapter-fidelity-notes.schema.json').read_text())
notes = tomli.loads((root / 'data' / 'adapter-fidelity-notes.toml').read_text())
jsonschema.validate(notes, schema)
print(f\"validated {len(notes['note'])} adapter fidelity notes\")
PY
"
```

Run:

```bash
bash tests/adapter-fidelity-notes-schema-smoke.sh
```

Expected: FAIL because the files do not exist yet.

- [ ] **Step 2: Add note schema**

Create `data/adapter-fidelity-notes.schema.json`:

```json
{
  "$schema": "https://json-schema.org/draft/2020-12/schema",
  "$id": "https://example.com/ab-validator/adapter-fidelity-notes.schema.json",
  "type": "object",
  "required": ["note"],
  "additionalProperties": false,
  "properties": {
    "note": {
      "type": "array",
      "items": { "$ref": "#/$defs/Note" }
    }
  },
  "$defs": {
    "Note": {
      "type": "object",
      "required": ["id", "adapter", "syntax_row_ids", "status", "summary"],
      "additionalProperties": false,
      "properties": {
        "id": { "type": "string", "minLength": 1 },
        "adapter": { "type": "string", "minLength": 1 },
        "syntax_row_ids": { "type": "array", "items": { "type": "string", "minLength": 1 } },
        "status": { "enum": ["active", "retired"] },
        "summary": { "type": "string", "minLength": 1 },
        "evidence": { "type": "string" },
        "retired_by": { "type": "string" }
      }
    }
  }
}
```

- [ ] **Step 3: Seed current notes**

Create `data/adapter-fidelity-notes.toml`:

```toml
[[note]]
id = "aozora-rs-gaiji-jis-unresolved"
adapter = "aozora-rs"
syntax_row_ids = ["gaiji.jis_code"]
status = "active"
summary = "Adapter faithfully follows aozora-rs-gaiji v0.6.0, which does not resolve some JIS-form gaiji that the oracle expects."
evidence = "docs/adapter-fidelity.md"

[[note]]
id = "aozora2-single-paragraph-before-block-reconstruction"
adapter = "aozora2"
syntax_row_ids = ["indentation.jisage", "block.keigakomi", "layout.caption"]
status = "active"
summary = "Adapter preserves block markers as raw nodes until block reconstruction is implemented and verified."
evidence = "adapters/aozora2/src/lib.rs"

[[note]]
id = "aozora2html-xhtml-projection"
adapter = "aozora2html"
syntax_row_ids = ["gaiji.marker", "warichu.basic"]
status = "active"
summary = "Adapter maps rendered XHTML, so source-only markup distinctions not represented in XHTML cannot always be recovered."
evidence = "adapters/aozora2html/README.md"
```

- [ ] **Step 4: Verify**

Run:

```bash
chmod +x tests/adapter-fidelity-notes-schema-smoke.sh
bash tests/adapter-fidelity-notes-schema-smoke.sh
```

Expected: PASS.

- [ ] **Step 5: Commit**

Run:

```bash
git add data/adapter-fidelity-notes.schema.json data/adapter-fidelity-notes.toml tests/adapter-fidelity-notes-schema-smoke.sh docs/adapter-fidelity.md
git commit -m "docs: structure adapter fidelity notes"
```

Expected: commit succeeds.

---

### Task 3: Add Oracle Case Schema With Path-Scoped Assertions

**Files:**
- Create: `data/aat-oracle-cases.schema.json`
- Create: `data/aat-oracle-cases.toml`
- Create: `tests/aat-oracle-cases-schema-smoke.sh`

- [ ] **Step 1: Add failing schema smoke test**

Create `tests/aat-oracle-cases-schema-smoke.sh`:

```bash
#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

nix-shell -p 'python3.withPackages(ps: [ ps.jsonschema ps.tomli ])' --run "
python3 - <<'PY'
import json
from pathlib import Path
import tomli
import jsonschema

root = Path('$repo_root')
schema = json.loads((root / 'data' / 'aat-oracle-cases.schema.json').read_text())
cases = tomli.loads((root / 'data' / 'aat-oracle-cases.toml').read_text())
jsonschema.validate(cases, schema)
print(f\"validated {len(cases['case'])} oracle cases\")
PY
"
```

Run:

```bash
bash tests/aat-oracle-cases-schema-smoke.sh
```

Expected: FAIL because schema and cases do not exist.

- [ ] **Step 2: Create schema**

Create `data/aat-oracle-cases.schema.json`:

```json
{
  "$schema": "https://json-schema.org/draft/2020-12/schema",
  "$id": "https://example.com/ab-validator/aat-oracle-cases.schema.json",
  "title": "AAT Oracle Cases",
  "type": "object",
  "required": ["aat_version", "case"],
  "additionalProperties": false,
  "properties": {
    "aat_version": { "const": 1 },
    "case": {
      "type": "array",
      "minItems": 1,
      "items": { "$ref": "#/$defs/Case" }
    }
  },
  "$defs": {
    "Case": {
      "type": "object",
      "required": ["id", "syntax_row_ids", "category", "source_utf8", "oracle"],
      "additionalProperties": false,
      "properties": {
        "id": { "type": "string", "minLength": 1 },
        "syntax_row_ids": { "type": "array", "minItems": 1, "items": { "type": "string", "minLength": 1 } },
        "category": { "type": "string", "minLength": 1 },
        "source_utf8": { "type": "string", "minLength": 1 },
        "notes": { "type": "string" },
        "oracle": { "$ref": "#/$defs/OracleExpectations" },
        "upstream": { "type": "array", "items": { "$ref": "#/$defs/UpstreamObservation" } }
      }
    },
    "OracleExpectations": {
      "type": "object",
      "additionalProperties": false,
      "properties": {
        "visible_text": { "type": "string" },
        "nodes": { "type": "array", "items": { "$ref": "#/$defs/NodeAssertion" } },
        "sequence": { "type": "array", "items": { "$ref": "#/$defs/SequenceAssertion" } },
        "gaiji": { "type": "array", "items": { "$ref": "#/$defs/GaijiAssertion" } }
      }
    },
    "NodeAssertion": {
      "type": "object",
      "required": ["selector", "kind"],
      "additionalProperties": false,
      "properties": {
        "selector": { "type": "string", "minLength": 1 },
        "kind": { "type": "string", "minLength": 1 },
        "count": { "type": "integer", "minimum": 0 },
        "min_count": { "type": "integer", "minimum": 0 },
        "absent": { "type": "boolean" },
        "fields": { "type": "object" },
        "field_absent": { "type": "array", "items": { "type": "string" } }
      }
    },
    "SequenceAssertion": {
      "type": "object",
      "required": ["selector", "kinds"],
      "additionalProperties": false,
      "properties": {
        "selector": { "type": "string", "minLength": 1 },
        "kinds": { "type": "array", "minItems": 1, "items": { "type": "string" } }
      }
    },
    "GaijiAssertion": {
      "type": "object",
      "required": ["selector", "description", "resolved"],
      "additionalProperties": false,
      "properties": {
        "selector": { "type": "string", "minLength": 1 },
        "description": { "type": "string", "minLength": 1 },
        "resolved": { "type": ["string", "null"] },
        "jis_code": { "type": ["string", "null"] },
        "unresolved_reason": { "type": ["string", "null"] },
        "source": { "type": "string" }
      }
    },
    "UpstreamObservation": {
      "type": "object",
      "required": ["adapter", "status", "summary"],
      "additionalProperties": false,
      "properties": {
        "adapter": { "type": "string", "minLength": 1 },
        "status": { "enum": ["parsed", "normalised", "unresolved", "dropped", "aborts", "unknown"] },
        "summary": { "type": "string", "minLength": 1 },
        "selector": { "type": "string" },
        "kind": { "type": "string" },
        "fields": { "type": "object" },
        "evidence": { "type": "string" }
      }
    }
  }
}
```

Selector syntax for v1 is deliberately small:

- `blocks.*` selects block objects.
- `blocks.*.content.*` selects direct inline children of paragraph-like blocks.
- `**` selects all objects recursively and must be used only when a case explicitly needs recursive matching.
- Counting defaults to the selector result only; it never scans the whole AAT recursively unless the selector is `**`.

- [ ] **Step 3: Add seed cases mapped to syntax matrix rows**

Create `data/aat-oracle-cases.toml`:

```toml
aat_version = 1

[[case]]
id = "gaiji.jis.2-13-47"
syntax_row_ids = ["gaiji.jis_code"]
category = "gaiji"
source_utf8 = "耳朶を※［＃「てへん＋掌」、第4水準2-13-47］えて"
notes = "JIS X 0213 plane 2 row 13 cell 47 should resolve to 撑."

[case.oracle]
visible_text = "耳朶を撑えて"

[[case.oracle.sequence]]
selector = "blocks.*.content"
kinds = ["text", "gaiji", "text"]

[[case.oracle.gaiji]]
selector = "blocks.*.content.*"
description = "「てへん＋掌」、第4水準2-13-47"
resolved = "撑"
jis_code = "2-13-47"
unresolved_reason = ""
source = "JIS X 0213"

[[case.upstream]]
adapter = "aozora-rs"
status = "unresolved"
summary = "aozora-rs-gaiji v0.6.0 does not resolve this JIS-form marker."
selector = "blocks.*.content.*"
kind = "gaiji"
fields = { resolved = "", unresolved_reason = "unresolved" }
evidence = "adapters/aozora-rs/src/aat.rs"

[[case.upstream]]
adapter = "aozora2"
status = "normalised"
summary = "aozora-core 0.7.1 resolves this JIS-form marker to 撑."
selector = "blocks.*.content.*"
kind = "gaiji"
fields = { resolved = "撑", jis_code = "2-13-47" }
evidence = "adapters/aozora2/src/lib.rs"

[[case]]
id = "gaiji.unicode.u546d"
syntax_row_ids = ["gaiji.unicode_codepoint"]
category = "gaiji"
source_utf8 = "※［＃「口＋世」、U+546D］は珍しい字。"

[case.oracle]
visible_text = "呭は珍しい字。"

[[case.oracle.gaiji]]
selector = "blocks.*.content.*"
description = "「口＋世」、U+546D"
resolved = "呭"
jis_code = ""
unresolved_reason = ""
source = "Unicode code point"

[[case]]
id = "ruby.gaiji.inline_base"
syntax_row_ids = ["gaiji_ruby.inline_base", "ruby.basic"]
category = "ruby"
source_utf8 = "※［＃「口＋愛」、第3水準1-15-23］《おくび》が出た。"

[case.oracle]
visible_text = "が出た。"

[[case.oracle.nodes]]
selector = "**"
kind = "ruby"
min_count = 1

[[case.oracle.nodes]]
selector = "**"
kind = "gaiji"
min_count = 1

[[case.oracle.nodes]]
selector = "**"
kind = "raw"
absent = true

[[case]]
id = "accent.basic"
syntax_row_ids = ["accent.diacritic"]
category = "accent"
source_utf8 = "［＃「e`」はアクセント分解された欧文］"

[case.oracle]
visible_text = ""

[[case.oracle.nodes]]
selector = "**"
kind = "accent"
min_count = 1

[[case]]
id = "figure.basic"
syntax_row_ids = ["figure.image_inline"]
category = "figure"
source_utf8 = "［＃挿絵（fig01.png、横４００×縦３００）入る］"

[case.oracle]
visible_text = ""

[[case.oracle.nodes]]
selector = "**"
kind = "figure"
min_count = 1
```

- [ ] **Step 4: Verify**

Run:

```bash
chmod +x tests/aat-oracle-cases-schema-smoke.sh
bash tests/aat-oracle-cases-schema-smoke.sh
```

Expected: PASS and print `validated 5 oracle cases`.

- [ ] **Step 5: Commit**

Run:

```bash
git add data/aat-oracle-cases.schema.json data/aat-oracle-cases.toml tests/aat-oracle-cases-schema-smoke.sh
git commit -m "test: add path-scoped aat oracle cases"
```

Expected: commit succeeds.

---

### Task 4: Add Shared Fidelity Test Environment and Preflight

**Files:**
- Create: `tests/lib/aat-fidelity-env.sh`
- Create: `tests/adapter-fidelity-preflight.sh`

- [ ] **Step 1: Create shared environment helper**

Create `tests/lib/aat-fidelity-env.sh`:

```bash
#!/usr/bin/env bash
set -euo pipefail

export AB_VALIDATOR_ROOT="${AB_VALIDATOR_ROOT:-$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)}"
export AB_DB_ROOT="${AB_DB_ROOT:-/db/ab-validator}"
export TMPDIR="${TMPDIR:-$AB_DB_ROOT/tmp}"
export TMP="${TMP:-$TMPDIR}"
export TEMP="${TEMP:-$TMPDIR}"
mkdir -p "$TMPDIR" "$AB_DB_ROOT/aat-fidelity"

target_for() {
  local name="$1"
  printf '%s\n' "$AB_DB_ROOT/target-$name"
}

run_cargo() {
  nix develop "$AB_VALIDATOR_ROOT#" --command cargo "$@"
}

adapter_bin_path() {
  local manifest="$1"
  local bin="$2"
  local target="$3"
  run_cargo build --manifest-path "$manifest" --target-dir "$target" --bin "$bin"
  printf '%s/%s\n' "$target/debug" "$bin"
}
```

- [ ] **Step 2: Add preflight script**

Create `tests/adapter-fidelity-preflight.sh`:

```bash
#!/usr/bin/env bash
set -euo pipefail

source "$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/lib/aat-fidelity-env.sh"

command -v nix >/dev/null
command -v bash >/dev/null

aozora2_target="$(target_for aozora2-preflight)"
aozora2_bin="$(adapter_bin_path "$AB_VALIDATOR_ROOT/adapters/aozora2/Cargo.toml" aozora2-adapter "$aozora2_target")"

"$aozora2_bin" --version | rg -n '^aozora2-adapter '
printf '吾輩《わがはい》は猫である。' | "$aozora2_bin" --mode aat | rg -n '"adapter":"aozora2"'

echo "adapter fidelity preflight ok"
```

- [ ] **Step 3: Verify**

Run:

```bash
chmod +x tests/lib/aat-fidelity-env.sh tests/adapter-fidelity-preflight.sh
bash tests/adapter-fidelity-preflight.sh
```

Expected: PASS and print `adapter fidelity preflight ok`.

- [ ] **Step 4: Commit**

Run:

```bash
git add tests/lib/aat-fidelity-env.sh tests/adapter-fidelity-preflight.sh
git commit -m "test: add adapter fidelity preflight"
```

Expected: commit succeeds.

---

### Task 5: Create `ab-oracle` Loader and Rich Assertion Evaluator

**Files:**
- Modify: `Cargo.toml`
- Create: `crates/ab-oracle/Cargo.toml`
- Create: `crates/ab-oracle/src/lib.rs`
- Create: `crates/ab-oracle/src/select.rs`
- Create: `crates/ab-oracle/src/evaluate.rs`
- Create: `crates/ab-oracle/src/main.rs`

- [ ] **Step 1: Add failing loader and selector tests**

Create `crates/ab-oracle/src/lib.rs`:

```rust
pub mod evaluate;
pub mod select;

use anyhow::Result;
use serde::Deserialize;
use std::{fs, path::Path};

#[derive(Debug, Deserialize)]
pub struct OracleFile {
    pub aat_version: u64,
    pub case: Vec<OracleCase>,
}

#[derive(Debug, Deserialize)]
pub struct OracleCase {
    pub id: String,
    pub syntax_row_ids: Vec<String>,
    pub category: String,
    pub source_utf8: String,
    #[serde(default)]
    pub notes: String,
    pub oracle: OracleExpectations,
    #[serde(default)]
    pub upstream: Vec<UpstreamObservation>,
}

#[derive(Debug, Deserialize, Default)]
pub struct OracleExpectations {
    #[serde(default)]
    pub visible_text: Option<String>,
    #[serde(default)]
    pub nodes: Vec<NodeAssertion>,
    #[serde(default)]
    pub sequence: Vec<SequenceAssertion>,
    #[serde(default)]
    pub gaiji: Vec<GaijiAssertion>,
}

#[derive(Debug, Deserialize)]
pub struct NodeAssertion {
    pub selector: String,
    pub kind: String,
    #[serde(default)]
    pub count: Option<usize>,
    #[serde(default)]
    pub min_count: Option<usize>,
    #[serde(default)]
    pub absent: bool,
    #[serde(default)]
    pub fields: std::collections::BTreeMap<String, toml::Value>,
    #[serde(default)]
    pub field_absent: Vec<String>,
}

#[derive(Debug, Deserialize)]
pub struct SequenceAssertion {
    pub selector: String,
    pub kinds: Vec<String>,
}

#[derive(Debug, Deserialize)]
pub struct GaijiAssertion {
    pub selector: String,
    pub description: String,
    pub resolved: Option<String>,
    #[serde(default)]
    pub jis_code: Option<String>,
    #[serde(default)]
    pub unresolved_reason: Option<String>,
    #[serde(default)]
    pub source: String,
}

#[derive(Debug, Deserialize)]
pub struct UpstreamObservation {
    pub adapter: String,
    pub status: String,
    pub summary: String,
    #[serde(default)]
    pub selector: String,
    #[serde(default)]
    pub kind: String,
    #[serde(default)]
    pub fields: std::collections::BTreeMap<String, toml::Value>,
    #[serde(default)]
    pub evidence: String,
}

pub fn load_oracle_file(path: &Path) -> Result<OracleFile> {
    let raw = fs::read_to_string(path)?;
    Ok(toml::from_str(&raw)?)
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    #[test]
    fn loads_seed_oracle_cases_with_version_and_row_links() {
        let oracle = load_oracle_file(std::path::Path::new("../../data/aat-oracle-cases.toml")).unwrap();
        assert_eq!(oracle.aat_version, 1);
        let case = oracle.case.iter().find(|case| case.id == "gaiji.jis.2-13-47").unwrap();
        assert_eq!(case.syntax_row_ids, vec!["gaiji.jis_code"]);
        assert!(case.upstream.iter().any(|obs| obs.adapter == "aozora-rs"));
    }

    #[test]
    fn selector_counts_only_selected_content_children() {
        let aat = json!({
            "blocks": [{
                "kind": "paragraph",
                "content": [
                    {"kind": "text", "value": "foo"},
                    {"kind": "ruby", "base": "bar", "reading": "baz", "base_content": [{"kind": "text", "value": "bar"}]}
                ]
            }]
        });
        let selected = select::select(&aat, "blocks.*.content.*").unwrap();
        assert_eq!(selected.len(), 2);
        assert_eq!(selected.iter().filter(|node| node["kind"] == "text").count(), 1);
    }
}
```

Run:

```bash
CARGO_TARGET_DIR=/db/ab-validator/target-ab-oracle-loader \
  nix develop .# --command cargo test --manifest-path crates/ab-oracle/Cargo.toml -- --nocapture
```

Expected: FAIL because the crate does not exist yet.

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

Add `crates/ab-oracle` to the root workspace if the workspace has explicit members.

- [ ] **Step 3: Implement selector**

Create `crates/ab-oracle/src/select.rs`:

```rust
use anyhow::{bail, Result};

pub fn select<'a>(root: &'a serde_json::Value, selector: &str) -> Result<Vec<&'a serde_json::Value>> {
    if selector == "**" {
        let mut out = Vec::new();
        collect_recursive(root, &mut out);
        return Ok(out);
    }

    let mut current = vec![root];
    for segment in selector.split('.') {
        let mut next = Vec::new();
        for value in current {
            match segment {
                "*" => {
                    if let Some(array) = value.as_array() {
                        next.extend(array);
                    } else {
                        bail!("selector segment '*' expected array in {selector:?}");
                    }
                }
                key => {
                    if let Some(child) = value.get(key) {
                        next.push(child);
                    }
                }
            }
        }
        current = next;
    }
    Ok(current)
}

fn collect_recursive<'a>(value: &'a serde_json::Value, out: &mut Vec<&'a serde_json::Value>) {
    if value.is_object() {
        out.push(value);
    }
    match value {
        serde_json::Value::Object(map) => {
            for child in map.values() {
                collect_recursive(child, out);
            }
        }
        serde_json::Value::Array(values) => {
            for child in values {
                collect_recursive(child, out);
            }
        }
        _ => {}
    }
}
```

- [ ] **Step 4: Implement evaluator with separate statuses**

Create `crates/ab-oracle/src/evaluate.rs`:

```rust
use crate::{select, GaijiAssertion, NodeAssertion, OracleCase, SequenceAssertion};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CaseEvaluation {
    pub case_id: String,
    pub schema_status: String,
    pub upstream_status: String,
    pub oracle_status: String,
    pub failures: Vec<String>,
}

pub fn evaluate_case(adapter: &str, case: &OracleCase, aat: &serde_json::Value) -> CaseEvaluation {
    let mut failures = Vec::new();

    evaluate_visible_text(case, aat, &mut failures);
    evaluate_node_assertions(case, aat, &mut failures);
    evaluate_sequence_assertions(case, aat, &mut failures);
    evaluate_gaiji_assertions(case, aat, &mut failures);

    let upstream_status = classify_upstream(adapter, case, aat);

    CaseEvaluation {
        case_id: case.id.clone(),
        schema_status: "not_checked".to_owned(),
        upstream_status,
        oracle_status: if failures.is_empty() { "pass" } else { "fail" }.to_owned(),
        failures,
    }
}

fn classify_upstream(adapter: &str, case: &OracleCase, aat: &serde_json::Value) -> String {
    let Some(observation) = case.upstream.iter().find(|obs| obs.adapter == adapter) else {
        return "no_observation".to_owned();
    };
    if observation.selector.is_empty() || observation.kind.is_empty() {
        return "not_applicable".to_owned();
    }
    let Ok(nodes) = select::select(aat, &observation.selector) else {
        return "wrapper_mismatch".to_owned();
    };
    let matched = nodes.iter().any(|node| {
        node.get("kind").and_then(serde_json::Value::as_str) == Some(observation.kind.as_str())
            && observation.fields.iter().all(|(key, expected)| field_matches(node, key, expected))
    });
    if matched { "faithful" } else { "wrapper_mismatch" }.to_owned()
}

fn evaluate_visible_text(case: &OracleCase, aat: &serde_json::Value, failures: &mut Vec<String>) {
    let Some(expected) = &case.oracle.visible_text else {
        return;
    };
    let actual = visible_text(aat);
    if &actual != expected {
        failures.push(format!(
            "visible_text mismatch: expected {expected:?}, got {actual:?}"
        ));
    }
}

fn evaluate_node_assertions(case: &OracleCase, aat: &serde_json::Value, failures: &mut Vec<String>) {
    for assertion in &case.oracle.nodes {
        evaluate_node_assertion(assertion, aat, failures);
    }
}

fn evaluate_node_assertion(assertion: &NodeAssertion, aat: &serde_json::Value, failures: &mut Vec<String>) {
    let nodes = match select::select(aat, &assertion.selector) {
        Ok(nodes) => nodes,
        Err(error) => {
            failures.push(format!("selector {:?} failed: {error}", assertion.selector));
            return;
        }
    };
    let matches: Vec<_> = nodes
        .into_iter()
        .filter(|node| node.get("kind").and_then(serde_json::Value::as_str) == Some(assertion.kind.as_str()))
        .filter(|node| assertion.fields.iter().all(|(key, expected)| field_matches(node, key, expected)))
        .filter(|node| assertion.field_absent.iter().all(|key| node.get(key).is_none()))
        .collect();

    if assertion.absent {
        if !matches.is_empty() {
            failures.push(format!(
                "expected no {:?} nodes at selector {:?}, found {}",
                assertion.kind,
                assertion.selector,
                matches.len()
            ));
        }
        return;
    }
    if let Some(count) = assertion.count {
        if matches.len() != count {
            failures.push(format!(
                "kind {:?} count mismatch at selector {:?}: expected {count}, got {}",
                assertion.kind,
                assertion.selector,
                matches.len()
            ));
        }
    }
    if let Some(min_count) = assertion.min_count {
        if matches.len() < min_count {
            failures.push(format!(
                "kind {:?} count below minimum at selector {:?}: expected at least {min_count}, got {}",
                assertion.kind,
                assertion.selector,
                matches.len()
            ));
        }
    }
}

fn evaluate_sequence_assertions(case: &OracleCase, aat: &serde_json::Value, failures: &mut Vec<String>) {
    for assertion in &case.oracle.sequence {
        evaluate_sequence_assertion(assertion, aat, failures);
    }
}

fn evaluate_sequence_assertion(assertion: &SequenceAssertion, aat: &serde_json::Value, failures: &mut Vec<String>) {
    let arrays = match select::select(aat, &assertion.selector) {
        Ok(arrays) => arrays,
        Err(error) => {
            failures.push(format!("sequence selector {:?} failed: {error}", assertion.selector));
            return;
        }
    };
    let matched = arrays.iter().any(|value| {
        let Some(values) = value.as_array() else {
            return false;
        };
        let kinds: Vec<_> = values
            .iter()
            .filter_map(|node| node.get("kind").and_then(serde_json::Value::as_str))
            .collect();
        kinds
            == assertion
                .kinds
                .iter()
                .map(String::as_str)
                .collect::<Vec<_>>()
    });
    if !matched {
        failures.push(format!(
            "sequence {:?} not found at selector {:?}",
            assertion.kinds, assertion.selector
        ));
    }
}

fn evaluate_gaiji_assertions(case: &OracleCase, aat: &serde_json::Value, failures: &mut Vec<String>) {
    for assertion in &case.oracle.gaiji {
        evaluate_gaiji_assertion(assertion, aat, failures);
    }
}

fn evaluate_gaiji_assertion(assertion: &GaijiAssertion, aat: &serde_json::Value, failures: &mut Vec<String>) {
    let nodes = match select::select(aat, &assertion.selector) {
        Ok(nodes) => nodes,
        Err(error) => {
            failures.push(format!("gaiji selector {:?} failed: {error}", assertion.selector));
            return;
        }
    };
    let Some(node) = nodes.into_iter().find(|node| {
        node.get("kind").and_then(serde_json::Value::as_str) == Some("gaiji")
            && node.get("description").and_then(serde_json::Value::as_str) == Some(assertion.description.as_str())
    }) else {
        failures.push(format!("expected gaiji {:?} was not present", assertion.description));
        return;
    };

    assert_json_string_or_null(node, "resolved", assertion.resolved.as_deref(), failures);
    if let Some(expected) = &assertion.jis_code {
        assert_json_string_or_null(node, "jis_code", Some(expected.as_str()), failures);
    }
    if let Some(expected) = &assertion.unresolved_reason {
        assert_json_string_or_null(node, "unresolved_reason", Some(expected.as_str()), failures);
    }
}

fn visible_text(value: &serde_json::Value) -> String {
    let mut out = String::new();
    append_visible_text(value, &mut out);
    out
}

fn append_visible_text(value: &serde_json::Value, out: &mut String) {
    match value {
        serde_json::Value::Array(values) => {
            for value in values {
                append_visible_text(value, out);
            }
        }
        serde_json::Value::Object(object) => match object.get("kind").and_then(serde_json::Value::as_str) {
            Some("text") => push_string_field(object, "value", out),
            Some("ruby") => push_string_field(object, "base", out),
            Some("gaiji") | Some("accent") => push_string_field(object, "resolved", out),
            Some("figure") | Some("raw") => {}
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
        _ => {}
    }
}

fn push_string_field(object: &serde_json::Map<String, serde_json::Value>, key: &str, out: &mut String) {
    if let Some(value) = object.get(key).and_then(serde_json::Value::as_str) {
        out.push_str(value);
    }
}

fn field_matches(node: &serde_json::Value, key: &str, expected: &toml::Value) -> bool {
    let Some(actual) = node.get(key) else {
        return false;
    };
    let expected_json = serde_json::to_value(expected).unwrap_or(serde_json::Value::Null);
    normalize_empty_null(actual) == normalize_empty_null(&expected_json)
}

fn normalize_empty_null(value: &serde_json::Value) -> serde_json::Value {
    match value {
        serde_json::Value::String(s) if s.is_empty() => serde_json::Value::Null,
        other => other.clone(),
    }
}

fn assert_json_string_or_null(
    node: &serde_json::Value,
    field: &str,
    expected: Option<&str>,
    failures: &mut Vec<String>,
) {
    let actual = node.get(field).unwrap_or(&serde_json::Value::Null);
    let expected_value = expected
        .filter(|value| !value.is_empty())
        .map(|value| serde_json::Value::String(value.to_owned()))
        .unwrap_or(serde_json::Value::Null);
    if normalize_empty_null(actual) != expected_value {
        failures.push(format!(
            "field {field:?} mismatch: expected {expected_value:?}, got {actual:?}"
        ));
    }
}
```

- [ ] **Step 5: Add CLI**

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
    println!("loaded {} oracle cases for AAT v{}", oracle.case.len(), oracle.aat_version);
    Ok(())
}
```

- [ ] **Step 6: Verify**

Run:

```bash
CARGO_TARGET_DIR=/db/ab-validator/target-ab-oracle-loader \
  nix develop .# --command cargo test --manifest-path crates/ab-oracle/Cargo.toml -- --nocapture

CARGO_TARGET_DIR=/db/ab-validator/target-ab-oracle-loader \
  nix develop .# --command cargo run --manifest-path crates/ab-oracle/Cargo.toml -- --oracle data/aat-oracle-cases.toml
```

Expected: tests pass and CLI prints `loaded 5 oracle cases for AAT v1`.

- [ ] **Step 7: Commit**

Run:

```bash
git add Cargo.toml crates/ab-oracle
git commit -m "feat: add path-scoped aat oracle evaluator"
```

Expected: commit succeeds.

---

### Task 6: Add Adapter Runner and Reports

**Files:**
- Create: `crates/ab-oracle/src/adapter_run.rs`
- Modify: `crates/ab-oracle/src/main.rs`
- Create: `tests/adapter-fidelity-smoke.sh`
- Create: `tests/adapter-oracle-report-smoke.sh`

- [ ] **Step 1: Add adapter command model**

Create `crates/ab-oracle/src/adapter_run.rs`:

```rust
use std::{io::Write, path::PathBuf, process::{Command, Stdio}};

use anyhow::{Context, Result};

#[derive(Debug, Clone)]
pub struct AdapterSpec {
    pub id: String,
    pub command: PathBuf,
    pub args: Vec<String>,
}

impl AdapterSpec {
    pub fn parse(raw: &str) -> Result<Self> {
        let (id, rest) = raw
            .split_once('=')
            .with_context(|| format!("adapter spec must be id=command, got {raw:?}"))?;
        let mut parts = rest.split_whitespace();
        let command = parts.next().context("adapter command missing")?;
        Ok(Self {
            id: id.to_owned(),
            command: PathBuf::from(command),
            args: parts.map(str::to_owned).collect(),
        })
    }
}

pub fn run_adapter(spec: &AdapterSpec, source: &str) -> Result<serde_json::Value> {
    let mut command = Command::new(&spec.command);
    command.args(&spec.args);
    if !spec.args.iter().any(|arg| arg == "--mode") {
        command.arg("--mode").arg("aat");
    }
    let mut child = command
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .spawn()
        .with_context(|| format!("failed to spawn adapter {}", spec.id))?;
    child.stdin.as_mut().context("adapter stdin missing")?.write_all(source.as_bytes())?;
    let output = child.wait_with_output()?;
    if !output.status.success() {
        anyhow::bail!("adapter {} failed: {}", spec.id, String::from_utf8_lossy(&output.stderr));
    }
    Ok(serde_json::from_slice(&output.stdout)?)
}
```

- [ ] **Step 2: Extend CLI with separate output modes**

Add CLI flags:

```rust
#[arg(long = "adapter")]
adapters: Vec<String>,

#[arg(long)]
case_id: Option<String>,

#[arg(long)]
aat: Option<PathBuf>,

#[arg(long)]
report_json: Option<PathBuf>,

#[arg(long)]
report_md_from_json: Option<PathBuf>,
```

Rules:

- `--aat --case-id` evaluates one existing AAT file against one case.
- `--adapter id=command` runs selected oracle cases through adapters and writes JSON when `--report-json` is present.
- `--report-md-from-json path --report-json input.json` renders Markdown from an existing JSON report. Markdown generation must not re-run adapters.

- [ ] **Step 3: Add smoke script for one adapter/case**

Create `tests/adapter-fidelity-smoke.sh`:

```bash
#!/usr/bin/env bash
set -euo pipefail

source "$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/lib/aat-fidelity-env.sh"

out_dir="$AB_DB_ROOT/aat-fidelity/smoke"
mkdir -p "$out_dir"

aozora2_target="$(target_for aozora2-smoke)"
aozora2_bin="$(adapter_bin_path "$AB_VALIDATOR_ROOT/adapters/aozora2/Cargo.toml" aozora2-adapter "$aozora2_target")"

run_cargo run --quiet --manifest-path "$AB_VALIDATOR_ROOT/crates/ab-oracle/Cargo.toml" --target-dir "$(target_for ab-oracle-smoke)" -- \
  --oracle "$AB_VALIDATOR_ROOT/data/aat-oracle-cases.toml" \
  --case-id gaiji.jis.2-13-47 \
  --adapter "aozora2=$aozora2_bin" \
  --report-json "$out_dir/report.json"

rg -n '"adapter":"aozora2"|"case_id":"gaiji.jis.2-13-47"' "$out_dir/report.json"
```

- [ ] **Step 4: Add report rendering smoke script**

Create `tests/adapter-oracle-report-smoke.sh`:

```bash
#!/usr/bin/env bash
set -euo pipefail

source "$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/lib/aat-fidelity-env.sh"

out_dir="$AB_DB_ROOT/aat-fidelity/report-smoke"
mkdir -p "$out_dir"

bash "$AB_VALIDATOR_ROOT/tests/adapter-fidelity-smoke.sh"
cp "$AB_DB_ROOT/aat-fidelity/smoke/report.json" "$out_dir/report.json"

run_cargo run --quiet --manifest-path "$AB_VALIDATOR_ROOT/crates/ab-oracle/Cargo.toml" --target-dir "$(target_for ab-oracle-report)" -- \
  --report-json "$out_dir/report.json" \
  --report-md-from-json "$out_dir/report.md"

test -s "$out_dir/report.md"
rg -n "Adapter|gaiji.jis.2-13-47|oracle_status" "$out_dir/report.md"
```

- [ ] **Step 5: Verify**

Run:

```bash
chmod +x tests/adapter-fidelity-smoke.sh tests/adapter-oracle-report-smoke.sh
bash tests/adapter-fidelity-preflight.sh
bash tests/adapter-fidelity-smoke.sh
bash tests/adapter-oracle-report-smoke.sh
```

Expected: all pass. Intermediate output is under `/db/ab-validator/aat-fidelity`.

- [ ] **Step 6: Commit**

Run:

```bash
git add crates/ab-oracle tests/adapter-fidelity-smoke.sh tests/adapter-oracle-report-smoke.sh
git commit -m "feat: report adapter oracle and upstream fidelity"
```

Expected: commit succeeds.

---

### Task 7: Validate Oracle Cases Against Syntax Coverage Rows

**Files:**
- Modify: `crates/ab-coverage/src/matrix.rs`
- Modify: `crates/ab-coverage/tests/schema_matrix.rs`
- Modify: `data/aozora-syntax-coverage.schema.json`
- Modify: `data/aozora-syntax-coverage.toml`

- [ ] **Step 1: Add failing matrix-row linkage test**

Add to `crates/ab-coverage/tests/schema_matrix.rs`:

```rust
#[test]
fn oracle_case_references_point_to_existing_syntax_rows() {
    let matrix = ab_coverage::matrix::CoverageMatrix::from_toml(
        std::path::Path::new("data/aozora-syntax-coverage.toml"),
    )
    .unwrap();
    let row_ids: std::collections::BTreeSet<_> = matrix.rows().iter().map(|row| row.id.as_str()).collect();
    let raw = std::fs::read_to_string("data/aat-oracle-cases.toml").unwrap();
    let parsed: toml::Value = toml::from_str(&raw).unwrap();
    for case in parsed["case"].as_array().unwrap() {
        for row_id in case["syntax_row_ids"].as_array().unwrap() {
            let row_id = row_id.as_str().unwrap();
            assert!(row_ids.contains(row_id), "oracle case references unknown row {row_id}");
        }
    }
}
```

Run:

```bash
CARGO_TARGET_DIR=/db/ab-validator/target-ab-coverage-oracle \
  nix develop .# --command cargo test --manifest-path crates/ab-coverage/Cargo.toml oracle_case_references_point_to_existing_syntax_rows -- --nocapture
```

Expected: FAIL if any seed case uses a row id that does not exist.

- [ ] **Step 2: Add optional reverse links to matrix rows**

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

- [ ] **Step 3: Add reverse links for seed rows**

Update the corresponding `data/aozora-syntax-coverage.toml` rows:

```toml
oracle_cases = ["gaiji.jis.2-13-47"]
```

Use `rg -n 'id = "gaiji.jis_code"|id = "gaiji.unicode_codepoint"|id = "gaiji_ruby.inline_base"|id = "accent.diacritic"|id = "figure.image_inline"' data/aozora-syntax-coverage.toml` to find row locations.

- [ ] **Step 4: Verify**

Run:

```bash
CARGO_TARGET_DIR=/db/ab-validator/target-ab-coverage-oracle \
  nix develop .# --command cargo test --manifest-path crates/ab-coverage/Cargo.toml -- --nocapture
```

Expected: PASS.

- [ ] **Step 5: Commit**

Run:

```bash
git add crates/ab-coverage/src/matrix.rs crates/ab-coverage/tests/schema_matrix.rs data/aozora-syntax-coverage.schema.json data/aozora-syntax-coverage.toml
git commit -m "feat: link oracle cases to syntax coverage rows"
```

Expected: commit succeeds.

---

### Task 8: Discover and Then Reconstruct `aozora2` Blocks

**Files:**
- Modify: `adapters/aozora2/src/lib.rs`
- Modify: `docs/adapter-fidelity.md`
- Modify: `data/adapter-fidelity-notes.toml`

- [ ] **Step 1: Add parser-output discovery tests before implementation**

Add tests to `adapters/aozora2/src/lib.rs`:

```rust
#[test]
fn aozora2_parser_emits_block_markers_for_jisage_source() {
    let source = "［＃ここから２字下げ］\n字下げされた段落。\n［＃ここで字下げ終わり］";
    let nodes = aozora_core::parse(&aozora_core::tokenize(source));
    assert!(nodes.iter().any(|node| matches!(node, Node::BlockStart { block_type: BlockType::Jisage, .. })), "{nodes:#?}");
    assert!(nodes.iter().any(|node| matches!(node, Node::BlockEnd { block_type: BlockType::Jisage, .. })), "{nodes:#?}");
}

#[test]
fn aozora2_parser_emits_nested_block_markers_when_source_is_nested() {
    let source = "［＃ここから罫囲み］\n［＃ここから２字下げ］\n本文。\n［＃ここで字下げ終わり］\n［＃ここで罫囲み終わり］";
    let nodes = aozora_core::parse(&aozora_core::tokenize(source));
    let starts = nodes.iter().filter(|node| matches!(node, Node::BlockStart { .. })).count();
    let ends = nodes.iter().filter(|node| matches!(node, Node::BlockEnd { .. })).count();
    assert!(starts >= 2 && ends >= 2, "{nodes:#?}");
}
```

Run:

```bash
CARGO_TARGET_DIR=/db/ab-validator/target-aozora2-blocks \
  nix develop .# --command cargo test --manifest-path adapters/aozora2/Cargo.toml aozora2_parser_emits -- --nocapture
```

Expected: PASS before block reconstruction. If this fails, stop and revise the block plan based on actual parser output.

- [ ] **Step 2: Add failing block-builder tests**

Only after Step 1 passes, add tests:

```rust
#[test]
fn block_builder_wraps_inline_content_outside_blocks_in_paragraph() {
    let nodes = vec![Node::Text("外側。".to_owned())];
    let blocks = aozora_nodes_to_aat_blocks(&nodes);
    assert_eq!(blocks[0]["kind"], "paragraph");
    assert_eq!(blocks[0]["content"][0]["value"], "外側。");
}

#[test]
fn block_builder_reconstructs_nested_blocks() {
    let nodes = vec![
        Node::BlockStart { block_type: BlockType::Keigakomi, params: BlockParams { is_block: true, ..BlockParams::default() } },
        Node::BlockStart { block_type: BlockType::Jisage, params: BlockParams { width: Some(2), is_block: true, ..BlockParams::default() } },
        Node::Text("本文。".to_owned()),
        Node::BlockEnd { block_type: BlockType::Jisage, params: BlockParams::default() },
        Node::BlockEnd { block_type: BlockType::Keigakomi, params: BlockParams::default() },
    ];
    let blocks = aozora_nodes_to_aat_blocks(&nodes);
    assert_eq!(blocks[0]["kind"], "keigakomi_block");
    assert_eq!(blocks[0]["children"][0]["kind"], "jisage_block");
    assert_eq!(blocks[0]["children"][0]["children"][0]["kind"], "paragraph");
}

#[test]
fn block_builder_splits_multiple_paragraphs_inside_block() {
    let nodes = vec![
        Node::BlockStart { block_type: BlockType::Jisage, params: BlockParams { width: Some(2), is_block: true, ..BlockParams::default() } },
        Node::Text("一段落。\n\n二段落。".to_owned()),
        Node::BlockEnd { block_type: BlockType::Jisage, params: BlockParams::default() },
    ];
    let blocks = aozora_nodes_to_aat_blocks(&nodes);
    assert_eq!(blocks[0]["children"].as_array().unwrap().len(), 2);
}
```

Run:

```bash
CARGO_TARGET_DIR=/db/ab-validator/target-aozora2-blocks \
  nix develop .# --command cargo test --manifest-path adapters/aozora2/Cargo.toml block_builder_ -- --nocapture
```

Expected: FAIL because `aozora_nodes_to_aat_blocks` does not exist.

- [ ] **Step 3: Implement block builder**

Implement a dedicated block builder. Requirements:

- maintain a stack of block frames;
- flush inline accumulation into paragraphs before opening and closing block frames;
- split text containing blank-line paragraph boundaries into multiple paragraphs;
- put inline content outside block frames into top-level paragraphs;
- emit `raw` for unmatched `BlockEnd`;
- emit `raw` for still-open frames at EOF unless a conservative complete reconstruction is possible.

- [ ] **Step 4: Verify and retire note if appropriate**

Run:

```bash
CARGO_TARGET_DIR=/db/ab-validator/target-aozora2-blocks \
  nix develop .# --command cargo test --manifest-path adapters/aozora2/Cargo.toml -- --nocapture

CARGO_TARGET_DIR=/db/ab-validator/target-ab-check-blocks \
  nix develop .# --command cargo test --manifest-path crates/ab-check/Cargo.toml -- --nocapture
```

If block reconstruction is robust, change `aozora2-single-paragraph-before-block-reconstruction` in `data/adapter-fidelity-notes.toml` to:

```toml
status = "retired"
retired_by = "feat: reconstruct aozora2 block containers"
```

- [ ] **Step 5: Commit**

Run:

```bash
git add adapters/aozora2/src/lib.rs docs/adapter-fidelity.md data/adapter-fidelity-notes.toml
git commit -m "feat: reconstruct aozora2 block containers"
```

Expected: commit succeeds.

---

### Task 9: Add Source Spans With Decoded-Text Coordinates

**Files:**
- Modify: `data/aat-schema.json`
- Modify: `docs/aat-contract.md`
- Modify: `adapters/aozora-rs/src/aat.rs`
- Modify: `adapters/aozora-rs/src/source.rs`

- [ ] **Step 1: Extend span schema with optional coordinate fields**

Modify `data/aat-schema.json` span definition to allow optional decoded-text coordinate detail:

```json
"coordinate_system": { "const": "decoded_utf8" },
"char_start": { "type": "integer", "minimum": 0 },
"char_end": { "type": "integer", "minimum": 0 },
"column_start": { "type": "integer", "minimum": 1 },
"column_end": { "type": "integer", "minimum": 1 },
"raw_byte_start": { "type": ["integer", "null"], "minimum": 0 },
"raw_byte_end": { "type": ["integer", "null"], "minimum": 0 }
```

Do not make these fields required for AAT v1.

- [ ] **Step 2: Add failing test with computed offsets**

Add to `adapters/aozora-rs/src/aat.rs` tests:

```rust
#[test]
fn source_derived_gaiji_span_uses_decoded_text_coordinates() {
    let marker = "※［＃「てへん＋掌」、第4水準2-13-47］";
    let source = format!("耳朶を{marker}えて");
    let expected_start = source.find(marker).expect("marker in fixture");
    let expected_end = expected_start + marker.len();

    let aat = build_aat_for_test(&source);
    let gaiji = find_first_kind(&aat, "gaiji").expect("gaiji node");

    assert_eq!(gaiji["span"]["coordinate_system"], "decoded_utf8");
    assert_eq!(gaiji["span"]["byte_start"], expected_start);
    assert_eq!(gaiji["span"]["byte_end"], expected_end);
}
```

Run:

```bash
nix develop .# --command cargo test --manifest-path adapters/aozora-rs/Cargo.toml source_derived_gaiji_span_uses_decoded_text_coordinates -- --nocapture
```

Expected: FAIL until spans are threaded through source-derived gaiji supplements.

- [ ] **Step 3: Implement source-derived spans**

Thread decoded UTF-8 byte offsets through source scanning and emit:

```json
{
  "coordinate_system": "decoded_utf8",
  "line_start": 1,
  "line_end": 1,
  "byte_start": 9,
  "byte_end": 58,
  "char_start": 3,
  "char_end": 40
}
```

The numeric example above is illustrative only. Tests must compute expected values from fixture strings.

- [ ] **Step 4: Verify**

Run:

```bash
nix develop .# --command cargo test --manifest-path adapters/aozora-rs/Cargo.toml -- --nocapture
CARGO_TARGET_DIR=/db/ab-validator/target-ab-check-spans nix develop .# --command cargo test --manifest-path crates/ab-check/Cargo.toml -- --nocapture
```

Expected: PASS.

- [ ] **Step 5: Commit**

Run:

```bash
git add data/aat-schema.json docs/aat-contract.md adapters/aozora-rs/src/aat.rs adapters/aozora-rs/src/source.rs
git commit -m "feat: add decoded-text spans to source-derived aat nodes"
```

Expected: commit succeeds.

---

### Task 10: Add Standalone AAT Fidelity Marimo Notebook

**Files:**
- Create: `reports/aat-fidelity/fidelity_explorer.py`
- Create: `reports/aat-fidelity/open-fidelity-explorer.sh`
- Create: `reports/aat-fidelity/README.md`
- Create: `tests/aat-fidelity-marimo-notebook-smoke.sh`

- [ ] **Step 1: Add smoke test for standalone notebook**

Create `tests/aat-fidelity-marimo-notebook-smoke.sh`:

```bash
#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

test -s "$repo_root/reports/aat-fidelity/fidelity_explorer.py"
rg -n "aat-fidelity|oracle_status|upstream_status|mo\\.json" "$repo_root/reports/aat-fidelity/fidelity_explorer.py"
python3 -m py_compile "$repo_root/reports/aat-fidelity/fidelity_explorer.py"
```

Run:

```bash
bash tests/aat-fidelity-marimo-notebook-smoke.sh
```

Expected: FAIL because notebook files do not exist.

- [ ] **Step 2: Create notebook**

Create `reports/aat-fidelity/fidelity_explorer.py` as a simple marimo notebook that:

- reads a user-specified report JSON path, defaulting to `/db/ab-validator/aat-fidelity/report-smoke/report.json`;
- shows result rows with adapter, case id, `schema_status`, `upstream_status`, `oracle_status`, and failures;
- uses controls only inside this fidelity view;
- uses `mo.json` for selected small AAT snippets when an `aat_path` exists;
- truncates large AAT previews unless the user explicitly selects full preview.

- [ ] **Step 3: Add launcher**

Create `reports/aat-fidelity/open-fidelity-explorer.sh`:

```bash
#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
host="${AB_MARIMO_HOST:-$(hostname).hyakutake-barbel.ts.net}"
port="${AB_MARIMO_PORT:-27189}"

cd "$repo_root"
uv run --isolated --no-project \
  --with 'marimo>=0.23.4' \
  --with 'duckdb>=1.3' \
  --with 'polars>=1.0' \
  marimo edit reports/aat-fidelity/fidelity_explorer.py \
  --host "$host" \
  --port "$port"
```

- [ ] **Step 4: Verify**

Run:

```bash
chmod +x reports/aat-fidelity/open-fidelity-explorer.sh tests/aat-fidelity-marimo-notebook-smoke.sh
bash tests/aat-fidelity-marimo-notebook-smoke.sh
```

Expected: PASS.

- [ ] **Step 5: Commit**

Run:

```bash
git add reports/aat-fidelity tests/aat-fidelity-marimo-notebook-smoke.sh
git commit -m "feat: add aat fidelity explorer notebook"
```

Expected: commit succeeds.

---

## Deferred Work

- Persisting fidelity results into the morphology warehouse schema.
- Integrating fidelity controls into `reports/morph-warehouse/warehouse_explorer.py`.
- Full corpus oracle runs.
- Original encoded-byte spans for Shift_JIS source files.
- Richer selector syntax beyond the small v1 selector language.

## Verification Bundle

Use per-crate target directories. Do not use one shared Cargo target for every crate.

```bash
bash tests/adapter-fidelity-preflight.sh
bash tests/adapter-fidelity-notes-schema-smoke.sh
bash tests/aat-oracle-cases-schema-smoke.sh
bash tests/adapter-fidelity-smoke.sh
bash tests/adapter-oracle-report-smoke.sh
bash tests/aat-fidelity-marimo-notebook-smoke.sh

CARGO_TARGET_DIR=/db/ab-validator/target-ab-oracle-all \
  nix develop .# --command cargo test --manifest-path crates/ab-oracle/Cargo.toml -- --nocapture

CARGO_TARGET_DIR=/db/ab-validator/target-ab-check-all \
  nix develop .# --command cargo test --manifest-path crates/ab-check/Cargo.toml -- --nocapture

CARGO_TARGET_DIR=/db/ab-validator/target-ab-coverage-all \
  nix develop .# --command cargo test --manifest-path crates/ab-coverage/Cargo.toml -- --nocapture

CARGO_TARGET_DIR=/db/ab-validator/target-aozora2-all \
  nix develop .# --command cargo test --manifest-path adapters/aozora2/Cargo.toml -- --nocapture

nix develop .# --command cargo test --manifest-path adapters/aozora-rs/Cargo.toml -- --nocapture

nix-shell -p 'python3.withPackages(ps: [ps.lxml ps.jsonschema ps.pytest])' \
  --run 'python3 -m pytest adapters/aozora2html/tests/ -v'
```

Expected: every command exits 0. Large generated outputs remain under `/db/ab-validator`.

## Self-Review

- Three-layer gap resolved: report rows separately expose schema, upstream, and oracle statuses.
- Oracle assertions hardened: selector-scoped nodes, sequences, negative assertions, field assertions, and field absence are covered.
- `count_kind` issue avoided: selectors determine scope; recursive matching requires explicit `**`.
- Block reconstruction de-risked: parser-output discovery tests precede implementation tests.
- Span ambiguity resolved: AAT v1 spans are decoded UTF-8 text coordinates; original encoded bytes are deferred.
- Environment standardized: scripts source one helper and use per-crate targets under `/db/ab-validator`.
- Marimo scope reduced: standalone fidelity notebook comes before warehouse integration.
- Schema/version links included: oracle cases declare `aat_version = 1` and map to syntax rows at creation time.
