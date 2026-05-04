# AAT Fidelity and Oracle Roadmap v3 Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Build AAT fidelity/oracle reporting by composing with existing `ab-ir`, `ab-check`, and syntax-coverage data instead of creating a parallel raw-JSON evaluator.

**Architecture:** AAT semantics live in `ab-ir` as a reusable typed AAT view and selector protocol. Schema validation remains in `ab-check`. Oracle expectations and upstream observations are separate TOML value files with different lifecycles. `ab-oracle` is a thin coordinator: load values, run adapters, validate AAT with `ab-check`, evaluate selectors through `ab-ir`, classify upstream faithfulness from observation data, and emit JSON/Markdown reports.

**Tech Stack:** Rust 2024 workspace crates, `ab-ir`, `ab-check`, `ab-plaintext`, JSON Schema 2020-12, TOML `1.1.2` resolving to crate release `1.1.2+spec-1.1.0`, Nix-managed commands, per-crate Cargo targets under `/db/ab-validator`.

---

## Design Corrections From v2 Review

- `ab-oracle` must not implement its own visible projection or generic JSON selector traversal. That reusable behavior belongs in `ab-ir` or `ab-check`.
- Selector syntax is a protocol. It must be documented in `docs/aat-contract.md` before implementation.
- Oracle cases and upstream observations are separate value files:
  - `data/aat-oracle-cases.toml`
  - `data/aat-upstream-observations.toml`
- `crates/ab-oracle` is explicitly added to the root workspace.
- `schema_status` is produced by `ab-check` and is only `pass` or `fail`.
- `aozora2` parser API checks are explicit before block discovery tests.
- Span coordinate semantics are audited before any schema or adapter changes.
- The marimo notebook is deferred until report JSON exists, and its smoke test must load a real fixture report.

## Result Axes

| Axis | Values | Owner | Meaning |
| --- | --- | --- | --- |
| `schema_status` | `pass`, `fail` | `ab-check` | AAT validates against `data/aat-schema.json`. |
| `upstream_status` | `faithful`, `wrapper_mismatch`, `no_observation`, `not_applicable` | `ab-oracle` using upstream observation values | Adapter output preserves known upstream parser behavior for the case. |
| `oracle_status` | `pass`, `fail` | `ab-oracle` using `ab-ir` AAT view | Adapter output matches independent Aozora expectations. |

Do not add `not_checked` or other implicit states. If a command cannot check a status, it should not emit a full report row.

## Files and Ownership

- `docs/aat-contract.md`: AAT node semantics, selector protocol, span coordinate protocol.
- `data/adapter-fidelity-notes.toml`: structured known adapter limitations and retired notes.
- `data/aat-oracle-cases.toml`: oracle expectations only.
- `data/aat-upstream-observations.toml`: upstream/parser behavior observations keyed by case id and adapter id.
- `crates/ab-ir/src/aat_view.rs`: typed read-only AAT view over schema-shaped JSON, selectors, visible projection delegation.
- `crates/ab-check`: schema validation API for AAT values.
- `crates/ab-oracle`: orchestration, reports, adapter runner.
- `/db/ab-validator/aat-fidelity/`: generated AAT outputs and reports.

---

### Task 0: Upgrade Workspace TOML Parser

**Files:**
- Modify: `Cargo.toml`
- Modify: `Cargo.lock`
- Test: crates that currently parse TOML files

- [ ] **Step 1: Confirm current crate version**

Run:

```bash
cargo search toml --limit 1
```

Expected: output includes `toml = "1.1.2+spec-1.1.0"` or a newer compatible release.

- [ ] **Step 2: Update workspace dependency**

Modify root `Cargo.toml`. Use the semver requirement without build metadata so Cargo does not warn; `Cargo.lock` records the exact crate release `1.1.2+spec-1.1.0`.

```toml
toml = "1.1.2"
```

Keep `toml_edit` unchanged unless a compile failure requires updating it too.

- [ ] **Step 3: Run focused TOML consumers**

Run:

```bash
CARGO_TARGET_DIR=/db/ab-validator/target-toml-upgrade-index \
  nix develop .# --command cargo test --manifest-path crates/ab-index/Cargo.toml -- --nocapture

CARGO_TARGET_DIR=/db/ab-validator/target-toml-upgrade-coverage \
  nix develop .# --command cargo test --manifest-path crates/ab-coverage/Cargo.toml -- --nocapture
```

Expected: both pass. If `toml` 1.1 changes APIs used by these crates, fix those compile errors in the same task.

- [ ] **Step 4: Commit**

Run:

```bash
git add Cargo.toml Cargo.lock
git commit -m "chore: upgrade workspace toml parser"
```

Expected: commit succeeds.

---

### Task 1: Contract and Selector Protocol

**Files:**
- Create: `docs/aat-contract.md`
- Modify: `docs/adapter-fidelity.md`

- [ ] **Step 1: Write the contract**

Create `docs/aat-contract.md`:

```markdown
# AAT Contract

The Aozora Adapter Tree (AAT) is the normalized JSON emitted by parser adapters. AAT records adapter output and provenance. It does not by itself assert linguistic correctness.

## Versioning

AAT documents use top-level `version`. Current AAT is `version = 1`.

Oracle cases use `aat_version = 1` to declare the AAT contract they target. Schema-compatible additions to AAT v1 may add optional fields. Required field changes or changed node semantics require AAT v2.

## Result Axes

| Axis | Question |
| --- | --- |
| Schema validity | Is this structurally valid AAT for the declared version? |
| Adapter faithfulness | Did the adapter preserve what its upstream parser emitted? |
| Oracle correctness | Does the output match independent Aozora/literary expectations? |

These axes are independent. A faithful adapter can fail oracle correctness when upstream is incomplete.

## Selector Protocol v1

Selectors are path-only. They have no predicates and no embedded comparisons. Predicate checks belong in oracle assertions.

Supported selectors:

| Selector | Meaning |
| --- | --- |
| `blocks` | The top-level blocks array. |
| `blocks.*` | Every top-level block object. |
| `blocks.*.content` | Direct `content` arrays on top-level blocks that have `content`; blocks without `content` are skipped. |
| `blocks.*.content.*` | Direct inline children inside top-level block `content` arrays. |
| `blocks.*.children` | Direct `children` arrays on top-level block containers that have `children`; blocks without `children` are skipped. |
| `blocks.*.children.*` | Direct child blocks inside top-level block containers. |
| `**` | Every object in the AAT tree. Recursive matching must be explicit. |

Path mismatches skip missing object fields. A `*` segment only expands arrays. If `*` is applied to a non-array, the selector is invalid and the evaluator reports an assertion failure.

## Empty String and Null

For gaiji `resolved`, `jis_code`, and `unresolved_reason`, oracle comparison treats empty string and JSON null as equivalent only for compatibility with existing adapter output. This equivalence is limited to those fields.

## Span Coordinate System

AAT v1 spans require `line_start`, `line_end`, `byte_start`, and `byte_end`. Before changing span semantics, implementations must audit current adapter span emission.

If `coordinate_system = "decoded_utf8"` is present, then:

- `byte_start` and `byte_end` are UTF-8 byte offsets into the decoded source string used by the adapter;
- `line_start` and `line_end` are one-based decoded-source line numbers;
- optional `char_start` and `char_end` are Unicode scalar offsets into the decoded source string;
- original encoded byte offsets require separate fields such as `raw_byte_start` and `raw_byte_end`.

Tests must compute expected offsets from fixture strings rather than using unexplained numeric literals.
```

- [ ] **Step 2: Link contract**

Append to `docs/adapter-fidelity.md`:

```markdown
For AAT node semantics, result axes, selector protocol, and span coordinate rules, see `docs/aat-contract.md`.
```

- [ ] **Step 3: Verify**

Run:

```bash
test -s docs/aat-contract.md
rg -n "Selector Protocol v1|Empty String and Null|Span Coordinate System" docs/aat-contract.md
```

Expected: exit 0.

- [ ] **Step 4: Commit**

Run:

```bash
git add docs/aat-contract.md docs/adapter-fidelity.md
git commit -m "docs: define aat contract protocols"
```

Expected: commit succeeds.

---

### Task 2: Structured Adapter Fidelity Notes

**Files:**
- Create: `data/adapter-fidelity-notes.schema.json`
- Create: `data/adapter-fidelity-notes.toml`
- Create: `tests/adapter-fidelity-notes-schema-smoke.sh`

- [ ] **Step 1: Add failing schema smoke test**

Create `tests/adapter-fidelity-notes-schema-smoke.sh`:

```bash
#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

nix develop "${repo_root}#aozora2html" --command python3 - "${repo_root}" <<'PY'
import json
from pathlib import Path
import sys
import tomli
import jsonschema

root = Path(sys.argv[1])
schema = json.loads((root / 'data' / 'adapter-fidelity-notes.schema.json').read_text())
notes = tomli.loads((root / 'data' / 'adapter-fidelity-notes.toml').read_text())
jsonschema.validate(notes, schema)
assert isinstance(notes.get('note'), list)
print(f\"validated {len(notes['note'])} adapter fidelity notes\")
PY
```

Run:

```bash
bash tests/adapter-fidelity-notes-schema-smoke.sh
```

Expected: FAIL because note files do not exist.

- [ ] **Step 2: Add schema**

Create `data/adapter-fidelity-notes.schema.json`:

```json
{
  "$schema": "https://json-schema.org/draft/2020-12/schema",
  "$id": "https://abc.local/schemas/adapter-fidelity-notes-v1.json",
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

- [ ] **Step 3: Add seed notes**

Create `data/adapter-fidelity-notes.toml`:

```toml
[[note]]
id = "aozora-rs-gaiji-jis-unresolved"
adapter = "aozora-rs"
syntax_row_ids = ["gaiji.jis_code"]
status = "active"
summary = "Adapter faithfully follows aozora-rs-gaiji v0.6.0 for JIS-form gaiji; some remain unresolved relative to the oracle."
evidence = "docs/adapter-fidelity.md"

[[note]]
id = "aozora2-blocks-raw-before-reconstruction"
adapter = "aozora2"
syntax_row_ids = ["indentation.jisage", "block.keigakomi", "layout.caption"]
status = "active"
summary = "Adapter preserves some block markers as raw nodes until block reconstruction is implemented and verified."
evidence = "adapters/aozora2/src/lib.rs"

[[note]]
id = "aozora2html-xhtml-projection"
adapter = "aozora2html"
syntax_row_ids = ["gaiji.marker", "warichu.basic"]
status = "active"
summary = "Adapter maps rendered XHTML, so source-only markup distinctions absent from XHTML cannot always be recovered."
evidence = "adapters/aozora2html/README.md"
```

- [ ] **Step 4: Verify and commit**

Run:

```bash
chmod +x tests/adapter-fidelity-notes-schema-smoke.sh
bash tests/adapter-fidelity-notes-schema-smoke.sh
git add data/adapter-fidelity-notes.schema.json data/adapter-fidelity-notes.toml tests/adapter-fidelity-notes-schema-smoke.sh
git commit -m "docs: structure adapter fidelity notes"
```

Expected: smoke test passes and commit succeeds.

---

### Task 3: Split Oracle Cases From Upstream Observations

**Files:**
- Create: `data/aat-oracle-cases.schema.json`
- Create: `data/aat-oracle-cases.toml`
- Create: `data/aat-upstream-observations.schema.json`
- Create: `data/aat-upstream-observations.toml`
- Create: `tests/aat-oracle-data-schema-smoke.sh`

- [ ] **Step 1: Add failing schema smoke test**

Create `tests/aat-oracle-data-schema-smoke.sh`:

```bash
#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

nix develop "${repo_root}#aozora2html" --command python3 - "${repo_root}" <<'PY'
import json
from pathlib import Path
import sys
import tomli
import jsonschema

root = Path(sys.argv[1])
for stem in ['aat-oracle-cases', 'aat-upstream-observations']:
    schema = json.loads((root / 'data' / f'{stem}.schema.json').read_text())
    data = tomli.loads((root / 'data' / f'{stem}.toml').read_text())
    jsonschema.validate(data, schema)
    print(f'validated {stem}')
PY
```

Run:

```bash
bash tests/aat-oracle-data-schema-smoke.sh
```

Expected: FAIL because the files do not exist.

- [ ] **Step 2: Add oracle schema**

Create `data/aat-oracle-cases.schema.json`:

```json
{
  "$schema": "https://json-schema.org/draft/2020-12/schema",
  "$id": "https://abc.local/schemas/aat-oracle-cases-v1.json",
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
        "oracle": { "$ref": "#/$defs/OracleExpectations" }
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
    }
  }
}
```

- [ ] **Step 3: Add upstream observation schema**

Create `data/aat-upstream-observations.schema.json`:

```json
{
  "$schema": "https://json-schema.org/draft/2020-12/schema",
  "$id": "https://abc.local/schemas/aat-upstream-observations-v1.json",
  "type": "object",
  "required": ["observation"],
  "additionalProperties": false,
  "properties": {
    "observation": {
      "type": "array",
      "items": { "$ref": "#/$defs/Observation" }
    }
  },
  "$defs": {
    "Observation": {
      "type": "object",
      "required": ["case_id", "adapter", "status", "summary"],
      "additionalProperties": false,
      "properties": {
        "case_id": { "type": "string", "minLength": 1 },
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

- [ ] **Step 4: Add seed oracle cases**

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
```

- [ ] **Step 5: Add seed upstream observations**

Create `data/aat-upstream-observations.toml`:

```toml
[[observation]]
case_id = "gaiji.jis.2-13-47"
adapter = "aozora-rs"
status = "unresolved"
summary = "aozora-rs-gaiji v0.6.0 does not resolve this JIS-form marker."
selector = "blocks.*.content.*"
kind = "gaiji"
fields = { resolved = "", unresolved_reason = "unresolved" }
evidence = "adapters/aozora-rs/src/aat.rs"

[[observation]]
case_id = "gaiji.jis.2-13-47"
adapter = "aozora2"
status = "normalised"
summary = "aozora-core 0.7.1 resolves this JIS-form marker to 撑."
selector = "blocks.*.content.*"
kind = "gaiji"
fields = { resolved = "撑", jis_code = "2-13-47" }
evidence = "adapters/aozora2/src/lib.rs"
```

- [ ] **Step 6: Verify and commit**

Run:

```bash
chmod +x tests/aat-oracle-data-schema-smoke.sh
bash tests/aat-oracle-data-schema-smoke.sh
git add data/aat-oracle-cases.schema.json data/aat-oracle-cases.toml data/aat-upstream-observations.schema.json data/aat-upstream-observations.toml tests/aat-oracle-data-schema-smoke.sh
git commit -m "test: add separated aat oracle and upstream data"
```

Expected: smoke test passes and commit succeeds.

---

### Task 4: Add `ab-ir` AAT View and Selector Module

**Files:**
- Modify: `crates/ab-ir/src/lib.rs`
- Create: `crates/ab-ir/src/aat_view.rs`

- [ ] **Step 1: Add failing tests for selector protocol and visible projection composition**

Create `crates/ab-ir/src/aat_view.rs`:

```rust
use serde_json::Value;

#[derive(Debug, Clone)]
pub struct AatDocument {
    root: Value,
}

impl AatDocument {
    pub fn from_value(root: Value) -> Self {
        Self { root }
    }

    pub fn visible_text(&self) -> String {
        crate::aat_view::visible_text_from_value(&self.root)
    }

    pub fn select(&self, selector: &str) -> Result<Vec<&Value>, SelectorError> {
        select(&self.root, selector)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SelectorError {
    pub selector: String,
    pub message: String,
}

pub fn visible_text_from_value(value: &Value) -> String {
    ab_plaintext::visible_text_projection(value)
}

pub fn select<'a>(_root: &'a Value, _selector: &str) -> Result<Vec<&'a Value>, SelectorError> {
    unimplemented!("implemented in Step 3")
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    #[test]
    fn selector_counts_direct_content_children_without_recursing_into_ruby_base_content() {
        let doc = AatDocument::from_value(json!({
            "version": 1,
            "work_id": "fixture",
            "blocks": [{
                "kind": "paragraph",
                "content": [
                    {"kind": "text", "value": "foo"},
                    {"kind": "ruby", "base": "bar", "reading": "baz", "base_content": [{"kind": "text", "value": "bar"}]}
                ]
            }],
            "meta": {"adapter":"fixture","adapter_version":"fixture","source_encoding":"utf-8","source_hash":"sha256:0000000000000000000000000000000000000000000000000000000000000000","parse_complete":true,"warnings":[]}
        }));

        let nodes = doc.select("blocks.*.content.*").unwrap();
        assert_eq!(nodes.len(), 2);
        assert_eq!(nodes.iter().filter(|node| node["kind"] == "text").count(), 1);
        assert_eq!(doc.select("**").unwrap().iter().filter(|node| node["kind"] == "text").count(), 2);
    }

    #[test]
    fn visible_text_delegates_to_plaintext_projection() {
        let value = json!({
            "version": 1,
            "work_id": "fixture",
            "blocks": [{"kind": "paragraph", "content": [{"kind": "gaiji", "description": "desc", "resolved": ""}]}],
            "meta": {"adapter":"fixture","adapter_version":"fixture","source_encoding":"utf-8","source_hash":"sha256:0000000000000000000000000000000000000000000000000000000000000000","parse_complete":true,"warnings":[]}
        });
        let doc = AatDocument::from_value(value.clone());
        assert_eq!(doc.visible_text(), ab_plaintext::visible_text_projection(&value));
    }
}
```

Add to `crates/ab-ir/src/lib.rs`:

```rust
pub mod aat_view;
```

Run:

```bash
CARGO_TARGET_DIR=/db/ab-validator/target-ab-ir-aat-view \
  nix develop .# --command cargo test --manifest-path crates/ab-ir/Cargo.toml aat_view -- --nocapture
```

Expected: FAIL because `ab-ir` does not yet depend on `ab-plaintext` and `select` is unimplemented.

- [ ] **Step 2: Add dependency**

Modify `crates/ab-ir/Cargo.toml` to include:

```toml
ab-plaintext = { workspace = true }
```

- [ ] **Step 3: Implement selector protocol**

Replace `select` with:

```rust
pub fn select<'a>(root: &'a Value, selector: &str) -> Result<Vec<&'a Value>, SelectorError> {
    if selector == "**" {
        let mut out = Vec::new();
        collect_recursive_objects(root, &mut out);
        return Ok(out);
    }

    let mut current = vec![root];
    for segment in selector.split('.') {
        let mut next = Vec::new();
        for value in current {
            match segment {
                "*" => {
                    let Some(array) = value.as_array() else {
                        return Err(SelectorError {
                            selector: selector.to_owned(),
                            message: "'*' segment applied to non-array".to_owned(),
                        });
                    };
                    next.extend(array);
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

fn collect_recursive_objects<'a>(value: &'a Value, out: &mut Vec<&'a Value>) {
    if value.is_object() {
        out.push(value);
    }
    match value {
        Value::Object(object) => {
            for child in object.values() {
                collect_recursive_objects(child, out);
            }
        }
        Value::Array(values) => {
            for child in values {
                collect_recursive_objects(child, out);
            }
        }
        _ => {}
    }
}
```

- [ ] **Step 4: Verify and commit**

Run:

```bash
CARGO_TARGET_DIR=/db/ab-validator/target-ab-ir-aat-view \
  nix develop .# --command cargo test --manifest-path crates/ab-ir/Cargo.toml -- --nocapture
git add crates/ab-ir/src/lib.rs crates/ab-ir/src/aat_view.rs crates/ab-ir/Cargo.toml
git commit -m "feat: add typed aat view selectors to ab-ir"
```

Expected: tests pass and commit succeeds.

---

### Task 5: Expose AAT Schema Validation From `ab-check`

**Files:**
- Modify: `crates/ab-check/src/check.rs`
- Modify: `crates/ab-check/src/lib.rs`

- [ ] **Step 1: Add failing validation API test**

Add to `crates/ab-check/src/check.rs` tests or create a test module:

```rust
#[test]
fn validate_aat_value_reports_schema_status() {
    let valid = serde_json::json!({
        "version": 1,
        "work_id": "fixture",
        "blocks": [{"kind": "paragraph", "content": [{"kind": "text", "value": "本文"}]}],
        "meta": {"adapter":"fixture","adapter_version":"fixture","source_encoding":"utf-8","source_hash":"sha256:0000000000000000000000000000000000000000000000000000000000000000","parse_complete":true,"warnings":[]}
    });
    assert!(validate_aat_value(&valid).is_ok());

    let invalid = serde_json::json!({"version": 1});
    assert!(validate_aat_value(&invalid).is_err());
}
```

Run:

```bash
CARGO_TARGET_DIR=/db/ab-validator/target-ab-check-schema-api \
  nix develop .# --command cargo test --manifest-path crates/ab-check/Cargo.toml validate_aat_value_reports_schema_status -- --nocapture
```

Expected: FAIL because `validate_aat_value` does not exist.

- [ ] **Step 2: Implement validation API**

Add to `crates/ab-check/src/check.rs`:

```rust
pub fn validate_aat_value(aat: &Value) -> Result<()> {
    let validator = schema_validator()?;
    validator
        .validate(aat)
        .map_err(|error| anyhow::anyhow!("AAT schema validation failed at {}: {error}", error.instance_path()))
}
```

Ensure `crates/ab-check/src/lib.rs` keeps `pub mod check;` so callers can use `ab_check::check::validate_aat_value`.

- [ ] **Step 3: Verify and commit**

Run:

```bash
CARGO_TARGET_DIR=/db/ab-validator/target-ab-check-schema-api \
  nix develop .# --command cargo test --manifest-path crates/ab-check/Cargo.toml -- --nocapture
git add crates/ab-check/src/check.rs crates/ab-check/src/lib.rs
git commit -m "feat: expose aat schema validation api"
```

Expected: tests pass and commit succeeds.

---

### Task 6: Add `ab-oracle` As Workspace Coordinator

**Files:**
- Modify: `Cargo.toml`
- Create: `crates/ab-oracle/Cargo.toml`
- Create: `crates/ab-oracle/src/lib.rs`
- Create: `crates/ab-oracle/src/main.rs`
- Create: `crates/ab-oracle/src/data.rs`
- Create: `crates/ab-oracle/src/evaluate.rs`

- [ ] **Step 1: Add workspace member explicitly**

Modify root `Cargo.toml`:

```toml
members = [
    "crates/ab-source-syntax",
    "crates/ab-ir",
    "crates/ab-index",
    "crates/ab-check",
    "crates/ab-compare",
    "crates/ab-coverage",
    "crates/ab-diff-utils",
    "crates/ab-morph-diff",
    "crates/ab-plaintext",
    "crates/ab-morph-analyzers",
    "crates/ab-morph-run",
    "crates/ab-oracle",
]
```

Add workspace dependency:

```toml
ab-check = { path = "crates/ab-check" }
```

- [ ] **Step 2: Add crate manifest**

Create `crates/ab-oracle/Cargo.toml`:

```toml
[package]
name = "ab-oracle"
version.workspace = true
edition.workspace = true
license.workspace = true

[dependencies]
anyhow = { workspace = true }
ab-check = { workspace = true }
ab-ir = { workspace = true }
clap = { workspace = true }
serde = { workspace = true }
serde_json = { workspace = true }
toml = { workspace = true }
```

- [ ] **Step 3: Add failing composition tests**

Create `crates/ab-oracle/src/lib.rs`:

```rust
pub mod data;
pub mod evaluate;
```

Create `crates/ab-oracle/src/evaluate.rs`:

```rust
use ab_ir::aat_view::AatDocument;
use serde_json::Value;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CaseEvaluation {
    pub case_id: String,
    pub schema_status: String,
    pub upstream_status: String,
    pub oracle_status: String,
    pub failures: Vec<String>,
}

pub fn schema_status(aat: &Value) -> String {
    if ab_check::check::validate_aat_value(aat).is_ok() {
        "pass".to_owned()
    } else {
        "fail".to_owned()
    }
}

pub fn visible_text(document: &AatDocument) -> String {
    document.visible_text()
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    #[test]
    fn schema_status_uses_ab_check() {
        assert_eq!(schema_status(&json!({"version": 1})), "fail");
    }

    #[test]
    fn visible_text_uses_ab_ir_aat_view() {
        let aat = json!({
            "version": 1,
            "work_id": "fixture",
            "blocks": [{"kind": "paragraph", "content": [{"kind": "text", "value": "本文"}]}],
            "meta": {"adapter":"fixture","adapter_version":"fixture","source_encoding":"utf-8","source_hash":"sha256:0000000000000000000000000000000000000000000000000000000000000000","parse_complete":true,"warnings":[]}
        });
        let doc = AatDocument::from_value(aat);
        assert_eq!(visible_text(&doc), "本文");
    }
}
```

Run:

```bash
CARGO_TARGET_DIR=/db/ab-validator/target-ab-oracle-compose \
  nix develop .# --command cargo test --manifest-path crates/ab-oracle/Cargo.toml -- --nocapture
```

Expected: FAIL until data module and binary are added.

- [ ] **Step 4: Add data loaders**

Create `crates/ab-oracle/src/data.rs` with typed loaders for `data/aat-oracle-cases.toml` and `data/aat-upstream-observations.toml`. Use `toml = { workspace = true }`, which should already be upgraded to semver requirement `1.1.2` by Task 0.

- [ ] **Step 5: Add CLI skeleton**

Create `crates/ab-oracle/src/main.rs`:

```rust
use std::path::PathBuf;

use anyhow::Result;
use clap::Parser;

#[derive(Debug, Parser)]
struct Args {
    #[arg(long, default_value = "data/aat-oracle-cases.toml")]
    oracle: PathBuf,

    #[arg(long, default_value = "data/aat-upstream-observations.toml")]
    upstream: PathBuf,
}

fn main() -> Result<()> {
    let args = Args::parse();
    let oracle = ab_oracle::data::load_oracle_cases(&args.oracle)?;
    let observations = ab_oracle::data::load_upstream_observations(&args.upstream)?;
    println!(
        "loaded {} oracle cases and {} upstream observations",
        oracle.case.len(),
        observations.observation.len()
    );
    Ok(())
}
```

- [ ] **Step 6: Verify and commit**

Run:

```bash
CARGO_TARGET_DIR=/db/ab-validator/target-ab-oracle-compose \
  nix develop .# --command cargo test --manifest-path crates/ab-oracle/Cargo.toml -- --nocapture
CARGO_TARGET_DIR=/db/ab-validator/target-ab-oracle-compose \
  nix develop .# --command cargo run --manifest-path crates/ab-oracle/Cargo.toml -- --oracle data/aat-oracle-cases.toml --upstream data/aat-upstream-observations.toml
git add Cargo.toml crates/ab-oracle
git commit -m "feat: add oracle coordinator crate"
```

Expected: tests pass, CLI prints loaded counts, commit succeeds.

---

### Task 7: Add Shared Fidelity Environment and Adapter Reports

**Files:**
- Create: `tests/lib/aat-fidelity-env.sh`
- Create: `tests/adapter-fidelity-preflight.sh`
- Modify: `crates/ab-oracle/src/main.rs`
- Create: `crates/ab-oracle/src/adapter_run.rs`
- Create: `crates/ab-oracle/src/report.rs`
- Create: `tests/adapter-fidelity-smoke.sh`
- Create: `tests/adapter-oracle-report-smoke.sh`

- [ ] **Step 1: Create environment helper**

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

- [ ] **Step 2: Create preflight**

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

- [ ] **Step 3: Add adapter runner and report output**

Implement adapter command specs in `crates/ab-oracle/src/adapter_run.rs`, report rows in `crates/ab-oracle/src/report.rs`, and CLI flags:

```rust
#[arg(long = "adapter")]
adapters: Vec<String>,
#[arg(long)]
case_id: Option<String>,
#[arg(long)]
report_json: Option<PathBuf>,
#[arg(long)]
report_md_from_json: Option<PathBuf>,
```

Rules:

- JSON report generation runs adapters and evaluates rows.
- Markdown generation reads JSON report files only and does not re-run adapters.
- Every row includes `schema_status`, `upstream_status`, and `oracle_status`.

- [ ] **Step 4: Add smoke scripts**

Create `tests/adapter-fidelity-smoke.sh` that builds `aozora2-adapter`, runs case `gaiji.jis.2-13-47`, and writes `/db/ab-validator/aat-fidelity/smoke/report.json`.

Create `tests/adapter-oracle-report-smoke.sh` that renders Markdown from that JSON and checks for `schema_status`, `upstream_status`, and `oracle_status`.

- [ ] **Step 5: Verify and commit**

Run:

```bash
chmod +x tests/lib/aat-fidelity-env.sh tests/adapter-fidelity-preflight.sh tests/adapter-fidelity-smoke.sh tests/adapter-oracle-report-smoke.sh
bash tests/adapter-fidelity-preflight.sh
bash tests/adapter-fidelity-smoke.sh
bash tests/adapter-oracle-report-smoke.sh
git add tests/lib/aat-fidelity-env.sh tests/adapter-fidelity-preflight.sh tests/adapter-fidelity-smoke.sh tests/adapter-oracle-report-smoke.sh crates/ab-oracle
git commit -m "feat: report adapter fidelity axes"
```

Expected: smoke tests pass and commit succeeds.

---

### Task 8: Link Oracle Cases to Syntax Coverage

**Files:**
- Modify: `crates/ab-coverage/src/matrix.rs`
- Modify: `crates/ab-coverage/tests/schema_matrix.rs`
- Modify: `data/aozora-syntax-coverage.schema.json`
- Modify: `data/aozora-syntax-coverage.toml`

- [ ] **Step 1: Add row-reference validation test**

Add a test in `crates/ab-coverage/tests/schema_matrix.rs` that loads `data/aat-oracle-cases.toml` with the workspace `toml` crate, extracts every `syntax_row_ids` value, and asserts each exists in `data/aozora-syntax-coverage.toml`.

- [ ] **Step 2: Add optional reverse links**

Add `oracle_cases: Vec<String>` to `crates/ab-coverage/src/matrix.rs::Row` with `#[serde(default)]`.

Add optional `oracle_cases` to `data/aozora-syntax-coverage.schema.json`.

- [ ] **Step 3: Backfill seed rows**

Use:

```bash
rg -n 'id = "gaiji.jis_code"|id = "gaiji.unicode_codepoint"|id = "gaiji_ruby.inline_base"|id = "ruby.basic"' data/aozora-syntax-coverage.toml
```

Add `oracle_cases = [...]` to those rows.

- [ ] **Step 4: Verify and commit**

Run:

```bash
CARGO_TARGET_DIR=/db/ab-validator/target-ab-coverage-oracle \
  nix develop .# --command cargo test --manifest-path crates/ab-coverage/Cargo.toml -- --nocapture
git add crates/ab-coverage/src/matrix.rs crates/ab-coverage/tests/schema_matrix.rs data/aozora-syntax-coverage.schema.json data/aozora-syntax-coverage.toml
git commit -m "feat: link oracle cases to syntax coverage"
```

Expected: tests pass and commit succeeds.

---

### Task 9: Audit Span Semantics Before Changing Span Schema

**Files:**
- Create: `docs/aat-span-audit.md`
- Modify: `docs/aat-contract.md`
- Modify: `data/aat-schema.json` only after audit conclusion

- [ ] **Step 1: Inspect existing span emission**

Run:

```bash
rg -n '"span"|line_start|byte_start|byte_end|SourceSpan|source_span' adapters/aozora-rs/src crates/ab-ir crates/ab-check data
```

Create `docs/aat-span-audit.md` with:

```markdown
# AAT Span Audit

## Existing Emitters

| File | Node type | Coordinate meaning | Evidence |
| --- | --- | --- | --- |

## Conclusion

State whether current `byte_start`/`byte_end` are decoded UTF-8 offsets, original file byte offsets, or mixed/unknown.

## Migration Decision

State whether to add `coordinate_system`, introduce raw-byte fields, or leave AAT v1 spans unchanged.
```

- [ ] **Step 2: Only then add schema changes**

If audit confirms current spans are decoded UTF-8 offsets, add optional `coordinate_system = "decoded_utf8"` and optional `char_start`/`char_end`.

If audit finds mixed semantics, do not add `coordinate_system` to existing `span`. Instead add a new optional `source_span_v2` object or defer schema changes to AAT v2.

- [ ] **Step 3: Verify and commit**

Run:

```bash
test -s docs/aat-span-audit.md
CARGO_TARGET_DIR=/db/ab-validator/target-ab-check-span-audit \
  nix develop .# --command cargo test --manifest-path crates/ab-check/Cargo.toml -- --nocapture
git add docs/aat-span-audit.md docs/aat-contract.md data/aat-schema.json
git commit -m "docs: audit aat span coordinate semantics"
```

Expected: tests pass and commit succeeds.

---

### Task 10: Discover and Then Reconstruct `aozora2` Blocks

**Files:**
- Modify: `adapters/aozora2/src/lib.rs`
- Modify: `data/adapter-fidelity-notes.toml`
- Modify: `docs/adapter-fidelity.md`

- [ ] **Step 0: Verify public `aozora_core` API**

Run:

```bash
rg -n 'pub fn parse|pub fn tokenize|pub enum Node|BlockStart|pub use node' \
  /home/bor/.local/state/agent-skills/profiles/codex/project-codex-skills-ab-validator-7faa4d46/.cargo/git/checkouts/aozora2-16854cf3f7c13d33/*/crates/aozora-core/src
```

Record the verified API shape in a short comment above the discovery tests. The expected current shape is:

```rust
let nodes: Vec<aozora_core::Node> = aozora_core::parse(&aozora_core::tokenize(source));
```

- [ ] **Step 1: Add parser-output discovery tests**

Add tests that assert real `aozora_core` output contains `BlockStart`/`BlockEnd` for representative jisage and nested block source. If these fail, stop and revise the reconstruction strategy based on actual parser output.

- [ ] **Step 2: Add failing block-builder tests**

Add tests for:

- inline content outside blocks wraps in a default paragraph;
- nested block frames reconstruct as nested block containers;
- multiple paragraphs inside one block are split correctly;
- unmatched end markers remain `raw`.

- [ ] **Step 3: Implement block builder**

Implement a dedicated block builder. Requirements:

- stack frames are values, not mutation hidden across helper calls;
- paragraph flush is explicit at block boundaries;
- unmatched block starts/ends produce `raw` nodes and preserve source evidence;
- existing `append_aozora_node` remains the inline projection function.

- [ ] **Step 4: Verify and commit**

Run:

```bash
CARGO_TARGET_DIR=/db/ab-validator/target-aozora2-blocks \
  nix develop .# --command cargo test --manifest-path adapters/aozora2/Cargo.toml -- --nocapture
CARGO_TARGET_DIR=/db/ab-validator/target-ab-check-blocks \
  nix develop .# --command cargo test --manifest-path crates/ab-check/Cargo.toml -- --nocapture
git add adapters/aozora2/src/lib.rs data/adapter-fidelity-notes.toml docs/adapter-fidelity.md
git commit -m "feat: reconstruct aozora2 block containers"
```

Expected: tests pass and commit succeeds.

---

### Task 11: Standalone Fidelity Report Notebook

**Files:**
- Create: `reports/aat-fidelity/fidelity_explorer.py`
- Create: `reports/aat-fidelity/open-fidelity-explorer.sh`
- Create: `reports/aat-fidelity/fixtures/report.json`
- Create: `tests/aat-fidelity-marimo-notebook-smoke.sh`

- [ ] **Step 1: Add fixture report**

Create `reports/aat-fidelity/fixtures/report.json` with at least one row:

```json
[
  {
    "adapter": "aozora2",
    "case_id": "gaiji.jis.2-13-47",
    "schema_status": "pass",
    "upstream_status": "faithful",
    "oracle_status": "pass",
    "failures": []
  }
]
```

- [ ] **Step 2: Add notebook smoke test**

Create `tests/aat-fidelity-marimo-notebook-smoke.sh`:

```bash
#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

test -s "$repo_root/reports/aat-fidelity/fidelity_explorer.py"
test -s "$repo_root/reports/aat-fidelity/fixtures/report.json"
rg -n "oracle_status|upstream_status|schema_status|mo\\.json" "$repo_root/reports/aat-fidelity/fidelity_explorer.py"
uv run --isolated --no-project --with 'marimo>=0.23.4' python -m py_compile "$repo_root/reports/aat-fidelity/fidelity_explorer.py"
```

- [ ] **Step 3: Create notebook**

Create a standalone marimo notebook that loads a report JSON path, defaults to `reports/aat-fidelity/fixtures/report.json`, and displays filterable rows. Do not integrate with the morphology warehouse yet.

- [ ] **Step 4: Verify and commit**

Run:

```bash
chmod +x tests/aat-fidelity-marimo-notebook-smoke.sh reports/aat-fidelity/open-fidelity-explorer.sh
bash tests/aat-fidelity-marimo-notebook-smoke.sh
git add reports/aat-fidelity tests/aat-fidelity-marimo-notebook-smoke.sh
git commit -m "feat: add standalone aat fidelity explorer"
```

Expected: smoke test passes and commit succeeds.

---

## Deferred Work

- Full typed conversion from every AAT JSON node into the existing `Block`/`Inline` IR. The first step is a read-only `AatDocument` view because current AAT has node kinds not represented in `Block`/`Inline`.
- Persisting fidelity results into the morphology warehouse.
- Integrating fidelity controls into `reports/morph-warehouse/warehouse_explorer.py`.
- Original encoded-byte spans for Shift_JIS source files.
- Selector predicates. Selector v1 remains path-only.

## Verification Bundle

Use per-crate target directories.

```bash
CARGO_TARGET_DIR=/db/ab-validator/target-toml-upgrade-index \
  nix develop .# --command cargo test --manifest-path crates/ab-index/Cargo.toml -- --nocapture

bash tests/adapter-fidelity-notes-schema-smoke.sh
bash tests/aat-oracle-data-schema-smoke.sh
bash tests/adapter-fidelity-preflight.sh
bash tests/adapter-fidelity-smoke.sh
bash tests/adapter-oracle-report-smoke.sh
bash tests/aat-fidelity-marimo-notebook-smoke.sh

CARGO_TARGET_DIR=/db/ab-validator/target-ab-ir-all \
  nix develop .# --command cargo test --manifest-path crates/ab-ir/Cargo.toml -- --nocapture

CARGO_TARGET_DIR=/db/ab-validator/target-ab-check-all \
  nix develop .# --command cargo test --manifest-path crates/ab-check/Cargo.toml -- --nocapture

CARGO_TARGET_DIR=/db/ab-validator/target-ab-oracle-all \
  nix develop .# --command cargo test --manifest-path crates/ab-oracle/Cargo.toml -- --nocapture

CARGO_TARGET_DIR=/db/ab-validator/target-ab-coverage-all \
  nix develop .# --command cargo test --manifest-path crates/ab-coverage/Cargo.toml -- --nocapture

CARGO_TARGET_DIR=/db/ab-validator/target-aozora2-all \
  nix develop .# --command cargo test --manifest-path adapters/aozora2/Cargo.toml -- --nocapture

nix develop .# --command cargo test --manifest-path adapters/aozora-rs/Cargo.toml -- --nocapture

nix develop "${repo_root}#aozora2html" --command python3 -m pytest adapters/aozora2html/tests/ -v
```

Expected: every command exits 0.

## Self-Review

- Composability: `ab-oracle` composes with `ab-ir` for AAT view/selector/visible projection and `ab-check` for schema validation.
- Protocols: selector and span coordinate protocols are documented before implementation.
- Values/lifecycles: oracle expectations and upstream observations are separate TOML files.
- Time: adapter notes have `active`/`retired` succession.
- Constraints: selector v1 remains path-only; predicates stay in assertions.
- Codebase reality: root workspace membership is explicit, `toml` is upgraded through a dedicated task, and `aozora_core` API discovery is required before block reconstruction.
