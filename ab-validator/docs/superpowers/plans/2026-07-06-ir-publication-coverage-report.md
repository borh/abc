# IR Publication Coverage Report Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Build an ab-validator report that proves whether every observed Parser-IR/source construct is mapped to TEI P5, TEI policy projection, ABC custom preservation, plaintext-only output, or a named unsupported gap.

**Architecture:** Add one focused Python report script that consumes the existing generated-TEI matrix, source-authority summary, plain-prose source-delta summary, Parser-IR schema, and AAT-to-parser-IR mapping artifact. The script emits a JSON/Markdown coverage report whose headline verdict is about full IR publication coverage, not TEI-EAJ Level 3 parity. A Bash smoke test and `just` recipe make the report repeatable.

**Tech Stack:** Python 3 standard library, Bash smoke tests with `jq`, existing JSON reports under `docs/superpowers/reports/`, existing `justfile` report pattern.

## Global Constraints

- TEI P5 remains the primary publication XML target where faithful.
- ABC custom schema/sidecar is required for IR facts that TEI does not carry exactly.
- Plaintext must remain metadata-free visible body text: no ruby readings, layout metadata, source notes routed outside body, custom sidecar records, parser warnings, or provenance.
- TEI-EAJ is calibration evidence, not source authority.
- The report must include all five active parser inputs where evidence is available: `aozora2html`, `aozora-epub3`, `aozora-rs`, `aozora2`, and `aozora`.
- Missing parser evidence is an evidence gap, not an implicit pass.
- A report with any `unsupported_gap` cannot claim full IR publication coverage.
- Do not implement the ABC custom schema in this repo; report `IR_PUBLICATION_COVERAGE_BLOCKED_CUSTOM_CONTRACT_MISSING` until ABC supplies a schema/contract identity.

---

## File Structure

- Create `reports/parser-ir/publication-coverage.py`
  - Owns the coverage classification model and report generation.
  - Reads existing summaries and schema artifacts.
  - Emits JSON and Markdown.
- Create `tests/parser-ir-publication-coverage-smoke.sh`
  - Builds fixture inputs in a temporary directory.
  - Proves the verdicts, five-parser evidence handling, unsupported-gap handling, custom-contract-missing handling, and plaintext policy.
- Modify `justfile`
  - Adds `parser-ir-publication-coverage-smoke`.
  - Adds `parser-ir-publication-coverage-report`.
- Create generated report outputs after implementation:
  - `docs/superpowers/reports/2026-07-06-ir-publication-coverage.summary.json`
  - `docs/superpowers/reports/2026-07-06-ir-publication-coverage.md`

---

### Task 1: Coverage Model And Fixture Smoke

**Files:**
- Create: `reports/parser-ir/publication-coverage.py`
- Create: `tests/parser-ir-publication-coverage-smoke.sh`

**Interfaces:**
- Produces Python constants:
  - `SCHEMA_VERSION = "ir-publication-coverage-v1"`
  - `REQUIRED_PARSERS = ("aozora2html", "aozora-epub3", "aozora-rs", "aozora2", "aozora")`
  - `PLAINTEXT_POLICY = {"plaintext_surface": "visible_body_text", "metadata_policy": "exclude_ruby_readings_layout_source_notes_custom_records_warnings_and_provenance"}`
  - `NODE_COVERAGE_CLASSES: dict[str, str]`
  - `CONSTRUCT_COVERAGE_CLASSES: dict[str, str]`
- Later tasks consume:
  - `node_coverage_from_schema(schema: dict[str, Any]) -> dict[str, Any]`
  - `source_construct_coverage(mapping: dict[str, Any]) -> dict[str, Any]`
  - `build_summary(...) -> dict[str, Any]`

- [ ] **Step 1: Write the failing smoke fixture**

Create `tests/parser-ir-publication-coverage-smoke.sh`:

```bash
#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
out_dir="$(mktemp -d "${TMPDIR:-/tmp}/ab-ir-publication-coverage.XXXXXX")"
trap 'rm -rf "$out_dir"' EXIT

parser_schema="$out_dir/parser-ir.schema.json"
mapping="$out_dir/mapping.json"
source_summary="$out_dir/source-summary.json"
matrix_summary="$out_dir/matrix-summary.json"
source_delta="$out_dir/source-delta.summary.json"
summary_json="$out_dir/coverage.summary.json"
report_md="$out_dir/coverage.md"

cat > "$parser_schema" <<'JSON'
{
  "$id": "https://w3id.org/abc/schemas/parser-ir.schema.json",
  "properties": {"schema_hash": {"const": "sha256:fixture-parser-ir"}},
  "$defs": {
    "node": {
      "oneOf": [
        {"$ref": "#/$defs/textNode"},
        {"$ref": "#/$defs/rubyNode"},
        {"$ref": "#/$defs/warigakiNode"},
        {"$ref": "#/$defs/rawSourceNode"}
      ]
    },
    "textNode": {"properties": {"type": {"const": "text"}}},
    "rubyNode": {"properties": {"type": {"const": "ruby"}}},
    "warigakiNode": {"properties": {"type": {"const": "warigaki"}}},
    "rawSourceNode": {"properties": {"type": {"const": "raw-source"}}}
  }
}
JSON

cat > "$mapping" <<'JSON'
{
  "mapping_id": "https://w3id.org/abc/mappings/fixture",
  "mapping_version": "0.2.3",
  "mapping_schema_hash": "sha256:38ec7f0e5affb10329b550a091cd3a6fb5a25e26fd469dfe9f8249970cf9adb4",
  "target_parser_ir_schema_id": "https://w3id.org/abc/schemas/parser-ir.schema.json",
  "target_parser_ir_schema_hash": "sha256:fixture-parser-ir",
  "transform_rule_descriptions": [
    {
      "rule_id": "A-01",
      "category": "AMBIGUITY",
      "aat_pointer": "blocks[].content[].ruby.direction",
      "parser_ir_pointer": "ruby.direction",
      "description": "ruby direction projects to parser-IR"
    },
    {
      "rule_id": "U-01",
      "category": "UNSUPPORTED",
      "aat_pointer": "blocks[].content[].raw",
      "parser_ir_pointer": "(none)",
      "description": "fixture unsupported raw source"
    },
    {
      "rule_id": "S-10",
      "category": "STRUCTURAL",
      "aat_pointer": "blocks[].content[].warigaki",
      "parser_ir_pointer": "warigaki",
      "description": "warigaki requires custom preservation"
    }
  ]
}
JSON

cat > "$source_summary" <<'JSON'
{
  "gate_status": "SOURCE_AUTHORITY_GATE_PASS",
  "works_scanned": 17894,
  "unallowlisted_unknown_markers_total": 0
}
JSON

cat > "$matrix_summary" <<'JSON'
{
  "schema_version": "tei-eaj-generated-comparison-v1",
  "inputs": {
    "adapter_preference": ["aozora2html", "aozora-epub3", "aozora-rs", "aozora2", "aozora"],
    "mapping": "mapping.json"
  },
  "totals": {
    "rows_attempted": 5,
    "materialization_succeeded": 5,
    "materialization_failed": 0,
    "rows_skipped": 0
  },
  "rows": [
    {"work_id": "1", "selected_aat": {"adapter": "aozora2html"}, "parser_ir": {"schema_hash": "sha256:fixture-parser-ir", "nodes": 3, "paragraph_count": 1}},
    {"work_id": "1", "selected_aat": {"adapter": "aozora-epub3"}, "parser_ir": {"schema_hash": "sha256:fixture-parser-ir", "nodes": 3, "paragraph_count": 1}},
    {"work_id": "1", "selected_aat": {"adapter": "aozora-rs"}, "parser_ir": {"schema_hash": "sha256:fixture-parser-ir", "nodes": 3, "paragraph_count": 1}},
    {"work_id": "1", "selected_aat": {"adapter": "aozora2"}, "parser_ir": {"schema_hash": "sha256:fixture-parser-ir", "nodes": 3, "paragraph_count": 1}},
    {"work_id": "1", "selected_aat": {"adapter": "aozora"}, "parser_ir": {"schema_hash": "sha256:fixture-parser-ir", "nodes": 3, "paragraph_count": 1}}
  ]
}
JSON

cat > "$source_delta" <<'JSON'
{
  "schema_version": "plain-prose-source-delta-v1",
  "parser_evidence_coverage": {
    "verdict": "FIVE_PARSER_EVIDENCE_COMPLETE",
    "observed_rows_by_parser": {
      "aozora2html": 1,
      "aozora-epub3": 1,
      "aozora-rs": 1,
      "aozora2": 1,
      "aozora": 1
    },
    "missing_parsers": []
  }
}
JSON

python3 "$repo_root/reports/parser-ir/publication-coverage.py" \
  --parser-ir-schema "$parser_schema" \
  --mapping "$mapping" \
  --source-summary "$source_summary" \
  --matrix-summary "$matrix_summary" \
  --source-delta-summary "$source_delta" \
  --summary-json "$summary_json" \
  --report-md "$report_md"

jq -e '.schema_version == "ir-publication-coverage-v1"' "$summary_json" >/dev/null
jq -e '.source_authority_gate.gate_status == "SOURCE_AUTHORITY_GATE_PASS"' "$summary_json" >/dev/null
jq -e '.parser_evidence_coverage.verdict == "FIVE_PARSER_EVIDENCE_COMPLETE"' "$summary_json" >/dev/null
jq -e '.plaintext_policy.metadata_policy == "exclude_ruby_readings_layout_source_notes_custom_records_warnings_and_provenance"' "$summary_json" >/dev/null
jq -e '.custom_contract.verdict == "CUSTOM_CONTRACT_MISSING"' "$summary_json" >/dev/null
jq -e '.node_coverage.by_node_type.text.class == "tei_exact"' "$summary_json" >/dev/null
jq -e '.node_coverage.by_node_type.ruby.class == "tei_exact"' "$summary_json" >/dev/null
jq -e '.node_coverage.by_node_type.warigaki.class == "tei_plus_abc_extension"' "$summary_json" >/dev/null
jq -e '.node_coverage.by_node_type."raw-source".class == "tei_policy_projection"' "$summary_json" >/dev/null
jq -e '.unsupported_gaps.count == 1' "$summary_json" >/dev/null
jq -e '.unsupported_gaps.items[0].aat_pointer == "blocks[].content[].raw"' "$summary_json" >/dev/null
jq -e '.verdict == "IR_PUBLICATION_COVERAGE_BLOCKED_UNSUPPORTED_GAPS"' "$summary_json" >/dev/null
rg -n "IR Publication Coverage" "$report_md" >/dev/null
rg -n "Unsupported gaps" "$report_md" >/dev/null
```

- [ ] **Step 2: Run the smoke to verify it fails**

Run:

```bash
bash tests/parser-ir-publication-coverage-smoke.sh
```

Expected:

```text
python3: can't open file '.../reports/parser-ir/publication-coverage.py': [Errno 2] No such file or directory
```

- [ ] **Step 3: Add the initial script skeleton and coverage constants**

Create `reports/parser-ir/publication-coverage.py` with executable permissions:

```python
#!/usr/bin/env python3
"""Report full Parser-IR publication coverage across TEI/custom/plaintext targets."""

from __future__ import annotations

import argparse
import hashlib
import json
import pathlib
from collections import Counter
from typing import Any

SCHEMA_VERSION = "ir-publication-coverage-v1"
REQUIRED_PARSERS = ("aozora2html", "aozora-epub3", "aozora-rs", "aozora2", "aozora")
PLAINTEXT_POLICY = {
    "plaintext_surface": "visible_body_text",
    "metadata_policy": "exclude_ruby_readings_layout_source_notes_custom_records_warnings_and_provenance",
}
NODE_COVERAGE_CLASSES = {
    "text": "tei_exact",
    "ruby": "tei_exact",
    "gaiji": "tei_exact",
    "editor-note": "tei_policy_projection",
    "emphasis": "tei_policy_projection",
    "heading": "tei_exact",
    "indentation": "tei_policy_projection",
    "page-break": "tei_exact",
    "line-break": "tei_exact",
    "image": "tei_exact",
    "caption": "tei_policy_projection",
    "quote": "tei_policy_projection",
    "source-note": "tei_policy_projection",
    "warigaki": "tei_plus_abc_extension",
    "raw-source": "tei_policy_projection",
}
CONSTRUCT_COVERAGE_CLASSES = {
    "raw": "tei_policy_projection",
    "warigaki": "tei_plus_abc_extension",
    "accent": "tei_plus_abc_extension",
    "caption_block": "tei_policy_projection",
    "quote_block": "tei_policy_projection",
    "keigakomi_block": "tei_policy_projection",
    "yokogumi_block": "tei_policy_projection",
    "gaiji.resolved": "tei_exact",
}
CLASS_ORDER = (
    "tei_exact",
    "tei_policy_projection",
    "tei_plus_abc_extension",
    "custom_sidecar",
    "plaintext_only",
    "unsupported_gap",
)
```

Task 3 adds the CLI, JSON helpers, and document hashing after the fixture has
proved the expected output shape. Task 1 only establishes the constants and
keeps the smoke red.

- [ ] **Step 4: Commit the failing smoke and skeleton**

Run:

```bash
git add reports/parser-ir/publication-coverage.py tests/parser-ir-publication-coverage-smoke.sh
git commit -m "test(parser-ir): add publication coverage smoke"
```

Expected: commit succeeds with the smoke still failing on missing implementation functions.

---

### Task 2: Node And Construct Coverage Classifier

**Files:**
- Modify: `reports/parser-ir/publication-coverage.py`
- Test: `tests/parser-ir-publication-coverage-smoke.sh`

**Interfaces:**
- Consumes:
  - `NODE_COVERAGE_CLASSES`
  - `CONSTRUCT_COVERAGE_CLASSES`
- Produces:
  - `extract_node_types(schema: dict[str, Any]) -> list[str]`
  - `node_coverage_from_schema(schema: dict[str, Any]) -> dict[str, Any]`
  - `construct_from_pointer(pointer: str | None) -> str`
  - `source_construct_coverage(mapping: dict[str, Any]) -> dict[str, Any]`

- [ ] **Step 1: Add focused unit-style fixture assertions to the smoke**

Append these assertions after the existing `node_coverage` checks:

```bash
jq -e '.node_coverage.counts_by_class.tei_exact == 2' "$summary_json" >/dev/null
jq -e '.node_coverage.counts_by_class.tei_plus_abc_extension == 1' "$summary_json" >/dev/null
jq -e '.node_coverage.counts_by_class.tei_policy_projection == 1' "$summary_json" >/dev/null
jq -e '.source_construct_coverage.counts_by_class.tei_exact == 1' "$summary_json" >/dev/null
jq -e '.source_construct_coverage.counts_by_class.tei_plus_abc_extension == 1' "$summary_json" >/dev/null
jq -e '.source_construct_coverage.counts_by_class.unsupported_gap == 1' "$summary_json" >/dev/null
```

- [ ] **Step 2: Run the smoke to verify classifier output is missing**

Run:

```bash
bash tests/parser-ir-publication-coverage-smoke.sh
```

Expected: FAIL because `.source_construct_coverage` or `.counts_by_class` is absent.

- [ ] **Step 3: Implement schema node extraction**

Add this code to `reports/parser-ir/publication-coverage.py`:

```python
def extract_node_types(schema: dict[str, Any]) -> list[str]:
    defs = schema.get("$defs", {})
    node = defs.get("node", {})
    node_refs = node.get("oneOf", [])
    result: list[str] = []
    for entry in node_refs:
        ref = entry.get("$ref")
        if not isinstance(ref, str) or not ref.startswith("#/$defs/"):
            continue
        def_name = ref.rsplit("/", 1)[-1]
        node_def = defs.get(def_name, {})
        const = (
            node_def.get("properties", {})
            .get("type", {})
            .get("const")
        )
        if const is None:
            for all_of in node_def.get("allOf", []):
                const = (
                    all_of.get("properties", {})
                    .get("type", {})
                    .get("const")
                )
                if const is not None:
                    break
        if isinstance(const, str):
            result.append(const)
    return sorted(set(result))


def count_by_class(items: dict[str, dict[str, Any]]) -> dict[str, int]:
    counts: Counter[str] = Counter(item["class"] for item in items.values())
    return {name: counts.get(name, 0) for name in CLASS_ORDER if counts.get(name, 0)}


def node_coverage_from_schema(schema: dict[str, Any]) -> dict[str, Any]:
    by_node_type: dict[str, dict[str, Any]] = {}
    unsupported: list[dict[str, Any]] = []
    for node_type in extract_node_types(schema):
        coverage_class = NODE_COVERAGE_CLASSES.get(node_type, "unsupported_gap")
        entry = {
            "class": coverage_class,
            "target": node_target(node_type, coverage_class),
        }
        by_node_type[node_type] = entry
        if coverage_class == "unsupported_gap":
            unsupported.append({"node_type": node_type, "owner": "abc_custom_schema"})
    return {
        "by_node_type": by_node_type,
        "counts_by_class": count_by_class(by_node_type),
        "unsupported": unsupported,
    }


def node_target(node_type: str, coverage_class: str) -> str:
    targets = {
        "text": "TEI text node and plaintext text",
        "ruby": "TEI ruby type=furigana; plaintext base text only",
        "gaiji": "TEI g plus charDecl",
        "editor-note": "TEI note; omitted from plaintext",
        "emphasis": "TEI hi with rend/rendition",
        "heading": "TEI head",
        "indentation": "TEI p@rend or seg/div policy",
        "page-break": "TEI pb",
        "line-break": "TEI lb",
        "image": "TEI figure/graphic",
        "caption": "TEI figure/head or typed div/seg policy",
        "quote": "TEI quote or cit policy",
        "source-note": "TEI front/body/back note routing",
        "warigaki": "TEI inline note plus ABC split-line preservation",
        "raw-source": "TEI raw-source segment or ABC recovery record",
    }
    return targets.get(node_type, f"unsupported {coverage_class}")
```

- [ ] **Step 4: Implement source construct classification**

Add this code:

```python
def construct_from_pointer(pointer: str | None) -> str:
    if not pointer:
        return "unknown"
    for token in (
        "caption_block",
        "quote_block",
        "keigakomi_block",
        "yokogumi_block",
        "warigaki",
        "gaiji.resolved",
        "accent",
        "raw",
    ):
        if token in pointer:
            return token
    if ".ruby" in pointer or "ruby." in pointer:
        return "ruby"
    return pointer.rsplit(".", 1)[-1].replace("[]", "")


def source_construct_coverage(mapping: dict[str, Any]) -> dict[str, Any]:
    by_construct: dict[str, dict[str, Any]] = {}
    unsupported: list[dict[str, Any]] = []
    for rule in mapping.get("transform_rule_descriptions", []):
        aat_pointer = rule.get("aat_pointer")
        construct = construct_from_pointer(aat_pointer)
        category = str(rule.get("category") or "unknown")
        coverage_class = CONSTRUCT_COVERAGE_CLASSES.get(construct)
        if category == "UNSUPPORTED":
            coverage_class = "unsupported_gap"
        if coverage_class is None:
            coverage_class = "tei_policy_projection"
        entry = by_construct.setdefault(
            construct,
            {
                "class": coverage_class,
                "rules": 0,
                "categories": {},
                "examples": [],
            },
        )
        entry["rules"] += 1
        entry["categories"][category] = entry["categories"].get(category, 0) + 1
        if len(entry["examples"]) < 3:
            entry["examples"].append(
                {
                    "rule_id": rule.get("rule_id"),
                    "aat_pointer": aat_pointer,
                    "parser_ir_pointer": rule.get("parser_ir_pointer"),
                    "category": category,
                }
            )
        if coverage_class == "unsupported_gap":
            unsupported.append(
                {
                    "rule_id": rule.get("rule_id"),
                    "aat_pointer": aat_pointer,
                    "parser_ir_pointer": rule.get("parser_ir_pointer"),
                    "category": category,
                    "owner": "parser_ir_schema",
                }
            )
    return {
        "by_construct": dict(sorted(by_construct.items())),
        "counts_by_class": count_by_class(by_construct),
        "unsupported": unsupported,
    }
```

- [ ] **Step 5: Run the smoke**

Run:

```bash
bash tests/parser-ir-publication-coverage-smoke.sh
```

Expected: still FAIL until summary assembly is implemented in Task 3, but classifier-related jq checks should be reachable after Task 3.

- [ ] **Step 6: Commit classifier**

Run:

```bash
git add reports/parser-ir/publication-coverage.py tests/parser-ir-publication-coverage-smoke.sh
git commit -m "feat(parser-ir): classify publication coverage constructs"
```

---

### Task 3: Summary Verdicts And Markdown Report

**Files:**
- Modify: `reports/parser-ir/publication-coverage.py`
- Test: `tests/parser-ir-publication-coverage-smoke.sh`

**Interfaces:**
- Consumes:
  - `node_coverage_from_schema`
  - `source_construct_coverage`
- Produces:
  - `build_summary(...) -> dict[str, Any]`
  - `render_markdown(summary: dict[str, Any]) -> str`
  - CLI outputs `--summary-json` and `--report-md`

- [ ] **Step 1: Add verdict assertions for the custom-contract case**

The Task 1 smoke already expects unsupported gaps to dominate. Add a second fixture run after the first one to prove custom-contract-missing blocks when unsupported gaps are absent:

```bash
supported_mapping="$out_dir/mapping-supported.json"
supported_summary_json="$out_dir/coverage-supported.summary.json"
supported_report_md="$out_dir/coverage-supported.md"

jq 'del(.transform_rule_descriptions[] | select(.category == "UNSUPPORTED"))' "$mapping" > "$supported_mapping"

python3 "$repo_root/reports/parser-ir/publication-coverage.py" \
  --parser-ir-schema "$parser_schema" \
  --mapping "$supported_mapping" \
  --source-summary "$source_summary" \
  --matrix-summary "$matrix_summary" \
  --source-delta-summary "$source_delta" \
  --summary-json "$supported_summary_json" \
  --report-md "$supported_report_md"

jq -e '.unsupported_gaps.count == 0' "$supported_summary_json" >/dev/null
jq -e '.verdict == "IR_PUBLICATION_COVERAGE_BLOCKED_CUSTOM_CONTRACT_MISSING"' "$supported_summary_json" >/dev/null
```

- [ ] **Step 2: Run the smoke to verify summary assembly is missing**

Run:

```bash
bash tests/parser-ir-publication-coverage-smoke.sh
```

Expected: FAIL because CLI output is incomplete or verdict fields are absent.

- [ ] **Step 3: Implement parser evidence and source authority gates**

Add this code:

```python
def source_authority_gate(source: dict[str, Any]) -> dict[str, Any]:
    return {
        "gate_status": source.get("gate_status"),
        "works_scanned": source.get("works_scanned"),
        "unallowlisted_unknown_markers_total": source.get("unallowlisted_unknown_markers_total"),
    }


def source_authority_passed(source: dict[str, Any]) -> bool:
    return (
        source.get("gate_status") == "SOURCE_AUTHORITY_GATE_PASS"
        and source.get("unallowlisted_unknown_markers_total") == 0
    )


def parser_evidence_coverage(matrix: dict[str, Any], source_delta: dict[str, Any]) -> dict[str, Any]:
    observed = Counter()
    for row in matrix.get("rows", []):
        adapter = row.get("selected_aat", {}).get("adapter")
        if adapter:
            observed[adapter] += 1
    delta_coverage = source_delta.get("parser_evidence_coverage", {})
    missing = sorted(parser for parser in REQUIRED_PARSERS if observed.get(parser, 0) == 0)
    verdict = "FIVE_PARSER_EVIDENCE_COMPLETE" if not missing else "FIVE_PARSER_EVIDENCE_INCOMPLETE"
    return {
        "verdict": verdict,
        "required_parsers": list(REQUIRED_PARSERS),
        "observed_rows_by_parser": {parser: observed.get(parser, 0) for parser in REQUIRED_PARSERS},
        "missing_parsers": missing,
        "plain_prose_delta_verdict": delta_coverage.get("verdict"),
    }
```

- [ ] **Step 4: Implement mapping and custom-contract blocks**

Add this code:

```python
def custom_contract_block(custom_contract_schema: pathlib.Path | None) -> dict[str, Any]:
    if custom_contract_schema is None:
        return {
            "verdict": "CUSTOM_CONTRACT_MISSING",
            "schema_id": None,
            "schema_version": None,
            "message": "ABC custom preservation contract has not been supplied to this report.",
        }
    contract = load_json(custom_contract_schema)
    return {
        "verdict": "CUSTOM_CONTRACT_PRESENT",
        "schema_id": contract.get("$id") or contract.get("schema_id"),
        "schema_version": contract.get("schema_version"),
        "path": str(custom_contract_schema),
    }


def mapping_block(mapping: dict[str, Any]) -> dict[str, Any]:
    return {
        "mapping_id": mapping.get("mapping_id"),
        "mapping_version": mapping.get("mapping_version"),
        "mapping_hash": document_hash(mapping),
        "mapping_schema_hash": mapping.get("mapping_schema_hash"),
        "target_parser_ir_schema_id": mapping.get("target_parser_ir_schema_id"),
        "target_parser_ir_schema_hash": mapping.get("target_parser_ir_schema_hash"),
        "generated_mapping_rules": len(mapping.get("transform_rule_descriptions", [])),
    }
```

- [ ] **Step 5: Implement verdict precedence**

Add this code:

```python
def unsupported_gaps(node_coverage: dict[str, Any], construct_coverage: dict[str, Any]) -> dict[str, Any]:
    items = []
    items.extend(node_coverage.get("unsupported", []))
    items.extend(construct_coverage.get("unsupported", []))
    return {"count": len(items), "items": items}


def publication_verdict(
    source_passed: bool,
    evidence: dict[str, Any],
    custom_contract: dict[str, Any],
    gaps: dict[str, Any],
) -> str:
    if not source_passed:
        return "IR_PUBLICATION_COVERAGE_BLOCKED_SOURCE_AUTHORITY"
    if evidence.get("verdict") != "FIVE_PARSER_EVIDENCE_COMPLETE":
        return "IR_PUBLICATION_COVERAGE_BLOCKED_INCOMPLETE_PARSER_EVIDENCE"
    if gaps.get("count", 0) > 0:
        return "IR_PUBLICATION_COVERAGE_BLOCKED_UNSUPPORTED_GAPS"
    if custom_contract.get("verdict") != "CUSTOM_CONTRACT_PRESENT":
        return "IR_PUBLICATION_COVERAGE_BLOCKED_CUSTOM_CONTRACT_MISSING"
    return "IR_PUBLICATION_COVERAGE_COMPLETE"
```

- [ ] **Step 6: Implement `build_summary` and Markdown**

Add this code:

```python
def build_summary(
    parser_schema: dict[str, Any],
    mapping: dict[str, Any],
    source: dict[str, Any],
    matrix: dict[str, Any],
    source_delta: dict[str, Any],
    custom_contract_schema: pathlib.Path | None,
) -> dict[str, Any]:
    node_coverage = node_coverage_from_schema(parser_schema)
    construct_coverage = source_construct_coverage(mapping)
    evidence = parser_evidence_coverage(matrix, source_delta)
    custom_contract = custom_contract_block(custom_contract_schema)
    gaps = unsupported_gaps(node_coverage, construct_coverage)
    source_passed = source_authority_passed(source)
    return {
        "schema_version": SCHEMA_VERSION,
        "verdict": publication_verdict(source_passed, evidence, custom_contract, gaps),
        "source_authority_gate": source_authority_gate(source),
        "parser_evidence_coverage": evidence,
        "parser_ir_schema": {
            "schema_id": parser_schema.get("$id"),
            "schema_hash": mapping.get("target_parser_ir_schema_hash") or parser_schema.get("properties", {}).get("schema_hash", {}).get("const"),
        },
        "tei_profile": {
            "role": "primary_publication_xml",
            "calibration": "tei_eaj_calibration_only",
        },
        "custom_contract": custom_contract,
        "mapping": mapping_block(mapping),
        "node_coverage": node_coverage,
        "source_construct_coverage": construct_coverage,
        "unsupported_gaps": gaps,
        "tei_eaj_calibration": {
            "matrix_schema_version": matrix.get("schema_version"),
            "rows_attempted": matrix.get("totals", {}).get("rows_attempted"),
            "materialization_failed": matrix.get("totals", {}).get("materialization_failed"),
            "role": "calibration_not_source_authority",
        },
        "plaintext_policy": PLAINTEXT_POLICY,
    }


def render_markdown(summary: dict[str, Any]) -> str:
    node_counts = summary["node_coverage"]["counts_by_class"]
    construct_counts = summary["source_construct_coverage"]["counts_by_class"]
    gaps = summary["unsupported_gaps"]
    lines = [
        "# IR Publication Coverage",
        "",
        f"Verdict: `{summary['verdict']}`",
        "",
        "## Node Coverage",
        "",
        "| Class | Node types |",
        "|---|---:|",
    ]
    for name, count in node_counts.items():
        lines.append(f"| `{name}` | {count} |")
    lines.extend(["", "## Source Construct Coverage", "", "| Class | Constructs |", "|---|---:|"])
    for name, count in construct_counts.items():
        lines.append(f"| `{name}` | {count} |")
    lines.extend(["", "## Unsupported gaps", "", f"Count: {gaps['count']}", ""])
    for item in gaps["items"][:20]:
        label = item.get("aat_pointer") or item.get("node_type")
        lines.append(f"- `{label}` owner `{item.get('owner')}`")
    lines.extend([
        "",
        "## Plaintext Policy",
        "",
        f"`{summary['plaintext_policy']['metadata_policy']}`",
        "",
        "## TEI-EAJ Calibration",
        "",
        "TEI-EAJ rows are calibration evidence, not source authority.",
        "",
    ])
    return "\n".join(lines)
```

- [ ] **Step 7: Implement CLI `main`**

Add this code:

```python
def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--parser-ir-schema", required=True, type=pathlib.Path)
    parser.add_argument("--mapping", required=True, type=pathlib.Path)
    parser.add_argument("--source-summary", required=True, type=pathlib.Path)
    parser.add_argument("--matrix-summary", required=True, type=pathlib.Path)
    parser.add_argument("--source-delta-summary", required=True, type=pathlib.Path)
    parser.add_argument("--custom-contract-schema", type=pathlib.Path)
    parser.add_argument("--summary-json", required=True, type=pathlib.Path)
    parser.add_argument("--report-md", required=True, type=pathlib.Path)
    return parser.parse_args()


def load_json(path: pathlib.Path) -> Any:
    return json.loads(path.read_text(encoding="utf-8"))


def write_json(path: pathlib.Path, value: Any) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(json.dumps(value, ensure_ascii=False, indent=2) + "\n", encoding="utf-8")


def write_text(path: pathlib.Path, value: str) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(value, encoding="utf-8")


def canonical_json(value: object) -> str:
    return json.dumps(value, ensure_ascii=False, sort_keys=True, separators=(",", ":")).replace("/", "\\/")


def document_hash(value: object) -> str:
    return "sha256:" + hashlib.sha256(canonical_json(value).encode("utf-8")).hexdigest()


def main() -> None:
    args = parse_args()
    summary = build_summary(
        parser_schema=load_json(args.parser_ir_schema),
        mapping=load_json(args.mapping),
        source=load_json(args.source_summary),
        matrix=load_json(args.matrix_summary),
        source_delta=load_json(args.source_delta_summary),
        custom_contract_schema=args.custom_contract_schema,
    )
    write_json(args.summary_json, summary)
    write_text(args.report_md, render_markdown(summary))


if __name__ == "__main__":
    main()
```

- [ ] **Step 8: Run the smoke**

Run:

```bash
bash tests/parser-ir-publication-coverage-smoke.sh
```

Expected: PASS.

- [ ] **Step 9: Commit summary implementation**

Run:

```bash
git add reports/parser-ir/publication-coverage.py tests/parser-ir-publication-coverage-smoke.sh
git commit -m "feat(parser-ir): report publication coverage verdicts"
```

---

### Task 4: Just Recipe And Current Report Generation

**Files:**
- Modify: `justfile`
- Create generated outputs:
  - `docs/superpowers/reports/2026-07-06-ir-publication-coverage.summary.json`
  - `docs/superpowers/reports/2026-07-06-ir-publication-coverage.md`

**Interfaces:**
- Consumes:
  - `reports/parser-ir/publication-coverage.py`
  - `docs/superpowers/reports/2026-07-04-tei-eaj-generated-matrix-comparison.summary.json`
  - `docs/superpowers/reports/2026-07-04-source-authority-representability.summary.json`
  - `docs/superpowers/reports/2026-07-05-plain-prose-source-delta.summary.json`
  - `data/abc-schemas/schemas/parser-ir.schema.json`
  - `data/aat-to-parser-ir-mapping-v1.json`
- Produces:
  - Repeatable `just parser-ir-publication-coverage-report`.

- [ ] **Step 1: Add failing just smoke invocation**

Run:

```bash
just parser-ir-publication-coverage-smoke
```

Expected:

```text
error: Justfile does not contain recipe `parser-ir-publication-coverage-smoke`
```

- [ ] **Step 2: Add recipes to `justfile`**

Insert after `parser-ir-plain-prose-source-delta-report`:

```make
parser-ir-publication-coverage-smoke:
	@bash "{{repo_root}}/tests/parser-ir-publication-coverage-smoke.sh"

parser-ir-publication-coverage-report PARSER_IR_SCHEMA="data/abc-schemas/schemas/parser-ir.schema.json" MAPPING="data/aat-to-parser-ir-mapping-v1.json" MATRIX_SUMMARY="docs/superpowers/reports/2026-07-04-tei-eaj-generated-matrix-comparison.summary.json" SOURCE_SUMMARY="docs/superpowers/reports/2026-07-04-source-authority-representability.summary.json" SOURCE_DELTA_SUMMARY="docs/superpowers/reports/2026-07-05-plain-prose-source-delta.summary.json" REPORT_MD="docs/superpowers/reports/2026-07-06-ir-publication-coverage.md" SUMMARY_JSON="docs/superpowers/reports/2026-07-06-ir-publication-coverage.summary.json" CUSTOM_CONTRACT_SCHEMA="":
	@args=(); if [ -n "{{CUSTOM_CONTRACT_SCHEMA}}" ]; then args+=(--custom-contract-schema "{{repo_root}}/{{CUSTOM_CONTRACT_SCHEMA}}"); fi; \
	python3 "{{repo_root}}/reports/parser-ir/publication-coverage.py" \
		--parser-ir-schema "{{repo_root}}/{{PARSER_IR_SCHEMA}}" \
		--mapping "{{repo_root}}/{{MAPPING}}" \
		--source-summary "{{repo_root}}/{{SOURCE_SUMMARY}}" \
		--matrix-summary "{{repo_root}}/{{MATRIX_SUMMARY}}" \
		--source-delta-summary "{{repo_root}}/{{SOURCE_DELTA_SUMMARY}}" \
		--summary-json "{{repo_root}}/{{SUMMARY_JSON}}" \
		--report-md "{{repo_root}}/{{REPORT_MD}}" \
		"${args[@]}"
```

- [ ] **Step 3: Run the smoke via just**

Run:

```bash
just parser-ir-publication-coverage-smoke
```

Expected: PASS.

- [ ] **Step 4: Generate the current report**

Run:

```bash
just parser-ir-publication-coverage-report
```

Expected:

- `docs/superpowers/reports/2026-07-06-ir-publication-coverage.summary.json` exists.
- `docs/superpowers/reports/2026-07-06-ir-publication-coverage.md` exists.
- Summary verdict is either `IR_PUBLICATION_COVERAGE_BLOCKED_UNSUPPORTED_GAPS` or `IR_PUBLICATION_COVERAGE_BLOCKED_CUSTOM_CONTRACT_MISSING`; it must not be `IR_PUBLICATION_COVERAGE_COMPLETE` until ABC supplies a custom contract and unsupported gaps are empty.

- [ ] **Step 5: Verify generated report invariants**

Run:

```bash
jq -e '.parser_evidence_coverage.required_parsers == ["aozora2html","aozora-epub3","aozora-rs","aozora2","aozora"]' docs/superpowers/reports/2026-07-06-ir-publication-coverage.summary.json
jq -e '.plaintext_policy.metadata_policy == "exclude_ruby_readings_layout_source_notes_custom_records_warnings_and_provenance"' docs/superpowers/reports/2026-07-06-ir-publication-coverage.summary.json
jq -e '.tei_eaj_calibration.role == "calibration_not_source_authority"' docs/superpowers/reports/2026-07-06-ir-publication-coverage.summary.json
jq -e '.verdict != "IR_PUBLICATION_COVERAGE_COMPLETE"' docs/superpowers/reports/2026-07-06-ir-publication-coverage.summary.json
```

Expected: all commands exit 0.

- [ ] **Step 6: Commit recipes and generated report**

Run:

```bash
git add justfile docs/superpowers/reports/2026-07-06-ir-publication-coverage.summary.json docs/superpowers/reports/2026-07-06-ir-publication-coverage.md
git commit -m "feat(parser-ir): publish IR publication coverage report"
```

---

### Task 5: Validation And Handoff

**Files:**
- Create: `docs/handoffs/ir-publication-coverage-contract.md`

**Interfaces:**
- Consumes:
  - Committed spec `docs/superpowers/specs/2026-07-06-ir-to-tei-or-custom-schema-design.md`
  - New report `docs/superpowers/reports/2026-07-06-ir-publication-coverage.summary.json`
- Produces:
  - A concise ABC handoff explaining what ABC must own next.

- [ ] **Step 1: Write ABC handoff**

Create `docs/handoffs/ir-publication-coverage-contract.md`:

```markdown
# IR Publication Coverage Contract Handoff

ab-validator now treats TEI-EAJ as calibration evidence and full publication
coverage as the real gate.

Required ABC-side decision:

- Define the custom preservation contract for Parser-IR facts that TEI P5 does
  not carry exactly.
- Decide whether v1 is JSON sidecar, TEI namespace extension, or both.
- Give the contract a stable schema id, schema version, and manifest linkage.
- Keep plaintext metadata-free.

ab-validator report:

- `docs/superpowers/reports/2026-07-06-ir-publication-coverage.md`
- `docs/superpowers/reports/2026-07-06-ir-publication-coverage.summary.json`

The report's complete verdict is intentionally blocked until:

- unsupported gaps are empty, and
- ABC supplies a custom contract schema identity to the report.

Five parser inputs are required evidence:

- `aozora2html`
- `aozora-epub3`
- `aozora-rs`
- `aozora2`
- `aozora`
```

- [ ] **Step 2: Run full local verification for this slice**

Run:

```bash
bash tests/parser-ir-publication-coverage-smoke.sh
just parser-ir-publication-coverage-smoke
python3 -m py_compile reports/parser-ir/publication-coverage.py
git diff --check HEAD~4..HEAD
```

Expected:

- smoke passes twice,
- Python compilation exits 0,
- `git diff --check` exits 0.

- [ ] **Step 3: Commit handoff**

Run:

```bash
git add docs/handoffs/ir-publication-coverage-contract.md
git commit -m "docs(parser-ir): hand off publication coverage contract"
```

---

## Self-Review

- Spec coverage:
  - TEI/custom/plaintext split -> Tasks 1-3.
  - Mapping classes -> Tasks 1-3.
  - Five-parser evidence -> Tasks 1, 3, 4.
  - Custom-contract missing verdict -> Tasks 1, 3, 5.
  - TEI-EAJ calibration role -> Tasks 3-4.
  - Plaintext metadata-free policy -> Tasks 1, 3-4.
- Placeholder scan:
  - No unresolved placeholder tokens.
  - No vague delegated implementation steps.
- Type consistency:
  - `node_coverage_from_schema`, `source_construct_coverage`, `build_summary`, and `render_markdown` are defined before use.
  - JSON field names match the committed spec: `source_authority_gate`, `parser_evidence_coverage`, `custom_contract`, `node_coverage`, `source_construct_coverage`, `unsupported_gaps`, `tei_eaj_calibration`, and `plaintext_policy`.
