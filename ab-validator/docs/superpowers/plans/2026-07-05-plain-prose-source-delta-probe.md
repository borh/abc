# Plain Prose Source Delta Probe Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Build a source-authoritative plain-prose Level 3 delta probe that separates adapter, Parser-IR, ABC renderer, policy, and evidence blockers while requiring five-parser evidence.

**Architecture:** Add one focused Python report generator under `reports/parser-ir/`, one fixture smoke under `tests/`, and `just`/flake wiring. The generator consumes existing summary artifacts first, emits a JSON+Markdown diagnostic report, and treats missing five-parser coverage as an explicit evidence blocker instead of a hidden caveat.

**Tech Stack:** Python 3 standard library, Bash, `jq`, `ripgrep`, existing `justfile`, existing Nix flake smoke-check pattern.

## Global Constraints

- Required parser labels are exactly `aozora2html`, `aozora-epub3`, `aozora-rs`, `aozora2`, and `aozora`.
- Source inventory is authoritative for Aozora source markup; parser and adapter outputs are supporting evidence.
- Plaintext is body base text only. Ruby, notes, source attribution, and other typed metadata are not plaintext.
- The probe does not admit Level 3; it produces next-action evidence.
- The fixture smoke must not read `/db` and must not use the network.
- The report must fail rather than emit an empty `mapping` object.
- Rows can have multiple classifications and multiple blocker owners; do not collapse them to a single precedence-ordered owner.
- No Rust schema or converter changes are required for this probe.

---

## File Structure

- Modify `reports/parser-ir/tei-eaj-generated-compare.py`
  - Owns the generated matrix comparison. Its default adapter preference must include all five adapters so future matrix runs can produce five-parser evidence.
- Create `reports/parser-ir/plain-prose-source-delta.py`
  - Owns the new diagnostic classification and output contract.
- Create `tests/parser-ir-plain-prose-source-delta-smoke.sh`
  - Builds a temporary fixture and asserts the report contract without `/db`.
- Modify `justfile`
  - Add `parser-ir-plain-prose-source-delta-smoke` and `parser-ir-plain-prose-source-delta-report`.
- Modify `flake.nix`
  - Add `plainProseSourceDeltaSmokeCheck` and expose `checks.<system>.parser-ir-plain-prose-source-delta-smoke`.
- Create generated reports:
  - `docs/superpowers/reports/2026-07-05-plain-prose-source-delta.md`
  - `docs/superpowers/reports/2026-07-05-plain-prose-source-delta.summary.json`

---

### Task 1: Make Generated Matrix Preference Five-Parser Complete

**Files:**
- Modify: `reports/parser-ir/tei-eaj-generated-compare.py`
- Test: `tests/parser-ir-plain-prose-source-delta-smoke.sh` (created in Task 2)

**Interfaces:**
- Consumes: existing CLI option `--adapter-preference`.
- Produces: a default adapter preference list of `aozora2html,aozora-epub3,aozora-rs,aozora2,aozora`.

- [ ] **Step 1: Add a module-level required parser constant**

In `reports/parser-ir/tei-eaj-generated-compare.py`, add this constant near the other module constants:

```python
REQUIRED_PARSERS = (
    "aozora2html",
    "aozora-epub3",
    "aozora-rs",
    "aozora2",
    "aozora",
)
```

- [ ] **Step 2: Change the CLI default**

Replace the current `--adapter-preference` default:

```python
default="aozora2html,aozora-epub3,aozora-rs,aozora2",
```

with:

```python
default=",".join(REQUIRED_PARSERS),
```

- [ ] **Step 3: Verify the default through Python argparse**

Run:

```bash
python - <<'PY'
import importlib.util
import pathlib
import sys

path = pathlib.Path("reports/parser-ir/tei-eaj-generated-compare.py")
spec = importlib.util.spec_from_file_location("tei_eaj_generated_compare", path)
module = importlib.util.module_from_spec(spec)
assert spec.loader is not None
sys.argv = [
    "tei-eaj-generated-compare.py",
    "--workset", "w.json",
    "--structural-summary", "s.json",
    "--mapping", "m.json",
    "--converter-bin", "bin",
    "--abc-root", "abc",
    "--abc-schema-root", "schemas",
    "--out-dir", "out",
]
spec.loader.exec_module(module)
args = module.parse_args()
assert args.adapter_preference == "aozora2html,aozora-epub3,aozora-rs,aozora2,aozora"
print(args.adapter_preference)
PY
```

Expected output:

```text
aozora2html,aozora-epub3,aozora-rs,aozora2,aozora
```

- [ ] **Step 4: Commit**

```bash
git add reports/parser-ir/tei-eaj-generated-compare.py
git commit -m "fix(parser-ir): include aozora in level3 matrix preference"
```

---

### Task 2: Add The Probe Smoke First

**Files:**
- Create: `tests/parser-ir-plain-prose-source-delta-smoke.sh`
- Not yet created but called by this test: `reports/parser-ir/plain-prose-source-delta.py`

**Interfaces:**
- Consumes CLI:
  - `--admission-summary PATH`
  - `--matrix-summary PATH`
  - `--structural-summary PATH`
  - `--source-summary PATH`
  - `--mapping PATH`
  - `--summary-json PATH`
  - `--report-md PATH`
  - optional `--allow-missing-parser-evidence`
- Produces JSON shape from the spec with `schema_version == "plain-prose-source-delta-probe-v1"`.

- [ ] **Step 1: Create the failing smoke**

Create `tests/parser-ir-plain-prose-source-delta-smoke.sh`:

```bash
#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
out_dir="$(mktemp -d "${TMPDIR:-/tmp}/ab-plain-prose-delta-smoke.XXXXXX")"
trap 'rm -rf "$out_dir"' EXIT

admission_summary="$out_dir/admission.summary.json"
matrix_summary="$out_dir/matrix.summary.json"
structural_summary="$out_dir/structural.summary.json"
source_summary="$out_dir/source.summary.json"
mapping_file="$out_dir/mapping.json"
summary_json="$out_dir/plain-prose-delta.summary.json"
report_md="$out_dir/plain-prose-delta.md"
strict_summary_json="$out_dir/plain-prose-delta-strict.summary.json"
strict_report_md="$out_dir/plain-prose-delta-strict.md"
strict_stderr="$out_dir/plain-prose-delta-strict.stderr"

cat > "$source_summary" <<'JSON'
{
  "gate_status": "SOURCE_AUTHORITY_GATE_PASS",
  "works_scanned": 17894,
  "unallowlisted_unknown_markers_total": 0
}
JSON

cat > "$mapping_file" <<'JSON'
{
  "mapping_id": "https://w3id.org/abc/mappings/aat-v1-to-parser-ir-v1/generated-probe",
  "mapping_version": "0.2.3",
  "mapping_schema_hash": "sha256:38ec7f0e5affb10329b550a091cd3a6fb5a25e26fd469dfe9f8249970cf9adb4",
  "target_parser_ir_schema_id": "https://w3id.org/abc/schemas/parser-ir.schema.json",
  "target_parser_ir_schema_hash": "sha256:8e56871965e647e40ade08fd9dd580a3516d33905be17957cc79750bd42ea64d",
  "transform_rule_descriptions": [
    {"rule_id": "S-01", "category": "STRUCTURAL"}
  ]
}
JSON

cat > "$admission_summary" <<'JSON'
{
  "schema_version": "profile-aware-level3-tei-admission-v1",
  "source_authority_gate": {
    "gate_status": "SOURCE_AUTHORITY_GATE_PASS",
    "works_scanned": 17894,
    "unallowlisted_unknown_markers_total": 0
  },
  "parser_ir_infrastructure_verdict": "LEVEL3_IR_INFRASTRUCTURE_READY",
  "plain_prose_admission": {
    "rows_total": 5,
    "rows_passed": 0,
    "rows_failed": 5,
    "verdict": "LEVEL3_PLAIN_PROSE_BLOCKED_ADAPTER_FIDELITY_AND_TEXT_POLICY",
    "blocking_owners": ["adapter", "policy"]
  },
  "mapping": {
    "mapping_id": "https://w3id.org/abc/mappings/aat-v1-to-parser-ir-v1/generated-probe",
    "mapping_version": "0.2.3",
    "mapping_hash": "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
    "mapping_schema_hash": "sha256:38ec7f0e5affb10329b550a091cd3a6fb5a25e26fd469dfe9f8249970cf9adb4",
    "target_parser_ir_schema_hash": "sha256:8e56871965e647e40ade08fd9dd580a3516d33905be17957cc79750bd42ea64d",
    "generated_mapping_rules": 1
  }
}
JSON

cat > "$structural_summary" <<'JSON'
{
  "schema_version": "tei-eaj-structural-expansion-v1",
  "rows": []
}
JSON

cat > "$matrix_summary" <<'JSON'
{
  "schema_version": "tei-eaj-generated-comparison-v1",
  "inputs": {
    "adapter_preference": ["aozora2html", "aozora-epub3", "aozora-rs", "aozora2", "aozora"],
    "mapping": "mapping.json",
    "candidate_mode": "all"
  },
  "rows": [
    {
      "work_id": "w1",
      "title": "Adapter over segmented",
      "tei_eaj_file": "data/plain/w1.xml",
      "selected_aat": {"adapter": "aozora2html"},
      "materialization": {"status": "passed"},
      "tei_eaj": {"structure_profile": "plain_prose"},
      "classification": {"paragraph_origin_bucket": "adapter_over_segmented", "source_note_body_excluded": false},
      "text": {"body_text_match_bucket": "base_equal"}
    },
    {
      "work_id": "w1",
      "title": "Adapter over segmented",
      "tei_eaj_file": "data/plain/w1.xml",
      "selected_aat": {"adapter": "aozora-epub3"},
      "materialization": {"status": "passed"},
      "tei_eaj": {"structure_profile": "plain_prose"},
      "classification": {"paragraph_origin_bucket": "aligned", "source_note_body_excluded": false},
      "text": {"body_text_match_bucket": "ruby_expanded_equal"}
    },
    {
      "work_id": "w1",
      "title": "Adapter over segmented",
      "tei_eaj_file": "data/plain/w1.xml",
      "selected_aat": {"adapter": "aozora-rs"},
      "materialization": {"status": "passed"},
      "tei_eaj": {"structure_profile": "plain_prose"},
      "classification": {"paragraph_origin_bucket": "source_note_back_routing", "source_note_body_excluded": true},
      "text": {"body_text_match_bucket": "base_drop_parentheticals_equal"}
    },
    {
      "work_id": "w1",
      "title": "Adapter over segmented",
      "tei_eaj_file": "data/plain/w1.xml",
      "selected_aat": {"adapter": "aozora2"},
      "materialization": {"status": "passed"},
      "tei_eaj": {"structure_profile": "plain_prose"},
      "classification": {"paragraph_origin_bucket": "aligned", "source_note_body_excluded": false},
      "text": {"body_text_match_bucket": "different"}
    },
    {
      "work_id": "w2",
      "title": "Missing parser evidence",
      "tei_eaj_file": "data/plain/w2.xml",
      "selected_aat": {"adapter": "aozora2html"},
      "materialization": {"status": "passed"},
      "tei_eaj": {"structure_profile": "plain_prose"},
      "classification": {"paragraph_origin_bucket": "aligned", "source_note_body_excluded": false},
      "text": {"body_text_match_bucket": "base_equal"}
    }
  ],
  "skipped": [
    {"tei_eaj_file": "data/plain/no-id.xml", "reason": "no_materializable_aat"}
  ]
}
JSON

set +e
python "$repo_root/reports/parser-ir/plain-prose-source-delta.py" \
  --admission-summary "$admission_summary" \
  --matrix-summary "$matrix_summary" \
  --structural-summary "$structural_summary" \
  --source-summary "$source_summary" \
  --mapping "$mapping_file" \
  --summary-json "$strict_summary_json" \
  --report-md "$strict_report_md" \
  2>"$strict_stderr"
strict_status=$?
set -e

test "$strict_status" -ne 0
rg -n "missing parser evidence" "$strict_stderr"

python "$repo_root/reports/parser-ir/plain-prose-source-delta.py" \
  --admission-summary "$admission_summary" \
  --matrix-summary "$matrix_summary" \
  --structural-summary "$structural_summary" \
  --source-summary "$source_summary" \
  --mapping "$mapping_file" \
  --summary-json "$summary_json" \
  --report-md "$report_md" \
  --allow-missing-parser-evidence

jq -e '.schema_version == "plain-prose-source-delta-probe-v1"' "$summary_json"
jq -e '.required_parsers == ["aozora2html", "aozora-epub3", "aozora-rs", "aozora2", "aozora"]' "$summary_json"
jq -e '.mapping.mapping_id == "https://w3id.org/abc/mappings/aat-v1-to-parser-ir-v1/generated-probe"' "$summary_json"
jq -e '.parser_evidence_coverage.verdict == "FIVE_PARSER_EVIDENCE_INCOMPLETE"' "$summary_json"
jq -e '.parser_evidence_coverage.missing_parsers == []' "$summary_json"
jq -e '.parser_evidence_coverage.rows_missing_required_parser_evidence == 2' "$summary_json"
jq -e '.classification_counts.adapter_paragraph_bug == 1' "$summary_json"
jq -e '.classification_counts.ruby_metadata_not_plaintext == 1' "$summary_json"
jq -e '.classification_counts.source_note_metadata_excluded == 1' "$summary_json"
jq -e '.classification_counts.source_text_policy_required == 1' "$summary_json"
jq -e '.classification_counts.missing_parser_evidence == 2' "$summary_json"
jq -e '.blocking_owners == ["adapter", "evidence", "policy"]' "$summary_json"
jq -e '[.rows[] | select(.classifications | index("ruby_metadata_not_plaintext"))] | length == 1' "$summary_json"
jq -e '[.rows[] | select(.classifications | index("source_note_metadata_excluded"))] | length == 1' "$summary_json"
jq -e '[.rows[] | select(.blocking_owners == ["evidence"])] | length == 1' "$summary_json"
jq -e '[.rows[] | select((.classifications | index("adapter_paragraph_bug")) and (.classifications | index("missing_parser_evidence")))] | length == 1' "$summary_json"

rg -n 'FIVE_PARSER_EVIDENCE_INCOMPLETE' "$report_md"
rg -n 'ruby_metadata_not_plaintext' "$report_md"
rg -n 'source_note_metadata_excluded' "$report_md"
```

- [ ] **Step 2: Make the smoke executable**

Run:

```bash
chmod +x tests/parser-ir-plain-prose-source-delta-smoke.sh
```

- [ ] **Step 3: Run the smoke and verify it fails**

Run:

```bash
bash tests/parser-ir-plain-prose-source-delta-smoke.sh
```

Expected: FAIL because `reports/parser-ir/plain-prose-source-delta.py` does not exist yet.

- [ ] **Step 4: Commit the failing test**

```bash
git add tests/parser-ir-plain-prose-source-delta-smoke.sh
git commit -m "test(parser-ir): add plain prose source delta smoke"
```

---

### Task 3: Implement The Plain-Prose Delta Probe

**Files:**
- Create: `reports/parser-ir/plain-prose-source-delta.py`
- Test: `tests/parser-ir-plain-prose-source-delta-smoke.sh`

**Interfaces:**
- Consumes the CLI from Task 2.
- Produces:
  - `load_json(path: pathlib.Path) -> Any`
  - `build_summary(admission, matrix, structural, source, mapping, allow_missing_parser_evidence) -> dict[str, Any]`
  - `render_markdown(summary: dict[str, Any]) -> str`

- [ ] **Step 1: Create the report generator**

Create `reports/parser-ir/plain-prose-source-delta.py` with this implementation structure:

```python
#!/usr/bin/env python
"""Classify plain-prose Level 3 source deltas from measured matrix evidence."""

from __future__ import annotations

import argparse
import json
import pathlib
from collections import Counter, defaultdict
from typing import Any

SCHEMA_VERSION = "plain-prose-source-delta-probe-v1"
REQUIRED_PARSERS = ("aozora2html", "aozora-epub3", "aozora-rs", "aozora2", "aozora")
PASSING_PARAGRAPH_ORIGINS = {"aligned", "source_note_back_routing"}
ADAPTER_PARAGRAPH_FAILURES = {
    "adapter_over_segmented",
    "adapter_under_segmented",
    "adapter_collapsed",
}
PARSER_IR_FAILURES = {"converter_paragraph_mismatch"}
ABC_RENDERER_FAILURES = {"renderer_paragraph_mismatch"}
RUBY_TEXT_BUCKETS = {
    "ruby_expanded_equal",
    "ruby_expanded_parenless_equal",
    "ruby_expanded_generated_contains_tei_eaj",
    "ruby_expanded_parenless_generated_contains_tei_eaj",
    "ruby_expanded_tei_eaj_contains_generated",
    "ruby_expanded_parenless_tei_eaj_contains_generated",
}
SOURCE_NOTE_TEXT_BUCKETS = {"base_drop_parentheticals_equal"}
PASSING_TEXT_BUCKETS = {"base_equal"}
OWNER_ORDER = {"adapter": 0, "evidence": 1, "policy": 2, "parser_ir": 3, "abc_renderer": 4}


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--admission-summary", required=True, type=pathlib.Path)
    parser.add_argument("--matrix-summary", required=True, type=pathlib.Path)
    parser.add_argument("--structural-summary", required=True, type=pathlib.Path)
    parser.add_argument("--source-summary", required=True, type=pathlib.Path)
    parser.add_argument("--mapping", required=True, type=pathlib.Path)
    parser.add_argument("--summary-json", required=True, type=pathlib.Path)
    parser.add_argument("--report-md", required=True, type=pathlib.Path)
    parser.add_argument("--allow-missing-parser-evidence", action="store_true")
    return parser.parse_args()


def load_json(path: pathlib.Path) -> Any:
    return json.loads(path.read_text(encoding="utf-8"))


def write_json(path: pathlib.Path, value: Any) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(json.dumps(value, ensure_ascii=False, indent=2) + "\n", encoding="utf-8")


def write_text(path: pathlib.Path, value: str) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(value, encoding="utf-8")


def require_mapping(admission: dict[str, Any], mapping: dict[str, Any]) -> dict[str, Any]:
    candidate = admission.get("mapping") if isinstance(admission.get("mapping"), dict) else {}
    fields = (
        "mapping_id",
        "mapping_version",
        "mapping_hash",
        "mapping_schema_hash",
        "target_parser_ir_schema_hash",
        "generated_mapping_rules",
    )
    if all(candidate.get(field) for field in fields):
        return {field: candidate[field] for field in fields}

    rules = mapping.get("transform_rule_descriptions")
    fallback = {
        "mapping_id": mapping.get("mapping_id"),
        "mapping_version": mapping.get("mapping_version"),
        "mapping_hash": candidate.get("mapping_hash"),
        "mapping_schema_hash": mapping.get("mapping_schema_hash"),
        "target_parser_ir_schema_hash": mapping.get("target_parser_ir_schema_hash"),
        "generated_mapping_rules": len(rules) if isinstance(rules, list) else None,
    }
    missing = [field for field in fields if not fallback.get(field)]
    if missing:
        raise SystemExit(f"mapping missing required fields: {', '.join(missing)}")
    return fallback


def row_profile(row: dict[str, Any]) -> str:
    return row.get("tei_eaj", {}).get("structure_profile") or "unknown"


def row_adapter(row: dict[str, Any]) -> str:
    return row.get("selected_aat", {}).get("adapter") or "unknown"


def row_key(row: dict[str, Any]) -> tuple[str, str]:
    return (str(row.get("work_id") or "unknown"), str(row.get("tei_eaj_file") or "unknown"))


def sorted_owners(owners: set[str]) -> list[str]:
    return sorted(owners, key=lambda owner: (OWNER_ORDER.get(owner, 99), owner))


def classify_text(row: dict[str, Any]) -> tuple[list[str], set[str], list[str]]:
    bucket = row.get("text", {}).get("body_text_match_bucket") or "unknown"
    source_note_excluded = bool(row.get("classification", {}).get("source_note_body_excluded"))
    classifications: list[str] = []
    owners: set[str] = set()
    reasons: list[str] = []

    if bucket in PASSING_TEXT_BUCKETS:
        return classifications, owners, reasons
    if source_note_excluded or bucket in SOURCE_NOTE_TEXT_BUCKETS:
        classifications.append("source_note_metadata_excluded")
        owners.add("policy")
        reasons.append("source-note or parenthetical source attribution belongs outside body plaintext")
    elif bucket in RUBY_TEXT_BUCKETS:
        classifications.append("ruby_metadata_not_plaintext")
        owners.add("policy")
        reasons.append("ruby-expanded surfaces are diagnostic only, not plaintext")
    else:
        classifications.append("source_text_policy_required")
        owners.add("policy")
        reasons.append(f"text policy required for {bucket}")
    return classifications, owners, reasons


def classify_row(row: dict[str, Any], missing_for_row: list[str]) -> dict[str, Any]:
    origin = row.get("classification", {}).get("paragraph_origin_bucket") or "unknown"
    text_bucket = row.get("text", {}).get("body_text_match_bucket") or "unknown"
    classifications: list[str] = []
    owners: set[str] = set()
    reasons: list[str] = []

    if missing_for_row:
        classifications.append("missing_parser_evidence")
        owners.add("evidence")
        reasons.append("missing required parser evidence: " + ", ".join(missing_for_row))

    if origin in PASSING_PARAGRAPH_ORIGINS:
        pass
    elif origin in ADAPTER_PARAGRAPH_FAILURES:
        classifications.append("adapter_paragraph_bug")
        owners.add("adapter")
        reasons.append(f"paragraph origin is {origin}")
    elif origin in PARSER_IR_FAILURES:
        classifications.append("parser_ir_projection_bug")
        owners.add("parser_ir")
        reasons.append(f"parser-IR projection mismatch: {origin}")
    elif origin in ABC_RENDERER_FAILURES:
        classifications.append("abc_renderer_bug")
        owners.add("abc_renderer")
        reasons.append(f"ABC renderer mismatch: {origin}")
    else:
        classifications.append("tei_eaj_editorial_segmentation")
        owners.add("policy")
        reasons.append(f"paragraph segmentation requires profile policy: {origin}")

    text_classes, text_owners, text_reasons = classify_text(row)
    classifications.extend(text_classes)
    owners.update(text_owners)
    reasons.extend(text_reasons)

    return {
        "work_id": row.get("work_id"),
        "title": row.get("title"),
        "tei_eaj_file": row.get("tei_eaj_file"),
        "adapter": row_adapter(row),
        "paragraph_origin_bucket": origin,
        "body_text_match_bucket": text_bucket,
        "classifications": classifications,
        "blocking_owners": sorted_owners(owners),
        "evidence": {
            "missing_required_parsers_for_row": missing_for_row,
            "materialization_status": row.get("materialization", {}).get("status"),
        },
        "reasons": reasons,
    }


def parser_evidence_coverage(rows: list[dict[str, Any]]) -> tuple[dict[str, Any], dict[tuple[str, str], list[str]]]:
    observed_rows_by_parser = Counter(row_adapter(row) for row in rows)
    observed_global = set(observed_rows_by_parser)
    missing_global = [parser for parser in REQUIRED_PARSERS if parser not in observed_global]
    adapters_by_row: dict[tuple[str, str], set[str]] = defaultdict(set)
    for row in rows:
        adapters_by_row[row_key(row)].add(row_adapter(row))
    missing_by_row = {
        key: [parser for parser in REQUIRED_PARSERS if parser not in adapters]
        for key, adapters in adapters_by_row.items()
    }
    rows_missing = sum(1 for missing in missing_by_row.values() if missing)
    verdict = "FIVE_PARSER_EVIDENCE_COMPLETE"
    if missing_global or rows_missing:
        verdict = "FIVE_PARSER_EVIDENCE_INCOMPLETE"
    return (
        {
            "verdict": verdict,
            "observed_rows_by_parser": dict(sorted(observed_rows_by_parser.items())),
            "missing_parsers": missing_global,
            "rows_missing_required_parser_evidence": rows_missing,
        },
        missing_by_row,
    )


def build_summary(
    admission: dict[str, Any],
    matrix: dict[str, Any],
    structural: dict[str, Any],
    source: dict[str, Any],
    mapping: dict[str, Any],
    allow_missing_parser_evidence: bool,
) -> dict[str, Any]:
    del structural
    plain_rows = [row for row in matrix.get("rows", []) if row_profile(row) == "plain_prose"]
    coverage, missing_by_row = parser_evidence_coverage(plain_rows)
    if coverage["verdict"] != "FIVE_PARSER_EVIDENCE_COMPLETE" and not allow_missing_parser_evidence:
        raise SystemExit("missing parser evidence; rerun with --allow-missing-parser-evidence for exploratory report")
    classified = [classify_row(row, missing_by_row.get(row_key(row), [])) for row in plain_rows]
    classification_counts = Counter(
        classification for row in classified for classification in row["classifications"]
    )
    owner_counts = Counter(owner for row in classified for owner in row["blocking_owners"])
    works = {row_key(row) for row in plain_rows}
    return {
        "schema_version": SCHEMA_VERSION,
        "required_parsers": list(REQUIRED_PARSERS),
        "parser_evidence_coverage": coverage,
        "source_authority_gate": admission.get("source_authority_gate") or source,
        "mapping": require_mapping(admission, mapping),
        "plain_prose_scope": {
            "rows_total": len(plain_rows),
            "works_total": len(works),
        },
        "classification_counts": dict(sorted(classification_counts.items())),
        "blocking_owners": sorted_owners(set(owner_counts)),
        "blocking_owner_counts": dict(sorted(owner_counts.items())),
        "rows": classified,
    }


def render_markdown(summary: dict[str, Any]) -> str:
    lines = [
        "# Plain Prose Source Delta Probe",
        "",
        "This report diagnoses plain-prose Level 3 blockers. It is not a Level 3 admission decision.",
        "",
        "## Verdict",
        "",
        f"- parser_evidence_coverage: `{summary['parser_evidence_coverage']['verdict']}`",
        f"- blocking_owners: `{', '.join(summary['blocking_owners']) or 'none'}`",
        f"- mapping_id: `{summary['mapping']['mapping_id']}`",
        f"- mapping_version: `{summary['mapping']['mapping_version']}`",
        "",
        "## Required Parsers",
        "",
    ]
    for parser in summary["required_parsers"]:
        count = summary["parser_evidence_coverage"]["observed_rows_by_parser"].get(parser, 0)
        lines.append(f"- `{parser}`: {count} rows")
    lines.extend(["", "## Classification Counts", "", "| classification | rows |", "|---|---:|"])
    for name, count in summary["classification_counts"].items():
        lines.append(f"| {name} | {count} |")
    lines.extend(["", "## Rows", "", "| work_id | adapter | classifications | owners | file |", "|---|---|---|---|---|"])
    for row in summary["rows"]:
        lines.append(
            "| {work_id} | {adapter} | {classes} | {owners} | `{file}` |".format(
                work_id=row.get("work_id"),
                adapter=row.get("adapter"),
                classes=", ".join(row["classifications"]) or "none",
                owners=", ".join(row["blocking_owners"]) or "none",
                file=row.get("tei_eaj_file"),
            )
        )
    lines.append("")
    return "\n".join(lines)


def main() -> int:
    args = parse_args()
    admission = load_json(args.admission_summary)
    matrix = load_json(args.matrix_summary)
    structural = load_json(args.structural_summary)
    source = load_json(args.source_summary)
    mapping = load_json(args.mapping)
    summary = build_summary(
        admission,
        matrix,
        structural,
        source,
        mapping,
        args.allow_missing_parser_evidence,
    )
    write_json(args.summary_json, summary)
    write_text(args.report_md, render_markdown(summary))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
```

- [ ] **Step 2: Run the smoke**

Run:

```bash
bash tests/parser-ir-plain-prose-source-delta-smoke.sh
```

Expected: PASS.

- [ ] **Step 3: Run Python syntax check**

Run:

```bash
python -m py_compile reports/parser-ir/plain-prose-source-delta.py
```

Expected: no output.

- [ ] **Step 4: Commit**

```bash
git add reports/parser-ir/plain-prose-source-delta.py tests/parser-ir-plain-prose-source-delta-smoke.sh
git commit -m "feat(parser-ir): classify plain prose source deltas"
```

---

### Task 4: Wire Just And Flake Checks

**Files:**
- Modify: `justfile`
- Modify: `flake.nix`
- Test: `tests/parser-ir-plain-prose-source-delta-smoke.sh`

**Interfaces:**
- Produces `just parser-ir-plain-prose-source-delta-smoke`.
- Produces `just parser-ir-plain-prose-source-delta-report`.
- Produces flake check `checks.<system>.parser-ir-plain-prose-source-delta-smoke`.

- [ ] **Step 1: Add just targets**

In `justfile`, add these recipes after `parser-ir-level3-admission-report`:

```make
parser-ir-plain-prose-source-delta-smoke:
	@bash "{{repo_root}}/tests/parser-ir-plain-prose-source-delta-smoke.sh"

parser-ir-plain-prose-source-delta-report ADMISSION_SUMMARY="docs/superpowers/reports/2026-07-05-profile-aware-level3-tei-admission.summary.json" MATRIX_SUMMARY="docs/superpowers/reports/2026-07-04-tei-eaj-generated-matrix-comparison.summary.json" STRUCTURAL_SUMMARY="docs/superpowers/reports/2026-07-04-tei-eaj-structural-expansion.summary.json" SOURCE_SUMMARY="docs/superpowers/reports/2026-07-04-source-authority-representability.summary.json" MAPPING="data/aat-to-parser-ir-mapping-v1.json" REPORT_MD="docs/superpowers/reports/2026-07-05-plain-prose-source-delta.md" SUMMARY_JSON="docs/superpowers/reports/2026-07-05-plain-prose-source-delta.summary.json":
	@python "{{repo_root}}/reports/parser-ir/plain-prose-source-delta.py" \
		--admission-summary "{{repo_root}}/{{ADMISSION_SUMMARY}}" \
		--matrix-summary "{{repo_root}}/{{MATRIX_SUMMARY}}" \
		--structural-summary "{{repo_root}}/{{STRUCTURAL_SUMMARY}}" \
		--source-summary "{{repo_root}}/{{SOURCE_SUMMARY}}" \
		--mapping "{{repo_root}}/{{MAPPING}}" \
		--summary-json "{{repo_root}}/{{SUMMARY_JSON}}" \
		--report-md "{{repo_root}}/{{REPORT_MD}}" \
		--allow-missing-parser-evidence
```

- [ ] **Step 2: Add the flake smoke derivation**

In `flake.nix`, add this derivation near `level3AdmissionSmokeCheck`:

```nix
        plainProseSourceDeltaSmokeCheck =
          pkgs.runCommand "parser-ir-plain-prose-source-delta-smoke-check"
            {
              nativeBuildInputs = [
                pkgs.bash
                pkgs.jq
                pkgs.python3
                pkgs.ripgrep
              ];
            }
            ''
              work_dir="$(mktemp -d)"
              cp -R "${source}" "$work_dir/source"
              chmod -R +w "$work_dir/source"
              cd "$work_dir/source"

              export TMPDIR="$work_dir/tmp"
              mkdir -p "$TMPDIR"
              export HOME="$work_dir/home"
              mkdir -p "$HOME"

              bash tests/parser-ir-plain-prose-source-delta-smoke.sh
              touch "$out"
            '';
```

- [ ] **Step 3: Expose the check**

In the `checks = { ... }` block, add:

```nix
          parser-ir-plain-prose-source-delta-smoke = plainProseSourceDeltaSmokeCheck;
```

- [ ] **Step 4: Run just smoke**

Run:

```bash
just parser-ir-plain-prose-source-delta-smoke
```

Expected: PASS.

- [ ] **Step 5: Run flake smoke**

Run:

```bash
system="$(nix eval --impure --raw --expr builtins.currentSystem)"
nix build ".#checks.$system.parser-ir-plain-prose-source-delta-smoke" --print-build-logs
```

Expected: PASS.

- [ ] **Step 6: Commit**

```bash
git add justfile flake.nix
git commit -m "build(parser-ir): gate plain prose source delta smoke"
```

---

### Task 5: Generate And Commit The Diagnostic Report

**Files:**
- Create: `docs/superpowers/reports/2026-07-05-plain-prose-source-delta.md`
- Create: `docs/superpowers/reports/2026-07-05-plain-prose-source-delta.summary.json`

**Interfaces:**
- Consumes `just parser-ir-plain-prose-source-delta-report`.
- Produces committed evidence that explicitly marks current five-parser coverage as incomplete if `aozora2` or `aozora` rows are still missing.

- [ ] **Step 1: Generate the report**

Run:

```bash
just parser-ir-plain-prose-source-delta-report
```

Expected: generated Markdown and JSON under `docs/superpowers/reports/`.

- [ ] **Step 2: Assert five parser contract and current evidence verdict**

Run:

```bash
jq -e '.required_parsers == ["aozora2html", "aozora-epub3", "aozora-rs", "aozora2", "aozora"]' docs/superpowers/reports/2026-07-05-plain-prose-source-delta.summary.json
jq -e '.mapping.mapping_id != null' docs/superpowers/reports/2026-07-05-plain-prose-source-delta.summary.json
jq -e '.parser_evidence_coverage.verdict == "FIVE_PARSER_EVIDENCE_INCOMPLETE" or .parser_evidence_coverage.verdict == "FIVE_PARSER_EVIDENCE_COMPLETE"' docs/superpowers/reports/2026-07-05-plain-prose-source-delta.summary.json
jq -e '.classification_counts | has("missing_parser_evidence")' docs/superpowers/reports/2026-07-05-plain-prose-source-delta.summary.json
rg -n 'FIVE_PARSER_EVIDENCE_' docs/superpowers/reports/2026-07-05-plain-prose-source-delta.md
```

Expected: all commands pass.

- [ ] **Step 3: Commit**

```bash
git add docs/superpowers/reports/2026-07-05-plain-prose-source-delta.md docs/superpowers/reports/2026-07-05-plain-prose-source-delta.summary.json
git commit -m "docs(parser-ir): report plain prose source deltas"
```

---

### Task 6: Final Verification

**Files:**
- Verify all files touched in Tasks 1-5.

**Interfaces:**
- Consumes all previous task outputs.
- Produces a clean final state for review or merge.

- [ ] **Step 1: Run smoke and report**

Run:

```bash
just parser-ir-plain-prose-source-delta-smoke
just parser-ir-plain-prose-source-delta-report
git diff --exit-code -- docs/superpowers/reports/2026-07-05-plain-prose-source-delta.md docs/superpowers/reports/2026-07-05-plain-prose-source-delta.summary.json
```

Expected: smoke passes and generated reports are reproducible.

- [ ] **Step 2: Run flake check**

Run:

```bash
system="$(nix eval --impure --raw --expr builtins.currentSystem)"
nix build ".#checks.$system.parser-ir-plain-prose-source-delta-smoke" --print-build-logs
```

Expected: PASS.

- [ ] **Step 3: Run syntax and whitespace checks**

Run:

```bash
python -m py_compile reports/parser-ir/plain-prose-source-delta.py reports/parser-ir/tei-eaj-generated-compare.py
git diff --check
```

Expected: no output.

- [ ] **Step 4: Check git status**

Run:

```bash
git status --short --branch
```

Expected: only pre-existing unrelated untracked files remain, if any.

---

## Self-Review

Spec coverage:

- Five-parser required labels: Tasks 1, 2, 3, 4, and 5.
- Source authority and plaintext rules: Tasks 2 and 3.
- Mapping cannot be empty: Tasks 2 and 3.
- Multi-owner classifications: Tasks 2 and 3.
- Fixture smoke without `/db`: Task 2.
- Just and flake gates: Task 4.
- Committed diagnostic evidence: Task 5.

Type consistency:

- CLI option names are consistent across smoke, script, and just target.
- JSON fields match the spec: `required_parsers`, `parser_evidence_coverage`, `source_authority_gate`, `mapping`, `plain_prose_scope`, `classification_counts`, `blocking_owners`, and `rows`.
- Classification names match the design spec exactly.

Open sequencing note:

- The generated report may still be `FIVE_PARSER_EVIDENCE_INCOMPLETE` until the current matrix summary is regenerated with complete `aozora2` and `aozora` rows. That is expected and must remain visible in the report.
