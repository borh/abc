# Profile-Aware Level 3 TEI Admission Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Build a profile-aware Level 3 TEI admission report that separates parser-IR infrastructure readiness from plain-prose adapter admission and routes drama, verse, notes, front/back matter, and Level 4 enrichment into policy lanes.

**Architecture:** Add a deterministic Python classifier over the existing generated-TEI matrix summary, source-authority summary, and the mapping file referenced by the matrix input. Do not rematerialize TEI in this gate; consume already-measured evidence, emit a new JSON/Markdown admission report, and wire a fixture smoke plus just/Nix gates.

**Tech Stack:** Python 3 standard library, Bash smoke tests, `jq`, existing `justfile`, existing `flake.nix` runCommand checks, existing reports under `docs/superpowers/reports/`.

## Global Constraints

- TEI-EAJ is comparison evidence; source inventory remains the authority for Aozora source constructs.
- First hard gate is `plain_prose`.
- `plain_prose` admission requires parser-IR `paragraphs[]`, source-note routing, materialization success, acceptable paragraph origin, and declared text policy.
- Plaintext admission uses body base text only. Ruby readings, source notes, provenance, and other metadata are excluded from plaintext.
- Only `base_equal` text passes automatically in v1. Ruby-expanded surfaces are diagnostic only; parenthetical-normalized matches remain policy-blocked unless the dropped text is typed metadata excluded from plaintext.
- `aligned` and `source_note_back_routing` pass paragraph-origin policy. `page_break_projection` is tracked separately and does not pass plain-prose admission in v1.
- `adapter_over_segmented`, `adapter_under_segmented`, and `adapter_collapsed` fail adapter admission.
- Drama, verse, notes, and front/back matter are `LANE_POLICY_REQUIRED`.
- Level 4 enrichment is `LANE_OUT_OF_SCOPE_FOR_LEVEL3` unless source markup explicitly encodes the fact in a future policy.
- Do not touch the unrelated untracked `docs/superpowers/specs/2026-07-05-interestingness-ranking-design.md`.

---

## File Structure

- Create `reports/parser-ir/level3-admission.py`
  - Pure report classifier. Consumes generated matrix JSON plus source-authority summary JSON, loads `inputs.mapping`, computes the mapping document hash, and emits admission JSON and Markdown.
- Create `tests/parser-ir-level3-admission-smoke.sh`
  - Fixture-only smoke. Writes tiny source/matrix summaries to a temp dir and asserts gate behavior with `jq`/`rg`.
- Modify `justfile`
  - Add `parser-ir-level3-admission-smoke`.
  - Add `parser-ir-level3-admission-report`.
- Modify `flake.nix`
  - Add `level3AdmissionSmokeCheck`.
  - Expose it as `checks.<system>.parser-ir-level3-admission-smoke`.
- Generate `docs/superpowers/reports/2026-07-05-profile-aware-level3-tei-admission.md`
  - Committed operator report from current matrix/source evidence.
- Generate `docs/superpowers/reports/2026-07-05-profile-aware-level3-tei-admission.summary.json`
  - Machine-readable admission summary.
- Modify `docs/superpowers/reports/2026-07-04-post-parser-ir-conversion-sync.md`
  - Replace stale source-authority caveat with the current pass and point to the new admission report.
- Modify `docs/superpowers/reports/2026-07-04-tei-eaj-structural-gap-analysis.md`
  - Replace stale blocker #1 and refer to the new profile-aware gate.

---

### Task 1: Add Profile-Aware Admission Classifier And Fixture Smoke

**Files:**
- Create: `reports/parser-ir/level3-admission.py`
- Create: `tests/parser-ir-level3-admission-smoke.sh`

**Interfaces:**
- Consumes:
  - `--matrix-summary <path>`: JSON shaped like `docs/superpowers/reports/2026-07-04-tei-eaj-generated-matrix-comparison.summary.json`.
  - `--source-summary <path>`: JSON shaped like `docs/superpowers/reports/2026-07-04-source-authority-representability.summary.json`.
  - Matrix row fields:
    - `tei_eaj.structure_profile: str`
    - `classification.paragraph_origin_bucket: str`
    - `text.body_text_match_bucket: str`
    - `materialization.status: str`
    - `selected_aat.adapter: str`
    - `tei_eaj_file: str`
    - `work_id: str | null`
    - `title: str | null`
  - Matrix top-level fields:
    - `totals.rows_attempted`
    - `totals.materialization_succeeded`
    - `totals.materialization_failed`
    - `inputs.mapping`
    - `skipped[]`
- Produces:
  - CLI: `python reports/parser-ir/level3-admission.py --matrix-summary MATRIX --source-summary SOURCE --summary-json OUT.json --report-md OUT.md`
  - Function: `build_summary(matrix: dict[str, Any], source: dict[str, Any], mapping: dict[str, Any], mapping_hash: str) -> dict[str, Any]`
  - JSON top-level fields from the spec: `schema_version`, `source_authority_gate`, `parser_ir_infrastructure_verdict`, `plain_prose_admission`, `profile_lanes`, `evidence_gaps`, `mapping`, `inputs`.
  - `mapping` must include `mapping_id`, `mapping_version`, `mapping_hash`, `mapping_schema_hash`, `target_parser_ir_schema_id`, `target_parser_ir_schema_hash`, and `generated_mapping_rules`.

- [ ] **Step 1: Write the failing smoke test**

Create `tests/parser-ir-level3-admission-smoke.sh`:

```bash
#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
out_dir="$(mktemp -d "${TMPDIR:-/tmp}/ab-level3-admission-smoke.XXXXXX")"
trap 'rm -rf "$out_dir"' EXIT

source_summary="$out_dir/source-summary.json"
failing_source_summary="$out_dir/source-summary-failing.json"
matrix_summary="$out_dir/matrix-summary.json"
mapping_file="$out_dir/mapping.json"
summary_json="$out_dir/admission.summary.json"
report_md="$out_dir/admission.md"
failing_summary_json="$out_dir/admission-source-fail.summary.json"
failing_report_md="$out_dir/admission-source-fail.md"

cat > "$source_summary" <<'JSON'
{
  "gate_status": "SOURCE_AUTHORITY_GATE_PASS",
  "works_scanned": 17894,
  "unallowlisted_unknown_markers_total": 0
}
JSON

cat > "$failing_source_summary" <<'JSON'
{
  "gate_status": "SOURCE_AUTHORITY_GATE_FAILING_REVIEW_REQUIRED",
  "works_scanned": 17894,
  "unallowlisted_unknown_markers_total": 2
}
JSON

cat > "$mapping_file" <<'JSON'
{
  "mapping_id": "https://w3id.org/abc/mappings/aat-v1-to-parser-ir-v1/generated-probe",
  "mapping_version": "0.2.3",
  "mapping_schema_hash": "sha256:38ec7f0e5affb10329b550a091cd3a6fb5a25e26fd469dfe9f8249970cf9adb4",
  "source_aat_version": 1,
  "target_parser_ir_schema_id": "https://w3id.org/abc/schemas/parser-ir.schema.json",
  "target_parser_ir_schema_hash": "sha256:8e56871965e647e40ade08fd9dd580a3516d33905be17957cc79750bd42ea64d",
  "transform_rule_descriptions": [
    {
      "rule_id": "A-01",
      "category": "AMBIGUITY",
      "aat_pointer": "blocks[].content[].ruby.direction",
      "parser_ir_pointer": "ruby.placement",
      "action": "project",
      "description": "fixture rule"
    }
  ],
  "loss_taxonomy": {
    "AMBIGUITY": {"description": "fixture", "default_action": "drop-sidecar", "records_sidecar": true}
  }
}
JSON

cat > "$matrix_summary" <<JSON
{
  "schema_version": "tei-eaj-generated-comparison-v1",
  "inputs": {
    "candidate_mode": "all",
    "structural_summary": "fixture-structural-summary.json",
    "mapping": "$mapping_file"
  },
  "totals": {
    "rows_attempted": 8,
    "tei_eaj_rows_attempted": 7,
    "materialization_succeeded": 8,
    "materialization_failed": 0,
    "rows_skipped": 1
  },
  "skipped": [
    {"tei_eaj_file": "data/etc/no-work-id.xml", "reason": "no_materializable_aat"}
  ],
  "rows": [
    {
      "work_id": "plain-pass",
      "title": "Plain pass",
      "tei_eaj_file": "data/plain-pass.xml",
      "selected_aat": {"adapter": "aozora2html", "adapter_version": "fixture"},
      "materialization": {"status": "passed"},
      "tei_eaj": {"structure_profile": "plain_prose"},
      "classification": {"paragraph_origin_bucket": "aligned"},
      "text": {"body_text_match_bucket": "base_equal"}
    },
    {
      "work_id": "plain-note",
      "title": "Plain source note",
      "tei_eaj_file": "data/plain-note.xml",
      "selected_aat": {"adapter": "aozora2html", "adapter_version": "fixture"},
      "materialization": {"status": "passed"},
      "tei_eaj": {"structure_profile": "plain_prose"},
      "classification": {"paragraph_origin_bucket": "source_note_back_routing"},
      "text": {"body_text_match_bucket": "base_equal"}
    },
    {
      "work_id": "plain-over",
      "title": "Plain over segmented",
      "tei_eaj_file": "data/plain-over.xml",
      "selected_aat": {"adapter": "aozora-rs", "adapter_version": "fixture"},
      "materialization": {"status": "passed"},
      "tei_eaj": {"structure_profile": "plain_prose"},
      "classification": {"paragraph_origin_bucket": "adapter_over_segmented"},
      "text": {"body_text_match_bucket": "base_equal"}
    },
    {
      "work_id": "plain-text-policy",
      "title": "Plain text policy",
      "tei_eaj_file": "data/plain-text-policy.xml",
      "selected_aat": {"adapter": "aozora-epub3", "adapter_version": "fixture"},
      "materialization": {"status": "passed"},
      "tei_eaj": {"structure_profile": "plain_prose"},
      "classification": {"paragraph_origin_bucket": "aligned"},
      "text": {"body_text_match_bucket": "ruby_expanded_equal"}
    },
    {
      "work_id": "plain-page-break",
      "title": "Plain page break",
      "tei_eaj_file": "data/plain-page-break.xml",
      "selected_aat": {"adapter": "aozora2html", "adapter_version": "fixture"},
      "materialization": {"status": "passed"},
      "tei_eaj": {"structure_profile": "plain_prose"},
      "classification": {"paragraph_origin_bucket": "page_break_projection"},
      "text": {"body_text_match_bucket": "base_equal"}
    },
    {
      "work_id": "drama",
      "title": "Drama",
      "tei_eaj_file": "data/drama.xml",
      "selected_aat": {"adapter": "aozora2html", "adapter_version": "fixture"},
      "materialization": {"status": "passed"},
      "tei_eaj": {"structure_profile": "drama"},
      "classification": {"paragraph_origin_bucket": "adapter_under_segmented"},
      "text": {"body_text_match_bucket": "different"}
    },
    {
      "work_id": "verse",
      "title": "Verse",
      "tei_eaj_file": "data/verse.xml",
      "selected_aat": {"adapter": "aozora2html", "adapter_version": "fixture"},
      "materialization": {"status": "passed"},
      "tei_eaj": {"structure_profile": "verse"},
      "classification": {"paragraph_origin_bucket": "adapter_over_segmented"},
      "text": {"body_text_match_bucket": "base_equal"}
    },
    {
      "work_id": "lv4",
      "title": "Level 4 enrichment",
      "tei_eaj_file": "data/lv4.xml",
      "selected_aat": {"adapter": "aozora2html", "adapter_version": "fixture"},
      "materialization": {"status": "passed"},
      "tei_eaj": {"structure_profile": "lv4_enrichment"},
      "classification": {"paragraph_origin_bucket": "aligned"},
      "text": {"body_text_match_bucket": "base_equal"}
    }
  ]
}
JSON

python "$repo_root/reports/parser-ir/level3-admission.py" \
  --matrix-summary "$matrix_summary" \
  --source-summary "$source_summary" \
  --summary-json "$summary_json" \
  --report-md "$report_md"

jq -e '.schema_version == "profile-aware-level3-tei-admission-v1"' "$summary_json"
jq -e '.source_authority_gate.gate_status == "SOURCE_AUTHORITY_GATE_PASS"' "$summary_json"
jq -e '.parser_ir_infrastructure_verdict == "LEVEL3_IR_INFRASTRUCTURE_READY"' "$summary_json"
jq -e '.mapping.mapping_id == "https://w3id.org/abc/mappings/aat-v1-to-parser-ir-v1/generated-probe"' "$summary_json"
jq -e '.mapping.mapping_version == "0.2.3"' "$summary_json"
jq -e '.mapping.mapping_hash | test("^sha256:[0-9a-f]{64}$")' "$summary_json"
jq -e '.mapping.generated_mapping_rules == 1' "$summary_json"
jq -e '.plain_prose_admission.rows_total == 5' "$summary_json"
jq -e '.plain_prose_admission.rows_passed == 2' "$summary_json"
jq -e '.plain_prose_admission.rows_failed == 3' "$summary_json"
jq -e '.plain_prose_admission.verdict == "LEVEL3_PLAIN_PROSE_BLOCKED_ADAPTER_FIDELITY_AND_TEXT_POLICY"' "$summary_json"
jq -e '.plain_prose_admission.blocking_owners == ["adapter", "policy"]' "$summary_json"
jq -e '.plain_prose_admission.failures_by_owner.adapter == 1' "$summary_json"
jq -e '.plain_prose_admission.failures_by_owner.policy == 2' "$summary_json"
jq -e '.plain_prose_admission.failures_by_adapter["aozora-rs"] == 1' "$summary_json"
jq -e '.plain_prose_admission.paragraph_origin_buckets.source_note_back_routing == 1' "$summary_json"
jq -e '.plain_prose_admission.paragraph_origin_buckets.page_break_projection == 1' "$summary_json"
jq -e '.plain_prose_admission.plaintext_policy.plaintext_surface == "body_base_text"' "$summary_json"
jq -e '.plain_prose_admission.plaintext_policy.ruby_expanded_surfaces == "diagnostic_only"' "$summary_json"
jq -e '.plain_prose_admission.plaintext_policy.metadata_policy == "exclude_typed_metadata_from_plaintext"' "$summary_json"
jq -e '.profile_lanes.drama.verdict == "LANE_POLICY_REQUIRED"' "$summary_json"
jq -e '.profile_lanes.verse.verdict == "LANE_POLICY_REQUIRED"' "$summary_json"
jq -e '.profile_lanes.lv4_enrichment.verdict == "LANE_OUT_OF_SCOPE_FOR_LEVEL3"' "$summary_json"
jq -e '.evidence_gaps.rows == 1' "$summary_json"
rg -n 'LEVEL3_PLAIN_PROSE_BLOCKED_ADAPTER_FIDELITY_AND_TEXT_POLICY' "$report_md"
rg -n 'Drama' "$report_md"
rg -n 'Level 4 enrichment' "$report_md"

python "$repo_root/reports/parser-ir/level3-admission.py" \
  --matrix-summary "$matrix_summary" \
  --source-summary "$failing_source_summary" \
  --summary-json "$failing_summary_json" \
  --report-md "$failing_report_md"

jq -e '.source_authority_gate.gate_status == "SOURCE_AUTHORITY_GATE_FAILING_REVIEW_REQUIRED"' "$failing_summary_json"
jq -e '.plain_prose_admission.rows_total == 5' "$failing_summary_json"
jq -e '.plain_prose_admission.rows_passed == 0' "$failing_summary_json"
jq -e '.plain_prose_admission.rows_failed == 5' "$failing_summary_json"
jq -e '.plain_prose_admission.verdict == "LEVEL3_PLAIN_PROSE_BLOCKED_ADAPTER_FIDELITY_AND_TEXT_POLICY_AND_EVIDENCE"' "$failing_summary_json"
jq -e '.plain_prose_admission.blocking_owners == ["adapter", "policy", "evidence"]' "$failing_summary_json"
jq -e '.plain_prose_admission.failures_by_owner.evidence == 5' "$failing_summary_json"
```

Make it executable:

```bash
chmod +x tests/parser-ir-level3-admission-smoke.sh
```

- [ ] **Step 2: Run smoke to verify it fails**

Run:

```bash
tests/parser-ir-level3-admission-smoke.sh
```

Expected: FAIL with:

```text
python: can't open file '.../reports/parser-ir/level3-admission.py'
```

- [ ] **Step 3: Implement the classifier**

Create `reports/parser-ir/level3-admission.py`:

```python
#!/usr/bin/env python
"""Classify profile-aware Level 3 TEI admission from measured reports."""

from __future__ import annotations

import argparse
import json
import pathlib
import hashlib
from collections import Counter
from typing import Any

SCHEMA_VERSION = "profile-aware-level3-tei-admission-v1"
PASSING_PARAGRAPH_ORIGINS = {"aligned", "source_note_back_routing"}
ADAPTER_PARAGRAPH_FAILURES = {
    "adapter_over_segmented",
    "adapter_under_segmented",
    "adapter_collapsed",
}
POLICY_PARAGRAPH_FAILURES = {"page_break_projection", "unknown"}
PARSER_IR_FAILURES = {"converter_paragraph_mismatch"}
ABC_RENDERER_FAILURES = {"renderer_paragraph_mismatch"}
PASSING_TEXT_BUCKETS = {"base_equal"}
POLICY_TEXT_BUCKETS = {
    "ruby_expanded_equal",
    "ruby_expanded_parenless_equal",
    "base_drop_parentheticals_equal",
    "ruby_expanded_parenless_generated_contains_tei_eaj",
    "ruby_expanded_generated_contains_tei_eaj",
    "base_drop_parentheticals_generated_contains_tei_eaj",
    "base_generated_contains_tei_eaj",
    "ruby_expanded_parenless_tei_eaj_contains_generated",
    "ruby_expanded_tei_eaj_contains_generated",
    "base_drop_parentheticals_tei_eaj_contains_generated",
    "base_tei_eaj_contains_generated",
    "different",
}
PLAINTEXT_POLICY = {
    "plaintext_surface": "body_base_text",
    "ruby_expanded_surfaces": "diagnostic_only",
    "metadata_policy": "exclude_typed_metadata_from_plaintext",
}
LANE_POLICY_REQUIRED = {"drama", "verse", "notes", "front_back_matter"}
LANE_OUT_OF_SCOPE = {"lv4_enrichment"}


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--matrix-summary", required=True, type=pathlib.Path)
    parser.add_argument("--source-summary", required=True, type=pathlib.Path)
    parser.add_argument("--summary-json", required=True, type=pathlib.Path)
    parser.add_argument("--report-md", required=True, type=pathlib.Path)
    return parser.parse_args()


def load_json(path: pathlib.Path) -> Any:
    return json.loads(path.read_text(encoding="utf-8"))


def canonical_json(value: object) -> str:
    return json.dumps(
        value,
        ensure_ascii=False,
        sort_keys=True,
        separators=(",", ":"),
    ).replace("/", "\\/")


def document_hash(value: object) -> str:
    return "sha256:" + hashlib.sha256(canonical_json(value).encode("utf-8")).hexdigest()


def resolve_path(raw: str, base_dir: pathlib.Path) -> pathlib.Path:
    path = pathlib.Path(raw)
    if path.is_absolute():
        return path
    return (base_dir / path).resolve()


def write_json(path: pathlib.Path, value: Any) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(json.dumps(value, ensure_ascii=False, indent=2) + "\n", encoding="utf-8")


def write_text(path: pathlib.Path, value: str) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(value, encoding="utf-8")


def source_authority_gate(source: dict[str, Any]) -> dict[str, Any]:
    return {
        "gate_status": source.get("gate_status"),
        "works_scanned": source.get("works_scanned"),
        "unallowlisted_unknown_markers_total": source.get(
            "unallowlisted_unknown_markers_total"
        ),
    }


def source_authority_passed(source: dict[str, Any]) -> bool:
    return (
        source.get("gate_status") == "SOURCE_AUTHORITY_GATE_PASS"
        and source.get("unallowlisted_unknown_markers_total") == 0
    )


def build_mapping_block(mapping: dict[str, Any], mapping_hash: str) -> dict[str, Any]:
    return {
        "mapping_id": mapping.get("mapping_id"),
        "mapping_version": mapping.get("mapping_version"),
        "mapping_hash": mapping_hash,
        "mapping_schema_hash": mapping.get("mapping_schema_hash"),
        "target_parser_ir_schema_id": mapping.get("target_parser_ir_schema_id"),
        "target_parser_ir_schema_hash": mapping.get("target_parser_ir_schema_hash"),
        "generated_mapping_rules": len(mapping.get("transform_rule_descriptions", [])),
    }


def parser_ir_infrastructure_verdict(matrix: dict[str, Any]) -> str:
    totals = matrix.get("totals", {})
    if totals.get("rows_attempted", 0) and totals.get("materialization_failed", 0) == 0:
        return "LEVEL3_IR_INFRASTRUCTURE_READY"
    return "LEVEL3_IR_INFRASTRUCTURE_BLOCKED"


def row_adapter(row: dict[str, Any]) -> str:
    return row.get("selected_aat", {}).get("adapter") or "unknown"


def row_profile(row: dict[str, Any]) -> str:
    return row.get("tei_eaj", {}).get("structure_profile") or "unknown"


def row_paragraph_origin(row: dict[str, Any]) -> str:
    return row.get("classification", {}).get("paragraph_origin_bucket") or "unknown"


def row_text_bucket(row: dict[str, Any]) -> str:
    return row.get("text", {}).get("body_text_match_bucket") or "unknown"


def row_materialization_ok(row: dict[str, Any]) -> bool:
    return row.get("materialization", {}).get("status") == "passed"


def classify_plain_prose_row(row: dict[str, Any], source_passed: bool) -> dict[str, Any]:
    owners: set[str] = set()
    reasons: list[str] = []
    origin = row_paragraph_origin(row)
    text_bucket = row_text_bucket(row)

    if not source_passed:
        owners.add("evidence")
        reasons.append("source authority gate did not pass")

    if not row_materialization_ok(row):
        owners.add("abc_renderer")
        reasons.append("materialization did not pass")

    if origin in PASSING_PARAGRAPH_ORIGINS:
        pass
    elif origin in ADAPTER_PARAGRAPH_FAILURES:
        owners.add("adapter")
        reasons.append(f"paragraph origin is {origin}")
    elif origin in POLICY_PARAGRAPH_FAILURES:
        owners.add("policy")
        reasons.append(f"paragraph origin requires policy: {origin}")
    elif origin in PARSER_IR_FAILURES:
        owners.add("parser_ir")
        reasons.append(f"parser-IR paragraph mismatch: {origin}")
    elif origin in ABC_RENDERER_FAILURES:
        owners.add("abc_renderer")
        reasons.append(f"ABC renderer paragraph mismatch: {origin}")
    else:
        owners.add("evidence")
        reasons.append(f"unknown paragraph origin: {origin}")

    if text_bucket in PASSING_TEXT_BUCKETS:
        pass
    elif text_bucket in POLICY_TEXT_BUCKETS:
        owners.add("policy")
        reasons.append(f"text policy required for {text_bucket}")
    else:
        owners.add("evidence")
        reasons.append(f"unknown text bucket: {text_bucket}")

    passed = not owners
    return {
        "work_id": row.get("work_id"),
        "title": row.get("title"),
        "tei_eaj_file": row.get("tei_eaj_file"),
        "adapter": row_adapter(row),
        "paragraph_origin_bucket": origin,
        "body_text_match_bucket": text_bucket,
        "passed": passed,
        "owners": sorted(owners),
        "reasons": reasons,
    }


def verdict_from_failures(
    rows_total: int,
    rows_failed: int,
    failures_by_owner: Counter[str],
) -> str:
    if rows_total == 0:
        return "LEVEL3_PLAIN_PROSE_NOT_EVALUATED"
    if rows_failed == 0:
        return "LEVEL3_PLAIN_PROSE_ADMITTED"
    blocking_owners = sorted(failures_by_owner, key=owner_sort_key)
    if blocking_owners == ["adapter"]:
        return "LEVEL3_PLAIN_PROSE_BLOCKED_ADAPTER_FIDELITY"
    if blocking_owners == ["policy"]:
        return "LEVEL3_PLAIN_PROSE_BLOCKED_TEXT_POLICY"
    if blocking_owners == ["evidence"]:
        return "LEVEL3_PLAIN_PROSE_BLOCKED_EVIDENCE"
    labels = "_AND_".join(OWNER_VERDICT_LABELS[owner] for owner in blocking_owners)
    return f"LEVEL3_PLAIN_PROSE_BLOCKED_{labels}"


OWNER_ORDER = {
    "adapter": 0,
    "policy": 1,
    "evidence": 2,
    "parser_ir": 3,
    "abc_renderer": 4,
}
OWNER_VERDICT_LABELS = {
    "adapter": "ADAPTER_FIDELITY",
    "policy": "TEXT_POLICY",
    "evidence": "EVIDENCE",
    "parser_ir": "PARSER_IR",
    "abc_renderer": "ABC_RENDERER",
}


def owner_sort_key(owner: str) -> tuple[int, str]:
    return (OWNER_ORDER.get(owner, 99), owner)


def build_plain_prose_admission(
    rows: list[dict[str, Any]],
    source_passed: bool,
) -> dict[str, Any]:
    classified = [
        classify_plain_prose_row(row, source_passed)
        for row in rows
        if row_profile(row) == "plain_prose"
    ]
    failures_by_owner: Counter[str] = Counter()
    failures_by_adapter: Counter[str] = Counter()
    text_policy_buckets: Counter[str] = Counter()
    paragraph_origin_buckets: Counter[str] = Counter()

    for item in classified:
        paragraph_origin_buckets[item["paragraph_origin_bucket"]] += 1
        text_policy_buckets[item["body_text_match_bucket"]] += 1
        if not item["passed"]:
            failures_by_adapter[item["adapter"]] += 1
            for owner in item["owners"]:
                failures_by_owner[owner] += 1

    rows_failed = sum(1 for item in classified if not item["passed"])
    rows_passed = len(classified) - rows_failed
    blocking_owners = sorted(failures_by_owner, key=owner_sort_key)
    return {
        "scope": "tei-eaj-generated-matrix plain_prose rows",
        "plaintext_policy": PLAINTEXT_POLICY,
        "rows_total": len(classified),
        "rows_passed": rows_passed,
        "rows_failed": rows_failed,
        "verdict": verdict_from_failures(len(classified), rows_failed, failures_by_owner),
        "blocking_owners": blocking_owners,
        "failures_by_owner": dict(sorted(failures_by_owner.items())),
        "failures_by_adapter": dict(sorted(failures_by_adapter.items())),
        "text_policy_buckets": dict(sorted(text_policy_buckets.items())),
        "paragraph_origin_buckets": dict(sorted(paragraph_origin_buckets.items())),
        "failed_rows": [item for item in classified if not item["passed"]],
    }


def lane_verdict(profile: str) -> str:
    if profile in LANE_OUT_OF_SCOPE:
        return "LANE_OUT_OF_SCOPE_FOR_LEVEL3"
    if profile in LANE_POLICY_REQUIRED:
        return "LANE_POLICY_REQUIRED"
    if profile == "plain_prose":
        return "LANE_READY_FOR_GATE_IMPLEMENTATION"
    return "LANE_BLOCKED_EVIDENCE"


def build_profile_lanes(rows: list[dict[str, Any]]) -> dict[str, Any]:
    counts: Counter[str] = Counter(row_profile(row) for row in rows)
    lanes: dict[str, Any] = {}
    for profile in sorted(set(counts) | LANE_POLICY_REQUIRED | LANE_OUT_OF_SCOPE):
        lanes[profile] = {
            "rows": counts.get(profile, 0),
            "verdict": lane_verdict(profile),
        }
    return lanes


def build_evidence_gaps(matrix: dict[str, Any]) -> dict[str, Any]:
    skipped = matrix.get("skipped", [])
    reasons: Counter[str] = Counter(item.get("reason", "unknown") for item in skipped)
    return {
        "rows": len(skipped),
        "reasons": dict(sorted(reasons.items())),
        "examples": skipped[:10],
    }


def build_summary(
    matrix: dict[str, Any],
    source: dict[str, Any],
    mapping: dict[str, Any],
    mapping_hash: str,
) -> dict[str, Any]:
    rows = matrix.get("rows", [])
    plain_prose = build_plain_prose_admission(rows, source_authority_passed(source))
    return {
        "schema_version": SCHEMA_VERSION,
        "source_authority_gate": source_authority_gate(source),
        "parser_ir_infrastructure_verdict": parser_ir_infrastructure_verdict(matrix),
        "plain_prose_admission": plain_prose,
        "profile_lanes": build_profile_lanes(rows),
        "evidence_gaps": build_evidence_gaps(matrix),
        "mapping": build_mapping_block(mapping, mapping_hash),
        "inputs": matrix.get("inputs", {}),
    }


def render_markdown(summary: dict[str, Any]) -> str:
    plain = summary["plain_prose_admission"]
    lines = [
        "# Profile-Aware Level 3 TEI Admission",
        "",
        "This report classifies measured parser-IR generated TEI evidence into profile-aware Level 3 admission buckets. TEI-EAJ remains comparison evidence; source inventory remains source authority.",
        "",
        "## Verdict",
        "",
        f"- source_authority_gate: `{summary['source_authority_gate'].get('gate_status')}`",
        f"- parser_ir_infrastructure_verdict: `{summary['parser_ir_infrastructure_verdict']}`",
        f"- plain_prose_verdict: `{plain['verdict']}`",
        f"- blocking_owners: `{', '.join(plain['blocking_owners']) or 'none'}`",
        f"- mapping_hash: `{summary['mapping']['mapping_hash']}`",
        "",
        "## Plain Prose Admission",
        "",
        f"- plaintext_surface: `{plain['plaintext_policy']['plaintext_surface']}`",
        f"- ruby_expanded_surfaces: `{plain['plaintext_policy']['ruby_expanded_surfaces']}`",
        f"- metadata_policy: `{plain['plaintext_policy']['metadata_policy']}`",
        "",
        "| metric | value |",
        "|---|---:|",
        f"| rows_total | {plain['rows_total']} |",
        f"| rows_passed | {plain['rows_passed']} |",
        f"| rows_failed | {plain['rows_failed']} |",
        "",
        "### Failures By Owner",
        "",
        "| owner | rows |",
        "|---|---:|",
    ]
    for owner, count in plain["failures_by_owner"].items():
        lines.append(f"| {owner} | {count} |")

    lines.extend(["", "### Paragraph Origin Buckets", "", "| bucket | rows |", "|---|---:|"])
    for bucket, count in plain["paragraph_origin_buckets"].items():
        lines.append(f"| {bucket} | {count} |")

    lines.extend(["", "### Text Policy Buckets", "", "| bucket | rows |", "|---|---:|"])
    for bucket, count in plain["text_policy_buckets"].items():
        lines.append(f"| {bucket} | {count} |")

    lines.extend(["", "## Profile Lanes", "", "| profile | rows | verdict |", "|---|---:|---|"])
    for profile, lane in summary["profile_lanes"].items():
        lines.append(f"| {profile} | {lane['rows']} | `{lane['verdict']}` |")

    lines.extend(["", "## Evidence Gaps", "", f"- rows: {summary['evidence_gaps']['rows']}"])
    for reason, count in summary["evidence_gaps"]["reasons"].items():
        lines.append(f"- {reason}: {count}")

    if plain["failed_rows"]:
        lines.extend(
            [
                "",
                "## Plain Prose Failed Rows",
                "",
                "| work_id | adapter | paragraph origin | text bucket | owners | file |",
                "|---|---|---|---|---|---|",
            ]
        )
        for row in plain["failed_rows"]:
            lines.append(
                "| {work_id} | {adapter} | {origin} | {text} | {owners} | `{file}` |".format(
                    work_id=row.get("work_id"),
                    adapter=row["adapter"],
                    origin=row["paragraph_origin_bucket"],
                    text=row["body_text_match_bucket"],
                    owners=", ".join(row["owners"]),
                    file=row.get("tei_eaj_file"),
                )
            )

    lines.append("")
    return "\n".join(lines)


def main() -> int:
    args = parse_args()
    matrix = load_json(args.matrix_summary)
    source = load_json(args.source_summary)
    mapping_input = matrix.get("inputs", {}).get("mapping")
    if not mapping_input:
        raise SystemExit("matrix summary missing inputs.mapping")
    mapping_path = resolve_path(mapping_input, args.matrix_summary.parent)
    mapping = load_json(mapping_path)
    summary = build_summary(matrix, source, mapping, document_hash(mapping))
    write_json(args.summary_json, summary)
    write_text(args.report_md, render_markdown(summary))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
```

Make it executable:

```bash
chmod +x reports/parser-ir/level3-admission.py
```

- [ ] **Step 4: Run smoke to verify it passes**

Run:

```bash
tests/parser-ir-level3-admission-smoke.sh
```

Expected: PASS with each `jq -e` printing `true` and `rg` finding the verdict/profile strings.

- [ ] **Step 5: Commit**

```bash
git add reports/parser-ir/level3-admission.py tests/parser-ir-level3-admission-smoke.sh
git commit -m "test(parser-ir): add profile-aware level3 admission smoke"
```

---

### Task 2: Wire Just Targets

**Files:**
- Modify: `justfile`

**Interfaces:**
- Consumes:
  - `reports/parser-ir/level3-admission.py`
  - `docs/superpowers/reports/2026-07-04-tei-eaj-generated-matrix-comparison.summary.json`
  - `docs/superpowers/reports/2026-07-04-source-authority-representability.summary.json`
- Produces:
  - Recipe: `just parser-ir-level3-admission-smoke`
  - Recipe: `just parser-ir-level3-admission-report MATRIX_SUMMARY SOURCE_SUMMARY REPORT_MD SUMMARY_JSON`
  - The report recipe relies on `MATRIX_SUMMARY.inputs.mapping` to locate and hash the mapping file; do not add a separate stale mapping-summary input.

- [ ] **Step 1: Add failing just-target smoke expectation**

Run before editing `justfile`:

```bash
just parser-ir-level3-admission-smoke
```

Expected: FAIL with:

```text
error: Justfile does not contain recipe `parser-ir-level3-admission-smoke`
```

- [ ] **Step 2: Modify `justfile`**

Add these recipes after `parser-ir-level3-tei-eaj-generated-matrix-audit`:

```make
parser-ir-level3-admission-smoke:
	@bash "{{repo_root}}/tests/parser-ir-level3-admission-smoke.sh"

parser-ir-level3-admission-report MATRIX_SUMMARY="docs/superpowers/reports/2026-07-04-tei-eaj-generated-matrix-comparison.summary.json" SOURCE_SUMMARY="docs/superpowers/reports/2026-07-04-source-authority-representability.summary.json" REPORT_MD="docs/superpowers/reports/2026-07-05-profile-aware-level3-tei-admission.md" SUMMARY_JSON="docs/superpowers/reports/2026-07-05-profile-aware-level3-tei-admission.summary.json":
	@python "{{repo_root}}/reports/parser-ir/level3-admission.py" \
		--matrix-summary "{{repo_root}}/{{MATRIX_SUMMARY}}" \
		--source-summary "{{repo_root}}/{{SOURCE_SUMMARY}}" \
		--summary-json "{{repo_root}}/{{SUMMARY_JSON}}" \
		--report-md "{{repo_root}}/{{REPORT_MD}}"
```

- [ ] **Step 3: Run just smoke**

Run:

```bash
just parser-ir-level3-admission-smoke
```

Expected: PASS.

- [ ] **Step 4: Generate report through just**

Run:

```bash
just parser-ir-level3-admission-report
```

Expected:

- creates `docs/superpowers/reports/2026-07-05-profile-aware-level3-tei-admission.md`
- creates `docs/superpowers/reports/2026-07-05-profile-aware-level3-tei-admission.summary.json`
- exits 0

- [ ] **Step 5: Commit**

```bash
git add justfile docs/superpowers/reports/2026-07-05-profile-aware-level3-tei-admission.md docs/superpowers/reports/2026-07-05-profile-aware-level3-tei-admission.summary.json
git commit -m "docs(parser-ir): generate level3 admission report"
```

---

### Task 3: Add Flake Smoke Check

**Files:**
- Modify: `flake.nix`

**Interfaces:**
- Consumes:
  - `tests/parser-ir-level3-admission-smoke.sh`
- Produces:
  - `checks.<system>.parser-ir-level3-admission-smoke`

- [ ] **Step 1: Verify flake check is absent**

Run:

```bash
system=$(nix eval --impure --raw --expr builtins.currentSystem)
nix eval ".#checks.$system.parser-ir-level3-admission-smoke.drvPath"
```

Expected: FAIL because the attribute is missing.

- [ ] **Step 2: Add `level3AdmissionSmokeCheck`**

In `flake.nix`, near the existing `sourceRepresentabilityGateCheck` and `abAatToParserIrCheck`, add:

```nix
        level3AdmissionSmokeCheck =
          pkgs.runCommand "parser-ir-level3-admission-smoke-check"
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

              bash tests/parser-ir-level3-admission-smoke.sh
              touch "$out"
            '';
```

Then add this key under `checks = { ... }`:

```nix
          parser-ir-level3-admission-smoke = level3AdmissionSmokeCheck;
```

- [ ] **Step 3: Format Nix**

Run:

```bash
nixfmt flake.nix
```

Expected: exits 0. If `nixfmt` is not available, use the repo's existing Nix formatting command if one is already configured; do not hand-align large Nix blocks.

- [ ] **Step 4: Run the flake check**

Run:

```bash
system=$(nix eval --impure --raw --expr builtins.currentSystem)
nix --option post-build-hook '' build ".#checks.$system.parser-ir-level3-admission-smoke" --print-build-logs
```

Expected: exits 0 and smoke assertions pass.

- [ ] **Step 5: Commit**

```bash
git add flake.nix
git commit -m "build(parser-ir): gate level3 admission smoke"
```

---

### Task 4: Update Stale Level 3 Narrative Reports

**Files:**
- Modify: `docs/superpowers/reports/2026-07-04-post-parser-ir-conversion-sync.md`
- Modify: `docs/superpowers/reports/2026-07-04-tei-eaj-structural-gap-analysis.md`

**Interfaces:**
- Consumes:
  - `docs/superpowers/reports/2026-07-04-source-authority-representability.summary.json`
  - `docs/superpowers/reports/2026-07-05-profile-aware-level3-tei-admission.summary.json`
- Produces:
  - Updated prose that no longer lists source-authority as failing or treats source-authority as a current blocker.
  - References to the profile-aware admission report as the current Level 3 gate artifact.

- [ ] **Step 1: Verify stale claims exist**

Run:

```bash
rg -n "SOURCE_AUTHORITY_GATE_FAILING_REVIEW_REQUIRED|source-authority representability is still failing|260 unallowlisted|current run is failing|PARSER_IR_LEVEL3_REPRESENTABLE_WITH_ADAPTER_AND_SOURCE_AUTHORITY_GAPS|strict error|strict errors" docs/superpowers/reports/2026-07-04-post-parser-ir-conversion-sync.md docs/superpowers/reports/2026-07-04-tei-eaj-structural-gap-analysis.md
```

Expected: finds stale source-authority failure claims and the transitional gap-analysis verdict.

- [ ] **Step 2: Update `2026-07-04-post-parser-ir-conversion-sync.md`**

Replace the `## Source Authority Caveat` section with:

```markdown
## Source Authority Status

The source-authority inventory now passes:

- report: `docs/superpowers/reports/2026-07-04-source-authority-representability.md`
- source_authority_gate: `SOURCE_AUTHORITY_GATE_PASS`
- works_scanned: 17,894
- unallowlisted_unknown_markers_total: 0

This removes the previous source-authority blocker for Level 2/3 parser-IR work. Parser evidence remains triangulation, but the source inventory now supplies the authority gate that all reached explicit Aozora markers have reviewed representation or explicit waiver.

The next Level 3 blocker is profile-aware admission, tracked by:

- spec: `docs/superpowers/specs/2026-07-05-profile-aware-level3-tei-admission.md`
- report: `docs/superpowers/reports/2026-07-05-profile-aware-level3-tei-admission.md`
- summary: `docs/superpowers/reports/2026-07-05-profile-aware-level3-tei-admission.summary.json`
```

- [ ] **Step 3: Update `2026-07-04-tei-eaj-structural-gap-analysis.md`**

In `## Current Blockers to Level 3 TEI Generation`, replace blocker 1 with:

```markdown
1. Source-authority representability has passed for the current corpus snapshot. The current source-authority report is `SOURCE_AUTHORITY_GATE_PASS` with 17,894 works scanned and 0 unallowlisted unknown markers. This removes source inventory as a current blocker, but future corpus or scanner changes must rerun the gate.
```

Then add this paragraph after the blocker list:

```markdown
The current admission policy is now specified in `docs/superpowers/specs/2026-07-05-profile-aware-level3-tei-admission.md` and measured in `docs/superpowers/reports/2026-07-05-profile-aware-level3-tei-admission.md`. That report separates `LEVEL3_IR_INFRASTRUCTURE_READY` from `LEVEL3_PLAIN_PROSE_ADMITTED` so parser-IR capability and adapter admission are not conflated.
```

If the report headline contains:

```text
PARSER_IR_LEVEL3_REPRESENTABLE_WITH_ADAPTER_AND_SOURCE_AUTHORITY_GAPS
```

replace that verdict with:

```text
PARSER_IR_LEVEL3_INFRASTRUCTURE_READY_PROFILE_ADMISSION_REQUIRED
```

If blocker 3 refers to source-authority strict errors as a current blocker, replace it with:

```markdown
3. Source-authority strict errors are closed for the current corpus snapshot. Future scanner or corpus changes must rerun `just source-authority-representability-gate`, but this is no longer a current Level 3 blocker.
```

- [ ] **Step 4: Verify stale claims are gone**

Run:

```bash
rg -n "SOURCE_AUTHORITY_GATE_FAILING_REVIEW_REQUIRED|source-authority representability is still failing|260 unallowlisted|current run is failing|PARSER_IR_LEVEL3_REPRESENTABLE_WITH_ADAPTER_AND_SOURCE_AUTHORITY_GAPS" docs/superpowers/reports/2026-07-04-post-parser-ir-conversion-sync.md docs/superpowers/reports/2026-07-04-tei-eaj-structural-gap-analysis.md
```

Expected: no output, exit 1.

- [ ] **Step 5: Commit**

```bash
git add docs/superpowers/reports/2026-07-04-post-parser-ir-conversion-sync.md docs/superpowers/reports/2026-07-04-tei-eaj-structural-gap-analysis.md
git commit -m "docs(parser-ir): refresh level3 blocker narrative"
```

---

### Task 5: Full Verification And Final Report

**Files:**
- No new files.
- Verify all touched files from Tasks 1-4.

**Interfaces:**
- Consumes:
  - all committed artifacts from previous tasks.
- Produces:
  - final verification evidence and clean worktree except unrelated pre-existing untracked files.

- [ ] **Step 1: Run script smoke**

```bash
just parser-ir-level3-admission-smoke
```

Expected: exits 0.

- [ ] **Step 2: Regenerate admission report**

```bash
just parser-ir-level3-admission-report
```

Expected: exits 0 and leaves no diff if the report was already regenerated in Task 2.

- [ ] **Step 3: Assert current admission summary shape**

```bash
jq -e '
  .schema_version == "profile-aware-level3-tei-admission-v1"
  and .source_authority_gate.gate_status == "SOURCE_AUTHORITY_GATE_PASS"
  and .parser_ir_infrastructure_verdict == "LEVEL3_IR_INFRASTRUCTURE_READY"
  and (.mapping.mapping_id | type == "string")
  and (.mapping.mapping_hash | test("^sha256:[0-9a-f]{64}$"))
  and (.plain_prose_admission.blocking_owners | type == "array")
  and .plain_prose_admission.plaintext_policy.plaintext_surface == "body_base_text"
  and .plain_prose_admission.plaintext_policy.ruby_expanded_surfaces == "diagnostic_only"
  and (.plain_prose_admission.rows_total > 0)
  and (.profile_lanes.lv4_enrichment.verdict == "LANE_OUT_OF_SCOPE_FOR_LEVEL3")
' docs/superpowers/reports/2026-07-05-profile-aware-level3-tei-admission.summary.json
```

Expected: prints `true`.

- [ ] **Step 4: Run flake smoke**

```bash
system=$(nix eval --impure --raw --expr builtins.currentSystem)
nix --option post-build-hook '' build ".#checks.$system.parser-ir-level3-admission-smoke" --print-build-logs
```

Expected: exits 0.

- [ ] **Step 5: Run whitespace checks**

```bash
git diff --check
```

Expected: exits 0. Do not run `cargo fmt --all --check` for this plan; this change touches Python, Bash, Nix, Markdown, and generated JSON only, and unrelated Rust formatting must not gate this work.

- [ ] **Step 6: Inspect worktree**

```bash
git status --short --branch
```

Expected:

- branch may be ahead of `origin/main` if commits have not been pushed,
- no unstaged/staged changes from this plan,
- the unrelated `?? docs/superpowers/specs/2026-07-05-interestingness-ranking-design.md` may still be present and must remain untouched.

- [ ] **Step 7: Final commit if verification changed reports**

If Step 2 changed generated reports, commit only those report files:

```bash
git add docs/superpowers/reports/2026-07-05-profile-aware-level3-tei-admission.md docs/superpowers/reports/2026-07-05-profile-aware-level3-tei-admission.summary.json
git commit -m "docs(parser-ir): refresh level3 admission report"
```

If Step 2 produced no diff, do not create an empty commit.

---

## Self-Review

- Spec coverage: The plan implements the spec's report contract, including non-empty mapping provenance loaded from the real mapping file, plain-prose hard gate, profile lanes, evidence gaps, and stale source-authority narrative cleanup.
- Placeholder scan: No unresolved placeholder tokens or vague validation/error-handling instructions remain.
- Type consistency: The plan consistently uses `build_summary(matrix, source, mapping, mapping_hash)`, `profile_lanes`, `plain_prose_admission`, `source_authority_gate`, `parser_ir_infrastructure_verdict`, and `blocking_owners`.
- Scope control: The plan does not modify parser-IR schema, ABC registry, adapter parsers, or TEI materialization logic. It classifies existing measured evidence.
- Gate honesty: The plan does not claim `LEVEL3_PLAIN_PROSE_ADMITTED` unless the measured plain-prose rows pass. The expected current broad-adapter result remains blocked by adapter fidelity and/or text policy.
