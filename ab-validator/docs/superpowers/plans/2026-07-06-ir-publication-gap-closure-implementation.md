# IR Publication Gap Closure Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Update the IR publication coverage report so known unsupported rows are folded into explicit closure families/lane/admission gates instead of being reported as undifferentiated unsupported gaps.

**Architecture:** Keep `reports/parser-ir/publication-coverage.py` as the report owner. Add a pure classifier layer that maps unsupported-derived rows to closure families from `docs/superpowers/specs/2026-07-06-ir-publication-gap-closure-design.md`, then report `classified_but_not_admitted` separately from `true_unsupported_gaps`. The top-level verdict remains blocked until true unsupported gaps are empty and all classified families are admitted by ABC/schema/converter work.

**Tech Stack:** Python 3 standard library, Bash smoke test, `jq`, existing `just` recipes.

## Global Constraints

- Do not change Parser-IR schema in this plan.
- Do not claim ABC custom contract admission in this plan.
- Keep plaintext policy exactly: `exclude_ruby_readings_layout_source_notes_custom_records_warnings_and_provenance`.
- Preserve five required parser evidence lanes: `aozora2html`, `aozora-epub3`, `aozora-rs`, `aozora2`, `aozora`.
- Do not remove mapping artifact rule IDs; fold only for publication coverage reporting.

---

### Task 1: Smoke Coverage For Closure Families

**Files:**
- Modify: `tests/parser-ir-publication-coverage-smoke.sh`

**Interfaces:**
- Consumes: current `publication-coverage.py` CLI.
- Produces: failing assertions for `closure_gaps.classified_but_not_admitted`, `closure_gaps.true_unsupported_gaps`, and the top-level blocked verdict.

- [ ] **Step 1: Add failing assertions to the first smoke scenario**

Add these assertions after the existing unsupported gap assertions:

```bash
jq -e '.closure_gaps.classified_but_not_admitted.count == 3' "$summary_json" >/dev/null
jq -e '.closure_gaps.true_unsupported_gaps.count == 0' "$summary_json" >/dev/null
jq -e '.closure_gaps.classified_but_not_admitted.items[] | select(.closure_family == "figure_metadata") | .closure_lane == "tei_policy_projection"' "$summary_json" >/dev/null
jq -e '.closure_gaps.classified_but_not_admitted.items[] | select(.closure_family == "style_rendition") | .closure_lane == "tei_policy_projection"' "$summary_json" >/dev/null
jq -e '.closure_gaps.classified_but_not_admitted.items[] | select(.closure_family == "span_coordinates") | .closure_lane == "custom_sidecar"' "$summary_json" >/dev/null
jq -e '.verdict == "IR_PUBLICATION_COVERAGE_BLOCKED_CLASSIFIED_GAPS"' "$summary_json" >/dev/null
```

- [ ] **Step 2: Add one unknown true-gap fixture assertion**

Keep the existing `unknown_mapping` scenario and add:

```bash
jq -e '.closure_gaps.true_unsupported_gaps.count == 1' "$unknown_summary_json" >/dev/null
jq -e '.closure_gaps.true_unsupported_gaps.items[0].closure_family == null' "$unknown_summary_json" >/dev/null
jq -e '.verdict == "IR_PUBLICATION_COVERAGE_BLOCKED_UNSUPPORTED_GAPS"' "$unknown_summary_json" >/dev/null
```

- [ ] **Step 3: Verify the test fails**

Run:

```bash
bash tests/parser-ir-publication-coverage-smoke.sh
```

Expected: FAIL because `.closure_gaps` does not exist yet.

- [ ] **Step 4: Commit after implementation, not now**

This task is red-only. Commit after Task 2 turns it green.

### Task 2: Closure Classifier

**Files:**
- Modify: `reports/parser-ir/publication-coverage.py`

**Interfaces:**
- Consumes: unsupported item dictionaries from `unsupported_gaps(...)`.
- Produces:
  - `classify_closure_gap(item: dict[str, Any]) -> dict[str, Any] | None`
  - `closure_gaps(gaps: dict[str, Any]) -> dict[str, Any]`

- [ ] **Step 1: Add closure metadata constants**

Add a dictionary keyed by family:

```python
CLOSURE_FAMILIES = {
    "span_coordinates": {
        "closure_lane": "custom_sidecar",
        "owner": CUSTOM_SCHEMA_OWNER,
        "admission_gate": "ABC custom contract preserves span coordinates or explicit IR pointer span records.",
    },
    "style_rendition": {
        "closure_lane": "tei_policy_projection",
        "owner": "policy",
        "admission_gate": "ABC TEI profile declares style/rendition vocabulary and sidecar preserves exact source marker where needed.",
    },
    "accent": {
        "closure_lane": "tei_plus_abc_extension",
        "owner": "policy",
        "admission_gate": "ABC TEI profile declares accent rendition vocabulary and custom contract preserves original accent code.",
    },
    "source_identity": {
        "closure_lane": "custom_sidecar",
        "owner": CUSTOM_SCHEMA_OWNER,
        "admission_gate": "ABC custom contract and manifest linkage preserve source identity and normalization fields.",
    },
    "provenance_metrics": {
        "closure_lane": "custom_sidecar",
        "owner": CUSTOM_SCHEMA_OWNER,
        "admission_gate": "ABC custom contract includes producer metrics or superseding validation/admission verdicts.",
    },
    "figure_metadata": {
        "closure_lane": "tei_policy_projection",
        "owner": "policy",
        "admission_gate": "ABC TEI profile maps figure dimensions/classes and custom contract preserves exact source class when TEI rend is not exact.",
    },
    "gaiji_unresolved_reason": {
        "closure_lane": "custom_sidecar",
        "owner": CUSTOM_SCHEMA_OWNER,
        "admission_gate": "ABC custom contract preserves gaiji resolution diagnostics.",
    },
    "heading_jisage_structure": {
        "closure_lane": "aat_to_parser_ir_converter_delta",
        "owner": "aat_to_parser_ir_converter",
        "admission_gate": "Converter emits paragraph/layout/heading facts consistently for heading and jisage structures.",
    },
    "font_tcy": {
        "closure_lane": "parser_ir_schema_delta",
        "owner": "parser_ir_schema",
        "admission_gate": "Parser-IR schema represents font_size and tcy, ABC profile declares rendition values, and custom contract preserves marker identity.",
    },
    "keigakomi_yokogumi": {
        "closure_lane": "parser_ir_schema_delta",
        "owner": "parser_ir_schema",
        "admission_gate": "Parser-IR schema represents inline/block layout containers and ABC profile names keigakomi/yokogumi TEI vocabulary.",
    },
}
```

- [ ] **Step 2: Add pointer family detection**

Implement:

```python
def gap_pointer(item: dict[str, Any]) -> str:
    return str(item.get("aat_pointer") or item.get("parser_ir_pointer") or item.get("node_type") or item.get("field") or "")


def closure_family_for_pointer(pointer: str) -> str | None:
    if pointer.endswith(".span") or ".span." in pointer:
        return "span_coordinates"
    if pointer.endswith(".style"):
        return "style_rendition"
    if ".accent" in pointer or pointer.endswith(".accent"):
        return "accent"
    if pointer in {"meta.source_encoding", "meta.source_hash", "source.normalization", "source.source_path", "schema_id/schema_hash"}:
        return "source_identity"
    if pointer in {"meta.metrics", "meta.parse_complete", "meta.semantic_summary"}:
        return "provenance_metrics"
    if ".figure." in pointer or pointer.endswith(".figure"):
        return "figure_metadata"
    if pointer.endswith(".gaiji.unresolved_reason"):
        return "gaiji_unresolved_reason"
    if pointer.endswith(".heading") or pointer.endswith(".jisage_block") or ".heading." in pointer:
        return "heading_jisage_structure"
    if pointer.endswith(".font_size") or pointer.endswith(".tcy"):
        return "font_tcy"
    if pointer.endswith(".keigakomi") or pointer.endswith(".yokogumi"):
        return "keigakomi_yokogumi"
    return None
```

- [ ] **Step 3: Add closure gap aggregation**

Implement:

```python
def classify_closure_gap(item: dict[str, Any]) -> dict[str, Any] | None:
    family = closure_family_for_pointer(gap_pointer(item))
    if family is None:
        return None
    spec = CLOSURE_FAMILIES[family]
    return {
        **item,
        "closure_family": family,
        "closure_lane": spec["closure_lane"],
        "closure_owner": spec["owner"],
        "admission_gate": spec["admission_gate"],
    }


def closure_gaps(gaps: dict[str, Any]) -> dict[str, Any]:
    classified = []
    true_unsupported = []
    for item in gaps.get("items", []):
        closure_item = classify_closure_gap(item)
        if closure_item is None:
            true_unsupported.append({**item, "closure_family": None, "closure_lane": "unsupported_gap"})
        else:
            classified.append(closure_item)
    return {
        "classified_but_not_admitted": {
            "count": len(classified),
            "counts_by_family": count_values(classified, "closure_family"),
            "counts_by_lane": count_values(classified, "closure_lane"),
            "items": classified,
        },
        "true_unsupported_gaps": {
            "count": len(true_unsupported),
            "counts_by_owner": count_values(true_unsupported, "owner"),
            "items": true_unsupported,
        },
    }
```

- [ ] **Step 4: Wire into summary and verdict**

Compute `closures = closure_gaps(gaps)` in `build_summary`, add it as
`"closure_gaps": closures`, and update `publication_verdict(...)` to accept
closures:

```python
if closures["true_unsupported_gaps"]["count"] > 0:
    return "IR_PUBLICATION_COVERAGE_BLOCKED_UNSUPPORTED_GAPS"
if closures["classified_but_not_admitted"]["count"] > 0:
    return "IR_PUBLICATION_COVERAGE_BLOCKED_CLASSIFIED_GAPS"
```

Keep custom-contract blocking after classified gaps.

- [ ] **Step 5: Verify green**

Run:

```bash
bash tests/parser-ir-publication-coverage-smoke.sh
python -m py_compile reports/parser-ir/publication-coverage.py
```

Expected: both exit 0.

- [ ] **Step 6: Commit**

```bash
git add reports/parser-ir/publication-coverage.py tests/parser-ir-publication-coverage-smoke.sh
git commit -m "feat(parser-ir): classify publication gap closure lanes"
```

### Task 3: Regenerate Report And Gate Current Corpus

**Files:**
- Modify: `docs/superpowers/reports/2026-07-06-ir-publication-coverage.summary.json`
- Modify: `docs/superpowers/reports/2026-07-06-ir-publication-coverage.md`

**Interfaces:**
- Consumes: `just parser-ir-publication-coverage-report`.
- Produces: updated report where known families are classified and current true unsupported gaps are counted separately.

- [ ] **Step 1: Regenerate report**

Run:

```bash
just parser-ir-publication-coverage-report
```

Expected: exits 0.

- [ ] **Step 2: Verify current family fold**

Run:

```bash
jq -e '.closure_gaps.true_unsupported_gaps.count == 0' docs/superpowers/reports/2026-07-06-ir-publication-coverage.summary.json
jq -e '.closure_gaps.classified_but_not_admitted.count == 165' docs/superpowers/reports/2026-07-06-ir-publication-coverage.summary.json
jq -e '.verdict == "IR_PUBLICATION_COVERAGE_BLOCKED_CLASSIFIED_GAPS"' docs/superpowers/reports/2026-07-06-ir-publication-coverage.summary.json
```

Expected: all exit 0.

- [ ] **Step 3: Verify required five-parser and plaintext constraints remain**

Run:

```bash
jq -e '.scope.required_parsers == ["aozora2html","aozora-epub3","aozora-rs","aozora2","aozora"]' docs/superpowers/reports/2026-07-06-ir-publication-coverage.summary.json
jq -e '.plaintext_policy.metadata_policy == "exclude_ruby_readings_layout_source_notes_custom_records_warnings_and_provenance"' docs/superpowers/reports/2026-07-06-ir-publication-coverage.summary.json
```

Expected: both exit 0.

- [ ] **Step 4: Run justfile smoke**

Run:

```bash
just parser-ir-publication-coverage-smoke
```

Expected: exits 0.

- [ ] **Step 5: Commit**

```bash
git add docs/superpowers/reports/2026-07-06-ir-publication-coverage.summary.json docs/superpowers/reports/2026-07-06-ir-publication-coverage.md
git commit -m "docs(parser-ir): refresh publication closure report"
```

## Self-Review

- Spec coverage: implements the closure-family reporting requested by `2026-07-06-ir-publication-gap-closure-design.md`; does not implement ABC custom contract or Parser-IR schema deltas.
- Placeholder scan: no TBD/TODO placeholders.
- Type consistency: `closure_family`, `closure_lane`, `closure_owner`, and `admission_gate` are introduced in Task 2 and consumed in Task 3.
