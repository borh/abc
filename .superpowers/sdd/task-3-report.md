# Task 3 Report: Plain-Prose Delta Probe

## Status

GREEN

## Summary

Implemented `reports/parser-ir/plain-prose-source-delta.py` as the plain-prose Level 3 source-delta probe described in Task 3. The script:

- loads the admission, matrix, structural, source, and mapping summaries,
- enforces the required five-parser evidence contract unless exploratory mode is enabled,
- classifies plain-prose rows into adapter, policy, parser-IR, ABC renderer, and evidence blockers,
- emits the required summary JSON shape, and
- renders a Markdown operator report.

## Files Changed

- `reports/parser-ir/plain-prose-source-delta.py`
- `tests/parser-ir-plain-prose-source-delta-smoke.sh`

## Smoke Update Rationale

I made one legitimate Task 2 smoke fix. The smoke contained an invalid `jq` filter at line 178, so the test failed with a `jq` parse error after the implementation started satisfying the JSON contract. I corrected that filter and did not widen the smoke beyond this syntax repair.

## Test Evidence

### 1. Smoke

Command:

```bash
bash tests/parser-ir-plain-prose-source-delta-smoke.sh
```

Result:

- exit status `0`
- strict mode correctly fails first with:
  - `missing parser evidence; rerun with --allow-missing-parser-evidence for exploratory report`
- exploratory mode passes all assertions, including:
  - `schema_version == "plain-prose-source-delta-probe-v1"`
  - required parser list equals the five specified labels
  - mapping id is populated
  - parser evidence verdict is `FIVE_PARSER_EVIDENCE_INCOMPLETE`
  - missing global parser is `["aozora"]`
  - `rows_missing_required_parser_evidence == 2`
  - classification counts for adapter, ruby, source-note, source-text-policy, and missing-parser-evidence all match fixture expectations
  - Markdown report contains `FIVE_PARSER_EVIDENCE_INCOMPLETE`, `ruby_metadata_not_plaintext`, and `source_note_metadata_excluded`

### 2. Python Syntax Check

Command:

```bash
python3 -m py_compile reports/parser-ir/plain-prose-source-delta.py
```

Result:

- exit status `0`
- no output

## Commit

- `824f3ac` `feat(parser-ir): classify plain prose source deltas`

## Self-Review

- The implementation matches the Task 3 plan’s interface and output shape.
- The mapping block fails closed if required fields cannot be recovered.
- Missing parser evidence is attached once per `(work_id, tei_eaj_file)` evidence group, which matches the smoke contract and avoids over-counting repeated adapter rows for the same work/file.
- The structural summary is accepted but not yet used; this is consistent with the task brief’s implementation block.

## Concerns

- No blocking concerns for Task 3.
- The current full-report semantics still depend on exploratory mode until five-parser matrix coverage exists for all in-scope plain-prose rows, which is expected by the design.

## Review Fix

Reviewer finding applied in the implementation and smoke:

- Removed the `missing_consumed` / one-tag-per-group behavior from `reports/parser-ir/plain-prose-source-delta.py`.
- Restored the task-brief behavior so each affected plain-prose row receives `missing_parser_evidence` via `classify_row(row, missing_by_row.get(row_key(row), []))`.
- Updated `tests/parser-ir-plain-prose-source-delta-smoke.sh` to expect `.classification_counts.missing_parser_evidence == 5` while keeping `.parser_evidence_coverage.rows_missing_required_parser_evidence == 2`.

### Review-Fix Test Evidence

Command:

```bash
bash tests/parser-ir-plain-prose-source-delta-smoke.sh
```

Result:

- exit status `0`
- strict mode still fails first with the expected missing-parser-evidence message
- exploratory mode passes with `classification_counts.missing_parser_evidence == 5`
- exploratory mode still reports `rows_missing_required_parser_evidence == 2`

Command:

```bash
python3 -m py_compile reports/parser-ir/plain-prose-source-delta.py
```

Result:

- exit status `0`
- no output
