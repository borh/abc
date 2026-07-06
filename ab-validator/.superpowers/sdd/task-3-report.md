# Task 3 Report — IR Publication Coverage Summary / CLI

## Status

- Completed

## Files Changed

- `reports/parser-ir/publication-coverage.py`
- `tests/parser-ir-publication-coverage-smoke.sh`

## Changes Implemented

- Added source authority and parser-evidence gate helpers:
  - `source_authority_gate`
  - `source_authority_passed`
  - `parser_evidence_coverage`
- Added custom contract and mapping blocks:
  - `custom_contract_block`
  - `mapping_block`
- Added verdict composition and precedence:
  - `unsupported_gaps`
  - `publication_verdict`
- Added summary construction and markdown rendering:
  - `build_summary`
  - `render_markdown`
- Added CLI support:
  - `parse_args`
  - `load_json`, `write_json`, `write_text`
  - `canonical_json`, `document_hash`
  - `main` with full input/output wiring
- Extended smoke script with a second fixture run proving
  `IR_PUBLICATION_COVERAGE_BLOCKED_CUSTOM_CONTRACT_MISSING` when unsupported
  gaps are absent.
- Added defensive handling in `custom_contract_block` for invalid contract JSON and a
  required contract identifier check, emitting `CUSTOM_CONTRACT_INVALID` unless
  the contract ID matches `https://w3id.org/abc/schemas/ir-publication-preservation-v1.json`.
- Updated `source_construct_coverage` so unknown/unclassified AAT pointers are
  classified as `unsupported_gap` and included with explicit owners in unsupported items.
- Added smoke coverage for:
  - unknown AAT pointer producing an `unsupported_gap`, and
  - valid/invalid custom contract payloads so invalid contracts do not reach
    `IR_PUBLICATION_COVERAGE_COMPLETE`.

## Tests Run

- `bash tests/parser-ir-publication-coverage-smoke.sh` — PASS
- `python3 -m py_compile reports/parser-ir/publication-coverage.py` — PASS

## Commit

- `d387792`

## Self Review

- Scope respected: only the two requested files were modified.
- Summary fields and verdict precedence now match the Task 3 interface contract.
- Markdown report includes required sections and appears with expected headings used by
  smoke checks.
- Smoke validates both unsupported-gap and custom-contract-missing branches.

## Concerns

- None.

## Post-Review Fixes

- Replaced `construct_from_pointer` substring matching with segment-aware matching:
  - normalizes `[]` suffixes on path segments
  - matches exact construct segments and exact compound tokens (including `gaiji.resolved`) only
  - prevents `raw_material` from being classified as `raw`
- Hardened `custom_contract_block` so `CUSTOM_CONTRACT_INVALID` now requires:
  - top-level contract schema `https://w3id.org/abc/schemas/ir-publication-preservation-v1.json`
  - presence of all required top-level fields:
    `schema_id`, `schema_version`, `parser_ir_schema_id`, `parser_ir_schema_hash`,
    `tei_profile_id`, `tei_profile_hash`, `source`, `producer`, `mapping`, `coverage`,
    and `records`
- Extended smoke coverage to assert `blocks[].content[].raw_material` is captured as an
  unsupported gap, and that a JSON file with only matching id (`schema_id`) is
  `CUSTOM_CONTRACT_INVALID`.
