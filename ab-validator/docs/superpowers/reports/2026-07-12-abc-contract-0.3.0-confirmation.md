# ABC Preservation Contract 0.3.0 Confirmation (Task 7)

**Label: integration confirmation (not independent semantic proof).**

This report confirms that `reports/parser-ir/publication-coverage.py`'s custom-contract
check (`custom_contract_block`) correctly recognizes the ABC-owned
`parser-ir-publication-preservation.schema.json` contract, which rotated
`0.2.0` -> `0.3.0` on the ABC side. It records the provenance for that
rotation and the empirical before/after behavior of the coverage report.

## Source ABC commit

- Rotation commit: `7280c390e6004ead1df32d308ef01314bb7582ea` — "feat(abc):
  preservation provenance for segmentation and orthographic detector (Issue
  5)" (Bor Hodošček, 2026-07-08 10:15:25 +0900)
- Prior commit touching this file: `4bb7b52a80c6af947591baa196b3ea5a0f4df104`
  — "Import abc component" (2026-07-06 22:54:46 +0900)
- Full history for the file: `git log --oneline -- abc/schemas/parser-ir-publication-preservation.schema.json`
  shows exactly these two commits.

## Per-artifact hashes (abc-side vs. vendored)

Both sides are byte-identical:

```
sha256sum ../abc/schemas/parser-ir-publication-preservation.schema.json \
  data/abc-schemas/schemas/parser-ir-publication-preservation.schema.json
```

```
b073df014a2b7c685059d3681dbfcbbde10ec66adfb0253ab80b6287c70c5599  ../abc/schemas/parser-ir-publication-preservation.schema.json
b073df014a2b7c685059d3681dbfcbbde10ec66adfb0253ab80b6287c70c5599  data/abc-schemas/schemas/parser-ir-publication-preservation.schema.json
```

This is not a coincidence of file copying: `ab-validator/data/abc-schemas/schemas`
is a committed symlink (mode `120000`) to `../../../abc/schemas`, put in place
by the pre-existing commit `9a5efc6e` ("Add monorepo schema drift gate",
2026-07-06 22:56:13 +0900). `pathlib.Path.resolve()` on either path yields the
identical canonical filesystem path:

```
/home/.../parser-fork-phase5/abc/schemas/parser-ir-publication-preservation.schema.json
```

The coverage script's own canonical-JSON document hash
(`contract_hash`/`trusted_schema_hash` in `custom_contract_block`) is also
equal on both sides: `sha256:d521fe8c249ef9b25655e4d0ef4fbcb6006564ac4584271985459a57999d3869`.

## Semantic diff, 0.2.0 -> 0.3.0

```
diff -u <(git show 4bb7b52a:abc/schemas/parser-ir-publication-preservation.schema.json) \
        <(git show 7280c390:abc/schemas/parser-ir-publication-preservation.schema.json)
```

```diff
@@ -2,7 +2,7 @@
   "$schema": "https://json-schema.org/draft/2020-12/schema",
   "$id": "https://w3id.org/abc/schemas/parser-ir-publication-preservation.schema.json",
   "title": "ABC Parser-IR Publication Preservation Sidecar",
-  "version": "0.2.0",
+  "version": "0.3.0",
   "type": "object",
   "required": [
     "schema_id",
@@ -22,7 +22,7 @@
       "const": "https://w3id.org/abc/schemas/parser-ir-publication-preservation.schema.json"
     },
     "schema_version": {
-      "const": "0.2.0"
+      "const": "0.3.0"
     },
     "schema_hash": {
       "$ref": "#/$defs/hash"
@@ -143,9 +143,11 @@
             "gaiji_resolution",
             "heading_jisage_structure",
             "mapping_identity",
+            "orthographic_annotation",
             "paragraph.node_range",
             "paragraph.source_pointer",
             "producer_metrics",
+            "sentence_segmentation",
             "source_identity",
             "source_note.classification",
             "span_coordinates",
```

**What changed:** the `version` and `schema_version` const bumped 0.2.0 ->
0.3.0, and exactly two values were appended to the `$defs.record.properties.construct.enum`
list: `orthographic_annotation` and `sentence_segmentation`. The
`$defs.record.properties.class.enum` (`["custom_sidecar",
"tei_profile_projection"]`) and the top-level `coverage.classes` enum are
unchanged. No record class or construct was removed or renamed. This is a
purely additive rotation (matches the STOP condition in the task brief: an
additive enum/metadata change, not a removal — no escalation needed).

**Why compatible:** the coverage report only ever reads enum membership
(`schema_enum_values`) to build `record_classes`/`constructs`, and only
compares `schema_version` against the report's own pinned constant. An
additive enum change cannot invalidate anything the report previously
admitted; it can only make previously-unadmitted rows admissible if their
`closure_family` now maps onto one of the newly enumerated constructs.

## Commands run

```
git log --oneline -- abc/schemas/parser-ir-publication-preservation.schema.json
git show 4bb7b52a:abc/schemas/parser-ir-publication-preservation.schema.json > /tmp/schema-0.2.0.json
git show 7280c390:abc/schemas/parser-ir-publication-preservation.schema.json > /tmp/schema-0.3.0.json
diff -u /tmp/schema-0.2.0.json /tmp/schema-0.3.0.json
sha256sum ../abc/schemas/parser-ir-publication-preservation.schema.json \
  data/abc-schemas/schemas/parser-ir-publication-preservation.schema.json
python3 -c "import pathlib; print(pathlib.Path('data/abc-schemas/schemas/parser-ir-publication-preservation.schema.json').resolve())"
```

## Step 2: failing reproduction (current code, before the fix)

Ran the exact Phase 4 postactivation input set (matrix/source/delta/bundle/next-work
summaries, unchanged) through `just parser-ir-publication-coverage-report`
with `AB_AAT_RUN_SET`/`SORANOHA_WORKSPACE_ROOT` unset, writing to scratch
paths, with the pin still at `ABC_PRESERVATION_SCHEMA_VERSION = "0.2.0"`:

- `custom_contract.verdict`: `CUSTOM_CONTRACT_CANDIDATE_PROVIDED` (reproduces
  the defect)
- `custom_contract.schema_version`: `0.3.0` (the file, unchanged)
- `custom_contract.trusted_schema_path_match`: **`true`** — this is an
  empirical correction to the task brief's root-cause statement. The brief's
  planning-time analysis assumed the coverage recipe's default
  `CUSTOM_CONTRACT_SCHEMA` (the vendored `data/abc-schemas/schemas/...` path)
  would resolve to a *different* canonical path than
  `TRUSTED_ABC_PRESERVATION_SCHEMA_PATH` (the abc-side `abc/schemas/...`
  path), making `trusted_schema_path_match` false. In this worktree that is
  no longer true: commit `9a5efc6e` (2026-07-06, predating this task)
  already replaced the vendored schema directory with a symlink into
  `abc/schemas`, so the two paths already resolve identically. The **sole**
  live defect reproduced here is the version-pin mismatch
  (`ABC_PRESERVATION_SCHEMA_VERSION == "0.2.0"` vs. the file's actual
  `"0.3.0"`).
- `closure_gaps`: `admitted_by_custom_contract: 0`, `admitted_by_tei_profile:
  0`, `classified_but_not_admitted: 151`
- Top-level verdict: `IR_PUBLICATION_COVERAGE_BLOCKED_CLASSIFIED_GAPS`

## Step 3: fixes applied

1. `reports/parser-ir/publication-coverage.py:41` — bumped
   `ABC_PRESERVATION_SCHEMA_VERSION` from `"0.2.0"` to `"0.3.0"`, with a
   one-line comment citing this report as the reviewed-rotation record.
2. `ab-validator/justfile`'s `parser-ir-publication-coverage-report` recipe —
   changed the `CUSTOM_CONTRACT_SCHEMA` default from the hardcoded vendored
   path to empty (`""`), and changed the arg-building logic so that when
   empty, the recipe resolves `--custom-contract-schema` to
   `{{abc_repo_root}}/schemas/parser-ir-publication-preservation.schema.json`
   (the ABC-side trusted path directly), rather than an ab-validator-relative
   vendored path. When the caller explicitly overrides
   `CUSTOM_CONTRACT_SCHEMA`, the previous repo-root-relative behavior is
   preserved. This closes the path-mismatch hazard permanently (independent
   of the symlink), matching the brief's stated root cause even though it
   was not the empirically live defect in this worktree today.
   - The script's own `--custom-contract-schema` argparse flag was
     deliberately **left without a default** (i.e., still `None` when
     omitted). Adding a default there, as one candidate "smaller diff"
     the brief floated, would have flipped several existing smoke-test
     invocations that intentionally omit the flag to test
     `CUSTOM_CONTRACT_MISSING`/no-contract-provided behavior
     (`tests/parser-ir-publication-coverage-smoke.sh` lines 479, 567, 856),
     silently confirming a contract they never asked for. The recipe-level
     fix achieves the required outcome without that regression risk.

## Step 4: regenerated summary (after the fix)

Regenerated via the same `just parser-ir-publication-coverage-report`
invocation (same input set, unset ambient env), writing to:

- `docs/superpowers/reports/2026-07-12-abc-contract-0.3.0-confirmation-coverage.summary.json`
- `docs/superpowers/reports/2026-07-12-abc-contract-0.3.0-confirmation-coverage.md`

Observed verdicts:

- `custom_contract.verdict`: `CUSTOM_CONTRACT_CONFIRMED_BY_ABC_INTEGRATION`
  (`schema_version: 0.3.0`, `trusted_schema_path_match: true`)
- `source_region_contract.verdict`: `SOURCE_REGION_CONTRACT_CONFIRMED_BY_ABC_INTEGRATION`
  (unchanged from Phase 4 postactivation baseline)
- `tei_profile_contract.verdict`: `TEI_PROFILE_CONTRACT_CONFIRMED_BY_ABC_INTEGRATION`
  (record_class `tei_profile_projection`; constructs `accent`,
  `figure_metadata`, `heading_jisage_structure`, `style_rendition` — all
  present, no `missing_record_classes`/`missing_constructs`)
- `publication_bundle_contract.verdict`: `PUBLICATION_BUNDLE_CONTRACT_CONFIRMED_BY_ABC_VALIDATION`
- Top-level verdict: **`IR_PUBLICATION_COVERAGE_COMPLETE`**
- `parser_evidence_coverage.verdict`: `FIVE_PARSER_EVIDENCE_COMPLETE`
- `source_authority_gate.gate_status`: `SOURCE_AUTHORITY_GATE_PASS`
- Three required zero counters
  (`unsupported_body_markup_occurrences`/`unknown_region_occurrences`/`unknown_unreviewed_occurrences`):
  all `0` (unchanged, frozen `2026-07-04-source-authority-representability.summary.json`
  input)
- `closure_gaps`: `admitted_by_custom_contract: 31`,
  `admitted_by_tei_profile: 120`, `classified_but_not_admitted: 0`,
  `true_unsupported_gaps: 0` — all 151 previously-classified rows now fold
  into one of the two admission lanes.

## Test evidence

- `nix develop .#aozora2html --command python3 -m pytest reports/aat-fidelity/tests reports/lib/tests reports/parser-conformance/tests reports/source-regions/tests -q`
  -> **251 passed**, including `reports/aat-fidelity/tests/test_verify_phase5_checkpoint.py`'s
  coverage-consuming C5-gate tests (`test_coverage_custom_contract_not_confirmed`,
  `test_coverage_evidence_not_complete`, `test_coverage_verdict_not_complete`,
  `test_coverage_source_authority_gate_fail`, 26 tests total in that file).
  (`nix build .#checks.x86_64-linux.reports-pytest` fails only because the
  pure build sandbox lacks a `git` binary on `PATH` — unrelated to this
  change; the same 42 tests fail identically before and after this task's
  edits.)
- `bash tests/parser-ir-publication-coverage-smoke.sh` — was already failing
  at HEAD before this task's changes, for a reason unrelated to the ABC
  preservation contract: `source_region_contract.policy_version` was
  hardcoded to the stale `"0.2.0"` even though `reports/lib/source_region.py`'s
  own pin had already been bumped to `"0.3.0"` by an earlier, unrelated
  commit (`75057f14`, "fix(instruments): accept source-region policy 0.3.0
  (reviewed rotation)"); a second stale assertion
  (`missing_measurement_split_classes == ["colophon_metadata",
  "terminal_provenance"]`) had the same root cause — the abc-side policy
  file's `measurement_status` for those two classes flipped to `"measured"`
  in the same earlier rotation. Both are one-line literal corrections to
  match already-correct runtime behavior (not a design change); fixing them
  was necessary to let the script reach and actually exercise the
  ABC-preservation-contract-confirmed code path this task changes. With
  those two corrected plus two new assertions pinning the 0.3.0 rotation
  (`custom_contract.schema_version == "0.3.0"`,
  `custom_contract.trusted_schema_path_match == true`), the full smoke
  script now passes end to end: **exit 0**.
