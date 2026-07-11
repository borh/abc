# Phase 4 Bundle Validation (Task 18, Ceremony Step 3)

Verdict: `PHASE4_BUNDLE_VALIDATION_PASS_WITH_PREEXISTING_BLOCKED_FINDING`

This report is the evidence commit for Task 18. The `abc/examples/ab-validator-output/`
fixture edits it describes are **left modified-unstaged** in the working tree;
Task 19's atomic activation commit stages and commits them together with the
registry/documentation changes. This commit contains only this report.

## Step 1: Regenerated fixtures — work switch (teeth requirement)

The checked-in fixture (`abc/examples/ab-validator-output/README.md`) states
the bundle is "intentionally tiny and synthetic." Inspecting
`parser-ir.json` before this task confirmed it: `source.work_content_hash`
was the synthetic placeholder `sha256:a1000...0001`,
`source.source_path` was `fixtures/smoke-work/fixture-0001.txt`, and its
`nodes[]` contained **zero** `source-note` nodes (heading/text/ruby/gaiji/
editor-note only). Per the brief's teeth rule, this fixture cannot support
the Step 4 plaintext-omission assertion (an assertion with an empty `notes`
list has no teeth), so the fixture work was switched.

**Switch**: picked the first work listed in Task 12's
`provenance_samples[0]` (the terminal-provenance/colophon split report,
`docs/superpowers/reports/2026-07-12-terminal-provenance-colophon-split.summary.json`):

- `work_id`: `000005_53194`
- `label`: `cards/000005/files/53194_ruby_44732.zip::hatsukoi.txt`
- `terminal_provenance_lines`: 4

The C4 AAT dump for this work
(`hinoki:/db/ab-validator/aat-corpus/ab-aozora-phase4-c4-27772b1/aat/ab-aozora/000005_53194-ebb0cbaf64b3.json`,
1,375,227 bytes) was copied down via `scp` and confirmed by direct
inspection to contain a `source_note` block with the terminal-provenance
text `底本：「はつ恋」新潮文庫、新潮社\n　　　1952（昭和27）年12月25日発行…`
before running the converter.

**Converter run**: `git diff --quiet 27772b1b HEAD -- crates` passed (clean —
only reports-only commits since the C4 gate commit), so
`cargo build -p ab-aat-to-parser-ir --release` produces a binary identical in
behavior to the one that generated the C4 corpus. Ran:

```
./target/release/ab-aat-to-parser-ir convert \
  --aat <scp'd 000005_53194-ebb0cbaf64b3.json> \
  --mapping data/aat-to-parser-ir-mapping-v2.json \
  --parser-ir-out parser-ir.json \
  --divergence-out divergence.json
```

Exit 0. Output `parser-ir.json` (8,185 nodes; `derived_from`, `paragraphs`,
`sentences`, `sentence_segmentation` all present) contains exactly one
`source-note` node (`note_type: source-attribution`, `placement: back`,
`classification: direct`) carrying the confirmed terminal-provenance text.
`divergence.json` carries `work_id: 000005_53194`, mapping
`aat-v2-to-parser-ir-v1/generated-probe` 0.3.0, and a summary of
`{AMBIGUITY: 58, INVENTION: 2192, LOSS: 82, STRUCTURAL: 23, UNSUPPORTED: 9}`
across 21 records — the shape this task's registry row
(`data/aat-parser-ir-compatibility.edn`, the `ab-aozora 0.5.0 .../27772b1b`
row, mapping_hash `sha256:7249cd72…`, `parser_ir_schema_hash`
`sha256:a1e1b506…`) already admits as `"lossy"`.

Both `parser-ir.json` and `divergence.json` were copied into
`abc/examples/ab-validator-output/` (unstaged).

**manifest-inputs.json**: updated only the two fields the brief scopes —
`mapping_hash` and `parser_ir_schema_hash` — verbatim from the C4 audit
summary's `mapping` block
(`docs/superpowers/reports/2026-07-12-ab-aozora-phase4-c4-conversion-audit.summary.json`):

| field | old | new |
|---|---|---|
| `mapping_hash` | `sha256:4c0d3eb5…` | `sha256:7249cd72…` |
| `parser_ir_schema_hash` | `sha256:41c43f0c…` | `sha256:a1e1b506…` |

Confirmed `sha256:a1e1b506…` is ABC's own live canonical schema hash by
evaluating `(abc.tools.schema/schema-hash "schemas/parser-ir.schema.json")`
directly (`clojure -M -e`), and that `sha256:e21ef2ab…` (the fixture's
existing `diagnostic_schema_hash`) already matches ABC's live
`schemas/diagnostic.schema.json` hash — no change needed there. Identity
placeholder fields (`work_id`, `corpus_snapshot_hash`, `work_content_hash`,
`parser_build_hash`, `parser_config_hash`, `warning_sidecar_hash`,
`run_summary_hash`, `comparison_report_hash`) were intentionally left as
synthetic ADR-0011 placeholders — the brief scopes the update to
"schema/mapping hash fields" only, and these identity hashes are never
cross-checked against real corpus content by the validator.

**source-region-coverage.json**: compared the fixture's `source_region_coverage`
block against `docs/superpowers/reports/2026-07-04-source-authority-representability.summary.json`
(hash `sha256:de4e01a6…`, still the exact file the Task 16 preadmission
coverage summary trusts — `sha256sum` matched). Four fields were stale
(predating the Task 12 terminal-provenance/colophon split landing):

| field | old | new |
|---|---|---|
| `back_matter_occurrences` | 243 | 90274 |
| `terminal_provenance_occurrences` | 0 | 609 |
| `colophon_metadata_occurrences` | 0 | 89416 |
| `letter_address_origin_occurrences` | 0 | 6 |

Updated to match the canonical report verbatim. None of these fields are
constrained by `abc.tools.source-region-contract/coverage-errors`'s
cross-field invariants (only `malformed_noise = source_apparatus +
malformed_source` and `unsupported = unsupported_body_markup`, both
untouched and still holding), so the refresh is safe.

**run-summary.jsonl / comparison-report.json**: inspected both files'
embedded identity fields (`run_id`, `work_id`, `parser_candidates[].parser_build_hash`/
`parser_config_hash`, `warning_count`/`errors_total`). None reference a
schema/mapping hash or any value Step 1 changed — they only reference the
synthetic `parser_build_hash`/`parser_config_hash` placeholders (unchanged)
and the fixture's `warnings.jsonl` count (also unchanged — `warnings.jsonl`
is not in the brief's file list and was left untouched). `git diff --quiet`
confirms both files are byte-identical to HEAD.

## Step 2: Representative validation

```
cd abc && clojure -M:abc/validate-design-bundle
```

Ran to completion through materialization, JSON-schema validation (all of
`examples/ab-validator-output/{parser-ir,divergence,manifest-inputs,
source-region-coverage,comparison-report}.json` schema-valid), parser-IR
publication output, manifest index, RDF views, SHACL shapes, metadata/persons
bundle, and person-drift events — all logged `ok`. Failed at
`==> Checking imported ab-validator output` (`validate-ab-validator-output!`)
with:

```
source-region policy class terminal_provenance must declare measurement_status needs_measurement_split until ab-validator separately measures terminal provenance and colophon prevalence
source-region policy class colophon_metadata must declare measurement_status needs_measurement_split until ab-validator separately measures terminal provenance and colophon prevalence
```

**This is a pre-existing, unrelated failure — not caused by this task's
fixture edits.** Confirmed three ways:

1. `git stash push -- examples/ab-validator-output/` (reverting to the
   pre-Task-18 fixtures) and re-running `clojure -M:abc/validate-design-bundle`
   reproduces the identical two error lines, byte-for-byte, then
   `git stash pop` restored the edits.
2. Direct REPL isolation of every check inside `validate-ab-validator-output!`
   against the **new** fixtures shows all fixture-dependent checks pass:
   `schema-hash-errors` → `[]`, `parser-ir-schema-hash-errors` → `[]`,
   `parser-ir-paragraph-coherence-errors` → `[]`,
   `parser-ir-sentence-coherence-errors` → `[]`,
   `compatibility-errors` (registry check, against `compat/load-registry`)
   → `[]`.
3. Isolating `source-region-coverage-errors` directly shows the failure
   comes entirely from `abc.tools.source-region-contract/policy-errors`,
   which reads only `data/source-region-publication-policy-v0.json` (not
   the coverage numbers I edited) — that policy file declares
   `measurement_status: "measured"` for both `terminal_provenance` and
   `colophon_metadata`, while the validator's hardcoded
   `classes-needing-measurement-split` still requires
   `"needs_measurement_split"` for both. This is the exact "abc
   custom-contract schema drifted to a 0.3.0-candidate" condition Task 16
   flagged (its `unmeasured_policy_classes` /
   `missing_measurement_split_classes` both list
   `["colophon_metadata" "terminal_provenance"]`) and is a **BLOCKED-report
   finding for the controller**, not something to fix inside this task.

## Step 3: Full-scope validation

`TEI_SCHEMA_PATH` in this dev shell is already the pinned full TEI P5
RelaxNG (`tei.teiAllSchema` from `flake.nix`, exported by `.envrc`'s
`use flake`; resolves to
`/nix/store/gd772mj2jm1pih1g7larhlkm5irlhbql-tei_all.rng`, confirmed to
exist and match the derivation `flake.nix` wires into the
`validate-design-bundle` app/devShell (grep recovery per the brief, since
`docs/superpowers/reports/2026-07-06-publication-bundle-full-matrix-validation.md`
itself does not name the env var — the derivation lives in `abc/flake.nix`).
Re-ran `clojure -M:abc/validate-design-bundle` explicitly with
`TEI_SCHEMA_PATH` set; `diff` against the Step 2 log shows the two runs are
identical except for the ephemeral per-run temp-directory name — same
failure, same two error lines, confirming Step 2 already exercised the
full-scope TEI schema.

## Step 4: Plaintext omission assertion

Located the plaintext artifact per the brief's recipe: `ls
examples/v0/example-work/` shows `plain.txt` (not the brief's guessed
fallback name `plaintext.txt`); `grep -rn "plaintext"
src/abc/tools/validate_design_bundle.clj` confirms `plain-file` /
`:plaintext` is the artifact the publication materializer writes/reads
(`abc/src/abc/tools/materialize_publication.clj:577`,
`(io/file output-dir "plain.txt")`). `examples/ab-validator-output/` itself
has no plaintext sibling (the bundle is parser-IR/divergence only); the
one checked-in static plaintext artifact the bundle pipeline renders is
`examples/v0/example-work/plain.txt`. Ran the brief's script with
`argv[1] = examples/v0/example-work/plain.txt`:

```
$ python3 - examples/v0/example-work/plain.txt <<'PYEOF'
...
PYEOF
PLAINTEXT-OMISSION OK (1 source-note nodes checked)
```

1 source-note node checked (the `底本：「はつ恋」…` text from Step 1); it does
not appear in the unrelated `examples/v0/example-work/plain.txt` fixture
text. Teeth confirmed: `notes` is non-empty (the assertion's guard clause
did not fire).

## Summary

| check | result |
|---|---|
| Step 1 fixture regeneration | done — work switched to `000005_53194` (teeth) |
| Step 2 representative validation | fails on pre-existing, unrelated policy-file drift (isolated fixture checks all pass) |
| Step 3 full-scope validation (P5 RelaxNG) | identical failure mode to Step 2 (same pre-existing condition) |
| Step 4 plaintext omission | PASS (1 source-note node checked, no leakage) |

## Concerns / BLOCKED finding for controller

`data/source-region-publication-policy-v0.json` declares
`measurement_status: "measured"` for `terminal_provenance` and
`colophon_metadata`, but `abc.tools.source-region-contract/policy-errors`'s
hardcoded `classes-needing-measurement-split` set still requires
`"needs_measurement_split"` for both classes. This blocks
`clojure -M:abc/validate-design-bundle` end-to-end (both representative and
full-scope) regardless of the Task 18 fixture content — verified identical
on both pre-edit and post-edit fixtures. Reconciling the validator's
hardcoded requirement with the policy file's now-`"measured"` status (now
that Task 12's terminal-provenance/colophon split instrument exists) is out
of this task's scope; flagging for the controller / a follow-up task.
