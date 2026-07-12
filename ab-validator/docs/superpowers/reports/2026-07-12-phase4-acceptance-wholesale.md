# Phase 4 Wholesale Acceptance Gate (Task 20, Ceremony Steps 5-6)

Status: **CHECKPOINT OK**

Activation commit: `1333b43dd103480d9064935cd1cf3027706531ee`
C3 candidate: `c2b9b396271755bc0b898fdaed17c9c9cbe4666d` (ab-aozora 0.4.0)
C4 candidate: `27772b1b75c9ceeb0b724095bbbb47f774f3a275` (ab-aozora 0.5.0)
Mapping: `0.3.0` / `sha256:7249cd727ef2da90dcd591e6009bead9235fe1c140697ee6bd70aacd9e85ee40`

This is the merge-precondition gate for `feat/parser-fork-phase4`. It
re-verifies everything Tasks 15-19 produced (the six C3/C4 gate summaries,
both conversion audits, the terminal-provenance/colophon split, the
admission capture, and the activation commit itself) with a fresh
fail-closed verifier, plus re-runs Task 16's coverage invocation against
the post-activation tree.

## Step 1: Post-activation coverage re-verification

Re-ran Task 16 Step 1's exact invocation (`reports/parser-ir/publication-coverage.py`
with the justfile's `parser-ir-publication-coverage-report` recipe's verbatim
inputs — legacy v1/0.2.8 mapping, the frozen 2026-07-04 source-authority and
matrix summaries, per Task 16's own reconciliation of the brief's inline flag
list against the justfile), with outputs renamed to
`2026-07-12-phase4-postactivation-coverage.{summary.json,md}`:

```
python3 reports/parser-ir/publication-coverage.py \
  --parser-ir-schema data/abc-schemas/schemas/parser-ir.schema.json \
  --mapping data/aat-to-parser-ir-mapping-v1.json \
  --source-summary docs/superpowers/reports/2026-07-04-source-authority-representability.summary.json \
  --matrix-summary docs/superpowers/reports/2026-07-04-tei-eaj-generated-matrix-comparison.summary.json \
  --source-delta-summary docs/superpowers/reports/2026-07-05-plain-prose-source-delta.summary.json \
  --summary-json docs/superpowers/reports/2026-07-12-phase4-postactivation-coverage.summary.json \
  --report-md docs/superpowers/reports/2026-07-12-phase4-postactivation-coverage.md \
  --custom-contract-schema data/abc-schemas/schemas/parser-ir-publication-preservation.schema.json \
  --bundle-validation-summary docs/superpowers/reports/2026-07-06-publication-bundle-full-matrix-validation.summary.json \
  --next-work-summary docs/superpowers/reports/2026-07-06-aozora-publication-next-work.summary.json
```

Exit 0.

### Required assertions

```
PASS: unsupported_body_markup_occurrences = 0
PASS: unknown_region_occurrences = 0
PASS: unknown_unreviewed_occurrences = 0
PASS: parser_evidence_coverage.verdict = 'FIVE_PARSER_EVIDENCE_COMPLETE'
PASS: ab-aozora present in the post-activation lane list
```

The three occurrence counters live in `source_region_coverage` inside the
frozen `2026-07-04-source-authority-representability.summary.json` input
(unchanged since Task 16 — verified directly, byte-for-byte the same file),
and transitively via this run's own
`source_authority_gate.gate_status == "SOURCE_AUTHORITY_GATE_PASS"` with
`unallowlisted_unknown_markers_total == 0` (same methodology Task 16 used).

`parser_evidence_coverage.verdict` reads `FIVE_PARSER_EVIDENCE_COMPLETE` in
this run. Its `required_parsers`/`observed_rows_by_parser` lane list is
`aozora2html, aozora-epub3, aozora-rs, aozora2, aozora` — the **legacy**
name, not `ab-aozora`. This is `REQUIRED_PARSERS` in
`reports/parser-ir/publication-coverage.py`, a frozen tuple keyed to the
2026-07-04 five-way TEI-EAJ matrix, which predates the C4 activation rename
by design (that matrix is comparison-lane evidence, not live-lane evidence,
and is explicitly out of scope for this task per the "known environmental
fact" below). So `ab-aozora` presence is asserted instead against the live,
post-activation `reports/aat-fidelity/run-sets/current.json`, whose
`adapters` keys are exactly `aozora2html, aozora-epub3, aozora-rs, aozora2,
ab-aozora` (five lanes, confirmed by both `git show` on the activation
commit and a direct read of the live file — see the verifier's activation
check below), and is confirmed identically in
`docs/handoffs/ir-publication-coverage-contract.md`'s own prose ("Five
parser lanes are present in the generated matrix: ... and `ab-aozora`.
`ab-aozora` is the designated publication lane (activated 2026-07-12)").

### Top-level verdict is not COMPLETE — traced, not a new regression

The report's top-level verdict is
`IR_PUBLICATION_COVERAGE_BLOCKED_SOURCE_REGION_CONTRACT_MISSING`
(`source_region_contract.verdict == SOURCE_REGION_CONTRACT_CANDIDATE_PROVIDED`,
not `CONFIRMED_BY_ABC_INTEGRATION`). This differs from Task 16's own
pre-admission coverage run, which read `SOURCE_REGION_CONTRACT_CONFIRMED_BY_ABC_INTEGRATION`
and blocked one stage later, at `IR_PUBLICATION_COVERAGE_BLOCKED_CLASSIFIED_GAPS`
(`closure_gaps.classified_but_not_admitted.count == 151`, unchanged in this
run too).

Mechanistic trace: `publication-coverage.py`'s `source_region_contract_block()`
compares the live `abc/data/source-region-publication-policy-v0.json`
(resolved via `policy_dir()`, which prefers the sibling `abc/` checkout
over ab-validator's own synced copy) against a hardcoded expected version
in `reports/lib/source_region.py`: `SOURCE_REGION_POLICY_VERSION = "0.2.0"`.
Commit `8660511e` ("feat(registry): ab-aozora 0.5.0 C4 row, admission
:admitted captured; source-region split measured", 2026-07-12 — landed
after Task 16's own `3df9389b` and before Task 18's bundle-validation
commit `407d42fd`, `38b12b45`'s Clojure-side fix, and the activation
commit `1333b43d`) legitimately rotated
`abc/data/source-region-publication-policy-v0.json`'s `policy_version` to
`0.3.0` as part of landing the terminal-provenance/colophon measurement
split (the same feature this checkpoint's confinement gate depends on).
Commit `38b12b45` ("fix(abc): measurement-split validator accepts measured
dispositions") fixed the **Clojure-side** design-bundle validator
(`abc.tools.source-region-contract/policy-errors`) to accept the rotated
0.3.0 policy's `measurement_status` values — this is the exact
`source-region-policy-measurement-status-drift` finding Task 18's bundle
validation report recorded as `"for": "controller / follow-up task"` and
`"not_in_scope_of_task_18": true`. Nobody has yet re-pinned the **Python**
consumer's expected version (`reports/lib/source_region.py`'s
`SOURCE_REGION_POLICY_VERSION` constant) to `0.3.0` — a distinct,
still-open follow-up in the same family, not something this task's file
scope (`verify-phase4-checkpoint.py`, its tests, and the two report pairs)
authorizes touching, and not caused by the activation commit (`8660511e`
and `38b12b45` are both ancestors of the activation commit, i.e. already
in the tree before Task 20 started).

Per this task's binding values, the three required zero counters are the
acceptance-criteria gate; this BLOCK is explained (traced to a specific,
pre-existing, already-partially-fixed version-pin lag, not an unexplained
regression), so it is documented here rather than escalated. The
CHECKPOINT verifier's own coverage check (below) asserts the three
counters (transitively) and `FIVE_PARSER_EVIDENCE_COMPLETE`, not the
`source_region_contract`/top-level verdict strings, for exactly this
reason.

## Step 2-3: Verifier

`reports/aat-fidelity/verify-phase4-checkpoint.py` clones the Phase 3
verifier's skeleton (`reports/aat-fidelity/verify-phase3-checkpoint.py`)
for two stages (`c3`: migration/conformance/perf, `c4`:
confinement/conformance/perf) and adds:

- **Admission**: `--admission-capture` must contain `:status :admitted`;
  `--admission-cmd` is re-run live via subprocess (`shlex.split`, `cwd`
  set to the `abc/` directory since the Clojure deps context lives there)
  and must exit 0 with `:status :admitted` in its stdout — a stale capture
  cannot pass alone.
- **Activation commit binding**: `git show --name-only --pretty=format: SHA`
  (repo-root `cwd`, derived from `--run-set`'s own git worktree via
  `git -C $(dirname run_set) rev-parse --show-toplevel` — the monorepo
  root one level above `ab-validator/`, not the `ab-validator` checkout
  itself) — changed paths must be a superset of `ACTIVATION_REQUIRED` and
  every other changed path must start with an `ACTIVATION_ALLOWED_PREFIXES`
  entry; `git show SHA^:ab-validator/reports/aat-fidelity/run-sets/current.json`
  must carry `aozora` and not `ab-aozora`; `git show SHA:...current.json`
  must carry `ab-aozora` (with `expected.adapter_version_contains == --c4`)
  and not `aozora`, with exactly five adapters; the live `--run-set` file
  is cross-checked against that same child shape.
- **Confinement**: `classes.identical + classes.source_note_appended ==
  17886` and `classes.source_note_appended == split.works_with_terminal_provenance`
  (both read `17735`; the split summary is Revision 2, per
  `2026-07-12-terminal-provenance-colophon-split.summary.json`'s
  `revision_2` block).
- **Coverage**: `source_authority_gate.gate_status ==
  "SOURCE_AUTHORITY_GATE_PASS"` with `unallowlisted_unknown_markers_total
  == 0`, and `parser_evidence_coverage.verdict ==
  "FIVE_PARSER_EVIDENCE_COMPLETE"`.

`ACTIVATION_ALLOWED_PREFIXES` includes `abc/test/abc/tools/materialize_import_test.clj`
(exact-path match, not a directory prefix) in addition to
`abc/examples/ab-validator-output/`: the activation commit's own message
documents this as a controller-ruled fixture-coupled fix (a stale
`mapping-hash` literal pinned against the ab-validator-output fixtures
Task 18 refreshed), folded into the atomic commit so the tree stays
self-consistent — not a stray or unrelated change.

Test harness: `reports/aat-fidelity/tests/test_verify_phase4_checkpoint.py`
models Phase 3's fixture-builder pattern (`write_all`/`mutate`) for the
gate/audit/split/coverage JSON fixtures, plus a dedicated
`build_activation_repo()` helper that materializes a real scratch git
repository (`git init` + two commits: parent without `ab-aozora` /
with `aozora`, child with `ab-aozora` / without `aozora`) to exercise the
`git show`-based activation checks end-to-end rather than mocking them.

## Step 4: Run

### Unit tests

```
python3 -m pytest reports/aat-fidelity/tests/test_verify_phase4_checkpoint.py -v
```

16 passed:

```
test_all_pass PASSED
test_verdict_fail PASSED
test_commit_mismatch PASSED
test_bin_mismatch PASSED
test_wrong_version_pattern PASSED
test_duplicate_candidates PASSED
test_migration_class_sum PASSED
test_confinement_wrong_mode PASSED
test_confinement_append_count_mismatch PASSED
test_audit_failed_files PASSED
test_audit_wrong_mapping PASSED
test_admission_rerun_fails PASSED
test_activation_commit_extra_path PASSED
test_activation_parent_already_active PASSED
test_activation_child_missing_lane PASSED
test_acceptance_counter_nonzero PASSED
```

### Real checkpoint (committed Phase 4 artifacts)

```
python3 reports/aat-fidelity/verify-phase4-checkpoint.py \
  --c3-gates docs/superpowers/reports/2026-07-12-phase4-c3-delta.summary.json \
             docs/superpowers/reports/2026-07-12-phase4-c3-conformance-gate.summary.json \
             docs/superpowers/reports/2026-07-12-phase4-c3-perf.summary.json \
  --c4-gates docs/superpowers/reports/2026-07-12-phase4-c4-confinement.summary.json \
             docs/superpowers/reports/2026-07-12-phase4-c4-conformance-gate.summary.json \
             docs/superpowers/reports/2026-07-12-phase4-c4-perf.summary.json \
  --audit-c3 docs/superpowers/reports/2026-07-12-ab-aozora-phase4-c3-conversion-audit.summary.json \
  --audit-c4 docs/superpowers/reports/2026-07-12-ab-aozora-phase4-c4-conversion-audit.summary.json \
  --split docs/superpowers/reports/2026-07-12-terminal-provenance-colophon-split.summary.json \
  --admission-capture docs/superpowers/reports/2026-07-12-phase4-admission-report.txt \
  --admission-cmd "clojure -M:abc/aat-compat-admission -- --candidates <repo-root>/ab-validator/docs/superpowers/reports/2026-07-12-ab-aozora-phase4-c4-compat.edn" \
  --activation-commit 1333b43dd103480d9064935cd1cf3027706531ee \
  --run-set reports/aat-fidelity/run-sets/current.json \
  --coverage docs/superpowers/reports/2026-07-12-phase4-postactivation-coverage.summary.json \
  --c3 c2b9b396271755bc0b898fdaed17c9c9cbe4666d \
  --c4 27772b1b75c9ceeb0b724095bbbb47f774f3a275
```

Output:

```
CHECKPOINT OK
```

Exit code: `0`.

### Negative probe (`--c4` set to the C3 sha)

```
python3 reports/aat-fidelity/verify-phase4-checkpoint.py \
  --c3-gates docs/superpowers/reports/2026-07-12-phase4-c3-delta.summary.json \
             docs/superpowers/reports/2026-07-12-phase4-c3-conformance-gate.summary.json \
             docs/superpowers/reports/2026-07-12-phase4-c3-perf.summary.json \
  --c4-gates docs/superpowers/reports/2026-07-12-phase4-c4-confinement.summary.json \
             docs/superpowers/reports/2026-07-12-phase4-c4-conformance-gate.summary.json \
             docs/superpowers/reports/2026-07-12-phase4-c4-perf.summary.json \
  --audit-c3 docs/superpowers/reports/2026-07-12-ab-aozora-phase4-c3-conversion-audit.summary.json \
  --audit-c4 docs/superpowers/reports/2026-07-12-ab-aozora-phase4-c4-conversion-audit.summary.json \
  --split docs/superpowers/reports/2026-07-12-terminal-provenance-colophon-split.summary.json \
  --admission-capture docs/superpowers/reports/2026-07-12-phase4-admission-report.txt \
  --admission-cmd "clojure -M:abc/aat-compat-admission -- --candidates <repo-root>/ab-validator/docs/superpowers/reports/2026-07-12-ab-aozora-phase4-c4-compat.edn" \
  --activation-commit 1333b43dd103480d9064935cd1cf3027706531ee \
  --run-set reports/aat-fidelity/run-sets/current.json \
  --coverage docs/superpowers/reports/2026-07-12-phase4-postactivation-coverage.summary.json \
  --c3 c2b9b396271755bc0b898fdaed17c9c9cbe4666d \
  --c4 c2b9b396271755bc0b898fdaed17c9c9cbe4666d
```

Output:

```
CHECKPOINT FAIL: candidate commits --c3/--c4 must be distinct
```

Exit code: `1`. (`<repo-root>` above is
`/home/bor/Projects/soranoha/.worktrees/parser-fork-phase4`, substituted
literally in the actual invocations.)

## Acceptance criteria table

Every criterion in `docs/superpowers/specs/2026-07-06-comprehensive-parser-acceptance-criteria.md`,
mapped to its Phase 4 evidence:

| # | 2026-07-06 criterion | Phase 4 evidence file | Result |
|---|---|---|---|
| 1 | Every source inventory row parsed/raw-preserved/apparatus-classified/diagnostic | `2026-07-12-ab-aozora-phase4-c4-conversion-audit.summary.json` (17886/17886/0), `2026-07-12-phase4-c4-confinement.summary.json` (classes sum to 17886) | PASS |
| 2 | `unsupported_body_markup_occurrences == 0` | `2026-07-04-source-authority-representability.summary.json` (frozen, unchanged) + `2026-07-12-phase4-postactivation-coverage.summary.json` (`source_authority_gate` PASS) | PASS |
| 3 | `unknown_region_occurrences == 0` | same | PASS |
| 4 | `unknown_unreviewed_occurrences == 0` | same | PASS |
| 5 | Reconciliation remains `SOURCE_REFERENCE_RECONCILIATION_COMPLETE` | `2026-07-12-phase4-preadmission-reconciliation.summary.json` (Task 16 Step 2) | PASS |
| 6 | Parser-IR validates against the current schema | `2026-07-12-ab-aozora-phase4-c{3,4}-conversion-audit.summary.json` (`parser_ir_schema_hash` recorded, 0 failures) | PASS |
| 7 | Parser-IR carries every body markup fact into an admitted lane | `2026-07-12-phase4-c3-delta.summary.json` (migration classes), `2026-07-12-phase4-c4-confinement.summary.json` (source-note-append), `2026-07-12-phase4-postactivation-coverage.summary.json` (`closure_gaps.true_unsupported_gaps.count == 0`) | PASS |
| 8 | Ruby/apparatus/notes/metadata/warnings/provenance excluded from plaintext | `2026-07-12-phase4-bundle-validation.md` (plaintext-omission assertion, Step 4) | PASS |
| 9 | Parser/adapter/mapping identity + schema hashes available to the bundle | All six `2026-07-12-phase4-{c3,c4}-*.summary.json` `candidate` blocks + both conversion audits' `mapping`/`compatibility_candidates` | PASS |
| 10 | Representative bundle validation passes | `2026-07-12-phase4-bundle-validation.summary.json` (`PHASE4_BUNDLE_VALIDATION_PASS_WITH_PREEXISTING_BLOCKED_FINDING`, `validation.representative`) | PASS |
| 11 | Full-scope bundle validation passes before lane replacement | same, `validation.full_scope` | PASS |
| 12 | Parser-IR/TEI/plaintext/preservation/source-region/manifests agree under ABC validation | same + `2026-07-12-phase4-postactivation-coverage.summary.json` `publication_bundle_contract.verdict == PUBLICATION_BUNDLE_CONTRACT_CONFIRMED_BY_ABC_VALIDATION` | PASS |
| 13 | Every custom preservation fact has schema/hash/owner/pointer/validation | `2026-07-06-publication-bundle-full-matrix-validation.summary.json` (285-row full matrix, referenced unchanged by the postactivation coverage run) | PASS |
| 14 | Explicit bounded performance budgets | `2026-07-12-phase4-{c3,c4}-perf.summary.json` (`threshold_pct: 10`) | PASS |
| 15 | DNF within budget recorded as evidence | same (`new_timeouts: 0` — none occurred; budget/DNF machinery is the same measured-performance-run harness as Phase 3) | PASS |
| 16 | DNF not retried indefinitely | same harness (unchanged since Phase 3) | PASS |
| 17 | Performance reports include wall-time/CPU evidence | `2026-07-12-phase4-{c3,c4}-perf.runner.json` (per-work medians, machine info) | PASS |
| Admission rule | Next-work ledger cites measured evidence, closes named items | `2026-07-12-phase4-preadmission-next-work.md` — **8 CLOSED / 5 CARRIED** (Task 16); admission capture `:status :admitted` re-confirmed live (this task) | PASS |

Next-work citation detail (from Task 16's pre-admission report,
re-confirmed unchanged by this task):

- **CLOSED (8)**: `source_authority`, `publication_bundle_contract`,
  `source_reference_reconciliation` (completed_gates);
  `source_region_disposition_samples`, `text_policy_calibration`,
  `adapter_fidelity_worksets`, `tei_p5_mapping_dossiers`,
  `parser_acceptance_criteria` (next_work_items).
- **CARRIED (5)**: `ir_publication_coverage` completed_gate (see the
  version-pin trace above — expected drift, not a regression);
  `five_parser_conversion` completed_gate (single-lane C4 audit
  substituted per brief; the frozen five-way matrix still backs
  `FIVE_PARSER_EVIDENCE_COMPLETE`); bare-toggle marker forms (explicit
  Phase 4 non-goal); keigakomi 44-marker denominator residual (explicit
  Phase 4 non-goal); warigaki/kunten vocabulary (awaits its own ADR).

## Known environmental fact (not a blocker)

The four legacy comparison lanes' (`aozora-rs`, `aozora2`, `aozora2html`,
`aozora-epub3`) dumps are absent from hinoki `/db` — pre-existing, unrelated
to this task. Nothing in this checkpoint revalidates those paths; the
gate/audit/confinement evidence above is scoped entirely to the `ab-aozora`
C3/C4 candidates and the frozen 2026-07-04 five-way matrix (comparison
evidence only, not live-lane evidence).

## Concerns for the merge decision

1. **Source-region contract version-pin lag** (traced above in Step 1):
   `reports/lib/source_region.py`'s `SOURCE_REGION_POLICY_VERSION = "0.2.0"`
   has not been re-pinned to the abc-side policy's rotated `0.3.0` (commit
   `8660511e`), so `IR_PUBLICATION_COVERAGE_*` reads
   `BLOCKED_SOURCE_REGION_CONTRACT_MISSING` rather than `COMPLETE`. This
   predates the activation commit, is the same class of drift Task 16/18
   already flagged (`source-region-policy-measurement-status-drift`,
   explicitly marked "for controller / follow-up task"), and does not
   affect any of the three required zero counters, `FIVE_PARSER_EVIDENCE_COMPLETE`,
   or `ab-aozora` lane presence. Recommend a small follow-up task bump the
   Python-side pin once the 0.3.0 policy's semantics are deliberately
   reviewed (mirroring the mapping/custom-contract re-pin Task 16 already
   flagged as outstanding).
2. The activation commit (`1333b43d`) is explicitly branch-only per its
   own commit message ("Branch-only until Task 20's checkpoint passes; if
   Task 20 fails, this commit is reverted/dropped on the branch before any
   merge"). This checkpoint reads `CHECKPOINT OK`, so that condition is
   satisfied and the commit can stand.
