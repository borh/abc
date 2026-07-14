# Task 4 report — Generate and validate immutable comparison reports

Status: **DONE_WITH_CONCERNS**

Branch: `feat/parser-release-qualification`
Commit: `4f61e0a0` — `docs(parser): publish preregistered existing-parser comparison`
(pushed to hinoki `08111458..4f61e0a0`).

Test summary: crate = 21/21 passing (16 contract + 5 generation/drift), cargo
clippy + fmt clean; ABC focused kaocha = 18/18 passing (incl. the new positive
registration test); all four heavy Nix gates green on hinoki
(cargo-check=0, cargo-clippy=0, cargo-fmt=0, clj-kondo=0).

---

## What was built (the acceptance-floor core, committed)

A deterministic report generator inside the existing Task-2 IR crate
`ab-validator/crates/ab-parser-study-report` — the frozen IR is reused, not
forked.

New/changed files:

- `src/generate.rs` (new) — pure function
  `generate_reports(run_manifests_json, preregistration_json) -> GeneratedReports`.
  Consumes the committed Task-3 compact run manifests and the frozen
  preregistration `.json`, builds a 108-row `StudyReport` via the public IR
  constructors, and renders both representations.
- `src/bin/generate-parser-study-report.rs` (new) — CLI that writes the two
  committed report files.
- `src/lib.rs` — one additive line: `pub mod generate;` (frozen IR types
  untouched).
- `tests/report_generation.rs` (new) — RED→GREEN generation + byte-identical
  drift test.
- `reports/parser-study/reports/aozora-parser-neutral-comparison-2026-07/comparison-result.json`
  (new) — machine-readable `StudyReport` IR.
- `reports/parser-study/reports/aozora-parser-neutral-comparison-2026-07/comparison-report.md`
  (new) — narrative report.
- `abc/src/abc/tools/malli.clj` — added `:neutral-comparison` evidence class and
  an optional `:study_contract` sha256 field to `::parser-evidence-entry`, plus
  a valid example.
- `abc/data/parser-evidence-citations.edn` — registered both reports as
  `:neutral-comparison` evidence, each bound to the frozen study contract by
  hash.
- `abc/test/abc/tools/parser_evidence_test.clj` — new positive test
  `neutral-comparison-reports-are-registered-with-study-contract-test`.

### Aggregation is exactly preregistered and provenance-tied

- **Only parse-completion robustness is `measured`**, because it is the only
  axis backed by the committed parse-outcome counts. For each of the 5
  executable candidates × 2 modes the row carries the exact preregistered
  denominator (the pinned `aozorabunko-source-snapshot` corpus, unit = work,
  17886), numerator = successes, failures/timeouts retained in the denominator.
  The generator errors (`MissingRun`, `DenominatorMismatch`) if a measured row
  cannot be tied to a raw manifest lane or if the outcome sum ≠ the pinned
  inventory size — i.e. it rejects any measured row missing raw provenance.
- Native and adapter-normalized are separate rows (separate provenance stacks:
  `[native_parser]` vs `[native_parser, adapter]`); adapter behavior is never
  credited as native.
- **Wilson 95% score intervals** are computed deterministically in Rust and
  rendered in the narrative (the frozen IR has no interval field, so the machine
  report carries only the exact counts — the primitive — and the narrative
  carries the derived interval; the frozen contract is not mutated).
- **Every axis without committed raw data is an explicit missing row, never a
  zero.** Construct-coverage, fidelity, diagnostics, spans, performance,
  maintenance, packaging, and license are emitted `status=failed`,
  `missingness=unavailable`, each with a caveat naming the exact blocker (e.g.
  "No committed construct-recognition oracle …"). `aozora-parser.js` is
  `build-failure` on every axis (frozen exclusion — no reproducible build);
  `ab-aozora` is `unavailable` on every axis (custom baseline, measured
  separately in the Task-5 appendix; ownership grants no comparison pass).
- **No unqualified aggregate score / overall winner.** The Interpretations
  section is explicitly labelled as interpretation and states no ranking; the
  Use-case sensitivity section requires every weighting to name its downstream
  use case.

### Narrative sections (all present and separated)

Observations (measured facts only, exact counts + denominators + Wilson CIs +
provenance hashes, native and adapter lanes in separate tables) · Interpretations
(labelled, no winner) · Limitations · Missing data (every missing axis/candidate
with its blocker) · Use-case sensitivity (bounded, since one axis is measured).

### Byte-identical regeneration / drift test

`tests/report_generation.rs`:
- `generation_is_byte_identical_across_runs` — two generations are byte-equal.
- `committed_reports_match_regeneration_from_raw_manifests` — the committed
  on-disk files equal a fresh regeneration from the committed manifests.
- `machine_report_is_a_valid_complete_study_report` — 108 rows, schema-valid
  against the frozen `parser-comparison-result.schema.json`.
- `robustness_rows_carry_exact_preregistered_parse_completion_counts`,
  `axes_without_committed_raw_data_are_missing_not_zero`.

### Neutral evidence registration (never admission)

New evidence class `:neutral-comparison`, structurally distinct from the three
admission/release classes (`:conversion-compatibility`, `:parser-selection`,
`:comparator-oracle`), so any predicate requiring one of those rejects it. Both
reports are registered with `:study_contract` =
`sha256:8715c30f…09500fbe` (the frozen preregistration `.json` hash). The
positive test asserts both reports register, content-address to their committed
hashes, carry the study-contract hash, and are not an admission/release class.
Task 6 can now add the negative rejection tests against this same type.

Report file hashes (registered in the citations index):
- `comparison-result.json` = `sha256:e04d737109381802a229145b22526178c13e8d6b8222530aa5e77b0d2c4ab306`
- `comparison-report.md` = `sha256:d97a83ef358cc9d6ad8714c381feccaa6af5018dd6784e7b72c46298cf56c8bb`

---

## Measured robustness result (folded in, real)

Parse completion = successes / 17886 (pinned corpus, failures+timeouts retained):

| Candidate | native succ. | adapter succ. |
| --- | ---: | ---: |
| aozora | 17885 (1 fail) | 17886 |
| aozora2 | 17886 | 17874 (12 timeout) |
| aozora-rs | 17886 | 17886 |
| aozora2html | 17886 | 17886 |
| aozora-epub3 | 17836 (50 fail) | 17773 (113 fail) |

These are the actual outcomes from Task 3's frozen executor runs — a genuine
corpus-scale (17886-work) robustness measurement, with native and adapter lanes
kept separate and Wilson intervals published in the narrative.

---

## Stretch attempt (fixture-scale) — genuinely attempted, honestly bounded

Per the "attempt real measurements" mandate I attempted the committed-fixture
axes on hinoki after building the pinned parser/adapter binaries
(`.#aozora2-adapter .#aozora2html-adapter .#aozora-epub3-adapter
.#aozora-rs-adapter .#upstream-parser-aozora2`, all cached from the prewarm).

**Malformed-input robustness fixture (`parser-comparison-robustness-v1.json`,
4 cases):** I ran the 4 cases through 8 readily-invocable candidate/mode lanes
via stdin using the frozen `required_argv` from the run manifests. The raw
output was inconsistent (aozora2 / aozora-rs succeeded on all 4; aozora2html and
the epub3 adapter *failed all 4*). I ran a **control** — a known-valid input
through the same binaries — and it *also* failed:

```
aozora2html --mode html : "error: --xhtml is required for --mode html"
aozora2html --mode aat  : "error: --source is required for --mode aat"
aozora-epub3 --mode aat : "--source required for --mode aat"
```

So those "failures" are **invocation artifacts, not malformed-input rejection**.
The frozen protocol runs each lane through `neutral_executor.py` with the exact
per-execution command vectors and materialization attestations, which live in
an *untracked resolver JSON* (documented in the runs `README.md`), not in the
committed fixtures. The committed fixtures define the cases but **not** the
executable invocation. Reproducing a valid, comparable malformed-fixture metric
therefore requires the Task-3 executor harness + resolver, which is not
committed. Reporting my ad-hoc numbers would be fabrication, so I did **not**
fold them in — this vindicates the core report's decision to keep the malformed
fixture out of the measured robustness count with a caveat.

**Diagnostics fixture:** correctly left `missing`. Producing a valid metric
needs per-parser diagnostic capture plus a cross-parser severity/span-matching
oracle (heterogeneous native diagnostic schemas) that the committed fixture does
not define.

**Performance fixture (largest-six):** correctly left `missing`. The committed
manifests carry parse-outcome/timeout counts but no per-repetition wall time,
and host comparability is `unavailable` for every lane except the aozora-rs
native replacement, so no Kaplan–Meier/bootstrap latency statistic is derivable.

Net: the genuine fixture-scale measurement that *is* real and folded in is the
17886-work corpus robustness; the small fixture axes remain `missing` because a
valid metric requires instruments/oracles/harness the committed fixtures do not
define — exactly the integrity rule's `missing`-with-a-caveat outcome, not
fabrication.

---

## Verification — exact commands and outputs

Local (light unit tests, per the hinoki guide):

- `cargo test -p ab-parser-study-report`
  → `16 passed` (contract) + `5 passed` (generation) + doctests `0`, 0 failures.
- `cargo clippy -p ab-parser-study-report --all-targets` → clean (no warnings).
- `cargo fmt -p ab-parser-study-report -- --check` → exit 0 (after `cargo fmt`).
- `clj-kondo --lint src/abc/tools/malli.clj test/abc/tools/parser_evidence_test.clj`
  → `errors: 0, warnings: 0`.
- `bin/kaocha --focus abc.tools.parser-evidence-test --focus abc.tools.malli-test`
  → `18 tests, 59 assertions, 0 failures.` (includes the new positive test
  `neutral-comparison-reports-are-registered-with-study-contract-test`).

Heavy gates on hinoki (`git push hinoki HEAD:feat/parser-release-qualification`
first; run from the worktree):

- `nix build ./ab-validator#checks.x86_64-linux.cargo-fmt` → `FMT_EXIT=0`.
- `nix build ./ab-validator#checks.x86_64-linux.cargo-check` → `CHECK_EXIT=0`.
- `nix build ./ab-validator#checks.x86_64-linux.cargo-clippy` → `CLIPPY_EXIT=0`.
- `nix build ./abc#checks.x86_64-linux.clj-kondo` → `KONDO_EXIT=0`.

(The `experimental Nix feature 'nix-command' is disabled … failed to copy built
paths to … Harmonia cache` lines are a **post-build hook** cache-push warning on
hinoki, not a build result; every check derivation built with exit 0.)

The hinoki worktree was clean before and after (`git status --short` empty); the
stretch probe wrote only to `/home/bor/*` and `/tmp`, never into the tree, and
was cleaned up.

---

## Concerns

1. **Pre-existing governance test errors on this branch (not caused by Task 4).**
   `abc.tools.adr-evidence-capture-test` has 7 errors of the form
   `:operational-input-set-mismatch … :missing ["src/abc/tools/parser_maintenance_evidence.clj"]`.
   I confirmed these are pre-existing by stashing my three ABC edits and
   re-running: the identical error is present on the clean tree. They are a
   namespace-closure drift unrelated to this task (`parser_maintenance_evidence.clj`
   is a file I never touched). The brief scoped ABC verification to the focused
   evidence kaocha tests + `clj-kondo`, all of which pass; I did not attempt to
   fix this separate governance drift (ADR/governance repair is Task 6 territory).

2. **Hash-pinned governance snapshots will need regeneration for the two edited
   ABC source/data files.** `abc/src/abc/tools/malli.clj` and
   `abc/data/parser-evidence-citations.edn` appear (by content hash) in several
   `docs/evidence/adr-runs/*.json` governance snapshots. No current unit test
   asserts live file content against those pinned hashes (the operational
   validators check path-sets and namespace closures, not content hashes — which
   is why the focused suites pass), so nothing regresses now. But a future
   artifact-backed-governance capture will show these two files' hashes drifted
   and the snapshots should be recaptured (again, Task 6 governance territory).
   Introducing `:neutral-comparison` + registering the reports necessarily edits
   these files — the pre-existing `historical-…-not-neutral-study-test`
   explicitly anticipates the `:study_contract` / `:neutral-comparison` shape,
   confirming this is the intended design.

3. **Fixture-scale axes remain `missing` by design, not by omission.** The
   diagnostics/performance/malformed-robustness axes could not yield a valid
   metric from the committed fixtures alone (see the stretch section). This is
   the correct integrity-rule outcome, but it means the report measures exactly
   one axis (robustness/parse-completion). If a future task commits the executor
   resolver + oracles, those axes can be folded in and the report regenerates.
