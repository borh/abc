# Task 7 report — Define and execute release qualification

Status: **DONE_WITH_CONCERNS** (gate machinery complete and green; release gate
honestly reports `not-qualified`, so ADR 0039 stays **Proposed** and the
`feat(parser): qualify project-owned parser for release` commit was NOT made, as
the admission≠release rule requires).

Branch: `feat/parser-release-qualification`. Final code HEAD: `052ee416`; this report was recorded at `bfcd91e7` and the branch merged to `main` at `1388022a`.

## Commits

| sha | message |
| --- | --- |
| `8c45cda6` | `feat(parser): add release-qualification gate` (corpus, predicates, evaluator + report schema, tests, ADR 0039 Proposed, regenerated adr-graph.mmd) |
| `fff062d1` | `docs(parser): record release-qualification gate execution` (captured measurements bundle + report JSON) |
| `052ee416` | `fix(abc): recapture ADR-graph evidence bundles for ADR 0039` (adr-graph-contract + diagram-registry-drift bundle recapture + registry re-hash) |

The `feat(parser): qualify project-owned parser for release` commit was
deliberately NOT made: the gate is not fully passing (below).

## One-line gate summary

Release predicates 5 **pass** / 0 fail / 4 **unavailable** (gate
`not-qualified`); ADR 0039 **Proposed**; `just validate-migration` exits **0** on
hinoki at `052ee416`.

## Deliverables

- **Pinned corpus** `abc/data/parser-release-qualification-corpus.edn`: 3
  deterministic workspace-tracked Aozora works (ruby / gaiji / both; work IDs
  `000001_1`, `000002_2`, `000003_3` from the `ab-index` fixtures), each with
  source path, sha256, category, reason, expected status. `:corpus_snapshot_hash`
  and `:list_hash` are recomputed and enforced.
- **Predeclared predicates** `abc/data/parser-release-qualification-predicates.edn`:
  fatal-failures, source-span-coverage, silent-drops, diagnostic-completeness,
  parser-IR-schema-validation, publication-structure, wall-time, memory,
  timeout-policy — each with an exact threshold fixed before measurement.
- **Evaluator + report schema** `abc/src/abc/tools/parser_release_qualification.clj`:
  measurement-agnostic; emits exact observed/expected + derived verdict; a missing
  instrument is `:unavailable` (never `:pass`); exact numeric comparison so
  `0.969` fails a `1.0` predicate. Consumes the Task-6 release evidence-class
  boundary (`assert-release-evidence!`).
- **Tests** `abc/test/abc/tools/parser_release_qualification_test.clj` (9 tests):
  corpus integrity + tamper rejection, the nine predeclared dimensions, the
  mechanical `0.969 < 1.0` boundary, unavailable-not-pass, gate/ADR promotion
  rule, comparison/neutral evidence rejection, report schema validity.
- **ADR 0039** `abc/docs/adr/0039-custom-parser-release-qualification.md`:
  **Proposed**, structurally clean under `adr-governance` (0 problems).
- **Captured execution** `abc/docs/reports/parser-release-qualification-{measurements.edn,report.json}`.

## Admission resolution (ADR 0023) — the release candidate is NOT the admitted tuple

Empirically confirmed on hinoki: the nix-built `.#ab-aozora` reports
`ab-aozora 0.6.0 aat-schema 2 facade 0.3.0 wire-schema 3 (git 90967157…)`. The
git coordinate is baked into the exact-match registry coordinate
(`AB_AOZORA_GIT_REV = self.rev`, `ab-aozora-aat/build.rs`). The admitted ADR-0023
row pins `git 004deaf548…`. HEAD ≠ `004deaf` (the `ab-aozora` crate changed after
`004deaf`), so the release candidate is not the admitted tuple and old admission
does not silently qualify current HEAD. Independently, `ab-aat-to-parser-ir
convert` under the admitted mapping `0.4.0` fails closed: the mapping pins
parser-IR schema hash `a1e1b506…` but the current `parser-ir.schema.json`
computes `43a6a6d8…` (schema drifted since admission). Admitting the HEAD build
requires a new exact ADR-0023 row backed by a fresh full-corpus conversion audit
— a separate campaign, not performed on this smoke-scale corpus; no admission
row was fabricated. Admission for HEAD is therefore `unavailable`, which
independently keeps the gate `not-qualified`.

## Predicate verdicts (captured on hinoki over the pinned corpus)

Instruments: nix-built `.#ab-aozora`, `.#ab-check`, `.#ab-index`,
`.#ab-aat-to-parser-ir`; `ab-index` build + `ab-check` batch (`--per-work-timeout
60s`, timed) + `ab-aozora --mode diagnostics` + `ab-aat-to-parser-ir convert`.

| predicate | observed | expected | verdict |
| --- | --- | --- | --- |
| fatal-failures | 0 | `<= 0` | **pass** |
| source-span-coverage | instrument-missing | `= 1.0` | **unavailable** |
| silent-drops | instrument-missing | `<= 0` | **unavailable** |
| diagnostic-completeness | 1.0 (0 diagnostics; vacuous) | `= 1.0` | **pass** |
| parser-IR-schema-validation | 1.0 (3/3 via live mapping 0.5.0) | `= 1.0` | **pass** |
| publication-structure | instrument-missing | `= 1.0` | **unavailable** |
| wall-time | 0.06 s | `<= 300` | **pass** |
| memory | instrument-missing | `<= 2 GiB` | **unavailable** |
| timeout-policy | 0 | `<= 0` | **pass** |

Unavailable blockers (named, not fabricated): no committed per-work source-span
coverage instrument for ab-aozora; no committed ADR-0002 silent-drop instrument
(the AAT→parser-IR divergence LOSS of 1/work is a mapping-layer signal, not the
source silent-drop metric); publication structure needs the TEI publication
pipeline not wired for this gate; ab-aozora/ab-check emit no peak-RSS field.

Note on measured build: measurements were captured at `90967157`; the
`ab-validator` parser crates are unchanged through final HEAD `052ee416` (only
`abc/` docs/data/evidence changed), so the build is equivalent and the report
records the exact measured `git 90967157` coordinate.

## Governance impact of adding ADR 0039 (kept the whole gate green)

Adding a new ADR regenerated `docs/adr/adr-graph.mmd`, a pinned input of the
`adr-graph-contract` and `diagram-registry-drift` evidence bundles. Recaptured
both through the real capture tool (`clojure -M:abc/adr-evidence-capture`) on
hinoki and refreshed their `:artifact-hash` in `docs/adr/adr-evidence.edn`
(surgical JCS re-hash, no register-tool corroboration clobbering). The four
ADR-enumerating evidence tests govern **Accepted** ADRs only and do not read the
Proposed 0039, so their descriptors/manifests/tests are unchanged. No pinned
hash was hand-forged: every hash was recomputed from real bytes.

## Final gate execution on hinoki (exact commands + results)

All on hinoki at `052ee416` (pushed via `git push hinoki
HEAD:feat/parser-release-qualification`):

- `just validate-migration` → **exit 0**; `monorepo-adr-governance` prints
  `ADR governance valid` (no `input-hash-mismatch`); log `/home/bor/rq-vmig2.log`.
- `nix build ./abc#checks.x86_64-linux.clj-nix-focused-tests` →
  **`1218 tests, 8103 assertions, 0 failures`** (includes the 9 new tests,
  `adr-evidence-capture-test` post-recapture, and `committed-diagrams-are-current`).
- `nix build ./abc#checks.x86_64-linux.clj-kondo` → **exit 0**.
- `ab-validator` `nix build .#checks.x86_64-linux.{cargo-check,cargo-clippy,cargo-fmt}`
  → **exit 0** each.
- `ab-validator` `nix build .#checks.x86_64-linux.cargo-test` → **exit 1**: 2
  pre-existing failures in the `ab-aozora-veb` crate —
  `eytzinger::tests::debug_panic_on_unsorted_input` and
  `map::tests::debug_panic_on_unsorted_keys`, both `#[should_panic]` tests gated
  on `debug_assert!` that cannot panic under the nix build profile (debug
  assertions off). Task 7 changed **no Rust**; this failure is independent of and
  unaffected by this task (confirmed by building the same check at baseline
  `6cf893e3`). It is not part of `just validate-migration`.

The Harmonia post-build-hook `failed to copy built paths` warning appears in
several logs (same benign artifact Task 6 documented); it does not affect build
exit codes.

## Concerns

1. **Gate not release-qualified (expected, honest).** 4 predicates are
   `unavailable` for want of committed release instruments, and the HEAD build is
   not an admitted tuple. Per the promotion rule, ADR 0039 stays Proposed and the
   release-qualify commit was not made. Moving off `unavailable` requires
   committed instruments (source-span coverage, silent-drop, publication
   structure, per-work memory) and an admitted HEAD build.
2. **Smoke-scale corpus (3 works).** The pinned corpus is intentionally small,
   deterministic, and tracked (no `/db`), covering ruby/gaiji/both. Broader
   category coverage (editor notes, images, large works, expected-failure
   fixtures) would need additional tracked corpus members.
3. **parser-IR predicate measured under the live mapping 0.5.0 / current schema**,
   not the admitted 0.4.0 tuple (which fails closed on schema drift). Recorded
   transparently in the report identity.
4. **`ab-validator` `cargo-test` is pre-existing RED** (2 `#[should_panic]`
   debug-assert tests in `ab-aozora-veb` cannot panic under the release build
   profile). Not caused by Task 7 (no Rust changed; reproduces at baseline
   `6cf893e3`) and not part of `just validate-migration`. Left untouched to avoid
   folding an unrelated Rust fix into this task; flagged for the parser owners.
