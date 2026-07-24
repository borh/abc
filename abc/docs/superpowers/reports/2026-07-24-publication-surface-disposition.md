# Publication Surface Disposition — Sole Publication Producer, Task 1 (closed Task 13)

Date: 2026-07-24 (opened); closed 2026-07-25 (Task 13)
Companion to `abc/docs/superpowers/specs/2026-07-24-sole-publication-producer-design.md`
and `abc/docs/superpowers/plans/2026-07-24-sole-publication-producer.md`. This
report changes no production code. It is the closed disposition audit
required before Lane 2 may delete anything: one row per Soranoha command,
Clojure entry point, root/`abc` flake app and alias, schema/example, Accepted
claim, active document, in-repository caller, and test family touched by the
migration.

**Closeout note (Task 13):** every disposition row below was written at Task 1
and corrected/rebased in Task 7; none is `unknown` (verified by `grep -i
unknown` over this file — the only disposition-relevant match is the
vocabulary sentence in §0 that states the rule; the other two matches below
are this closeout's own prose citing that check and an unrelated `--version`
string, not unresolved rows). §§9-13 below record the measured
before/after deltas, the added-vs-deleted separation the brief requires, and
the honestly-deferred follow-ups. Measurements compare `main@d5048939` (the
migration's branch point) against this branch's HEAD at close
(`605a453e`).

## Disposition vocabulary

Every row below uses exactly one of the eight dispositions named in the Task
1 brief:

- **release authority** — part of the sole release assembler/installer or its
  fail-closed verifier.
- **shared domain capability** — an independently useful value/behavior with
  an owner outside release assembly (request-set, source-snapshot, analysis,
  annotation, workflow, snapshot-index value/schema).
- **non-release renderer adapter** — writes candidate artifacts through
  `materialize-publication!` but cannot authenticate a source, close an index,
  or install a release root.
- **projection** — reads/validates/explains/stages the release index without
  regenerating canonical content; staging is included here (the brief does not
  carry a separate "downstream derived producer" term).
- **fixture characterization** — a test/dev-only apparatus whose job is to
  pin or exercise behavior, not to serve a release or a domain owner.
- **qualification instrument** — produces parser-qualification evidence
  through the shared renderer; not a release.
- **frozen historical reference** — a fact fixed at the time it was written
  (a report, a plan, a historical fixture); not a live producer/consumer
  obligation.
- **retire** — the confirmed-dead rehearsal/reproduction composition, its
  dispatcher wiring, and only its own apparatus tests.

No row below is marked "unknown"; where a consumer audit is still open for a
later lane, that is stated in the row's notes rather than used to defer the
disposition itself.

## 1. Soranoha commands and Clojure entry points

`abc.tools.soranoha` (`abc/src/abc/tools/soranoha.clj`) dispatch table plus
`abc.tools.soranoha-build-publication` (`abc/src/abc/tools/soranoha_build_publication.clj`):

| Command / entry point | Clojure fn | Disposition | Reason |
| --- | --- | --- | --- |
| `build-publication` | `soranoha-build-publication/build-publication!` | **release authority** | Sole real full-corpus source-to-release composition; owns strict/best-effort failure policy, concurrency, and atomic installation (`prepare-output-root!` / `promote-output-root!`). |
| — (internal) | `soranoha-build-publication/materialize-source-selection-step`, `write-build-records-step`, `materialize-publications-step` | **release authority** | Internal steps of the one release workflow (`soranoha.build-publication.v1`); reused only inside `build-publication!`. |
| `list-request-sets` | `soranoha/list-request-sets!` | **shared domain capability** | Enumerates checked-in request-set labels; independent of any end-to-end composition. |
| `explain-request-set` | `soranoha/explain-request-set!` | **shared domain capability** | Pure projection over request-set identity, not over a release index. |
| `resolve-request-set` | `soranoha/resolve-request-set!` | **shared domain capability** | Request-set resolution against a source snapshot; used directly by fixture setup across test files (`soranoha_test.clj`, `content_sim_test.clj` indirectly through `abc.sim.harness`). |
| `source-snapshot` | `soranoha/source-snapshot!` | **shared domain capability** | Source-snapshot materialization; independently useful, has its own domain test (`source-snapshot-command-generates-workset-and-snapshot-test`). |
| `snapshot-index` | `soranoha/snapshot-index!` (+ `soranoha/build-snapshot-index`) | **retire** | The Soranoha snapshot-index *producer* command named for explicit retirement in the brief; synthesizes a request-set-bound 0.1.1 identity, which direct publication must not reuse. |
| `reproduce` | `soranoha/reproduce!` (+ `soranoha/materialize-snapshot-root!` as its direct caller) | **retire** | Named for explicit retirement in the brief; the superseded end-to-end rehearsal/reproduction composition (per-work reproduction + snapshot-index synthesis). |
| `publication-rehearsal` | `soranoha/publication-rehearsal!` | **retire** | Named for explicit retirement in the brief; the fixture/request-set-bound composition the direct build superseded per commit `587bd182`. |
| `validate-workflow` | `soranoha/validate-workflow!` | **retire** | Named for explicit retirement in the brief; has no live operator/automation consumer outside its own CLI test — `workflow/validate-run` (its domain logic) is retained separately with its own test file (`abc/test/abc/tools/workflow_test.clj`). |
| `validate` | `soranoha/validate!` | **projection** | Validates a snapshot root/index; retained, but its current tests build fixtures via `reproduce` (see §9) — a Lane 2 consumer-migration concern, not a Task 1 change. |
| `explain-snapshot` | `soranoha/explain-snapshot!` | **projection** | Explains a snapshot index; retained, current test fixture also built via `snapshot-index` command (§9). |
| `publication-report` | `soranoha/publication-report!` | **projection** | Derived report over a snapshot root; retained. |
| `layout-report` | `soranoha/layout-report!` | **projection** | Derived static-layout comparison; retained. |
| `stage-publication` | `soranoha/stage-publication!` (delegates to `abc.tools.soranoha-stage-publication`) | **projection** | Downstream derived producer that copies/arranges bytes and cites the source index; explicitly retained per the brief. |
| `annotation-join-stats` / `annotation-join-stats-run` | `annotation-join-stats/run-join-stats!`, `annotation-join-stats-run/run-annotation-join-stats-run!` | **shared domain capability** | Independent analysis/annotation capability; out of the publication-producer cut entirely. |
| — (internal, not dispatched) | `soranoha/materialize-snapshot-root!` | **retire** | Corrected in Task 7 from the earlier "shared domain capability, pending Lane 4 relocation". Its only *production* callers are inside the retiring slice (`reproduce!` and `publication-rehearsal-steps`); the only other reference was the apparatus test `soranoha_annotation_test.clj`, which builds its whole fixture via this producer + the deleted demo snapshot plans and therefore retires with it. Its exclusive helpers `soranoha/write-annotation-artifacts!` and `soranoha/validate-annotation-manifests!` retire with it; the independent annotation capability is the retained `abc.tools.materialize-annotations` namespace (its own test `materialize_annotations_test.clj` and the retained `validate_design_bundle.clj` consumer are untouched). |
| — (internal, not dispatched) | `materialize_publication.clj#materialize-release-publication!` | **non-release renderer adapter**, target: retire as a *release-named* boundary | Thin `assert-release-allowed!` + `materialize-publication!` wrapper used by the single-work CLI's default (non-`--batch`) path; per the design, its rights check should move to `build-publication` and the name should stop implying release authority. Not renamed or removed in Task 1. |

## 2. Root and `abc` flake apps, `deps.edn` aliases, launchers, checks

| Surface | Disposition | Reason |
| --- | --- | --- |
| `abc` flake app `soranoha` / alias `abc/soranoha`; root flake's adapter-aware `nix run .#soranoha` wrapper (`flake.nix`, `mkAdapterAwareSoranohaApp`) | **release authority** launcher | The only launcher through which `build-publication` is exercised with real adapter/parser/mapping wiring; retained and the future real-wiring CI check (plan Lane 0) will invoke it. |
| `abc` flake app `materialize-publication` / alias `abc/materialize-publication` | **non-release renderer adapter** | Single-work renderer CLI; live `ab-validator` and smoke-script consumer (§6). |
| `abc` flake app `materialize-publications-batch` / alias `abc/materialize-publications-batch` (same underlying `-main`, `--batch`) | **non-release renderer adapter** | Batch renderer CLI; live `ab-validator` batch/report consumer (§6); retained pending explicit consumer migration per the design. |
| `abc` flake alias `abc/parser-rq-publication-materialize` | **qualification instrument** | Parser-RQ publication materializer; produces evidence through the shared renderer, not releases. |
| `abc` flake alias `abc/materialize-source-snapshot`, `abc/source-snapshot-workset` | **shared domain capability** | Source-snapshot/workset value construction independent of the rehearsal composition. |
| Root flake checks `soranoha-monorepo-*` (schema-drift, tei-version-coherence, flake-input-policy, runtime-config, active-path-hygiene, workflow-run-lib, aat-run-set, fidelity-lock-idempotency, batch-run-staleness, aat-materialization-workflow, python-quality, nix-format) | **projection** / **shared domain capability** (per-check) | None of these checks invoke the retiring commands; they audit schemas, workflow-run shape, or repo hygiene, and stay untouched by this migration. |
| Root flake check `soranoha-parser-rq-production-wiring` | **qualification instrument** wiring check | Exercises parser-RQ production wiring, not the publication release path. |
| No root-flake check currently invokes the real `soranoha build-publication` app over a committed fixture | **retire/absent** (gap, not a disposition of a live surface) | Recorded here because plan Lane 0 requires adding exactly this check; Task 1 does not add it. |

## 3. Output files, schema contracts, fixtures, mirrors

| Surface | Disposition | Reason |
| --- | --- | --- |
| `abc/schemas/snapshot-index.schema.json` (0.1.1) | **shared domain capability** (value/schema owner: `abc.tools.snapshot-index`) | The design retains this namespace as the value/schema owner and versions it to 0.2.0 later (Task 8); the *schema file* itself is not retired, only its request-set-bound live-producer role. |
| `abc.tools.snapshot-index` namespace (`build-snapshot-index`, `write-snapshot-index!`, `validate-snapshot-index!`, `snapshot-identity-hash`, etc.) | **shared domain capability** | Explicitly retained per the brief as the value/schema owner, distinct from the Soranoha `snapshot-index` *producer command*, which is retired. |
| `abc/schemas/workflow-run.schema.json`, `abc.tools.workflow` (`run-workflow!`, `validate-run`, `validate-plan!`) | **shared domain capability** | Explicitly retained per the brief (`workflow/run-workflow!`); used by `build-publication!` (release authority caller), `publication-rehearsal!` (retiring caller), and `annotation-join-stats-run` (independent caller). The function itself has three callers and is not scoped to the retiring composition. |
| `abc/schemas/soranoha-publication-build-config.schema.json`, `config/publication-basic-ja.json`, `config/full-corpus-publication-basic-ja.json`, `config/full-corpus-publication-custom-parser-ja.json` | **release authority** input contract | Config schema/examples for `build-publication`; versioned further in Task 4, not touched here. |
| `abc/data/snapshot-plans/*`, request-set fixtures (`full-corpus-basic-ja`, `full-corpus-publication-basic-ja`, `smoke-basic-ja`, `demo-basic-ja`, `demo-annotation-ja`) | **shared domain capability** / **fixture characterization** (per consumer) | Used both by the retiring `reproduce`/`snapshot-index` commands and by retained `resolve-request-set`/annotation tests; the request-set *value* functions are retained even though the rehearsal composition that most often exercises them is retired. |
| `abc/schemas/manifest.schema.json`, `abc/schemas/parser-ir.schema.json`, TEI profile trio (`tei-profile.odd`/`.rng`/`.sch`), `data/parser-ir-publication-policy-v0.json`, `schemas/parser-ir-publication-preservation.schema.json` | **release authority** / **non-release renderer adapter** shared contract | Consumed identically by `materialize-publication!` regardless of caller; not specific to the retiring composition. |
| `publications-report.json` (emitted by `materialize-publications-step` inside `build-publication!`) | **projection** (derived, non-authoritative) | Per the design spec, explicitly not an authoritative identity; stays a derived operational/corpus report. |
| `build-plan.json`, `build-config.json`, `source-selection-report.json`, `workflow-plan.json`, `workflow-run.json` under a `build-publication` output root | **release authority** operational trace | Retained introspection surface; `build-plan.json`'s `materialized_root` field is the temporary-absolute-path defect Task 6 removes (characterized in Task 1, not fixed — see the new `build-plan-records-temporary-absolute-materialized-root-test`). |

## 4. Accepted decision claims and cited evidence

`abc/docs/adr/decisions.edn` was searched for every slug touching this
surface:

| Decision (slug) | Cites publication-rehearsal/reproduce/snapshot-index/validate-workflow? | Disposition impact |
| --- | --- | --- |
| `custom-parser-release-qualification` (0039) | No — cites parser-RQ evidence artifacts, not the Soranoha dispatcher | **qualification instrument** claims are independent of this migration; Task 2/3 wire this decision into `build-publication`'s admissibility check, not into the retiring commands. |
| `manifest-identity`, `manifest-identity-hardening` | No | **release authority** / **shared domain capability** manifest-identity contracts are unaffected; `materialize-publication!` and `build-publication!` continue to satisfy them. |
| `tei-odd-schematron-validation` | No | **non-release renderer adapter** / **release authority** TEI validation stays as-is; not touched by the rehearsal retirement. |
| All other Accepted decisions (`nix-materialization`, `supply-chain-release-security`, `operational-runtime`, `v0-design-bundle-validation`, `external-parser-validation-boundary`, `abc-tools-runtime`, `imported-output-materialization`, `generated-fixture-policy`, `cultural-heritage-lod-profile`, `iiif-applicability`, `temporal-modeling`, `edtf-level1-decade-century`, `vocabulary-review`, `person-identity-drift-*`, `upstream-ingest-drift-awareness`, ...) | No | No Accepted claim was found that cites `snapshot-index`, `reproduce`, `publication-rehearsal`, or `validate-workflow` as evidence. Retiring them requires no decision amendment. |

No Accepted decision claim in `decisions.edn` references the four retiring
commands. This means their retirement (a later task) needs no governance
amendment — only the ordinary consumer/test audit already captured in this
report.

## 5. Active documentation and runbooks

`rg` over `abc/docs/adr/*.md` (excluding `INDEX.md`, which only lists slugs)
found no ADR prose citing `publication-rehearsal`, `soranoha reproduce`, or
`validate-workflow`. Two ADRs reference retained capabilities that happen to
share a namespace with retiring code:

| Document | Reference | Disposition |
| --- | --- | --- |
| `abc/docs/adr/ruby-annotation-view.md` | `abc.tools.soranoha/materialize-snapshot-root!` (annotation manifest resolution) and the snapshot-index schema | **shared domain capability** | Cites the retained annotation-materialization behavior, not the retiring `reproduce`/`snapshot-index` commands. |
| `abc/docs/adr/analysis-artifact-identity.md` | "the snapshot-index schema, and the Soranoha request-set inspection command" | **shared domain capability** | Cites `explain-request-set`/schema, both retained. |

All other matches for `publication-rehearsal`, `soranoha/publication-rehearsal!`,
and `soranoha/materialize-snapshot-root!` outside ADRs live under
`abc/docs/superpowers/plans/*` and `specs/*` (design history for the workflow
orchestration and build-publication-command features from 2026-07-08 and
2026-07-10). Those are:

| Document class | Disposition | Reason |
| --- | --- | --- |
| `abc/docs/superpowers/plans/2026-07-08-workflow-orchestration-implementation.md`, `2026-07-08-workflow-orchestration-design.md`, `2026-07-08-build-publication-command-implementation.md`, `2026-07-10-per-request-set-annotation-materialization.md` | **frozen historical reference** | Historical implementation plans that recorded the pre-cutover contract at the time; not live runbooks and not treated as consumers. |

No currently-active runbook or operator document (outside the above
historical plans and this migration's own spec/plan/report set) advertises
`snapshot-index`, `reproduce`, `publication-rehearsal`, or `validate-workflow`
as a supported operator path.

## 6. In-monorepo consumers

| Consumer | What it calls | Disposition | Reason |
| --- | --- | --- | --- |
| `ab-validator/justfile` (`materialize`, `materialize-batch` recipes) | `clojure -M:abc/materialize-publication`, `clojure -M:abc/materialize-publications-batch` | **non-release renderer adapter**, live | Confirmed live shell-out consumer of both the single-work and batch renderer CLIs. |
| `ab-validator/tests/parser-ir-publication-coverage-smoke.sh`, `parser-ir-level3-tei-eaj-compare-smoke.sh`, `parser-ir-ortho-publication-smoke.sh`, `parser-ir-level3-publication-smoke.sh` | Same two renderer CLIs | **non-release renderer adapter**, live | Smoke-test shell scripts that render candidate artifacts through the adapter, not through a release. |
| `ab-validator/reports/parser-ir/tei-eaj-generated-compare.py`, `publication-bundle-validate.py` | Same two renderer CLIs | **non-release renderer adapter**, live | Report-generation tooling that reuses the renderer CLIs; historical `.summary.json` records under `ab-validator/docs/superpowers/reports/` that already cite past runs are **frozen historical reference**. |
| `abc.tools.parser-rq-publication-materialize` | `materialize-publication/materialize-publication!` directly (library call, not CLI) | **qualification instrument** | Confirmed: produces parser-RQ qualification fixtures through the shared renderer; no release or workflow authority. |
| `abc.tools.soranoha-stage-publication` (+ `abc/test/abc/tools/soranoha_stage_publication_test.clj`) | Snapshot-index-shaped values, not the retiring commands | **projection** | Downstream derived producer; explicitly retained per the brief. |
| `abc/test/abc/tools/soranoha_annotation_test.clj` | `soranoha/materialize-snapshot-root!` directly | **shared domain capability** | Live in-repo consumer bypassing `reproduce!`; see the caveat in §1. |
| `abc.tools.annotation-join-stats-run` | `abc.tools.workflow/run-workflow!` | **shared domain capability** | Independent workflow consumer; unaffected by the rehearsal retirement. |

## 7. Known out-of-repository/public consumers

No out-of-repository consumer of `snapshot-index`, `reproduce`,
`publication-rehearsal`, or `validate-workflow` was found or is documented
anywhere in this repository (no published CLI reference, no external-facing
release notes advertise these as a stable public interface). The single-work
and batch renderer CLIs (`abc/materialize-publication`,
`abc/materialize-publications-batch`) are documented only as in-monorepo
`ab-validator` tooling (§6); no public consumer surface was found for them
either. This row cannot be upgraded beyond "none found in this repository";
a public/downstream audit outside this monorepo is out of scope for Task 1.

## 8. Tests: domain characterization vs. apparatus self-tests

Per the brief, only the *apparatus* tests for the retiring commands are
scoped for eventual retirement — domain characterization tests, and tests
that merely use a retiring command as fixture setup for a retained
capability, are not.

### Apparatus self-tests for the four retiring commands (future retirement target)

All in `abc/test/abc/tools/soranoha_test.clj`:

| Test | Disposition | Retiring command it self-tests |
| --- | --- | --- |
| `snapshot-index-command-generates-index-from-plan-test` | **fixture characterization** (retiring apparatus) | `snapshot-index` |
| `snapshot-index-command-accepts-resolved-request-set-file-test` | **fixture characterization** (retiring apparatus) | `snapshot-index` |
| `reproduce-command-writes-default-smoke-snapshot-index-test` | **fixture characterization** (retiring apparatus) | `reproduce` |
| `reproduce-command-accepts-resolved-request-set-file-test` | **fixture characterization** (retiring apparatus) | `reproduce` |
| `reproduce-command-materializes-demo-request-set-test` | **fixture characterization** (retiring apparatus) | `reproduce` |
| `reproduce-command-uses-generated-full-corpus-request-set-test` | **fixture characterization** (retiring apparatus) | `reproduce` |
| `reproduce-command-skips-analysis-for-generated-publication-request-set-test` | **fixture characterization** (retiring apparatus) | `reproduce` |
| `publication-rehearsal-command-runs-full-chain-test` | **fixture characterization** (retiring apparatus) | `publication-rehearsal` |

`validate-workflow` has no dedicated apparatus test of its own; it is
exercised incidentally at the end of
`build-publication-command-materializes-real-publications-test` (against
`build-publication!`'s own, retained workflow-run.json). That call site
needs to move to `workflow/validate-run` directly when the command is
retired — noted here, not changed in Task 1.

### Tests that used a retiring command only as fixture setup for a retained capability — Task 7 resolution

The retained projection **commands** stay (`validate`, `explain-snapshot`,
`publication-report`, `layout-report`, `stage-publication`); only their
now-unbuildable command **tests** are affected. The projections read/validate a
0.1.1 index; two inspect the index value directly and were rebased onto the
checked-in 0.1.1 example (`examples/v0/snapshot/snapshot-index.json`), the rest
required a full materialized root that no retained producer can build until
Task 8/10 introduces a proper completed-root example, so their tests are retired
now with coverage explicitly re-established later.

| Test | Command retained? | Task 7 disposition |
| --- | --- | --- |
| `explain-snapshot-command-validates-and-prints-identity-test` | `explain-snapshot` — yes | **rebased**, kept: now `explain-snapshot-command-explains-checked-in-example-index-test` reads the checked-in 0.1.1 example index directly. |
| `validate-command-accepts-snapshot-root-test` (index-value part) | `validate` — yes | **rebased**, kept: now `validate-command-validates-checked-in-example-index-test` validates the checked-in 0.1.1 example index file (read-only, non-directory path). |
| `validate-command-rejects-missing-referenced-manifest-test` (root-directory reference validation) | `validate` — yes | **test retired now; command retained; coverage re-established Task 8** — needs a full materialized root with a referenced manifest, unbuildable by any retained producer until the Task 8 completed-root example. |
| `publication-report-command-writes-citable-reproduction-evidence-test` | `publication-report` — yes | **test retired now; command retained; coverage re-established Task 10** (publications-report.json coverage). |
| `layout-report-command-compares-static-layout-strategies-test` | `layout-report` — yes | **test retired now; command retained; coverage re-established Task 8** against the 0.2.0 completed-root fixture. |
| `stage-publication-command-writes-mixed-static-layout-test` | `stage-publication` — yes | **test retired now; command retained; coverage re-established Task 8** against the 0.2.0 completed-root fixture. |

These are **not silent drops**: the projection commands are retained, and the
retired command-level characterizations are re-established in Task 8 (layout-report,
stage-publication, root-reference validation) and Task 10 (publication-report).
The underlying projection library logic remains covered now by the dedicated
domain tests `soranoha_layout_report_test.clj` and `soranoha_stage_publication_test.clj`.
`snapshot-index-command-*` and `reproduce-command-*` tests (whose sole subject is
the retired producer) and `archive-summary-does-not-descend-through-directory-symlinks-test`
(exercises the retired private `archive-summary`) are retired outright.

### Domain characterization tests unaffected by retirement

`workflow_test.clj` (`validate-run-reports-semantic-invariants-test` and
siblings — **shared domain capability**), `soranoha_annotation_test.clj`
(**shared domain capability**), `source_bundle_test.clj`,
`materialize_publication_test.clj` (**non-release renderer adapter** /
**release authority** shared contract), and all of
`abc/test/abc/tools/soranoha_test.clj`'s `build-publication-*` tests plus
`abc/test/abc/sim/content_sim_test.clj` in its entirety (**release
authority** characterization, extended by this task) are untouched by the
rehearsal/reproduction retirement.

## Required explicit retains and retires (brief cross-check)

Explicitly retained, per the brief:

- single-work materializer CLI/Nix app (`materialize-publication`) — §2, §6;
- batch materializer CLI/Nix app (`materialize-publications-batch`) — §2, §6;
- parser-RQ publication materializer (`parser-rq-publication-materialize`) — §2, §6;
- `workflow/run-workflow!` — §3;
- source/request-set domain values — §1, §3;
- staging (`stage-publication`, `soranoha-stage-publication`) — §1, §6;
- `abc.tools.snapshot-index` as the value/schema owner — §3.

Explicitly retired, per the brief (commands, their dispatcher wiring, and
only their apparatus tests):

- Soranoha `snapshot-index` producer command — §1, §8;
- `reproduce` — §1, §8;
- `publication-rehearsal` — §1, §8;
- `validate-workflow` — §1, §8.

## Open items carried to later tasks (not resolved by Task 1)

1. **Resolved in Task 7.** `materialize-snapshot-root!`'s only non-production
   reference was the apparatus test `soranoha_annotation_test.clj`, which builds
   its fixture entirely via that producer + the deleted demo snapshot plans, so
   the file was retired with the producer (not migrated). Independent annotation
   coverage remains in `materialize_annotations_test.clj`. Its exclusive helpers
   `write-annotation-artifacts!`/`validate-annotation-manifests!` retired with it.
2. **Resolved in Task 7.** `validate`/`explain-snapshot` characterizations were
   rebased onto the checked-in 0.1.1 example index (kept); the
   `publication-report`/`layout-report`/`stage-publication`/root-directory-`validate`
   command tests were retired now (their full-root fixture is unbuildable by any
   retained producer) with coverage re-established in Task 8 (layout-report,
   stage-publication, root-reference validation) and Task 10 (publication-report).
   The projection **commands** themselves are all retained.
3. **Resolved in Task 11 — and RED by design, deferred per explicit user
   decision.** `checks.<system>.publication-build-real-wiring`
   (`tests/publication-build-real-wiring-smoke.sh`) now invokes the real
   `soranoha build-publication` app over a committed fixture through the
   flake-exported adapter/parser/mapping paths, closing the Lane 0 gap this
   row used to record. Running it (`nix build
   .#checks.x86_64-linux.publication-build-real-wiring --print-build-logs`)
   is honestly RED: `release_admissible: false` /
   `build_exit_code: 1` with two non-rights problems —
   `release-converter-build-hash-mismatch` (actual
   `sha256:8073c1dc520f...`, expected `sha256:a2656fc9404b...`) and
   `release-parser-build-hash-mismatch` (actual `sha256:ff31d3036ab6...`,
   expected `sha256:482728cad5bc...`, the same accepted P5
   `ab-aozora` executable hash pinned in
   `parser_release_authority_test.clj`). This is a genuine
   candidate-reproducibility drift the authentication code correctly
   surfaced, not a flake-wiring defect — see §13 for the full record and
   re-qualification follow-up.

## 9. Measured before/after deltas (Task 13 closeout)

Measured with `git diff --stat d5048939..HEAD`, `git show <rev>:<path> | wc
-l`, `rg`, and direct `nix build`/`bin/kaocha` runs against a throwaway
`git worktree add ... d5048939` checkout of the branch point (removed after
measurement; no files from it are part of this commit).

### 9.1 Retired-family source/test LOC (the four retiring commands and their
exclusive apparatus, per §§1/8)

| File | Before (d5048939) | After (HEAD) | Δ | Note |
| --- | --- | --- | --- | --- |
| `abc/src/abc/tools/soranoha.clj` | 1296 | 424 | **−872** | Dispatcher shrinks from 15 to 11 commands (§9.2); every internal helper exclusive to `snapshot-index`/`reproduce`/`publication-rehearsal`/`validate-workflow`/`materialize-snapshot-root!` (`build-snapshot-index`, `write-parser-ir-artifacts!`, `write-publication-artifacts!`, `write-annotation-artifacts!`, `materialization-entries`, `run-summary`, `materialize-entry!`, `validate-annotation-manifests!`, `materialize-snapshot-root!`, `reproduce!`, `validate-manifest-reference-fields!`, `validate-loose-reference!`, `validate-archive-member-reference!`, `validate-run-summary!`, `validate-workflow!`, `zstd-archives`, `archive-summary`, `validate-staged-root!`, `publication-rehearsal-report`, `publication-rehearsal-steps`, `publication-rehearsal!`, plus their private generated-fixture/analysis-recipe helpers — 39 `defn`/`defn-` forms total) is gone. The 11 retained commands (`list-request-sets`, `explain-request-set`, `resolve-request-set`, `validate`, `explain-snapshot`, `publication-report`, `layout-report`, `stage-publication`, `source-snapshot`, `annotation-join-stats`, `annotation-join-stats-run`) and their thin wrappers remain, unchanged in behavior. |
| `abc/test/abc/tools/soranoha_test.clj` | 1401 | 1252 | −149 | 15 apparatus/fixture-only tests removed (the 8 named in §8's apparatus table + the 6 fixture-setup tests Task 7 retired + `archive-summary-does-not-descend-through-directory-symlinks-test`), 10 new tests added characterizing `build-publication!`, the rebased `explain-snapshot`/`validate` example-index tests, and the retired-surface-absence proof (`retired-publication-producer-surface-is-absent-test`). Net LOC drop is smaller than the source file's because the surviving/added `build-publication-*` characterization is denser per line than the deleted apparatus. |
| `abc/test/abc/tools/soranoha_annotation_test.clj` | 104 | 0 (deleted) | **−104** | Its whole fixture was built via the now-retired `materialize-snapshot-root!` + deleted demo snapshot plans; independent annotation coverage remains in `materialize_annotations_test.clj` (untouched). |
| `abc/data/snapshot-plans/{demo-annotation-ja,demo-basic-ja,smoke-basic-ja}.json` | 270 (fixture JSON, 3 files) | 0 (all deleted) | **−270** | Rehearsal/reproduce/snapshot-index-only fixtures; `full-corpus-*` request-set plans (still used by retained `resolve-request-set`/`build-publication`) are untouched. |
| **Retired-family total (source + test + apparatus fixtures)** | — | — | **≈ −1,395 lines** | Sum of the four rows above (872 + 149 + 104 + 270); this is the actual deletion the migration bought, not a net-LOC-across-the-whole-diff number. |

### 9.2 Command / alias count

| Surface | Before | After | Δ |
| --- | --- | --- | --- |
| `abc.tools.soranoha` dispatch-table commands (`positional-command "..."` entries) | 15 | 11 | **−4** (`snapshot-index`, `reproduce`, `publication-rehearsal`, `validate-workflow` retired; confirmed by direct `grep -n 'positional-command "'` diff against both revisions) |
| `abc/deps.edn` Soranoha-family aliases (`:abc/soranoha`, `:abc/materialize-import`, `:abc/materialize-publication`, `:abc/materialize-publications-batch`, `:abc/source-snapshot-workset`, `:abc/materialize-source-snapshot`, `:abc/parser-rq-publication-materialize`) | 7 | 7 | **0** — the four retired commands lived inside the single `:abc/soranoha` dispatcher, not as their own aliases, so alias count is unaffected; the reduction is entirely internal to the dispatch table. |
| Root `flake.nix` exported apps referencing `soranoha`/`materialize-publication*` (`mkAdapterAwareSoranohaApp` wrapper, `soranoha`, `materialize-publication`, `materialize-publications-batch`) | same 3 named apps (refactored to take `system` explicitly; no app added or removed) | same 3 | **0**, refactor only |

### 9.3 Schema count and content

| Metric | Before | After | Δ |
| --- | --- | --- | --- |
| Files under `abc/schemas/` | 83 | 83 | **0** — no schema file added or removed by this migration. |
| `abc/schemas/snapshot-index.schema.json` `"version"` | `0.1.1` | `0.2.0` | In-place identity-v2 cut (§1's `snapshot-index` schema-owner disposition); a new `schema_version: {"const": "0.2.0"}` field was added to the schema body. Same file, new content — not counted as a new schema. |
| `abc/schemas/soranoha-publication-build-config.schema.json` | present, unversioned bump in Task 4 | present | Content changed (admissibility/parser-identity fields); file count unaffected. |

### 9.4 Namespace-dependency count (`abc.tools.soranoha`'s own `:require`)

| Metric | Before | After | Δ |
| --- | --- | --- | --- |
| `abc.tools.*` namespaces required by `soranoha.clj` | 22 | 15 | **−7** |

Removed (no longer needed once the retiring commands and their helpers left
the dispatcher): `hash`, `manifest-index`, `materialize-analysis`,
`materialize-annotations`, `materialize-publication`, `parser-evidence`,
`tar`, `workflow` (8 removed). Added: `publication-release` (1 added, the new
release-admissibility/verifier boundary the dispatcher's `validate!` path now
calls through `print-release-verdict!`). Net **8 removed − 1 added = −7**,
confirmed by direct line count of the `:require` block's `abc.tools` entries
in both revisions (`22` → `15`).

### 9.5 Standing check / test-suite time

No before/after pair exists for `checks.<system>.publication-build-real-wiring`
itself — before Task 11 no root-flake check invoked the real build at all
(the exact gap §2/item-3 above records), so there is nothing to diff it
against; only its current cost is measurable: one full `nix build
.#checks.x86_64-linux.publication-build-real-wiring --print-build-logs` run
(uncached, `--rebuild`-equivalent cost) took **≈1m24s** wall-clock and ends
RED as described in item 3 above and §13.

The full `abc` Kaocha suite *is* commensurably measurable both sides,
measured via a throwaway `git worktree add <scratch> d5048939` checkout run
with the same `TEI_SCHEMA_PATH` nix-store `tei_all.rng` export used on HEAD:

| Metric | Before (d5048939) | After (HEAD) | Δ |
| --- | --- | --- | --- |
| `bin/kaocha` (full suite) tests | 1064 | 1124 | **+60** |
| assertions | 11050 | 11575 | **+525** |
| wall-clock (`time bin/kaocha`, single uncached run each side) | ≈51.4s | ≈45.2s | not a reliable delta (single-sample, local-machine noise); directionally flat-to-faster despite +60 tests, consistent with the deleted rehearsal/reproduce apparatus (which materialized real filesystem trees per test) being replaced by more, but lighter, `build-publication!`/`parser-release-authority`/`publication-release` characterization tests. Not claimed as a measured performance win — recorded only because the brief asks for standing check time "where measurable." |

## 10. Correctness code ADDED vs. apparatus DELETED (honesty separation, per
the brief)

The brief requires this report to **not** present net LOC as "architectural
simplification." The two are kept apart here:

### 10.1 Added — trust/identity correctness code (this is added surface
area, not a simplification credit)

| File | Status | LOC | Purpose |
| --- | --- | --- | --- |
| `abc/src/abc/tools/parser_release_authority.clj` | new | 95 | Authenticates the release parser by binary + mapping hash against the committed P5 qualification provenance (Task 2/3); the fail-closed gate `checks.<system>.publication-build-real-wiring` exercises. |
| `abc/src/abc/tools/publication_release.clj` | new | 324 | Assembles the one closed release value / `verify-release-root!` recomputable admissibility check (Task 5/9). |
| `abc/test/abc/tools/parser_release_authority_test.clj` | new | 154 | Characterizes the authentication boundary against the exact committed P5 candidate tuple (`sha256:15affdfb…`/`sha256:6f365a44…`/`sha256:482728cad5…`). |
| `abc/test/abc/tools/publication_release_test.clj` | new | 360 | Characterizes release assembly/verification. |
| **Added correctness total** | | **933** | 419 source + 514 test. This is cost paid for trust/identity, credited nowhere as a deletion. |

Also added (smaller, same category): `abc/src/abc/tools/manifest.clj` (+81),
`abc/src/abc/tools/materialize_import.clj` (+25),
`abc/src/abc/tools/publication_policy.clj` (+68),
`abc/test/abc/tools/materialize_import_test.clj` (+52),
`abc/test/abc/tools/source_assertion_test.clj` (+19, new file), plus growth
inside `abc/src/abc/tools/snapshot_index.clj` (294→462, +168, the v0.2.0
identity cut) and `abc/src/abc/tools/soranoha_build_publication.clj`
(793→1203, +410, absorbing the one real release-composition workflow that
used to be split across the dispatcher and the rehearsal path) and their
test files (`snapshot_index_test.clj` 538 lines of diff,
`soranoha_build_publication_test.clj` +224). None of this growth is "the
simplification" — it is the correctness apparatus the sole-producer design
requires, and it is why the whole-repo diffstat (`+5237/−3026`, net +2211
lines) is *not* the number this report reports as savings.

### 10.2 Deleted — rehearsal/reproduce/snapshot-index apparatus (this is the
actual simplification)

Per §9.1: **≈1,395 lines** of source + test + fixture apparatus retired
outright (the four dispatcher commands, their 39 exclusive `soranoha.clj`
helpers, 15 apparatus tests, one whole annotation-fixture test file, and
three JSON fixture files), plus the dispatcher's own namespace-dependency
footprint shrinking by 7 (§9.4). This deletion — not the net LOC delta — is
the architectural simplification the migration claims: one real
source-to-release composition (`build-publication!`) replaces the rehearsal/
reproduction/snapshot-index-producer path that a second, competing assembler
used to walk.

### 10.3 Why net LOC is not reported as the headline number

Whole-repo `git diff --stat d5048939..HEAD`: **55 files changed, 5237
insertions(+), 3026 deletions(-)** (net **+2211**). Reporting that number
alone would read as "the migration added complexity," which is true by raw
line count and *false* as a characterization of what changed: the added
lines are almost entirely the new trust boundary (parser/release
authentication, closed-index assembly, their tests — §10.1) plus the
snapshot-index v0.2.0 identity cut, while the actual retired surface (§9.1,
§10.2) is a clean, complete deletion of a second, unauthenticated,
rehearsal-shaped release path. The simplification is "one producer, fail-
closed," not "fewer lines."

## 11. Coverage-gap closures (Task 7 deletions, re-established Task 8/Task
10) — closed

Task 7 retired the command-level tests for `publication-report`,
`layout-report`, `stage-publication`, and the root-directory-reference path
of `validate` because their full-materialized-root fixture was unbuildable
by any retained producer at the time (§8 table above). This is now **closed**:

- **Task 8** re-established `layout-report-command-*` and
  `stage-publication-command-*` coverage against a committed v0.2.0
  completed-root fixture, and re-established the root-reference-validation
  path of `validate` against the same fixture.
- **Task 10** re-established `publication-report-command-*` coverage
  (`publications-report.json`) against the same class of completed-root
  fixture.

No command-level coverage gap from Task 7's deletions remains open; the
projection commands themselves were never removed (§1), only their
fixture-dependent tests, and those fixtures now exist.

## 12. `validate-design-bundle` shrink — accepted, out-of-scope, Lane 5

`abc/src/abc/tools/validate_design_bundle.clj` and its consumer surface were
explicitly out of scope for every task in this migration (Tasks 1–13 never
read or modified `validate_design_bundle*`; confirmed again here — no task
report in `.superpowers/sdd/task-{1..13}-report.md` lists it among changed
files). Its Accepted-claim evidence citations (42 counted against
`decisions.edn` at Task 1 scoping time) are unaffected by the sole-producer
cutover and remain a deferred Lane 5 follow-up, not a defect of this
migration.

## 13. Deferred: P5 candidate reproducibility drift + re-qualification
follow-up (explicit user decision: land now, re-qualify separately)

Per an explicit user decision to land the sole-publication-producer
migration now and re-qualify the P5 candidate as a separate, later action,
this item is recorded here as a real, tracked, currently-RED outcome — not
smoothed over.

**What is RED and why it is correct for it to be RED:** running
`nix build .#checks.x86_64-linux.publication-build-real-wiring
--print-build-logs` (Task 11's real-wiring check, now closing the former
Lane 0 gap — §2/item 3 above) against the fresh `ab-aozora`/
`ab-aat-to-parser-ir` build the flake wires today produces
`release_admissible: false`, `build_exit_code: 1`, and exactly two non-rights
problems:

- `release-converter-build-hash-mismatch` — actual
  `sha256:8073c1dc520f2a829375d43473d61a3b9a3dc03bc26df60f0cdd3011a0d1b81f`,
  expected `sha256:a2656fc9404be9a8eb08e5ba16667db814c8a22ad45bdf684d78973befaf5936`.
- `release-parser-build-hash-mismatch` — actual
  `sha256:ff31d3036ab636c2ffec11802d223e78c9be4ca777df191a89c4732b48fb8bff`,
  expected `sha256:482728cad5bc663c0742ca9e8c6d6fa7031c1a84117d024921cd48628e2eb034`
  (the same P5 `ab-aozora` executable hash pinned as
  `p5-ab-aozora-executable-sha256` in
  `abc/test/abc/tools/parser_release_authority_test.clj:25`).

Task 11 traced the root cause: rebuilding `ab-validator`'s
`parser-rq-candidate` package fresh from current HEAD reproduces
`ab-aat-to-parser-ir`'s hash exactly, but **not** `ab-aozora`'s — despite an
identical `--version` string (`ab-aozora 0.6.0 aat-schema 2 facade 0.3.0
wire-schema 3 (git unknown)`) in both. The frozen `parser-rq-candidate`
derivation no longer reproduces its own recorded provenance hash; this is a
build/dependency-drift reproducibility gap in the *qualified* candidate, and
the migration's authentication code (`parser-release-authority`,
`publication-release/verify-release-root!`) is working exactly as designed
— it exposed a pre-existing drift rather than silently accepting an
unauthenticated parser build. The release-rights problem this same check
surfaced at Task 11 time (`release-rights-blocked
:blocked-pending-assessment-migration`) is no longer present as of this
measurement — only the two hash-mismatch problems remain.

**Re-qualification follow-up (tracked, not done in this migration):**

1. Re-run the P5 capture (`abc.tools.parser-rq-*` campaign machinery) against
   the current `ab-aozora`/`ab-aat-to-parser-ir` build to regenerate:
   - a fresh `executable-provenance.json` under a new
     `docs/reports/parser-rq/runs/<candidate_ref>/`,
   - a new `candidate_ref` (currently `sha256:15affdfb677cc6a9…`),
   - a new `qualification_identity_ref` (currently
     `sha256:6f365a44b975465943da88d0e3fe4f123672e00913285a3e998ab465bc79edca`).
2. Update the Accepted `custom-parser-release-qualification` decision
   (`abc/docs/adr/decisions.edn`, slug at line 1594) to cite the new
   provenance/candidate/qualification-identity tuple.
3. Update the pinned hashes in
   `abc/test/abc/tools/parser_release_authority_test.clj`
   (`p5-candidate-ref` = `sha256:15affdfb…`, `p5-qualification-identity-ref` =
   `sha256:6f365a44…`, `p5-ab-aozora-executable-sha256` =
   `sha256:482728cad5…`) and the same tuple pinned in
   `abc/test/abc/tools/soranoha_build_publication_test.clj` (lines 91, 219).
4. Update the `parser-rq-p5-promotion-audit` check to the regenerated
   evidence.
5. Resolve, as an explicit governance decision, whether the release build
   should bind the frozen `parser-rq-candidate` binaries or continue binding
   mainline `ab-aozora`/`ab-aat-to-parser-ir` — the two are different
   derivations (different `cargoBuildFlags` recipes) and will keep diverging
   either way; qualification and release-build wiring need to agree on which
   one is authoritative.

Until this follow-up lands, `checks.<system>.publication-build-real-wiring`
is expected to stay RED in CI for this specific, understood, tracked reason
— not because the sole-producer migration itself is broken.
