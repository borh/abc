# Publication Surface Disposition — Sole Publication Producer, Task 1

Date: 2026-07-24
Companion to `abc/docs/superpowers/specs/2026-07-24-sole-publication-producer-design.md`
and `abc/docs/superpowers/plans/2026-07-24-sole-publication-producer.md`. This
report changes no production code. It is the closed disposition audit
required before Lane 2 may delete anything: one row per Soranoha command,
Clojure entry point, root/`abc` flake app and alias, schema/example, Accepted
claim, active document, in-repository caller, and test family touched by the
migration.

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
3. No root-flake CI check yet invokes the real `soranoha build-publication`
   app over a committed fixture through the flake-exported adapter/parser/
   mapping paths (plan Lane 0 requirement); this is a gap recorded here, not
   an existing surface to disposition.
