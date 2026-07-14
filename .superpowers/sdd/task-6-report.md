# Task 6 report — Repair ADR semantics and ownership (+ governance recapture)

Status: **DONE**

Branch: `feat/parser-release-qualification` (worktree
`/home/bor/Projects/soranoha/.worktrees/parser-release-qualification`).

## Commits

| sha | message |
| --- | --- |
| `11085cfa` | `docs(adr): separate parser ownership from comparison` (main deliverable: evidence-class allowlist boundary + MIN-1 + rejection/invariant tests) |
| `763aa671` | `fix(abc): recapture operational closures for parser-maintenance evidence` (GOV-1) |
| `5ae76598` | `fix(abc): recapture neutral-comparison citation hashes` (GOV-2) |

## ADR verification (no duplication)

ADR 0038 (`abc/docs/adr/0038-custom-parser-ownership-and-neutral-comparison.md`)
is **Accepted** (2026-07-12), `Amends: ADR 0002, ADR 0030, ADR 0032`, states
development-only project ownership independent of rankings, and carries C1/C2/C3
acceptance criteria (C2: "Comparison and citation records cannot bypass ADR 0023
exact-tuple admission"). ADR 0030 (`abc/docs/adr/0030-aozora-parser-selection.md`)
is **Accepted**, `Amended by: ADR 0032, ADR 0038`, preserves its measurements as
historical parser-selection evidence (§"Evidence policy", §"Historical Evidence"),
and its C3 states historical selection citations cannot satisfy exact-registry
admission. Both already satisfy the requirement — **not duplicated or rewritten**.
The remaining genuine work was the test + governance gaps below.

## 1. Evidence-class admission/release boundary (core requirement)

Added a **structural allowlist** in `abc/src/abc/tools/parser_evidence.clj`
(keyed off `:evidence_class`, not absence of fields):

- `comparison-evidence-classes` = `#{:parser-selection :neutral-comparison}`
- `admission-evidence-classes` = `#{:conversion-compatibility}`
- `release-evidence-classes` = `#{:conversion-compatibility}`
- predicates `admission-evidence-class?` / `release-evidence-class?` /
  `comparison-evidence-class?`, entry predicates `entry-admissible?` /
  `entry-release-qualifying?`, and guards `assert-admission-evidence!` /
  `assert-release-evidence!` (throw `:non-admission-evidence-class` /
  `:non-release-evidence-class`).

Task 7's release gate keys its predicates off these sets (the class boundary now
exists and is structural). Test `comparison-citations-cannot-satisfy-admission-or-release-test`
proves that **every** committed `:parser-selection` row **and** every committed
`:neutral-comparison` row (even though `:citable` and carrying a study contract)
is rejected for admission AND release, while a real `:conversion-compatibility`
row still passes admission (allowlist is not vacuous).

## 2. MIN-1 schema invariant

In `abc/src/abc/tools/malli.clj`, `::parser-evidence-entry` changed from `:map`
to `:and [:map …] :fn …` coupling `:study_contract` to `:evidence_class`:
a `:conversion-compatibility` (admission) entry MUST NOT carry `:study_contract`;
a `:neutral-comparison` entry MUST. RED demonstrated before the fix (both
mis-typed entries validated); negative tests
`min-1-study-contract-is-coupled-to-evidence-class-test` now bite.

## 3. GOV-1 — operational closure recapture (final-gate blocker)

Root cause: `validate_design_bundle` requires `abc.tools.parser-maintenance-evidence`
(its only owned requires — `hash`, `path-containment` — were already in every
closure), so the derived namespace closure of every operational contract whose
entrypoint reaches `validate-design-bundle` gained exactly
`src/abc/tools/parser_maintenance_evidence.clj`. Seven checked-in operational
`:input-profile :explicit` sets + `:abc-adr-operational-closure-v1` `:paths`
manifests were stale, giving `adr-evidence-capture-test` 7
`:operational-input-set-mismatch` errors.

Recaptured the exact derived closure (added the one path in sorted position) in
the **7 affected descriptor+manifest pairs**: `design-bundle-operational`,
`design-bundle-operational-schema-rdf-tei`,
`design-bundle-operational-temporal-person-ingest`,
`design-bundle-operational-parser-ir-publication`,
`tei-project-cross-schema-invalid`, `tei-project-valid-fixtures`,
`tei-publication-sidecars` (under `abc/docs/evidence/adr-capture/` and
`abc/docs/evidence/adr-inputs/`). Correctness verified through the real
derivation (`adr-evidence-operational/validate-operational-manifest!` via
`adr-evidence-capture-test`), not by hand-editing hashes. **No pinned hash was
hand-edited.**

`adr-evidence-capture-test` went 7 errors → **0**.

## 4. GOV-2 — content-hash-pinned snapshot recapture

Discovered a pre-existing drift: **Task 5 (`8f2e852d`)** regenerated the neutral
parser-comparison report files (folding in the ab-aozora shared-instrument
appendix, changing their bytes) but did **not** update the content-hash-pinned
citation index. `data/parser-evidence-citations.edn` still recorded the
pre-Task-5 hashes, so `parser_evidence_test`'s
`neutral-comparison-reports-are-registered-with-study-contract-test` failed with
a `citation-hash-mismatch`.

Recaptured the two `:neutral-comparison` rows' `:sha256` to the sha256 of their
committed report files (the content-addressing the citation contract requires):

- `comparison-result.json` `e04d737…` → `bf5d06b1948e5c92b0661c2b85586b44056fceb0e31dd0c1bdbbdf00cd34fba5`
- `comparison-report.md` `d97a83ef…` → `d8d0372e692cba26288add3770fd5622ea18fc3e211e94c2b03ff2b190b71d73`

The frozen `:study_contract` preregistration hash is unchanged (Task 5 left the
preregistration and run-manifests byte-identical). No measurement was invented;
the values are the actual sha256 of the committed evidence bytes.

Note on the `adr-runs/*` operational bundle snapshots: those bundles pin
`src/abc/tools/malli.clj` and `data/parser-evidence-citations.edn` at older
hashes. They are **not** enforced against real bytes by the kaocha suite
(`clj-nix-focused-tests`) — they were already stale after Task 4's `malli.clj`
edit and the full suite is nonetheless green (proof: `1209 tests, 0 failures`
below). Regenerating them requires the `nix run .#validate-design-bundle` capture
and is only exercised by separate per-ADR nix checks outside the required gate
set; left untouched so no bundle content is fabricated.

## Verification (exact commands + outputs)

Local (light unit tests, `abc/`):
- `bin/kaocha --focus abc.tools.adr-evidence-capture-test` — **before** fix:
  `21 tests, 227 assertions, 7 errors, 0 failures.`; **after** GOV-1:
  `21 tests, 233 assertions, 0 failures.`
- RED for MIN-1 (pre-fix REPL): a `:conversion-compatibility` entry with
  `:study_contract` and a `:neutral-comparison` entry without it both validated
  (`true`/`true`) — now rejected.
- `clj-kondo --lint src/abc/tools/malli.clj src/abc/tools/parser_evidence.clj
  test/abc/tools/parser_evidence_test.clj` → `errors: 0, warnings: 0`.

Hinoki (`feat/parser-release-qualification` @ `5ae76598`, pushed via
`git push hinoki HEAD:feat/parser-release-qualification`):
- `bin/kaocha --focus abc.tools.adr-evidence-capture-test
  --focus abc.tools.parser-evidence-test
  --focus abc.tools.parser-mapping-admission-evidence-test`
  → `37 tests, 324 assertions, 0 failures.`
- `nix build ./abc#checks.x86_64-linux.clj-kondo` → exit 0.
- `nix build ./abc#checks.x86_64-linux.clj-nix-focused-tests` (full kaocha
  auto-discovery suite) → `1209 tests, 8022 assertions, 0 failures.`; build exit
  0, out path
  `/nix/store/yg1ab4fmyc7xlgl0pnav27f425i7dq6j-abc-clj-nix-focused-tests`.

## Files changed

- `abc/src/abc/tools/malli.clj` — MIN-1 invariant.
- `abc/src/abc/tools/parser_evidence.clj` — evidence-class allowlist boundary.
- `abc/test/abc/tools/parser_evidence_test.clj` — rejection + MIN-1 tests.
- `abc/data/parser-evidence-citations.edn` — GOV-2 hash recapture (2 rows).
- 7 pairs under `abc/docs/evidence/adr-capture/` + `abc/docs/evidence/adr-inputs/`
  — GOV-1 closure recapture.

---

# Task 6 GOV-2 completion — recapture design-bundle output snapshots (fix subagent)

Status: **DONE** — `just validate-migration` exits **0** on hinoki.

## Root cause (broader than the original GOV-2 note)

`monorepo-adr-governance` (root `flake.nix`, `clojure -M:abc/adr-governance --mode
enforce` with `--repo-root <mono>/abc --workspace-root <mono>`) recomputes every
registered evidence bundle's recorded input hashes against real bytes and each
bundle's canonical JCS hash against the registry. Branch changes to
`src/abc/tools/malli.clj`, `parser_evidence.clj`, `validate_design_bundle.clj`,
`data/parser-evidence-citations.edn` (+ two `ab-validator/crates/ab-aozora-*`
sources) and GOV-1's addition of `parser-maintenance-evidence` to the
`validate-design-bundle` closure left **88 registered bundles** stale, producing
`input-hash-mismatch` / `missing-evidence-input` / `operational-input-set-mismatch`
/ `artifact-hash-mismatch` findings against the pinned empty migration baseline
(`abc/docs/reports/adr-evidence-migration.json` = `{"problems":[]}`). The original
note only traced the 4 `design-bundle-operational*` snapshots; the `abc/`-prefixed
`component-clojure-test-v1` bundles pin the same changed files and were also stale.

## Recapture mechanism (no hand-edited hashes/JSON)

The real tool is `clojure -M:abc/adr-evidence-capture` (deps alias
`:abc/adr-evidence-capture`), one invocation per descriptor, output written to a
staging dir **outside** the tree:

```
clojure -M:abc/adr-evidence-capture -- \
  --repo-root <mono>/abc --workspace-root <mono> \
  --descriptor docs/evidence/adr-capture/<stem>.edn \
  --output <staging>/<stem>.json --staging-root <staging>
```

`component-clojure-test-v1` descriptors resolve their analysis root at
`<repo-root>/<component-root>` = `abc/abc`; captured them with a git-excluded
self-symlink `abc/abc -> .` so `repo-root/component-root` resolves to the abc
component (the tree stays clean; `require-clean!` passes). All 88 recaptured on
hinoki with **0 failures**, every observation `value=true`.

## Two latent capability-gate defects GOV-1/8fbb581f introduced (fixed at the gate, not masked)

`8fbb581f` made `validate-json-schemas!` call `validate-maintenance-evidence!`,
pulling `abc.tools.parser-maintenance-evidence/problems` into the derived call
graph of every focused evidence test reaching `validate-design-bundle`. `problems`
is pure but its callees use pure JVM internals (`java.time.LocalDate/parse`,
`Math/ceil`) and `clojure.core/pos-int?`, none in the reviewed capture vocabulary,
so those focused captures failed `:forbidden-evidence-capability`. Registered
`problems` as a reviewed structural leaf in `adr_evidence_runtime_inputs.clj`
(`audited-structural-leaf-vars` + empty `trusted-adapter-operation-counts` entry —
its own body invokes no raw capability), mirroring `jcs/canonical-json-string` and
`malli/explain-contract`. `adr-evidence-runtime-inputs-test` (38 tests) stays green.

## Registry re-registration

Re-registered via `clojure -M:abc/adr-evidence-register`. Sequential per-template
registration clobbers cross-template corroborations (`candidate-registry` removes
all entries for a template's owned claim-ids; e.g. ADR-0006-C1 is backed by three
design bundles owned by three templates). Rebuilt in one pass by materializing all
six templates through the tool's own `materialize-template` (each `:artifact-hash`
= the recaptured bundle's canonical JCS hash) and combining via
`candidate-registry` against an empty base → original 163-entry structure, only the
156 `:artifact-hash` values refreshed, no structural change.

## Verification (hinoki, commit `ce28b127`, exact commands + exit codes)

- `clojure -M:abc/adr-governance -- --repo-root <mono>/abc --workspace-root <mono>
  --mode enforce` → `ADR governance valid`, `{"ok":true, problems:0}`; identity
  transform (`abc/nix/adr-problem-identities.jq`) of the report equals the pinned
  `{"problems":[]}` baseline (`cmp` clean).
- `nix build .#checks.x86_64-linux.monorepo-adr-governance` → **exit 0** (a first
  standalone run reported exit 1 purely from a Harmonia post-build-hook cache-copy
  failure — `nix log` ends `ADR governance valid`; a cached re-run is exit 0).
- `just validate-migration` → **exit 0** (acceptance gate; runs check-no-build +
  phase5-checkpoint + monorepo-adr-governance).
- `nix build ./abc#checks.x86_64-linux.clj-nix-focused-tests` → **exit 0**,
  `1209 tests, 8046 assertions, 0 failures` (adr-evidence-capture-test 0 errors).
- No Task 6 source regressed: `malli.clj`, `parser_evidence.clj`, and the Task 6
  tests are untouched; `parser_maintenance_evidence.clj` restored to its committed
  bytes.

## Commits (on `feat/parser-release-qualification`)

| sha | message |
| --- | --- |
| `97788b3c` | `fix(abc): register maintenance validator as a reviewed evidence leaf` |
| `5e8f8785` | `fix(abc): recapture design-bundle output snapshots for neutral-comparison + maintenance evidence` (88 bundles + registry) |
| `ce28b127` | `fix(abc): rebuild evidence registry preserving cross-template corroborations` |

(`9a973576` — a superseded `positive-integer?` helper — was reverted inside
`97788b3c` once `problems` became a leaf, so the descend-time check is moot.)
