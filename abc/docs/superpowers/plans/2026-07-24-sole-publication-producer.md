# Sole Publication Producer Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make `soranoha build-publication` the sole assembler and installer of
an authenticated publication root, make release admissibility a recomputable
fail-closed predicate, emit one truthful snapshot-index 0.2.0 value from the
direct build, and delete the request-set rehearsal/reproduction composition.

**Architecture:** The retained system has four deep boundaries.
`abc.tools.parser-release-authority` authenticates the exact accepted parser
candidate from the strict decision corpus and parser-RQ promotion values.
`abc.tools.materialize-publication/materialize-publication!` remains the only
per-work publication renderer. `abc.tools.snapshot-index` constructs and
validates the one corpus identity from actual selected sources, parser
coordinates, failures, and manifest references.
`abc.tools.publication-release/evaluate` is a pure predicate over a loaded
release closure and explicit authority values. The build façade owns source
trust, assembly, installation, and the release-facing exit status. Projection
commands consume the index; none render or reconstruct it.

**Tech Stack:** Clojure 1.12, `clojure.test` and Kaocha, JSON Schema 2020-12,
EDN decision/registry/policy values, RFC 8785/JCS SHA-256 identities, Nix
flakes, shell smoke checks, Git provenance.

**Spec:** Read
`abc/docs/superpowers/specs/2026-07-24-sole-publication-producer-design.md`
before implementation. Paths below are relative to the monorepo root.

## Global Constraints

- Preserve unrelated worktree changes. Use `apply_patch` for hand edits; never
  reset or overwrite user work.
- Work on an isolated branch/worktree. Commit after every green task. Do not
  merge or push an intermediate state that advertises snapshot-index 0.2.0
  while the rehearsal producer is still a supported command.
- Root `flake.nix` and root `justfile` remain the primary integration surface.
- ABC owns publication schemas, TEI policy, manifest identity, and registry
  admission. `ab-validator` continues to own parser/adaptor measurement, AAT
  evidence, parser-IR conversion evidence, and corpus reports.
- `build-publication` is the only operation that may select official source,
  render a corpus, construct `snapshot-index.json`, and install the resulting
  root. Do not add a compatibility wrapper that repeats that composition.
- `materialize-publication!` remains callable by qualification and fixture
  tools. Those callers render candidates; they do not assemble a release.
- Admission is not a receipt. No `admission.json`, admitted marker, mutable
  registry row, or occupancy-based inference may be introduced.
- `publication-release/evaluate` returns
  `{:admissible? boolean :problems vector-of-problem-maps
  :authority-hashes {:decisions sha256 :registry sha256 :rights-policy
  sha256}}` and
  performs no writes. It projects
  `publication-release/release-problems` and
  `publication-release/release-admissible?`; it is not a second policy
  implementation. Installation alone never implies admissibility.
- The direct index is exactly version `0.2.0`. Do not emit 0.1.1 beside it, do
  not synthesize `request_set_id`, and do not carry tokenizer/analysis
  identities that do not determine publication artifacts.
- The index and all manifest/reference locators are relative. Absolute source,
  cache, temporary, or installed-root paths remain operational values outside
  identity.
- `candidate_ref` and `qualification_identity_ref` may be null only for an
  explicitly diagnostic parser profile. `parser_build_hash`,
  `parser_config_hash`, `aat_parser_ir_mapping_hash`, and
  `parser_ir_schema_hash` are always concrete hashes. The admissibility
  predicate rejects either nullable authority coordinate.
- Official source mode fails before rendering when Git state cannot be proved
  clean. Fixture mode is an explicit config value, is recorded, may build a
  diagnostic root, and can never be release-admissible.
- Remove direct-publication cache reuse. Every build rerenders per-work
  publication artifacts and regenerates manifests. Do not replace it with a
  new content-cache protocol in this campaign.
- Strict failure never replaces the prior root. Best-effort failure may install
  one inspectable diagnostic root, returns exit 1, and remains inadmissible.
- The current rights state in `abc/data/publication-policy.edn` remains
  fail-closed. This campaign moves the check into the shared predicate; it does
  not change which rights state authorizes publication.
- Preserve the live single-work and batch renderer adapters. Retire only their
  misleading release authority and duplicate orchestration where the task
  names it.
- Historical reports/specs retain recorded command strings and old schema
  examples. Active examples, help, architecture docs, and runbooks describe
  only the surviving path.
- Run `scripts/comment-hygiene-check.sh` after active-source edits. Do not cite
  this plan from source comments.

## Target Value Contracts

### Snapshot index 0.2.0

The live `snapshot-index.json` has these top-level keys and no others:

```clojure
{"schema_id" snapshot-index-schema-id
 "schema_version" "0.2.0"
 "schema_hash" sha256
 "snapshot_date" "YYYY-MM-DD"
 "generated_at" rfc3339
 "snapshot_identity_hash" sha256
 "snapshot_index_identity_object" identity-object
 "source_selection_identity_object" source-selection
 "parser_runtime_identity_object" parser-runtime-identity
 "failure_policy" failure-policy
 "layout_policy" layout-policy
 "failures" vector-of-failure-records
 "artifact_references" vector-of-artifact-references
 "summary" summary}
```

The identity object has these exact keys:

```clojure
{"snapshot_index_schema_hash" sha256
 "source_selection_hash" sha256
 "candidate_ref" (or sha256 nil)
 "qualification_identity_ref" (or sha256 nil)
 "parser_build_hash" sha256
 "parser_config_hash" sha256
 "aat_parser_ir_mapping_hash" sha256
 "parser_ir_schema_hash" sha256
 "artifact_set_hash" sha256
 "failure_set_hash" sha256
 "failure_policy_hash" sha256
 "layout_policy_hash" sha256
 "schema_hashes" vector-of-sha256}
```

`source_selection_identity_object` has:

```clojure
{"trust_mode" "official-git"|"fixture"
 "aozora_git_commit" (or 40-lowercase-hex nil)
 "catalog_csv_hash" sha256
 "snapshot_date" "YYYY-MM-DD"
 "sources"
 [{"work_id" string
   "person_id" string
   "slug" string
   "text_zip_relpath" relative-path
   "archive_hash" sha256
   "bundle_hash" sha256
   "primary_text_member" relative-archive-path
   "primary_text_hash" sha256
   "metadata_record_hash" sha256}]} ; zero or more source rows
```

Sources sort by `[work_id person_id text_zip_relpath]`. Failure identity is the
sorted vector of `{"stage" string, "work_slug" string-or-null, "code" string}`;
diagnostic messages and host paths may appear in the top-level failure records
but do not enter `failure_set_hash`. Artifact identity excludes locators, as in
0.1.1, so staging may change places without changing the source release value.

`parser_runtime_identity_object` has these exact path-free keys:

```clojure
{"adapter_id" string
 "adapter_argv_template" vector-of-strings
 "converter_argv_template" vector-of-strings
 "parser_build_hash" sha256
 "converter_build_hash" sha256
 "aat_parser_ir_mapping_hash" sha256
 "parser_ir_schema_hash" sha256}
```

Artifact references close over four canonical artifact kinds for every
successful work: `source`, `parser-ir`, `plaintext`, and `tei`. The source
manifest names `source-bundle.json` as content; the parser-IR manifest names
`parser-ir.json`; the TEI manifest already authenticates preservation and
validation sidecars. Every reference has a required `work_slug`; staging uses
that value instead of parsing identity from a locator string. A closure
verifier rejects an unreferenced regular file inside a referenced per-work
publication directory or a referenced source/parser/publication file with a
wrong hash. The exact derived
`publications/publications-report.json` is outside that scan and outside
release identity.

### Parser runtime identity

`resolve-adapter` returns the existing execution paths plus:

```clojure
{:adapter-id string
 :wrapper absolute-path
 :converter absolute-path
 :mapping absolute-path
 :extra-env {string string}
 :argv-template vector-of-strings
 :converter-argv-template vector-of-strings
 :parser-build-hash sha256
 :converter-build-hash sha256
 :mapping-hash sha256
 :parser-ir-schema-hash sha256
 :parser-config-hash sha256}
```

`parser-build-hash` hashes the actual source-to-AAT executable bytes.
`mapping-hash` uses the mapping document's governed JSON construction, matching
the parser-RQ qualification identity rather than hashing whitespace bytes.
`converter-build-hash` hashes the actual AAT-to-parser-IR executable bytes.
`parser-config-hash` is JCS SHA-256 over the complete
`parser_runtime_identity_object`: adapter id, both argv templates, both
executable hashes, mapping hash, and parser-IR schema hash. It contains no Nix
store path. The parser runtime identity object is explanatory index data; its
hash is the authoritative `parser_config_hash`.

### Release evaluation input

`abc.tools.publication-release/evaluate` consumes one explicit value:

```clojure
{:index snapshot-index
 :closure-problems vector-of-problem-maps
 :parser-authority parser-authority
 :rights-policy rights-policy
 :authority-hashes
 {:decisions sha256 :registry sha256 :rights-policy sha256}}
```

Problem maps have exactly `:code`, `:message`, and optional `:path`,
`:expected`, and `:actual`. Tests assert codes, not prose.
Authority hashes are SHA-256 hashes of the exact decision-corpus,
compatibility-registry, and rights-policy file bytes used for the evaluation.
Source trust and runtime parser identity are read from the immutable index
value; they are not supplied a second time in parallel parameters.

---

### Task 1: Freeze the Current Surface and State Transitions

**Files:**
- Create: `abc/docs/superpowers/reports/2026-07-24-publication-surface-disposition.md`
- Modify: `abc/test/abc/tools/soranoha_test.clj`
- Modify: `abc/test/abc/sim/content_sim_test.clj`

**Interfaces:** This task changes no production behavior. It records the
producer/projection/consumer disposition and pins strict failure, best-effort
diagnostic installation, atomic replacement, renderer equivalence, and
operational-path normalization.

- [ ] **Step 1: Write the closed disposition report**

Inventory with `rg` and record one row for every Soranoha command, Clojure
entry point, root/ABC app and alias, schema/example, Accepted claim, active
document, in-repository caller, and test family touched by the migration. Use
only these dispositions: `release authority`, `shared domain capability`,
`non-release renderer adapter`, `projection`, `fixture characterization`,
`qualification instrument`, `frozen historical reference`, and `retire`.

The report must explicitly retain the single/batch renderer adapters,
parser-RQ publication materializer, `workflow/run-workflow!`, source/request-set
domain values, and staging. It must explicitly retire
`snapshot-index`, `reproduce`, `publication-rehearsal`, `validate-workflow`,
their dispatcher wiring, and only their apparatus tests.

- [ ] **Step 2: Add failing characterization assertions**

Extend the current build tests without changing production code:

- strict derive failure leaves an existing sentinel root byte-for-byte intact;
- best-effort derive failure installs the current partial root and exits 1;
- a successful build replaces an existing root only with `--replace`;
- one direct-build publication equals a direct
  `materialize-publication!` invocation after removing only generated-at
  provenance;
- `build-plan.json` currently contains a temporary absolute
  `materialized_root`, pinning the defect that Task 6 removes.

Run:

```sh
cd abc
bin/kaocha --focus abc.tools.soranoha-test
bin/kaocha --focus abc.sim.content-sim-test
```

Expected: all assertions describing current behavior pass.

- [ ] **Step 3: Commit the characterization**

```sh
git add abc/docs/superpowers/reports/2026-07-24-publication-surface-disposition.md \
  abc/test/abc/tools/soranoha_test.clj \
  abc/test/abc/sim/content_sim_test.clj
git commit -m "test(publication): characterize producer cutover"
```

---

### Task 2: Share the Strict Decision-Corpus Boundary

**Files:**
- Modify: `abc/src/abc/tools/decisions.clj`
- Modify: `abc/test/abc/tools/decisions_test.clj`
- Modify: `abc/src/abc/tools/parser_rq_campaign.clj`
- Modify: `abc/test/abc/tools/parser_rq_campaign_test.clj`

**Interfaces:**

```clojure
(decisions/load-shape-valid-corpus! path) ; => corpus, otherwise ex-info :errors
(decisions/decision-by-slug! corpus slug) ; => one record, otherwise ex-info
```

- [ ] **Step 1: Write failing decision-boundary tests**

Cover exactly one valid form, two EDN forms, invalid corpus shape, duplicate
slugs, and a missing slug. Assert stable `:errors` vectors. Keep corpus semantic
governance outside this loader; this boundary promises strict load plus shape.

- [ ] **Step 2: Implement the two functions**

Compose the existing `load-corpus` and `shape-problems`; do not add a second
Malli schema or EDN reader. `decision-by-slug!` searches the already
shape-valid corpus and fails when absent.

- [ ] **Step 3: Rebase promotion status loading**

Replace `parser_rq_campaign.clj`'s private loader/shape composition with
`load-shape-valid-corpus!` and `decision-by-slug!`. Keep current CLI messages
and ADR 0040/0041 dependency behavior.

Run:

```sh
cd abc
bin/kaocha --focus abc.tools.decisions-test
bin/kaocha --focus abc.tools.parser-rq-campaign-test
```

Expected: both focused suites pass, including the malformed-corpus promotion
tests.

- [ ] **Step 4: Commit**

```sh
git add abc/src/abc/tools/decisions.clj \
  abc/test/abc/tools/decisions_test.clj \
  abc/src/abc/tools/parser_rq_campaign.clj \
  abc/test/abc/tools/parser_rq_campaign_test.clj
git commit -m "refactor(decisions): share strict corpus resolution"
```

---

### Task 3: Expose One Authenticated Parser-Promotion Value

**Files:**
- Modify: `abc/src/abc/tools/parser_rq_campaign.clj`
- Modify: `abc/test/abc/tools/parser_rq_campaign_test.clj`
- Create: `abc/src/abc/tools/parser_release_authority.clj`
- Create: `abc/test/abc/tools/parser_release_authority_test.clj`

**Interfaces:**

```clojure
(campaign/promotion-verification opts)
; => {:problems vector-of-problem-strings
;     :candidate candidate
;     :qualification-report report
;     :evaluation evaluation-index
;     :registry-ref sha256
;     :provenance executable-provenance}

(parser-release-authority/authenticate opts)
; => {:candidate-ref sha256
;     :qualification-identity-ref sha256
;     :qualification-identity map
;     :executable-provenance map
;     :decision record
;     :authority-hashes {:decisions sha256 :registry sha256}}
; throws ex-info {:problems vector-of-problem-maps} on any failure
```

- [ ] **Step 1: Refactor `promotion-errors` under a failing result test**

Move the existing `promotion-errors` derivation into
`promotion-verification`. Return authenticated values only when their existing
checks can be evaluated; return problems on malformed input.
`promotion-errors` becomes exactly:

```clojure
(defn promotion-errors [opts]
  (:problems (promotion-verification opts)))
```

The existing `verify-promotion` command and P5 tests must remain byte-for-byte
compatible at stdout/stderr.

Run and commit the behavior-preserving extraction:

```sh
cd abc
bin/kaocha --focus abc.tools.parser-rq-campaign-test
cd ..
git add abc/src/abc/tools/parser_rq_campaign.clj \
  abc/test/abc/tools/parser_rq_campaign_test.clj
git commit -m "refactor(parser-rq): expose authenticated promotion value"
```

- [ ] **Step 2: Write parser release authority tests**

Use the exact committed P5 candidate
`sha256:15affdfb677cc6a94a4a5364da68ca2d11441f899737651e727dbac90eddc5ab`.
Assert the authenticated value contains qualification ref
`sha256:6f365a44b975465943da88d0e3fe4f123672e00913285a3e998ab465bc79edca`,
mapping hash
`sha256:9be58ff3fea272c2a94ae16f05e3e362425e8bcdd20c482a4a842c13fe067142`,
and the `ab-aozora` executable hash from bound provenance.

Add negative tests for malformed decisions, missing decision, non-accepted
status, wrong `:release-authority`, wrong `:validation-scope`, wrong candidate
ref, stale registry, and promotion verification problems.

- [ ] **Step 3: Implement the authority namespace**

Use `decisions/load-shape-valid-corpus!` to resolve
`custom-parser-release-qualification`. Require exactly:

```clojure
{:status :accepted
 :release-authority :publication
 :validation-scope :smoke-corpus}
```

Call `campaign/promotion-verification` with explicit paths for the runs root,
registry, measurements, qualification report, executable provenance, and
decisions. Do not re-read or independently reinterpret candidate/evaluation
values after campaign verification.

Run:

```sh
cd abc
bin/kaocha --focus abc.tools.parser-rq-campaign-test
bin/kaocha --focus abc.tools.parser-release-authority-test
nix build .#checks.x86_64-linux.parser-rq-p5-promotion-audit --print-build-logs
```

Expected: all focused tests pass and the committed P5 audit prints `ok`.

- [ ] **Step 4: Commit the new authority consumer**

```sh
git add abc/src/abc/tools/parser_release_authority.clj \
  abc/test/abc/tools/parser_release_authority_test.clj
git commit -m "feat(publication): authenticate parser release authority"
```

---

### Task 4: Make Source Trust and Parser Runtime Identity Explicit

**Files:**
- Modify: `abc/schemas/soranoha-publication-build-config.schema.json`
- Modify: `abc/config/full-corpus-publication-basic-ja.json`
- Modify: `abc/config/full-corpus-publication-custom-parser-ja.json`
- Modify: `abc/config/publication-basic-ja.json`
- Modify: `abc/src/abc/tools/soranoha_build_publication.clj`
- Modify: `abc/test/abc/tools/soranoha_build_publication_test.clj`
- Modify: `abc/test/abc/tools/soranoha_test.clj`
- Modify: `abc/test/abc/sim/content_sim_test.clj`

**Interfaces:** Config version 0.2.0 removes `request_set_label` and
`snapshot_scope`, adds `config_schema_version`, `source_trust_mode`, and
`parser_candidate_ref`.

- [ ] **Step 1: Write failing config and Git-trust tests**

Pin these cases:

- `official-git` requires successful `git rev-parse HEAD` and
  `git status --porcelain -- cards index_pages`;
- command failure, a non-Git directory, or nonblank status fails with stable
  codes `source-git-unavailable` or `source-git-dirty`;
- `fixture` records null Git commit and never calls Git;
- all existing temp-directory fixtures explicitly use `fixture`;
- custom-parser config carries the exact P5 candidate ref;
- diagnostic `aozora2html` configs carry null candidate ref.

- [ ] **Step 2: Version the config schema and files**

The required keys become:

```json
[
  "config_schema_id",
  "config_schema_version",
  "source_trust_mode",
  "parser_profile",
  "parser_candidate_ref",
  "publication_profile",
  "continue_on_failure",
  "materialization_scope"
]
```

Set `config_schema_version` to `0.2.0`. Do not keep deprecated keys.

- [ ] **Step 3: Compute runtime parser identity once**

Resolve the adapter before source derivation. Hash actual executable bytes,
parse/hash the mapping using its governed JSON construction, read the target
parser-IR schema hash from the mapping, resolve and hash
`AB_AAT_TO_PARSER_IR_BIN` once, and compute the path-free config hash. Pass the
resolved parser/converter value into every parser invocation; do not resolve
environment variables once per work.

For `ab-aozora`, authenticate the configured candidate through
`parser-release-authority/authenticate` and compare adapter name, parser build
hash, converter build hash, mapping hash, mapping/schema coordinates, and both
argv templates. Record problems instead of inventing an admitted flag. For
`aozora2html`, record the runtime hashes and an absent-candidate problem.

- [ ] **Step 4: Move source trust ahead of temporary-root writes**

Replace `git-provenance`'s nil-means-clean behavior with an explicit
`source-provenance!` boundary. Official failure throws before
`prepare-output-root!`. Fixture mode returns a recorded non-release value.

Run:

```sh
cd abc
bin/kaocha --focus abc.tools.soranoha-build-publication-test
bin/kaocha --focus abc.tools.soranoha-test
bin/kaocha --focus abc.sim.content-sim-test
```

Expected: official unprovable/dirty tests fail closed, fixture builds reach the
renderer, and config tests accept only 0.2.0.

- [ ] **Step 5: Commit**

```sh
git add abc/schemas/soranoha-publication-build-config.schema.json \
  abc/config/full-corpus-publication-basic-ja.json \
  abc/config/full-corpus-publication-custom-parser-ja.json \
  abc/config/publication-basic-ja.json \
  abc/src/abc/tools/soranoha_build_publication.clj \
  abc/test/abc/tools/soranoha_build_publication_test.clj \
  abc/test/abc/tools/soranoha_test.clj \
  abc/test/abc/sim/content_sim_test.clj
git commit -m "fix(publication): fail closed on source and parser identity"
```

---

### Task 5: Propagate Parser Identity Through the Sole Renderer

**Files:**
- Modify: `abc/src/abc/tools/materialize_publication.clj`
- Modify: `abc/test/abc/tools/materialize_publication_test.clj`
- Modify: `abc/src/abc/tools/soranoha_build_publication.clj`
- Modify: `abc/test/abc/tools/soranoha_test.clj`

**Interface:**

```clojure
(materialize-publication!
 {:parser-ir-path "work/parser-ir.json"
  :metadata-record-path "work/metadata-record.json"
  :persons-dir "persons"
  :source-manifest-path "work/source.manifest.json"
  :output-dir "publications/work-slug"
  :generated-at "2026-07-24T00:00:00Z"
  :parser-identity
  {:parser-build-hash sha256
   :parser-config-hash sha256
   :mapping-hash sha256
   :parser-ir-schema-hash sha256}})
```

- [ ] **Step 1: Write failing renderer identity tests**

Assert all four hashes appear in both plaintext and TEI
`manifest_identity_object` values and in their sorted provenance `used`
arrays. Assert changing any one hash rotates the artifact id while leaving
rendered plaintext/TEI bytes unchanged.

Keep the argument optional for the live non-release single/batch adapters;
their manifests may retain null parser coordinates until those callers can
supply authenticated values.

- [ ] **Step 2: Implement and pass the value through the build**

Change `manifest-inputs`, `plaintext-manifest`, `tei-manifest`, and
`materialize-publication!` to consume the explicit value. Do not read
environment variables or parser authority inside the renderer.

Run:

```sh
cd abc
bin/kaocha --focus abc.tools.materialize-publication-test
bin/kaocha --focus abc.tools.soranoha-test
```

Expected: renderer tests prove identity propagation and the build's manifests
contain four non-null parser coordinates.

- [ ] **Step 3: Commit**

```sh
git add abc/src/abc/tools/materialize_publication.clj \
  abc/test/abc/tools/materialize_publication_test.clj \
  abc/src/abc/tools/soranoha_build_publication.clj \
  abc/test/abc/tools/soranoha_test.clj
git commit -m "feat(publication): bind renderer manifests to parser identity"
```

---

### Task 6: Delete the Unsafe Publication Cache and Place-Valued Records

**Files:**
- Modify: `abc/src/abc/tools/soranoha_build_publication.clj`
- Modify: `abc/test/abc/tools/soranoha_test.clj`
- Modify: `abc/test/abc/sim/content_sim_test.clj`
- Modify: `abc/test/abc/sim/oracle.clj`
- Modify: `abc/test/abc/sim/content_test.clj`

**Behavior:** Every selected successful work renders in the fresh temporary
root. Reports use `passed`/`failed`; `reused` and `skipped` are not
direct-publication statuses.

- [ ] **Step 1: Replace cache expectations with identity expectations**

Delete tests whose only subject is `publication-up-to-date?`,
`copy-dir-files!`, `source_work_content_hash.txt`, or a `reused`/`skipped`
status. Preserve P16.3 and source-bundle equality tests. Add tests proving a
second identical build rerenders but produces the same artifact ids after
normalizing `generated_at`, and parser/mapping/profile changes rotate the
appropriate ids.

- [ ] **Step 2: Delete cache production code**

Remove `publication-up-to-date?`, `copy-dir-files!`, `prior-output-root`,
cache-marker writes, old publication-directory copying, and reused/skipped
report counters. Keep `--replace` solely as atomic installation policy.

- [ ] **Step 3: Remove absolute places from retained records**

`build-plan.json` records relative logical locators
`materialized-root`, `source-selection-report.json`, and `publications`; it
does not record `aozora_root`, the temporary `materialized_root`, the prior
output root, or an installed absolute path. Workflow state may contain places
in memory but must not serialize them as release identity.

Run:

```sh
cd abc
bin/kaocha --focus abc.tools.soranoha-test
bin/kaocha --focus abc.sim.content-sim-test
bin/kaocha --focus abc.sim.content-test
```

Expected: no direct-publication test or report mentions reused/skipped/cache
markers, and P16.3 remains green.

- [ ] **Step 4: Commit**

```sh
git add abc/src/abc/tools/soranoha_build_publication.clj \
  abc/test/abc/tools/soranoha_test.clj \
  abc/test/abc/sim/content_sim_test.clj \
  abc/test/abc/sim/oracle.clj \
  abc/test/abc/sim/content_test.clj
git commit -m "fix(publication): remove unsafe manifest reuse"
```

---

### Task 7: Retire the Competing Publication Composition Atomically

**Files:**
- Modify: `abc/src/abc/tools/soranoha.clj`
- Modify: `abc/test/abc/tools/soranoha_test.clj`
- Modify: `abc/test/abc/tools/soranoha_annotation_test.clj`
- Modify: `abc/deps.edn`
- Modify: `abc/flake.nix`
- Modify: `abc/docs/architecture.md`
- Modify: `abc/tools/tei_eaj_aozora_reports.py`
- Create: `abc/tools/test_tei_eaj_aozora_reports.py`
- Delete: `abc/data/snapshot-plans/demo-annotation-ja.json`
- Delete: `abc/data/snapshot-plans/demo-basic-ja.json`
- Delete: `abc/data/snapshot-plans/smoke-basic-ja.json`

**Deletion set:** `build-snapshot-index`, `snapshot-index!`,
`materialize-snapshot-root!`, `reproduce!`, `publication-rehearsal!`, the
`snapshot-index`, `reproduce`, `publication-rehearsal`, and `validate-workflow`
dispatcher commands, and their exclusive helpers/tests. This deletion lands
before index v2: the project temporarily has one direct publication producer
but does not advertise a release-index protocol until Task 8.

- [ ] **Step 1: Run the closed consumer sweep**

Run:

```sh
rg -n 'build-snapshot-index|snapshot-index!|materialize-snapshot-root!|reproduce!|publication-rehearsal!|validate-workflow|publication-rehearsal|soranoha reproduce|soranoha snapshot-index' \
  abc/src abc/test abc/deps.edn abc/flake.nix abc/bin abc/data abc/config abc/docs \
  ab-validator justfile flake.nix scripts tests
```

Update the Task 1 disposition report. Stop deletion if any row is `unknown`.
Historical report/plan command strings are frozen references and remain.

- [ ] **Step 2: Delete orchestration tests before production**

Delete tests whose sole subject is request-set publication reproduction,
snapshot-plan production, rehearsal workflow/report, or the dead dispatcher
command. Keep validation, explanation, report, and staging characterization
against the checked-in 0.1.1 example until Task 8 rebases those projections.
Keep annotation domain tests that do not require the retired producer.

- [ ] **Step 3: Delete the vertical slice**

Remove the five producer functions and exclusive helpers from
`soranoha.clj`. Remove command-table entries, usage/help expectations,
exclusive deps aliases/launchers, workflow/report emitters, and the three
snapshot-plan files listed above. Retain all request-set definitions and
resolved request sets: `full-corpus-publication-basic-ja` is also a live
plaintext-view domain value consumed by analysis identity and design-bundle
validation. The current ABC flake exports only the general Soranoha
app, so change no Nix app unless the Step 1 sweep finds new live wiring. Keep
`read-valid-snapshot-index`, live projections, request-set/analysis/annotation
domain capabilities, and `workflow/validate-run`.

- [ ] **Step 4: Remove the stale report fallback place**

Change `default_abc_tei_dirs` to return only the explicit
`ABC_TEI_EAJ_ABC_TEI_DIRS` values, or an empty vector. Delete the implicit
`target/soranoha/full-corpus-publication-basic-ja/artifacts` fallback: it names
the retired producer's place and is not a value contract. Add a unittest for
explicit env, empty env, and a coincidentally existing old target directory.

- [ ] **Step 5: Make absence executable**

Add a focused test that the four retired command names return the unknown
command exit and that the five Clojure vars do not resolve. Add an `rg` absence
assertion to the existing architecture/contract check; exempt historical
`docs/superpowers/{plans,reports,specs}`.

Run:

```sh
cd abc
bin/kaocha --focus abc.tools.soranoha-test
bin/kaocha --focus abc.tools.soranoha-annotation-test
bin/kaocha --focus abc.tools.workflow-test
bin/kaocha
cd ..
python -m unittest discover -s abc/tools -p 'test_tei_eaj_aozora_reports.py'
just python-quality
```

Expected: the full ABC suite passes with the retired commands/functions absent
and the retained read-only 0.1.1 projections still characterized.

- [ ] **Step 6: Commit the breaking deletion**

```sh
git add -u -- abc/src/abc/tools/soranoha.clj \
  abc/test/abc/tools/soranoha_test.clj \
  abc/test/abc/tools/soranoha_annotation_test.clj \
  abc/deps.edn abc/flake.nix abc/docs/architecture.md \
  abc/data/snapshot-plans \
  abc/tools/tei_eaj_aozora_reports.py
git add -- abc/tools/test_tei_eaj_aozora_reports.py
git add -- abc/docs/superpowers/reports/2026-07-24-publication-surface-disposition.md
git commit -m "refactor(publication)!: retire rehearsal release producer"
```

---

### Task 8: Version Snapshot Index and Its Projections Atomically

**Files:**
- Modify: `abc/schemas/snapshot-index.schema.json`
- Modify: `abc/src/abc/tools/snapshot_index.clj`
- Replace: `abc/test/abc/tools/snapshot_index_test.clj`
- Replace: `abc/examples/v0/snapshot/snapshot-index.json`
- Modify: `abc/test/abc/tools/schema_test.clj`
- Modify: `abc/src/abc/tools/validate_design_bundle.clj`
- Modify: `abc/test/abc/tools/validate_design_bundle_test.clj`
- Modify: `abc/src/abc/tools/soranoha.clj`
- Modify: `abc/src/abc/tools/soranoha_layout_report.clj`
- Modify: `abc/test/abc/tools/soranoha_layout_report_test.clj`
- Modify: `abc/src/abc/tools/soranoha_stage_publication.clj`
- Modify: `abc/test/abc/tools/soranoha_stage_publication_test.clj`
- Modify: `abc/schemas/schema-contracts.json`
- Modify: `ab-validator/data/abc-schemas/nix-schemas/snapshot-index.schema.json`
- Modify: `ab-validator/data/abc-schemas/schema-contracts.json`

**Interfaces:**

```clojure
(snapshot-index/source-selection-hash source-selection) ; => sha256
(snapshot-index/failure-set-hash failures)              ; => sha256
(snapshot-index/artifact-set-hash references)           ; => sha256
(snapshot-index/build-snapshot-index args)               ; => v0.2.0 map
(snapshot-index/validate-snapshot-index! index)           ; => true or throws
(snapshot-index/closure-problems root index) ; => vector-of-problem-maps
```

- [ ] **Step 1: Replace 0.1.1 tests with 0.2.0 value tests**

Test canonical source sorting, stable failure projection, locator-independent
artifact identity, schema/policy hashes, nullable candidate coordinates for
diagnostics, non-null runtime parser hashes, and identity rotation for every
identity field. Require `work_slug` on every artifact reference and include it
in artifact-reference identity and sorting. Test that request-set, tokenizer,
analysis, old snapshot label, and manifest-index keys are rejected by the
closed schema.

- [ ] **Step 2: Implement the 0.2.0 schema and pure builders**

Use the exact contract under “Target Value Contracts.” Remove
`snapshot-plans-dir`, `snapshot-label?`, `read-snapshot-plan`,
`build-snapshot-index-from-plan`, request-set fields, tokenizer/analysis
arrays, and the hardcoded snapshot-label fallback from this namespace.

- [ ] **Step 3: Implement closed-reference verification**

For every artifact reference:

1. require a relative, normalized locator contained by the root;
2. rehash the manifest and compare `manifest_content_hash`;
3. validate the manifest schema;
4. rehash its content and sidecars;
5. compare artifact id, kind, validation status, and content hash;
6. reject extra regular files in each referenced per-work publication
   directory, while allowing only the exact derived
   `publications/publications-report.json` at the collection root.

Return problem maps; do not render, repair, or mutate.

- [ ] **Step 4: Replace the active example and schema expectations**

The example must contain one source, parser-IR, plaintext, and TEI reference
with coherent hashes. Regenerate it through Clojure constructors, then commit
the deterministic bytes. `schema_test.clj` expects `0.2.0`.

Run:

```sh
cd abc
bin/kaocha --focus abc.tools.snapshot-index-test
bin/kaocha --focus abc.tools.schema-test
bin/kaocha --focus abc.tools.validate-design-bundle-test
```

Expected: the 0.2.0 example validates and every tamper test returns a stable
problem.

- [ ] **Step 5: Rebase read/validate/explain/report fields**

`read-valid-snapshot-index` remains the single file-loading boundary.
Validation/explanation/layout reports use `source_selection_hash`,
candidate/qualification refs, parser/mapping hashes, failure-set hash, and
snapshot identity. Remove request-set id/label, old snapshot label, tokenizer,
analysis, and manifest-index projections.

- [ ] **Step 6: Adapt staging at the locator seam**

Stage source, parser-IR, plaintext, and TEI references from their actual
locators. Read the work identity from each reference's `work_slug`; do not
parse a slug from a path. For loose output, place the unchanged manifest,
content, and every sidecar together under
`artifacts/<kind>/by-work/<work_slug>/` so existing relative `path_hint`
values remain true. For batched output, include that same closed file set under
the work member prefix. Keep byte copying and locator rewriting; do not
regenerate manifests. After rewriting locators, revalidate the staged closure and prove
`snapshot_identity_hash` is unchanged because locator is outside artifact
identity. The staged output records `source_snapshot_identity_hash`; it does
not mint an admitted release.

- [ ] **Step 7: Prove projections never render**

Add tests with `materialize-publication!` redefined to throw. Run validation,
explanation, layout report, and staging; all must succeed from a completed
fixture root without invoking the renderer.

Run:

```sh
cd abc
bin/kaocha --focus abc.tools.soranoha-layout-report-test
bin/kaocha --focus abc.tools.soranoha-stage-publication-test
bin/kaocha --focus abc.tools.soranoha-test
bin/kaocha
```

Expected: the full suite passes, all projections consume only v2, and staging
preserves the source identity.

- [ ] **Step 8: Regenerate both contract manifests and the Nix mirror**

Run from the monorepo root:

```sh
cd abc
python ../scripts/abc_schema_contracts.py --profile abc --write
cd ../ab-validator
python ../scripts/abc_schema_contracts.py --profile ab-validator --write
cd ..
just sync-schema-mirror
```

Expected: both manifests record snapshot-index 0.2.0 and the isolated Nix
schema copy is byte-identical to ABC.

- [ ] **Step 9: Commit the schema and all live consumers together**

```sh
git add abc/schemas/snapshot-index.schema.json \
  abc/src/abc/tools/snapshot_index.clj \
  abc/test/abc/tools/snapshot_index_test.clj \
  abc/examples/v0/snapshot/snapshot-index.json \
  abc/test/abc/tools/schema_test.clj \
  abc/src/abc/tools/validate_design_bundle.clj \
  abc/test/abc/tools/validate_design_bundle_test.clj \
  abc/src/abc/tools/soranoha.clj \
  abc/src/abc/tools/soranoha_layout_report.clj \
  abc/test/abc/tools/soranoha_layout_report_test.clj \
  abc/src/abc/tools/soranoha_stage_publication.clj \
  abc/test/abc/tools/soranoha_stage_publication_test.clj \
  abc/test/abc/tools/soranoha_test.clj \
  abc/schemas/schema-contracts.json \
  ab-validator/data/abc-schemas/nix-schemas/snapshot-index.schema.json \
  ab-validator/data/abc-schemas/schema-contracts.json
git commit -m "feat(snapshot-index)!: cut projections to publication identity v2"
```

---

### Task 9: Implement the Pure Release-Admissibility Predicate

**Files:**
- Create: `abc/src/abc/tools/publication_release.clj`
- Create: `abc/test/abc/tools/publication_release_test.clj`
- Modify: `abc/src/abc/tools/publication_policy.clj`
- Modify: `abc/test/abc/tools/materialize_publication_test.clj`
- Modify: `abc/src/abc/tools/soranoha.clj`
- Modify: `abc/test/abc/tools/soranoha_test.clj`

**Interfaces:**

```clojure
(publication-policy/release-problem rights-policy) ; => nil or problem
(publication-release/release-problems input) ; => vector-of-problem-maps
(publication-release/release-admissible? input)    ; => boolean
(publication-release/evaluate input)
; => {:admissible? boolean
;     :problems vector-of-problem-maps
;     :authority-hashes
;     {:decisions sha256 :registry sha256 :rights-policy sha256}}
```

- [ ] **Step 1: Write the complete predicate table**

Start with a valid in-memory index/closure and vary one fact per test:

- closure tamper;
- dirty/unprovable/fixture source;
- missing/wrong decision authority;
- absent or mismatched candidate/qualification;
- parser build/config/mapping/schema mismatch;
- nonempty failure set;
- rights policy blocked/malformed;
- authority hash absent or malformed.

Also prove the function is deterministic and writes no files.

- [ ] **Step 2: Make rights policy a value**

Keep `rights-publication-state` for loading. Add a pure problem function that
preserves the existing allowed state semantics. Remove renderer-facing tests
that treat `assert-release-allowed!` as per-work release authority.

- [ ] **Step 3: Implement one problem projection and two views**

Concatenate precomputed closure problems with pure source, parser, failure, and
rights comparisons. Sort/deduplicate by
`[:code :path :expected :actual]` in `release-problems`.
`release-admissible?` is exactly `(empty? (release-problems input))`.
`evaluate` calls `release-problems` once and packages that vector, the boolean,
and the supplied authority hashes. Never infer admissibility from an output
path or write a marker.

- [ ] **Step 4: Make retained validation recompute current admissibility**

Wire `validate-snapshot-root` and `explain-snapshot` to load the current
decision corpus, compatibility registry, and rights policy, authenticate the
candidate named by the index, compute closure problems, and call the shared
predicate. They print current admissibility plus authority hashes. They do not
trust or rewrite the build-time result in `publications-report.json`.

Run:

```sh
cd abc
bin/kaocha --focus abc.tools.publication-release-test
bin/kaocha --focus abc.tools.materialize-publication-test
bin/kaocha --focus abc.tools.soranoha-test
```

Expected: the valid table has no problems; every single-fault row is
inadmissible with its expected code.

- [ ] **Step 5: Commit**

```sh
git add abc/src/abc/tools/publication_release.clj \
  abc/test/abc/tools/publication_release_test.clj \
  abc/src/abc/tools/publication_policy.clj \
  abc/test/abc/tools/materialize_publication_test.clj \
  abc/src/abc/tools/soranoha.clj \
  abc/test/abc/tools/soranoha_test.clj
git commit -m "feat(publication): define recomputable release admissibility"
```

---

### Task 10: Make the Direct Build Assemble the Closed Release Value

**Files:**
- Modify: `abc/src/abc/tools/soranoha_build_publication.clj`
- Modify: `abc/test/abc/tools/soranoha_test.clj`
- Modify: `abc/test/abc/sim/content_sim_test.clj`
- Modify: `abc/src/abc/tools/manifest.clj`
- Modify: `abc/src/abc/tools/materialize_import.clj`
- Modify: `abc/test/abc/tools/materialize_import_test.clj`
- Modify: `abc/docs/adr/decisions.edn`

**Interfaces:** The build writes valid source/parser manifests, constructs
snapshot-index 0.2.0 from actual results, evaluates it, installs according to
strict/best-effort policy, and exits 0 only when admissible.

- [ ] **Step 1: Put reusable source/parser manifests with the manifest owner**

Move `materialize_import.clj`'s existing parser-IR manifest construction into
`abc.tools.manifest/parser-ir-artifact-manifest`, accepting explicit content,
source, parser, mapping, and generated-at values without assuming a CLI
directory layout. Add
`abc.tools.manifest/source-bundle-artifact-manifest` beside it. Preserve the
materialize-import CLI by adapting it to those functions; do not make the
release build depend on the import command namespace.

Tests must prove the direct build and import path produce the same manifest for
the same explicit value.

- [ ] **Step 2: Separate selection identity from derivation**

In `soranoha_build_publication.clj`, make source selection return the sorted
source identity rows before parser rendering. The selection phase may inspect
archives and materialize metadata records, but it does not invoke either
parser. Compute `source_selection_hash` from official
Git/catalog/archive/bundle/primary-text/metadata facts. Pass that hash as the
corpus snapshot hash used by source and publication manifests. Parser output
is not part of source selection; it enters through the parser-IR artifact
reference and parser coordinates.

- [ ] **Step 3: Collect all four manifest references**

For each successful work, write valid source and parser-IR manifests and
collect their references with the plaintext/TEI references returned by
`materialize-publication!`. Build failures with stable `stage`, `work_slug`,
and `code` fields. Include the path-free parser runtime identity object and
prove its hash equals `parser_config_hash`. Construct and write
`snapshot-index.json` only after all works finish.

- [ ] **Step 4: Evaluate before release-facing success**

Load the rights policy, decision/registry authority hashes, closure problems,
and parser authority. The predicate reads source trust and runtime parser
identity from the index itself. Call `publication-release/evaluate`. Put the
derived result in `publications/publications-report.json`; do not put the
result inside index identity.

State transitions:

- strict exception: delete temporary root, leave prior target untouched;
- best-effort partial or fixture/rights diagnostic: atomically install the
  diagnostic root only when configured, print `release_admissible: false`, exit
  1;
- no problems: atomically install, print `release_admissible: true`, exit 0.

- [ ] **Step 5: Replace old report booleans and assertions**

Delete `source-selection-report.json`'s locally derived
`release_admissible`. Tests read the shared evaluation from
`publications-report.json` and independently recompute it from the installed
root.

Run:

```sh
cd abc
bin/kaocha --focus abc.tools.materialize-import-test
bin/kaocha --focus abc.tools.soranoha-test
bin/kaocha --focus abc.sim.content-sim-test
bin/kaocha --focus abc.tools.snapshot-index-test
bin/kaocha
```

Expected: fixture builds install diagnostic index-v2 roots and exit 1, strict
failures preserve the prior root, a fully authorized in-memory policy test
exits 0, and the full ABC suite passes.

- [ ] **Step 6: Govern the changed release contract**

Amend `source-bundle-identity` claim c11 so it names the shared
release-admissibility predicate, strict no-replacement behavior, best-effort
diagnostic installation, and nonzero exit. Add an Accepted
`sole-publication-release-identity` decision whose claims bind:

- `build-publication` as sole release assembler/installer;
- snapshot-index 0.2.0 as the sole live publication identity;
- recomputable admissibility with no receipt;
- exact parser/source/manifest closure.

Use the final focused test files as evidence. Run:

```sh
cd abc
bin/kaocha --focus abc.tools.decisions-test
nix build .#checks.x86_64-linux.adr-governance --print-build-logs
```

Expected: the decision corpus is shape/semantically valid and all cited
evidence paths exist.

- [ ] **Step 7: Commit**

```sh
git add abc/src/abc/tools/soranoha_build_publication.clj \
  abc/test/abc/tools/soranoha_test.clj \
  abc/test/abc/sim/content_sim_test.clj \
  abc/src/abc/tools/manifest.clj \
  abc/src/abc/tools/materialize_import.clj \
  abc/test/abc/tools/materialize_import_test.clj \
  abc/docs/adr/decisions.edn
git commit -m "feat(publication): assemble one closed release value"
```

---

### Task 11: Pin the Real Root-Flake Wiring

**Files:**
- Create: `tests/publication-build-real-wiring-smoke.sh`
- Modify: `flake.nix`
- Modify: `tests/root-flake-output-contract-smoke.sh`

**Interface:** Root check
`checks.<system>.publication-build-real-wiring` invokes the same wrapped
Soranoha program exported as `apps.<system>.soranoha`.

- [ ] **Step 1: Write the smoke script**

The script accepts the wrapped Soranoha program and a writable work directory.
It:

1. copies the committed 000127 catalog fixture;
2. creates `index_pages/list_person_all_extended_utf8.zip`;
3. creates one `cards/000879/files/000001_ruby_fixture.zip`;
4. initializes Git, sets local author identity, adds/commits the fixture;
5. writes config 0.2.0 with `official-git`, `ab-aozora`, and the P5 candidate;
6. runs `build-publication`;
7. accepts exit 1 only because the committed rights policy is blocked;
8. asserts parser-IR, TEI, four manifest kinds, and snapshot-index 0.2.0 exist;
9. asserts the only admissibility problem is the rights policy.

The script must not set adapter/mapping environment variables and must not
invoke Clojure directly.

- [ ] **Step 2: Share one wrapper constructor in `flake.nix`**

Lift `mkAdapterAwareSoranohaApp` out of the `apps` local `let`. Use the same
derivation for the app and check; do not copy the export list. Add `git`,
`zip`, `jq`, and core shell tools to the check inputs.

- [ ] **Step 3: Pin root output presence**

Extend the root output smoke to require
`checks.<system>.publication-build-real-wiring`.

Run:

```sh
nixfmt flake.nix
just nix-format-check
bash tests/root-flake-output-contract-smoke.sh
system="$(nix eval --impure --raw --expr builtins.currentSystem)"
nix build ".#checks.${system}.publication-build-real-wiring" --print-build-logs
```

Expected: the check executes real `ab-aozora` and mapping wiring, produces a
diagnostic root, and fails release only on rights.

- [ ] **Step 4: Commit**

```sh
git add tests/publication-build-real-wiring-smoke.sh \
  tests/root-flake-output-contract-smoke.sh flake.nix
git commit -m "test(publication): pin real flake adapter wiring"
```

---

### Task 12: Remove Misleading Release and Duplicate Run Boundaries

**Files:**
- Modify: `abc/src/abc/tools/materialize_publication.clj`
- Modify: `abc/test/abc/tools/materialize_publication_test.clj`
- Modify: `abc/src/abc/tools/soranoha.clj`
- Modify: `abc/src/abc/tools/soranoha_build_publication.clj`
- Modify: `abc/test/abc/tools/soranoha_test.clj`
- Modify: `abc/src/abc/tools/workflow.clj`
- Modify: `abc/test/abc/tools/workflow_test.clj`

- [ ] **Step 1: Retire `materialize-release-publication!`**

Move the only rights decision to `publication-release/evaluate`. Route the
single-work CLI through `materialize-publication!` and label it a non-release
renderer in help. Preserve live CLI inputs and outputs.

- [ ] **Step 2: Thin the Soranoha dispatcher**

After Task 7's deletion, move no new domain logic into `soranoha.clj`. Keep
command declaration, option parsing, and delegation. Delete helpers made
unreferenced by the Task 7 cut. Make no new namespace split in this task; the
surviving projection wrappers remain delegations to their current domain
owners.

- [ ] **Step 3: Reassess the workflow runner with evidence**

Run:

```sh
rg -n 'run-workflow!|validate-run' abc/src abc/test abc/flake.nix abc/deps.edn
```

The expected live `run-workflow!` callers are direct publication build and
annotation join stats. Keep the runner and its domain tests. Make the current
private `summarize-run` and `step-record` constructors public as
`workflow/run-value` and `workflow/step-value`, with their existing map
contracts. Delete the materializer's duplicate `workflow-run-schema-id`,
`now-utc`, `batch-step-record`, and `batch-workflow-run`; its batch adapter
uses those two shared constructors and preserves its current summary and
`workflow-run.json` bytes.

Run:

```sh
cd abc
bin/kaocha --focus abc.tools.materialize-publication-test
bin/kaocha --focus abc.tools.soranoha-test
bin/kaocha --focus abc.tools.workflow-test
```

Expected: renderer adapters remain live, no per-work function claims release
authority, and workflow has exactly its independently owned callers.

- [ ] **Step 4: Commit**

```sh
git add abc/src/abc/tools/materialize_publication.clj \
  abc/test/abc/tools/materialize_publication_test.clj \
  abc/src/abc/tools/soranoha.clj \
  abc/src/abc/tools/soranoha_build_publication.clj \
  abc/test/abc/tools/soranoha_test.clj \
  abc/src/abc/tools/workflow.clj \
  abc/test/abc/tools/workflow_test.clj
git commit -m "refactor(publication): deepen surviving module boundaries"
```

---

### Task 13: Close the Measured Disposition

**Files:**
- Modify: `abc/docs/superpowers/reports/2026-07-24-publication-surface-disposition.md`

- [ ] **Step 1: Close the disposition report with measured deltas**

Record before/after source and test LOC for the retired family, command/alias
count, schema count, namespace dependency count, and standing check time. Mark
every row retained or retired; no `unknown` remains. Separate correctness code
added for trust/identity from source/test/apparatus deleted so the report does
not claim net LOC alone as architectural simplification.

Run:

```sh
cd abc
bin/kaocha --focus abc.tools.decisions-test
nix build .#checks.x86_64-linux.adr-governance --print-build-logs
cd ..
just schema-drift
```

Expected: decision shape/governance and both schema mirrors pass.

- [ ] **Step 2: Commit the measured closeout**

```sh
git add abc/docs/superpowers/reports/2026-07-24-publication-surface-disposition.md
git commit -m "docs(publication): close sole producer disposition"
```

---

### Task 14: Final Architecture and Migration Verification

**Files:**
- Modify only files required by failures found in this task.

- [ ] **Step 1: Prove the architectural invariants**

Run:

```sh
rg -n 'build-snapshot-index|snapshot-index!|materialize-snapshot-root!|reproduce!|publication-rehearsal!|validate-workflow' \
  abc/src abc/test abc/deps.edn abc/flake.nix abc/bin
rg -n 'write-snapshot-index|snapshot-index.json' abc/src
rg -n 'materialize-publication!' abc/src ab-validator
rg -n 'release-admissible|release_admissible|publication-release/evaluate' abc/src abc/test
rg -n 'request_set_id|request_set_label|tokenizer_profile_hashes|analysis_recipe_hashes' \
  abc/src/abc/tools/snapshot_index.clj abc/schemas/snapshot-index.schema.json \
  abc/examples/v0/snapshot/snapshot-index.json
```

Expected:

- the first and fourth old-field searches return no active matches;
- only `soranoha_build_publication.clj` writes the index;
- all canonical per-work rendering reaches `materialize-publication!`;
- one shared predicate owns admissibility and no marker/receipt exists.

- [ ] **Step 2: Run language and hygiene gates**

```sh
scripts/comment-hygiene-check.sh
just python-quality
just nix-format-check
nix build ./abc#checks.x86_64-linux.clj-kondo --print-build-logs
nix build ./abc#checks.x86_64-linux.clj-nix-focused-tests --print-build-logs
```

Expected: all pass; clj-kondo has zero errors.

- [ ] **Step 3: Run release-boundary and cross-repository gates**

```sh
system="$(nix eval --impure --raw --expr builtins.currentSystem)"
nix build ".#checks.${system}.publication-build-real-wiring" --print-build-logs
nix build "./abc#checks.${system}.parser-rq-p5-promotion-audit" --print-build-logs
nix build "./abc#checks.${system}.adr-governance" --print-build-logs
just schema-drift
```

Expected: real wiring, P5 promotion, governance, and schema mirror checks pass.

- [ ] **Step 4: Run the full migration gate**

```sh
just validate-migration
```

Expected: root, ABC, and ab-validator evaluation plus standing evidence gates
all pass.

- [ ] **Step 5: Review the diff and commit only verified repairs**

```sh
git status --short
git diff --check
git diff --stat "$(git merge-base HEAD main)"..HEAD
```

Confirm unrelated user changes remain untouched. If verification required
repairs, commit them with the narrowest matching conventional-commit message.
Do not merge until the branch contains the Lane 2 deletion, the Lane 3
index/projection cutover, and every command above is green.
