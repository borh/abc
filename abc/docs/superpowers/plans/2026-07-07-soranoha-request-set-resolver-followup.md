# Soranoha Request-Set Resolver Followup Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Replace request-set shape fixtures as the runtime source of truth with a resolver that produces canonical ADR 0026 request-set values from named request-set definitions, then use those request sets to generate snapshot indexes through the Soranoha CLI.

**Architecture:** Keep ADR 0026 request-set hashing in `abc.tools.analysis-identity`. Add a narrow resolver layer that reads label definitions, resolves semantic recipe labels to content hashes and registry-entry hashes, computes the canonical `request_set_identity_object`, and emits resolved request-set JSON. Keep checked-in resolved request sets as golden outputs for tests and CLI inspection. Snapshot indexes are generated from request-set identity plus manifest references in a snapshot plan; checked-in snapshot JSON is generated output, not the command's runtime source of truth.

**Tech Stack:** Clojure 1.12, deterministic JSON helpers, RFC 8785 JCS hashing through existing `abc.tools.hash`, JSON Schema draft 2020-12, Kaocha/Nix checks.

## Global Constraints

- Request-set labels are lookup handles, not identity.
- `request_set_id = sha256(RFC8785-JCS(request_set_identity_object))`.
- Only recipe/profile content hashes participate in request-set identity.
- Semantic recipe labels resolve to content hash and registry-entry hash for audit.
- `request_set_id`, generated time, operator, local paths, run id, resolved labels, and batch policy stay outside `request_set_identity_object`.
- Tokenizer profile arrays remain empty for the current text-only snapshot slice.
- Full-corpus scope definitions use source-snapshot subject sources; generated source snapshots are built from materialized ABC work directories, not inline subject shape fixtures.

---

### Task 1: Request-Set Schema and Definitions

**Files:**
- Create: `abc/schemas/request-set.schema.json`
- Create: `abc/data/request-set-definitions/smoke-basic-ja.json`
- Create: `abc/data/request-set-definitions/demo-basic-ja.json`
- Create: `abc/data/request-set-definitions/full-corpus-publication-basic-ja.json`
- Create: `abc/data/request-set-definitions/full-corpus-analysis-basic-ja.json`
- Create: `abc/data/request-set-definitions/full-corpus-basic-ja.json`
- Modify: `abc/tools/schema_contracts.py`
- Modify: `scripts/abc_schema_contracts.py`
- Modify: `ab-validator/data/abc-schemas/schema-contracts.json`
- Create: `ab-validator/data/abc-schemas/nix-schemas/request-set.schema.json`

**Interfaces:**
- Consumes: existing resolved request-set JSON field shapes.
- Produces: source definitions consumed by `abc.tools.request-set-resolver`.

- [x] **Step 1: Add failing schema/definition test**

Add tests in `test/abc/tools/request_set_resolver_test.clj` that expect the resolver to read definitions from `data/request-set-definitions` and to stamp the live `schemas/request-set.schema.json` hash into `request_set_identity_object.schema_hash`.

- [x] **Step 2: Run focused test to verify RED**

Run:

```bash
cd abc
clojure -M:test -e "(require 'abc.tools.request-set-resolver-test 'clojure.test) (let [r (clojure.test/run-tests 'abc.tools.request-set-resolver-test)] (System/exit (if (and (zero? (:fail r)) (zero? (:error r))) 0 1)))"
```

Expected: failure because `abc.tools.request-set-resolver` does not exist.

- [x] **Step 3: Add schema and definition files**

Create `schemas/request-set.schema.json` for resolved request sets and definition JSON files that contain semantic recipe labels, subjects, input views, missing policy, and pack policy.

- [x] **Step 4: Sync schema contracts**

Regenerate ABC and ab-validator schema contracts, and copy `request-set.schema.json` to the isolated Nix schema mirror.

### Task 2: Resolver Implementation

**Files:**
- Create: `abc/src/abc/tools/request_set_resolver.clj`
- Test: `abc/test/abc/tools/request_set_resolver_test.clj`
- Modify: `abc/data/request-sets/*.json`

**Interfaces:**
- Produces: `request-set-labels`, `request-set-definition-path`, `read-request-set-definition`, `resolve-request-set`, `write-resolved-request-set!`.
- Consumes: `abc.tools.analysis-identity/request-set-identity-object`, `abc.tools.analysis-identity/request-set-id`, `abc.tools.analysis-identity/analysis-recipe-hash`, `abc.tools.analysis-identity/resolved-recipe-label`, and `abc.tools.manifest/schema-hash`.

- [x] **Step 1: Add failing resolver behavior tests**

Test that resolving `smoke-basic-ja` computes the live request-set schema hash, resolves `literary-basic-ja-v1` to its content hash, records a registry-entry hash outside identity, computes `request_set_id`, and does not carry `fixture_role`.

- [x] **Step 2: Run focused test to verify RED**

Run the focused test command from Task 1.

Expected: failure on missing resolver functions.

- [x] **Step 3: Implement minimal resolver**

Implement the resolver using deterministic hashes and existing analysis identity helpers.

- [x] **Step 4: Update golden resolved request sets**

Write resolved outputs to `data/request-sets/*.json` and keep tests checking golden equality.

- [x] **Step 5: Run focused tests to verify GREEN**

Run the focused request-set resolver, fixture, analysis-identity, Soranoha, and snapshot-index tests.

### Task 3: Soranoha CLI Uses Resolver

**Files:**
- Modify: `abc/src/abc/tools/soranoha.clj`
- Modify: `abc/test/abc/tools/soranoha_test.clj`

**Interfaces:**
- Consumes: `abc.tools.request-set-resolver/resolve-request-set`.
- Produces: `list-request-sets` and `explain-request-set` based on definitions rather than resolved fixtures.

- [x] **Step 1: Add failing CLI test**

Test that `explain-request-set smoke-basic-ja` prints a resolver-derived request-set id and no `fixture_role` line.

- [x] **Step 2: Run Soranoha focused test to verify RED**

Run:

```bash
cd abc
clojure -M:test -e "(require 'abc.tools.soranoha-test 'clojure.test) (let [r (clojure.test/run-tests 'abc.tools.soranoha-test)] (System/exit (if (and (zero? (:fail r)) (zero? (:error r))) 0 1)))"
```

Expected: failure because the CLI still reads `data/request-sets`.

- [x] **Step 3: Wire CLI to resolver**

Replace direct fixture reads in `abc.tools.soranoha` with resolver reads.

- [x] **Step 4: Run focused CLI tests to verify GREEN**

Run the Soranoha focused test command above.

### Task 4: Generated Snapshot Indexes and CLI

**Files:**
- Create: `abc/data/snapshot-plans/smoke-basic-ja.json`
- Modify: `abc/src/abc/tools/snapshot_index.clj`
- Modify: `abc/src/abc/tools/soranoha.clj`
- Modify: `abc/test/abc/tools/snapshot_index_test.clj`
- Modify: `abc/test/abc/tools/soranoha_test.clj`
- Modify: `abc/examples/v0/snapshot/snapshot-index.json`

**Interfaces:**
- Produces: manifest-to-artifact-reference conversion, generated snapshot-index builder, plan-driven snapshot generation, `snapshot-index`, `reproduce`, `validate`, and `explain-snapshot` Soranoha commands.
- Consumes: resolved request sets, snapshot plans, manifest files, live schema hashes, failure/layout policy values.

- [x] **Step 1: Add failing builder tests**

Test that manifest files become artifact references with manifest content hashes and that a snapshot index can be built from a resolved request set plus manifest references.

- [x] **Step 2: Implement snapshot-index builder**

Add manifest-reference conversion, summary calculation, request-set-derived identity fields, plan loading, and deterministic index writing.

- [x] **Step 3: Add failing Soranoha CLI tests**

Test plan-driven `snapshot-index`, `reproduce`, `validate`, and `explain-snapshot`.

- [x] **Step 4: Wire Soranoha commands**

Make the CLI generate snapshot indexes from `data/snapshot-plans/<label>.json`; keep checked-in snapshot JSON as generated fixture output.

- [x] **Step 5: Align and regenerate smoke artifacts**

Align `smoke-basic-ja` with the example-work `work_content_hash`, regenerate `data/request-sets/smoke-basic-ja.json`, and regenerate `examples/v0/snapshot/snapshot-index.json`.

- [x] **Step 6: Run smoke reproduce/validate/explain**

Run `soranoha reproduce smoke-basic-ja`, `soranoha validate target/soranoha/smoke-basic-ja`, and `soranoha explain-snapshot target/soranoha/smoke-basic-ja/snapshot-index.json`.

Note: this task first introduced the metadata/index path. Task 6 below replaces the `reproduce` path with a materialized smoke root.

### Task 5: Verification

**Files:**
- All touched files.

**Interfaces:**
- Consumes: completed resolver and CLI changes.
- Produces: verified request-set and snapshot-index followup slice.

- [x] Run focused resolver/snapshot/Soranoha tests.
- [x] Run `bash scripts/monorepo-schema-drift.sh`.
- [x] Run `nix build .#checks.x86_64-linux.abc-adr-acceptance-criteria -L`.
- [x] Run `git diff --check`.
- [x] Run `nix flake check`.

### Task 6: End-to-End Smoke Snapshot Root

**Files:**
- Modify: `abc/data/snapshot-plans/smoke-basic-ja.json`
- Modify: `abc/src/abc/tools/soranoha.clj`
- Modify: `abc/test/abc/tools/soranoha_test.clj`

**Interfaces:**
- Produces: `soranoha reproduce smoke-basic-ja` materializes parser-IR, plaintext, TEI, and token-independent analysis outputs under `target/soranoha/smoke-basic-ja`, then builds the snapshot index from the generated manifest files.
- Consumes: existing parser-IR, publication, analysis, request-set, and snapshot-index helpers.

- [x] **Step 1: Add failing reproduce-root test**

Test that `reproduce smoke-basic-ja` writes parser-IR, plaintext, TEI, and analysis artifacts plus manifests, and that the generated snapshot index references those generated manifests.

- [x] **Step 2: Implement plan-driven smoke materialization**

Add a `materialization` section to the smoke snapshot plan and wire `reproduce` to create the smoke root before writing `snapshot-index.json`.

- [x] **Step 3: Add root-level validation**

Make `validate <snapshot-root>` verify loose manifest locators exist and match the manifest hashes recorded in the snapshot index.

- [x] **Step 4: Run smoke reproduce/validate/explain**

Run `soranoha reproduce smoke-basic-ja`, `soranoha validate target/soranoha/smoke-basic-ja`, and `soranoha explain-snapshot target/soranoha/smoke-basic-ja/snapshot-index.json`.

### Task 7: Smoke-Root Slice Verification

**Files:**
- All touched files.

**Interfaces:**
- Consumes: completed smoke-root materialization and validation changes.
- Produces: verified end-to-end smoke snapshot slice.

- [x] Run focused materialize-analysis/materialize-publication/snapshot/Soranoha tests.
- [x] Run `git diff --check`.
- [x] Run `nix flake check`.

### Task 8: End-to-End Demo Snapshot Root

**Files:**
- Create: `abc/data/snapshot-plans/demo-basic-ja.json`
- Modify: `abc/src/abc/tools/soranoha.clj`
- Modify: `abc/test/abc/tools/soranoha_test.clj`

**Interfaces:**
- Produces: `soranoha reproduce demo-basic-ja` materializes two request-set subjects under work-scoped artifact directories, then builds and validates a snapshot index with parser-IR, plaintext, TEI, and token-independent analysis artifacts for each subject.
- Consumes: the existing `demo-basic-ja` request-set definition and the smoke-root materialization helpers.

- [x] **Step 1: Add failing demo reproduce-root test**

Test that `reproduce demo-basic-ja` writes two work-scoped artifact roots, generates eight manifest references, preserves the two demo work content hashes, and validates the generated snapshot root.

- [x] **Step 2: Implement plural materializations**

Allow snapshot plans to define either one `materialization` or multiple `materializations`. Keep the existing smoke output layout unchanged, and write multi-work artifacts under `artifacts/works/<artifact_subdir>/`.

- [x] **Step 3: Add a demo snapshot plan**

Add `data/snapshot-plans/demo-basic-ja.json` with two work entries bound to the existing demo request-set subjects.

- [x] **Step 4: Run demo reproduce/validate/explain**

Run `soranoha reproduce demo-basic-ja`, `soranoha validate target/soranoha/demo-basic-ja`, and `soranoha explain-snapshot target/soranoha/demo-basic-ja/snapshot-index.json`.

### Task 9: Demo-Root Slice Verification

**Files:**
- All touched files.

**Interfaces:**
- Consumes: completed demo-root materialization and validation changes.
- Produces: verified two-work demo snapshot slice.

- [x] Run focused materialize-analysis/materialize-publication/snapshot/Soranoha tests.
- [x] Run `git diff --check`.
- [x] Run `nix flake check`.

### Task 10: Source-Snapshot Backed Full-Corpus Subjects

**Files:**
- Create: `abc/examples/v0/source-snapshot/source-snapshot.fixture.json`
- Modify: `abc/src/abc/tools/request_set_resolver.clj`
- Modify: `abc/test/abc/tools/request_set_resolver_test.clj`
- Modify: `abc/data/request-set-definitions/full-corpus-*.json`
- Modify: `abc/data/request-sets/full-corpus-*.json`

**Interfaces:**
- Produces: request-set `subject_source` expansion from a source-corpus snapshot descriptor.
- Consumes: source snapshot `snapshot_hash` and `snapshot_identity_object.snapshot_inputs`.

- [x] Add failing resolver tests for source-snapshot subject expansion and stale snapshot hash rejection.
- [x] Implement source-snapshot subject expansion while preserving inline smoke/demo subjects.
- [x] Switch full-corpus definitions from inline shape subjects to `subject_source`.
- [x] Regenerate full-corpus resolved request-set goldens.
- [x] Run focused request-set/Soranoha/design-bundle tests.

### Task 11: Full-Corpus Subject-Source Slice Verification

**Files:**
- All touched files.

**Interfaces:**
- Consumes: completed source-snapshot subject-source resolver changes.
- Produces: verified request-set identity path for source-snapshot backed full-corpus definitions.

- [x] Run `git diff --check`.
- [x] Run `nix build .#checks.x86_64-linux.abc-clj-nix-focused-tests -L`.
- [x] Run `nix flake check`.

### Task 12: Source-Snapshot Workset Generator

**Files:**
- Create: `abc/src/abc/tools/source_snapshot_workset.clj`
- Create: `abc/test/abc/tools/source_snapshot_workset_test.clj`
- Modify: `abc/src/abc/tools/materialize_source_snapshot.clj`
- Modify: `abc/deps.edn`

**Interfaces:**
- Produces: `source-snapshot-workset` CLI and `workset-from-root` / `write-workset!` helpers.
- Consumes: materialized ABC work directories containing `aat.json`, `parser-ir.json`, and `metadata-record.json`.

- [x] Add failing tests for deterministic source-snapshot workset generation.
- [x] Generate relative workset paths from materialized work directories.
- [x] Resolve relative workset paths inside `materialize-source-snapshot` so generated descriptors are portable across working directories.
- [x] Add a CLI alias: `clojure -M:abc/source-snapshot-workset`.
- [x] Add an integration test showing generated worksets feed `materialize-source-snapshot`.

### Task 13: Source-Snapshot Workset Slice Verification

**Files:**
- All touched files.

**Interfaces:**
- Consumes: completed source-snapshot workset generator.
- Produces: verified bridge from materialized corpus outputs to source-snapshot descriptors.

- [x] Run focused source-snapshot workset and materializer tests.
- [x] Run `git diff --check`.
- [x] Run `nix build .#checks.x86_64-linux.abc-clj-nix-focused-tests -L`.
- [x] Run `nix flake check`.

### Task 14: Soranoha Source-Snapshot Command

**Files:**
- Modify: `abc/src/abc/tools/soranoha.clj`
- Modify: `abc/test/abc/tools/soranoha_test.clj`

**Interfaces:**
- Produces: `soranoha source-snapshot <materialized-root> <output-root> <snapshot-scope> <snapshot-date>`.
- Consumes: `abc.tools.source-snapshot-workset/write-workset!` and `abc.tools.materialize-source-snapshot/materialize-source-snapshot!`.

- [x] Add a failing CLI test for generating a source-snapshot workset, descriptor, and per-work source manifest.
- [x] Implement the Soranoha wrapper command.
- [x] Print source-snapshot workset path, descriptor path, source snapshot hash, and work count.
- [x] Run focused Soranoha tests.

### Task 15: Soranoha Source-Snapshot Command Verification

**Files:**
- All touched files.

**Interfaces:**
- Consumes: completed Soranoha source-snapshot command.
- Produces: verified public CLI bridge from materialized corpus outputs to source-snapshot descriptors.

- [x] Run focused Soranoha and source-snapshot tests.
- [x] Run `git diff --check`.
- [x] Run `nix build .#checks.x86_64-linux.abc-clj-nix-focused-tests -L`.
- [x] Run `nix flake check`.

### Task 16: Source-Snapshot Backed Request-Set Resolution Command

**Files:**
- Modify: `abc/src/abc/tools/request_set_resolver.clj`
- Modify: `abc/src/abc/tools/soranoha.clj`
- Modify: `abc/test/abc/tools/request_set_resolver_test.clj`
- Modify: `abc/test/abc/tools/soranoha_test.clj`

**Interfaces:**
- Produces: `soranoha resolve-request-set <label> <output-path> <source-snapshot-path>`.
- Consumes: source snapshots produced by `soranoha source-snapshot`.

- [x] Add failing resolver test for resolving `full-corpus-basic-ja` against a supplied source snapshot descriptor.
- [x] Add failing Soranoha CLI test that generates a source snapshot, resolves a full-corpus request set from it, and writes canonical JSON.
- [x] Implement `:subject-source-path` resolver override for definitions that already declare `subject_source`.
- [x] Implement the `resolve-request-set` Soranoha command and print output path, request-set id, and subject count.
- [x] Run focused request-set resolver and Soranoha tests.

### Task 17: Source-Snapshot Backed Request-Set Resolution Verification

**Files:**
- All touched files.

**Interfaces:**
- Consumes: completed request-set resolution command.
- Produces: verified bridge from generated source snapshot descriptors to canonical request-set JSON.

- [x] Run focused request-set/Soranoha/source-snapshot/snapshot tests.
- [x] Run `git diff --check`.
- [x] Run `nix build .#checks.x86_64-linux.abc-clj-nix-focused-tests -L`.
- [x] Run `nix flake check`.

### Task 18: Resolved Request-Set JSON Inputs

**Files:**
- Modify: `abc/src/abc/tools/soranoha.clj`
- Modify: `abc/test/abc/tools/soranoha_test.clj`

**Interfaces:**
- Produces: `soranoha snapshot-index <label-or-request-set-json> <output-path>`.
- Produces: `soranoha reproduce <label-or-request-set-json>`.
- Produces: `soranoha explain-request-set <label-or-request-set-json>`.
- Consumes: canonical request-set JSON produced by `soranoha resolve-request-set`.

- [x] Add failing Soranoha tests showing `snapshot-index` and `reproduce` must accept a resolved request-set JSON file, not only a checked-in label.
- [x] Implement one request-set reference boundary: labels resolve through `abc.tools.request-set-resolver`; JSON files are read and identity-checked before use.
- [x] Use the embedded request-set label from JSON files to select snapshot plans and default output roots.
- [x] Run focused Soranoha tests.

### Task 19: Resolved Request-Set JSON Input Verification

**Files:**
- All touched files.

**Interfaces:**
- Consumes: completed request-set JSON input support.
- Produces: verified bridge from generated request-set JSON into downstream snapshot-index and reproduce commands.

- [x] Run focused request-set/Soranoha/snapshot tests.
- [x] Run `git diff --check`.
- [x] Run `nix build .#checks.x86_64-linux.abc-clj-nix-focused-tests -L`.
- [x] Run `nix flake check`.
