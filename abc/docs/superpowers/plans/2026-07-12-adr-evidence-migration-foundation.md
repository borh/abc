# ADR Evidence Migration: Foundation, Runtime, and Identity Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Establish the shared claim-migration ledger and hermetic design-bundle boundary, then migrate every foundation/runtime/identity Accepted criterion to honest typed, artifact-backed evidence while remaining in audit mode.

**Architecture:** A content-hashed baseline and reviewed EDN ledger preserve every pre-review criterion while ADR Markdown is corrected and assigned stable claim IDs. Focused fixture/structural bundles stay separate from the shared operational Nix-app bundle; the design-bundle app becomes store-hermetic and Git-cliff configuration moves to its own synthetic-repository Nix check. Family Stage A corrects claims and lifecycle headers; Stage B captures only from the clean Stage A commit and joins immutable bundles through `docs/adr/adr-evidence.edn`.

**Tech Stack:** Clojure 1.12, Kaocha/clojure.test/test.check, deterministic JSON/JCS SHA-256, EDN, Nix flakes, Bash, git-cliff, JSON Schema.

## Global Constraints

- Work only in the isolated migration worktree; preserve unrelated user changes.
- Remain in ADR governance `audit` mode. Only plan 6 may promote ADR 0034 or switch the Nix gate to enforcement.
- The immutable baseline contains all 146 pre-review criteria and their exact UTF-8 text hashes; never regenerate it after criterion editing.
- Criterion correction precedes stable claim-ID assignment. Once authored, claim IDs are stable.
- No passing process may support a statement that its command/test does not assert.
- Command behavior uses `:operational-behavior` / `:operational-observation`; helper tests do not impersonate the Nix app, wrapper, or CI.
- Checked-in capture descriptors are explicit inputs to their own bundles. Every dynamic schema, fixture, registry, report, and config input is explicit.
- Capture only from a clean Stage A commit. A failed capture creates no registry entry.
- Historical evidence retains exact historical coordinates; future guards move to `## Future Verification` and acquire no evidence obligation.
- The baseline's 36 explicitly inventoried normative-section hashes are immutable: 26 exact `## Decision` sections, ADR 0014's exact `## Decision Matrix`, eight exact `## Hard Rule` sections, and ADR 0016's exact `## Hard Rule (carried forward from ADR 0015)`. “Move” instructions relocate only duplicated Acceptance Criteria or Implementation Status prose; never cut, rewrite, or append to an original normative section.
- Publication remains blocked by `data/publication-policy.edn`; release authority records decision scope, not current releasability.
- Generated inventory and governance reports change in the same commit as their source ledger/registry changes.
- Use `apply_patch` for hand edits, `nixfmt` for Nix formatting, and the root `justfile` as the final development entry point.

---

## File and interface map

- Create `schemas/adr-claim-migration-baseline.schema.json`: closed JSON
  contract for the immutable 146-row criterion baseline plus the 36 exact,
  explicitly coordinated normative-section hashes for the original 26
  Accepted ADRs.
- Create `src/abc/tools/adr_claim_migration.clj`: baseline hashing/loading and migration-ledger validation.
  - `(criterion-text-hash body) -> "sha256:<64 lowercase hex>"`, hashing the exact parsed criterion body UTF-8 bytes.
  - `(baseline-value revision adrs) -> deterministic JSON value`.
  - `(baseline-hash baseline) -> formatted JCS SHA-256` over the whole baseline value.
  - `(validate-baseline baseline) -> vector of problem maps`.
  - `(normative-section-problems baseline current-adrs) -> vector of problem
    maps`; existing Accepted normative sections are immutable, while new ADRs
    absent from the baseline are outside this comparison.
  - `(validate-ledger baseline ledger claims-by-id {:keys [require-complete?]}) -> vector of problem maps`.
  - `(load-migration-state repo-root {:keys [require-complete?]}) -> {:baseline ... :ledger ... :by-key ... :problems [...]}`.
- Create `docs/adr/adr-claim-migration-baseline.json`: immutable pre-edit snapshot whose normative inventory is keyed by exact `[ADR, heading]` coordinates rather than heading-prefix inference.
- Create/extend `docs/adr/adr-claim-migration.edn`: source ledger keyed by `[adr-number criterion-text-hash]`.
- Modify `src/abc/tools/adr_evidence_inventory.clj`: `(inventory-value adrs migration-state)` joins dispositions/resulting IDs by baseline key; one-arity use is removed so callers cannot silently omit migration state.
- Create `src/abc/tools/adr_evidence_register.clj`: materialize reviewed entry templates by deriving each bundle's canonical JCS hash, validate the resulting partial registry, and replace only the template-owned claim entries atomically.
  - `(materialize-template repo-root template) -> {:entries [...] :problems [...]}`.
  - `(candidate-registry current entries) -> deterministic registry value`, replacing every current entry whose claim ID occurs in `entries` and sorting by `[claim-id artifact-path observation-key]`.
  - `(register! {:keys [repo-root template-path registry-path]}) -> {:registry ... :problems [...]}`; write only when every problem other than unrelated `:missing-claim-evidence` is absent.
- Create checked-in entry templates under `docs/evidence/adr-entries/`; templates never contain `:artifact-hash`, inline observations, inputs, or verdicts.
- Create `nix/adr-family-clean.jq`: the one family-closure predicate, matching
  `.file`, `.['claim-id']`, and every `.['affected-claim-ids'][]` value.
- Extend capture with explicit `--repo-root` plus
  `component-clojure-test-v1`; this is the sole authorized way for an ABC
  evidence bundle to bind monorepo-relative `abc/...` and `ab-validator/...`
  inputs while retaining real-path containment.
- Create `src/abc/tools/evidence_io.clj` and
  `src/abc/tools/adr_evidence_runtime_inputs.clj`: the common traceable
  repository-read boundary and checked runtime-input-manifest validator used
  by every focused Clojure evidence boundary. Static namespace traversal owns
  source closure only; it never certifies schemas, fixtures, registries,
  reports, or configuration read at runtime.
- Create sorted family manifests under `docs/evidence/adr-inputs/`.
  Descriptor tests require exact equality between each boundary's manifest
  paths and its declared runtime-data inputs, separately from descriptor
  self-binding and derived namespace inputs.
- Modify `src/abc/tools/adr.clj`: emit `:claim-header-outside-acceptance` when either exact non-acceptance section contains a typed claim header.
- Create `nix/check-git-cliff-config.sh`: initialize a temporary repository, commit `feat: fixture`, and run git-cliff against the checked-in config.
- Modify `flake.nix`: remove Git-cliff from the design-bundle app and add `checks.<system>.git-cliff-config`.
- Create `test/abc/tools/foundation_evidence_test.clj`: direct schema/fixture assertions and wrapper/CI wiring assertions that are not honestly covered today.
- Modify `src/abc/tools/validate_design_bundle.clj` to expose
  `(evidence-input-paths) -> sorted-vector` containing every repository file
  dynamically read by the supported design-bundle validation path; descriptor
  tests require exact equality with this vector plus descriptor/flake wiring.
- Create checked-in descriptors under `docs/evidence/adr-capture/` and Stage B bundles under `docs/evidence/adr-runs/`.
- Shared cross-plan operational boundary owned here:
  `docs/evidence/adr-capture/design-bundle-operational.edn` ->
  `docs/evidence/adr-runs/design-bundle-operational.json`, observation
  `design-bundle-exits-zero`. Later plans may join it but must not recapture it.

### Task 1: Baseline and migration-ledger contracts

**Files:**
- Create: `abc/schemas/adr-claim-migration-baseline.schema.json`
- Create: `abc/src/abc/tools/adr_claim_migration.clj`
- Create: `abc/test/abc/tools/adr_claim_migration_test.clj`
- Modify: `abc/src/abc/tools/validate_design_bundle.clj`
- Modify: `abc/test/abc/tools/validate_design_bundle_test.clj`

**Interfaces:**
- Consumes: `abc.tools.hash/sha256-string`, `sha256-json-jcs`, `format-sha256`; parsed ADR maps from `abc.tools.adr/parse-all`.
- Produces: the six `abc.tools.adr-claim-migration` functions in the file map. Later plans must call `load-migration-state`; they must not parse/hash ledger keys independently.

- [ ] **Step 1: Write failing hash/baseline tests**

Add tests asserting exact-body sensitivity, deterministic ADR/index ordering,
146-row count, schema rejection of extra keys, duplicate `[adr,text_hash]`,
text/hash disagreement, and baseline revision preservation. Also assert all
36 explicit normative-coordinate byte sensitivities, stable section ordering, and rejection
when an original Accepted ADR's normative section is added, removed, or
changed; Implementation Status, relation headers, Acceptance Criteria,
Historical Evidence, and Future Verification edits do not affect that guard.
The inline fixture includes distinct exact `Decision`, `Decision Matrix`,
`Hard Rule`, and `Hard Rule (carried forward from ADR 0015)` headings and
proves no prefix-based discovery is used.
Use an inline two-ADR fixture and assert the baseline row shape exactly:

```clojure
{"adr" 1 "file" "0001-one.md" "original_criterion_index" 0
 "original_text" "First."
 "original_text_hash" (migration/criterion-text-hash "First.")}
```

- [ ] **Step 2: Run the focused test and verify RED**

Run: `cd abc && bin/kaocha --focus abc.tools.adr-claim-migration-test`

Expected: FAIL because `abc.tools.adr-claim-migration` and the schema do not exist.

- [ ] **Step 3: Implement the closed baseline value and hash validation**

The baseline JSON top-level keys are exactly:

```json
{
  "schema_version": "abc-adr-claim-migration-baseline-v1",
  "baseline_revision": "<40-hex commit>",
  "accepted_adr_count": 26,
  "criterion_count": 146,
  "criteria": [],
  "normative_section_count": 36,
  "normative_sections": []
}
```

Sort rows by `[adr, original_criterion_index]`. Validate formatted hashes, exact recomputation from `original_text`, unique `[adr, original_text_hash]`, and count fields. Do not hash normalized Markdown or trim the parsed body.
Each normative row has exactly `adr`, `file`, `section`, `original_text`, and
`original_text_hash`. Generate the inventory from an explicit checked
coordinate table containing all 26 `[ADR, "Decision"]` coordinates, `[0014,
"Decision Matrix"]`, the eight exact `[ADR, "Hard Rule"]` coordinates, and
`[0016, "Hard Rule (carried forward from ADR 0015)"]`. Do not discover
normative sections with `starts-with?`, a regex prefix, or a global heading
name set. Fail when a coordinate is missing or when one of these 36 exact
coordinates is duplicated. Hash exact parsed section body UTF-8 bytes and
sort by `[adr, section]`.

- [ ] **Step 4: Write failing ledger-validation tests**

Use this exact ledger contract:

```clojure
{:schema-version "abc-adr-claim-migration-v1"
 :baseline-revision "0123456789012345678901234567890123456789"
 :baseline-manifest-hash "sha256:..."
 :entries
 {[1 "sha256:..."]
  {:disposition :correct
   :rationale "Narrow to the implemented artifact-ID oracle."
   :planned-evidence-boundaries [:manifest-index-oracle]
   :resulting-claim-ids ["ADR-0001-C4"]}}}
```

Assert rejection of an unknown/missing baseline key (when `require-complete?` is true), invalid disposition, missing non-retain rationale, empty planned boundaries on retained/corrected rows, duplicate resulting claim ID, absent current claim ID, baseline revision/hash mismatch, and a moved-out entry with nonempty resulting IDs. With `require-complete? false`, unreviewed baseline rows remain explicit inventory debt but already-present entries receive full validation; this is the only migration-time relaxation. Plan 5 first invokes complete mode after all 146 dispositions exist, and plan 6 revalidates complete mode before switching enforcement.

- [ ] **Step 5: Run ledger tests and verify RED**

Run: `cd abc && bin/kaocha --focus abc.tools.adr-claim-migration-test`

Expected: FAIL on missing `validate-ledger`/`load-migration-state` behavior.

- [ ] **Step 6: Implement ledger loading and validation**

Problem kinds must be stable keywords: `:invalid-migration-baseline`,
`:baseline-hash-mismatch`, `:baseline-revision-mismatch`,
`:unknown-baseline-key`, `:unresolved-baseline-key`, `:invalid-disposition`,
`:missing-disposition-rationale`, `:missing-evidence-boundary`,
`:duplicate-resulting-claim-id`, `:missing-resulting-claim-id`, and
`:accepted-normative-section-drift`. `load-migration-state` always runs the
normative comparison; `require-complete? false` relaxes missing ledger rows,
not historical mutation of any of the 36 inventoried normative coordinates.

- [ ] **Step 7: Include the new JSON schema in design-bundle schema validation**

Extend `validate-json-schemas!` and its focused schema-contract test so an extra property and wrong `schema_version` fail.

- [ ] **Step 8: Run GREEN verification**

Run: `cd abc && bin/kaocha --focus abc.tools.adr-claim-migration-test --focus abc.tools.validate-design-bundle-test`

Expected: PASS; no repository files are generated by the tests.

- [ ] **Step 9: Commit the contract**

```bash
git add abc/schemas/adr-claim-migration-baseline.schema.json \
  abc/src/abc/tools/adr_claim_migration.clj \
  abc/test/abc/tools/adr_claim_migration_test.clj \
  abc/src/abc/tools/validate_design_bundle.clj \
  abc/test/abc/tools/validate_design_bundle_test.clj
git commit -m "feat(adr): add claim migration ledger contract"
```

### Task 2: Immutable baseline, reviewed foundation ledger, and disposition inventory

**Files:**
- Create: `abc/docs/adr/adr-claim-migration-baseline.json`
- Create: `abc/docs/adr/adr-claim-migration.edn`
- Modify: `abc/src/abc/tools/adr_evidence_inventory.clj`
- Modify: `abc/test/abc/tools/adr_evidence_inventory_test.clj`
- Modify: `abc/deps.edn`
- Modify: `abc/docs/reports/adr-claim-migration-inventory.json`

**Interfaces:**
- Consumes: Task 1 migration state.
- Produces: inventory rows with `baseline_key`, `disposition`, `disposition_rationale`, `planned_evidence_boundaries`, and `resulting_claim_ids`; later family plans update their ledger rows and call the same generator.

- [ ] **Step 1: Write failing inventory join tests**

Assert that a retained row joins by exact text hash even if its descriptive index changes, a corrected baseline row maps to two live claim IDs, a moved-out row remains in a `baseline_criteria` array but not the live `criteria` array, and reviewed rows never emit null dispositions. Assert byte-identical generation twice.

- [ ] **Step 2: Run RED**

Run: `cd abc && bin/kaocha --focus abc.tools.adr-evidence-inventory-test`

Expected: FAIL because `inventory-value` does not accept/join migration state.

- [ ] **Step 3: Implement the migration-aware inventory**

Change the public signature to:

```clojure
(defn inventory-value [adrs migration-state] ...)
(defn write-inventory! [output adrs migration-state] ...)
```

The CLI loads `docs/adr/adr-claim-migration-baseline.json` and
`docs/adr/adr-claim-migration.edn`, exits 1 on migration-state problems, and
still exits 1 for an unclassified live family.

- [ ] **Step 4: Generate the baseline exactly once from pre-edit HEAD**

Add alias `:abc/adr-claim-migration` and a `--write-baseline PATH --revision REV`
mode. Run:

```bash
cd abc
revision="$(git rev-parse HEAD)"
clojure -M:abc/adr-claim-migration -- \
  --write-baseline docs/adr/adr-claim-migration-baseline.json \
  --revision "$revision"
```

Expected: the manifest reports 26 ADRs, 146 criteria, and 36 normative
sections (26 Decision, ADR 0014's Decision Matrix, eight exact Hard Rules, and
ADR 0016's carried Hard Rule); re-running to `/tmp/baseline.json` is
byte-identical.

- [ ] **Step 5: Create the source ledger and enter all 35 foundation dispositions**

Use baseline text hashes as keys. Apply this exact disposition vector by
original criterion order:

```clojure
{1  [:correct :retain :retain :correct :correct :move-out-of-acceptance]
 8  [:move-out-of-acceptance :retain :correct :correct :correct]
 9  [:correct :retain :retain :retain :retain :retain :retain]
 10 [:retain :retain :correct :retain :correct]
 11 [:correct :retain :retain]
 33 [:retain :correct :retain :retain :retain :correct :correct :correct :correct]}
```

Every `:correct` rationale must state the exact narrowing/split specified in
Task 6's claim-by-claim correction; moved ADR 0001 C5 targets
`:future-verification`, and moved ADR 0008 C0 records that its implementation
entry point is already present in the byte-immutable Decision section. Do not add
`resulting-claim-ids` until Task 6 finalizes Markdown. Non-foundation rows are
intentionally absent at this family checkpoint and appear as unreviewed
baseline debt; plans 2–5 own them. This uses `require-complete? false`; plan 6
does not own the first complete-mode validation. Plan 5 performs the first
`require-complete? true` validation after all 146 dispositions exist; plan 6
revalidates completeness and alone switches governance enforcement.

- [ ] **Step 6: Generate and inspect the audit inventory**

Run:

```bash
cd abc
clojure -M:abc/adr-evidence-inventory -- \
  --output docs/reports/adr-claim-migration-inventory.json
jq '{baseline:.baseline_criterion_count,
     foundation_dispositions:
       ([.baseline_criteria[]
         | select(.family == "foundation-runtime-identity"
                  and .disposition != null)] | length)}' \
  docs/reports/adr-claim-migration-inventory.json
```

Expected: baseline count 146; foundation disposition count 35; no reviewed
foundation row has a null disposition.

- [ ] **Step 7: Run GREEN and commit**

Run: `cd abc && bin/kaocha --focus abc.tools.adr-claim-migration-test --focus abc.tools.adr-evidence-inventory-test`

Expected: PASS.

```bash
git add abc/docs/adr/adr-claim-migration-baseline.json \
  abc/docs/adr/adr-claim-migration.edn \
  abc/src/abc/tools/adr_evidence_inventory.clj \
  abc/test/abc/tools/adr_evidence_inventory_test.clj abc/deps.edn \
  abc/docs/reports/adr-claim-migration-inventory.json
git commit -m "docs(adr): snapshot claim migration baseline"
```

### Task 2A: Deterministic evidence registration interface

**Files:**
- Create: `abc/src/abc/tools/adr_evidence_register.clj`
- Create: `abc/test/abc/tools/adr_evidence_register_test.clj`
- Create: `abc/nix/adr-family-clean.jq`
- Modify: `abc/deps.edn`

**Interfaces:**
- Consumes: a checked-in EDN entry template plus already-materialized evidence bundles.
- Produces: canonical-hash-complete entries and an atomic update of `docs/adr/adr-evidence.edn` without hiding unrelated registry failures.

- [ ] **Step 1: Write failing template and merge tests**

Use this closed template contract:

```clojure
{:schema-version :abc-adr-evidence-registration-v1
 :entries
 [{:claim-id "ADR-0001-C1"
   :claim-kind :fixture-behavior
   :evidence-kind :fixture-conformance
   :artifact-path "docs/evidence/adr-runs/example.json"
   :observation-key "example-passes"
   :expected {:operator := :value true}}]}
```

Assert that `materialize-template` derives `:artifact-hash` with
`adr-evidence-bundle/load-bundle`; rejects unknown/missing keys, a supplied
`:artifact-hash`, forbidden inline values, an absent bundle, and a missing
observation; and returns no partial entries on error. Assert that
`candidate-registry` replaces all prior entries for a template-owned claim,
preserves other claims, retains multiple template entries for one claim, rejects
duplicate template entries, and produces byte-stable ordering.

- [ ] **Step 2: Run RED**

Run: `cd abc && bin/kaocha --focus abc.tools.adr-evidence-register-test`

Expected: FAIL because the namespace does not exist.

- [ ] **Step 3: Implement materialization and atomic registration**

The CLI contract is exactly:

```bash
clojure -M:abc/adr-evidence-register -- \
  --entries docs/evidence/adr-entries/foundation.edn \
  --registry docs/adr/adr-evidence.edn
```

Add alias `:abc/adr-evidence-register`. Resolve both paths through the shared
repository-containment helper. Load every artifact through `load-bundle`, copy
its `:canonical-hash` into the candidate entry, and require the named
observation to exist. Build the candidate registry, parse the current ADR
claims, and run the ordinary evidence validator over the complete candidate.
During migration, only `:missing-claim-evidence` for claim IDs not owned by the
template may remain; any artifact, hash, freshness, compatibility, duplicate,
predicate, or owned-claim coverage problem aborts without writing. Write a
temporary sibling file and atomically rename it only after validation succeeds.
The command prints the number of registered entries and exits 0; every failure
prints the accumulated problem vector, exits 1, and leaves the registry
byte-identical.

- [ ] **Step 4: Test failure atomicity and idempotence**

In temporary repositories, assert a bad second artifact leaves the registry
unchanged, a successful run injects the canonical hash, and a second successful
run is byte-identical. Assert that an unrelated missing claim is allowed in
migration mode but an unrelated stale/hash/predicate problem is not.

Create `nix/adr-family-clean.jq` with this exact program:

```jq
def family_problem($pattern):
  (((.["claim-id"] // "") | test($pattern))
   or any((.["affected-claim-ids"] // [])[]; test($pattern))
   or ((.file // "") as $file
       | any(($pattern | scan("[0-9]{4}"));
             . as $number | $file | startswith($number))));
[.problems[] | select(family_problem($family))] | length == 0
```

In the registrar test namespace, invoke it against a temporary report holding
one claim-local problem, one artifact-root problem, and one file-only problem.
Each matching case must make `jq -e` fail; an unrelated problem must pass.
Every family plan uses this checked program rather than reimplementing its own
field-name logic.

- [ ] **Step 5: Run GREEN and commit**

Run: `cd abc && bin/kaocha --focus abc.tools.adr-evidence-register-test --focus abc.tools.adr-evidence-test --focus abc.tools.adr-evidence-bundle-test`

Expected: PASS.

```bash
git add abc/src/abc/tools/adr_evidence_register.clj \
  abc/test/abc/tools/adr_evidence_register_test.clj \
  abc/nix/adr-family-clean.jq abc/deps.edn
git commit -m "feat(adr): add deterministic evidence registrar"
```

### Task 2B: Capture profiles and transitive runtime-input closure

**Files:**
- Modify: `abc/schemas/adr-evidence-run.schema.json`
- Modify: `abc/src/abc/tools/adr_evidence_bundle.clj`
- Modify: `abc/src/abc/tools/adr_evidence_capture.clj`
- Create: `abc/src/abc/tools/evidence_io.clj`
- Create: `abc/src/abc/tools/adr_evidence_runtime_inputs.clj`
- Create: `abc/data/evidence-higher-order-calls/manifest-content.edn`
- Create: `abc/data/evidence-higher-order-calls/adr-validate-repository-star.edn`
- Modify: `abc/deps.edn`
- Modify: `abc/src/abc/tools/files.clj`
- Modify: `abc/src/abc/tools/json.clj`
- Modify: `abc/src/abc/tools/hash.clj`
- Modify: `abc/src/abc/tools/adr_evidence.clj`
- Modify: `abc/src/abc/tools/adr_evidence_register.clj`
- Modify: `abc/src/abc/tools/adr_governance.clj`
- Modify: `abc/test/abc/tools/adr_evidence_bundle_test.clj`
- Modify: `abc/test/abc/tools/adr_evidence_capture_test.clj`
- Create: `abc/test/abc/tools/evidence_io_test.clj`
- Create: `abc/test/abc/tools/adr_evidence_runtime_inputs_test.clj`
- Modify: `abc/test/abc/tools/adr_governance_test.clj`
- Modify: `abc/test/abc/tools/adr_evidence_register_test.clj`
- Modify: `abc/flake.nix`
- Modify: `flake.nix`
- Modify: `justfile`

**Interfaces:**
- Consumes: the version-1 capture descriptor and repository-containment
  primitive, preserving version 1 only for already-created legacy fixtures.
- Produces:
  - capture descriptor version `abc-adr-evidence-capture-v2`; every v2
    `clojure-test-v1` or `component-clojure-test-v1` descriptor requires
    a `:runtime-input-manifest` path under `docs/evidence/adr-inputs/` whose
    basename equals the descriptor basename, while
    `repo-files-v1` operational descriptors remain version 1 and bind their
    Nix determinants explicitly;
  - closed manifest value
    `{:schema-version :abc-adr-runtime-inputs-v1 :paths [...]}` whose paths
    are sorted, unique, contained, existing repository/workspace-relative
    runtime-data paths;
  - `(abc.tools.evidence-io/with-read-trace {:identity-root ... :cwd-root ...}
    thunk) -> {:value ... :repository-paths [...] :ephemeral-paths [...]}` and
    `(record-read! path) -> path`; relative paths resolve from `cwd-root`,
    while absolute or relative paths canonically contained by `identity-root`
    normalize to the same identity-root-relative key.
    `(with-ephemeral-root temp-root thunk)` grants a narrow capability for
    generated reads only below one canonical root outside `identity-root` and
    excludes them from manifest equality; repository reads inside that scope
    remain recorded and every other external path throws
    `:external-read-denied`. The namespace
    owns only an invocation-local trace hook and path recording and must not
    require `abc.tools.files` or `abc.tools.hash`;
  - traced physical adapters for text, bytes, streams, readers, JSON, EDN,
    JSONL, directory enumeration, ZIP, Jena, and TEI loads; existing
    `abc.tools.files` repository-read helpers, `abc.tools.json/read-json-file`, and
    `abc.tools.hash/sha256-file` call `record-read!` before reading, so tracing
    covers production helpers transitively invoked by an evidence test;
    add `abc.tools.files/read-text`, `read-bytes`, `input-stream`, `reader`,
    and `list-files` for callers that previously used raw APIs;
  - `(assert-runtime-input-closure! {:repo-root ... :workspace-root ...
    :descriptor ... :repository-paths ...}) -> true` or throws with exact
    `:missing-runtime-input`, `:undeclared-runtime-input`,
    `:invalid-runtime-input-manifest`, `:external-read-denied`,
    `:invalid-ephemeral-root`, or containment problem data;
  - `(with-validated-read-trace! options thunk) -> thunk-value` owns the deep
    version-2 boundary: it completes `with-read-trace`, passes that exact
    trace's `:repository-paths` to `assert-runtime-input-closure!`, and only
    then returns the value. Every exact focused v2 Var must be a single-arity
    `defn`/`defn-` or a `deftest` whose parsed body contains a top-level list
    expression headed by this exact resolved Var. Nesting it under `when`,
    `if`, `is`, another macro, or any dead branch does not establish ownership;
    neither do disconnected calls to the two lower-level operations;
  - `(derive-nix-clojure-source-closure repo-root focused-vars) ->
    sorted-vector<relative-source-or-test-path>` and a closed checked manifest
    `{:schema-version :abc-adr-nix-clojure-closure-v1 :focused-vars [...]
    :paths [...]}`. A version-1 Nix/Clojure descriptor explicitly binds
    `deps.edn`, `deps-lock.json`, `tests.edn`, this manifest, and every listed path; descriptor tests require
    exact expansion rather than trusting the manifest hash alone;
  - immutable caller-scoped higher-order target contract
    `{:schema-version :abc-evidence-higher-order-call-v1
      :caller qualified-var
      :parameters {symbol [qualified-var ...]}}`, stored at the canonical
    caller-derived path under `data/evidence-higher-order-calls/`. Reachability
    binds only contracts it consults, expands every allowed target, and rejects
    an unregistered or non-finite function-valued call;
  - CLI option `--repo-root PATH`, defaulting to `.`;
  - validator option `--workspace-root PATH`, used only to resolve the new
    profile and required whenever such an artifact is present;
  - input-profile kind `component-clojure-test-v1` with required
    `component_root`, `roots`, and `explicit` fields in the JSON bundle and
    corresponding descriptor field `:input-profile :component-root`;
  - component namespace discovery under
    `<repo-root>/<component-root>/{test,src}` while every emitted input key
    remains monorepo-root-relative;
  - `(validate-bundle {:artifact-root abc-root :workspace-root workspace-root}
    artifact-path expected-hash affected-claim-ids)`, preserving ABC-relative
    artifact paths while resolving only component-profile inputs at workspace
    root;
  - `validate-registry` and governance options `:repo-root` (ABC artifact/ADR
    root) plus `:workspace-root` (monorepo input root), with workspace defaulting
    to repo root for legacy/local bundles;
  - root-flake `checks.<system>.monorepo-adr-governance`, built from root
    `self`, as the sole full-registry check once workspace bundles exist.
  - registrar CLI option `--workspace-root PATH`, defaulting to the ABC root,
    so registration performs the same two-root full-registry validation before
    its atomic write.

- [ ] **Step 1: Write failing monorepo-profile tests**

Create a temporary Git repository with `abc/src/example/core.clj`,
`abc/test/example/core_test.clj`, and `ab-validator/docs/report.md`. Use this
descriptor value:

```clojure
{:schema-version "abc-adr-evidence-capture-v2"
 :tool "abc/bin/kaocha"
 :argv ["abc/bin/kaocha" "--focus" "example.core-test/runtime-input-contract"]
 :runtime-input-manifest "abc/docs/evidence/adr-inputs/example.edn"
 :input-profile {:kind "component-clojure-test-v1"
                 :component-root "abc"
                 :roots ["example.core-test"]
                 :explicit ["abc/docs/evidence/adr-capture/example.edn"
                            "abc/docs/evidence/adr-inputs/example.edn"
                            "abc/bin/kaocha"
                            "ab-validator/docs/report.md"]}
 :observation-key "component-check-passes"}
```

Create `abc/docs/evidence/adr-inputs/example.edn` with
`{:schema-version :abc-adr-runtime-inputs-v1 :paths
["ab-validator/docs/report.md"]}`. Assert `derive-minimum-inputs` returns the
explicit paths plus
`abc/test/example/core_test.clj` and `abc/src/example/core.clj`. Assert capture
with `:repo-root` equal to the temporary monorepo runs the command from that
root and emits those monorepo-relative keys. Reject absolute, `..`, symlink-
escaping, missing, and non-Git-root `--repo-root` values; reject a missing or
escaping `component_root`; reject `component_root` on the other profile kinds.
Assert registry/governance validation fails closed when a component-profile
bundle is present without a workspace root, and succeeds when `repo-root` is
the ABC directory and `workspace-root` is its containing monorepo. The
workspace must be exactly the Git root containing root `flake.nix`, `justfile`,
`abc/flake.nix`, and `ab-validator/flake.nix`.
Add a mixed-registry fixture containing one ordinary `clojure-test-v1` bundle
with `src/example/core.clj` below the ABC root and one component-profile bundle
with `abc/src/example/core.clj` plus `ab-validator/docs/report.md` below the
workspace root. Prove both validate together, sibling report drift is caught,
and the component key is never resolved below the ABC root. Extend the root-
flake output contract smoke test to require `monorepo-adr-governance`.
In registrar tests, materialize a component-profile bundle and prove
registration fails without `--workspace-root`, succeeds with the temporary
monorepo root, detects sibling drift, and remains byte-atomic/idempotent.
Add governance CLI tests for both conventional argument vectors and an
optional leading `--`; both must parse identical mode/root/workspace/report
options. This normalizer matches the capture and inventory CLIs and prevents a
separator from becoming a positional repository path.
Add runtime-input tests proving:

- version 2 rejects a missing manifest field, version 1 rejects that new
  field, and a version-2 Clojure descriptor rejects a manifest path absent
  from `:explicit`;
- a manifest rejects unsorted, duplicate, absolute, `..`, missing, lexical-
  escape, and symlink-escape paths;
- `with-read-trace` records each traceable repository read once in sorted
  order and does not record writes; relative and absolute spellings of the
  same contained file collapse to one key. An ephemeral scope rejects a root
  inside the identity tree, rejects reads outside its declared temporary root,
  and cannot launder a repository file copied into that root without recording
  the original repository read;
- `assert-runtime-input-closure!` succeeds only when observed trace paths,
  manifest `:paths`, and descriptor runtime-data inputs are exactly equal;
  descriptor self, manifest self, and statically derived Clojure namespaces
  are excluded from that equality;
- `with-validated-read-trace!` validates its own completed trace. Each exact
  focused Var succeeds only with the owner as an unconditional direct parsed
  body expression; omission, `when`/`if`/`is` nesting, dead branches, and a
  wrapper containing disconnected lower-level trace and assertion calls fail
  `:missing-evidence-boundary-owner`;
- a Nix/Clojure closure manifest rejects unresolved focused Vars, unsorted or
  missing source paths, and a descriptor that binds the manifest but omits one
  listed source/test file or any of `deps.edn`, `deps-lock.json`, and
  `tests.edn`;
- the caller-scoped contracts accept initial finite targets for
  `manifest/content` `sha256-file-fn` and the actual parameter-owning private
  caller `adr/validate-repository*`
  `validate-fn`; unknown target,
  missing target Var, duplicate parameter, caller/path mismatch, or a target
  graph with forbidden I/O fails closed;
- the statically derived reachable-Var graph from a v2 descriptor's exact
  focused Var fails the default-deny lint when it uses an unadapted file,
  stream, reader, directory-enumeration, archive, Jena/TEI library-loader,
  network, or subprocess API. This includes `slurp`, `io/reader`,
  `io/input-stream`, `FileReader`, `Files/readAllBytes`, `Files/readString`,
  `Files/newInputStream`, `line-seq`, path-based `clojure.edn/read`,
  `file-seq`, `.listFiles`, `ZipFile`, `ProcessBuilder`, `shell/sh`, and
  `babashka.process`. `spit` and other writes are not reads.
  the exact named adapters `abc.tools.files/read-json-lines`,
  `abc.tools.files/with-zip-file`, `abc.tools.files/load-jena-model`,
  `abc.tools.files/parse-xml-document`, the other reviewed traced helpers in
  `abc.tools.files` and `abc.tools.hash`, `abc.tools.json/read-json-file`, and a small named adapter
  that calls `record-read!` immediately before a library path-load are the
  only exemptions. A bypass in a transitive production helper therefore
  fails the descriptor contract, while an unrelated unreachable helper does
  not contaminate a narrow boundary. Dynamic Var resolution, reflection-based
  I/O, and an unresolved call edge are forbidden in a v2 evidence graph.
  A higher-order callable must be either a direct symbol resolving to one exact
  reviewed Var, a single-arity literal function whose body is recursively
  analyzed under its bindings, or a defn parameter whose caller-scoped
  contract enumerates every possible target. Computed, conditional,
  collection-hidden, and let-bound callables fail closed; target and literal
  function bodies receive the same lint. The audited signature table names the
  callable argument positions for every admitted higher-order Var, including
  thunk/callback index 1 for `with-validated-read-trace!`,
  `evidence-io/with-read-trace`, `evidence-io/with-ephemeral-root`, and
  `files/with-zip-file`; there are no callback-taking trusted APIs outside
  that inventory. Threading forms are reconstructed with the threaded value
  in its semantic position before those signatures are checked.

- [ ] **Step 2: Run RED**

Run:

```bash
cd abc
bin/kaocha --focus abc.tools.adr-evidence-bundle-test \
  --focus abc.tools.adr-evidence-capture-test \
  --focus abc.tools.evidence-io-test \
  --focus abc.tools.adr-evidence-runtime-inputs-test
```

Expected: FAIL because the new profile and CLI option are unsupported.

- [ ] **Step 3: Implement the exact profile and CLI boundary**

Extend only the executable-run `inputProfile` schema. Preserve the existing
closed shapes for `clojure-test-v1` and `repo-files-v1`; add a third `oneOf`
branch:

```json
{
  "type": "object",
  "additionalProperties": false,
  "required": ["kind", "component_root", "roots", "explicit"],
  "properties": {
    "kind": {"const": "component-clojure-test-v1"},
    "component_root": {"$ref": "#/$defs/nonEmptyString"},
    "roots": {"type": "array", "uniqueItems": true,
              "items": {"$ref": "#/$defs/nonEmptyString"}},
    "explicit": {"type": "array", "uniqueItems": true,
                  "items": {"$ref": "#/$defs/nonEmptyString"}}
  }
}
```

Update the descriptor validator's closed key set and version dispatch. Version
1 retains exactly the old keys. Version 2 permits and requires
`:runtime-input-manifest` for either Clojure profile and rejects it for
`repo-files-v1`. Do not silently reinterpret a version-1 descriptor as
version 2. The emitted run bundle remains `abc-adr-evidence-run-v1`: the
descriptor and manifest are themselves hashed inputs, so this strengthens
capture preconditions without changing the observation artifact shape.

Implement `evidence-io` as an acyclic trace-hook leaf. `with-read-trace` binds
an invocation-local collector. Do not pass an absolute path to
`path-containment/path-state`, whose contract intentionally rejects absolutes.
Instead, `record-read!` first resolves a relative path against canonical
`cwd-root` or normalizes an absolute path, then classifies the canonical result
against canonical `identity-root` and active ephemeral roots. Only after a
repository path is relativized to `identity-root` may existing containment
validation run on that relative key. A dynamically bound ephemeral scope may
suppress only generated reads canonically below its declared temporary root,
which must be outside the identity root; it never suppresses a repository read
or permits another external path. It must not use global mutable state and
must not require `files` or `hash`. Modify all physical repository-reading
functions in
`abc.tools.files`, `abc.tools.json/read-json-file`, and
`abc.tools.hash/sha256-file` to call the hook before their existing read. A
production helper that must give a path directly to Jena or another library
uses a named traced adapter which calls `record-read!` immediately before the
library call; do not whitelist arbitrary callers.

Add traced directory, archive, Jena, and TEI adapters. Directory enumeration
records every returned regular file so additions and removals affect exact
closure. The closure-wide lint rejects every subprocess API in a version-2
descriptor's derived namespaces. Tests needing a real child command must use
a version-1 sandboxed Nix descriptor; injected fake runners remain allowed in
pure unit tests.

Use clj-kondo analysis to resolve Vars and a directly pinned
`org.clojure/tools.reader` source-form pass to identify exact list-head and
direct defn-parameter invocation positions. Match source spans exactly; do not
infer invocation from column ordering. The analyzer supports a closed, audited
subset of Clojure forms and rejects unsupported macros, computed heads,
invoked let-bound functions, dynamic resolution, and reflective invocation.
Source/test namespace files remain freshness inputs, but the default-deny
semantic scan follows only exactly classified resolved call edges. Fail closed
on a missing focus, unresolved invocation, or a reachable raw
I/O/network/process symbol.
The initial higher-order contracts are not a blanket allowlist. If RED
analysis finds another legitimate function-valued call, stop and add a
reviewed finite caller/parameter/target entry with positive and unknown-target
negative tests before any family descriptor may rely on it.

The `:descriptor` option is exactly `{:path "..." :value descriptor-map}`;
this avoids adding a second path option while keeping the descriptor path out
of its own serialized value. `assert-runtime-input-closure!` loads the descriptor's manifest through the
same contained read boundary, validates the closed two-key EDN shape, and
compares sets only after separately proving the original vectors sorted and
unique. For an ABC-local profile, runtime paths resolve below `repo-root`; for
a component profile they resolve below `workspace-root`. Its expected
explicit set is exactly:

```clojure
(into (sorted-set)
      (concat (:paths manifest)
              [descriptor-path (:runtime-input-manifest descriptor)]))
```

The focused boundary calls `with-validated-read-trace!`; that owner supplies
`:repository-paths` from its own completed `with-read-trace` result to
`assert-runtime-input-closure!`. Capture refuses to execute a v2 descriptor
whose static manifest/explicit comparison fails or when any exact focused Var
lacks that owner as a top-level direct body call. Merely reaching the owner
transitively, nesting it under a conditional/assertion macro, or co-locating
disconnected lower-level calls does not satisfy the contract.

`derive-minimum-inputs` resolves the contained component root once and searches
only its `test/` and `src/` directories. `capture!` continues to receive an
explicit `repo-root`; the CLI parses `--repo-root`, verifies its real path is
exactly `git rev-parse --show-toplevel`, and passes it through. The descriptor
path and every explicit path are still evidence inputs relative to that root.
The subprocess working directory is the monorepo root; component selection is
recorded by the exact repository runner `abc/bin/kaocha`, which resolves and
enters its own ABC root before executing Clojure. An ordinary ABC-local v2
descriptor uses exactly `:tool "bin/kaocha"`; a component v2 descriptor uses
exactly `:tool "<component-root>/bin/kaocha"`; `argv[0]` equals that tool and
the remaining argv is one or more exact `--focus`, qualified-Var string pairs.
Focus pairs are unique and each focused Var must resolve to a `deftest` source
form whose direct top-level body owns `with-validated-read-trace!`; ordinary
functions and namespace-wide focus are not evidence boundaries. No shell,
extra option, or positional argument is admitted. The normalized runner path
is an explicit hashed input and must resolve through `path-state` to a
contained, regular, executable file; missing runners and symlink escapes fail
before execution. After execution, capture parses Kaocha's stable summary and
requires its executed-test count to equal the unique focus count. Missing,
zero-test, skipped, or count-mismatched summaries invalidate capture even when
the process exits zero. Do not add an ambient working-directory field or
permit repository-parent inputs.
Every v2 derived input set includes only the caller-scoped higher-order
contract files actually consulted by its reachable graph (with `abc/data/...`
keys for a component profile). Adding a contract for an unrelated future
caller therefore cannot stale prior evidence. Nix/Clojure closure descriptors
apply the same rule and bind every consulted contract.

Thread `workspace-root` separately through `validate-registry`,
`validate-bundle`, and the governance CLI. Change `validate-bundle` to the
two-root options-map signature above. Ordinary artifact/registry paths and all
ordinary/external profile inputs remain ABC-root-relative; only
`component-clojure-test-v1` input keys resolve against the validated workspace.
Thread the same option through `adr-evidence-register/register!` and add its
CLI `--workspace-root` flag; the registrar must not weaken validation or write
before the two-root candidate registry is clean.
Add the same `(if (= "--" (first args)) (rest args) args)` normalizer already
used by capture/inventory before `tools.cli/parse-opts` in governance.
Add the exact operational invocation from the monorepo root:

```bash
nix run ./abc#adr-governance -- \
  --repo-root "$PWD/abc" --workspace-root "$PWD" --mode audit
```

Expose an ABC `adr-governance` app wrapper, then add root-flake check
`monorepo-adr-governance` from root `self` with this exact body:

```nix
monorepo-adr-governance = pkgs.runCommand "soranoha-monorepo-adr-governance" {
  src = self;
} ''
  mkdir -p "$out"
  ${abcApps.adr-governance.program} \
    --repo-root "$src/abc" \
    --workspace-root "$src" \
    --mode audit \
    --report "$out/report.json"
'';
```

Define `abcApps = optionalOutputAttrs abc "apps" system;` in the root checks
let where this body is constructed.

The app's code/dependency closure may come from the `abc` flake input, but both
validated roots are the same root `self` snapshot; this prevents a separately
pinned ABC source from validating sibling inputs that are absent from its
artifact snapshot.
Make this root check the sole full-registry authority in `just
validate-migration`; remove the component-only `abc#checks...adr-governance`
from that aggregate because it cannot see workspace-bound evidence and must not
silently skip it. Plan 6 changes this one root check from audit to enforce.

- [ ] **Step 4: Run GREEN and commit**

Run:

```bash
cd abc
bin/kaocha --focus abc.tools.adr-evidence-bundle-test \
  --focus abc.tools.adr-evidence-capture-test \
  --focus abc.tools.evidence-io-test \
  --focus abc.tools.adr-evidence-runtime-inputs-test \
  --focus abc.tools.adr-evidence-test \
  --focus abc.tools.adr-governance-test \
  --focus abc.tools.adr-evidence-register-test
cd ..
bash tests/root-flake-output-contract-smoke.sh
system="$(nix eval --impure --raw --expr builtins.currentSystem)"
nix build --no-link ".#checks.${system}.monorepo-adr-governance"
```

Expected: all tests and the root audit check pass.

```bash
git add abc/schemas/adr-evidence-run.schema.json \
  abc/src/abc/tools/adr_evidence_bundle.clj \
  abc/src/abc/tools/adr_evidence_capture.clj \
  abc/src/abc/tools/evidence_io.clj \
  abc/src/abc/tools/adr_evidence_runtime_inputs.clj \
  abc/data/evidence-higher-order-calls/manifest-content.edn \
  abc/data/evidence-higher-order-calls/adr-validate-repository-star.edn \
  abc/src/abc/tools/files.clj \
  abc/src/abc/tools/json.clj \
  abc/src/abc/tools/hash.clj \
  abc/src/abc/tools/adr_evidence.clj \
  abc/src/abc/tools/adr_evidence_register.clj \
  abc/src/abc/tools/adr_governance.clj \
  abc/test/abc/tools/adr_evidence_bundle_test.clj \
  abc/test/abc/tools/adr_evidence_capture_test.clj \
  abc/test/abc/tools/evidence_io_test.clj \
  abc/test/abc/tools/adr_evidence_runtime_inputs_test.clj \
  abc/test/abc/tools/adr_evidence_test.clj \
  abc/test/abc/tools/adr_governance_test.clj \
  abc/test/abc/tools/adr_evidence_register_test.clj \
  abc/deps.edn abc/flake.nix flake.nix justfile tests/root-flake-output-contract-smoke.sh
git commit -m "feat(adr): capture component tests from monorepo root"
```

### Task 3: Historical Evidence and Future Verification claim-header lint

**Files:**
- Modify: `abc/src/abc/tools/adr.clj`
- Modify: `abc/test/abc/tools/adr_test.clj`

**Interfaces:**
- Consumes: parsed `:section-bodies` already returned by `parse-adr`.
- Produces: `:claim-header-outside-acceptance` problems with `:section` equal to exactly `Historical Evidence` or `Future Verification`.
- Also routes `adr-files` directory enumeration through
  `abc.tools.files/list-files` and `parse-adr` content reads through
  `abc.tools.files/read-text`; their behavior and sort order remain unchanged,
  but Plan 5's graph/policy evidence can now trace them.

- [ ] **Step 1: Write failing parser/repository tests**

Create temp ADRs where both exact sections contain
`**ADR-0042-C1 — structural-invariant:**`; assert two problems. Assert ordinary
bold prose and a header only in Acceptance Criteria remain valid.
Add adapter-characterization tests proving `adr-files` returns the same sorted
Markdown names and `parse-adr` returns the same value when the traced helpers
are active; a synthetic direct `.listFiles` or `slurp` version fails Plan 1's
reachable-Var lint.

- [ ] **Step 2: Run RED**

Run: `cd abc && bin/kaocha --focus abc.tools.adr-test`

Expected: FAIL because the parser currently ignores claim-looking headers in
non-acceptance sections.

- [ ] **Step 3: Implement the narrow lint**

Search only the two exact section bodies with the existing
`claim-header-pattern`. Do not forbid claim-like English elsewhere and do not
turn either section into Acceptance Criteria.

- [ ] **Step 4: Run GREEN and commit**

Run: `cd abc && bin/kaocha --focus abc.tools.adr-test`

Expected: PASS.

```bash
git add abc/src/abc/tools/adr.clj abc/test/abc/tools/adr_test.clj
git commit -m "feat(adr): lint claims in non-acceptance sections"
```

### Task 4: Hermetic design-bundle and synthetic Git-cliff check

**Files:**
- Modify: `abc/src/abc/tools/validate_design_bundle.clj`
- Modify: `abc/test/abc/tools/validate_design_bundle_test.clj`
- Create: `abc/nix/check-git-cliff-config.sh`
- Modify: `abc/flake.nix`

**Interfaces:**
- Produces: `nix run ./abc#validate-design-bundle` as store-hermetic content/schema/fixture validation; `nix build ./abc#checks.<system>.git-cliff-config` as the sole store-backed Git-cliff config check.

- [ ] **Step 1: Record RED command evidence before editing**

Run from monorepo root: `nix run ./abc#validate-design-bundle`

Expected: exit 1 after `Checking git-cliff configuration`, with “could not find repository” in the Nix-store source.

- [ ] **Step 2: Write a failing boundary test**

In `validate_design_bundle_test.clj`, with-redefine `validate-git-cliff!` to
throw and invoke `validate-design-bundle!` with other costly external passes
stubbed at their existing seam. Assert the throw is never reached. This fails
before removing the call.

- [ ] **Step 3: Run RED focused test**

Run: `cd abc && bin/kaocha --focus abc.tools.validate-design-bundle-test`

Expected: FAIL with the sentinel Git-cliff exception.

- [ ] **Step 4: Remove repository-history work from the design bundle**

Delete `validate-git-cliff!` and its final invocation/log messages. Remove
`pkgs.git-cliff` from the app PATH; keep `pkgs.libxml2` and `TEI_SCHEMA_PATH`.

- [ ] **Step 5: Add the synthetic-repository script and Nix check**

The script must:

```bash
set -euo pipefail
repo="$(mktemp -d)"
trap 'rm -rf "$repo"' EXIT
cp "$1" "$repo/cliff.toml"
git -C "$repo" init -q
git -C "$repo" config user.email cliff@example.invalid
git -C "$repo" config user.name 'Git Cliff Fixture'
touch "$repo/fixture"
git -C "$repo" add fixture cliff.toml
git -C "$repo" commit -q -m 'feat: fixture'
git -C "$repo" cliff --config cliff.toml --unreleased --strip header \
  --output "$repo/changelog.md"
test -s "$repo/changelog.md"
```

The Nix check binds `./cliff.toml` and the script, with `pkgs.git` and
`pkgs.git-cliff` as native inputs.

- [ ] **Step 6: Format and run GREEN boundaries**

Run:

```bash
nixfmt abc/flake.nix
cd abc && bin/kaocha --focus abc.tools.validate-design-bundle-test
cd ..
nix run ./abc#validate-design-bundle
system="$(nix eval --impure --raw --expr builtins.currentSystem)"
nix build "./abc#checks.${system}.git-cliff-config" --print-build-logs
```

Expected: focused tests PASS; both Nix commands exit 0; design-bundle output has
no Git-cliff stage; the separate check creates a nonempty synthetic changelog.

- [ ] **Step 7: Commit the trust-boundary repair**

```bash
git add abc/src/abc/tools/validate_design_bundle.clj \
  abc/test/abc/tools/validate_design_bundle_test.clj \
  abc/nix/check-git-cliff-config.sh abc/flake.nix
git commit -m "fix(abc): make design bundle validation hermetic"
```

### Task 5: Direct foundation assertions missing from current tests

**Files:**
- Create: `abc/test/abc/tools/foundation_evidence_test.clj`
- Modify: `abc/src/abc/tools/validate_design_bundle.clj`
- Modify: `abc/test/abc/tools/validate_design_bundle_test.clj`
- Modify: `abc/test/abc/tools/materialize_import_test.clj`
- Modify: `abc/test/abc/sim/divergences_test.clj`

**Interfaces:**
- Produces narrowly assertive boundaries used by Stage B: manifest example
schema conformance, generated manifest conformance, failure-manifest identity,
wrapper/CI wiring, and dated D7 structural state.

- [ ] **Step 1: Write the direct fixture tests**

Add tests that:

- validate committed success/failure manifests against `manifest.schema.json`;
- assert failure status, `errors` sidecar, exact non-null attempted-output/input coordinates, and top-level-only `artifact_id`;
- materialize parser/warnings manifests and validate both against the schema;
- assert `bin/validate-design-bundle.sh` delegates with `exec clojure -M:abc/validate-design-bundle "$@"` and contains no validation implementation;
- parse `.github/workflows/validation.yml` and assert it invokes
  `nix run ... .#validate-design-bundle` under the ABC checkout boundary;
- assert D7 is `:fixed` and its notes contain `2026-07-12`.

- [ ] **Step 2: Run RED against the intended exact assertions**

Run: `cd abc && bin/kaocha --focus abc.tools.foundation-evidence-test --focus abc.sim.divergences-test`

Expected: new namespace/tests fail until helper fixture construction and exact
wiring paths are supplied; no assertion may merely test file existence.

- [ ] **Step 3: Add only the minimal fixture helpers/assertions**

Reuse `materialize/materialize-import!`, `schema/validation-errors`, and
`files/read-json`; do not duplicate the production identity algorithm.

- [ ] **Step 4: Strengthen schema-hash compatibility wording with tests**

In `materialize_import_test.clj`, keep parser registered-compatibility behavior
separate from diagnostic exact-current behavior. Add a test whose mismatched
diagnostic hash produces an error; do not claim a diagnostic compatibility
registry exists.

Add `evidence-input-paths` to `validate_design_bundle.clj` as the sorted union
of the existing schema, fixture, context, policy, registry, and generated-view
catalogs actually passed to file-reading helpers. In its focused test, wrap
the shared file readers, run the supported pure validation path, and assert the
observed repository-relative read set is exactly `evidence-input-paths`.

- [ ] **Step 5: Run GREEN and commit**

Run: `cd abc && bin/kaocha --focus abc.tools.foundation-evidence-test --focus abc.tools.materialize-import-test --focus abc.sim.divergences-test`

Expected: PASS.

```bash
git add abc/test/abc/tools/foundation_evidence_test.clj \
  abc/src/abc/tools/validate_design_bundle.clj \
  abc/test/abc/tools/validate_design_bundle_test.clj \
  abc/test/abc/tools/materialize_import_test.clj \
  abc/test/abc/sim/divergences_test.clj
git commit -m "test(abc): assert foundation evidence boundaries"
```

### Task 6: Foundation Stage A — correct ADRs, assign claims, and bind lifecycle

**Files:**
- Modify: `abc/docs/adr/0001-manifest-identity.md`
- Modify: `abc/docs/adr/0008-abc-tools-runtime.md`
- Modify: `abc/docs/adr/0009-imported-output-materialization.md`
- Modify: `abc/docs/adr/0010-manifest-identity-hardening.md`
- Modify: `abc/docs/adr/0011-generated-fixture-policy.md`
- Modify: `abc/docs/adr/0033-source-bundle-identity.md`
- Modify: `abc/docs/adr/adr-claim-migration.edn`
- Modify: `abc/docs/reports/adr-claim-migration-inventory.json`
- Modify: `abc/docs/reports/adr-evidence-migration.json`

**Interfaces:**
- Consumes: Tasks 1–5.
- Produces: final claim IDs `ADR-0001-C1..C5`, `ADR-0008-C1..C4`,
  `ADR-0009-C1..C7`, `ADR-0010-C1..C5`, `ADR-0011-C1..C3`, and
  `ADR-0033-C1..C11`; complete foundation baseline-to-live mappings.

- [ ] **Step 1: Add lifecycle headers exactly as approved**

```text
ADR 0001: Validation scope: fixture       / Release authority: publication
ADR 0008: Validation scope: operational   / Release authority: development
ADR 0009: Validation scope: fixture       / Release authority: development
ADR 0010: Validation scope: fixture       / Release authority: publication
ADR 0011: Validation scope: fixture       / Release authority: development
ADR 0033: Validation scope: full-corpus   / Release authority: publication
```

- [ ] **Step 2: Correct ADR 0001 and assign five claims**

Use these observable statements and kinds:

1. `fixture-behavior`: committed success/failure and generated parser/warning
   manifests validate against the manifest schema.
2. `fixture-behavior`: inline canonical values pin null and array order.
3. `structural-invariant`: the schema rejects nested `artifact_id`.
4. `fixture-behavior`: the failure fixture carries the exact asserted fields.
5. `structural-invariant`: two successful entries with the same `artifact_id`
   and different content hashes produce a reproducibility conflict.

Move the non-JVM RFC 8785 condition verbatim under `## Future Verification`.
Delete the nonexistent nondeterministic schema exception.

- [ ] **Step 3: Correct ADR 0008 and assign four claims**

Remove the duplicate unprovisioned raw-Clojure command from Acceptance
Criteria while retaining the already-existing Decision text byte-for-byte as
the implementation entry point. Keep claims for wrapper delegation
(`structural-invariant`), supported Nix app exit zero (`operational-behavior`),
workflow wiring (`structural-invariant`), and named pure-helper behavior
(`structural-invariant`). Do not claim a local/CI run from workflow text.

- [ ] **Step 4: Correct ADRs 0009–0011 and assign their declared ranges**

- ADR 0009 C1 uses the new direct generated-manifest schema test, not the old
  materialization citation. Separate parser registered compatibility from
  diagnostic exact-current matching. Keep temp orchestration and wrapper
  delegation as separate claims.
- ADR 0010 replaces “test covers” prose with the identity behavior it asserts;
  its operational claim names the supported Nix app.
- ADR 0011 C1 names the supported Nix app and its temp materialization/schema
  behavior; C2/C3 remain the ordered-map and two-run byte claims.

- [ ] **Step 5: Split/narrow ADR 0033 into eleven claims**

Use this order and kinds:

1. `fixture-behavior` canonical all-member schema fixture.
2. `fixture-behavior` Clojure known-answer reproduction.
3. `fixture-behavior` repack/image evolution.
4. `fixture-behavior` parser-IR bundle/primary roles.
5. `fixture-behavior` role-specific snapshot validation.
6. `fixture-behavior` ungated P16.3.
7. `structural-invariant` dated D7 fixed state.
8. `fixture-behavior` complete legacy-shaped workset fixture readability.
9. `corpus-behavior` pinned source-bundle corpus report maxima under commit
    `0e9ea3e586eb0aa34039fabfc85a407d2f98b165`.
10. `fixture-behavior` member-count, per-member-byte, and total-byte limit
    enforcement at both declared-size and streamed-byte boundaries.
11. `fixture-behavior` exact strict atomic abort and best-effort counted,
    non-release-admissible behavior.

Move broad “historical manifests” prose to `## Historical Evidence`. State
best-effort evidence exactly: `release_admissible=false`, partial workflow,
and build exit 1; do not invent a separate release validator. Keep the Rust
consumer/producer authority rule in Decision: current tests show supplied hash
consumption, but absence of a second producer is not an independently durable
acceptance observation.

- [ ] **Step 6: Add resulting claim IDs to all 35 foundation ledger rows**

Moved rows get `[]`; corrected split rows get multiple IDs; all retained rows
get one ID. Validate that the union is exactly the 35 final live claim IDs.

- [ ] **Step 7: Regenerate inventory and audit report**

```bash
cd abc
clojure -M:abc/adr-evidence-inventory -- \
  --output docs/reports/adr-claim-migration-inventory.json
clojure -M:abc/adr-governance -- --mode audit \
  --report docs/reports/adr-evidence-migration.json
```

Expected audit delta: all six foundation ADRs lose missing lifecycle problems;
all final foundation claims lose `:missing-claim-header` and acquire only
`:missing-claim-evidence`; no new parser, dependency, graph, malformed-header,
ledger, or claim-kind problem appears. Compare stable problem identities, not
the global problem count, because later family corrections may land at a
different checkpoint.

- [ ] **Step 8: Run Stage A verification**

```bash
cd abc
bin/kaocha --focus abc.tools.adr-test \
  --focus abc.tools.adr-claim-migration-test \
  --focus abc.tools.adr-evidence-inventory-test \
  --focus abc.tools.foundation-evidence-test
clojure -M:abc/adr-governance -- --mode audit
git diff --check
```

Expected: tests PASS; audit exits 0 with exactly the expected migration debt;
worktree diff contains no bundles or registry entries yet.

- [ ] **Step 9: Commit clean Stage A**

```bash
git add abc/docs/adr/0001-manifest-identity.md \
  abc/docs/adr/0008-abc-tools-runtime.md \
  abc/docs/adr/0009-imported-output-materialization.md \
  abc/docs/adr/0010-manifest-identity-hardening.md \
  abc/docs/adr/0011-generated-fixture-policy.md \
  abc/docs/adr/0033-source-bundle-identity.md \
  abc/docs/adr/adr-claim-migration.edn \
  abc/docs/reports/adr-claim-migration-inventory.json \
  abc/docs/reports/adr-evidence-migration.json
git commit -m "docs(adr): bind foundation evidence claims"
git status --short
```

Expected: clean tree. Do not begin capture otherwise.

### Task 7: Check in capture descriptors at the Stage A boundary

**Files:**
- Create: `abc/docs/evidence/adr-capture/foundation-manifest-identity.edn`
- Create: `abc/docs/evidence/adr-capture/foundation-import-materialization.edn`
- Create: `abc/docs/evidence/adr-capture/foundation-validation-helpers.edn`
- Create: `abc/docs/evidence/adr-capture/source-bundle-fixtures.edn`
- Create: `abc/docs/evidence/adr-capture/source-identity-simulation.edn`
- Create: `abc/docs/evidence/adr-capture/source-snapshot-fixtures.edn`
- Create: `abc/docs/evidence/adr-capture/source-bundle-corpus.edn`
- Create: `abc/docs/evidence/adr-capture/design-bundle-operational.edn`
- Create: six same-stem manifests under `abc/docs/evidence/adr-inputs/` for
  the focused Clojure descriptors
- Create: `abc/docs/evidence/adr-inputs/source-bundle-corpus-nix-clojure-closure.edn`
- Create: `abc/docs/evidence/adr-inputs/design-bundle-operational-nix-clojure-closure.edn`
- Create: `abc/docs/evidence/adr-entries/foundation.edn`

**Interfaces:**
- Produces immutable commands/observation keys used in Task 8.

- [ ] **Step 1: Author descriptors with exact boundaries**

Use `abc-adr-evidence-capture-v2` for the six focused Clojure descriptors and
version 1 for the two `repo-files-v1` Nix boundaries. Every v2 descriptor sets
`:runtime-input-manifest` with the same basename under
`docs/evidence/adr-inputs/` and includes
both its descriptor and manifest paths in `:input-profile :explicit`. Its
manifest `:paths` vector is exactly the remaining explicit runtime-data paths.
Each v2 descriptor also binds `bin/kaocha`, sets both `:tool` and `argv[0]` to
`bin/kaocha`, and lists only exact `--focus`, qualified-Var pairs after it.
Those focus pairs are unique `deftest` Vars, and successful capture requires
Kaocha to report exactly one executed test for each pair.

| Descriptor | Profile/command | Observation |
|---|---|---|
| `foundation-manifest-identity.edn` | exact focus `abc.tools.foundation-evidence-test/foundation-manifest-identity-contract`; this dedicated wrapper directly asserts the selected JCS/code-as-spec/index predicates | `manifest-identity-contracts-pass` |
| `foundation-import-materialization.edn` | exact focus `abc.tools.foundation-evidence-test/foundation-import-materialization-contract`; explicitly bind `examples/ab-validator-output/{README.md,comparison-report.json,divergence.json,manifest-inputs.json,parser-ir.json,run-summary.jsonl,source-region-coverage.json,warnings.jsonl}`, manifest/IR/diagnostic schemas, and compatibility data | `import-materialization-contracts-pass` |
| `foundation-validation-helpers.edn` | exact focus `abc.tools.foundation-evidence-test/foundation-validation-helpers-contract`; bind every schema/fixture/data path reached by the selected pure helper calls | `validation-helper-contracts-pass` |
| `source-bundle-fixtures.edn` | exact focus `abc.tools.foundation-evidence-test/source-bundle-fixtures-contract`; bind known answer and source-bundle schema | `source-bundle-fixtures-pass` |
| `source-identity-simulation.edn` | exact focus `abc.tools.foundation-evidence-test/source-identity-simulation-contract`; bind simulation configs and D7 table | `source-identity-simulation-pass` |
| `source-snapshot-fixtures.edn` | exact focus `abc.tools.foundation-evidence-test/source-snapshot-fixtures-contract`; bind schema/data fixtures | `source-snapshot-fixtures-pass` |
| `source-bundle-corpus.edn` | `repo-files-v1`; command `nix build --no-link .#checks.$(nix eval --impure --raw --expr builtins.currentSystem).source-bundle-corpus --print-build-logs`; bind descriptor, flake/locks/dependency configs, checked closure manifest for `abc.tools.source-bundle-report/-main` plus every expanded path, source-bundle schema, and the checked corpus summary | `source-bundle-corpus-reproduced` |
| `design-bundle-operational.edn` | `repo-files-v1`; command `nix run .#validate-design-bundle`; bind descriptor, flake/locks/dependency configs, checked closure manifest for `abc.tools.validate-design-bundle/-main` plus every expanded path, wrapper, and exactly `(validate-design-bundle/evidence-input-paths)` | `design-bundle-exits-zero` |

For Nix descriptors use `bash -lc` as argv so shell substitution is recorded
literally. Do not attach fixture claims to the operational observation.
Both Nix/Clojure descriptors bind `deps.edn`, `deps-lock.json`, and
`tests.edn`. Generate their closure manifests with Plan 1's helper and assert
exact descriptor expansion with missing/extra-path negative tests.
Each version-2 wrapper has `with-validated-read-trace!` as a direct top-level
body expression inside its `deftest` source form, scopes every freshly
generated directory with
`with-ephemeral-root`, and calls only the exact operation under evidence.
Do not focus a whole legacy test namespace or call another `deftest` Var.

- [ ] **Step 2: Validate descriptor closure with tests**

Extend `adr_evidence_capture_test.clj` to load every checked-in descriptor and
assert exact key set, stable observation key, nonempty explicit set, and that
the descriptor path is explicit.
For every version-2 descriptor, invoke
`adr-evidence-runtime-inputs/assert-runtime-input-closure!` first with the
checked manifest paths as the observed trace to prove static descriptor
equality, then run the focused evidence test that obtains its real observed
trace through `evidence-io/with-read-trace`. Require zero missing or undeclared
runtime paths. Starting from each exact focused Var, scan its statically
resolved reachable-Var graph across source and test namespaces; reject direct
repository-read bypasses except the named traced
adapters defined in Task 2B, and reject every network/subprocess API. This
central descriptor test is mandatory even when a family also owns a focused
closure test, so a descriptor plus hand-written manifest cannot capture
without executing the real trace assertion. It resolves every exact focused
Var and verifies that the owning namespace contains the descriptor-keyed
`with-read-trace` plus `assert-runtime-input-closure!` call before running that
Var; missing focus or missing closure wiring fails before capture.
Also exercise the real repository runner against a known non-test Var, a
duplicate focus, a missing/non-executable runner, and an external symlink;
each must fail capture before it can certify a passing observation. Pin the
Kaocha summary-count parser with zero, missing, and focus-count-mismatch cases.
For `design-bundle-operational.edn`, assert its explicit set equals the fixed
wrapper/flake set union `(validate-design-bundle/evidence-input-paths)`; a new
runtime-read file therefore fails the descriptor contract until it is bound.

Author `foundation.edn` with one row for every final foundation claim and an
additional corroborating row only where a criterion explicitly requires two
independent artifacts. Map claims to the narrowest observation in the table
above: manifest/JCS/oracle claims to `foundation-manifest-identity`, imported
output/determinism claims to `foundation-import-materialization`, validation
helper/wrapper/CI-structure claims to `foundation-validation-helpers`, the
supported Nix exit claim to `design-bundle-operational`, source-bundle schema
and known-answer claims to `source-bundle-fixtures`, P16.3/evolution/D7/admission
claims to `source-identity-simulation`, legacy/source-role claims to
`source-snapshot-fixtures`, and the pinned corpus claim only to
`source-bundle-corpus`. The template claim/evidence kinds must exactly match
Task 6 and the compatibility matrix; every predicate is
`{:operator := :value true}`. Do not place an artifact hash in the template.

- [ ] **Step 3: Run tests and commit descriptors before capture**

Run: `cd abc && bin/kaocha --focus abc.tools.adr-evidence-capture-test`

Expected: PASS.

```bash
git add abc/docs/evidence/adr-capture abc/docs/evidence/adr-inputs \
  abc/docs/evidence/adr-entries/foundation.edn \
  abc/test/abc/tools/adr_evidence_capture_test.clj
git commit -m "docs(adr): pin foundation evidence capture commands"
git status --short
```

Expected: clean tree. This descriptor commit is the final Stage A capture
boundary and the producer revision recorded by every Stage B bundle.

### Task 8: Foundation Stage B — capture bundles and join claims

**Files:**
- Create: `abc/docs/evidence/adr-runs/foundation-manifest-identity.json`
- Create: `abc/docs/evidence/adr-runs/foundation-import-materialization.json`
- Create: `abc/docs/evidence/adr-runs/foundation-validation-helpers.json`
- Create: `abc/docs/evidence/adr-runs/source-bundle-fixtures.json`
- Create: `abc/docs/evidence/adr-runs/source-identity-simulation.json`
- Create: `abc/docs/evidence/adr-runs/source-snapshot-fixtures.json`
- Create: `abc/docs/evidence/adr-runs/source-bundle-corpus.json`
- Create: `abc/docs/evidence/adr-runs/design-bundle-operational.json`
- Create: `abc/docs/evidence/adr-entries/foundation.edn` (committed in Task 7)
- Modify: `abc/docs/adr/adr-evidence.edn`
- Modify: `abc/docs/reports/adr-claim-migration-inventory.json`
- Modify: `abc/docs/reports/adr-evidence-migration.json`

**Interfaces:**
- Consumes: clean descriptor commit from Task 7.
- Produces: passing compatible evidence for every final foundation claim. The
  design-bundle artifact is shared read-only with later plans.

- [ ] **Step 1: Capture each descriptor from a clean tree into an external staging directory**

The capture tool rejects any dirty tree. Writing the first bundle directly
under `docs/evidence/adr-runs/` would therefore prevent the second capture.
Create one staging directory outside the repository, then for each stem run
from `abc/`:

```bash
stage="$(mktemp -d)"
clojure -M:abc/adr-evidence-capture -- \
  --descriptor "docs/evidence/adr-capture/${stem}.edn" \
  --output "$stage/${stem}.json"
```

Expected: exit 0 for all eight; each JSON observation value is `true`; input
map contains the descriptor itself; `git status --short` remains empty after
every capture. If any command fails, delete only its staged output and repair
forward in a new clean commit—do not add a registry entry.

- [ ] **Step 2: Validate all staged artifacts, then materialize them together**

Read each staged file and pass its value to
`abc.tools.adr-evidence-bundle/validate-bundle-value`; assert zero schema/value
problems, then copy the exact generated bytes together:

```bash
mkdir -p docs/evidence/adr-runs
for stem in foundation-manifest-identity foundation-import-materialization \
  foundation-validation-helpers source-bundle-fixtures \
  source-identity-simulation source-snapshot-fixtures source-bundle-corpus \
  design-bundle-operational; do
  install -m 0644 "$stage/$stem.json" "docs/evidence/adr-runs/$stem.json"
done
```

Expected: only the eight intended run bundles make the worktree dirty.

- [ ] **Step 3: Materialize and validate all claim joins through the registrar**

```bash
cd abc
clojure -M:abc/adr-evidence-register -- \
  --entries docs/evidence/adr-entries/foundation.edn \
  --registry docs/adr/adr-evidence.edn
```

Expected: exit 0 and exactly 35 distinct foundation claim IDs registered. The
tool derives canonical artifact hashes, validates observation existence and
kind compatibility, and writes the sorted registry atomically. Run it a second
time and assert `git diff -- docs/adr/adr-evidence.edn` is unchanged. Never
substitute `sha256sum` or hand-edit a derived hash.

- [ ] **Step 4: Regenerate reports and assert family delta**

```bash
cd abc
clojure -M:abc/adr-evidence-inventory -- \
  --output docs/reports/adr-claim-migration-inventory.json
clojure -M:abc/adr-governance -- --mode audit \
  --report docs/reports/adr-evidence-migration.json
jq -e --arg family '^(ADR-)?(0001|0008|0009|0010|0011|0033)-' \
  -f nix/adr-family-clean.jq \
  docs/reports/adr-evidence-migration.json
```

Expected: exit 0: no family problems, including shared artifact-root problems whose
ownership is carried only by `affected-claim-ids`. Only later-family debt may
remain. No foundation missing, stale, incompatible-kind, failed-predicate,
missing-input, or artifact-hash problem is permitted.

- [ ] **Step 5: Run proportional verification**

```bash
cd abc
bin/kaocha --focus abc.tools.adr-test \
  --focus abc.tools.adr-claim-migration-test \
  --focus abc.tools.adr-evidence-test \
  --focus abc.tools.adr-evidence-bundle-test \
  --focus abc.tools.adr-evidence-capture-test \
  --focus abc.tools.adr-evidence-inventory-test \
  --focus abc.tools.foundation-evidence-test \
  --focus abc.tools.materialize-import-test \
  --focus abc.tools.source-bundle-test \
  --focus abc.tools.materialize-source-snapshot-test \
  --focus abc.sim.content-sim-test
cd ..
nix run ./abc#validate-design-bundle
system="$(nix eval --impure --raw --expr builtins.currentSystem)"
nix build "./abc#checks.${system}.git-cliff-config" --print-build-logs
nix build "./abc#checks.${system}.source-bundle-corpus" --print-build-logs
just nix-format-check
```

Expected: all commands exit 0; audit family problem set remains empty.

- [ ] **Step 6: Commit bundles, registry, and reports atomically**

```bash
git add abc/docs/evidence/adr-runs \
  abc/docs/adr/adr-evidence.edn \
  abc/docs/reports/adr-claim-migration-inventory.json \
  abc/docs/reports/adr-evidence-migration.json
git commit -m "docs(adr): bind foundation evidence artifacts"
```

### Task 9: Plan-1 handoff gate

**Files:**
- Verify only; no source changes expected.

**Interfaces:**
- Produces a clean audit-mode checkpoint for plans 2–5.

- [ ] **Step 1: Run the shared migration gate**

Run: `just validate-migration`

Expected: PASS. If this broad gate reveals unrelated pre-existing worktree
failures, record the exact command/output and do not alter unrelated code.

- [ ] **Step 2: Verify shared artifacts and ownership**

Assert:

- baseline remains 146 rows and matches its ledger header hash;
- 35 foundation baseline rows have dispositions/resulting IDs;
- every foundation claim has compatible passing evidence;
- `nix run ./abc#validate-design-bundle` exits 0 without Git history;
- `git-cliff-config` passes only through its synthetic-repo check;
- `design-bundle-operational.json` has one owner (this plan);
- governance remains audit mode and ADR 0034 remains Proposed;
- `git status --short` is empty.

- [ ] **Step 3: Publish the handoff facts (no commit unless a tracked handoff file is requested)**

Report the Stage A/Stage B audit counts, capture revision, shared bundle path,
canonical bundle hash, and any later-family claims authorized to join the
shared operational bundle. Do not offer to switch enforcement.

## Self-review results

- Spec coverage: shared baseline/ledger, disposition inventory, both exact
  non-acceptance headings, hermetic Git-cliff split, lifecycle assignments,
  every foundation criterion correction, two-stage clean capture, audit
  deltas, and cross-plan bundle ownership are assigned above.
- Placeholder scan: no implementation step uses TBD/TODO or delegates an
  unspecified “similar” change.
- Type consistency: all later tasks consume the Task 1 migration-state API;
  descriptor and bundle stems/observation keys are identical across Tasks 7–9.
- Deliberate migration-mode boundary: incomplete non-foundation ledger coverage
  is allowed only with `require-complete? false` so sequential family commits
  are possible. Plan 5 first proves complete mode after all family dispositions
  land. Plan 6 repeats that proof while assembling the atomic enforcement
  candidate and remains the sole audit-to-enforce authority.
