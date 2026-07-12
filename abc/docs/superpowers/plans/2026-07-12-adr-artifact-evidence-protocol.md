# ADR Artifact-Backed Evidence Protocol Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Replace self-certified inline ADR observations with hashed evidence bundles, parse stable claim headers, and make audit-mode governance report the complete typed-evidence migration inventory.

**Architecture:** Keep Markdown and lifecycle parsing in `abc.tools.adr`; add a focused `abc.tools.adr-evidence-bundle` module for immutable bundle/schema/path/input validation; make `abc.tools.adr-evidence` own registry-to-claim joins and predicate evaluation. `abc.tools.adr-governance` remains the single aggregate command and keeps legacy mode unchanged while audit mode gains typed-evidence diagnostics.

**Tech Stack:** Clojure 1.12, JSON Schema 2020-12, EDN, RFC 8785/JCS SHA-256, `clojure.test`, `clojure.test.check`, Kaocha, Nix.

## Global Constraints

- Implement `docs/superpowers/specs/2026-07-12-adr-artifact-backed-evidence-enforcement-design.md` exactly.
- This plan implements the protocol and audit inventory only. It does not migrate the 146 historical criteria or switch Nix to enforcement.
- Retire the existing inline `:observed`/`:inputs` contract; do not support old and new entry shapes concurrently.
- Evidence bundles reject every non-integer JSON number and integers outside `[-9007199254740991, 9007199254740991]` recursively.
- Benchmark observations use scaled integers and one of `nanoseconds`, `bytes`, `count`, or `parts-per-million`.
- Legacy governance mode retains the pre-migration Markdown/path behavior.
- Audit mode accumulates typed problems and exits zero; enforce mode exits nonzero on the same problems.
- Keep `governance-as-of.edn` as the single explicit repository evaluation epoch; never read the wall clock.
- Treat absent or unparseable required external-evidence dates as invalid artifacts; malformed dates never disable expiry.
- Ordinary deterministic Clojure owns runtime validation. Logic/solver tooling remains review-only.
- Executable capture requires a clean Git worktree and has no dirty-tree override in version 1.
- Before execution, integrate the active Phase 5 and simulation/replay branches and require a clean worktree. Do not overwrite their uncommitted files or repair simulation-owned lint failures in this branch.
- Every commit runs its focused tests. The final gate is root `just validate-migration`; build the actual ADR governance and focused-test derivations as specified below.

---

## File Structure

- `abc/schemas/adr-evidence-run.schema.json` — closed executable run-bundle contract.
- `abc/schemas/adr-external-evidence.schema.json` — closed external-authority bundle contract.
- `ab-validator/data/abc-schemas/nix-schemas/adr-evidence-run.schema.json` — executable-bundle schema mirror.
- `ab-validator/data/abc-schemas/nix-schemas/adr-external-evidence.schema.json` — external-bundle schema mirror.
- `abc/src/abc/tools/adr.clj` — criterion and claim-header parsing; no bundle reads.
- `abc/src/abc/tools/path_containment.clj` — shared lexical and real-path containment primitive for legacy citations and evidence bundles.
- `abc/test/abc/tools/path_containment_test.clj` — traversal and symlink-escape characterization tests shared by both callers.
- `abc/src/abc/tools/adr_evidence_bundle.clj` — bundle loading, schema validation, safe-number walk, canonical hash, contained paths, input profiles, and root-cause diagnostics.
- `abc/src/abc/tools/adr_evidence.clj` — new artifact-backed registry contract, claim coverage, compatibility, observation selection, and typed predicates.
- `abc/src/abc/tools/adr_governance.clj` — aggregate Markdown plus typed-evidence problems.
- `abc/src/abc/tools/adr_evidence_capture.clj` — execute an argv vector and capture a deterministic bundle with derived input hashes.
- `abc/src/abc/tools/adr_evidence_inventory.clj` — deterministic audit inventory writer for follow-up migration plans.
- `abc/test/abc/tools/adr_evidence_bundle_test.clj` — bundle and input-profile tests.
- `abc/test/abc/tools/adr_evidence_test.clj` — replacement registry/join/predicate tests; no inline-observation assertions remain.
- `abc/test/abc/tools/adr_test.clj` — claim-header parser and corpus inventory tests.
- `abc/test/abc/tools/adr_governance_test.clj` — aggregate audit/enforce tests.
- `abc/docs/reports/adr-evidence-migration.json` — audit output with lifecycle and typed-claim problems.
- `abc/docs/reports/adr-claim-migration-inventory.json` — 26-ADR/146-criterion review inventory.

### Task 0: Reconcile the stacked branches and prove the execution baseline

**Files:** No planned source edits.

**Interfaces:**
- Consumes: integrated `main`, `feat/adr-remediation-foundation`, and `feat/rights-containment` commits.
- Produces: one clean implementation branch containing audit governance and rights containment without overwriting Phase 5 or simulation/replay work.

- [ ] **Step 1: Inspect integration state**

Run from the monorepo root:

```bash
git status --short --branch
git worktree list
git log --oneline --decorate -12
```

Expected: the implementation worktree is clean; active external worktrees are either merged or explicitly still isolated. If `main` contains uncommitted files, do not merge or stash them—STOP and coordinate integration.

- [ ] **Step 2: Integrate current main without rebasing published evidence commits**

```bash
git merge --no-edit main
```

Expected: merge succeeds without changes to another worktree's uncommitted state.

- [ ] **Step 3: Verify the inherited governance and rights checkpoint**

```bash
cd abc
bin/kaocha --focus abc.tools.source-assertion-test \
  --focus abc.tools.adr-evidence-test \
  --focus abc.tools.adr-test \
  --focus abc.tools.adr-governance-test \
  --focus abc.tools.metadata-record-test \
  --focus abc.tools.materialize-publication-test
```

Expected: zero failures and zero errors.

```bash
cd ..
nix build ./abc#checks.x86_64-linux.clj-kondo
```

Expected: exit 0. If the only failures remain the previously observed unresolved `git/root/work` symbols in `abc/test/abc/sim/audit_sim_test.clj`, STOP and wait for the simulation/replay integration rather than editing that file here.

---

### Task 1: Add closed executable and external evidence schemas

**Files:**
- Create: `abc/schemas/adr-evidence-run.schema.json`
- Create: `abc/schemas/adr-external-evidence.schema.json`
- Create: `ab-validator/data/abc-schemas/nix-schemas/adr-evidence-run.schema.json`
- Create: `ab-validator/data/abc-schemas/nix-schemas/adr-external-evidence.schema.json`
- Modify: `abc/schemas/schema-contracts.json`
- Modify: `ab-validator/data/abc-schemas/schema-contracts.json`
- Modify: `scripts/abc_schema_contracts.py`
- Modify: `abc/src/abc/tools/validate_design_bundle.clj`
- Modify: `abc/src/abc/tools/schema.clj`
- Modify: `abc/test/abc/tools/validate_design_bundle_test.clj`

**Interfaces:**
- Consumes: JSON Schema 2020-12 and the existing schema-contract generator.
- Produces: schemas with IDs `https://w3id.org/abc/schemas/adr-evidence-run.schema.json` and `https://w3id.org/abc/schemas/adr-external-evidence.schema.json`, both version `0.1.0`.

- [ ] **Step 1: Write failing schema-registration tests**

Add `validate-json-schemas-includes-adr-evidence-contracts-test` to `abc/test/abc/tools/validate_design_bundle_test.clj`. The test writes one valid executable bundle and one valid external bundle to temporary files, invokes `validate-json-schemas!`, and asserts that changing either `schema_version` or adding `"unexpected": true` produces a validation error. It also asserts that `retrieved_at` or `review_after` values with slash separators, impossible calendar dates, or missing required values are rejected. Use these exact valid values:

```clojure
{"schema_version" "abc-adr-evidence-run-v1"
 "producer" {"tool" "bin/kaocha"
             "command" "bin/kaocha --focus abc.tools.adr-evidence-test"
             "revision" "0000000000000000000000000000000000000000"}
 "input_profile" {"kind" "clojure-test-v1"
                  "roots" ["abc.tools.adr-evidence-test"]
                  "explicit" []}
 "inputs" {"test/abc/tools/adr_evidence_test.clj"
           "sha256:0000000000000000000000000000000000000000000000000000000000000000"}
 "observations" {"contract" {"value" true
                              "details" {"tests" 4
                                         "failures" 0
                                         "errors" 0}}}}
```

```clojure
{"schema_version" "abc-adr-external-evidence-v1"
 "source_url" "https://example.invalid/authority"
 "retrieved_at" "2026-07-12"
 "review_after" "2027-07-12"
 "summary" {"path" "docs/evidence/authority-summary.md"
            "hash" "sha256:1111111111111111111111111111111111111111111111111111111111111111"}
 "input_profile" {"kind" "external-authority-v1" "explicit" []}
 "inputs" {}
 "observations" {"source-contract" {"value" "documented" "details" {}}}}
```

- [ ] **Step 2: Run the test and verify RED**

```bash
cd abc
bin/kaocha --focus abc.tools.validate-design-bundle-test/validate-json-schemas-includes-adr-evidence-contracts-test
```

Expected: FAIL because the two schemas are absent from `validate-json-schemas!`.

- [ ] **Step 3: Implement both schemas**

Both top-level schemas use `additionalProperties: false`. Define a recursive `$defs/safeValue` permitting only null, boolean, string, safe-range integer, arrays of `safeValue`, and objects whose values are `safeValue`. Do not include JSON Schema's `number` type.

The executable schema requires exactly:

```json
[
  "schema_version",
  "producer",
  "input_profile",
  "inputs",
  "observations"
]
```

`input_profile.kind` is `clojure-test-v1` or `repo-files-v1`; `roots` and `explicit` are arrays of unique non-empty strings. `inputs` is a string-keyed object whose values match `^sha256:[0-9a-f]{64}$`. Observation keys match `^[a-z0-9][a-z0-9-]*$`; each observation is closed and requires `value`, with optional `details` using `$defs/safeValue`.

The external schema requires exactly:

```json
[
  "schema_version",
  "source_url",
  "retrieved_at",
  "review_after",
  "summary",
  "input_profile",
  "inputs",
  "observations"
]
```

Dates require both `format: date` and
`pattern: "^[0-9]{4}-[0-9]{2}-[0-9]{2}$"`; `summary` is closed and requires
`path` plus a formatted hash; `input_profile.kind` is const
`external-authority-v1`. Schema tests must prove that both lexical mistakes
such as `2026/07/12` and impossible dates such as `2026-13-40` are rejected by
the repository's configured validator.
Configure the shared NetworkNT registry with format assertions enabled; the
lexical pattern remains mandatory and Task 3 still parses dates independently
as defense in depth.

- [ ] **Step 4: Register and mirror schemas mechanically**

Add both filenames to `SCHEMA_FILES` in `scripts/abc_schema_contracts.py`, add them to `validate-json-schemas!`, copy the two canonical schema files byte-for-byte into `ab-validator/data/abc-schemas/nix-schemas/`, and regenerate both manifests:

```bash
cp abc/schemas/adr-evidence-run.schema.json \
  ab-validator/data/abc-schemas/nix-schemas/adr-evidence-run.schema.json
cp abc/schemas/adr-external-evidence.schema.json \
  ab-validator/data/abc-schemas/nix-schemas/adr-external-evidence.schema.json
(cd abc && python3 ../scripts/abc_schema_contracts.py --profile abc --write)
(cd ab-validator && python3 ../scripts/abc_schema_contracts.py \
  --profile ab-validator --write)
```

Verify:

```bash
cmp abc/schemas/adr-evidence-run.schema.json \
  ab-validator/data/abc-schemas/nix-schemas/adr-evidence-run.schema.json
cmp abc/schemas/adr-external-evidence.schema.json \
  ab-validator/data/abc-schemas/nix-schemas/adr-external-evidence.schema.json
```

Expected: both commands exit 0 with no output.

- [ ] **Step 5: Add safe-number negative cases and run GREEN**

Extend the test with nested `0.5`, `9007199254740992`, and `-9007199254740992` values under both `value` and `details`; each must be rejected. Then run:

```bash
cd abc
bin/kaocha --focus abc.tools.validate-design-bundle-test/validate-json-schemas-includes-adr-evidence-contracts-test
cd ..
(cd abc && python3 ../scripts/abc_schema_contracts.py --profile abc)
(cd ab-validator && python3 ../scripts/abc_schema_contracts.py \
  --profile ab-validator)
```

Expected: test passes and schema contracts report no drift.

- [ ] **Step 6: Commit**

```bash
git add abc/schemas/adr-evidence-run.schema.json \
  abc/schemas/adr-external-evidence.schema.json \
  abc/schemas/schema-contracts.json \
  ab-validator/data/abc-schemas/nix-schemas/adr-evidence-run.schema.json \
  ab-validator/data/abc-schemas/nix-schemas/adr-external-evidence.schema.json \
  ab-validator/data/abc-schemas/schema-contracts.json \
  scripts/abc_schema_contracts.py \
  abc/src/abc/tools/schema.clj \
  abc/src/abc/tools/validate_design_bundle.clj \
  abc/test/abc/tools/validate_design_bundle_test.clj
git commit -m "feat(adr): add immutable evidence bundle schemas"
```

---

### Task 2: Parse exact claim headers without changing legacy governance

**Files:**
- Modify: `abc/src/abc/tools/adr.clj`
- Modify: `abc/test/abc/tools/adr_test.clj`
- Modify: `abc/docs/adr/README.md`

**Interfaces:**
- Consumes: Acceptance Criterion bullet bodies from `criterion-bodies`.
- Produces: each parsed ADR has `:criteria`, a vector of `{:criterion-index int :body string :claim-id string-or-nil :claim-kind keyword-or-nil}`, plus strict claim-header problems.

- [ ] **Step 1: Write failing parser tests**

Add tests for this exact valid criterion:

```markdown
- **ADR-0042-C1 — structural-invariant:** A multiline claim whose continuation
  remains part of the same criterion.
```

Assert parsed `:claim-id` is `"ADR-0042-C1"`, `:claim-kind` is `:structural-invariant`, and the multiline body is preserved. Add distinct negative assertions for:

```text
missing bold prefix                         -> :missing-claim-header
ADR-42-C1                                  -> :malformed-claim-header
ADR-0041-C1 inside ADR 0042                -> :claim-adr-mismatch
ADR-0042-C0                                -> :malformed-claim-header
ADR-0042-C1 — unknown-kind                 -> :unknown-claim-kind
duplicate ADR-0042-C1 in two criteria      -> :duplicate-claim-id
```

Also assert Proposed criteria may omit claim headers without a problem, but a malformed header-like prefix on a Proposed criterion is reported.

- [ ] **Step 2: Run RED**

```bash
cd abc
bin/kaocha --focus abc.tools.adr-test --fail-fast
```

Expected: FAIL because parsed ADRs have no `:criteria` or claim problems.

- [ ] **Step 3: Implement exact parsing**

Add:

```clojure
(def claim-kinds
  #{:structural-invariant :fixture-behavior :corpus-behavior
    :performance-bound :external-semantics :implementation-agreement
    :domain-interpretation :operational-behavior})

(def ^:private claim-header-pattern
  #"^\*\*(ADR-([0-9]{4})-C([1-9][0-9]*) — ([a-z]+(?:-[a-z]+)*)):\*\*(?:\s|$)")
```

Parse every criterion once. A criterion with no `**ADR-` prefix is missing only when its ADR is Accepted. A criterion beginning `**ADR-` but not matching is malformed for every status. Convert the kind token with `keyword`, compare its ADR digits to `:num`, and detect duplicate IDs across `validate-adrs`, not only within one file.

Add all six claim problem kinds to `audit-only-problem-kinds` so `validate-adrs-legacy` remains unchanged during migration.

- [ ] **Step 4: Update the ADR README**

Document the exact claim syntax, eight kinds, Accepted coverage rule, and Proposed optional-header rule immediately after the Acceptance Criteria edit policy.

- [ ] **Step 5: Run GREEN and prove legacy compatibility**

```bash
cd abc
bin/kaocha --focus abc.tools.adr-test
clojure -M:abc/adr-governance --mode legacy
```

Expected: zero test failures; legacy command exits 0. Audit mode may now report missing claim headers only after Task 5 wires strict typed validation into the aggregate command.

- [ ] **Step 6: Commit**

```bash
git add abc/src/abc/tools/adr.clj abc/test/abc/tools/adr_test.clj abc/docs/adr/README.md
git commit -m "feat(adr): parse stable typed claim headers"
```

---

### Task 3: Validate bundles, contained paths, and mechanically derived inputs

**Files:**
- Create: `abc/src/abc/tools/path_containment.clj`
- Create: `abc/test/abc/tools/path_containment_test.clj`
- Modify: `abc/src/abc/tools/adr.clj`
- Modify: `abc/test/abc/tools/adr_test.clj`
- Create: `abc/src/abc/tools/adr_evidence_bundle.clj`
- Create: `abc/test/abc/tools/adr_evidence_bundle_test.clj`

**Interfaces:**
- Produces:
  - `load-bundle repo-root artifact-path -> {:value map :canonical-hash string} | {:problems vector}`
  - `derive-minimum-inputs repo-root input-profile -> sorted-set<string>`
  - `validate-bundle repo-root artifact-path expected-hash affected-claim-ids -> {:bundle map-or-nil :problems vector}`
  - `observation bundle observation-key -> value-or-nil`

- [ ] **Step 1: Write failing contained-path and hash tests**

Use a temporary repository with `src/example/core.clj`, `src/example/helper.clj`, `test/example/core_test.clj`, and a fixture file. Write bundles through `abc.tools.json/write-deterministic-json-file!`. Assert:

- the canonical artifact hash is `hash/format-sha256 (hash/sha256-json-jcs value)`;
- wrong expected hash yields one `:artifact-hash-mismatch` carrying sorted affected claim IDs;
- missing bundle yields one `:missing-evidence-artifact`;
- `../`, absolute, malformed, and symlink-escape artifact/input paths yield the corresponding path problem;
- missing observation lookup returns nil without throwing.

Add characterization cases around the existing legacy ADR evidence-path
behavior. Traversal and symlink-escape inputs must yield the same problem kinds
before and after extraction to the shared containment namespace.

- [ ] **Step 2: Write failing input-profile tests**

The test namespace requires `example.core`, which requires `example.helper`. For:

```clojure
{:kind "clojure-test-v1"
 :roots ["example.core-test"]
 :explicit ["fixtures/example.json"]}
```

assert the minimum set is exactly:

```clojure
#sorted-set{"fixtures/example.json"
            "src/example/core.clj"
            "src/example/helper.clj"
            "test/example/core_test.clj"}
```

Assert cycles terminate, `:refer`, `:as`, and vector libspecs resolve, reader evaluation is disabled, missing namespaces produce `:missing-evidence-input`, and omitting helper from bundle `inputs` produces `:missing-evidence-input`.

Also prove the boundary of mechanical derivation: a `require` evaluated outside
the first `ns` form and a runtime-read fixture are not discovered
automatically, and both become covered only when their repository paths are
listed in `input_profile.explicit`.

- [ ] **Step 3: Run RED**

```bash
cd abc
bin/kaocha --focus abc.tools.adr-evidence-bundle-test
```

Expected: ERROR because the namespace does not exist.

- [ ] **Step 4: Extract shared path containment and implement schema validation**

Move the lexical normalization, traversal, and real-path/symlink-escape checks
from the private `abc.tools.adr/evidence-path-state` implementation into
`abc.tools.path-containment`. Keep the shared function policy-free: it returns
stable path states and resolved paths, while each caller maps states to its own
problem values. Rewire the legacy ADR caller and prove its characterization
tests remain green. Do not retain a second containment implementation in the
bundle namespace.

Use that shared primitive to allow only repository-relative bundle and input
paths below the repository root. Load the correct cached schema based on
`schema_version`; unknown versions produce `:invalid-evidence-artifact`.
Accumulate schema errors into one artifact-root problem with
`:errors-humanized` and sorted `:affected-claim-ids`. Independently parse both
required external dates as calendar dates; a missing or unparseable value is
`:invalid-evidence-artifact`, even if schema `format` assertions are disabled
or bypassed by a focused unit test.

Compute the canonical hash from the parsed JSON value, not file bytes:

```clojure
(hash/format-sha256 (hash/sha256-json-jcs value))
```

- [ ] **Step 5: Implement Clojure namespace closure**

For `clojure-test-v1`, translate namespace symbols with dots to `/` and hyphens to `_`; resolve roots under `test/` then `src/`. Read only the first form using `clojure.lang.LineNumberingPushbackReader` with `*read-eval* false`. Extract lib symbols from the `:require` clause, recursively resolve repository-local namespaces, and use a sorted visited set for deterministic cycle termination. Ignore JDK/Clojure/dependency namespaces that have no repository file. A declared root that has no repository file is an error.

For `repo-files-v1` and `external-authority-v1`, the minimum set is `input_profile.explicit`; external bundles additionally require `summary.path`.

- [ ] **Step 6: Validate all bound inputs and safe values**

Require every derived minimum path to appear in `inputs`; allow extras. Recompute each current file hash with `hash/sha256-file`. Group identical missing/hash-drift root causes once and attach sorted affected claim IDs. Walk the parsed value recursively as defense in depth and reject non-integers and unsafe integers even if schema validation is bypassed in a unit test.

- [ ] **Step 7: Run GREEN**

```bash
cd abc
bin/kaocha --focus abc.tools.path-containment-test \
  --focus abc.tools.adr-test \
  --focus abc.tools.adr-evidence-bundle-test
```

Expected: zero failures and zero errors.

- [ ] **Step 8: Commit**

```bash
git add abc/src/abc/tools/adr_evidence_bundle.clj \
  abc/test/abc/tools/adr_evidence_bundle_test.clj \
  abc/src/abc/tools/path_containment.clj \
  abc/test/abc/tools/path_containment_test.clj \
  abc/src/abc/tools/adr.clj abc/test/abc/tools/adr_test.clj
git commit -m "feat(adr): validate hashed evidence bundles and input profiles"
```

---

### Task 4: Replace the inline registry contract with artifact-backed joins

**Files:**
- Modify: `abc/src/abc/tools/adr_evidence.clj`
- Rewrite: `abc/test/abc/tools/adr_evidence_test.clj`
- Modify: `abc/docs/adr/adr-evidence.edn`

**Interfaces:**
- Consumes: parsed claims from Task 2 and bundle validation from Task 3.
- Produces:
  - `evaluate-result expected observed -> {:status :pass|:fail|:type-error}`
  - `validate-registry {:repo-root path :claims vector :registry map :matrix map :as-of string} -> vector<problem>`

- [ ] **Step 1: Rewrite the valid-entry fixture before implementation**

Replace the existing inline fixture with:

```clojure
{:claim-id "ADR-0034-C1"
 :claim-kind :structural-invariant
 :evidence-kind :structural-test
 :artifact-path "docs/evidence/adr-runs/governance.json"
 :artifact-hash input-hash
 :observation-key "typed-evidence-contract"
 :expected {:operator := :value true}}
```

Delete assertions that mutate `[:observed :value]` and `[:observed :inputs :source]`. Add assertions that any entry containing `:observed`, `:inputs`, or `:verdict` yields `:forbidden-inline-evidence-value`.

- [ ] **Step 2: Add failing join/coverage tests**

With `adr-evidence-bundle/validate-bundle` redefined to return a valid in-memory bundle, assert:

- one matching claim and entry passes;
- an Accepted claim with no entry yields `:missing-claim-evidence`;
- duplicate exact entries yield `:duplicate-evidence-entry`;
- registry versus Markdown kind mismatch yields `:claim-kind-mismatch`;
- unknown and incompatible kinds remain distinct;
- a missing observation yields `:missing-observation` for that claim;
- two valid corroborating entries pass;
- one passing plus one invalid corroborating entry still reports the invalid entry;
- a shared artifact hash failure is emitted once with both affected IDs.

- [ ] **Step 3: Add typed predicate tests**

Assert ordered operators accept only safe integers, equality accepts any schema-valid same-type value, `:contains` requires an array, and `:set=` requires two arrays. Incomparable operands return `{:status :type-error}` and become `:predicate-type-mismatch`; comparable false results become `:predicate-failed`. For `:benchmark`, assert an integer observation without an allowed `details.unit` is `:invalid-evidence-artifact`.

Retain the `test.check` small-integer agreement property using `evaluate-result` status/value.

- [ ] **Step 4: Run RED**

```bash
cd abc
bin/kaocha --focus abc.tools.adr-evidence-test
```

Expected: FAIL because the implementation still expects inline observations and the old function signature.

- [ ] **Step 5: Implement the new registry validator**

Reject unknown registry keys using the exact key set:

```clojure
#{:claim-id :claim-kind :evidence-kind :artifact-path
  :artifact-hash :observation-key :expected}
```

Group claims by ID, entries by claim ID, and artifact references by `[artifact-path artifact-hash]`. Validate each artifact once with the union of affected claim IDs. Resolve each entry's named observation only after the artifact passes root validation. External expiry comes from the external bundle's `review_after`, compared to explicit `as-of`.

- [ ] **Step 6: Keep the checked-in registry structurally empty**

Leave `docs/adr/adr-evidence.edn` as `{:entries []}` in this task. The current corpus must report missing coverage in audit mode after Task 5; do not add manufactured entries to make unit tests green.

- [ ] **Step 7: Run GREEN and prove the old contract is gone**

```bash
cd abc
bin/kaocha --focus abc.tools.adr-evidence-test \
  --focus abc.tools.adr-evidence-bundle-test
rg -n ':observed|:inputs|:stale-inputs' \
  src/abc/tools/adr_evidence.clj test/abc/tools/adr_evidence_test.clj
```

Expected: tests pass; `rg` finds only negative assertions/documentation for forbidden inline keys and no `:stale-inputs` implementation path.

- [ ] **Step 8: Commit**

```bash
git add abc/src/abc/tools/adr_evidence.clj \
  abc/test/abc/tools/adr_evidence_test.clj \
  abc/docs/adr/adr-evidence.edn
git commit -m "feat(adr): replace inline observations with artifact-backed evidence"
```

---

### Task 5: Make governance the aggregate audit gate

**Files:**
- Modify: `abc/src/abc/tools/adr_governance.clj`
- Modify: `abc/test/abc/tools/adr_governance_test.clj`
- Modify: `abc/flake.nix`
- Regenerate: `abc/docs/reports/adr-evidence-migration.json`

**Interfaces:**
- Consumes: strict ADR problems, parsed claims, registry, matrix, as-of date, and bundle validator.
- Produces: stable aggregate audit/enforce results; legacy output remains ADR-only.

- [ ] **Step 1: Write failing aggregate tests**

Redefine ADR validation to return one lifecycle problem and typed evidence validation to return one evidence problem. Assert audit returns both in stable order and exit 0; enforce returns both and exit 1; legacy invokes no typed evidence function and exits according to legacy ADR problems only.

Add a repository fixture with one Accepted criterion and empty registry. Assert audit contains `:missing-claim-evidence` plus any lifecycle problem, and its JSON report includes `file`, `criterion-index`, `claim-id`, and `kind`.

- [ ] **Step 2: Run RED**

```bash
cd abc
bin/kaocha --focus abc.tools.adr-governance-test
```

Expected: FAIL because governance requires only `adr` and `json`.

- [ ] **Step 3: Implement aggregate validation**

Require `abc.tools.adr-evidence`. For audit/enforce:

1. parse the ADR corpus once with `adr/parse-all`;
2. compute strict ADR problems with `adr/validate-adrs`;
3. collect parsed claims with ADR file/status/criterion index attached;
4. call `adr-evidence/validate-registry` with explicit repo root, matrix, registry, and as-of;
5. concatenate ADR problems followed by typed problems, each internally stable-sorted.

Legacy continues to call only `validate-repository-legacy` and never loads artifacts.

- [ ] **Step 4: Keep Nix in audit mode and strengthen its marker**

Do not change `--mode audit`. Change the result marker to:

```text
ADR lifecycle, dependency, claim, artifact, freshness, and evidence audit completed.
```

Update `nix-governance-check-selects-audit-mode-test` to require both the audit argument and marker.

- [ ] **Step 5: Regenerate the migration report**

```bash
cd abc
clojure -M:abc/adr-governance --mode audit \
  --report docs/reports/adr-evidence-migration.json
```

Expected: exit 0. The report includes existing lifecycle problems, missing claim headers for every unmigrated Accepted criterion, and no false `ok: true`.

- [ ] **Step 6: Add the exact audit inventory assertion**

Update `current-repository-audit-inventory-is-explicit` to assert:

```clojure
(= 26 accepted-adr-count)
(= 146 accepted-criterion-count)
(= 146 missing-claim-header-count)
```

If any active branch legitimately changed the Accepted inventory before execution, update the spec and plan with the new measured counts before changing this assertion.

- [ ] **Step 7: Run GREEN and Nix gate**

```bash
cd abc
bin/kaocha --focus abc.tools.adr-test \
  --focus abc.tools.adr-evidence-test \
  --focus abc.tools.adr-evidence-bundle-test \
  --focus abc.tools.adr-governance-test
cd ..
nix build ./abc#checks.x86_64-linux.adr-governance
```

Expected: focused tests pass; Nix audit derivation exits 0 despite reported migration problems.

- [ ] **Step 8: Commit**

```bash
git add abc/src/abc/tools/adr_governance.clj \
  abc/test/abc/tools/adr_governance_test.clj \
  abc/test/abc/tools/adr_test.clj \
  abc/flake.nix \
  abc/docs/reports/adr-evidence-migration.json
git commit -m "feat(adr): aggregate artifact evidence in governance audit"
```

---

### Task 6: Add the deterministic executable evidence capture tool

**Files:**
- Create: `abc/src/abc/tools/adr_evidence_capture.clj`
- Create: `abc/test/abc/tools/adr_evidence_capture_test.clj`
- Modify: `abc/deps.edn`

**Interfaces:**
- Consumes: a capture descriptor EDN and `derive-minimum-inputs` from Task 3.
- Produces CLI alias `:abc/adr-evidence-capture` and one schema-valid executable JSON bundle.

- [ ] **Step 1: Write failing capture tests**

In a temporary Git repository, create one Clojure root namespace and this descriptor:

```clojure
{:schema-version "abc-adr-evidence-capture-v1"
 :tool "sh"
 :argv ["sh" "-c" "exit 0"]
 :input-profile {:kind "repo-files-v1"
                 :roots []
                 :explicit ["src/example/core.clj"]}
 :observation-key "command-passed"}
```

Assert the tool:

- executes the argv vector directly through `ProcessBuilder`, never through an implicitly constructed shell;
- writes `schema_version: abc-adr-evidence-run-v1`;
- records the repository `git rev-parse HEAD` value;
- writes `value: true`, `details.exit_code: 0`, and input hashes derived from the profile;
- refuses before command execution when `git status --porcelain
  --untracked-files=all` is non-empty, with no partial output;
- refuses after a command dirties the repository and before hashing or writing
  the bundle, with no partial output;
- emits byte-identical JSON across two successful clean-tree runs at the same
  commit, writing outputs outside the temporary repository (or removing the
  first output before the second cleanliness check);
- writes `value: false` and exit code 7 for `argv ["sh" "-c" "exit 7"]`, then exits 1 itself after the bundle is safely written;
- rejects descriptor keys outside the exact set `#{:schema-version :tool :argv :input-profile :observation-key}`.

- [ ] **Step 2: Run RED**

```bash
cd abc
bin/kaocha --focus abc.tools.adr-evidence-capture-test
```

Expected: ERROR because the namespace does not exist.

- [ ] **Step 3: Implement capture as values plus a thin CLI**

Expose:

```clojure
(capture! {:repo-root path :descriptor map :output path})
;; => {:bundle map :exit-code int :output java.io.File}
```

Validate the descriptor, then require `git status --porcelain
--untracked-files=all` to produce no output before executing the evidence
command. Refuse a dirty tree with exit 2 and do not offer an override in version
1. Run `ProcessBuilder` with the exact argv vector and repository root as its
working directory. Consume stdout and
stderr concurrently to avoid process-pipe deadlock, but do not place their
machine- or timing-sensitive contents in the bundle. The deterministic command
display is `(string/join " " (map pr-str argv))`. Repeat the cleanliness check
after the command and before deriving inputs or writing output; a command that
dirties the repository is invalid evidence capture. Derive and hash all profile
inputs only after that check, obtain revision via
`git rev-parse --verify HEAD`, write through
`json/write-deterministic-json-file!`, and validate the result with the Task 3
bundle validator before returning.

- [ ] **Step 4: Add CLI alias**

Add:

```clojure
:abc/adr-evidence-capture
{:main-opts ["-m" "abc.tools.adr-evidence-capture"]}
```

The exact invocation is:

```bash
clojure -M:abc/adr-evidence-capture -- \
  --descriptor PATH --output PATH
```

Invalid descriptors or validation failures exit 2 without writing a partial
file. A dirty worktree is a validation failure; before execution the command is
not started, and after execution no bundle is written. A completed, clean-tree
failing command writes its bundle and exits 1. A passing command writes its
bundle and exits 0. Document that capture must remain clean across execution
and that the output should normally be outside the repository until it is
reviewed and committed.

- [ ] **Step 5: Run GREEN and commit**

```bash
cd abc
bin/kaocha --focus abc.tools.adr-evidence-capture-test \
  --focus abc.tools.adr-evidence-bundle-test
cd ..
git add abc/src/abc/tools/adr_evidence_capture.clj \
  abc/test/abc/tools/adr_evidence_capture_test.clj abc/deps.edn
git commit -m "feat(adr): capture deterministic executable evidence bundles"
```

---

### Task 7: Generate the review inventory for decomposed corpus migration

**Files:**
- Create: `abc/src/abc/tools/adr_evidence_inventory.clj`
- Create: `abc/test/abc/tools/adr_evidence_inventory_test.clj`
- Modify: `abc/deps.edn`
- Create: `abc/docs/reports/adr-claim-migration-inventory.json`

**Interfaces:**
- Produces CLI alias `:abc/adr-evidence-inventory` and deterministic JSON with every Accepted ADR criterion, current evidence-path citations, suggested family, and migration disposition unset as data rather than prose.

- [ ] **Step 1: Write failing inventory tests**

For a two-ADR/three-criterion fixture, assert the exact summary fields and the
first criterion row:

```clojure
{"schema_version" "abc-adr-claim-migration-inventory-v1"
 "accepted_adr_count" 2
 "accepted_criterion_count" 3
 "families" {"foundation-runtime-identity" 1
             "schema-rdf-tei" 0
             "temporal-person-ingest" 0
             "parser-ir-publication" 1
             "diagrams-governance" 0
             "unclassified" 1}
 "criteria" [{"adr" 1
               "file" "0001-one.md"
               "criterion_index" 0
               "body" "Criterion text."
               "claim_id" nil
               "claim_kind" nil
               "evidence_paths" ["test/example.clj"]
               "family" "foundation-runtime-identity"
               "disposition" nil}]}
```

Assert criteria sort by ADR then criterion index and output is byte-identical
across two runs.

- [ ] **Step 2: Run RED**

```bash
cd abc
bin/kaocha --focus abc.tools.adr-evidence-inventory-test
```

Expected: ERROR because the namespace does not exist.

- [ ] **Step 3: Implement deterministic classification**

Use an explicit map in the namespace, not filename heuristics:

```clojure
{1 "foundation-runtime-identity" 2 "parser-ir-publication"
 6 "schema-rdf-tei" 7 "parser-ir-publication"
 8 "foundation-runtime-identity" 9 "foundation-runtime-identity"
 10 "foundation-runtime-identity" 11 "foundation-runtime-identity"
 12 "schema-rdf-tei" 13 "schema-rdf-tei" 14 "schema-rdf-tei"
 15 "temporal-person-ingest" 16 "temporal-person-ingest"
 17 "schema-rdf-tei" 18 "schema-rdf-tei"
 20 "temporal-person-ingest" 21 "temporal-person-ingest"
 22 "temporal-person-ingest" 23 "parser-ir-publication"
 24 "parser-ir-publication" 25 "parser-ir-publication"
 29 "diagrams-governance" 30 "parser-ir-publication"
 31 "diagrams-governance" 32 "parser-ir-publication"
 33 "foundation-runtime-identity"}
```

Unknown Accepted ADRs are `unclassified` and make the CLI exit 1 after writing the report, forcing an explicit plan assignment.

- [ ] **Step 4: Add CLI and generate the real inventory**

Add:

```clojure
:abc/adr-evidence-inventory
{:main-opts ["-m" "abc.tools.adr-evidence-inventory"]}
```

Run:

```bash
cd abc
clojure -M:abc/adr-evidence-inventory -- \
  --output docs/reports/adr-claim-migration-inventory.json
```

Expected: exit 0; report says 26 Accepted ADRs and 146 criteria, with no `unclassified` rows.

- [ ] **Step 5: Run GREEN and commit**

```bash
cd abc
bin/kaocha --focus abc.tools.adr-evidence-inventory-test
cd ..
git add abc/src/abc/tools/adr_evidence_inventory.clj \
  abc/test/abc/tools/adr_evidence_inventory_test.clj \
  abc/deps.edn \
  abc/docs/reports/adr-claim-migration-inventory.json
git commit -m "docs(adr): inventory typed-claim migration corpus"
```

---

### Task 8: Verify the protocol checkpoint and prepare family-plan handoff

**Files:**
- Possibly regenerate: `abc/schemas/schema-contracts.json`
- Possibly regenerate: `ab-validator/data/abc-schemas/schema-contracts.json`
- Possibly regenerate: `abc/docs/reports/adr-evidence-migration.json`
- Possibly regenerate: `abc/docs/reports/adr-claim-migration-inventory.json`

**Interfaces:**
- Produces: a reviewed audit-mode protocol checkpoint; follow-up plans consume `adr-claim-migration-inventory.json` and may be independently rejected.

- [ ] **Step 1: Run all focused protocol tests**

```bash
cd abc
bin/kaocha --focus abc.tools.adr-test \
  --focus abc.tools.path-containment-test \
  --focus abc.tools.adr-evidence-bundle-test \
  --focus abc.tools.adr-evidence-test \
  --focus abc.tools.adr-governance-test \
  --focus abc.tools.adr-evidence-capture-test \
  --focus abc.tools.adr-evidence-inventory-test \
  --focus abc.tools.validate-design-bundle-test/validate-json-schemas-includes-adr-evidence-contracts-test
```

Expected: zero failures and zero errors.

- [ ] **Step 2: Build the actual Nix gates**

```bash
nix build ./abc#checks.x86_64-linux.clj-nix-focused-tests
nix build ./abc#checks.x86_64-linux.adr-governance
nix build ./abc#checks.x86_64-linux.diagram-drift
```

Expected: every build exits 0. Run `nix build ./abc#checks.x86_64-linux.clj-kondo` as well after the simulation/replay prerequisite has merged; it must exit 0 before integration.

- [ ] **Step 3: Validate the complete design bundle in its environment**

```bash
cd abc
nix develop .#validation -c clojure -M:abc/validate-design-bundle
```

Expected final line: `design bundle validation ok`.

- [ ] **Step 4: Run the root gate**

```bash
cd ..
just validate-migration
```

Expected: exit 0 with all root and component evaluations successful.

- [ ] **Step 5: Confirm audit honesty**

```bash
cd abc
clojure -M:abc/adr-governance --mode audit \
  --report /tmp/adr-evidence-protocol-audit.json
clojure -M:abc/adr-governance --mode enforce
```

Expected: audit exits 0 with `ok: false`; enforce exits 1 on the same unmigrated corpus. A passing enforce command at this checkpoint is a blocker because it means typed coverage is not biting.

- [ ] **Step 6: Create follow-up plan inputs, not implementations**

Use the committed inventory to write five separate plans in later reviewed sessions:

1. foundation/runtime/identity claims;
2. schema/RDF/TEI claims;
3. temporal/person/ingest claims;
4. parser/IR/publication claims;
5. diagrams/governance, ADR 0034 self-certification, and enforcement.

Each plan must classify unsupported claims for scoped correction or demotion rather than inventing evidence.

- [ ] **Step 7: Commit regenerated artifacts or assert a clean worktree**

```bash
git status --short
```

If verification regenerated tracked artifacts, inspect and commit only those files with:

```bash
git commit -m "test(adr): verify artifact evidence protocol checkpoint"
```

If the worktree is already clean, do not create an empty verification commit.

## Plan Self-Review

- Spec coverage: bundle schemas, safe numerics, input profiles, inline-contract retirement, claim parsing, typed joins, error deduplication, audit aggregation, and migration inventory each have an owning task.
- Input-closure boundary: `clojure-test-v1` derives only the statically visible first-`ns`-form repository namespace graph. Runtime data, `load`, in-body `require`, and dynamic dependencies remain reviewable explicit inputs rather than being mislabeled as mechanically complete.
- Trust boundary: traversal and real-path checks have one shared implementation, and executable capture refuses provenance from dirty worktrees.
- Scope: the 146-criterion scientific review is deliberately decomposed into five follow-up plans; this plan ends at an honest failing enforcement checkpoint.
- Type consistency: `claim-id`, `claim-kind`, `artifact-path`, `artifact-hash`, `observation-key`, `input_profile`, and problem-kind names match the approved spec throughout.
- No runtime solver dependency is introduced.
