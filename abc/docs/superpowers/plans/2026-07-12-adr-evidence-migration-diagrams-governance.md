# Diagrams and Governance ADR Evidence Migration Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Migrate ADR 0029 and the structural scope of ADR 0031 to honest artifact-backed evidence after all ordinary ADR edits have settled.

**Architecture:** Split graph contracts, workflow fixture behavior, registry drift, and ADR policy fixtures into five narrow observations. ADR 0031 remains structural and does not self-certify complete typed-evidence coverage; ADR 0034 is the sole authority for the complete-corpus observation and enforcement transition. Commit all descriptors and Plan 1's registration template first, capture every bundle into one external staging directory from the same clean revision, install them together, and register them atomically through Plan 1's tested registrar.

**Tech Stack:** Clojure/Kaocha, Mermaid generated views, EDN registries, deterministic JSON evidence bundles, Nix.

## Global Constraints

- Require completion of plans 1–4; before Task 1 the only unresolved immutable baseline rows are the eight `diagrams-governance` rows.
- Use Plan 1's complete ledger validator, disposition-aware inventory, capture descriptors, hash-free entry templates, atomic registrar, and checked `nix/adr-family-clean.jq` predicate.
- Diagram bytes prove deterministic declared views, not semantic correctness of the declared architecture.
- Existing Accepted Decision/Hard Rule bodies remain byte-identical under plan 1's normative baseline; this plan changes criteria, lifecycle/relation headers, and generated views only.
- ADR 0031 owns structural governance behavior only. ADR 0034 alone owns complete typed-evidence coverage, its immutable pre-promotion snapshot, and the enforcement transition.
- Bind every runtime-read ADR, sidecar, architecture registry/prose file, workflow fixture, schema, and registered Mermaid output explicitly.
- Capture all five bundles from one clean committed revision into a directory outside the repository. Install no bundle if any capture fails.
- Audit mode remains active. ADR 0034 and the Nix enforcement flip are owned by plan 6.

---

### Task 0: Establish the settled ADR corpus

**Files:**
- Read: `docs/reports/adr-claim-migration-inventory.json`
- Read: `docs/reports/adr-evidence-migration.json`
- Read: `docs/adr/adr-claim-migration.edn`

**Interfaces:**
- Consumes: completed family corrections/evidence from plans 1–4.
- Produces: a stable input set for graph and structural-governance captures.

- [ ] **Step 1: Verify no unfinished ordinary family row**

Regenerate the disposition-aware inventory and assert that the only unresolved
immutable baseline rows are the eight rows this plan owns. Query
`baseline_criteria`, not live `criteria`, so moved-out historical rows cannot
disappear from the preflight.

```bash
cd abc
clojure -M:abc/adr-evidence-inventory -- \
  --output /tmp/adr-inventory-before-governance.json
jq -e '
  [.baseline_criteria[] | select(.disposition == null)] as $unresolved
  | ($unresolved | length) == 8
    and ($unresolved | all(.family == "diagrams-governance"))
' /tmp/adr-inventory-before-governance.json
```

Expected: both commands exit 0. Task 1 must then use Plan 1's
`require-complete? true` API after filling these eight rows.

- [ ] **Step 2: Verify derived views are current before correction**

```bash
git status --porcelain --untracked-files=all
nix build ./abc#checks.x86_64-linux.diagram-drift \
  ./abc#checks.x86_64-linux.presentation-diagram-drift
```

Expected: status prints nothing and both derivations build successfully.

---

### Task 1: Correct ADR 0029 and ADR 0031 criteria

**Files:**
- Modify: `docs/adr/0029-diagrams-as-gated-derived-views.md`
- Modify: `docs/adr/0031-adr-governance-validation.md`
- Modify: `docs/adr/adr-claim-migration.edn`
- Modify: `test/abc/tools/adr_evidence_inventory_test.clj`
- Regenerate: `docs/reports/adr-claim-migration-inventory.json`
- Regenerate: `docs/adr/adr-graph.mmd`

**Interfaces:**
- Produces: five ADR 0029 claims, four ADR 0031 claims, and a fully resolved
  146-row immutable baseline ledger.

- [ ] **Step 1: Replace ADR 0029 criteria with exact boundaries**

```markdown
- **ADR-0029-C1 — structural-invariant:** ADR graph construction consumes
  shared parsed ADR values, preserves relation scopes, and rejects unknown,
  dangling, malformed, or header-owned sidecar edges.
- **ADR-0029-C2 — structural-invariant:** Every declared architecture-stage
  schema and ADR coordinate resolves in the current repository contract.
- **ADR-0029-C3 — fixture-behavior:** The committed passed workflow fixture
  renders deterministically with producer-to-consumer edges.
- **ADR-0029-C4 — structural-invariant:** Every registered committed diagram
  is byte-equal to a fresh render of its registry entry.
- **ADR-0029-C5 — structural-invariant:** Diagram `run!` returns `:ok? false`
  for lint problems without terminating the host process.
```

Set `Validation scope: fixture` and `Release authority: none`.

- [ ] **Step 2: Replace ADR 0031 criteria with exact boundaries**

```markdown
- **ADR-0031-C1 — structural-invariant:** The ADR parser and lifecycle,
  relation, dependency, claim-header, and section policies reject their
  enumerated invalid fixtures.
- **ADR-0031-C2 — structural-invariant:** ADR graph construction consumes the
  shared parsed value model and preserves scoped relation labels.
- **ADR-0031-C3 — structural-invariant:** Manifest identity coordinates and
  declared owner references are structurally total for the current contract.
- **ADR-0031-C4 — fixture-behavior:** Schema-invalid and semantically invalid
  workflow fixtures are rejected before rendering.
```

Set `Validation scope: structural` and `Release authority: none`. Move the old
“current repository is clean” portion to unheaded prose under
`## Future Verification`:

```markdown
Complete typed claim/evidence coverage is not self-certified by this ADR.
ADR 0034 owns the complete-corpus coverage observation, its immutable
pre-promotion snapshot, and the final enforcement transition.
```

No claim header may appear in that section.

- [ ] **Step 3: Record baseline dispositions and resulting IDs**

Use the immutable original text hashes as ledger keys and record this exact
mapping:

| ADR | Original index | Disposition | Resulting claim IDs | Planned boundary / rationale |
|---:|---:|---|---|---|
| 29 | 0 | `:correct` | `["ADR-0029-C1"]` | `:adr-graph-contract`; replace coverage prose with the asserted graph/lint contract. |
| 29 | 1 | `:correct` | `["ADR-0029-C2"]` | `:architecture-graph-contract`; state coordinate resolution directly. |
| 29 | 2 | `:correct` | `["ADR-0029-C3"]` | `:workflow-graph-fixtures`; state committed fixture behavior directly. |
| 29 | 3 | `:correct` | `["ADR-0029-C4" "ADR-0029-C5"]` | `:diagram-registry-drift`; split registry parity from `run!` lint behavior. |
| 31 | 0 | `:correct` | `["ADR-0031-C1"]` | `:adr-policy-fixtures`; retain invalid-fixture behavior and delegate complete coverage to ADR 0034. |
| 31 | 1 | `:correct` | `["ADR-0031-C2"]` | `:adr-graph-contract`; state shared-model/scope preservation directly. |
| 31 | 2 | `:correct` | `["ADR-0031-C3"]` | `:architecture-graph-contract`; state identity-owner totality directly. |
| 31 | 3 | `:correct` | `["ADR-0031-C4"]` | `:workflow-graph-fixtures`; state invalid workflow rejection directly. |

- [ ] **Step 4: Regenerate inventory and graph**

First update `adr_evidence_inventory_test.clj` to assert all eight family
baseline rows have dispositions and the live family claim-ID set is exactly
`ADR-0029-C1..C5` plus `ADR-0031-C1..C4`.

```bash
cd abc
clojure -M:abc/adr-evidence-inventory -- \
  --output docs/reports/adr-claim-migration-inventory.json
clojure -M:abc/presentation-diagrams adr-graph
bin/kaocha --focus abc.tools.adr-evidence-inventory-test \
  --focus abc.tools.diagram.adr-graph-test
clojure -M -e '
(require (quote [abc.tools.adr-claim-migration :as migration]))
(let [{:keys [problems]} (migration/load-migration-state
                          "." {:require-complete? true})]
  (when (seq problems)
    (binding [*out* *err*] (prn problems))
    (System/exit 1))
  (println "complete baseline ledger: 146 rows"))'
```

Expected: no unresolved ledger key, nine live family claims, zero test
failures, and the complete-mode call prints `complete baseline ledger: 146
rows`.

- [ ] **Step 5: Commit Stage A**

```bash
git add abc/docs/adr/0029-diagrams-as-gated-derived-views.md \
  abc/docs/adr/0031-adr-governance-validation.md \
  abc/docs/adr/adr-claim-migration.edn \
  abc/test/abc/tools/adr_evidence_inventory_test.clj \
  abc/docs/reports/adr-claim-migration-inventory.json \
  abc/docs/adr/adr-graph.mmd
git commit -m "docs(adr): correct diagram and governance claims"
git status --porcelain --untracked-files=all
```

Expected: commit succeeds and status is empty.

---

### Task 2: Define ADR graph and architecture capture descriptors

**Files:**
- Create: `docs/evidence/adr-capture/adr-graph-contract.edn`
- Create: `docs/evidence/adr-capture/architecture-graph-contract.edn`

**Interfaces:**
- Produces: two committed, self-bound descriptors. Capturing is deferred until
  all five family descriptors and the registration template are committed.

- [ ] **Step 1: Write descriptors**

Create `adr-graph-contract.edn` with the closed descriptor key set, schema
version `abc-adr-evidence-capture-v1`, tool `bin/kaocha`, argv focused on
`abc.tools.diagram.adr-graph-test`, a `clojure-test-v1` profile rooted at that
namespace, observation `adr-graph-contracts-pass`, and explicit inputs
containing self, every current `docs/adr/*.md` as literal sorted paths,
`docs/adr/adr-relations.edn`, and `docs/adr/adr-graph.mmd`.

Create `architecture-graph-contract.edn` with the same closed key set, tool
`bin/kaocha`, argv focused on
`abc.tools.diagram.architecture-graph-test`, roots containing that namespace,
observation `architecture-graph-contracts-pass`, and explicit inputs: self;
every current `docs/adr/*.md`; `docs/architecture-stages.edn`;
`docs/architecture.md`; `docs/architecture.mmd`;
`schemas/schema-contracts.json`; and `schemas/manifest.schema.json`.

Prove both descriptors contain the exact current Markdown set:

```bash
cd abc
expected="$(rg --files docs/adr -g '*.md' | LC_ALL=C sort)"
for descriptor in docs/evidence/adr-capture/{adr-graph-contract,architecture-graph-contract}.edn; do
  actual="$(clojure -M -e '
    (require (quote [clojure.edn :as edn]))
    (->> (:explicit (:input-profile (edn/read-string (slurp (first *command-line-args*)))))
         (filter #(re-matches #"docs/adr/.*[.]md" %))
         sort
         (run! println))' "$descriptor")"
  test "$actual" = "$expected" || exit 1
done
```

- [ ] **Step 2: Run focused tests before capture**

```bash
cd abc
bin/kaocha --focus abc.tools.diagram.adr-graph-test \
  --focus abc.tools.diagram.architecture-graph-test
```

Expected: zero failures.

- [ ] **Step 3: Commit both descriptors**

```bash
git add abc/docs/evidence/adr-capture/adr-graph-contract.edn \
  abc/docs/evidence/adr-capture/architecture-graph-contract.edn
git commit -m "test(diagrams): define graph evidence captures"
git status --porcelain --untracked-files=all
```

Expected: commit succeeds and status is empty. Do not capture yet.


---

### Task 3: Define workflow and diagram-registry descriptors

**Files:**
- Create: `docs/evidence/adr-capture/workflow-graph-fixtures.edn`
- Create: `docs/evidence/adr-capture/diagram-registry-drift.edn`

**Interfaces:**
- Produces: two committed, self-bound descriptors; no run bundle or registry
  mutation occurs in this task.

- [ ] **Step 1: Define the workflow descriptor**

Use tool `bin/kaocha`, argv/root focused on
`abc.tools.diagram.workflow-graph-test`, observation
`workflow-graph-fixtures-pass`, and explicit inputs containing self,
`examples/workflow/passed.workflow-run.json`, and
`schemas/workflow-run.schema.json`.

- [ ] **Step 2: Define the registry/core descriptor**

Use tool `bin/kaocha`, argv/roots focused on both
`abc.tools.diagram.registry-test` and `abc.tools.diagram.core-test`,
observation `diagram-registry-contracts-pass`, and explicit inputs containing
self; every current `docs/adr/*.md` as literal sorted paths;
`docs/adr/adr-relations.edn`; `docs/architecture-stages.edn`;
`docs/architecture.md`; `schemas/schema-contracts.json`;
`schemas/manifest.schema.json`; `docs/adr/adr-graph.mmd`; and
`docs/architecture.mmd`. Run the exact Markdown-set comparison from Task 2
against this descriptor before commit.

- [ ] **Step 3: Run focused checks**

```bash
cd abc
bin/kaocha --focus abc.tools.diagram.workflow-graph-test \
  --focus abc.tools.diagram.registry-test \
  --focus abc.tools.diagram.core-test
```

Expected: zero failures.

- [ ] **Step 4: Commit both descriptors**

```bash
git add abc/docs/evidence/adr-capture/workflow-graph-fixtures.edn \
  abc/docs/evidence/adr-capture/diagram-registry-drift.edn
git commit -m "test(diagrams): define workflow and registry captures"
git status --porcelain --untracked-files=all
```

Expected: commit succeeds and status is empty. Do not capture yet.

---

### Task 4: Define governance-policy descriptor

**Files:**
- Create: `docs/evidence/adr-capture/adr-policy-fixtures.edn`

**Interfaces:**
- Produces: the fifth committed, self-bound descriptor.

- [ ] **Step 1: Define and run the focused boundary**

Use tool `bin/kaocha`, argv/root focused on `abc.tools.adr-test`, observation
`adr-policy-fixtures-pass`, and explicit inputs containing self; every current
`docs/adr/*.md` as literal sorted paths; `docs/adr/adr-relations.edn`;
`docs/adr/claim-evidence-compatibility.edn`; and
`docs/adr/governance-as-of.edn`. Synthetic temp fixtures remain derived test
values and need no explicit path. Run the exact Markdown-set comparison from
Task 2 before commit.

```bash
cd abc
bin/kaocha --focus abc.tools.adr-test
```

Expected: zero failures.

- [ ] **Step 2: Commit the descriptor**

```bash
git add abc/docs/evidence/adr-capture/adr-policy-fixtures.edn
git commit -m "test(adr): define policy fixture capture"
git status --porcelain --untracked-files=all
```

Expected: commit succeeds and status is empty. Do not capture yet.

---

### Task 5: Stage all bundles externally and register through Plan 1

**Files:**
- Create: `docs/evidence/adr-entries/diagrams-governance.edn`
- Create: `docs/evidence/adr-runs/adr-graph-contract.json`
- Create: `docs/evidence/adr-runs/architecture-graph-contract.json`
- Create: `docs/evidence/adr-runs/workflow-graph-fixtures.json`
- Create: `docs/evidence/adr-runs/diagram-registry-drift.json`
- Create: `docs/evidence/adr-runs/adr-policy-fixtures.json`
- Modify: `docs/adr/adr-evidence.edn` through Plan 1's registrar only

**Interfaces:**
- Consumes: all five committed descriptors and Plan 1's tested registrar.
- Produces: five same-revision bundles and nine validated claim entries.

- [ ] **Step 1: Author and commit the hash-free registration template**

Create `docs/evidence/adr-entries/diagrams-governance.edn` exactly as follows:

```clojure
{:schema-version :abc-adr-evidence-registration-v1
 :entries
 [{:claim-id "ADR-0029-C1" :claim-kind :structural-invariant
   :evidence-kind :structural-test
   :artifact-path "docs/evidence/adr-runs/adr-graph-contract.json"
   :observation-key "adr-graph-contracts-pass"
   :expected {:operator := :value true}}
  {:claim-id "ADR-0029-C2" :claim-kind :structural-invariant
   :evidence-kind :structural-test
   :artifact-path "docs/evidence/adr-runs/architecture-graph-contract.json"
   :observation-key "architecture-graph-contracts-pass"
   :expected {:operator := :value true}}
  {:claim-id "ADR-0029-C3" :claim-kind :fixture-behavior
   :evidence-kind :fixture-conformance
   :artifact-path "docs/evidence/adr-runs/workflow-graph-fixtures.json"
   :observation-key "workflow-graph-fixtures-pass"
   :expected {:operator := :value true}}
  {:claim-id "ADR-0029-C4" :claim-kind :structural-invariant
   :evidence-kind :structural-test
   :artifact-path "docs/evidence/adr-runs/diagram-registry-drift.json"
   :observation-key "diagram-registry-contracts-pass"
   :expected {:operator := :value true}}
  {:claim-id "ADR-0029-C5" :claim-kind :structural-invariant
   :evidence-kind :structural-test
   :artifact-path "docs/evidence/adr-runs/diagram-registry-drift.json"
   :observation-key "diagram-registry-contracts-pass"
   :expected {:operator := :value true}}
  {:claim-id "ADR-0031-C1" :claim-kind :structural-invariant
   :evidence-kind :structural-test
   :artifact-path "docs/evidence/adr-runs/adr-policy-fixtures.json"
   :observation-key "adr-policy-fixtures-pass"
   :expected {:operator := :value true}}
  {:claim-id "ADR-0031-C2" :claim-kind :structural-invariant
   :evidence-kind :structural-test
   :artifact-path "docs/evidence/adr-runs/adr-graph-contract.json"
   :observation-key "adr-graph-contracts-pass"
   :expected {:operator := :value true}}
  {:claim-id "ADR-0031-C3" :claim-kind :structural-invariant
   :evidence-kind :structural-test
   :artifact-path "docs/evidence/adr-runs/architecture-graph-contract.json"
   :observation-key "architecture-graph-contracts-pass"
   :expected {:operator := :value true}}
  {:claim-id "ADR-0031-C4" :claim-kind :fixture-behavior
   :evidence-kind :fixture-conformance
   :artifact-path "docs/evidence/adr-runs/workflow-graph-fixtures.json"
   :observation-key "workflow-graph-fixtures-pass"
   :expected {:operator := :value true}}]}
```

```bash
git add abc/docs/evidence/adr-entries/diagrams-governance.edn
git commit -m "docs(evidence): define diagram governance entries"
git status --porcelain --untracked-files=all
```

Expected: commit succeeds and status is empty. No entry contains
`:artifact-hash`, inline observations, inputs, or verdicts. This is the common
producer revision for all five captures.

- [ ] **Step 2: Capture all five bundles into one external staging directory**

```bash
cd abc
stage="$(mktemp -d)"
for stem in \
  adr-graph-contract \
  architecture-graph-contract \
  workflow-graph-fixtures \
  diagram-registry-drift \
  adr-policy-fixtures
do
  clojure -M:abc/adr-evidence-capture -- \
    --descriptor "docs/evidence/adr-capture/${stem}.edn" \
    --output "${stage}/${stem}.json" || exit 1
  test -z "$(git status --porcelain --untracked-files=all)" || exit 1
done
```

Expected: all captures exit 0 and every status check is empty. If any capture
fails, install nothing; repair and commit, then recapture all five so they
share one producer revision.

- [ ] **Step 3: Validate all staged bundles before installing any**

```bash
for stem in \
  adr-graph-contract architecture-graph-contract workflow-graph-fixtures \
  diagram-registry-drift adr-policy-fixtures
do
  jq -e '
    .schema_version == "abc-adr-evidence-run-v1"
    and (.observations | length == 1)
    and ([.observations[].value] | all)
  ' "${stage}/${stem}.json" || exit 1
done
mkdir -p docs/evidence/adr-runs
for stem in \
  adr-graph-contract architecture-graph-contract workflow-graph-fixtures \
  diagram-registry-drift adr-policy-fixtures
do
  install -m 0644 "${stage}/${stem}.json" \
    "docs/evidence/adr-runs/${stem}.json"
done
```

Expected: every staged value validates before the first install; afterward
exactly five run bundles are newly dirty.

- [ ] **Step 4: Register all nine claims atomically and prove idempotence**

```bash
clojure -M:abc/adr-evidence-register -- \
  --workspace-root .. \
  --entries docs/evidence/adr-entries/diagrams-governance.edn \
  --registry docs/adr/adr-evidence.edn
cp docs/adr/adr-evidence.edn /tmp/diagrams-governance-registry.edn
clojure -M:abc/adr-evidence-register -- \
  --workspace-root .. \
  --entries docs/evidence/adr-entries/diagrams-governance.edn \
  --registry docs/adr/adr-evidence.edn
cmp -s docs/adr/adr-evidence.edn /tmp/diagrams-governance-registry.edn
```

Expected: both calls exit 0 and report nine registered claim IDs; the second
call is byte-identical. The registrar derives canonical hashes and aborts on
missing observations, stale inputs, incompatible kinds, or unrelated
non-missing-evidence problems.

---

### Task 6: Close diagrams/governance family audit

**Files:**
- Regenerate: `docs/reports/adr-claim-migration-inventory.json`
- Regenerate: `docs/reports/adr-evidence-migration.json`
- Regenerate: all registered diagram outputs.

- [ ] **Step 1: Regenerate reports and diagrams**

```bash
cd "$(git rev-parse --show-toplevel)"
workspace_root="$PWD"
cd abc
clojure -M:abc/adr-evidence-inventory -- \
  --output docs/reports/adr-claim-migration-inventory.json
clojure -M:abc/adr-governance -- \
  --repo-root "$workspace_root/abc" --workspace-root "$workspace_root" \
  --mode audit \
  --report docs/reports/adr-evidence-migration.json
clojure -M:abc/presentation-diagrams all
```

Expected: all commands exit 0.

- [ ] **Step 2: Assert family closure**

```bash
jq -e --arg family '^(ADR-)?(0029|0031)-' \
  -f nix/adr-family-clean.jq \
  docs/reports/adr-evidence-migration.json
jq -e '
  [.criteria[] | select(.family == "diagrams-governance")] as $claims
  | ($claims | length) == 9
    and ($claims | all(.disposition != null
                       and .claim_id != null
                       and .claim_kind != null))
' docs/reports/adr-claim-migration-inventory.json
```

Expected: both commands exit 0. The checked Plan 1 jq predicate covers
file-owned, literal `claim-id`, and deduplicated `affected-claim-ids` problems;
do not replace it with an underscore-field filter. ADR 0034 may remain Proposed
and absent from required Accepted coverage.

- [ ] **Step 3: Run drift and focused gates**

```bash
cd "$(git rev-parse --show-toplevel)"
git add abc/docs/evidence/adr-runs/adr-graph-contract.json \
  abc/docs/evidence/adr-runs/architecture-graph-contract.json \
  abc/docs/evidence/adr-runs/workflow-graph-fixtures.json \
  abc/docs/evidence/adr-runs/diagram-registry-drift.json \
  abc/docs/evidence/adr-runs/adr-policy-fixtures.json \
  abc/docs/adr/adr-evidence.edn \
  abc/docs/reports/adr-claim-migration-inventory.json \
  abc/docs/reports/adr-evidence-migration.json \
  abc/docs/adr/adr-graph.mmd abc/docs/architecture.mmd
git diff --cached --check
nix build ./abc#checks.x86_64-linux.diagram-drift \
  ./abc#checks.x86_64-linux.presentation-diagram-drift \
  .#checks.x86_64-linux.monorepo-adr-governance \
  ./abc#checks.x86_64-linux.clj-kondo
```

Expected: all derivations build successfully.

- [ ] **Step 4: Commit family closure**

```bash
git diff --cached --name-only
git commit -m "feat(adr): migrate diagram and governance evidence"
git status --porcelain --untracked-files=all
```

Expected: commit succeeds and status is empty.

## Plan Self-Review

- Graph/API, workflow fixture, registry drift, and ADR policy observations are separate.
- The complete 146-row immutable baseline ledger passes `require-complete? true`.
- ADR 0031 remains structural and has no complete-corpus bundle to become circular or stale; ADR 0034 is the sole complete-coverage authority.
- All five bundles are staged externally from one clean revision, installed together, and joined through Plan 1's hash-deriving registrar/template interface.
- Family closure uses checked `nix/adr-family-clean.jq`, covering file, `claim-id`, and `affected-claim-ids` ownership.
- No diagram claim asserts semantic correctness of declared architecture values.
- Audit remains active; plan 6 alone owns enforcement.
