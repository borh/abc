# ADR Evidence Migration Enforcement Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Close the evidence migration with an immutable, non-self-referential ADR 0034 bootstrap observation and atomically promote ADR 0034 while changing the Nix governance gate from audit to enforcement.

**Architecture:** Plans 1–5 leave one clean, fully evidenced pre-promotion corpus. A focused bootstrap module snapshots that exact corpus, embeds every audited byte behind a manifest, validates the closed snapshot without consulting later-mutated live files, and supplies ADR-0034-C3; two synthetic focused bundles supply C1 and C2. A detached clean Stage-A worktree supplies post-promotion input hashes for C1–C3 and the four Plan 5 bundles staled by ADR/view changes; the completed tree is soft-reset and squashed directly onto the pre-promotion parent, fully verified, then fast-forwarded onto the integration branch once.

**Tech Stack:** Clojure 1.12, Kaocha/clojure.test, deterministic JSON/JCS SHA-256, JSON Schema, EDN, Git, jq, Mermaid derived views, Nix flakes, Just.

## Global Constraints

- Consume completed plans 1–5. Do not redo ordinary family claim correction or evidence capture here.
- Work only in the isolated migration worktree and preserve unrelated user changes.
- The immutable bootstrap subject is the pre-promotion Accepted corpus excluding ADR 0034; it does not claim that ADR 0034 proves its own semantic correctness.
- Audit exit zero is not evidence. Snapshot creation and C3 capture succeed only when report `ok` is true and `problems` is empty.
- Never hash-bind C3 to the later-mutated live registry, ADR 0034 Markdown, final report, its own run bundle, or an input manifest containing its own artifact hash.
- The snapshot contains exact pre-promotion bytes and hashes; the validator verifies only that closed value after promotion.
- Checked-in descriptors are explicit inputs to their bundles. C1/C2 bind only their synthetic assertion boundary and runtime-read schemas/matrix, never the live ADR corpus.
- Capture from a clean committed pre-promotion tree into an external staging directory. A failed capture creates no registry entry.
- ADR 0034 promotes with `Validation scope: full-corpus` and `Release authority: none`.
- The final commit contains ADR 0034 Accepted, C1/C2/C3 entries and bundles, recaptured Plan 5 graph/policy bundles, regenerated inventory/governance reports and diagrams, and the root Nix `--mode enforce` switch. No Accepted/audit-only intermediate lands on the integration branch.
- Publication remains governed independently by `data/publication-policy.edn`; enforcement does not imply publication admission.
- Use `apply_patch` for hand edits, `nixfmt` for Nix, and the root `justfile` for the final gate.

---

## File and Interface Map

- Create `schemas/adr-evidence-bootstrap.schema.json`: closed schema for the immutable pre-promotion snapshot.
- Create `src/abc/tools/adr_evidence_bootstrap.clj` with these public interfaces:
  - `(snapshot-input-paths abc-root workspace-root adrs registry) -> sorted-vector<workspace-relative-path>`; returns the exact Accepted ADR, governance source/config, referenced artifact/input across both components, migration-ledger, and generated-view closure.
  - `(snapshot-value abc-root workspace-root) -> deterministic JSON value`; requires ADR 0034 Proposed, complete migration ledger, strict governance `ok = true`, and zero problems before embedding bytes and hashes.
  - `(validate-snapshot-value value) -> vector<problem>`; validates schema, Base64 payloads, hashes, Accepted-set/count agreement, report mode/ok/problem count, and manifest closure without reading live mutable governance files.
  - `(validate-snapshot-file path) -> vector<problem>`.
  - `(final-transition-problems abc-root workspace-root snapshot) -> vector<problem>`; requires ADR 0034 Accepted, pre-count + 1 Accepted ADR, pre-count + 3 criteria, complete ledger, zero live strict problems, and an enforcement-mode root Nix check.
  - CLI: every `--write PATH`, `--verify PATH`, and `--verify-final PATH` invocation also requires explicit `--repo-root PATH --workspace-root PATH`; each exits nonzero on any problem.
- Create `test/abc/tools/adr_evidence_bootstrap_test.clj`: schema, manifest, snapshot, and final-transition tests.
- Create `test/abc/tools/adr_0034_evidence_test.clj`: synthetic, repository-independent assertions for C1 and C2.
- Modify `test/abc/tools/adr_governance_test.clj`: real invalid-fixture audit/enforce problem-set parity and Nix enforcement selection.
- Create `fixtures/adr-governance-invalid/docs/adr/0001-invalid.md`: deliberately invalid lifecycle fixture used by parity tests.
- Add alias `:abc/adr-evidence-bootstrap` to `deps.edn`.
- Create `docs/evidence/adr-bootstrap/pre-promotion.json`: immutable snapshot.
- Create descriptors `docs/evidence/adr-capture/adr-0034-c1.edn`, `adr-0034-c2.edn`, and `adr-0034-c3.edn`.
- Create run bundles `docs/evidence/adr-runs/adr-0034-c1.json`, `adr-0034-c2.json`, and `adr-0034-c3.json`.
- Create `docs/evidence/adr-entries/adr-0034.edn`: hash-free input to plan 1's deterministic evidence registrar.
- Modify `docs/adr/0034-typed-evidence-and-lifecycle-closure.md`, `docs/adr/adr-evidence.edn`, and root `flake.nix` only in the staged atomic transition.
- Regenerate `docs/reports/adr-claim-migration-inventory.json`, `docs/reports/adr-evidence-migration.json`, `docs/adr/adr-graph.mmd`, and every registry-owned Mermaid output affected by promotion.

### Snapshot JSON contract

The schema is closed and requires this shape:

```json
{
  "schema_version": "abc-adr-evidence-bootstrap-v1",
  "subject": "pre-promotion Accepted ADR corpus excluding ADR 0034",
  "producer_revision": "0000000000000000000000000000000000000000",
  "governance_as_of": "YYYY-MM-DD",
  "accepted_adr_numbers": [1, 2],
  "accepted_adr_count": 2,
  "accepted_criterion_count": 7,
  "audit_report": {"mode": "enforce", "ok": true, "problems": []},
  "files": {
    "abc/docs/adr/0001-example.md": {
      "sha256": "sha256:e4ad9729a18e2a9ee7a3331863fc765bd2eb177b93ae28f5391f832cfab13b13",
      "content_base64": "YWNjZXB0ZWQgYnl0ZXMK"
    }
  }
}
```

`files` is the manifest and payload together, so each hash is recomputed from decoded bytes. It contains the exact pre-promotion registry and evidence artifacts but no hash of the snapshot itself. `validate-snapshot-value` never compares these bytes with post-promotion live paths.

---

### Task 0: Verify plans 1–5 and establish the sole corpus authority

**Files:**
- Read: `docs/adr/adr-claim-migration-baseline.json`
- Read: `docs/adr/adr-claim-migration.edn`
- Read: `docs/reports/adr-claim-migration-inventory.json`
- Read: `docs/reports/adr-evidence-migration.json`
- Read: `docs/adr/adr-evidence.edn`

**Interfaces:**
- Consumes: complete ledger and evidence joins from plans 1–5.
- Produces: a clean, zero-problem, audit-mode pre-promotion revision safe to snapshot.

- [ ] **Step 1: Require a clean completed family checkpoint**

```bash
git status --short
workspace_root="$(git rev-parse --show-toplevel)"
cd "$workspace_root/abc"
clojure -M:abc/adr-evidence-inventory -- \
  --output /tmp/adr-plan6-inventory.json
jq -e '[.baseline_criteria[] | select(.disposition == null)] | length == 0' \
  /tmp/adr-plan6-inventory.json
clojure -M:abc/adr-governance -- \
  --repo-root "$workspace_root/abc" --workspace-root "$workspace_root" \
  --mode audit \
  --report /tmp/adr-plan6-audit.json
jq -e '.ok == true and (.problems | length) == 0' \
  /tmp/adr-plan6-audit.json
```

Expected: clean tree; complete ledger; audit report is `ok: true` with zero problems; ADR 0034 remains Proposed and is not yet a required claim.

- [ ] **Step 2: Confirm ADR 0031 has no recursive corpus authority**

```bash
test ! -e docs/evidence/adr-runs/adr-corpus-conformance.json
jq -e '[.criteria[] | select(.claim_id | startswith("ADR-0031-"))]
       | length == 4 and all(.claim_kind != "corpus-behavior")' \
  docs/reports/adr-claim-migration-inventory.json
```

Expected: both assertions pass. ADR 0034 is the sole complete-corpus authority;
there is no older Accepted claim or live-registry-bound artifact to stale during
promotion.

- [ ] **Step 3: Verify enforcement is still off**

```bash
rg -n 'monorepo-adr-governance' ../flake.nix
rg -U 'monorepo-adr-governance[\s\S]*?--mode audit' ../flake.nix
! rg -U 'monorepo-adr-governance[\s\S]*?--mode enforce' ../flake.nix
rg -n '^Status: Proposed$' docs/adr/0034-typed-evidence-and-lifecycle-closure.md
```

Expected: all assertions succeed.

---

### Task 1: Implement the immutable bootstrap schema and validator

**Files:**
- Create: `schemas/adr-evidence-bootstrap.schema.json`
- Create: `src/abc/tools/adr_evidence_bootstrap.clj`
- Create: `test/abc/tools/adr_evidence_bootstrap_test.clj`
- Modify: `deps.edn`

**Interfaces:**
- Consumes: `abc.tools.adr/parse-all`, `abc.tools.adr-governance/run!`, `abc.tools.adr-claim-migration/load-migration-state`, `abc.tools.adr-evidence-bundle/load-bundle`, `abc.tools.hash/sha256-file`, and deterministic JSON helpers.
- Produces: all six bootstrap functions and three CLI modes in the file map.

- [ ] **Step 1: Write failing closed-schema and byte-manifest tests**

Add tests whose minimal valid value matches the Snapshot JSON contract. Assert rejection of an unknown top-level key, malformed revision/date/hash/Base64, duplicate ADR numbers, count disagreement, a missing required governance file, a payload hash mismatch, report mode other than `enforce`, `ok: false`, and nonempty `problems`.

```clojure
(deftest closed-snapshot-recomputes-every-payload-hash
  (let [bytes (.getBytes "accepted bytes\n" java.nio.charset.StandardCharsets/UTF_8)
        encoded (.encodeToString (java.util.Base64/getEncoder) bytes)
        value (valid-snapshot
               {"abc/docs/adr/0001-one.md"
                {"sha256" (hash/format-sha256 (hash/sha256-bytes bytes))
                 "content_base64" encoded}})]
    (is (empty? (bootstrap/validate-snapshot-value value)))
    (is (contains? (set (map :kind
                             (bootstrap/validate-snapshot-value
                              (assoc-in value ["files" "abc/docs/adr/0001-one.md"
                                               "content_base64"] "YmFk"))))
                   :bootstrap-file-hash-mismatch))))
```

- [ ] **Step 2: Run RED**

Run: `cd abc && bin/kaocha --focus abc.tools.adr-evidence-bootstrap-test`

Expected: FAIL because the schema and namespace do not exist.

- [ ] **Step 3: Implement schema validation and closed byte verification**

Use the repository JSON Schema loader for the closed top-level/nested shapes. Decode every payload with the basic RFC 4648 decoder, recompute SHA-256 over exact decoded bytes, sort paths, reject unsafe/absolute/`..` paths, and require these manifest roots:

```clojure
#{"abc/docs/adr/adr-evidence.edn"
  "abc/docs/adr/claim-evidence-compatibility.edn"
  "abc/docs/adr/governance-as-of.edn"
  "abc/docs/adr/adr-claim-migration-baseline.json"
  "abc/docs/adr/adr-claim-migration.edn"
  "abc/src/abc/tools/adr.clj"
  "abc/src/abc/tools/adr_governance.clj"
  "abc/src/abc/tools/adr_evidence.clj"
  "abc/src/abc/tools/adr_evidence_bundle.clj"
  "abc/src/abc/tools/adr_claim_migration.clj"
  "abc/schemas/adr-evidence-run.schema.json"
  "abc/schemas/adr-external-evidence.schema.json"}
```

Also require every listed Accepted ADR Markdown file under `abc/`, every
artifact referenced by the embedded registry, every component- or
workspace-relative input named by those embedded bundles (including
`ab-validator/...`), and every registered generated Mermaid view used by the
audit.

- [ ] **Step 4: Write failing snapshot-generation tests**

With temporary repository fixtures and function redefinitions only at external process seams, assert `snapshot-value` rejects ADR 0034 Accepted, an incomplete ledger, any strict problem, `ok: false`, missing referenced artifact/input, or a path outside the repository. Assert the valid result embeds exact bytes, exact Accepted numbers/counts, governance date, HEAD revision, and strict report.

- [ ] **Step 5: Run RED for generation**

Run: `cd abc && bin/kaocha --focus abc.tools.adr-evidence-bootstrap-test`

Expected: FAIL on missing `snapshot-input-paths` and `snapshot-value` behavior.

- [ ] **Step 6: Implement deterministic generation and CLI**

`snapshot-input-paths` builds the sorted workspace-relative union of:

1. every pre-promotion Accepted ADR Markdown path prefixed `abc/`, excluding Proposed ADR 0034;
2. the fixed governance roots above plus `abc/docs/adr/adr-relations.edn` and plan-1 ledger inputs;
3. every ABC-relative registry `artifact-path` prefixed `abc/`; for each loaded
   artifact, prefix ordinary `clojure-test-v1`, `repo-files-v1`, and
   `external-authority-v1` input keys with `abc/`, while preserving
   `component-clojure-test-v1` keys as already monorepo-relative;
4. all `abc/src/abc/tools/adr*.clj`, `path_containment.clj`, `hash.clj`,
   `json.clj`, and `files.clj` used by strict validation; and
5. all registered committed Mermaid paths under `abc/` and their sidecars/data roots.

Run `governance/run!` with `:repo-root abc-root`, `:workspace-root
workspace-root`, and `:mode :enforce` before reading bytes and again after
reading bytes; both results must be identical, `:ok? true`, and empty. Run
`git -C workspace-root status --porcelain --untracked-files=all` before and
after. Require `workspace-root` to be the Git root and `abc-root` to be its
contained `abc/` directory. Add the `:abc/adr-evidence-bootstrap` alias and
write output only after the value validates.

- [ ] **Step 7: Write and implement failing final-transition assertions**

Tests start from a valid snapshot and stubbed live values. Require exactly one newly Accepted ADR numbered 34, exactly three newly binding criteria, `Validation scope: full-corpus`, `Release authority: none`, complete evidence coverage, strict `ok`, empty strict problems, and `flake.nix` selecting enforce rather than audit.

```clojure
(deftest final-transition-requires-one-adr-three-claims-and-enforcement
  (let [kinds (set (map :kind
                        (bootstrap/final-transition-problems
                         abc-root workspace-root (valid-snapshot))))]
    (is (contains? kinds :adr-0034-not-accepted))
    (is (contains? kinds :governance-gate-not-enforced))))
```

Implement stable problem kinds and the `--verify-final` CLI mode. Do not add an override for counts or strict failures.

- [ ] **Step 8: Run GREEN and commit the bootstrap implementation**

```bash
cd abc
bin/kaocha --focus abc.tools.adr-evidence-bootstrap-test \
  --focus abc.tools.adr-evidence-bundle-test \
  --focus abc.tools.adr-evidence-capture-test
cd ..
git add abc/schemas/adr-evidence-bootstrap.schema.json \
  abc/src/abc/tools/adr_evidence_bootstrap.clj \
  abc/test/abc/tools/adr_evidence_bootstrap_test.clj \
  abc/deps.edn
git commit -m "feat(adr): validate immutable promotion snapshots"
```

Expected: zero failures and a clean tree after commit.

---

### Task 2: Add direct ADR 0034 structural assertions and invalid-fixture parity

**Files:**
- Create: `test/abc/tools/adr_0034_evidence_test.clj`
- Create: `fixtures/adr-governance-invalid/docs/adr/0001-invalid.md`
- Modify: `test/abc/tools/adr_governance_test.clj`
- Modify: `src/abc/tools/adr_evidence_inventory.clj`
- Modify: `test/abc/tools/adr_evidence_inventory_test.clj`

**Interfaces:**
- Produces: repository-independent test selections `adr-0034-c1-contract` and `adr-0034-c2-contract`; a deliberately invalid fixture proving audit/enforce problem equality and distinct exit codes.

- [ ] **Step 1: Write C1 tests before descriptor capture**

Add one `deftest` named `adr-0034-c1-contract` that uses temporary artifact repositories and directly asserts: matrix closure; forbidden inline `:observed`/`:inputs`/`:verdict`; artifact hash mismatch; current-input drift; predicate derivation/type errors; and deterministic external expiry at two explicit governance dates. It must not call the live registry or parse the live ADR corpus.

- [ ] **Step 2: Write C2 tests before descriptor capture**

Add `adr-0034-c2-contract` using synthetic ADR maps/files. Assert lifecycle vocabulary, complete dependency closure, cycle termination, stable shortest witness paths, ADR-then-evidence aggregation, audit exit 0 on problems, enforce exit 1 on the same problems, and legacy isolation. Do not assert real-corpus cleanliness.

- [ ] **Step 3: Add the invalid repository fixture and parity test**

The fixture contains a calendar-valid Draft ADR with one invalid dependency target and no evidence obligation:

```markdown
# ADR 0001: Invalid dependency fixture

Status: Draft
Date: 2026-07-12
Depends on: ADR 9999

## Decision

This fixture deliberately references a missing ADR.
```

In `adr_governance_test.clj`, parse that fixture for ADR problems while redefining only the typed-evidence loader to an empty valid registry. Assert exact equality of audit/enforce `:problems`, audit exit 0, enforce exit 1, and both `:ok? false`.

Add ADR 0034 to the closed `diagrams-governance` family map. Extend inventory
tests to prove a Proposed ADR 0034 creates no live row, while an otherwise
identical Accepted fixture creates three classified rows and never appears in
`unclassified`. This mapping must land before the detached promotion candidate
regenerates inventory.

- [ ] **Step 4: Run RED or prove existing behavior**

```bash
cd abc
bin/kaocha --focus abc.tools.adr-0034-evidence-test \
  --focus abc.tools.adr-governance-test
```

Expected: new C1/C2 assertions expose any missing behavior; if all pass immediately, retain the tests as direct characterization and record that no implementation change was required.

- [ ] **Step 5: Implement only behavior required by failing direct assertions**

Modify the owning focused module (`adr.clj`, `adr_evidence.clj`, `adr_evidence_bundle.clj`, or `adr_governance.clj`) only when a new direct assertion fails. Keep pure validation APIs deterministic and preserve existing problem kinds. Re-run the exact focused command after each minimal fix.

- [ ] **Step 6: Commit the stable assertion boundary**

```bash
git add abc/test/abc/tools/adr_0034_evidence_test.clj \
  abc/test/abc/tools/adr_governance_test.clj \
  abc/src/abc/tools/adr_evidence_inventory.clj \
  abc/test/abc/tools/adr_evidence_inventory_test.clj \
  abc/fixtures/adr-governance-invalid \
  abc/src/abc/tools/adr.clj \
  abc/src/abc/tools/adr_evidence.clj \
  abc/src/abc/tools/adr_evidence_bundle.clj \
  abc/src/abc/tools/adr_governance.clj
git commit -m "test(adr): pin governance self-certification behavior"
```

Expected: commit only files actually changed; clean tree afterward.

---

### Task 3: Materialize and commit the pre-promotion snapshot and capture descriptors

**Files:**
- Create: `docs/evidence/adr-bootstrap/pre-promotion.json`
- Create: `docs/evidence/adr-capture/adr-0034-c1.edn`
- Create: `docs/evidence/adr-capture/adr-0034-c2.edn`
- Create: `docs/evidence/adr-capture/adr-0034-c3.edn`
- Create: `docs/evidence/adr-entries/adr-0034.edn`
- Modify: `test/abc/tools/adr_evidence_capture_test.clj`

**Interfaces:**
- Consumes: clean Tasks 1–2 revision and zero-problem pre-promotion corpus.
- Produces: immutable snapshot plus three checked-in clean-capture descriptors.

- [ ] **Step 1: Generate the snapshot outside the repository**

```bash
cd "$(git rev-parse --show-toplevel)"
workspace_root="$PWD"
bootstrap_stage="${TMPDIR:-/tmp}/soranoha-adr-plan6-bootstrap"
rm -rf "$bootstrap_stage"
mkdir -p "$bootstrap_stage"
(cd abc && clojure -M:abc/adr-evidence-bootstrap -- \
  --repo-root "$workspace_root/abc" --workspace-root "$workspace_root" \
  --write "$bootstrap_stage/pre-promotion.json")
(cd abc && clojure -M:abc/adr-evidence-bootstrap -- \
  --repo-root "$workspace_root/abc" --workspace-root "$workspace_root" \
  --verify "$bootstrap_stage/pre-promotion.json")
jq -e '.subject == "pre-promotion Accepted ADR corpus excluding ADR 0034"
       and .audit_report.ok == true
       and (.audit_report.problems | length) == 0' \
  "$bootstrap_stage/pre-promotion.json"
```

Expected: all exit 0 and the worktree remains clean.

- [ ] **Step 2: Add the exact descriptors**

`adr-0034-c1.edn`:

```clojure
{:schema-version "abc-adr-evidence-capture-v1"
 :tool "bash"
 :argv ["bash" "-lc" "cd abc && bin/kaocha --focus abc.tools.adr-0034-evidence-test/adr-0034-c1-contract"]
 :input-profile {:kind "component-clojure-test-v1"
                 :component-root "abc"
                 :roots ["abc.tools.adr-0034-evidence-test"]
                 :explicit ["abc/docs/evidence/adr-capture/adr-0034-c1.edn"
                            "abc/docs/adr/claim-evidence-compatibility.edn"
                            "abc/schemas/adr-evidence-run.schema.json"
                            "abc/schemas/adr-external-evidence.schema.json"]}
 :observation-key "typed-artifact-protocol-passes"}
```

`adr-0034-c2.edn` uses the same tool, component profile, and root; its argv
focuses `.../adr-0034-c2-contract`; its explicit inputs are
`abc/docs/evidence/adr-capture/adr-0034-c2.edn` and
`abc/fixtures/adr-governance-invalid/docs/adr/0001-invalid.md`; and its
observation is `lifecycle-and-mode-contract-passes`.

`adr-0034-c3.edn` uses tool `bash`, `component-clojure-test-v1`, component root
`abc`, root `abc.tools.adr-evidence-bootstrap`, and argv
`["bash" "-lc" "cd abc && clojure -M:abc/adr-evidence-bootstrap -- --repo-root . --workspace-root .. --verify docs/evidence/adr-bootstrap/pre-promotion.json"]`.
Its explicit inputs are exactly
`abc/docs/evidence/adr-capture/adr-0034-c3.edn`,
`abc/docs/evidence/adr-bootstrap/pre-promotion.json`, and
`abc/schemas/adr-evidence-bootstrap.schema.json`; namespace closure derives
the bootstrap validator and helper sources. Its observation key is
`pre-promotion-corpus-conforms`.

- [ ] **Step 3: Extend descriptor contract tests**

Load all three descriptors and assert exact key set, component root `abc`,
descriptor self-binding with `abc/...` paths, stable observation keys, C1/C2
lack all live `abc/docs/adr/*.md` and `abc/docs/adr/adr-evidence.edn` inputs,
and C3 lacks live registry/final report/ADR 0034 inputs.

Create this exact hash-free registration template:

```clojure
{:schema-version :abc-adr-evidence-registration-v1
 :entries
 [{:claim-id "ADR-0034-C1"
   :claim-kind :structural-invariant
   :evidence-kind :structural-test
   :artifact-path "docs/evidence/adr-runs/adr-0034-c1.json"
   :observation-key "typed-artifact-protocol-passes"
   :expected {:operator := :value true}}
  {:claim-id "ADR-0034-C2"
   :claim-kind :structural-invariant
   :evidence-kind :structural-test
   :artifact-path "docs/evidence/adr-runs/adr-0034-c2.json"
   :observation-key "lifecycle-and-mode-contract-passes"
   :expected {:operator := :value true}}
  {:claim-id "ADR-0034-C3"
   :claim-kind :corpus-behavior
   :evidence-kind :corpus-measurement
   :artifact-path "docs/evidence/adr-runs/adr-0034-c3.json"
   :observation-key "pre-promotion-corpus-conforms"
   :expected {:operator := :value true}}]}
```

- [ ] **Step 4: Copy snapshot, run tests, and commit snapshot/descriptors**

```bash
cd "$workspace_root"
bootstrap_stage="${TMPDIR:-/tmp}/soranoha-adr-plan6-bootstrap"
install -D -m 0644 "$bootstrap_stage/pre-promotion.json" \
  abc/docs/evidence/adr-bootstrap/pre-promotion.json
(cd abc && bin/kaocha --focus abc.tools.adr-evidence-bootstrap-test \
  --focus abc.tools.adr-evidence-capture-test \
  --focus abc.tools.adr-0034-evidence-test)
(cd abc && clojure -M:abc/adr-evidence-bootstrap -- \
  --repo-root "$workspace_root/abc" --workspace-root "$workspace_root" \
  --verify docs/evidence/adr-bootstrap/pre-promotion.json)
git add abc/docs/evidence/adr-bootstrap/pre-promotion.json \
  abc/docs/evidence/adr-capture/adr-0034-c1.edn \
  abc/docs/evidence/adr-capture/adr-0034-c2.edn \
  abc/docs/evidence/adr-capture/adr-0034-c3.edn \
  abc/docs/evidence/adr-entries/adr-0034.edn \
  abc/test/abc/tools/adr_evidence_capture_test.clj
git commit -m "docs(adr): freeze pre-promotion governance snapshot"
rm -rf "$bootstrap_stage"
```

Expected: all checks pass and the tree is clean.

---

### Task 4: Build a detached clean post-promotion capture revision

**Files:**
- Modify only in detached worktree: `abc/docs/adr/0034-typed-evidence-and-lifecycle-closure.md`
- Regenerate only in detached worktree: migration inventory and registered Mermaid views.

**Interfaces:**
- Consumes: clean pre-promotion HEAD from Task 3.
- Produces: a detached clean Stage-A commit with Accepted ADR 0034 and current views while the root gate remains audit mode. It never enters integration-branch history.

- [ ] **Step 1: Create persistent external state and a detached worktree**

```bash
workspace_root="$(git rev-parse --show-toplevel)"
state_dir="${TMPDIR:-/tmp}/soranoha-adr-plan6"
rm -rf "$state_dir"
mkdir -p "$state_dir/bundles"
pre_head="$(git -C "$workspace_root" rev-parse HEAD)"
candidate_dir="$state_dir/worktree"
git -C "$workspace_root" worktree add --detach "$candidate_dir" "$pre_head"
printf 'workspace_root=%q\nstate_dir=%q\npre_head=%q\ncandidate_dir=%q\n' \
  "$workspace_root" "$state_dir" "$pre_head" "$candidate_dir" \
  > "$state_dir/state.env"
test -z "$(git -C "$workspace_root" status --porcelain --untracked-files=all)"
test -z "$(git -C "$candidate_dir" status --porcelain --untracked-files=all)"
```

- [ ] **Step 2: Promote ADR 0034 only inside the detached candidate**

Set `Status: Accepted`, add `Accepted: 2026-07-12`, set `Validation scope:
full-corpus`, retain `Release authority: none`, and update Implementation Status
to distinguish C1/C2 structural evidence, the immutable C3 snapshot, and final
live enforcement. Replace C3 with:

```markdown
- **ADR-0034-C3 — corpus-behavior:** The immutable pre-promotion snapshot of
  every Accepted ADR except ADR 0034 records the exact corpus and criterion
  counts, committed governance epoch, `ok = true`, and zero strict problems;
  final promotion additionally requires live strict conformance including
  this ADR.
```

Keep root `flake.nix` in audit mode, then run:

```bash
. "${TMPDIR:-/tmp}/soranoha-adr-plan6/state.env"
cd "$candidate_dir/abc"
clojure -M:abc/adr-evidence-inventory -- \
  --output docs/reports/adr-claim-migration-inventory.json
clojure -M:abc/presentation-diagrams all
clojure -M:abc/adr-governance -- \
  --repo-root "$candidate_dir/abc" --workspace-root "$candidate_dir" \
  --mode audit --report /tmp/adr-plan6-stage-a-audit.json
```

Expected: audit reports ADR-0034 missing-evidence debt plus stale input/hash
roots for exactly `adr-graph-contract.json`, `architecture-graph-contract.json`,
`diagram-registry-drift.json`, and `adr-policy-fixtures.json`. Those are the
four bundles Task 5 recaptures. No header, lifecycle, dependency, or generated-
view problem is allowed.

- [ ] **Step 3: Commit the detached clean Stage-A capture revision**

```bash
. "${TMPDIR:-/tmp}/soranoha-adr-plan6/state.env"
git -C "$candidate_dir" add abc/docs/adr/0034-typed-evidence-and-lifecycle-closure.md \
  abc/docs/reports/adr-claim-migration-inventory.json
git -C "$candidate_dir" add -u abc/docs
git -C "$candidate_dir" diff --cached --check
git -C "$candidate_dir" commit -m \
  "temp: stage ADR 0034 promotion evidence inputs"
candidate_commit="$(git -C "$candidate_dir" rev-parse HEAD)"
printf 'candidate_commit=%q\n' "$candidate_commit" >> "$state_dir/state.env"
test -z "$(git -C "$candidate_dir" status --porcelain --untracked-files=all)"
test "$(git -C "$workspace_root" rev-parse HEAD)" = "$pre_head"
```

Expected: the integration worktree remains byte-identical at `pre_head`.

---

### Task 5: Capture seven bundles and assemble the enforced final tree

**Files:**
- Modify: `docs/adr/0034-typed-evidence-and-lifecycle-closure.md`
- Modify: `docs/adr/adr-evidence.edn`
- Create: `docs/evidence/adr-runs/adr-0034-c1.json`
- Create: `docs/evidence/adr-runs/adr-0034-c2.json`
- Create: `docs/evidence/adr-runs/adr-0034-c3.json`
- Replace: `docs/evidence/adr-runs/adr-graph-contract.json`
- Replace: `docs/evidence/adr-runs/architecture-graph-contract.json`
- Replace: `docs/evidence/adr-runs/diagram-registry-drift.json`
- Replace: `docs/evidence/adr-runs/adr-policy-fixtures.json`
- Modify: root `flake.nix`
- Modify: `test/abc/tools/adr_governance_test.clj`
- Regenerate: `docs/reports/adr-claim-migration-inventory.json`
- Regenerate: `docs/reports/adr-evidence-migration.json`
- Regenerate: `docs/adr/adr-graph.mmd` and affected registered views.

**Interfaces:**
- Consumes: clean detached `candidate_commit` and Task 3 descriptors/templates.
- Produces: one clean final detached commit parented directly to `pre_head`; all seven bundles record `candidate_commit` as producer revision.

- [ ] **Step 1: Capture ADR 0034 C1/C2/C3 from the monorepo root**

```bash
. "${TMPDIR:-/tmp}/soranoha-adr-plan6/state.env"
for stem in adr-0034-c1 adr-0034-c2 adr-0034-c3; do
  (cd "$candidate_dir/abc" && clojure -M:abc/adr-evidence-capture -- \
    --repo-root "$candidate_dir" \
    --descriptor "abc/docs/evidence/adr-capture/${stem}.edn" \
    --output "$state_dir/bundles/${stem}.json")
  jq -e '.observations | length == 1
         and ([.observations[].value] | all)' \
    "$state_dir/bundles/${stem}.json"
done
test -z "$(git -C "$candidate_dir" status --porcelain --untracked-files=all)"
```

- [ ] **Step 2: Recapture the four Plan 5 bundles staled by ADR/view changes**

Plan 5 descriptors are ABC-root-relative ordinary profiles. Invoke them from
`candidate_dir/abc` without the monorepo `--repo-root` flag:

```bash
. "${TMPDIR:-/tmp}/soranoha-adr-plan6/state.env"
for stem in adr-graph-contract architecture-graph-contract \
  diagram-registry-drift adr-policy-fixtures; do
  (cd "$candidate_dir/abc" && clojure -M:abc/adr-evidence-capture -- \
    --descriptor "docs/evidence/adr-capture/${stem}.edn" \
    --output "$state_dir/bundles/${stem}.json")
  jq -e '.observations | length == 1
         and ([.observations[].value] | all)' \
    "$state_dir/bundles/${stem}.json"
done
test -z "$(git -C "$candidate_dir" status --porcelain --untracked-files=all)"
```

Do not recapture `workflow-graph-fixtures.json`; none of its inputs changed.

- [ ] **Step 3: Prove C3 is non-self-referential before installation**

```bash
. "${TMPDIR:-/tmp}/soranoha-adr-plan6/state.env"
jq -e '
  (.inputs | has("abc/docs/adr/adr-evidence.edn") | not) and
  (.inputs | has("abc/docs/adr/0034-typed-evidence-and-lifecycle-closure.md") | not) and
  (.inputs | has("abc/docs/reports/adr-evidence-migration.json") | not) and
  (.inputs | has("abc/docs/evidence/adr-runs/adr-0034-c3.json") | not)
' "$state_dir/bundles/adr-0034-c3.json"
```

Expected: true. C3 binds only the immutable snapshot/schema/validator closure.

- [ ] **Step 4: Install exact bytes and register Plan 5 before ADR 0034**

```bash
. "${TMPDIR:-/tmp}/soranoha-adr-plan6/state.env"
for stem in adr-0034-c1 adr-0034-c2 adr-0034-c3 \
  adr-graph-contract architecture-graph-contract \
  diagram-registry-drift adr-policy-fixtures; do
  install -m 0644 "$state_dir/bundles/${stem}.json" \
    "$candidate_dir/abc/docs/evidence/adr-runs/${stem}.json"
done
(cd "$candidate_dir/abc" && clojure -M:abc/adr-evidence-register -- \
  --workspace-root "$candidate_dir" \
  --entries docs/evidence/adr-entries/diagrams-governance.edn \
  --registry docs/adr/adr-evidence.edn)
(cd "$candidate_dir/abc" && clojure -M:abc/adr-evidence-register -- \
  --workspace-root "$candidate_dir" \
  --entries docs/evidence/adr-entries/adr-0034.edn \
  --registry docs/adr/adr-evidence.edn)
```

The order matters: Plan 5 hashes are refreshed while the registrar tolerates
only ADR-0034 missing-evidence debt; then the ADR-0034 registrar closes it.

- [ ] **Step 5: Write the enforcement-selection test, then switch root policy**

Change the governance test to require root `../flake.nix`'s
`monorepo-adr-governance` derivation to set `src = self;` and invoke
`--repo-root "$src/abc" --workspace-root "$src" --mode enforce`, and not
`--mode audit`. Run
`cd "$candidate_dir/abc" && bin/kaocha --focus
abc.tools.adr-governance-test` and require failure while the root check remains
audit. Then change only
the root check body from `--mode audit` to `--mode enforce`, retain
`--report "$out/report.json"`, update its result message to say enforcement
passed, run `nixfmt flake.nix` from `candidate_dir`, and rerun the same focused
test expecting zero failures.

- [ ] **Step 6: Regenerate and verify the complete live strict state**

```bash
. "${TMPDIR:-/tmp}/soranoha-adr-plan6/state.env"
cd "$candidate_dir/abc"
clojure -M:abc/adr-evidence-inventory -- \
  --output docs/reports/adr-claim-migration-inventory.json
clojure -M:abc/presentation-diagrams all
clojure -M:abc/adr-governance -- \
  --repo-root "$candidate_dir/abc" --workspace-root "$candidate_dir" \
  --mode enforce --report docs/reports/adr-evidence-migration.json
jq -e '.mode == "enforce" and .ok == true and (.problems | length) == 0' \
  docs/reports/adr-evidence-migration.json
clojure -M:abc/adr-evidence-bootstrap -- \
  --repo-root "$candidate_dir/abc" --workspace-root "$candidate_dir" \
  --verify-final docs/evidence/adr-bootstrap/pre-promotion.json
```

- [ ] **Step 7: Prove registrar idempotence and squash directly onto pre-head**

```bash
. "${TMPDIR:-/tmp}/soranoha-adr-plan6/state.env"
cp "$candidate_dir/abc/docs/adr/adr-evidence.edn" \
  "$state_dir/registry-before-idempotence.edn"
(cd "$candidate_dir/abc" && clojure -M:abc/adr-evidence-register -- \
  --workspace-root "$candidate_dir" \
  --entries docs/evidence/adr-entries/diagrams-governance.edn \
  --registry docs/adr/adr-evidence.edn)
(cd "$candidate_dir/abc" && clojure -M:abc/adr-evidence-register -- \
  --workspace-root "$candidate_dir" \
  --entries docs/evidence/adr-entries/adr-0034.edn \
  --registry docs/adr/adr-evidence.edn)
cmp -s "$state_dir/registry-before-idempotence.edn" \
  "$candidate_dir/abc/docs/adr/adr-evidence.edn"
git -C "$candidate_dir" add -A
git -C "$candidate_dir" diff --cached --check
git -C "$candidate_dir" reset --soft "$pre_head"
git -C "$candidate_dir" commit -m \
  "feat(adr): enforce artifact-backed governance"
final_commit="$(git -C "$candidate_dir" rev-parse HEAD)"
printf 'final_commit=%q\n' "$final_commit" >> "$state_dir/state.env"
test "$(git -C "$candidate_dir" rev-parse HEAD^)" = "$pre_head"
test -z "$(git -C "$candidate_dir" status --porcelain --untracked-files=all)"
```

The bundles retain the detached Stage-A `candidate_commit` producer revision.
The final commit has `pre_head` as its direct parent; input hashes remain the
authority, so no commit-hash fixed point is introduced.

---

### Task 6: Verify the clean final commit, fast-forward once, and clean up

**Files:** Verification and branch update only.

**Interfaces:**
- Consumes: clean detached `final_commit` parented to `pre_head`.
- Produces: one atomic integration-branch commit or leaves the branch byte-identical to `pre_head`.

- [ ] **Step 1: Verify final counts and focused protocol behavior**

```bash
. "${TMPDIR:-/tmp}/soranoha-adr-plan6/state.env"
(cd "$candidate_dir/abc" && clojure -M:abc/adr-evidence-bootstrap -- \
  --repo-root "$candidate_dir/abc" --workspace-root "$candidate_dir" \
  --verify-final docs/evidence/adr-bootstrap/pre-promotion.json)
(cd "$candidate_dir/abc" && bin/kaocha \
  --focus abc.tools.adr-0034-evidence-test \
  --focus abc.tools.adr-evidence-bootstrap-test \
  --focus abc.tools.adr-test \
  --focus abc.tools.adr-evidence-test \
  --focus abc.tools.adr-evidence-bundle-test \
  --focus abc.tools.adr-evidence-capture-test \
  --focus abc.tools.adr-evidence-inventory-test \
  --focus abc.tools.adr-governance-test \
  --focus abc.tools.diagram.adr-graph-test \
  --focus abc.tools.diagram.registry-test)
```

Expected: `--verify-final` proves Accepted count = snapshot + 1, criterion
count = snapshot + 3, empty strict governance, and root enforcement selection;
all focused tests pass, including invalid-fixture audit/enforce parity.

- [ ] **Step 2: Run strict Nix gates against the clean detached final commit**

```bash
. "${TMPDIR:-/tmp}/soranoha-adr-plan6/state.env"
system="$(nix eval --impure --raw --expr builtins.currentSystem)"
(cd "$candidate_dir" && nix build \
  ".#checks.${system}.monorepo-adr-governance" --no-link --print-build-logs)
(cd "$candidate_dir" && nix build \
  "./abc#checks.${system}.diagram-drift" \
  "./abc#checks.${system}.presentation-diagram-drift" \
  "./abc#checks.${system}.clj-kondo" \
  "./abc#checks.${system}.clj-nix-focused-tests" \
  --no-link --print-build-logs)
```

Expected: all builds succeed; ADR governance output says enforcement passed.

- [ ] **Step 3: Run the full root migration gate on the same final commit**

```bash
. "${TMPDIR:-/tmp}/soranoha-adr-plan6/state.env"
(cd "$candidate_dir" && just validate-migration)
test -z "$(git -C "$candidate_dir" status --porcelain --untracked-files=all)"
```

Expected: exit 0 across ABC, ab-validator, Python, Nix formatting, and migration checks.

- [ ] **Step 4: Fast-forward the integration branch exactly once**

```bash
. "${TMPDIR:-/tmp}/soranoha-adr-plan6/state.env"
test "$(git -C "$workspace_root" rev-parse HEAD)" = "$pre_head"
test -z "$(git -C "$workspace_root" status --porcelain --untracked-files=all)"
git -C "$workspace_root" merge --ff-only "$final_commit"
test "$(git -C "$workspace_root" rev-parse HEAD)" = "$final_commit"
test "$(git -C "$workspace_root" rev-parse HEAD^)" = "$pre_head"
```

Expected: one commit lands on the integration branch, directly parented to the
pre-promotion checkpoint. The temporary Stage-A commit is not an ancestor.

- [ ] **Step 5: Verify live enforcement, then remove temporary state**

```bash
. "${TMPDIR:-/tmp}/soranoha-adr-plan6/state.env"
(cd "$workspace_root" && nix run ./abc#adr-governance -- \
  --repo-root "$workspace_root/abc" --workspace-root "$workspace_root" \
  --mode enforce \
  --report /tmp/adr-final-enforce.json)
jq -e '.ok == true and (.problems | length) == 0' \
  /tmp/adr-final-enforce.json
git -C "$workspace_root" worktree remove "$candidate_dir"
rm -rf "$state_dir"
test -z "$(git -C "$workspace_root" status --porcelain --untracked-files=all)"
```

### Abort protocol

Before Step 4's fast-forward, any failure leaves the integration branch at
`pre_head`. Remove the detached worktree with `git -C "$workspace_root"
worktree remove --force "$candidate_dir"`, delete `state_dir`, and repair
forward from the clean integration worktree. After fast-forward, do not reset
or weaken evidence; the same final commit already passed all gates, so diagnose
and repair forward.

---

## Plan Self-Review

- Spec coverage: immutable snapshot/schema/validator, exact pre-promotion subject/counts/epoch/report, C1/C2/C3 bundles, invalid-fixture parity, atomic promotion/registry/report/view/Nix switch, strict Nix checks, and root validation each have an owning task.
- Non-self-reference: C3 binds only immutable snapshot validation inputs. The final registry/report/ADR 0034 and C3 bundle itself are excluded because they are outside its explicitly pre-promotion subject, not through a validator exception.
- Audit honesty: snapshot generation uses strict result content and `enforce` semantics; audit exit zero is never consumed as a Boolean pass.
- Type consistency: C1/C2 use `:structural-test`; C3 uses `:corpus-measurement`; observation keys and artifact stems match descriptors, bundles, and joins.
- Atomicity: descriptors/snapshot are safely committed while ADR 0034 is
  Proposed. A detached clean candidate supplies post-promotion input hashes for
  the four stale Plan 5 bundles, but never becomes integration history; the
  only branch commit that marks ADR 0034 Accepted also contains the recaptures,
  all joins, reports/views, and enforcement.
- Placeholder scan: canonical hashes are execution-produced by plan 1's registrar from exact Task 5 bundle bytes; no hand-computed, templated, or file-byte hash is permitted.
