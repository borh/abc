# ADR Evidence and Lifecycle Remediation Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Enforce typed claim evidence, evidence freshness, explicit validation/release scope, and Accepted-only transitive ADR dependencies.

**Architecture:** Keep Markdown parsing in `abc.tools.adr`; add a focused `abc.tools.adr-evidence` module for the checked-in evidence protocol. Introduce the policy in audit mode, migrate the corpus, self-validate the governance ADR, then enable enforcement.

**Tech Stack:** Clojure 1.12, EDN, `clojure.test`, `clojure.test.check`, Kaocha, Nix checks.

## Global Constraints

- Follow `2026-07-12-remediation-program-sequencing.md`; this plan runs after
  the shared foundation and owns reserved ADR 0033.
- Implement `2026-07-11-adr-evidence-and-lifecycle-remediation-design.md` exactly.
- Keep graph traversal in ordinary deterministic Clojure; logic/solver models
  are review evidence and must not become production dependencies.
- Do not edit existing ADR Decisions in place; normative changes use new ADRs.
- Preserve unrelated worktree changes.
- Run focused tests with `cd abc && bin/kaocha --focus <namespace>`.
- Final gate is `just validate-migration` from the monorepo root.

---

## File Structure

- `abc/docs/adr/claim-evidence-compatibility.edn` — closed claim/evidence matrix.
- `abc/docs/adr/adr-evidence.edn` — claim evidence registry.
- `abc/docs/adr/governance-as-of.edn` — pinned reference date for deterministic expiry.
- `abc/src/abc/tools/adr_evidence.clj` — pure registry validation and predicate evaluation.
- `abc/src/abc/tools/adr.clj` — lifecycle fields and dependency closure.
- `abc/src/abc/tools/adr_governance.clj` — audit/enforce orchestration and report output.
- `abc/test/abc/tools/adr_evidence_test.clj` — protocol and property tests.
- `abc/test/abc/tools/adr_test.clj` — header and graph-closure tests.
- `abc/docs/adr/0033-typed-evidence-and-lifecycle-closure.md` — governing ADR.

### Task 1: Define the evidence protocol

**Files:** Create the matrix, registry, module, and `adr_evidence_test.clj`.

**Interfaces:** Produce `load-matrix`, `load-registry`, `validate-entry`, `evaluate`, and `validate-registry`, each returning values/problems without exiting.

- [ ] Write failing tests covering all eight claim kinds, all admissible pairs, unknown kinds, incompatible pairs, derived verdicts, stale input hashes, and external evidence expired relative to an explicit `LocalDate` argument.
- [ ] Run `cd abc && bin/kaocha --focus abc.tools.adr-evidence-test`; expect failures because the namespace/files do not exist.
- [ ] Create the exact matrix and `governance-as-of.edn`; implement predicate operators `:=`, `:not=`, `:<`, `:<=`, `:>`, `:>=`, `:contains`, and `:set=`. Load the pinned date once and pass it explicitly to freshness validation; never call the wall clock. Do not accept a stored `:verdict` key.
- [ ] Add a `test.check` property: for any comparable observed/expected scalar, `evaluate` agrees with the corresponding Clojure predicate and never trusts an input verdict.
- [ ] Re-run the focused tests; expect zero failures.
- [ ] Commit only these files with `git commit -m "feat(adr): define typed claim evidence protocol"`.

### Task 2: Install audit-mode orchestration before strict policy

**Files:** Modify `adr_governance.clj`, `deps.edn`, `flake.nix`, and tests; create `abc/docs/reports/adr-evidence-migration.json` through the command, not by hand.

- [ ] Write a failing command test asserting `--mode audit --report PATH` exits successfully while recording every problem, `--mode enforce` fails on problems, and omitted mode preserves the current legacy behavior during migration.
- [ ] Implement deterministic JSON reporting with ADR, claim, problem kind, evidence path, and dependency path fields.
- [ ] Change the Nix `adr-governance` check to invoke `--mode audit`; run the check and require exit 0 plus a nonempty audit report.
- [ ] Commit command, Nix, and report support with `git commit -m "feat(adr): add evidence governance audit mode"`.

### Task 3: Add lifecycle dimensions and dependency closure in audit mode

**Files:** Modify `adr.clj` and `adr_test.clj`.

**Interfaces:** Extend parsed ADR values with `:validation-scope` and `:release-authority`; produce `:noncanonical-dependency-path` containing the full path. These problems are reported but do not fail the Nix audit gate until Task 5.

- [ ] Write failing tests for missing/invalid lifecycle fields on Accepted ADRs and for direct/transitive Accepted-to-Proposed dependencies.
- [ ] Add a generated cyclic dependency fixture proving closure traversal terminates and reports paths deterministically.
- [ ] Run the focused ADR tests; expect the new assertions to fail.
- [ ] Add the two closed header fields and replace the scoped waiver with
  deterministic visited-set breadth-first closure over `:depends-on`; emit the
  shortest stable witness path for each noncanonical target.
- [ ] Re-run tests; expect zero failures and the current corpus to report ADR 0031 → ADR 0029.
- [ ] Run the Nix audit gate; require exit 0 while its report contains the ADR 0031 → ADR 0029 path.
- [ ] Commit with `git commit -m "feat(adr): audit lifecycle scope and dependency closure"`.

### Task 4: Author ADR 0033 and repair canonical dependency closure

**Files:** Create ADR 0033 and repair ADR 0031/0029 by extracting or accepting only the implemented graph contract.

- [ ] Add ADR 0033 as Proposed with structural evidence for the validator and full-corpus evidence as a promotion condition.
- [ ] Resolve the only current noncanonical dependency and run audit mode; require no `:noncanonical-dependency-path` problems.
- [ ] Commit the ADR/graph repair separately with `git commit -m "docs(adr): define typed governance and close canonical dependencies"`.

### Task 5: Migrate evidence corpus and enable enforcement

**Files:** Modify ADR headers/criteria and both EDN registries; update the Nix mode.

- [ ] Add validation scope, release authority, stable claim IDs, input bindings, and evidence entries to every Accepted ADR without weakening historical criteria.
- [ ] Replace overclaiming `prove/proves` language where the claim is non-structural.
- [ ] Run audit mode; expect an empty problem list.
- [ ] Switch the Nix `adr-governance` check from audit to enforcement, rerun governance, and require exit 0 with an empty problem array.
- [ ] Promote ADR 0033 only if its own new-regime evidence passes.
- [ ] Commit corpus migration with `git commit -m "docs(adr): migrate acceptance evidence and lifecycle metadata"`.

### Task 6: Verify the governance workstream

- [ ] Run the two focused test namespaces; require Kaocha to report zero failures and zero errors.
- [ ] Run `cd abc && clojure -M:abc/adr-governance --mode enforce`; require exit 0 and `ADR governance valid`.
- [ ] Run `cd abc && nix build .#checks.x86_64-linux.adr-governance`; require exit 0 and a result containing the governance success marker.
- [ ] Run `just validate-migration` from the root; require exit 0 from every prerequisite and nested flake evaluation.
- [ ] Commit only generated report/hash updates with `git commit -m "test(adr): verify typed governance migration"`.
