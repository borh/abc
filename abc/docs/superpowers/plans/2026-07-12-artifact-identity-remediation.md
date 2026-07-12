# Artifact Identity Remediation Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Separate derivation, content, subject, release, and equivalence identity while preserving historical `artifact_id` compatibility.

**Architecture:** Alias the existing hash to `derivation_id`, add an explicit artifact-kind coordinate matrix, and store cross-generation assertions as separate provenance artifacts. Publish rights and temporal mapping families independently.

**Tech Stack:** Clojure, JSON Schema, JCS/SHA-256, EDN policy, graph validation, `clojure.test.check`.

## Global Constraints

- Follow `2026-07-12-remediation-program-sequencing.md`; begin after governance
  enforcement, consume either migration family independently, and reserve ADR 0036.
- Do not change the existing derivation hash algorithm.
- `artifact_id` remains an exact compatibility alias during migration.
- Equivalence assertions never feed linked derivation identities.
- Use union-find and deterministic DAG/topological algorithms in production;
  logic/solver models remain independent review evidence.
- Historical manifests are immutable.
- Task 1 inventory may proceed immediately after governance enforcement. Tasks
  2–4 use the integrated main branch, including Phase 4 at `ac2be926`; do not
  derive coordinate applicability or generation links from the unmerged
  simulation worktree state.

---

## File Structure

- `abc/data/artifact-kind-identity-coordinates.edn` — applicability matrix.
- `abc/schemas/equivalence-assertion.schema.json` — generation-link contract.
- `abc/src/abc/tools/identity_contract.clj` — aliases and coordinate validation.
- `abc/src/abc/tools/equivalence_index.clj` — relation-aware graph builder.
- Existing manifest/index/snapshot schemas and tools — compatibility integration.
- `abc/docs/adr/0036-identity-lattice-and-generation-equivalence.md` — governing ADR.
- `abc/docs/reports/cross-generation-identity-migration.json` — mapping report.

### Task 1: Inventory and classify identifiers

- [ ] Generate a deterministic inventory of every schema field ending in `_id`, `_hash`, or `_iri` and every manifest identity coordinate.
- [ ] Classify each as subject, source-record, component-coordinate, derivation, content, release, or equivalence-assertion identity.
- [ ] Build the artifact-kind coordinate matrix, including analysis kinds where `metadata_record_hash` is intentionally null.
- [ ] Add a failing completeness test comparing schemas to the inventory/matrix.
- [ ] Commit `docs(identity): inventory identifier equality contracts`.

### Task 2: Add the derivation alias safely

- [ ] Write failing manifest tests: old-only accepted, matching dual fields accepted, mismatching dual fields rejected.
- [ ] Add `derivation_id` to the manifest schema and normalize legacy `artifact_id` internally in manifest/index/snapshot tools.
- [ ] Keep reproducibility conflicts keyed by normalized derivation ID and content hashes.
- [ ] Add properties for same coordinates→same derivation ID and differing derivations with same bytes→different derivation IDs/same content hash.
- [ ] Commit `feat(identity): introduce derivation identity alias`.

### Task 3: Implement equivalence relation algebra

- [ ] Write failing tests showing `equivalent-facts` cycles are valid, `supersedes` and `migration-of` cycles fail, and contradictory latest successors fail.
- [ ] Implement equivalence classes with union-find and directed relations with deterministic DAG/topological validation.
- [ ] Validate schema generation monotonicity for `migration-of`.
- [ ] Add `test.check` graph properties and compare small generated graphs to a simple reference model.
- [ ] Commit `feat(identity): add relation-aware generation graph`.

### Task 4: Consume independent migration families

- [ ] Add importers for rights and temporal old/new mapping reports; either may be absent.
- [ ] Validate subject IDs, alias equality, hashes, and relation algebra before adding assertions.
- [ ] Build a deterministic cross-generation index and quarantine unresolved/contradictory links.
- [ ] Verify changing an equivalence assertion changes only its own hash/index, never linked derivation IDs.
- [ ] Commit `feat(identity): publish independent generation mappings`.

### Task 5: Documentation, compatibility, and verification

- [ ] Update public diagrams/docs to use derivation/content/subject/release terminology accurately.
- [ ] Create ADR 0036 with typed evidence and artifact-kind applicability results.
- [ ] Run manifest, manifest-index, snapshot-index, analysis identity, and diagram tests.
- [ ] Run `cd abc && clojure -M:abc/validate-design-bundle`; require exit 0 with manifest and index validation successful.
- [ ] Run `just validate-migration`; require exit 0 from all root and component checks.
- [ ] Promote only after historical reads and cross-generation queries pass; commit `docs(adr): accept explicit identity lattice`.
