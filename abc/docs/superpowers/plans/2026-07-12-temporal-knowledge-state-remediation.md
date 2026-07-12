# Temporal Knowledge-State Remediation Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Replace ambiguous nullable dates with tagged, provenance-bearing temporal knowledge values.

**Architecture:** Reuse the shared source-assertion envelope, keep date parsing pure, and migrate person records through a total/disjoint classifier. Known EDTF behavior remains intact; non-known states never emit fabricated date literals.

**Tech Stack:** Clojure, JSON Schema, EDTF strings, RDF/SHACL, `clojure.test.check`.

## Global Constraints

- Follow `2026-07-12-remediation-program-sequencing.md`; consume the completed
  shared source-assertion foundation and reserve ADR 0036.
- Requires enforced typed governance before promotion.
- Preserve `不詳`/`未詳` as explicit unknown source assertions.
- Empty cells become `not-recorded`; `not-applicable` requires a named rule.
- Historical schemas/manifests remain readable.
- Tasks 1–2 may proceed while simulation hardening is in flight. Do not execute
  Task 3's ingest integration until `fix/aozora-evolution-divergences` is merged
  and both ABC unit and simulation suites pass on the integrated branch.

---

## File Structure

- `abc/schemas/temporal-knowledge.schema.json` — tagged value contract.
- `abc/schemas/source-assertion.schema.json` — foundation-owned dependency; do not create or modify in this plan.
- `abc/src/abc/tools/temporal_knowledge.clj` — classifier and validation.
- Existing CSV/person/ingest/RDF/SHACL files — integration.
- `abc/docs/adr/0036-temporal-knowledge-state.md` — corrective ADR.
- `abc/docs/reports/temporal-knowledge-migration.json` — partition and hashes.

### Task 1: Characterize current temporal behavior

- [ ] Add tests demonstrating `不詳`, `未詳`, and empty input currently collide at null while corrections differ.
- [ ] Preserve existing tests for full, partial, BCE, decade, and century EDTF.
- [ ] Run `cd abc && bin/kaocha --focus abc.tools.aozora-csv-test`; expect only new collision assertions to fail.
- [ ] Commit characterization tests separately as `test(time): characterize nullable knowledge loss`.

### Task 2: Define and implement the tagged value

- [ ] Add schema tests for every valid/invalid state/value/precision combination and the shared source assertion.
- [ ] Implement `classify`, `validation-errors`, and `known-edtf` in `temporal_knowledge.clj`.
- [ ] Map explicit unknown markers to `unknown`, empty cells to `not-recorded`, valid dates to `known`, and nothing to `not-applicable` without a rule argument.
- [ ] Emit a stable lossy-correction diagnostic for sub-century qualifiers.
- [ ] Run focused tests; expect zero failures and unchanged valid EDTF values.
- [ ] Commit `feat(time): add tagged temporal knowledge values`.

### Task 3: Integrate person records and publication views

- [ ] Confirm the simulation-hardening branch is merged; run the ABC unit and
  simulation suites and require zero failures/errors before editing ingest.
- [ ] Preserve its source-vs-work failure taxonomy and no-partial-write
  reconciliation guarantees while changing temporal values.
- [ ] Rotate `person-record.schema.json` and update CSV ingest/person record validation.
- [ ] Update RDF emission: known values emit EDTF; non-known values emit no date literal and may emit an ABC knowledge-state node.
- [ ] Update SHACL to enforce state/value dependency.
- [ ] Add old-schema reader fixtures and new-schema writer fixtures.
- [ ] Run person, ingest, and SHACL tests; expect zero failures.
- [ ] Commit `feat(time): migrate person temporal publication contract`.

### Task 4: Prove migration partition properties

- [ ] Add `test.check` generators for empty, unknown-marker, valid EDTF, invalid prose, and sub-century rows.
- [ ] Assert exactly one of known/unknown/not-recorded/not-applicable/quarantine receives every generated input.
- [ ] Assert bucket union equals input population and intersections are empty for the pinned corpus.
- [ ] Generate the migration report with state counts, corrections, quarantines, and old/new record and derivation hashes.
- [ ] Commit `test(time): verify total temporal migration partition`.

### Task 5: Verify and promote

- [ ] Run focused temporal, CSV, person, ingest, RDF, and SHACL tests; require zero failures and zero errors.
- [ ] Run `cd abc && clojure -M:abc/validate-design-bundle`; require exit 0 with the temporal schema and derived-view gates successful.
- [ ] Run `just validate-migration`; require exit 0 from all root and component checks.
- [ ] Create/promote ADR 0036 only when the typed full-corpus evidence passes.
- [ ] Commit `docs(adr): accept temporal knowledge-state contract`.
