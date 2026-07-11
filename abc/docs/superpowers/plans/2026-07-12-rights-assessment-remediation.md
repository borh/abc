# Rights Assessment Remediation Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Stop unsupported external rights assertions and migrate work/person metadata to provenance-bearing rights assessments.

**Architecture:** Consume the foundation-owned source-assertion envelope, land a small containment change, then investigate the upstream contract and implement the rights-assessment schema. External IRIs are optional derived assertions guarded by evidence-backed rules.

**Tech Stack:** Clojure, JSON Schema 2020-12, Jena RDF/SHACL, `clojure.test.check`, ADR typed evidence.

## Global Constraints

- Follow `2026-07-12-remediation-program-sequencing.md`; consume the completed
  shared source-assertion foundation and reserve ADR 0034.
- Requires governance audit mode for containment and enforced governance before schema promotion.
- Never infer `InC` from false, nil, absent, malformed, or unknown input.
- Published manifests are immutable; `--refresh-manifest` is development-fixture-only.
- Tasks 1–3 may proceed while simulation hardening is in flight. Do not execute
  Task 4's `aozora_ingest.clj` or ingest-test changes until
  `fix/aozora-evolution-divergences` is merged and both ABC unit and simulation
  suites pass on the integrated branch.
- Final gate is `just validate-migration`.

---

## File Structure

- `abc/schemas/source-assertion.schema.json` — shared provenance envelope supplied by the foundation plan; do not modify without coordinating temporal consumers.
- `abc/schemas/rights-assessment.schema.json` — source knowledge plus assessment.
- `abc/src/abc/tools/rights_assessment.clj` — validation/mapping/RDF decision boundary.
- Existing CSV, ingest, metadata/person, SHACL, schema, and test files — consumers.
- `abc/docs/evidence/aozora-rights-source-contract.md` — bounded authoritative evidence.
- `abc/docs/adr/0034-rights-assessment-and-external-statements.md` — corrective ADR.
- `abc/docs/reports/rights-assessment-migration.json` — deterministic migration report.
- `abc/data/publication-policy.edn` — explicit temporary rights-publication blocker.

### Task 1: Contain unconditional rights emission

- [ ] Add failing metadata-record tests for false and nil showing current `rights-iri` emits `InC`.
- [ ] Change RDF emission so no `dcterms:rights` triple is emitted without an explicit assessment mapping; retain an ABC-local source assertion where available.
- [ ] Add SHACL fixtures for source-only rights and remove the exactly-one two-value constraint.
- [ ] Set `:rights-publication :blocked-pending-assessment-migration` in the publication policy and make publication/citation-set commands fail closed with that reason. Development fixture validation remains allowed.
- [ ] Add a test proving a publication attempt is blocked during containment; document that legitimate public-domain triples are temporarily absent and must not be released in this window.
- [ ] Run metadata and SHACL focused tests; expect zero failures.
- [ ] Record the containment amendment under governance audit mode and commit `fix(rights): stop unsupported external rights assertions`.

### Task 2: Establish authoritative source semantics

- [ ] Gather the Aozora field documentation, exact lexical values, work/person differences, jurisdiction, and effective-time semantics into the evidence document.
- [ ] Hash the evidence document and register it as `:external-authority` with retrieval/review dates and source snapshot bindings.
- [ ] Decide mappings strictly by the spec's two outcomes: exact match or source-only. Ambiguity selects source-only.
- [ ] Add the resulting mapping table to ADR 0034; do not place interpretation only in code comments.
- [ ] Commit `docs(rights): record Aozora rights source semantics`.

### Task 3: Add the rights-assessment schema and pure assessment module

- [ ] Write failing schema/module tests consuming the already-registered shared envelope, the four source knowledge states, four assessment statuses, split `status_effective_at`/`assessed_at`, and conditional `statement_iri`.
- [ ] Implement `parse-source-assertion`, `assess`, `external-statement-iri`, and `validation-errors` as pure functions.
- [ ] Add a property test that no non-positive assessment status can produce an external IRI.
- [ ] Run focused tests and schema validation; expect zero failures.
- [ ] Commit `feat(rights): add provenance-bearing rights assessments`.

### Task 4: Migrate ingest, records, RDF, and SHACL

- [ ] Confirm the simulation-hardening branch is merged; run the ABC unit and
  simulation suites and require zero failures/errors before editing ingest.
- [ ] Characterize and preserve its two-tier failure taxonomy, empty/non-ZIP
  source guards, and build/reconcile/write safety behavior.
- [ ] Replace work/person Boolean production in `aozora_csv.clj` and `aozora_ingest.clj` with the new value while retaining legacy readers.
- [ ] Rotate metadata/person schemas and update `metadata_record.clj`, `person_record.clj`, RDF fixtures, and SHACL conditional rules.
- [ ] Restrict `--refresh-manifest` to paths explicitly marked unpublished; reject release/citation-set manifests.
- [ ] Add a `test.check` partition property: every input record occurs exactly once in an output state or quarantine.
- [ ] Generate the migration report with state/jurisdiction counts and old/new hashes/derivation IDs.
- [ ] After migration and RDF/SHACL tests pass, change the publication policy to `:rights-publication :assessment-required` and prove publication rejects records lacking a valid assessment while permitting migrated records.
- [ ] Commit `feat(rights): migrate metadata and publication views`.

### Task 5: Verify and promote

- [ ] Run focused CSV, ingest, metadata, person, SHACL, and manifest tests; require zero failures and zero errors.
- [ ] Run `cd abc && clojure -M:abc/validate-design-bundle`; require exit 0 with schema, RDF, and SHACL gates reported successful.
- [ ] Run `just validate-migration`; require exit 0 from all root and component checks.
- [ ] Verify historical manifests against their original schemas and the migration partition totals.
- [ ] Promote ADR 0034 only when typed evidence and full migration results pass; commit `docs(adr): accept rights assessment contract`.
