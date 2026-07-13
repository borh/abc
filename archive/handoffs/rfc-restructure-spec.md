# RFC Restructure Spec: Canonical Architecture Surface

> **Scope:** Promote the decided core of `docs/high-level-architecture-note.md` into a canonical `docs/architecture.md`, move surveyed alternatives to `docs/design-survey.md`, reconcile ADR statuses against the green test gate, and leave no source content deleted. This is a promotion-discipline spec; the executor does not delete prose, only relocates or duplicates it with cross-references.

---

## 1. Document Split and Section Classification

### 1.1 Top-level section classification

| Section in `docs/high-level-architecture-note.md` | Line | Classification | Rationale |
|---|---|---|---|
| `Status and Scope` | `13:30` | **SPLIT** | Keep the decision-promotion rule in `architecture.md`; move the "still open" language to `design-survey.md`. |
| `Glossary` | `31:48` | **DECIDED-CORE** | v0 identity terms are adopted by `ADR 0001` and enforced by `validate-design-bundle`. |
| `Project Shape` | `49:59` | **DECIDED-CORE** | Corpus-pipeline framing is the project charter. |
| `Principles and Policies` | `60:77` | **DECIDED-CORE** | Boundary policies are accepted architecture. |
| `Existing Parser Ecosystem` | `78:126` | **SURVEY** | No parser has been selected; criteria live in `ADR 0002` (Draft). |
| `Manifest Specification` | `127:340` | **SPLIT** | Core identity/hash/canonicalization/RDF rules are accepted; ShEx/rudof, SHACL-DS, CID, SLSA/in-toto/Sigstore, and OWL reasoning options are survey. |
| `Packaging, FAIR, and Schema Evolution` | `341:379` | **SPLIT** | FAIR alignment and schema-evolution registry are core; RO-Crate and CID/IPLD are survey. |
| `Pipeline Layers` | `380:412` | **DECIDED-CORE** | Layered invalidation model is implemented/tested. |
| `Parser IR Boundary` | `413:455` | **SPLIT** | JSON schema + warning/error contract are core; CBOR/Arrow/streaming are survey. |
| `Aozora-Specific Design Concerns` | `456:486` | **DECIDED-CORE** | gaiji/ruby/editor-note/bibliographic-drift policies are enforced by schemas and fixtures. |
| `TEI and XML Profile` | `487:542` | **SPLIT** | ODD+RelaxNG+Schematron is core (`ADR 0012`); libxml2 lighter-path and "evaluate before committing to JVM" are survey. |
| `SOTA Alignment Deltas` | `543:557` | **SPLIT / REMOVE-if-redundant** | Distribute implemented rows (Schematron, Linked Art, IIIF) into core sections; retain only the ShEx/rudof row in `design-survey.md`, then remove the table. |
| `Ontology, RDF, and Querying` | `558:602` | **SPLIT** | RDF/PROV-O provenance is core; query-store options (XTDB/Dolt/SQLite/…), OWL engines (Horned-OWL/OWLAPI/Tawny), and Plow.pm are survey. |
| `Corpus Evolution` | `603:634` | **DECIDED-CORE** | Layered invalidation model is accepted. |
| `Concurrency and Programmatic Access` | `635:662` | **SPLIT** | File-bound atomicity and CLI v0 API are core; multi-host CAS/database and REST/gRPC/streaming are survey. |
| `Runtime Orchestration` | `663:682` | **SPLIT** | CLI/script orchestrator and batching expensive runtimes are core; workflow-engine adoption is survey. |
| `Observability and Retention` | `683:720` | **SPLIT** | Run summaries and retention tiers are core; batched full-corpus storage evaluation is survey. |
| `Nix and Materialization` | `721:805` | **SPLIT** | Nix as recipe backend, external index, corpus input policy, and archive triggers are core; cost-envelope acceptance numbers and the "evaluate if blocked" path are survey. |
| `Experiment and ML Reproducibility` | `806:840` | **DECIDED-CORE** | Reproducibility strata are accepted policy. |
| `Downstream Requirement Examples` | `841:862` | **SURVEY** | Pressure tests, not implemented outputs. |
| `Risks and Non-Goals` | `863:910` | **SPLIT** | Threat model and failure manifests are core; SLSA evaluation specifics are survey (`ADR 0004` is Draft). |
| `Candidate Runtime Roles` | `911:943` | **SURVEY** | Explicitly candidate/survey section. |
| `Current Repository Reading` | `944:967` | **DECIDED-CORE** (appendix) | Maps legacy Clojure code to the manifest-first model; keep as a short appendix. |
| `Validation and Testing Strategy` | `968:990` | **DECIDED-CORE** | v0 test gates are implemented. |
| `CI/CD` | `991:1012` | **DECIDED-CORE** | PR gate policy is accepted. |
| `Near-Term Design Questions` | `1013:1027` | **REMOVE-if-redundant** | Superseded by ADRs and `Suggested Next Step`; do not carry into `architecture.md`. |
| `Suggested Next Step` | `1028:1092` | **REMOVE-if-redundant** | Implementation log; migrate any still-open items to `design-survey.md` and archive the rest. |
| `References` | `1093:1141` | **SPLIT** | Core standards (JSON Schema, RFC 8785, PROV-O, SHACL, TEI) stay in `architecture.md`; survey references (RO-Crate, SLSA, Sigstore, ShEx, rudof, CID, Horned-OWL, XTDB, Dolt, etc.) move to `design-survey.md`. |
| `Changelog` | `1142:1170` | **REMOVE-if-redundant** | Archive or drop; `architecture.md` gets a single `Version`/`Last updated` line. |

### 1.2 Proposed `docs/architecture.md` contents (~400-line target)

Copy the following existing blocks into `docs/architecture.md` and trim survey paragraphs:

| Target section in `architecture.md` | Source block | Notes |
|---|---|---|
| Title + version | `high-level-architecture-note.md:1:12` | Drop "Draft RFC" language; call it `ABC Architecture`. |
| Status + Promotion Contract | `high-level-architecture-note.md:13:30` | Keep only the rule that decisions are promoted via Accepted ADRs + green tests; insert the Promotion Contract paragraph below. |
| Glossary | `high-level-architecture-note.md:31:48` | As-is. |
| Project Shape | `high-level-architecture-note.md:49:59` | As-is. |
| Principles and Policies | `high-level-architecture-note.md:60:77` | As-is. |
| Manifest Specification — core identity | `high-level-architecture-note.md:127:340` | **Remove:** ShEx/rudof paragraph (`~272`), SHACL-DS watch paragraph (`~277`), CID/IPLD paragraph in this section (`~199`), SLSA/in-toto/Sigstore provenance paragraph (`~176`). **Keep:** canonical JSON, ArtifactID formula, identity-field table, canonicalization rules, RDF/PROV-O view, JSON Schema gate, SHACL gate (core), signature placeholder. |
| Packaging, FAIR, and Schema Evolution — core | `high-level-architecture-note.md:341:379` | Keep FAIR alignment and schema-evolution registry. Move RO-Crate and CID/IPLD paragraphs to survey. |
| Pipeline Layers | `high-level-architecture-note.md:380:412` | As-is; also add a sentence reflecting the drift-event audit sidecars from `ADR 0020/0021`. |
| Parser IR Boundary — core | `high-level-architecture-note.md:413:455` | Keep JSON/JSON-Schema contract, warnings sidecar, error taxonomy. Move CBOR/Arrow/streaming paragraph to survey. |
| Aozora-Specific Design Concerns | `high-level-architecture-note.md:456:486` | As-is. |
| TEI and XML Profile — core | `high-level-architecture-note.md:487:542` | Keep ODD/profile decision, Jing baseline, RelaxNG+Schematron two-layer gate. Move the libxml2/xmllint lighter-path paragraph to survey. **Update stale text:** the ODD is no longer a stub; it is canonical per `ADR 0012`. |
| Corpus Evolution | `high-level-architecture-note.md:603:634` | As-is. |
| Concurrency and Programmatic Access — core | `high-level-architecture-note.md:635:662` | Keep single-host atomicity and CLI/file API. Move multi-host CAS/database and REST/gRPC/streaming bullets to survey. |
| Runtime Orchestration — core | `high-level-architecture-note.md:663:682` | Keep CLI/script bias and batching expensive runtimes. Move workflow-engine adoption to survey. |
| Observability and Retention — core | `high-level-architecture-note.md:683:720` | Keep run summaries, retention tiers, failure-manifest retention. Move full-corpus batched-storage evaluation to survey. |
| Nix and Materialization — core | `high-level-architecture-note.md:721:805` | Keep Nix-as-backend, external incremental index, corpus input policy, archive triggers. Move cost-envelope acceptance numbers (until measured and accepted) and the "if blocked, fall back to script runner" paragraph to survey. |
| Experiment and ML Reproducibility | `high-level-architecture-note.md:806:840` | As-is. |
| Risks and Non-Goals — core | `high-level-architecture-note.md:863:910` | Keep threat model, failure manifests, license metadata. Move SLSA/L2/L3 specifics to survey (`ADR 0004`). |
| Current Repository Reading | `high-level-architecture-note.md:944:967` | Shorten to a "Legacy code mapping" appendix. |
| Validation and Testing Strategy | `high-level-architecture-note.md:968:990` | As-is; add references to `validate-design-bundle` and `nix flake check`. |
| CI/CD | `high-level-architecture-note.md:991:1012` | As-is. |
| Core References | `high-level-architecture-note.md:1093:1141` | Keep only PROV-O, RFC 8785, JSON Schema 2020-12, SHACL, TEI, Software Heritage. |

### 1.3 Proposed `docs/design-survey.md` contents

Move each of the following blocks into `docs/design-survey.md` as a short section (one paragraph + trigger condition).

| Source block | Trigger for promotion to `architecture.md` |
|---|---|
| `Existing Parser Ecosystem` `high-level-architecture-note.md:78:126` | A parser candidate is accepted by `ADR 0002` and at least one ABC manifest is produced from its output in `examples/`. |
| Manifest Specification — ShEx/rudof paragraph `high-level-architecture-note.md:~272` | An ADR accepts ShEx (or `rudof`) as a required validation language, or an external consumer contractually requires ShEx. |
| Manifest Specification — SHACL-DS paragraph `high-level-architecture-note.md:~277` | An ADR accepts SHACL-DS (or SHACL 1.2) for cross-graph/named-graph validation. |
| Manifest Specification — CID/IPLD paragraph `high-level-architecture-note.md:~199` and `Packaging, FAIR…` CID paragraph `high-level-architecture-note.md:~376` | An ADR accepts CID-style identifiers and the multicodec/content-type story is pinned for ABC artifacts. |
| `Packaging, FAIR, and Schema Evolution` — RO-Crate paragraph `high-level-architecture-note.md:~343` | An ADR accepts RO-Crate 1.2 (Detached Crates / Profile Crates) as a publication package format. |
| `Parser IR Boundary` — CBOR/Arrow/binary encoding paragraph `high-level-architecture-note.md:~448` | A concrete consumer or performance benchmark shows JSON is a bottleneck and an ADR accepts a secondary encoding. |
| `TEI and XML Profile` — libxml2/xmllint lighter path `high-level-architecture-note.md:~528` | The lighter validator passes the same fixture corpus as Jing and an ADR (or `ADR 0012` amendment) accepts it. |
| `SOTA Alignment Deltas` — ShEx/rudof row `high-level-architecture-note.md:543:557` | Same trigger as the ShEx/rudof paragraph above. |
| `Ontology, RDF, and Querying` — OWL engine options (Horned-OWL vs OWLAPI/Tawny) `high-level-architecture-note.md:~593` | A concrete inference task appears and an ADR selects an OWL implementation. |
| `Ontology, RDF, and Querying` — query-store options (XTDB/Dolt/SQLite/RDF store/DataFusion) `high-level-architecture-note.md:~596` | An ADR selects a query runtime after access-pattern evidence is collected. |
| `Ontology, RDF, and Querying` — ontology packaging (Plow.pm) `high-level-architecture-note.md:~600` | ABC publishes reusable vocabulary/ODD and an ADR evaluates distribution mechanisms. |
| `Concurrency and Programmatic Access` — multi-host publication protocols and REST/gRPC/streaming `high-level-architecture-note.md:~653` | An API/runtime ADR is accepted. |
| `Runtime Orchestration` — workflow-engine adoption `high-level-architecture-note.md:~678` | The CLI/script orchestrator fails on a real coordination problem and an ADR selects a workflow engine. |
| `Observability and Retention` — full-corpus batched-storage evaluation `high-level-architecture-note.md:~715` | A release candidate exceeds inode/directory-traversal budgets and an ADR selects a packed storage format. |
| `Nix and Materialization` — cost-envelope acceptance criteria and fallback-to-script path `high-level-architecture-note.md:~760` | Cost envelope is measured and accepted, or Nix is demonstrably blocked and an ADR records the fallback. |
| `Risks and Non-Goals` — SLSA/in-toto/Sigstore specifics `high-level-architecture-note.md:~889` | `ADR 0004` is accepted and release verification is implemented. |
| `Candidate Runtime Roles` `high-level-architecture-note.md:911:943` | Each role is promoted individually when an ADR selects the implementation. |
| `Downstream Requirement Examples` `high-level-architecture-note.md:841:862` | Each example is promoted when a corresponding consumer/test is added to the bundle. |
| Survey references block | Extract survey references from `high-level-architecture-note.md:1093:1141` and keep them with their topics. |

---

## 2. Promotion Contract

Insert the following paragraph as the first subsection after the title/status block in `docs/architecture.md`:

> **Promotion Contract.** A surveyed alternative described in `docs/design-survey.md` is promoted into `docs/architecture.md`, and acquires implementation obligation, only when **(a)** an ADR recording the decision is accepted, or **(b)** a concrete consumer or test in the v0 design bundle forces the behavior (e.g., a fixture gate, a schema check, or an external contract). Until one of those conditions is met, surveyed items are documentation-only evaluation records and carry no implementation obligation. When promotion occurs, the executor updates the relevant ADR status to `Accepted`, adds an `Implementation Status` section if the bundle already exercises it, and ensures `nix run .#validate-design-bundle` remains green.

---

## 3. ADR Status Reconciliation

The canonical green gate is `nix run .#validate-design-bundle` (`flake.nix:98:110`, `deps.edn:90`), which exercises the checks in `src/abc/tools/validate_design_bundle.clj`.

| Filename | Current status | Evidence it is implemented | Proposed status | Reconciliation action |
|---|---|---|---|---|
| `0001-manifest-identity.md:3` | Accepted | `validate-design-bundle` validates canonicalization fixtures (`src/abc/tools/validate_design_bundle.clj:148:162`) and checks `artifact_id` stays outside the identity object (`test/abc/tools/validate_design_bundle_test.clj`). | Accepted | None. |
| `0002-parser-evaluation.md:3` | Draft | No candidate reports exist; `validate-design-bundle` does not execute a parser. The only parser-related gate validates the imported `ab-validator` fixture boundary. | Draft | Keep Draft; add note that implementation is blocked on `../ab-validator` candidate reports. |
| `0003-nix-materialization.md:3` | Draft | Nix apps exist (`flake.nix:98:184`), but the cost-envelope acceptance criteria and smoke-corpus policy are not yet implemented. | Draft | Keep Draft; add justification note referencing unmeasured cost envelope. |
| `0004-supply-chain-release-security.md:3` | Draft | Manifest schema reserves `signatures` (`schemas/manifest.schema.json`); no release verification workflow exists. | Draft | Keep Draft; add note that it is a release-gate ADR, not a v0 design-bundle gate. |
| `0005-operational-runtime.md:3` | Draft | File/CLI surface, run-summary schema, retention tiers, and failure-manifest indexing exist (`src/abc/tools/validate_design_bundle.clj:79`, `:89`, `:108`, `:528`, `schemas/run-summary.schema.json`), but distributed services and workflow engines remain deferred. | Draft | Keep Draft with note that core v0 operational surface is implemented, while distributed/runtime choices remain open. |
| `0006-v0-design-bundle-validation.md:3` | Accepted | `nix run .#validate-design-bundle` is the CI gate (`flake.nix:98:110`) and runs all harness checks. | Accepted | None. |
| `0007-external-parser-validation-boundary.md:3` | Draft | `validate-ab-validator-output!` runs over `examples/ab-validator-output/` (`src/abc/tools/validate_design_bundle.clj:133:147`, `src/abc/tools/validate_design_bundle.clj:549:551`). | Accepted | Accept; add `Implementation Status` noting the fixture gate is live. |
| `0008-abc-tools-runtime.md:3` | Accepted | Clojure tools exposed as Nix apps (`flake.nix:98:184`, `deps.edn:90:96`); Bash wrappers are thin. | Accepted | None. |
| `0009-imported-output-materialization.md:3` | Draft | `materialize-import` command exists (`src/abc/tools/materialize_import.clj:83:125`, `deps.edn:91`, `flake.nix:115:123`) and `validate-design-bundle` materializes the fixture and validates generated manifests (`src/abc/tools/validate_design_bundle.clj:507:548`). | Accepted | Accept; add `Implementation Status`. |
| `0010-manifest-identity-hardening.md:3` | Draft | Generated manifests compute `manifest_schema_hash` via JCS (`src/abc/tools/manifest.clj`, `test/abc/tools/materialize_import_test.clj`); `validate-design-bundle` fails on mismatched producer schema hashes (`src/abc/tools/validate_design_bundle.clj:34:47`). | Accepted | Accept; add `Implementation Status`. |
| `0011-generated-fixture-policy.md:4` | Accepted | Materialized manifests are generated in temp space and compared byte-for-byte (`src/abc/tools/validate_design_bundle.clj:507:548`, `test/abc/tools/materialize_import_test.clj:180:197`). | Accepted | None. |
| `0012-tei-odd-schematron-validation.md:3` | Accepted | `schemas/tei-profile.odd` generates `.rng` and `.sch` reproducibly; `validate-design-bundle` runs project RNG + Schematron fixtures (`src/abc/tools/validate_design_bundle.clj:556:633`); `nix flake check` includes `tei-profile-drift` (`flake.nix:371:385`). | Accepted | None. |
| `0013-cultural-heritage-lod-profile.md:3` | Accepted | `validate-publication-view!` regenerates and byte-compares LOD fixtures (`src/abc/tools/validate_design_bundle.clj:335:373`, `src/abc/tools/validate_design_bundle.clj:666:674`, `src/abc/tools/linked_art.clj:1`). | Accepted | None. |
| `0014-iiif-applicability.md:3` | Accepted | `abc.tools.iiif/validate-applicability!` runs on `examples/v0/example-work/iiif/applicability.json` (`src/abc/tools/validate_design_bundle.clj:675:677`, `src/abc/tools/iiif.clj:1`, `test/abc/tools/iiif_test.clj`). | Accepted | None. |
| `0015-temporal-modeling.md:3` | Draft | `schemas/person-record.schema.json` admits constrained EDTF; `abc.tools.aozora-csv/parse-date` normalizes partial/BCE dates (`src/abc/tools/aozora_csv.clj:111:124`); RDF emits `abc:edtfDateOfBirth/Death` (`src/abc/tools/person_record.clj`); SHACL pattern enforces the grammar (`schemas/manifest.shacl.ttl:287:296`). | Accepted | Accept; add `Implementation Status`. |
| `0016-edtf-level1-decade-century.md:3` | Accepted | Grammar widened to `\d{3}X` / `\d{2}XX`; BCE century prose translated; tests cover decade/century markers (`test/abc/tools/person_record_test.clj:219:246`, `test/abc/tools/aozora_csv_test.clj:173:211`). | Accepted | None. |
| `0017-vocabulary-review.md:3` | Accepted | `contexts/abc-v0.jsonld` declares `abc:` -> `https://w3id.org/abc/`; linked-art harness extracts identity invariant at that IRI (`src/abc/tools/linked_art.clj:28:35`, `ADR 0017` decision). | Accepted | None. |
| `0018-predicate-rename-batch-1.md:3` | Accepted | `metadata-record.ttl` emits `dcndl:titleTranscription` and `dcterms:rights` (`examples/v0/example-work/metadata-record.ttl:22:23`); SHACL enforces rights IRI set (`schemas/manifest.shacl.ttl:212:214`); JSON contracts unchanged. | Accepted | None. |
| `0020-person-identity-drift-data-model.md:3` | Accepted | ABC-local IDs, drift-event vocabulary, and lineage-only Position L are implemented (`schemas/person-record.schema.json:21`, `src/abc/tools/person_record.clj:78:79`, `src/abc/tools/person_drift.clj`). | Accepted | None. |
| `0021-person-identity-drift-harness.md:3` | Accepted | `_events/` + `_indexes/` layout exists (`examples/v0/example-persons/_events/`, `examples/v0/example-persons/_indexes/`); `validate-drift-events!` runs in `validate-design-bundle` (`src/abc/tools/validate_design_bundle.clj:542:548`, `src/abc/tools/person_drift.clj:409`). | Accepted | None. |
| `0022-upstream-ingest-drift-awareness.md:3` | Accepted | `aozora-history-audit` accepts `--drift-persons-dir` and `--fail-on-drift-participant-updates`; `nix run .#aozora-upstream-audit` wraps it (`flake.nix:175:193`, `src/abc/tools/aozora_history_audit.clj:93:94`). | Accepted | None. |

*Note: There is no `docs/adr/0019-*.md`. The sequence gap is intentional and should not be filled by this restructure.*

---

## 4. Contradictions and Stale Content

| Location | Issue | Resolution in the restructure |
|---|---|---|
| `high-level-architecture-note.md:543:557` (`SOTA Alignment Deltas` table, row at `:553`) | Still calls the ODD a stub and treats upstream `tei_all.rng` as the primary validation target. | Update to state `schemas/tei-profile.odd` is the canonical TEI contract per `ADR 0012`; upstream `tei_all.rng` is retained only as a compatibility baseline until the project RNG/Schematron fixture corpus is complete. |
| `high-level-architecture-note.md:543:557` (`SOTA Alignment Deltas`) | Lists Schematron, Linked Art, and IIIF as gaps, but all three are now accepted and gated. | Remove the table after migrating the ShEx/rudof row to `design-survey.md`; fold the implemented rows into their respective `architecture.md` sections. |
| `high-level-architecture-note.md:1013:1092` (`Near-Term Design Questions` + `Suggested Next Step`) | Item statuses are out of date (items 1-6 implemented, 7-13 evaluated by ADRs). | Remove both sections; their useful content is already in ADRs and the new `architecture.md`/`design-survey.md` split. |
| `high-level-architecture-note.md:408` (`Pipeline Layers`) | Describes drift only as "metadata-version changes," with no split/merge event model. | Add a sentence referencing `ADR 0020` lineage-only drift events and the `_events/`/`_indexes/` audit sidecars. |
| `ADR 0006-v0-design-bundle-validation.md:146:148` | Says Linked Art/IIIF decision artifacts are mandatory before generated views become blockers. This is now true; the ADR wording is consistent but should reference `ADR 0013` and `ADR 0014` as accepted. | No text change required unless the executor wants to tighten the cross-reference. |
| `ADR 0006-v0-design-bundle-validation.md` Implementation Status | Still mentions TEI validation against upstream `tei_all.rng` as current, and says `ADR 0012` supersedes the stub status. | Update Implementation Status to reflect that project-derived RNG/Schematron gates are now live (`src/abc/tools/validate_design_bundle.clj:580:633`). |

---

## 5. Implementation Plan

Execute in small, reversible steps. Do not delete `docs/high-level-architecture-note.md` until both new documents are accepted and links updated (the spec author recommends keeping it as `docs/high-level-architecture-note.md` and adding a deprecation header after the new docs land).

### Task 1: Create `docs/architecture.md` from decided core

**Files:**
- Create: `docs/architecture.md`

**Steps:**
- [ ] Copy the title/version block from `high-level-architecture-note.md:1:12`; change title to `ABC Architecture` and status to `Accepted v0 contract`.
- [ ] Insert the Promotion Contract paragraph from Section 2 of this spec after the status block.
- [ ] Copy `Glossary`, `Project Shape`, `Principles and Policies` as-is.
- [ ] Copy `Manifest Specification` core identity material; remove survey paragraphs per Section 1.2.
- [ ] Copy `Pipeline Layers`; add drift-event sidecar sentence.
- [ ] Copy `Parser IR Boundary`, `Aozora-Specific Design Concerns`, `TEI and XML Profile` core material; update stale ODD text.
- [ ] Copy `Corpus Evolution`.
- [ ] Copy core halves of `Concurrency`, `Runtime Orchestration`, `Observability and Retention`, `Nix and Materialization`, `Risks and Non-Goals`.
- [ ] Copy `Experiment and ML Reproducibility`, `Validation and Testing Strategy`, `CI/CD`.
- [ ] Add a short "Legacy Code Mapping" appendix from `Current Repository Reading`.
- [ ] Add core references.
- [ ] Run:
  ```bash
  nix run .#validate-design-bundle
  ```
  Expected: exit 0 (document-only change has no test impact).

### Task 2: Create `docs/design-survey.md` from surveyed alternatives

**Files:**
- Create: `docs/design-survey.md`

**Steps:**
- [ ] Add a title and the Promotion Contract cross-reference.
- [ ] Move each block listed in Section 1.3 into a short section with one paragraph summary + trigger condition.
- [ ] Move survey references from `high-level-architecture-note.md:1093:1141`.
- [ ] Add a final "How to promote an item" paragraph linking back to the Promotion Contract.
- [ ] Run:
  ```bash
  nix run .#validate-design-bundle
  nix flake check
  ```
  Expected: both exit 0.

### Task 3: Reconcile ADR statuses

**Files:**
- Modify: `docs/adr/0007-external-parser-validation-boundary.md`
- Modify: `docs/adr/0009-imported-output-materialization.md`
- Modify: `docs/adr/0010-manifest-identity-hardening.md`
- Modify: `docs/adr/0015-temporal-modeling.md`
- Modify: `docs/adr/0006-v0-design-bundle-validation.md` (Implementation Status update only)

**Steps:**
- [ ] For each ADR in the reconciliation table with action `Accept`, change `Status: Draft` to `Status: Accepted`, set `Accepted:` date to execution date, and add an `Implementation Status` section pointing to the relevant code/test lines from Section 3.
- [ ] For Draft ADRs kept Draft, add a one-sentence note under `Implementation Status` explaining what is missing.
- [ ] Update `ADR 0006` Implementation Status to record that project-derived RNG/Schematron gates are live.
- [ ] Run:
  ```bash
  nix run .#validate-design-bundle
  nix flake check
  ```
  Expected: exit 0.

### Task 4: Update stale cross-references

**Files:**
- Modify: `docs/v0-design-bundle/README.md`
- Modify: `docs/high-level-architecture-note.md`

**Steps:**
- [ ] In `docs/v0-design-bundle/README.md`, replace the `Source RFC` link with a pointer to `docs/architecture.md` and add a note that surveyed options are in `docs/design-survey.md`.
- [ ] In `docs/high-level-architecture-note.md`, add a prominent deprecation header at the top directing readers to `docs/architecture.md` and `docs/design-survey.md`, and stating the note is kept for historical reference only.
- [ ] Run:
  ```bash
  nix run .#validate-design-bundle
  nix flake check
  ```
  Expected: exit 0.

### Task 5: Final verification

**Steps:**
- [ ] Confirm `docs/architecture.md` is under ~450 lines.
- [ ] Confirm `docs/design-survey.md` contains all surveyed blocks from Section 1.3.
- [ ] Confirm no prose was deleted from `docs/high-level-architecture-note.md`.
- [ ] Confirm every ADR with action `Accept` now reads `Status: Accepted`.
- [ ] Run the full verification suite:
  ```bash
  nix run .#validate-design-bundle
  nix flake check
  ```
  Expected: both exit 0.

---

## Verification Summary

- **Primary gate:** `nix run .#validate-design-bundle` (documented at `docs/adr/0006-v0-design-bundle-validation.md`, implemented at `flake.nix:98:110`).
- **Secondary gate:** `nix flake check` (includes `clj-nix-focused-tests`, `contract-surface`, and `tei-profile-drift` at `flake.nix:277:385`).
- **ADR status checks:** Each Accepted ADR must reference a live code, schema, fixture, or test gate.

---

*Spec produced: 2026-07-02. Source revision inspected: docs/high-level-architecture-note.md v0.5.1 and ADRs 0001-0022.*

## Errata applied 2026-07-02

Five citation errors identified by verification (and re-verified against the live source files before applying) were corrected in place. No structural or recommendation changes were made — only the cited line ranges.

1. **Section 3, ADR 0001 evidence** — `src/abc/tools/validate_design_bundle.clj:551:555` → `:148:162`. The old range pointed at the `(validate-canonicalization!)` orchestration call inside `validate-design-bundle!`; the actual canonicalization-fixture validation is the `validate-canonicalization!` function body (loads the expected fixture digest, compares `manifest-identity-object.canonical.json`, and rejects array-ordering collisions).
2. **Section 3, ADR 0005 evidence** — `src/abc/tools/validate_design_bundle.clj:133:147` → `:79`, `:89`, `:108`, `:528`. The old range pointed at `validate-ab-validator-output!`, which is ADR 0007 evidence. The operational-runtime evidence is: run-summary schema loaded (`:79`), self-validated as a schema (`:89`), applied to `run-summary.jsonl` (`:108`), plus the `failure-manifest.example.json` SHACL target in the bundle orchestration (`:528`).
3. **Section 3, ADR 0011 evidence** — `test/abc/tools/materialize_import_test.clj:164:184` → `:180:197`. The old range landed mid-`materialize-falls-back-to-warning-when-run-summary-missing-test`; the actual byte-for-byte deterministic test is `materialized-output-is-deterministic-test` (deftest at `:180`, with the `slurp` equality assertions on `parser-ir.manifest.json` and `warnings.manifest.json` ending at `:197`).
4. **Section 3, ADR 0020 evidence** — `src/abc/tools/person_record.clj:82:83` → `:78:79`. The old range pointed at a blank line / the start of `person-iri`; the ABC-local ID logic is the `abc-local-person-id?` predicate (`^abc-[0-9a-f]{12}$`) at `:78:79` (consumed by `person-iri` at `:89` to mint `https://w3id.org/abc/persons/` IRIs).
5. **Section 4, stale-content claim (a)** — location changed from `high-level-architecture-note.md:487:542` (TEI and XML Profile section) to `:543:557` (SOTA Alignment Deltas table, offending row at `:553`). The "ODD stub, Jing validation against upstream `tei_all.rng`" language lives in the SOTA table row, not in the TEI section; the TEI section prose speaks of generating/selecting an ODD/profile and does not call it a stub.
