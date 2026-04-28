# ABC v0 Design Bundle

Status: Draft planning bundle
Date: 2026-04-26
Source RFC: `docs/high-level-architecture-note.md` v0.5.1

This bundle turns the architecture note into concrete planning artifacts. It
does not implement the corpus pipeline. Its job is to make the first
implementation small enough to review: one manifest schema, one parser IR
schema, one TEI profile stub, one example artifact bundle, and the ADRs that
bound the choices.

## Deliverables

| Artifact | Path | Purpose | Acceptance Check |
| --- | --- | --- | --- |
| Manifest identity ADR | `docs/adr/0001-manifest-identity.md` | Freeze v0 ArtifactID semantics | Explains schema hash, JCS, null dimensions, failure manifests |
| Parser evaluation ADR | `docs/adr/0002-parser-evaluation.md` | Define parser gates before choosing Rust/Clojure/wrapper path | Must-pass and benchmark criteria are explicit |
| Nix materialization ADR | `docs/adr/0003-nix-materialization.md` | Keep Nix scoped to bounded builds | Uses external incremental index bias |
| Supply-chain security ADR | `docs/adr/0004-supply-chain-release-security.md` | Bound release trust without blocking local v0 | Identifies signature and attestation options |
| Operational runtime ADR | `docs/adr/0005-operational-runtime.md` | Keep v0 file/CLI oriented | Defers distributed services and defines retention |
| Design validation ADR | `docs/adr/0006-v0-design-bundle-validation.md` | Define the local/CI smoke validation command | Keeps validation repository-local and parser-free |
| ab-validator boundary ADR | `docs/adr/0007-external-parser-validation-boundary.md` | Keep parser comparison in `../ab-validator` | Defines imported output bundle contract |
| ABC tools runtime ADR | `docs/adr/0008-abc-tools-runtime.md` | Move pipeline tooling into Clojure commands exposed by Nix | Keeps Bash as compatibility wrappers only |
| Imported materialization ADR | `docs/adr/0009-imported-output-materialization.md` | Turn imported parser output into ABC manifests | Generates parser IR and warning manifests from fixture input |
| Manifest identity hardening ADR | `docs/adr/0010-manifest-identity-hardening.md` | Tie generated identity to bundled schema value | Computes manifest schema hash with the ADR 0001 JCS rule |
| Generated output policy ADR | `docs/adr/0011-generated-fixture-policy.md` | Define how generated materialized-import manifests are handled | Regenerates in temp space and compares two generated runs byte-for-byte |
| TEI ODD + Schematron ADR | `docs/adr/0012-tei-odd-schematron-validation.md` | Make ODD-derived Relax NG and Schematron both first-class TEI gates | Requires valid, invalid, and warning TEI fixtures with expected rule IDs |
| Cultural-heritage LOD ADR | `docs/adr/0013-cultural-heritage-lod-profile.md` | Keep Linked Art as a derived publication view | Prevents Linked Art from becoming a competing identity system |
| IIIF applicability ADR | `docs/adr/0014-iiif-applicability.md` | Define when IIIF is required, optional, or out of scope | Keeps text-only v0 valid without IIIF |
| Manifest schema | `schemas/manifest.schema.json` | Canonical JSON contract for success and failure manifests | Draft 2020-12 schema validates example manifests |
| Parser IR schema | `schemas/parser-ir.schema.json` | Language-neutral parser boundary | Covers spans, ruby, gaiji, notes, warnings, errors |
| Diagnostic schema | `schemas/diagnostic.schema.json` | Standalone warning/error sidecar contract | Validates diagnostic JSON Lines fixtures |
| Run summary schema | `schemas/run-summary.schema.json` | Parser run event contract | Validates run-start, work-result, and run-complete JSON Lines |
| Manifest inputs schema | `schemas/manifest-inputs.schema.json` | Imported bundle identity inputs | Validates hashes used to construct ABC manifests |
| Comparison report schema | `schemas/comparison-report.schema.json` | Advisory parser comparison contract | Validates the optional comparison fixture shape |
| Manifest SHACL | `schemas/manifest.shacl.ttl` | RDF/PROV-O publication view check | Requires artifact, status, activity, and derivation links |
| TEI profile | `schemas/tei-profile.odd` | Canonical ODD profile contract | Produces project Relax NG and Schematron validation artifacts |
| TEI Relax NG artifact | `schemas/tei-profile.rng` | Derived structural schema target | Reproducibly generated from the ODD before upstream baseline is retired |
| TEI Schematron artifact | `schemas/tei-profile.sch` | Derived Schematron rule set | Validates named ABC business rules |
| TEI validation-result schema | `schemas/tei-validation-result.schema.json` | Sidecar contract for TEI validation runs | Records Relax NG and Schematron outcomes by rule ID |
| TEI validation plan | `docs/tei-validation.md` | Human-readable TEI rule table and harness expectations | Mirrors the ODD rule inventory |
| Manifest-to-RDF mapping | `docs/v0-design-bundle/manifest-to-rdf.md` | Deterministic JSON to RDF view plan | Maps core manifest fields to PROV-O terms |
| ab-validator boundary | `docs/v0-design-bundle/ab-validator-boundary.md` | File-level handoff contract for parser evaluation output | Lists accepted imported files and ownership boundaries |
| RO-Crate evaluation | `docs/v0-design-bundle/ro-crate-evaluation.md` | Decide publication packaging scope | Evaluates RO-Crate 1.2 detached/profile crates |
| Linked Art crosswalk | `docs/lod/linked-art-crosswalk.md` | Candidate cultural-heritage JSON-LD mapping | Derived publication view only |
| JSON-LD context policy | `docs/lod/json-ld-context-policy.md` | Version and hash context documents | Context changes never affect ArtifactID |
| ABC JSON-LD context fixture | `contexts/abc-v0.jsonld` | Initial context for ABC JSON-LD publication views | Parses and is hashable as publication-profile metadata |
| Tokenizer determinism | `docs/v0-design-bundle/tokenizer-determinism.md` | Bound exact tokenization claims | Lists required pins and fixture expectations |
| CI smoke corpus | `docs/v0-design-bundle/ci-smoke-corpus.md` | Define representative PR gate corpus | Keeps target runtime under five minutes |
| Canonicalization fixtures | `fixtures/canonicalization/` | Cross-implementation identity checks | Includes canonical identity object and expected hash policy |
| Example bundle fixture | `examples/v0/example-work/` | Target layout for one Aozora work | Includes success and failure design fixtures |
| ab-validator output fixture | `examples/ab-validator-output/` | Imported parser-evaluation output contract | Validates parser IR, diagnostics, run summary, and manifest inputs |

## Sequencing

1. Review and adopt ADR 0001 before treating any hash as stable.
2. Adopt ADR 0006, ADR 0008, and ADR 0012 after the command contract is
   unified around ODD-derived Relax NG plus Schematron.
3. Adopt ADR 0007 after the imported-output schemas are accepted.
4. Adopt ADR 0009 and ADR 0010 after materialized identity behavior is reconciled.
5. Keep ADR 0002, ADR 0003, ADR 0004, ADR 0005, and ADR 0011 Draft until their gates are exercised by implementation.
6. Validate the example manifest against `schemas/manifest.schema.json`.
7. Validate the parser IR fixture against `schemas/parser-ir.schema.json`.
8. Generate the RDF view from the JSON manifest using the mapping rules.
9. Add Linked Art and IIIF decision artifacts before enabling their generated
   fixture gates.
10. Validate imported `ab-validator` output against the ABC boundary contract.
11. Select the first real Aozora work and replace the design fixture values.

## CI Gate Additions

TEI:

- ODD parses.
- ODD-derived RNG exists or is generated reproducibly.
- ODD-derived Schematron exists or is generated reproducibly.
- Valid TEI fixture passes RNG plus Schematron.
- Invalid TEI fixtures fail for expected rule IDs.

LOD:

- ABC JSON-LD context parses.
- Linked Art candidate fixture compacts/expands deterministically if enabled.
- SHACL still validates the PROV-O view.

IIIF:

- IIIF applicability fixture exists.
- IIIF manifest validates if generated.
- Text-only works are not forced to generate IIIF.

Linked Art and IIIF are not mandatory build blockers until their ADRs are
accepted. The decision artifacts are mandatory first.

## Non-Goals

- No full-corpus materialization.
- No final parser choice.
- No public release signing workflow.
- No database commitment beyond example indexes.
- No claim that the TEI ODD is complete until it reproducibly yields both
  project Relax NG and Schematron artifacts.
- No v0 requirement that text-only works generate Linked Art or IIIF
  publication artifacts.
- No v0 JSON Schema yet for `validation.json` or `query-index-entry.json`;
  those sidecar/index schemas are deferred until the first orchestrator and
  query-index implementation exist.
