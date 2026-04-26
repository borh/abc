# ABC v0 Design Bundle

Status: Draft planning bundle
Date: 2026-04-26
Source RFC: `docs/high-level-architecture-note.md` v0.5

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
| Manifest schema | `schemas/manifest.schema.json` | Canonical JSON contract for success and failure manifests | Draft 2020-12 schema validates example manifests |
| Parser IR schema | `schemas/parser-ir.schema.json` | Language-neutral parser boundary | Covers spans, ruby, gaiji, notes, warnings, errors |
| Diagnostic schema | `schemas/diagnostic.schema.json` | Standalone warning/error sidecar contract | Validates diagnostic JSON Lines fixtures |
| Run summary schema | `schemas/run-summary.schema.json` | Parser run event contract | Validates run-start, work-result, and run-complete JSON Lines |
| Manifest inputs schema | `schemas/manifest-inputs.schema.json` | Imported bundle identity inputs | Validates hashes used to construct ABC manifests |
| Comparison report schema | `schemas/comparison-report.schema.json` | Advisory parser comparison contract | Validates the optional comparison fixture shape |
| Manifest SHACL | `schemas/manifest.shacl.ttl` | RDF/PROV-O publication view check | Requires artifact, status, activity, and derivation links |
| TEI profile stub | `schemas/tei-profile.odd` | Minimal ODD starting point | Documents TEI P5 ruby and gaiji policy anchors |
| Manifest-to-RDF mapping | `docs/v0-design-bundle/manifest-to-rdf.md` | Deterministic JSON to RDF view plan | Maps core manifest fields to PROV-O terms |
| ab-validator boundary | `docs/v0-design-bundle/ab-validator-boundary.md` | File-level handoff contract for parser evaluation output | Lists accepted imported files and ownership boundaries |
| RO-Crate evaluation | `docs/v0-design-bundle/ro-crate-evaluation.md` | Decide publication packaging scope | Evaluates RO-Crate 1.2 detached/profile crates |
| Tokenizer determinism | `docs/v0-design-bundle/tokenizer-determinism.md` | Bound exact tokenization claims | Lists required pins and fixture expectations |
| CI smoke corpus | `docs/v0-design-bundle/ci-smoke-corpus.md` | Define representative PR gate corpus | Keeps target runtime under five minutes |
| Canonicalization fixtures | `fixtures/canonicalization/` | Cross-implementation identity checks | Includes canonical identity object and expected hash policy |
| Example bundle fixture | `examples/v0/example-work/` | Target layout for one Aozora work | Includes success and failure design fixtures |
| ab-validator output fixture | `examples/ab-validator-output/` | Imported parser-evaluation output contract | Validates parser IR, diagnostics, run summary, and manifest inputs |

## Sequencing

1. Review and adopt ADR 0001 before treating any hash as stable.
2. Adopt ADR 0006 and ADR 0008 after the command contract is unified.
3. Adopt ADR 0007 after the imported-output schemas are accepted.
4. Adopt ADR 0009 and ADR 0010 after materialized identity behavior is reconciled.
5. Keep ADR 0002, ADR 0003, ADR 0004, ADR 0005, and ADR 0011 Draft until their gates are exercised by implementation.
6. Validate the example manifest against `schemas/manifest.schema.json`.
7. Validate the parser IR fixture against `schemas/parser-ir.schema.json`.
8. Generate the RDF view from the JSON manifest using the mapping rules.
9. Validate imported `ab-validator` output against the ABC boundary contract.
10. Select the first real Aozora work and replace the design fixture values.

## Non-Goals

- No full-corpus materialization.
- No final parser choice.
- No public release signing workflow.
- No database commitment beyond example indexes.
- No claim that the TEI ODD is complete.
- No v0 JSON Schema yet for `validation.json` or `query-index-entry.json`;
  those sidecar/index schemas are deferred until the first orchestrator and
  query-index implementation exist.
