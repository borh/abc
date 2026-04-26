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
| Manifest schema | `schemas/manifest.schema.json` | Canonical JSON contract for success and failure manifests | Draft 2020-12 schema validates example manifests |
| Parser IR schema | `schemas/parser-ir.schema.json` | Language-neutral parser boundary | Covers spans, ruby, gaiji, notes, warnings, errors |
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
2. Review ADR 0002 before adapting or writing a parser.
3. Review ADR 0003 before adding Nix derivations.
4. Validate the example manifest against `schemas/manifest.schema.json`.
5. Validate the parser IR fixture against `schemas/parser-ir.schema.json`.
6. Generate the RDF view from the JSON manifest using the mapping rules.
7. Validate imported `ab-validator` output against the ABC boundary contract.
8. Select the first real Aozora work and replace the design fixture values.

## Non-Goals

- No full-corpus materialization.
- No final parser choice.
- No public release signing workflow.
- No database commitment beyond example indexes.
- No claim that the TEI ODD is complete.
- No v0 JSON Schema yet for `validation.json` or `query-index-entry.json`;
  those sidecar/index schemas are deferred until the first orchestrator and
  query-index implementation exist.
