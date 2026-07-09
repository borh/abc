# ADR 0006: v0 Design Bundle Validation CLI

Status: Accepted
Date: 2026-04-26
Accepted: 2026-04-28
Supersedes: none
Source: `docs/high-level-architecture-note.md` v0.5 and
`docs/v0-design-bundle/README.md`

## Implementation Status

As of 2026-07-03 the validation CLI runs:

- TEI P5 4.11.0 Relax NG compatibility validation via Jing in-process
  (schema pinned through `pkgs.fetchurl` and exported as `TEI_SCHEMA_PATH`).
  See archived plan
  `docs/superpowers/plans/archive/2026-04-27-tei-relaxng-validation.md`.
- Project TEI validation against `schemas/tei-profile.rng`, derived from the
  canonical `schemas/tei-profile.odd` per ADR 0012.
- Project Schematron validation against `schemas/tei-profile.sch`, also
  derived from the canonical ODD. The harness checks the declared rule universe
  and the negative/warning fixture partition rather than treating the
  Schematron artifact as a stub.
- SHACL validation of the RDF/PROV-O view against `schemas/manifest.shacl.ttl`
  for every manifest the harness produces or carries as a fixture, including
  `MetadataRecordWorkShape` / `MetadataRecordPersonShape`. See archived plan
  `docs/superpowers/plans/archive/2026-04-27-shacl-enforcement-and-failure-rdf.md`.
- `metadata-record` schema/identity/SHACL gate plus TEI-EAJ-aligned
  `<teiHeader>` regeneration. See archived plan
  `docs/superpowers/plans/archive/2026-04-27-metadata-data-model.md`.

The remaining `release-smoke` items are signature verification, provenance
verification, and archive/mirror hash verification. The full `tei_all.rng`
step is retained as a compatibility baseline; it is no longer the only TEI
validation target.

## Context

The v0 design bundle now contains JSON Schemas, SHACL shapes, a canonical TEI
ODD with derived Relax NG and Schematron artifacts, canonicalization fixtures,
example manifests, example parser IR, an RDF view, and changelog tooling. These
files were validated manually during review, but manual checks are not enough
once implementation starts.

The first implementation step should make the design bundle mechanically
checkable without committing to a parser runtime, database, RDF store, or corpus
materialization strategy.

## Decision

The canonical local and CI validation command is:

```bash
nix run .#validate-design-bundle
```

Developer direct execution is:

```bash
clojure -M:abc/validate-design-bundle
```

`bin/validate-design-bundle.sh` remains as a compatibility wrapper and
delegates to the Clojure/Nix entry point where available.

The script validates only repository-local design artifacts. The v0 target
sequence is:

1. Validate canonical JSON manifests with JSON Schema.
2. Generate or verify `schemas/tei-profile.rng` from `schemas/tei-profile.odd`.
3. Generate or verify `schemas/tei-profile.sch` from `schemas/tei-profile.odd`.
4. Validate sample TEI with Jing against the project Relax NG schema.
5. Validate sample TEI with Schematron.
6. Validate derived RDF/PROV-O with SHACL.
7. Validate example Linked Art / IIIF fixtures if their ADRs enable those
   derived publication views.

The current implementation validates:

- `schemas/manifest.schema.json` is a valid JSON Schema Draft 2020-12 schema.
- `schemas/parser-ir.schema.json` is a valid JSON Schema Draft 2020-12 schema.
- The example success, source, and failure manifests validate against the
  manifest schema.
- The example parser IR validates against the parser IR schema.
- Canonicalization fixture hashes match the expected digest values.
- Array-ordering negative fixtures produce different digests.
- `schemas/tei-profile.odd` is the canonical project TEI contract, with
  generated `schemas/tei-profile.rng` and `schemas/tei-profile.sch` artifacts
  checked by the harness.
- The example TEI and TEI fixture corpus validate against the project Relax NG
  target where structurally valid.
- The project Schematron fixture partition validates negative and warning
  fixtures against the ODD-declared rule universe.
- `cliff.toml` is accepted by `git-cliff`.

The validation CLI is intentionally a smoke gate. It has three named levels:

- `design-smoke`: repository-local schema sanity, XML well-formedness, fixture
  hash checks, and generated fixture comparison.
- `contract-smoke`: parser IR schema validation, diagnostic JSONL schema
  validation, run-summary JSONL schema validation, manifest-to-RDF deterministic
  fixture comparison, and materialized import fixture comparison.
- `release-smoke`: signature verification, provenance verification, and
  archive/mirror hash verification. (TEI Relax NG validation and SHACL
  validation moved into the regular run on 2026-04-28; see Implementation
  Status above.)

The current command implements `design-smoke`, the imported-output parts of
`contract-smoke`, TEI Relax NG compatibility validation against upstream
`tei_all.rng`, ODD-derived project Relax NG validation, ODD-derived project
Schematron validation, and SHACL validation of every manifest's RDF view. It
does not yet prove that any parser candidate satisfies the IR contract.

## Runtime Policy

The script must run inside `nix develop .#validation` using pinned dev-shell
tools where possible. The validation dev shell provides:

- Clojure with the `com.networknt/json-schema-validator` JSON Schema validator
  (pinned at 3.0.6),
- `xmllint` via `libxml2`,
- Jing for Relax NG compatibility validation,
- an ISO Schematron-capable processor,
- `git-cliff`,
- `jq` for future fixture checks.

The script may also run outside Nix when equivalent tools are on `PATH`, but CI
uses `nix develop` as the supported path.

The default development shell still includes local NLP tooling. CI uses the
smaller validation shell so design-bundle checks do not depend on local corpus
or dictionary overlays.

## CI Policy

CI runs `nix run .#validate-design-bundle` on push and pull request. CI should
remain a smoke gate, not a full corpus build:

- no Aozora corpus checkout,
- no parser candidate execution,
- no JVM-per-work validation loop,
- no Nix derivation matrix evaluation.

## Acceptance Criteria

- `nix run .#validate-design-bundle` exits `0` on the committed v0 bundle.
- `bin/validate-design-bundle.sh` delegates to the supported command rather
  than duplicating validation logic.
- A broken example manifest causes the script to exit non-zero.
- A TEI fixture that passes Relax NG but violates a project Schematron rule
  causes the script to exit non-zero and materializes a failure manifest.
- A figure accessibility warning fixture reports the expected rule ID without
  failing the run unless policy promotes that warning to an error.
- A canonicalization fixture hash mismatch causes the script to exit non-zero.
- Linked Art and IIIF decision artifacts are mandatory; generated Linked Art
  JSON-LD and IIIF manifests are build blockers only after ADR 0013/0014 enable
  them for the relevant fixture class.
- `nix flake check` succeeds after adding the validation dependencies.
- CI invokes the same script used locally.

## Rollback

If the validation command grows beyond simple orchestration, split it into
smaller Clojure namespaces behind the same Nix app. The externally visible
contract remains the same command path: `nix run .#validate-design-bundle`.
