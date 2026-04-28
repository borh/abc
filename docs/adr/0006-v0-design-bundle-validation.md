# ADR 0006: v0 Design Bundle Validation CLI

Status: Accepted
Date: 2026-04-26
Accepted: 2026-04-28
Supersedes: none
Source: `docs/high-level-architecture-note.md` v0.5 and
`docs/v0-design-bundle/README.md`

## Implementation Status

As of 2026-04-28 the validation CLI also runs:

- TEI P5 4.11.0 RelaxNG validation via Jing in-process (schema pinned through
  `pkgs.fetchurl` and exported as `TEI_SCHEMA_PATH`). See archived plan
  `docs/superpowers/plans/archive/2026-04-27-tei-relaxng-validation.md`.
- SHACL validation of the RDF/PROV-O view against `schemas/manifest.shacl.ttl`
  for every manifest the harness produces or carries as a fixture, including
  `MetadataRecordWorkShape` / `MetadataRecordPersonShape`. See archived plan
  `docs/superpowers/plans/archive/2026-04-27-shacl-enforcement-and-failure-rdf.md`.
- `metadata-record` schema/identity/SHACL gate plus TEI-EAJ-aligned
  `<teiHeader>` regeneration. See archived plan
  `docs/superpowers/plans/archive/2026-04-27-metadata-data-model.md`.

The remaining `release-smoke` items are signature verification, provenance
verification, and archive/mirror hash verification. The TEI step still
validates against the full `tei_all.rng`; promoting `schemas/tei-profile.odd`
from stub to a project-specific ODD-derived schema is the next TEI step.

## Context

The v0 design bundle now contains JSON Schemas, SHACL shapes, a TEI ODD stub,
canonicalization fixtures, example manifests, example parser IR, an RDF view,
and changelog tooling. These files were validated manually during review, but
manual checks are not enough once implementation starts.

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

The script validates only repository-local design artifacts:

- `schemas/manifest.schema.json` is a valid JSON Schema Draft 2020-12 schema.
- `schemas/parser-ir.schema.json` is a valid JSON Schema Draft 2020-12 schema.
- The example success, source, and failure manifests validate against the
  manifest schema.
- The example parser IR validates against the parser IR schema.
- Canonicalization fixture hashes match the expected digest values.
- Array-ordering negative fixtures produce different digests.
- `schemas/tei-profile.odd` and `examples/v0/example-work/tei.xml` are
  well-formed XML.
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
`contract-smoke`, TEI RelaxNG validation against the upstream `tei_all.rng`,
and SHACL validation of every manifest's RDF view. It does not yet prove that
`schemas/tei-profile.odd` generates a project-specific Relax NG schema, nor
that any parser candidate satisfies the IR contract.

## Runtime Policy

The script must run inside `nix develop .#validation` using pinned dev-shell
tools where possible. The validation dev shell provides:

- Clojure with the `m3` JSON Schema validator,
- `xmllint` via `libxml2`,
- `git-cliff`,
- `jq` for future fixture checks.

The script may also run outside Nix when equivalent tools are on `PATH`, but CI
uses `nix develop` as the supported path.

The default development shell still includes local NLP tooling. CI uses the
smaller validation shell so design-bundle checks do not depend on local corpus
or dictionary overlays.

## CI Policy

CI runs `nix run .#validate-design-bundle` on push and pull request. CI should remain a smoke
gate, not a full corpus build:

- no Aozora corpus checkout,
- no parser candidate execution,
- no JVM-per-work validation loop,
- no Nix derivation matrix evaluation.

## Acceptance Criteria

- `nix run .#validate-design-bundle` exits `0` on the committed v0 bundle.
- `bin/validate-design-bundle.sh` delegates to the supported command rather
  than duplicating validation logic.
- A broken example manifest causes the script to exit non-zero.
- A canonicalization fixture hash mismatch causes the script to exit non-zero.
- `nix flake check` succeeds after adding the validation dependencies.
- CI invokes the same script used locally.

## Rollback

If the validation command grows beyond simple orchestration, split it into
smaller Clojure namespaces behind the same Nix app. The externally visible
contract remains the same command path: `nix run .#validate-design-bundle`.
