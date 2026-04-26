# ADR 0006: v0 Design Bundle Validation CLI

Status: Draft
Date: 2026-04-26
Supersedes: none
Source: `docs/high-level-architecture-note.md` v0.5 and
`docs/v0-design-bundle/README.md`

## Context

The v0 design bundle now contains JSON Schemas, SHACL shapes, a TEI ODD stub,
canonicalization fixtures, example manifests, example parser IR, an RDF view,
and changelog tooling. These files were validated manually during review, but
manual checks are not enough once implementation starts.

The first implementation step should make the design bundle mechanically
checkable without committing to a parser runtime, database, RDF store, or corpus
materialization strategy.

## Decision

Add `bin/validate-design-bundle.sh` as the v0 validation entry point.

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

The validation CLI is intentionally a smoke gate. It does not prove that the
TEI ODD generates a complete Relax NG schema, that SHACL validates the whole RDF
view, or that any parser candidate satisfies the IR contract.

## Runtime Policy

The script must run inside `nix develop .#validation` using pinned dev-shell
tools where possible. The validation dev shell provides:

- Python with `jsonschema`,
- `xmllint` via `libxml2`,
- `git-cliff`,
- `jq` for future fixture checks.

The script may also run outside Nix when equivalent tools are on `PATH`, but CI
uses `nix develop` as the supported path.

The default development shell still includes local NLP tooling. CI uses the
smaller validation shell so design-bundle checks do not depend on local corpus
or dictionary overlays.

## CI Policy

CI runs the validation CLI on push and pull request. CI should remain a smoke
gate, not a full corpus build:

- no Aozora corpus checkout,
- no parser candidate execution,
- no JVM-per-work validation loop,
- no Nix derivation matrix evaluation.

## Acceptance Criteria

- `bin/validate-design-bundle.sh` exits `0` on the committed v0 bundle.
- A broken example manifest causes the script to exit non-zero.
- A canonicalization fixture hash mismatch causes the script to exit non-zero.
- `nix flake check` succeeds after adding the validation dependencies.
- CI invokes the same script used locally.

## Rollback

If the shell script grows beyond simple orchestration, replace it with a small
Python or Clojure validator in a new ADR. The externally visible contract remains
the same command path: `bin/validate-design-bundle.sh`.
