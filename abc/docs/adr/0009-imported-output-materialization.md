# ADR 0009: Imported Parser Output Materialization

Status: Accepted
Date: 2026-04-26
Accepted: 2026-07-03
Validation scope: fixture
Release authority: development
Supersedes: none
Amended by: ADR 0010
Source: `docs/adr/0007-external-parser-validation-boundary.md` and `docs/adr/0008-abc-tools-runtime.md`

## Implementation Status

Accepted after `abc.tools.materialize-import/materialize-import!` became the
live materialization path and the design-bundle gate began exercising it. The
command reads `examples/ab-validator-output/`, writes parser-IR and warnings
manifests, attaches an optional mapping-divergence sidecar, computes real
content hashes and byte lengths, and is run inside `validate-design-bundle`
against a temporary output directory before validating the generated manifests,
manifest index, RDF views, and SHACL shapes.

## Context

ABC can validate an imported `ab-validator` output bundle, but it does not yet
turn that bundle into ABC artifact manifests. The next pipeline slice should
exercise ABC's central responsibility: materializing external parser output as
content-addressed artifact records.

## Decision

Add a Clojure command:

```bash
clojure -M:abc/materialize-import examples/ab-validator-output out/imported --generated-at 2026-04-26T00:00:00Z
```

Nix exposes the same command as:

```bash
nix run .#materialize-import -- examples/ab-validator-output out/imported --generated-at 2026-04-26T00:00:00Z
```

The command reads:

- `manifest-inputs.json`,
- `parser-ir.json`,
- `warnings.jsonl`,
- optional `divergence.json` or legacy `divergence.jsonl`.

The canonical mapping-divergence sidecar is `divergence.json`; the
`divergence.jsonl` reader is a legacy fallback for older `ab-validator`
output that recorded divergence records as JSONL. The fallback is only used
when `divergence.json` is absent; the committed fixture ships `divergence.json`
only, and the JSONL path is retained so historical producer bundles still
materialize without conversion. New producer output should emit `divergence.json`.

It writes:

- `parser-ir.manifest.json`,
- `warnings.manifest.json`.

For v0, the command computes real content hashes and byte lengths for the local
files it materializes. It uses manifest input hashes supplied by
`ab-validator` for parser build, parser config, parser IR schema, diagnostic
schema, corpus snapshot, and work content. Parser IR uses `parser_ir_schema_hash`
as its `output_format_spec_hash`; warnings use `diagnostic_schema_hash` so the
two artifacts have distinct identities. Metadata, tokenizer, TEI, and analysis
dimensions are `null`.

## ArtifactID Rule

The materializer computes `artifact_id` as:

```text
sha256(RFC8785-JCS(manifest_identity_object))
```

The same JCS implementation is used for schema hashes and ArtifactID
computation. Deterministic pretty JSON output for generated regression tests is
not the ArtifactID canonicalization algorithm.

## Acceptance Criteria

- **ADR-0009-C1 — fixture-behavior:** Fresh parser-IR and warnings manifests
  both validate against `schemas/manifest.schema.json`, as asserted by
  `test/abc/tools/foundation_evidence_test.clj`.
- **ADR-0009-C2 — fixture-behavior:** Generated content hashes equal the exact
  imported parser-IR and warnings bytes, as asserted by
  `test/abc/tools/materialize_import_test.clj`.
- **ADR-0009-C3 — fixture-behavior:** Materialization selects
  `divergence.json` as the canonical `mapping-divergence` sidecar and falls
  back to legacy `divergence.jsonl` only when the canonical file is absent,
  as asserted by `test/abc/tools/materialize_import_test.clj`.
- **ADR-0009-C4 — fixture-behavior:** Each generated parser-IR and warnings
  `artifact_id` is distinct from its own `content.content_hash`, as asserted
  by `test/abc/tools/materialize_import_test.clj`.
- **ADR-0009-C5 — structural-invariant:** Pinned AAT mapping plus adapter
  registry agreement governs AAT parser-IR conversion compatibility, while
  diagnostic schema identity requires exact-current equality. Evidence:
  `test/abc/tools/validate_design_bundle_test.clj` and
  `test/abc/tools/materialize_import_test.clj`.
- **ADR-0009-C6 — fixture-behavior:** Design-bundle fixture orchestration
  materializes into a temporary directory and validates both generated
  manifests. Evidence: `test/abc/tools/validate_design_bundle_test.clj` and
  `test/abc/tools/foundation_evidence_test.clj`.
- **ADR-0009-C7 — structural-invariant:** The Bash entry point contains
  delegation only; Clojure owns materialization logic, as asserted by
  `test/abc/tools/foundation_evidence_test.clj`.

## Rollback

If the materialized manifest shape changes, keep the command path stable and
version the manifest schema hash. Existing generated manifests remain valid only
under the schema hash used to produce them.
