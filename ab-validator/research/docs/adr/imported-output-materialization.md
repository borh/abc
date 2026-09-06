# Imported Parser Output Materialization

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

## Rollback

If the materialized manifest shape changes, keep the command path stable and
version the manifest schema hash. Existing generated manifests remain valid only
under the schema hash used to produce them.
