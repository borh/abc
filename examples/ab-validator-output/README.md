# ab-validator Output Fixture

This directory is a checked-in example of the file bundle ABC expects from the
external `../ab-validator` project.

Files:

- `manifest-inputs.json`: identity-bearing hashes ABC needs when constructing
  artifact manifests, including distinct parser IR and diagnostic schema hashes.
- `parser-ir.json`: parser IR export conforming to `schemas/parser-ir.schema.json`.
- `warnings.jsonl`: warning/error diagnostics, one JSON object per line.
- `run-summary.jsonl`: machine-readable run events.
- `comparison-report.json`: parser comparison summary.
- `divergence.json`: AAT to parser-IR mapping provenance and divergence
  records, conforming to `schemas/aat-parser-ir-divergence-bundle.schema.json`.

`divergence.json` carries the AAT adapter, mapping version, mapping schema hash,
and parser-IR target schema identity checked against
`data/aat-parser-ir-compatibility.edn`. Legacy parser-IR exports may carry the
same provenance in `parser-ir.json` `derived_from`; the current converter keeps
it in the sidecar bundle.

The fixture is intentionally tiny and synthetic. It validates the boundary
between the projects and the completed `ab-aat-to-parser-ir` output shape; it
is not a parser benchmark.
