# ab-validator Output Fixture

This directory is a checked-in example of the file bundle ABC expects from the
external `../ab-validator` project.

Files:

- `manifest-inputs.json`: identity-bearing hashes ABC needs when constructing
  artifact manifests, including distinct parser IR and diagnostic schema hashes.
- `parser-ir.json`: parser IR export conforming to `schemas/parser-ir.schema.json`,
  including AAT mapping provenance checked against `data/aat-parser-ir-compatibility.edn`.
- `warnings.jsonl`: warning/error diagnostics, one JSON object per line.
- `run-summary.jsonl`: machine-readable run events.
- `comparison-report.json`: parser comparison summary.

The fixture is intentionally tiny and synthetic. It validates the boundary
between the projects; it is not a parser benchmark.
