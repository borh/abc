# ab-validator Boundary

ABC does not own parser-candidate execution in v0. That work lives in
`../ab-validator`.

ABC consumes a file bundle exported by `ab-validator`:

- `parser-ir.json`: parser IR conforming to `schemas/parser-ir.schema.json`.
- `warnings.jsonl`: one parser diagnostic JSON object per line.
- `run-summary.jsonl`: one `run-start`, one or more `work-result`, and one
  `run-complete` event.
- `comparison-report.json`: parser comparison summary. This is advisory for
  ABC v0 and not identity-bearing.
- `manifest-inputs.json`: hashes and labels ABC needs to construct artifact
  manifests.

The fixture in `examples/ab-validator-output/` is the current contract example.
It is checked by `bin/validate-design-bundle.sh`, but the script does not run
`../ab-validator` or inspect its source tree.

## Boundary Rules

- ABC schemas are authoritative for parser IR and diagnostics accepted into the
  ABC pipeline.
- `ab-validator` may use a different internal representation.
- ABC should reject imported bundles that fail schema validation before building
  manifests from them.
- Parser performance metrics and parser-candidate comparisons are advisory
  inputs until an ADR promotes a parser implementation.
- The imported bundle should contain content hashes, not mutable path-only
  references, for identity-bearing inputs.
