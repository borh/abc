# ab-validator Boundary

ABC does not own parser-candidate execution in v0. That work lives in
`../ab-validator`.

ABC consumes a file bundle exported by `ab-validator`:

- `parser-ir.json`: parser IR conforming to `schemas/parser-ir.schema.json`.
- `warnings.jsonl`: one parser diagnostic JSON object per line, conforming to
  `schemas/diagnostic.schema.json`.
- `run-summary.jsonl`: one `run-start`, one or more `work-result`, and one
  `run-complete` event, each conforming to `schemas/run-summary.schema.json`.
- `comparison-report.json`: parser comparison summary. This is advisory for
  ABC v0, not identity-bearing, and must conform to
  `schemas/comparison-report.schema.json` when present.
- `divergence.json`: AAT to parser-IR mapping provenance and divergence
  records, conforming to
  `schemas/aat-parser-ir-divergence-bundle.schema.json` when present. Its
  individual records conform to
  `schemas/aat-parser-ir-divergence.schema.json`.
- `manifest-inputs.json`: hashes and labels ABC needs to construct artifact
  manifests, conforming to `schemas/manifest-inputs.schema.json`. It must
  include distinct `parser_ir_schema_hash` and `diagnostic_schema_hash` values
  so parser IR and warning artifacts have distinct output format identities.
  AAT-derived parser IR must also include `mapping_hash`.

The fixture in `examples/ab-validator-output/` is the current contract example.
It is checked by `nix run .#validate-design-bundle`, but validation does not
run `../ab-validator` or inspect its source tree.

## Boundary Rules

- ABC schemas are authoritative for parser IR and diagnostics accepted into the
  ABC pipeline.
- `ab-validator` may use a different internal representation.
- ABC should reject imported bundles that fail schema validation before building
  manifests from them.
- ABC should reject imported bundles whose producer-supplied parser IR or
  diagnostic schema hashes differ from the checked-in ABC schemas used for
  validation, unless a registered compatibility rule exists.
- ABC should reject AAT-derived parser IR unless the mapping provenance in
  either legacy `parser-ir.json` `derived_from` or current `divergence.json`
  matches `manifest-inputs.json` `mapping_hash` and a measured compatibility
  entry in `data/aat-parser-ir-compatibility.edn`.
- Parser performance metrics and parser-candidate comparisons are advisory
  inputs until an ADR promotes a parser implementation.
- The imported bundle should contain content hashes, not mutable path-only
  references, for identity-bearing inputs.
