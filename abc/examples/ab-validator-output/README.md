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

`parser-ir.json` and `divergence.json` are current converter outputs from this
synthetic UTF-8 source (LF line endings, including a final newline):

```text
作品名
著者名

先生は｜下《した》に。

底本：テスト本
```

The four-node example distinguishes parser-text byte offsets from exact decoded
source extents, including ruby markup and source attribution. Convert the source
with `ab-aozora --mode aat`, then `ab-aat-to-parser-ir convert` with the repository's
v2 mapping and ABC assets root. The corresponding one-file conversion audit is
recorded as `synthetic ABC import boundary fixture` in
`data/aat-parser-ir-compatibility.edn`; it does not establish corpus coverage.
The other files provide synthetic envelope examples for their independent boundary
schemas.
