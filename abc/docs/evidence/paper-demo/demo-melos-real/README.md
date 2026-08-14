# Real Parser-IR Demo: 走れメロス

This directory is the paper-facing real-work parser-IR demo for 走れメロス
by 太宰治.

## Source Identity

- Aozora person ID: `000035`
- Aozora work ID: `001567`
- Aozora card URL: `https://www.aozora.gr.jp/cards/000035/card1567.html`
- AAT input:
  `/db/ab-validator/aat-corpus/aozora2html-aat/aozora2html-adapter/000035_1567-32ff5a089d67.json`
- Adapter: `aozora2html`
- Adapter version: `aozora2html-adapter 0.1.0 gem-3.0.1`
- Mapping version: `0.2.1`
- Parser-IR schema hash:
  `sha256:41c43f0c88a66c31ae4fbf9b9eeb04de92756082acaaaa1c2e21f1a5bf74a396`
- Work content hash:
  `sha256:03def0d5f4322d1cf1bb812a7cca4e490464cb95090182a34a6e2665dde22ba7`

## Generated Artifacts

- `parser-ir.aozora2html.json`: parser-IR converted from the real AAT source.
- `divergence.aozora2html.json`: conversion audit bundle.
- `metadata-record.json`: fresh ABC metadata record generated from the local
  Aozora extended person list.
- `persons/000035.json`: fresh ABC person record for 太宰治.
- `plain.txt`: plaintext rendered from parser-IR.
- `tei.xml`: TEI rendered from parser-IR plus ABC metadata/person header input.
- `plaintext.manifest.json`: ABC plaintext artifact manifest.
- `tei.manifest.json`: ABC TEI artifact manifest.
- `tei-validation-result.json`: TEI validation sidecar.
- `source.manifest.json`: source artifact manifest pointing at the shared
  paper-demo source corpus snapshot.
- `../demo-source-corpus-snapshot.json`: shared source corpus snapshot descriptor.

## Counts

- Parser-IR nodes: `253`
- Parser-IR warnings: `1`
- Parser-IR errors: `0`
- Divergence records: `14`
- Divergence summary:
  - `AMBIGUITY`: `330`
  - `INVENTION`: `94`
  - `LOSS`: `2`
  - `STRUCTURAL`: `75`
  - `UNSUPPORTED`: `0`
- TEI validation: `passed`
- TEI validation findings: `0`

## Source Snapshot

`source.manifest.json` carries real work content and metadata hashes. Its
`corpus_snapshot_hash` is:

`sha256:537736a73abf100317a371791533cdb2dc0ed5c8fc4d4f23a8a822174a929586`

That hash is defined by `../demo-source-corpus-snapshot.json`, which covers the
selected Rashomon and Melos AAT JSON files used for the paper demo.
