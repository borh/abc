# Real Rashomon Demo

This directory is the paper-facing real-work parser-IR demo for 羅生門
(`work_id` `000127`, person `000879`).

Generated inputs:

- AAT source:
  `/db/ab-validator/aat-corpus/aozora2html-aat/aozora2html-adapter/000879_127-13290a9f54a1.json`
- Converter:
  `../ab-validator/crates/ab-aat-to-parser-ir`
- Mapping:
  `../ab-validator/data/aat-to-parser-ir-mapping-v1.json`
- Mapping version:
  `0.2.1`
- Mapping hash:
  `sha256:b508665af72c237fc60f00b720f80db2b16148aa64b5d1cc723a2948ee576390`

Files:

- `parser-ir.aozora2html.json`: real parser-IR converted from the AAT source.
- `divergence.aozora2html.json`: divergence bundle emitted by the converter.
- `metadata-record.json`: ABC metadata record for 羅生門.
- `persons/000879.json`: ABC person record for 芥川龍之介.
- `plain.txt`: ABC plaintext rendering from the real parser-IR.
- `tei.xml`: ABC TEI rendering from the real parser-IR and Rashomon metadata.
- `plaintext.manifest.json`, `tei.manifest.json`, `tei-validation-result.json`:
  ABC publication materializer sidecars.
- `source.manifest.json`: source artifact manifest pointing at the shared
  paper-demo source corpus snapshot.
- `../demo-source-corpus-snapshot.json`: shared source corpus snapshot descriptor.

Source snapshot:

`source.manifest.json` carries real work content and metadata hashes. Its
`corpus_snapshot_hash` is:

`sha256:537736a73abf100317a371791533cdb2dc0ed5c8fc4d4f23a8a822174a929586`

That hash is defined by `../demo-source-corpus-snapshot.json`, which covers the
selected Rashomon and Melos AAT JSON files used for the paper demo.

Validation:

`tei-validation-result.json` records passed well-formedness, project Relax NG,
and project Schematron layers with zero findings.

Regeneration:

```bash
cd ../ab-validator
cargo run -p ab-aat-to-parser-ir -- convert \
  --aat /db/ab-validator/aat-corpus/aozora2html-aat/aozora2html-adapter/000879_127-13290a9f54a1.json \
  --mapping data/aat-to-parser-ir-mapping-v1.json \
  --parser-ir-out /tmp/abc-rashomon-parser-ir/aozora2html-parser-ir.json \
  --divergence-out /tmp/abc-rashomon-parser-ir/aozora2html-divergence.json \
  --abc-root data/abc-schemas

cd ../abc
nix run .#materialize-publication -- \
  paper/demo-rashomon-real/parser-ir.aozora2html.json \
  paper/demo-rashomon-real/metadata-record.json \
  paper/demo-rashomon-real/persons \
  paper/demo-rashomon-real \
  --source-manifest paper/demo-rashomon-real/source.manifest.json \
  --generated-at 2026-07-04T00:00:00Z
```
