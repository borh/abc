# ab-aat-to-parser-ir

`ab-aat-to-parser-ir` converts schema-valid AAT v1 JSON into ABC parser-IR and
a divergence bundle backed by the measured mapping artifact in
`data/aat-to-parser-ir-mapping-v1.json`.

The crate is a measurement consumer, not a generic mapping DSL. Runtime
divergence records are authorized by the checked-in mapping, and parser-IR plus
divergence output are validated against the ABC schemas mirrored under
`data/abc-schemas`.

## Commands

Convert one AAT file:

```sh
cargo run -p ab-aat-to-parser-ir -- convert \
  --aat path/to/input.aat.json \
  --mapping data/aat-to-parser-ir-mapping-v1.json \
  --parser-ir-out /tmp/parser-ir.json \
  --divergence-out /tmp/divergence.json \
  --abc-root data/abc-schemas
```

### `--ortho-annotations <PATH>`

Optional path to an orthographic annotations JSON file produced by the
`ab-ortho-detect` layer. When used with parser-IR schema 0.6.0 or newer,
`--ortho-annotations` emits both:

- `orthographic_annotations`: detector provenance and byte ranges
- `sentences[].tags`: renderer-facing sentence tags, including
  `orthographic-katakana`

The annotation file must describe the same source as the AAT input:
`work_id` must equal `AAT.work_id`, and `work_content_hash` must equal
`AAT.meta.source_hash`.

```json
{
  "work_id": "000000",
  "work_content_hash": "sha256:1111111111111111111111111111111111111111111111111111111111111111",
  "coordinate_system": "decoded_utf8",
  "detector_id": "HeuristicV1",
  "annotations": [
    {
      "source_byte_range": { "start": 0, "end": 24 },
      "normalized_text": "吾輩は猫である。",
      "kind": "ScriptKatakanaToHiragana",
      "confidence": null
    }
  ]
}
```

Audit a corpus:

```sh
cargo run -p ab-aat-to-parser-ir -- audit-corpus \
  --aat-dir scratch/morph-full-corpus/aats/aozora-rs-adapter \
  --aat-dir /db/ab-validator/aat-corpus/aozora2html-full-20260703T020301Z/aat/aozora2html-adapter \
  --aat-dir /db/ab-validator/aat-corpus/aozora-epub3-full-20260704T050652Z-300s/aat/aozora-epub3-adapter \
  --mapping data/aat-to-parser-ir-mapping-v1.json \
  --summary-json docs/superpowers/reports/2026-07-04-aat-parser-ir-full-corpus-conversion.summary.json \
  --report-md docs/superpowers/reports/2026-07-04-aat-parser-ir-full-corpus-conversion.md \
  --jobs 24 \
  --abc-root data/abc-schemas
```

The flake exposes the release binary as `.#ab-aat-to-parser-ir`; the smoke check
is `checks.<system>.aat-to-parser-ir-smoke`.

## Current Evidence

- Mapping version: `0.2.5`
- Mapping hash:
  `sha256:20a3b9a7079b727918ccc5ef20924bc0cef5e0359a9a9647535c1a173c8781f4`
- Mapping schema hash:
  `sha256:23a2822cbae88533168121e8a09648441276d8af6484269ae666b90030eb1e06`
- Parser-IR schema hash:
  `sha256:0b495bb5c12c4d76482afefdaedb5464a74672ffbd5282f9c67d5f419d39a340`
- Latest full-corpus conversion audit:
  `docs/superpowers/reports/2026-07-04-aat-parser-ir-full-corpus-conversion.md`

## Verification

```sh
cargo test -p ab-aat-to-parser-ir
bash tests/aat-to-parser-ir-cli-smoke.sh
nix build .#checks.$(nix eval --impure --raw --expr builtins.currentSystem).aat-to-parser-ir-smoke --print-build-logs
```
