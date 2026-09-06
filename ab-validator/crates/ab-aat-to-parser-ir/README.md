# ab-aat-to-parser-ir

`ab-aat-to-parser-ir` converts schema-valid AAT v1 JSON into ABC parser-IR and
a divergence bundle backed by the measured mapping artifact in
`data/aat-to-parser-ir-mapping-v1.json`.

The crate is a measurement consumer, not a generic mapping DSL. Runtime
divergence records are authorized by the checked-in mapping, and parser-IR plus
divergence output are validated against the ABC schemas mirrored under
`data/abc-schemas`.

## Byte coordinates

Parser-IR `span` uses `parser_text_utf8`: UTF-8 bytes in the converter's text
projection, before publication whitespace filtering. Ruby occupies its base text
width and resolved gaiji occupy their Unicode width. Headings and source notes
also advance this projection; it is not the final plaintext export.

Optional `source_span` uses `decoded_utf8`: the exact AAT byte extent in the full
decoded source, including markup and source line metadata. Unknown extents remain
absent. Sentence splitting does not copy a parent's source extent onto newly
created fragments when their exact source mapping is unknown.

## Commands

Convert one AAT file:

```sh
cargo run -p ab-aat-to-parser-ir -- convert \
  --aat path/to/input.aat.json \
  --mapping data/aat-to-parser-ir-mapping-v1.json \
  --work-content-hash sha256:bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb \
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

The annotation file must use `parser_text_utf8` coordinates in the converted
sentence text, and identify the same AAT input: `work_id` must equal `AAT.work_id`, and `primary_text_hash` must equal
`AAT.meta.primary_text_hash` (or historical `AAT.meta.source_hash`). Historical
annotation files using `work_content_hash` are accepted and migrated on read;
new output emits only `primary_text_hash`. Bundles tagged `decoded_utf8` are
rejected; actual source offsets cannot be reinterpreted as parser-text offsets.

```json
{
  "work_id": "000000",
  "primary_text_hash": "sha256:1111111111111111111111111111111111111111111111111111111111111111",
  "coordinate_system": "parser_text_utf8",
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

### Mapping v1 (historical AAT compatibility)

- File: `data/aat-to-parser-ir-mapping-v1.json` (identity-rotated compatibility
  artifact; further edits require an explicit protocol/version rotation)
- Mapping version: `0.4.0`
- `source_aat_version`: `1` (selects `data/aat-schema-v1.json`)
- Mapping hash:
  `sha256:ffdfde35482b8e92fb929c40119cdfa8fa9019df53aa08f19359ac71584275c2`
- Frozen pre-rotation v1 `0.2.8` artifact:
  `data/aat-to-parser-ir-mapping-v1-0.2.8.json` (canonical hash
  `sha256:952620ced4eb22f9771e6a10c3a1d4d93de604a8c33e360311f82b6e1eafc5b7`),
  retained so historical compatibility-registry evidence remains reproducible
- Mapping schema hash:
  `sha256:e6af01115ccdb7c5cad086eee4c458230f6b6f55e0dfee7791730b48994283e2`
- Parser-IR schema hash:
  `sha256:43a6a6d86ca5eca062508e6cae633d19bf5248f15c5bb46153a6d8580ea916ec`
- Historical full-corpus conversion audit (predates and does not admit mapping
  0.4.0):
  `docs/superpowers/reports/2026-07-04-aat-parser-ir-full-corpus-conversion.md`

### Mapping v2 (AAT schema 2)

- File: `data/aat-to-parser-ir-mapping-v2.json` (identity-rotated compatibility
  artifact; further edits require an explicit protocol/version rotation)
- Mapping version: `0.5.0`
- `source_aat_version`: `2` (selects `data/aat-schema.json`)
- Mapping hash:
  `sha256:9be58ff3fea272c2a94ae16f05e3e362425e8bcdd20c482a4a842c13fe067142`
- Frozen v2 `0.3.0` artifact:
  `data/aat-to-parser-ir-mapping-v2-0.3.0.json` (canonical hash
  `sha256:7249cd727ef2da90dcd591e6009bead9235fe1c140697ee6bd70aacd9e85ee40`),
  retained so historical compatibility-registry evidence remains reproducible
- Frozen Phase 5 v2 `0.4.0` artifact:
  `data/aat-to-parser-ir-mapping-v2-0.4.0.json` (canonical hash
  `sha256:cf177bee98af086fe728cbc1942e4f631b26ed5bb55aedc7d91b21f467c41f30`),
  retained byte-for-byte for the admitted C5 registry row and frozen reports
- Adds explicit source-note authority (`source_note` blocks), `jizume_block`
  layout projection, and typed (non-`x-`) layout fields with `x-` fallback,
  on top of the v1 rule set
- `SchemaSet::load_for_aat_version` selects the (AAT schema, mapping)
  tuple by the mapping's `source_aat_version`; both mappings share the same
  mapping schema and target parser-IR schema

#### Generations

| Mapping version | File | Hash | Notes |
| --- | --- | --- | --- |
| `0.3.0` | `data/aat-to-parser-ir-mapping-v2-0.3.0.json` (frozen, byte-identical to the live file as it stood at the freeze commit; never edit) | `sha256:7249cd727ef2da90dcd591e6009bead9235fe1c140697ee6bd70aacd9e85ee40` | Registry coordinate of the 0.2.0-era…C4 rows. |
| `0.4.0` | `data/aat-to-parser-ir-mapping-v2-0.4.0.json` (frozen; never edit) | `sha256:cf177bee98af086fe728cbc1942e4f631b26ed5bb55aedc7d91b21f467c41f30` | Adds inline `yokogumi`/`keigakomi` container rules (`S-13`, `S-14`); admitted C5 coordinate. |
| `0.5.0` | `data/aat-to-parser-ir-mapping-v2.json` (live) | `sha256:9be58ff3fea272c2a94ae16f05e3e362425e8bcdd20c482a4a842c13fe067142` | Preserves C5 rules and separates authoritative bundle identity from primary-text provenance. |

A generation is frozen the moment its hash is cited by a registry row —
copy it to a version-suffixed file (as `0.3.0` and `0.4.0` were) before
continuing to edit the live file for the next generation.

Converter subcommands that load a mapping accept
`--expect-mapping-version <v>` and `--expect-mapping-hash <sha256:…>` to bind
an invocation to a specific mapping generation. When either is given and the
loaded mapping does not match, the command fails closed before doing any
conversion work, instead of silently running against whatever the live file
currently contains.

## Verification

```sh
cargo test -p ab-aat-to-parser-ir
bash tests/aat-to-parser-ir-cli-smoke.sh
nix build .#checks.$(nix eval --impure --raw --expr builtins.currentSystem).aat-to-parser-ir-smoke --print-build-logs
```
