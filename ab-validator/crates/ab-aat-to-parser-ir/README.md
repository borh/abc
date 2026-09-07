# ab-aat-to-parser-ir

`ab-aat-to-parser-ir` converts schema-valid AAT v1 or v2 JSON into parser-IR and
a divergence bundle backed by the measured mapping artifact in
`data/aat-to-parser-ir-mapping-v1.json`.

The crate is a measurement consumer, not a generic mapping DSL. Runtime
divergence records are authorized by the checked-in mapping, and parser-IR plus
divergence output are validated against compiled schemas. Research invocations can select other schemas
with `--research-root`.

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
  --divergence-out /tmp/divergence.json
```

### `--ortho-annotations <PATH>`

Optional path to an orthographic annotations JSON file produced by the
`ab-ortho-detect` layer. The converter retains detector provenance and byte ranges
in `orthographic_annotations` without changing source content nodes.

The annotation file must use `parser_text_utf8` coordinates in the converted
visible text and identify the same AAT input: `work_id` must equal `AAT.work_id`,
and `primary_text_hash` must equal
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

Audit one or more directories of AAT files:

```sh
cargo run -p ab-aat-to-parser-ir -- audit-corpus \
  --aat-dir /path/to/aat \
  --mapping data/aat-to-parser-ir-mapping-v2.json \
  --summary-json /tmp/conversion-summary.json \
  --report-md /tmp/conversion-report.md
```

The flake exposes the release binary as `.#ab-aat-to-parser-ir`; the smoke check
is `checks.<system>.aat-to-parser-ir-smoke`.

## Mapping generations

The `source_aat_version` field selects the AAT schema: version 1 uses
`data/aat-schema-v1.json`; version 2 uses `data/aat-schema.json`. Both mappings
use the same target parser-IR schema. Version-suffixed mapping files preserve
published registry coordinates; the unsuffixed files select the current mapping.
Use the artifact itself for its version and canonical hash.

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
