# aozora2html adapter

Wraps the official Ruby `aozora2html` parser gem (`aozora2html` 3.0.1) so its
XHTML output can be compared with the Rust adapters under `ab-check` /
`ab-compare`.

## Pipeline

`aozora2html-adapter` orchestrates a three-stage pipeline:

1. **Encoding shim.** `lib/aozora2html.rb:157` opens its input with
   `'rb:Shift_JIS'` regardless of `--use-unicode`, and `Jstream` requires CRLF
   line endings. `aozora2html-adapter` therefore detects encoding from raw stdin (UTF-8 BOM
   / UTF-8 / Shift_JIS fallback), transcodes to CP932 with `iconv` if needed,
   and rewrites line endings to CRLF before invoking Ruby.
   If stdin is a source fragment rather than a full Aozora file with separator
   lines, the wrapper feeds Ruby a synthetic Aozora document around that
   fragment. `adapter.py` still hashes and reports the original stdin bytes.
2. **Ruby parser.** `aozora2html --error-utf8 --use-unicode <crlf_src> <xhtml>`
   under `nix shell nixpkgs#ruby`. The wrapper installs the released gem into
   `/db/ab-validator/gems/aozora2html-3.0.1` by default, keeping dependency
   cache data outside the repo. Stderr is redirected so progress chatter does
   not pollute the AAT JSON written to stdout.
3. **XHTML → AAT mapping.** `adapter.py` parses the XHTML with `lxml`, walks
   the tree, and emits AAT JSON. It reads `--source` (the original raw stdin
   bytes) for `meta.source_hash` and `meta.source_encoding`, so those fields
   reflect the user's input — not the Shift_JIS intermediate.

## Contract

`aozora2html-adapter` ↔ `adapter.py` communicate through two temp files passed as
positional arguments:

- `--source <path>` — original stdin bytes, used for hashing/encoding metadata.
- `--xhtml <path>` — Ruby's XHTML output, used for AAT projection.
- `--mode <aat|html>` — output AAT JSON or the intermediate XHTML.
- `--version` — prints `aozora2html-adapter <semver> gem-<aozora2html-version>`.

## Encoding duplication

Encoding detection logic exists in two places:

- Rust (`crates/ab-source-syntax/src/lib.rs::decode_source_bytes`).
- Python (`adapters/aozora2html/adapter.py::detect_encoding`).

The Python copy follows the same UTF-8-BOM → UTF-8 → CP932 priority order
used by the test adapter (`adapters/test-adapter/test-adapter:19-26`). Sharing
a helper across language boundaries is out of scope for this plan.

## Metrics

`meta.metrics` is omitted. The schema's `metrics` object encodes 20+ fields
tied to internal Rust adapter phases (`decode_ms`, `tokenize_ms`, …) that
have no analogue in a Ruby+Python pipeline. `metrics` is optional in
`data/aat-schema.json`, so omission is valid. `ab-compare` should not be
expected to produce metrics deltas for this adapter.

## Running

```bash
echo "テスト\n著者\n\n-------------------------------------------------------\n凡例\n-------------------------------------------------------\n本文\n" \
  | adapters/aozora2html/aozora2html-adapter --mode aat
```

## Parser-behavior caveats

Captured during fixture review (`tests/fixtures/*.xhtml`):

- **Resolved gaiji become plain text.** When `--use-unicode` succeeds (e.g.
  `※［＃「口＋世」、U+546D］` → `&#x546D;` → `吭`), the parser inlines the
  resolved character as text and discards the marker. The adapter therefore
  cannot emit a `gaiji.marker` semantic-summary entry for this case — the
  Rust adapters that retain the marker in their IR will report a
  `gaiji.marker` row that has no counterpart on this side. This is a
  parser-side decision, not a mapping bug. `ab-compare` will see the
  asymmetry as `summary:gaiji.marker` mismatch and that is correct
  signal: it tells the user the two parsers chose different abstractions.
- **Some `［＃...］` markers stay as `<span class="notes">`.** Page breaks
  (`［＃改ページ］`) and the alternative-form heading marker
  (`［＃「タイトル」は大見出し］`) are emitted by `aozora2html` as literal
  notes spans, not as structural blocks. The adapter preserves unsupported
  notes as `{kind: "style", style_type: "notes", ...}`.
- **Narrow source-note enrichment is explicit.** Image notes like
  `［＃挿絵（fig01.png、横４００×縦３００）入る］` are reconstructed as
  AAT `figure` nodes with `x-provenance = "source-derived"` because the
  source marker text is still present in the rendered XHTML. These nodes are
  useful for oracle checks but are not counted as upstream-XHTML
  faithfulness.
- **Rendered image/caption pairs are linked conservatively.** Non-gaiji
  `<img>` elements become AAT `figure` nodes. If the immediately following
  paragraph is only a rendered caption span, the adapter attaches that caption
  to the figure with `x-caption-provenance = "source-derived"`.
- **Warichu is split from rendered warichu text.** `［＃割り注］上行／下行［＃割り注終わり］`
  comes through as `<span class="warichu">（上行／下行）</span>`. The adapter
  maps that XHTML class to AAT `warigaki`, splitting on `／` or `/` when
  present and leaving `lower` empty otherwise.
- **`gaiji.marker.value.kind` is a Python-side simplification.** Rust's
  `ab-ir` stores the full `format!("{:?}", GaijiKind)` debug string
  (e.g. `"UnicodeCodepoint { value: '吭' }"`,
  `"JisLevel { level: 3, row: 15, cell: 23 }"`). The adapter emits stable
  variant names only — `"Image"`, `"UnicodeCodepoint"`, `"Unknown"` — so
  expect `summary:gaiji.marker` value-hash mismatches against the Rust
  adapters even when description and resolved character agree. The other
  fields (`source`, `description`, `resolved`, `ruby_reading`) still align.

## Tests

```bash
nix-shell -p 'python3.withPackages(ps: [ps.lxml ps.jsonschema ps.pytest])' \
  --run 'python3 -m pytest adapters/aozora2html/tests/ -v'
```
