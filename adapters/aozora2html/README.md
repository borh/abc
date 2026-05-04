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
   under `nix develop .#aozora2html`. The wrapper installs the released gem into
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

## Backend selector (migration)

The mapping stage runs under `aozora2html-adapter` and defaults to Rust.
To opt into Python explicitly:

```bash
AOZORA2HTML_BACKEND=rust adapters/aozora2html/aozora2html-adapter --mode aat
```

If Rust is not available, set `AOZORA2HTML_BACKEND=python` for emergency fallback.
Rust parity and parity-test tasks are:

```bash
just aozora2html-rust-build
AOZORA2HTML_PARITY=1 pytest adapters/aozora2html/tests/test_mapper.py -vv
```

## Parser-behavior caveats

Captured during fixture review (`tests/fixtures/*.xhtml`):

- **Resolved gaiji may become plain text or image fallback.** When
  `--use-unicode` succeeds (e.g. `※［＃「口＋世」、U+546D］` → `&#x546D;` →
  `呭`), the parser inlines the resolved character as text and discards the
  marker. When it cannot resolve through XHTML, it emits a gaiji image. For
  single-line source fragments whose rendered projection still aligns, the
  adapter reconstructs a gaiji AAT node from the original source marker and
  marks it with `x-provenance = "source-derived"`. Broader cases remain
  parser-side abstraction differences rather than mapping bugs.
- **Some `［＃...］` markers stay as `<span class="notes">`.** The adapter
  reconstructs the supported subset that the source marker still identifies:
  explicit line breaks (`［＃改行］`), page breaks (`［＃改ページ］`), and
  heading variants such as `［＃「タイトル」は大見出し］` or
  `［＃「タイトル」の大見出し］`. Unsupported notes remain
  `{kind: "style", style_type: "notes", ...}`.
- **Narrow source-derived enrichment is explicit.** Gaiji marker recovery and
  image notes like `［＃挿絵（fig01.png、横４００×縦３００）入る］` are
  reconstructed as AAT nodes with `x-provenance = "source-derived"` when the
  original source marker gives information the rendered XHTML has flattened
  or kept only as note text. These nodes are useful for oracle checks but
  must be read as adapter normalization, not raw upstream-XHTML structure.
- **Reviewed source-note recoveries use source markers as the oracle bridge.**
  The adapter now reconstructs the reviewed subset of ruby placement,
  annotation ruby, kunten, one-line indentation, TCY/yokogumi, caption
  blocks, parenthetical warigaki, front-reference boten, and editor-note
  metadata from the original source text when the XHTML output flattens those
  constructs. These recoveries are intentionally conservative and carry
  `x-provenance = "source-derived"` where the AAT node has no direct XHTML
  counterpart.
- **Plain source image annotations are also recovered narrowly.** Text of the
  form `猫の図（fig00001_01.png、横321×縦123）入る` is mapped to a
  source-derived AAT `figure` node when it survives as plain text.
- **Rendered image/caption pairs are linked conservatively.** Non-gaiji
  `<img>` elements become AAT `figure` nodes. If the immediately following
  paragraph is only a rendered caption span, the adapter attaches that caption
  to the figure with `x-caption-provenance = "source-derived"`.
- **Warichu is split from rendered warichu text.** `［＃割り注］上行／下行［＃割り注終わり］`
  comes through as `<span class="warichu">（上行／下行）</span>`. The adapter
  maps that XHTML class to AAT `warigaki`, splitting on `／` or `/` when
  present and leaving `lower` empty otherwise.
- **Decoration classes are normalized to AAT terms.** Rendered class names
  such as `futoji`, `shatai`, `white_sesame_dot`, `underline_double`, `dai2`,
  and `keigakomi` are parser-specific XHTML vocabulary. The adapter maps the
  reviewed subset to parser-neutral AAT `style`, `font_size`, or `keigakomi`
  nodes with `x-provenance = "source-derived"` and keeps the relevant variant
  metadata (`x-boten-kind`, `x-line-kind`, `x-placement`, `level`).
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
cd /path/to/ab-validator
nix develop .#aozora2html --command python3 -m pytest adapters/aozora2html/tests/ -v
```
