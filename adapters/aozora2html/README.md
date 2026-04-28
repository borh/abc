# aozora2html adapter

Wraps the official Ruby `aozora2html` parser
(`references/parsers/aozora2html`, pinned at SHA `9ca5395`) so its XHTML output
can be compared with the Rust adapters under `ab-check` / `ab-compare`.

## Pipeline

`run.sh` orchestrates a three-stage pipeline:

1. **Encoding shim.** `lib/aozora2html.rb:157` opens its input with
   `'rb:Shift_JIS'` regardless of `--use-unicode`, and `Jstream` requires CRLF
   line endings. `run.sh` therefore detects encoding from raw stdin (UTF-8 BOM
   / UTF-8 / Shift_JIS fallback), transcodes to CP932 with `iconv` if needed,
   and rewrites line endings to CRLF before invoking Ruby.
2. **Ruby parser.** `aozora2html --error-utf8 --use-unicode <crlf_src> <xhtml>`
   under `nix shell nixpkgs#ruby`. Stderr is redirected so progress chatter
   does not pollute the AAT JSON written to stdout.
3. **XHTML → AAT mapping.** `adapter.py` parses the XHTML with `lxml`, walks
   the tree, and emits AAT JSON. It reads `--source` (the original raw stdin
   bytes) for `meta.source_hash` and `meta.source_encoding`, so those fields
   reflect the user's input — not the Shift_JIS intermediate.

## Contract

`run.sh` ↔ `adapter.py` communicate through two temp files passed as
positional arguments:

- `--source <path>` — original stdin bytes, used for hashing/encoding metadata.
- `--xhtml <path>` — Ruby's XHTML output, used for AAT projection.
- `--mode <aat|ir>` — output mode (currently only `aat` is wired up).
- `--version` — prints `aozora2html-adapter <semver> <ruby-parser-sha>`.

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
  | bash adapters/aozora2html/run.sh --mode aat
```

## Tests

```bash
nix-shell -p 'python3.withPackages(ps: [ps.lxml ps.jsonschema ps.pytest])' \
  --run 'python3 -m pytest adapters/aozora2html/tests/ -v'
```
