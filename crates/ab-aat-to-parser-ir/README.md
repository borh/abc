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

- Mapping version: `0.2.3`
- Mapping hash:
  `sha256:9812e1cc0d956143e61cdd9d0a5bccaa3ec301c7984279230107c23919936e8b`
- Mapping schema hash:
  `sha256:38ec7f0e5affb10329b550a091cd3a6fb5a25e26fd469dfe9f8249970cf9adb4`
- Parser-IR schema hash:
  `sha256:d98eb9684e7a88f5b62693dd582e28f14834ff85011dc7297e7b41516f7be913`
- Latest full-corpus conversion audit:
  `docs/superpowers/reports/2026-07-04-aat-parser-ir-full-corpus-conversion.md`

## Verification

```sh
cargo test -p ab-aat-to-parser-ir
bash tests/aat-to-parser-ir-cli-smoke.sh
nix build .#checks.$(nix eval --impure --raw --expr builtins.currentSystem).aat-to-parser-ir-smoke --print-build-logs
```
