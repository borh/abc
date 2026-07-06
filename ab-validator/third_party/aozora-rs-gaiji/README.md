# aozora-rs-gaiji build patch

This is the `aozora-rs-gaiji` crate from `kinoko0518/aozora-rs` tag `v0.6.0`
with a narrow build-script patch:

- use HTTPS for the X0213 table;
- allow Nix to provide pinned input files via environment variables;
- allow Nix to provide `libpdfium.so` instead of downloading pdfium at build time.

The runtime gaiji parser/resolver code is otherwise kept from upstream so the
`aozora-rs` adapter can exercise upstream gaiji behavior.
