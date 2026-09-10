# The Aozora parser, and the research that checks it

`ab-validator` contains the Rust Aozora parser, validation harnesses,
source-authority measurement tools, and morphology utilities. Its primary
executables are `ab-aozora` and `ab-aat-to-parser-ir`, which process every source
published by Soranoha. Research adapters, schemas, fixtures, and reports live
under `research/`.

The parser core under `crates/` is forked from
[P4suta/aozora](https://github.com/P4suta/aozora) (revision
`1a4f864603970983719655aa4af4525958ac2d38`), dual-licensed MIT OR Apache-2.0.
Upstream licence files, notices, and source headers are preserved in each crate root.

Eight crates are forked: seven prefixed with `ab-aozora-`, plus
`ab-notation-strategies` (forked from `aozora-proptest`). The CLI binary
`ab-aozora` is locally authored.

The [workspace crate guide](crates/README.md) describes the library boundaries
and marks each crate as forked or local.
The [AAT contract](docs/aat-contract.md) defines the adapter interchange format.

Use the monorepo root for development:

```sh
just validate
nix build ./ab-validator#checks.x86_64-linux.cargo-check
nix build ./ab-validator#checks.x86_64-linux.cargo-clippy
nix build ./ab-validator#checks.x86_64-linux.cargo-fmt
```
