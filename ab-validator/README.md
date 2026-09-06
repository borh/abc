# Parser validation and source research

`ab-validator` contains the Rust Aozora parser, AAT-to-parser-IR conversion,
source-authority measurement, and morphology tools. Soranoha consumes the
`ab-aozora` and `ab-aat-to-parser-ir` executables for publication. Research
adapters, schemas, fixtures, and reports live under `research/`.

The [workspace crate guide](crates/README.md) describes the library boundaries.
The [AAT contract](docs/aat-contract.md) defines the adapter interchange format.

Use the monorepo root for development:

```sh
just validate
nix build ./ab-validator#checks.x86_64-linux.cargo-check
nix build ./ab-validator#checks.x86_64-linux.cargo-clippy
nix build ./ab-validator#checks.x86_64-linux.cargo-fmt
```
