# Parser validation and source research

`ab-validator` contains the Rust Aozora parser, AAT-to-parser-IR conversion,
source-authority measurement, and morphology tools. Soranoha consumes the
`ab-aozora` and `ab-aat-to-parser-ir` executables for publication. Research
adapters, schemas, fixtures, and reports live under `research/`.

The parser core is not original work. The ten `ab-aozora-*` crates are an
independent fork of [P4suta/aozora](https://github.com/P4suta/aozora) at
revision `1a4f864603970983719655aa4af4525958ac2d38`, dual-licensed MIT or
Apache-2.0. Each carries `LICENSE-MIT`, `LICENSE-APACHE` and `NOTICE`, and
each crate root names the upstream crate and that revision in a header. This
is a methodological fact as well as a licence one: the forked code determines
the transcription of every work Soranoha publishes.

The `ab-aozora-` prefix marks that lineage. `ab-aozora` without a suffix is
locally authored despite the name.

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
