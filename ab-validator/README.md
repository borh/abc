# The Aozora parser, and the research that checks it

`ab-validator` builds the Rust Aozora parser. Its primary output is two
executables, `ab-aozora` and `ab-aat-to-parser-ir`, which Soranoha runs on
every source it publishes, so the parser's reading of a source is the edition's
reading of it. The name records the activity that grew up around the
parser rather than the parser itself: validation, source-authority measurement,
and morphology tools all exist to establish that the reading is right. Research
adapters, schemas, fixtures, and reports live under `research/`.

The parser core is not original work. Eight crates are an independent fork of
[P4suta/aozora](https://github.com/P4suta/aozora) at revision
`1a4f864603970983719655aa4af4525958ac2d38`, dual-licensed MIT or Apache-2.0.
Each carries `LICENSE-MIT`, `LICENSE-APACHE` and `NOTICE`, and each crate root
names the upstream crate and that revision in a header. This matters
methodologically as well as legally because the forked code determines the
transcription of every work Soranoha publishes.

Seven of the eight keep the `ab-aozora-` prefix. The exception is
`ab-notation-strategies`, which descends from upstream's `aozora-proptest` and
was renamed when the fork was reduced, so read the crate-root header rather
than the name. `ab-aozora` without a suffix is locally authored.

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
