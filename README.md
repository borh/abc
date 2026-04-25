# ab-validator

`ab-validator` is a set of standalone tools for evaluating Aozora Bunko parsers
against corpus features and a shared JSON Abstract Annotation Tree (AAT) format.

Current tools:

- `ab-index`: scans a corpus and builds a feature index.
- `ab-check`: validates AAT output and runs parser invariant checks.

Reference parser adapters live under `adapters/`. Local research and parser
checkouts may live under `references/`; that directory is intentionally ignored
by Git.

## Development

```bash
nix develop
cargo fmt
cargo test
```
