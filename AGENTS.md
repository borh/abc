# Agent Notes

This repository is the Soranoha monorepo. It contains two main components:

- `abc/`: Clojure/Nix publication, schema, TEI, manifest, and validation tools.
- `ab-validator/`: Rust adapters, parser/IR validation, corpus measurement, and report tooling.

## Working Rules

- Treat the root flake and root `justfile` as the primary development entry points.
- Do not depend on untracked `references/` paths from active code. Use flake inputs, repo data, or explicit local configuration.
- Do not hardcode machine-local output paths such as `/db/...` in active code. Use the runtime config/environment wrappers.
- Keep generated or corpus-scale artifacts out of source unless an existing report/spec explicitly treats them as checked-in evidence.
- Preserve user changes in the worktree. Do not reset or revert unrelated files.

## Common Checks

Run from the monorepo root:

```sh
just validate-migration
```

Useful focused checks:

```sh
just python-quality
just nix-format-check
nix build .#checks.x86_64-linux.abc-clj-kondo
nix build .#checks.x86_64-linux.abc-clj-nix-focused-tests
nix build .#checks.x86_64-linux.ab-validator-cargo-check
nix build .#checks.x86_64-linux.ab-validator-cargo-clippy
nix build .#checks.x86_64-linux.ab-validator-cargo-fmt
```

## Language Conventions

- Rust: use the `ab-validator-*` Nix checks for cargo check, clippy, fmt, and tests.
- Python: all tracked Python should pass ruff format/check and mypy via `just python-quality`.
- Clojure: `abc-clj-kondo` checks all `abc/src` and `abc/test` with clj-kondo plus cljfmt.
- Nix: run `nixfmt` or `just nix-format-check` for Nix changes.

## Design Boundaries

- ABC owns publication schemas, TEI profile policy, manifest identity, and registry admission.
- ab-validator owns parser/adaptor measurement, AAT evidence, parser-IR conversion evidence, and corpus reports.
- Parser outputs are supporting evidence; source-authority measurements are the authority for Aozora markup coverage.
- Plaintext output should remain visible body text only; ruby, source apparatus, provenance, and other metadata belong in TEI/custom sidecars.
