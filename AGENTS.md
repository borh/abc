# Agent Notes

This repository is the Soranoha monorepo. It contains two components:

- `soranoha/`: publication build kernel, TEI/plaintext conversion, record schemas,
  TEI profile, assessment evaluation, and the snh protocol.
- `ab-validator/`: the Rust Aozora parser, whose two executables read every
  source Soranoha publishes, and the adapters, parser/IR validation, source-authority
  research, corpus measurement, and report tooling built around it. Retained
  research tools and fixtures live under `ab-validator/research/`.

## Working Rules

- Treat the root flake and root `justfile` as the primary development entry points.
- Do not depend on untracked `references/` paths from active code. Use flake inputs, repo data, or explicit local configuration.
- Do not hardcode machine-local output paths such as `/db/...` in active code. Use the runtime config/environment wrappers.
- Keep generated or corpus-scale artifacts out of source unless an existing report/spec explicitly treats them as checked-in evidence.
- Preserve user changes in the worktree. Do not reset or revert unrelated files.

## Common Checks

Run from the monorepo root:

```sh
just validate
```

Useful focused checks:

```sh
just python-quality
just nix-format-check
just soranoha-tests
nix build ./ab-validator#checks.x86_64-linux.cargo-check
nix build ./ab-validator#checks.x86_64-linux.cargo-clippy
nix build ./ab-validator#checks.x86_64-linux.cargo-fmt
```

## Language Conventions

- Rust: use the `./ab-validator#checks...` Nix checks for cargo check, clippy,
  fmt, and tests.
- Python: all tracked Python should pass ruff format/check and mypy via `just python-quality`.
- Clojure: `just soranoha-tests` runs publication tests, clj-kondo, cljfmt, and rejects project reflection warnings. Kaocha enables `clojure.core/*warn-on-reflection*` through `:bindings`; add precise Java type hints where interop needs them.
- Clojure domain variants: use namespaced keywords internally (for example, `:assessment/available` and `:aozora/available`). Preserve external vocabulary through explicit boundary codecs that reject values from another domain. `just typecheck` checks the publication-whitespace contracts with development-only Typed Clojure dependencies.
- Nix: run `nixfmt` or `just nix-format-check` for Nix changes.
- Comments: follow `docs/comment-standards.md`; verify with
  `scripts/comment-hygiene-check.sh`. Keep repository content self-contained;
  keep plans, handoffs, reviews and temporary decision records in the tracker.
  Retain current contracts and permanent architectural rationale in the repository,
  without issue references.

## Design Boundaries

- `soranoha/` owns publication behavior, record schemas and TEI profile policy,
  including the frozen protocol schemas (`soranoha/resources/snh/schemas/`),
  conformance vectors (`soranoha/resources/snh/vectors/`), boundary decode, wire
  encodings, and admission-evidence formats. Frozen protocol changes require a
  permanent architectural decision record describing the changed contract and rationale.
- ab-validator owns the parser itself, along with parser/adaptor measurement, AAT evidence, parser-IR conversion evidence, and corpus reports.
- Parser outputs are supporting evidence; source-authority measurements are the authority for Aozora markup coverage.
- Plaintext output should remain visible body text only; ruby, source apparatus, provenance, and other metadata belong in TEI/custom sidecars.
