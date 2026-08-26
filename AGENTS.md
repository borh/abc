# Agent Notes

This repository is the Soranoha monorepo. It contains three main components:

- `abc/`: Clojure/Nix publication, schema, TEI, manifest, and validation tools.
- `ab-validator/`: Rust adapters, parser/IR validation, corpus measurement, and report tooling.
- `soranoha/`: the publication-rearchitecture build kernel and snh protocol
  implementation (Slices 1–2 of
  `docs/design/2026-08-24-publication-rearchitecture.md`): content-addressed
  store + constructive-trace engine + copied per-work pipeline, plus the
  frozen snh-protocol-v1 schemas (D16.1), conformance vectors, boundary
  decode, and wire/signature encodings (`src/soranoha/snh/`,
  `resources/snh/`). Zero
  requires into abc namespaces; abc-owned assets (record schemas, TEI
  profile) are consumed via an explicit assets root.

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
just soranoha-tests
nix build ./abc#checks.x86_64-linux.clj-kondo
nix build ./abc#checks.x86_64-linux.clj-nix-focused-tests
nix build ./ab-validator#checks.x86_64-linux.cargo-check
nix build ./ab-validator#checks.x86_64-linux.cargo-clippy
nix build ./ab-validator#checks.x86_64-linux.cargo-fmt
```

## Language Conventions

- Rust: use the `./ab-validator#checks...` Nix checks for cargo check, clippy,
  fmt, and tests.
- Python: all tracked Python should pass ruff format/check and mypy via `just python-quality`.
- Clojure: `abc-clj-kondo` checks all `abc/src` and `abc/test` with clj-kondo plus cljfmt.
- Nix: run `nixfmt` or `just nix-format-check` for Nix changes.
- Comments: follow `docs/comment-standards.md`; verify with
  `scripts/comment-hygiene-check.sh` (no transient task/plan/spec/issue
  references in `ab-validator/`, `abc/src/`, or `soranoha/src/` comments;
  soranoha additionally bans design-ledger F/D tags, ported tree excluded).

## Design Boundaries

- ABC owns TEI profile policy and its own legacy publication schemas and
  manifest formats.
- `soranoha/` owns the build kernel (CAS, trace store, copied renderers)
  AND — transferred 2026-08-25 with the first Slice-2 manifest work (design
  ledger D20-as-amended/F56) — snh publication-schema and manifest-identity
  ownership: the frozen protocol schemas
  (`soranoha/resources/snh/schemas/`), the conformance vectors
  (`soranoha/resources/snh/vectors/`), boundary decode, wire encodings, and
  admission-evidence formats. After the D16.1 freeze these change only via a
  decision-log entry in the design ledger.
- ab-validator owns parser/adaptor measurement, AAT evidence, parser-IR conversion evidence, and corpus reports.
- Parser outputs are supporting evidence; source-authority measurements are the authority for Aozora markup coverage.
- Plaintext output should remain visible body text only; ruby, source apparatus, provenance, and other metadata belong in TEI/custom sidecars.
