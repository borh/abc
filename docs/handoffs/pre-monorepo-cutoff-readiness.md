# Pre-Monorepo Cutoff Readiness

Status: active handoff
Date: 2026-07-06

## Purpose

This is the ab-validator producer-side companion to ABC's
`docs/handoffs/pre-monorepo-cutoff-readiness.md`. It records the local gates
that should be green before moving `ab-validator` and `abc` into one physical
repository.

This is not a GitHub CI plan. The expected automation target is a local or
Forgejo-hosted runner with access to the project data volumes and caches.

## Starting Point

The cutoff hardening pass started from:

- `ab-validator` main: `935a0b2` (`Merge branch 'feat/glue-followups'`)
- `abc` main: `2ead1e4` (`Harden parser-IR schema contract rotation`)
- AAT to parser-IR mapping version: `0.2.4`
- Mapping hash: `sha256:61d0d549bb73d45765757a739d738effc6a1979fa24cb8c878e9d2ba9d73f53e`
- Mapping schema hash: `sha256:23a2822cbae88533168121e8a09648441276d8af6484269ae666b90030eb1e06`
- Parser-IR schema hash: `sha256:0ab6f07e681b7adb14b9cacb14e4f406ef122151df4d1554503e77a3f1faf8c2`

## Producer-Side Gates

Run from the ab-validator component root:

```bash
cargo fmt --all --check
CARGO_BUILD_JOBS=24 cargo check --workspace -j 24
CARGO_BUILD_JOBS=24 cargo clippy --workspace --all-targets -j 24 -- -D warnings
python3 scripts/schema_contracts.py
python3 scripts/compare_abc_schema_contracts.py --abc /path/to/abc
bash tests/abc-schema-contract-compare-smoke.sh
bash tests/parser-ir-publication-next-work-smoke.sh
nix flake check --no-build
```

Run full-corpus measurements only on a runner with the `/db` corpus and parser
artifacts mounted. Treat `DNF` within a bounded timeout as useful measurement,
not as a reason to wait indefinitely.

## Path Policy

- Durable evidence paths use logical component paths such as
  `ab-validator/docs/superpowers/reports/...`.
- Local paths such as `/db/...`, `/home/...`, and checkout paths are rerun
  locators only.
- `AB_WORKSPACE_ROOT` identifies a local workspace containing logical
  components such as `abc/` and `ab-validator/`.
- `AB_ABC_ROOT` overrides the ABC checkout root for local two-repo operation.
