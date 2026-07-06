# Pre-Monorepo Cutoff Readiness

Status: active handoff
Date: 2026-07-06

## Purpose

This handoff records the small gates that should be green immediately before
moving `abc` and `ab-validator` into one physical repository. It is not a
GitHub CI plan. The expected automation target is a local or Forgejo-hosted
runner with access to the project data volumes and caches.

## Starting Point

The cutoff hardening pass started from:

- `abc` main: `2ead1e4` (`Harden parser-IR schema contract rotation`)
- `ab-validator` main: `935a0b2` (`Merge branch 'feat/glue-followups'`)
- AAT to parser-IR mapping version: `0.2.4`
- Mapping hash: `sha256:61d0d549bb73d45765757a739d738effc6a1979fa24cb8c878e9d2ba9d73f53e`
- Mapping schema hash: `sha256:23a2822cbae88533168121e8a09648441276d8af6484269ae666b90030eb1e06`
- Parser-IR schema hash: `sha256:0ab6f07e681b7adb14b9cacb14e4f406ef122151df4d1554503e77a3f1faf8c2`

## Cutoff Checks

Run these from the checked-out component roots before the physical merge.

ABC:

```bash
clojure -M:test:kaocha -m kaocha.runner --focus abc.tools.validate-design-bundle-test
clojure -M:test:kaocha -m kaocha.runner --focus abc.tools.schema-test
clojure -M:test:kaocha -m kaocha.runner --focus abc.tools.parser-evidence-test
nix flake check --no-build
```

ab-validator:

```bash
cargo fmt --all --check
CARGO_BUILD_JOBS=24 cargo check --workspace -j 24
CARGO_BUILD_JOBS=24 cargo clippy --workspace --all-targets -j 24 -- -D warnings
python3 scripts/schema_contracts.py
python3 scripts/compare_abc_schema_contracts.py --abc ../abc
bash tests/abc-schema-contract-compare-smoke.sh
bash tests/parser-ir-publication-next-work-smoke.sh
nix flake check --no-build
```

Use the heavier full-corpus measurements only when the data volume is mounted
on the runner. GitHub Actions is not expected to carry this workload.

## Identity Rules

- Durable evidence paths use logical component paths such as
  `ab-validator/docs/superpowers/reports/...`.
- Local paths such as `/db/...`, `/home/...`, or sibling checkouts are rerun
  locators only. They are not artifact identity.
- ABC schema contracts are authoritative in `abc/schemas/schema-contracts.json`.
  The ab-validator copy is a vendored snapshot and must match by schema id,
  version, title, and hash.
- The first monorepo move must not rename `abc` vocabulary URIs, Schematron
  rule IDs, Clojure namespaces, or evidence component labels.

## Post-Merge Work

Do after the physical monorepo layout exists:

1. Replace the vendored `ab-validator/data/abc-schemas/schemas/` copy with a
   symlink or generated view over `abc/schemas/`.
2. Add a monorepo-native schema drift gate that no longer needs an explicit
   `--abc` checkout path.
3. Update path-producing reports to emit both local physical locators and
   logical component paths where they do not already.
4. Move local runner recipes to the Forgejo runner surface; do not resurrect
   GitHub Actions as the source of truth.
5. Revisit the Soranoha rename only after the physical merge and parser-IR
   publication mapping work stop moving.
