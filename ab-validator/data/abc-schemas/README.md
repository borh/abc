# Vendored ABC Schema Contracts

This directory is ab-validator's checked-in contract view of the ABC schemas it
consumes at the adapter and publication boundary.

In the monorepo, `schemas/` is a symlink to the monorepo `abc/schemas` directory
so schema bytes have one source of truth. The source-region publication policy
under `data/source-region-publication-policy-v0.json` is likewise a symlink to
ABC's policy file, because ABC owns publication disposition policy.

The `nix-schemas/` directory is a checked-in mirror of the same contract schemas
for isolated ab-validator flake checks. Nix stores a source tree with the
`schemas/` symlink intact but without its out-of-subproject target, so smoke
checks that need schema bytes stage files from `nix-schemas/`. The monorepo
schema drift gate byte-compares this mirror against `abc/schemas`.

The `schema-contracts.json` manifest remains a producer-side snapshot used to
compare schema ids, versions, titles, and hashes against ABC's authoritative
`abc/schemas/schema-contracts.json`.

The cross-repo contract surface is listed in `schema-contracts.json`. Each row
records the schema `$id`, `version`, and ABC schema hash using
`abc-legacy-json-c14n-v0`, matching ABC's own manifest and compatibility
identity checks.

Refresh procedure:

1. Keep `schemas/` as a symlink to the monorepo `abc/schemas` directory, keep
   `nix-schemas/` byte-identical to the contract files listed in
   `abc/schemas/schema-contracts.json`, and keep
   `data/source-region-publication-policy-v0.json` as a symlink to ABC's source
   region publication policy.
2. Copy or regenerate the ABC `schema-contracts.json` snapshot:

   ```sh
   python scripts/schema_contracts.py --write
   ```

3. Run the drift check:

   ```sh
   python scripts/schema_contracts.py
   ```

4. From the monorepo root, run the drift gate:

   ```sh
   just schema-drift
   ```

For temporary two-repo operation, compare the snapshot against an explicit ABC
checkout:

   ```sh
   python scripts/compare_abc_schema_contracts.py --abc /path/to/abc
   ```

Schema bytes, versions, and hashes must move together. Historical mapping or
compatibility evidence should keep its historical schema hashes; new evidence
uses the current manifest row.
