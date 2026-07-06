# Vendored ABC Schema Contracts

This directory is ab-validator's checked-in contract view of the ABC schemas it
consumes at the adapter and publication boundary.

In the monorepo, `schemas/` is a symlink to `../../../abc/schemas` so schema
bytes have one source of truth. The `schema-contracts.json` manifest remains a
producer-side snapshot used to compare schema ids, versions, titles, and hashes
against ABC's authoritative `abc/schemas/schema-contracts.json`.

The cross-repo contract surface is listed in `schema-contracts.json`. Each row
records the schema `$id`, `version`, and ABC schema hash using
`abc-legacy-json-c14n-v0`, matching ABC's own manifest and compatibility
identity checks.

Refresh procedure:

1. Keep `schemas/` as a symlink to `../../../abc/schemas`.
2. Copy or regenerate the ABC `schema-contracts.json` snapshot:

   ```sh
   python3 scripts/schema_contracts.py --write
   ```

3. Run the drift check:

   ```sh
   python3 scripts/schema_contracts.py
   ```

4. From the monorepo root, run the drift gate:

   ```sh
   just schema-drift
   ```

For temporary two-repo operation, compare the snapshot against an explicit ABC
checkout:

   ```sh
   python3 scripts/compare_abc_schema_contracts.py --abc /path/to/abc
   ```

Schema bytes, versions, and hashes must move together. Historical mapping or
compatibility evidence should keep its historical schema hashes; new evidence
uses the current manifest row.
