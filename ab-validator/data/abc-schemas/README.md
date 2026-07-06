# Vendored ABC Schema Contracts

This directory is ab-validator's checked-in snapshot of the ABC schemas it
consumes at the adapter and publication boundary.

The cross-repo contract surface is listed in `schema-contracts.json`. Each row
records the schema `$id`, `version`, and ABC schema hash using
`abc-legacy-json-c14n-v0`, matching ABC's own manifest and compatibility
identity checks.

Refresh procedure:

1. Copy the relevant `abc/schemas/*.schema.json` files into `schemas/`.
2. Copy or regenerate the ABC `schema-contracts.json` snapshot:

   ```sh
   python3 scripts/schema_contracts.py --write
   ```

3. Run the drift check:

   ```sh
   python3 scripts/schema_contracts.py
   ```

4. When an ABC checkout is available, compare the vendored snapshot against
   ABC's authoritative contract manifest:

   ```sh
   python3 scripts/compare_abc_schema_contracts.py --abc /path/to/abc
   ```

Schema bytes, versions, and hashes must move together. Historical mapping or
compatibility evidence should keep its historical schema hashes; new evidence
uses the current manifest row.
