# ABC Schema Snapshot

This directory vendors the minimal ABC JSON schema snapshot needed by
`ab-aat-to-parser-ir` in pure Nix checks:

- `schemas/aat-parser-ir-mapping.schema.json`
- `schemas/parser-ir.schema.json`
- `schemas/aat-parser-ir-divergence.schema.json`

The files are copied byte-for-byte from the ABC repo revision used to generate
`data/aat-to-parser-ir-mapping-v1.json`. The converter still accepts
`--abc-root` and `AB_ABC_ROOT` so callers can validate against an explicit ABC
checkout when needed.
