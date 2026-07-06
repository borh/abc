# ABC Schema Snapshot

This directory vendors the minimal ABC JSON schema and policy snapshot needed by
ab-validator pure Nix checks and publication-coverage gates:

- `schemas/aat-parser-ir-mapping.schema.json`
- `schemas/parser-ir.schema.json`
- `schemas/aat-parser-ir-divergence.schema.json`
- `schemas/parser-ir-publication-preservation.schema.json`
- `schemas/source-region-coverage.schema.json`
- `schemas/manifest.schema.json`
- `data/source-region-publication-policy-v0.json`

The files are copied byte-for-byte from the ABC repo revision used by the
current checked-in reports. The converter still accepts `--abc-root` and
`AB_ABC_ROOT` so callers can validate against an explicit ABC checkout when
needed.
