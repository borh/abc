# AAT-to-Parser-IR Mapping Generation

Date: 2026-07-03

## Summary

| Metric | Value |
|---|---:|
| files scanned | 53427 |
| files failed to parse | 0 |
| files with UNSUPPORTED | 1630 |
| files with warigaki | 243 |
| total parser-IR nodes emitted | 26888713 |
| total ledger entries | 48734891 |
| generated mapping rules | 130 |

## Category Counts

| Category | Count |
|---|---:|
| LOSS | 3815445 |
| AMBIGUITY | 27617002 |
| INVENTION | 10691975 |
| UNSUPPORTED | 27464 |
| STRUCTURAL | 6583005 |

## Schema Hashes

- mapping schema hash: `sha256:38ec7f0e5affb10329b550a091cd3a6fb5a25e26fd469dfe9f8249970cf9adb4`
- target parser-IR schema hash: `sha256:41c43f0c88a66c31ae4fbf9b9eeb04de92756082acaaaa1c2e21f1a5bf74a396`

## Identity Projection

- `version`, `meta.adapter`, and `meta.adapter_version` project into parser-IR `derived_from`.
- `mapping_id`, `mapping_version`, and `mapping_schema_hash` project from the mapping document into parser-IR `derived_from`.
- `mapping_hash` remains an external manifest input and is not stored inside parser-IR `derived_from`.

## Policy Checks

- `ruby.direction` projects directly into parser-IR.
- `style` maps to parser-IR `emphasis`.
- `windows-31j-lossy` maps to parser-IR `source.encoding = Shift_JIS` with an `AMBIGUITY` entry.
- Generated mapping is derived from folded measured rule buckets, not the historical 27-rule synthesized table.


## Inputs

- `/home/bor/Projects/ab-validator/scratch/morph-full-corpus/aats/aozora-rs-adapter`
- `/db/ab-validator/aat-corpus/aozora2html-full-20260703T020301Z/aat/aozora2html-adapter`
- `/db/ab-validator/aat-corpus/aozora-epub3-full-20260704T050652Z-300s/aat/aozora-epub3-adapter`
