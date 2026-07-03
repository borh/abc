# AAT-to-Parser-IR Mapping Generation

Date: 2026-07-03

## Summary

| Metric | Value |
|---|---:|
| files scanned | 35583 |
| files failed to parse | 0 |
| files with UNSUPPORTED | 1012 |
| files with warigaki | 192 |
| total parser-IR nodes emitted | 16230882 |
| total ledger entries | 27322032 |
| generated mapping rules | 119 |

## Category Counts

| Category | Count |
|---|---:|
| LOSS | 344869 |
| AMBIGUITY | 16722096 |
| INVENTION | 7043185 |
| UNSUPPORTED | 14031 |
| STRUCTURAL | 3197851 |

## Schema Hashes

- mapping schema hash: `sha256:38ec7f0e5affb10329b550a091cd3a6fb5a25e26fd469dfe9f8249970cf9adb4`
- target parser-IR schema hash: `sha256:41c43f0c88a66c31ae4fbf9b9eeb04de92756082acaaaa1c2e21f1a5bf74a396`

## Policy Checks

- `ruby.direction` projects directly into parser-IR.
- `style` maps to parser-IR `emphasis`.
- `windows-31j-lossy` maps to parser-IR `source.encoding = Shift_JIS` with an `AMBIGUITY` entry.
- Generated mapping is derived from folded measured rule buckets, not the historical 27-rule synthesized table.


## Inputs

- `scratch/morph-full-corpus/aats/aozora-rs-adapter`
- `/db/ab-validator/aat-corpus/aozora2html-full-20260703T020301Z/aat/aozora2html-adapter`
