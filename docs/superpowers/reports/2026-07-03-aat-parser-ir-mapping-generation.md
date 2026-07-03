# AAT-to-Parser-IR Mapping Generation

Date: 2026-07-03

## Summary

| Metric | Value |
|---|---:|
| files scanned | 17894 |
| files failed to parse | 0 |
| files with UNSUPPORTED | 0 |
| files with warigaki | 0 |
| total parser-IR nodes emitted | 7828615 |
| total ledger entries | 4781187 |
| generated mapping rules | 25 |

## Category Counts

| Category | Count |
|---|---:|
| LOSS | 159855 |
| AMBIGUITY | 111115 |
| INVENTION | 3864599 |
| UNSUPPORTED | 0 |
| STRUCTURAL | 645618 |

## Schema Hashes

- mapping schema hash: `sha256:38ec7f0e5affb10329b550a091cd3a6fb5a25e26fd469dfe9f8249970cf9adb4`
- target parser-IR schema hash: `sha256:41c43f0c88a66c31ae4fbf9b9eeb04de92756082acaaaa1c2e21f1a5bf74a396`

## Policy Checks

- `ruby.direction` projects directly into parser-IR.
- `style` maps to parser-IR `emphasis`.
- `windows-31j-lossy` maps to parser-IR `source.encoding = Shift_JIS` with an `AMBIGUITY` entry.
- Generated mapping is derived from folded measured rule buckets, not the historical 27-rule synthesized table.

Mapping and adapter-boundary follow-up: `docs/superpowers/specs/2026-07-03-mapping-and-adapter-boundary-decision.md`.
