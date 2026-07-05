# AAT-to-Parser-IR Mapping Generation

Date: 2026-07-03

## Summary

| Metric | Value |
|---|---:|
| files scanned | 89169 |
| files failed to parse | 0 |
| files with UNSUPPORTED | 23564 |
| files with warigaki | 560 |
| total parser-IR nodes emitted | 32793931 |
| total ledger entries | 61600541 |
| generated mapping rules | 714 |

## Category Counts

| Category | Count |
|---|---:|
| LOSS | 9593256 |
| AMBIGUITY | 33464788 |
| INVENTION | 13474742 |
| UNSUPPORTED | 4836371 |
| STRUCTURAL | 231384 |

## Schema Hashes

- mapping schema hash: `sha256:38ec7f0e5affb10329b550a091cd3a6fb5a25e26fd469dfe9f8249970cf9adb4`
- target parser-IR schema hash: `sha256:d98eb9684e7a88f5b62693dd582e28f14834ff85011dc7297e7b41516f7be913`

## Identity Projection

- `version`, `meta.adapter`, and `meta.adapter_version` project into parser-IR `derived_from`.
- `mapping_id`, `mapping_version`, and `mapping_schema_hash` project from the mapping document into parser-IR `derived_from`.
- `mapping_hash` remains an external manifest input and is not stored inside parser-IR `derived_from`.

## Policy Checks

- `ruby.direction` projects directly into parser-IR.
- Whole-paragraph layout `style` wrappers and paragraph-only `jisage_block` containers project to parser-IR `paragraphs[].layout`.
- Non-layout `style` containers map to parser-IR `emphasis`.
- `windows-31j-lossy` maps to parser-IR `source.encoding = Shift_JIS` with an `AMBIGUITY` entry.
- Generated mapping is derived from folded measured rule buckets, not the historical 27-rule synthesized table.


## Inputs

- `/home/bor/Projects/ab-validator/scratch/morph-full-corpus/aats/aozora-rs-adapter`
- `/db/ab-validator/aat-corpus/aozora2html-full-20260703T020301Z/aat/aozora2html-adapter`
- `/db/ab-validator/aat-corpus/aozora-epub3-full-20260704T050652Z-300s/aat/aozora-epub3-adapter`
- `/db/ab-validator/aat-corpus/aozora2-full-20260705T083650Z-layout-fix5/aat/aozora2-adapter`
- `/db/ab-validator/aat-corpus/aozora-full-20260705T015007Z/aat/aozora-adapter`
