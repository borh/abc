# CI Smoke Corpus v0

Status: Draft
Date: 2026-04-26

The smoke corpus should validate parser, manifest, TEI, and RDF boundaries on
every PR without attempting a full corpus build.

## Runtime Target

The default gate should run in under five minutes on baseline CI hardware. The
corpus size is adjustable; start with fewer than 100 works and increase only
when runtime remains acceptable.

## Coverage Buckets

| Bucket | Purpose | Initial Count |
| --- | --- | --- |
| Ruby scope | explicit, inferred, group, mid-word, ambiguous ruby | 4 |
| Gaiji | resolved Unicode, IVS, unresolved, image fallback | 4 |
| Editor notes | headings, emphasis, indentation, page breaks | 4 |
| Images/captions | figure and caption preservation | 2 |
| Metadata join | author/title/source/date edge cases | 3 |
| Large work | memory and batching behavior | 1 |
| Encoding comparison | Himawari or other alternate-encoding works | 1 |
| Expected warnings | unsupported or ambiguous constructs | 3 |
| Expected failures | failure manifest fixture | 1 |

## Required Checks

- Manifest JSON Schema validation.
- Parser IR JSON Schema validation.
- Canonicalization fixture checks.
- Warning/error taxonomy aggregation.
- TEI validation against the selected Relax NG schema.
- Deterministic manifest-to-RDF generation.
- Query index entry generation for each successful artifact.
- Parser performance regression report for the smoke corpus.

## Selection Policy

Each chosen work must record Aozora snapshot hash, work ID, source path, reason
for inclusion, and expected warning/failure behavior. The list itself is a
versioned and hash-addressed input to CI.

## Versioned List Format

The selected corpus list should be stored as JSON or EDN with these fields per
entry:

```text
work_id:
source_path:
aozora_snapshot_hash:
category:
reason:
expected_status:
expected_warning_codes:
```

The list file hash is recorded in CI run summaries and parser benchmark
reports.
