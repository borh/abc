# Comprehensive Parser Acceptance Criteria

Status: Accepted
Date: 2026-07-06
Owner boundary: ab-validator owns parser measurement, source coverage, and
acceptance evidence. ABC owns publication rendering, TEI profile, custom
preservation schemas, and final bundle validation.

## Purpose

A future comprehensive Aozora parser is useful only if it improves measured
publication accounting. It must not replace the admission ledger or bypass
source authority.

Acceptance is based on source inventory, parser-IR emission, publication bundle
validation, and bounded performance evidence. TEI-EAJ remains calibration
evidence, not the admission authority.

This status accepts the criteria for judging parser candidates. It does not
admit any parser candidate by itself; parser admission still requires measured
evidence against these criteria.

## Required Evidence Inputs

- `docs/superpowers/reports/2026-07-06-aozora-publication-next-work.summary.json`
- `docs/superpowers/reports/2026-07-06-source-reference-reconciliation.summary.json`
- `docs/superpowers/reports/2026-07-04-source-authority-representability.summary.json`
- `docs/superpowers/reports/2026-07-04-aat-parser-ir-full-corpus-conversion.summary.json`
- `docs/superpowers/reports/2026-07-04-parser-performance-measurement.md`
- `docs/superpowers/specs/tei-p5-mapping-dossiers/`

## Source Coverage Criteria

- Every source inventory row observed in the corpus is parsed, raw-preserved,
  source-apparatus-classified, or diagnostically preserved.
- `source_region_coverage.unsupported_body_markup_occurrences == 0`.
- `source_region_coverage.unknown_region_occurrences == 0`.
- `source_region_coverage.unknown_unreviewed_occurrences == 0`.
- `docs/superpowers/reports/2026-07-06-source-reference-reconciliation.summary.json`
  remains `SOURCE_REFERENCE_RECONCILIATION_COMPLETE`.

## Parser-IR Emission Criteria

- Parser-IR validates against the current schema.
- Parser-IR carries every source-observed body markup fact into one of the
  admitted publication lanes: TEI P5, TEI plus ABC extension, custom sidecar,
  diagnostic preservation, or body-visible-text-only output.
- Parser-IR does not place ruby readings, source apparatus, source notes,
  layout metadata, custom records, warnings, or provenance into plaintext.
- Parser identity, adapter identity, mapping identity, and schema hashes remain
  available to the publication bundle.

## Publication Bundle Criteria

- Representative bundle validation passes.
- Full-scope bundle validation passes before the parser can replace an existing
  comparison lane in admission evidence.
- Parser-IR, TEI, plaintext, custom preservation, source-region evidence, and
  manifests agree under ABC validation.
- Every custom preservation fact has schema, hash, owner, pointer or explicit
  no-pointer reason, and validation.

## Performance And DNF Criteria

- Parser measurement uses explicit bounded budgets.
- DNF within the allotted budget is recorded as evidence.
- DNF is not retried indefinitely during recurring comparison runs.
- Performance reports include enough wall-time, CPU, or memory evidence to
  compare parser candidates and guide a future Rust parser.

## Editorial Enrichment Non-Goals

TEI-EAJ editorial enrichment is not required unless source markup contains the
fact. A parser is not required to infer people, places, speakers, roles, speech
acts, or other semantic enrichment from plain text.

## Admission Rule

A parser is admissible only when the next-work ledger can cite measured evidence
that the parser satisfies the source coverage, parser-IR emission, publication
bundle, and performance criteria above. New parser work must not start as a
replacement for the admission ledger; it must consume the ledger and close named
items in it.
