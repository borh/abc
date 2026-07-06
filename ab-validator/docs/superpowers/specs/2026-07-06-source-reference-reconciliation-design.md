# Source Reference Reconciliation Design

**Goal:** Compare the current source inventory against the curated official-Aozora syntax table and the pinned P4suta notation-spec vectors, without treating parser consensus or unofficial vectors as source authority.

## Context

The publication goal is complete Aozora Bunko markup accounting: every observed source marker must be represented by TEI P5, TEI plus ABC extension, custom sidecar, plaintext body text, diagnostic preservation, or an explicit unsupported blocker.

The repo already has three related evidence streams:

- `docs/superpowers/reports/2026-07-04-source-authority-representability.summary.json`: corpus-observed source inventory.
- `data/aozora-syntax-coverage.toml`: curated syntax rows with official Aozora reference sources, source patterns, parser evidence, adapter evidence, and representability.
- `docs/superpowers/reports/2026-07-05-aozora-notation-spec-comparison.summary.json`: P4suta notation-spec conformance evidence across parser lanes.

These streams answer different questions and should not be collapsed:

- Source inventory answers what appears in the corpus.
- The syntax table answers what this repo has admitted and modeled, with links to official Aozora rule files.
- P4suta vectors answer how parsers behave against an unofficial but useful comparison spec.

## Design

Add a report builder that emits a reconciliation summary:

```text
source inventory rows
  -> matched to syntax.representability.source_inventory_row
  -> matched to official-reference syntax rows
  -> compared with P4suta vector feature families
```

The report classifies rows and features into mechanically checkable buckets:

- `documented_observed`: syntax rows with an official Aozora reference and a matched source-inventory row with occurrences.
- `documented_unobserved`: syntax rows with an official Aozora reference but no observed source-inventory occurrence.
- `observed_without_syntax_row`: source-inventory rows with occurrences that no syntax row claims.
- `p4suta_feature_mapped`: P4suta feature families mapped to one or more syntax/source-inventory rows.
- `p4suta_feature_unmapped`: P4suta feature families with no mapped syntax/source-inventory row.
- `p4suta_feature_unobserved`: mapped P4suta feature families whose mapped inventory rows have zero occurrences.

## Authority Rules

- Official Aozora authority is represented by `data/aozora-syntax-coverage.toml` rows that cite `references/aozorabunko/rules/...`.
- P4suta `aozora-notation-spec` is comparison evidence only; unmapped P4suta features become review items, not automatic unsupported blockers.
- Corpus source inventory remains the authority for observed prevalence.
- Parser lanes remain supporting evidence only.
- Plaintext policy is unaffected: no ruby readings, source apparatus, source notes, layout metadata, custom records, warnings, or provenance.

## Inputs And Outputs

Inputs:

- `--syntax-coverage data/aozora-syntax-coverage.toml`
- `--source-summary docs/superpowers/reports/2026-07-04-source-authority-representability.summary.json`
- `--notation-summary docs/superpowers/reports/2026-07-05-aozora-notation-spec-comparison.summary.json`
- optional `--manual-root references/aozorabunko/rules`

Outputs:

- `docs/superpowers/reports/2026-07-06-source-reference-reconciliation.summary.json`
- `docs/superpowers/reports/2026-07-06-source-reference-reconciliation.md`

## Report Verdicts

- `SOURCE_REFERENCE_RECONCILIATION_COMPLETE`: no observed source row lacks a syntax row and no P4suta feature family is unmapped.
- `SOURCE_REFERENCE_RECONCILIATION_REVIEW_REQUIRED`: one or more observed source rows or P4suta feature families need review.

For the current measured corpus, the expected verdict is complete when every
observed source row is claimed by a syntax row and every P4suta feature is
mapped or explicitly comparison-only. `documented_unobserved` rows are retained
as coverage notes, not blockers, because they have no corpus prevalence.

## Self-Review

- No parser consensus is used as authority.
- P4suta is explicitly comparison-only.
- Official Aozora references are kept through syntax-table source paths rather than scraped into an unstable second manual parser.
- The output is a report/gate input, not a schema or renderer policy change.
