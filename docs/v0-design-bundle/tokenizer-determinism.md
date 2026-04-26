# Tokenizer Determinism v0

Status: Draft
Date: 2026-04-26

Tokenization belongs in the Exact reproducibility tier only when the tokenizer
and dictionary are fully pinned and empirical fixtures demonstrate stable
output.

## Required Pins

- Tokenizer name and version.
- Tokenizer build/source hash.
- Dictionary archive hash.
- Dictionary build options.
- Locale and encoding settings.
- Thread/concurrency settings.
- Normalization policy for input text.
- Output format schema or descriptor hash.

## Known-Good Configuration Template

```text
name:
version:
tokenizer_build_hash:
dictionary_name:
dictionary_archive_hash:
dictionary_build_options:
locale:
encoding:
threads:
input_normalization:
output_format_spec_hash:
fixture_hash:
```

The fixture itself should live under `fixtures/tokenizer/<tokenizer-id>/` once
a tokenizer is chosen. Until then, this document defines the evidence required
to classify a tokenizer as Exact.

## Classification Procedure

1. Record the full configuration template above.
2. Run the tokenizer twice on the same fixture with the same environment.
3. Compare byte-identical output and diagnostic sidecars.
4. Change one pinned input, such as dictionary hash, and confirm the artifact
   identity changes.
5. If any output varies without a changed pinned input, classify the tokenizer
   as Stable IR or Bounded instead of Exact.

The parser/tokenizer ADR owner decides classification from the fixture output
and records the result in an ADR update.

## Normalization Policy

Tokenization fixtures must state whether input text and metadata strings use
source bytes, NFC, or NFKC. Japanese names and bibliographic strings can change
hashes under different normalization choices, so tokenizer artifacts must not
inherit an implicit locale or Unicode normalization policy.

## Acceptance Criteria

- Running the fixture twice produces byte-identical output.
- Running with a changed dictionary hash produces a distinct ArtifactID.
- A tokenizer that cannot meet exact repeatability is classified as Stable IR
  or Bounded rather than Exact.
