# Post Parser-IR Conversion Sync

Date: 2026-07-04

## Verdict

`PARSER_IR_CONVERTER_CORPUS_CLEAN`

The `crates/ab-aat-to-parser-ir` converter has consumed the July 3
`CLI_READY_WITH_LOWER_BOUND_CAVEAT` gate. It now converts both measured local
AAT corpora into ABC parser-IR with zero conversion failures, full measured rule
coverage, and release-smoke coverage through Cargo, the shell smoke, and the
flake check.

This sync supersedes the "Next Gate" section in
`docs/superpowers/reports/2026-07-03-post-measurement-abc-sync.md`. The residual
aozora2html runtime/oracle buckets remain adapter-fidelity work; they are not a
parser-IR protocol blocker.

## Evidence

- Converter crate: `crates/ab-aat-to-parser-ir`
- Mapping artifact: `data/aat-to-parser-ir-mapping-v1.json`
- Mapping version: `0.2.0`
- Mapping hash:
  `sha256:68b0868b25f3b072a47d781099178bf2a31e4b16c561814f5e13e3801714d089`
- Mapping schema hash:
  `sha256:38ec7f0e5affb10329b550a091cd3a6fb5a25e26fd469dfe9f8249970cf9adb4`
- Target parser-IR schema hash:
  `sha256:41c43f0c88a66c31ae4fbf9b9eeb04de92756082acaaaa1c2e21f1a5bf74a396`
- Full-corpus audit:
  `docs/superpowers/reports/2026-07-04-aat-parser-ir-full-corpus-conversion.md`
- Summary JSON:
  `docs/superpowers/reports/2026-07-04-aat-parser-ir-full-corpus-conversion.summary.json`

| Metric | Value |
|---|---:|
| files attempted | 35,583 |
| files succeeded | 35,583 |
| files failed | 0 |
| parser-IR nodes | 16,243,174 |
| divergence records | 541,167 |
| divergence occurrences | 30,347,883 |
| mapping rules total | 116 |
| mapping rules emitted | 116 |
| mapping rules missing | 0 |

| Corpus | Files | Result |
|---|---:|---|
| aozora-rs-adapter | 17,894 | 17,894 succeeded, 0 failed |
| aozora2html-adapter | 17,689 | 17,689 succeeded, 0 failed |

## Identity and Compatibility

Parser-IR outputs now carry `derived_from` with AAT producer identity and mapping
identity:

- `aat_version`, `aat_adapter`, and `aat_adapter_version` come from the AAT
  document.
- `mapping_id`, `mapping_version`, and `mapping_schema_hash` come from the
  mapping document.
- `mapping_hash` remains outside parser-IR and is supplied as a manifest input
  for ABC compatibility validation.

The full audit generated two ABC registry candidates in
`docs/superpowers/reports/2026-07-04-aat-parser-ir-compatibility-candidates.edn`.
ABC admitted those measured entries in commit `4cb15df`:

| adapter | adapter_version | files_succeeded | rules_emitted | rules_missing | unsupported_occurrences |
|---|---|---:|---:|---:|---:|
| aozora-rs | aozora-rs-adapter 0.1.0 2b4e8d1 | 17,894 | 26 | 90 | 0 |
| aozora2html | aozora2html-adapter 0.1.0 gem-3.0.1 | 17,689 | 115 | 1 | 14,230 |

Adapter-version matching is exact. A future adapter-version tuple requires a
new measured conversion-audit entry rather than wildcard or prefix registry
matching.

## Release Surface

The converter is exposed as:

- Cargo package: `ab-aat-to-parser-ir`
- Flake package/app: `.#ab-aat-to-parser-ir`
- Flake check: `checks.<system>.aat-to-parser-ir-smoke`
- Just smoke target: `just aat-to-parser-ir-smoke`
- Just full-audit target: `just aat-to-parser-ir-full-audit 24`

The crate-specific usage notes live in `crates/ab-aat-to-parser-ir/README.md`.

## Next Work

Proceed with the residual aozora2html bucket characterization:

- verify the `normalize_figure_alt` protocol-error fix remains covered,
- measure and document timeout-tail behavior before changing default timeouts,
- commit parse-incomplete classification tooling and report,
- characterize `visible_text_body_order` failures without changing the
  AAT-to-parser-IR mapping or divergence protocol.
