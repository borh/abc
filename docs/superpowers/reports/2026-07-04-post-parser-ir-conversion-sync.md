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
- Mapping version: `0.1.1`
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
| divergence records | 612,333 |
| divergence occurrences | 30,419,049 |
| mapping rules total | 118 |
| mapping rules emitted | 118 |
| mapping rules missing | 0 |

| Corpus | Files | Result |
|---|---:|---|
| aozora-rs-adapter | 17,894 | 17,894 succeeded, 0 failed |
| aozora2html-adapter | 17,689 | 17,689 succeeded, 0 failed |

## Release Surface

The converter is exposed as:

- Cargo package: `ab-aat-to-parser-ir`
- Flake package/app: `.#ab-aat-to-parser-ir`
- Flake check: `checks.<system>.aat-to-parser-ir-smoke`
- Just smoke target: `just aat-to-parser-ir-smoke`
- Just full-audit target: `just aat-to-parser-ir-full-audit JOBS=24`

The crate-specific usage notes live in `crates/ab-aat-to-parser-ir/README.md`.

## Next Work

Proceed with the residual aozora2html bucket characterization:

- verify the `normalize_figure_alt` protocol-error fix remains covered,
- measure and document timeout-tail behavior before changing default timeouts,
- commit parse-incomplete classification tooling and report,
- characterize `visible_text_body_order` failures without changing the
  AAT-to-parser-IR mapping or divergence protocol.
