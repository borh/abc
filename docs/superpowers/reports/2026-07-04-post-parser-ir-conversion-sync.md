# Post Parser-IR Conversion Sync

Date: 2026-07-04

## Verdict

`PARSER_IR_CONVERTER_CORPUS_CLEAN`

The `crates/ab-aat-to-parser-ir` converter has consumed the July 3
`CLI_READY_WITH_LOWER_BOUND_CAVEAT` gate and the July 4 `aozora-epub3` adapter
measurement. It now converts all three measured local AAT corpora into ABC
parser-IR with zero conversion failures, full measured rule coverage, and
release-smoke coverage through Cargo, the shell smoke, and the flake check.

This sync supersedes the "Next Gate" section in
`docs/superpowers/reports/2026-07-03-post-measurement-abc-sync.md`. The residual
aozora2html runtime/oracle buckets remain adapter-fidelity work; they are not a
parser-IR protocol blocker.

## Evidence

- Converter crate: `crates/ab-aat-to-parser-ir`
- Mapping artifact: `data/aat-to-parser-ir-mapping-v1.json`
- Mapping version: `0.2.1`
- Mapping hash:
  `sha256:b508665af72c237fc60f00b720f80db2b16148aa64b5d1cc723a2948ee576390`
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
| files attempted | 53,427 |
| files succeeded | 53,427 |
| files failed | 0 |
| parser-IR nodes | 26,914,048 |
| divergence records | 784,485 |
| divergence occurrences | 55,027,637 |
| mapping rules total | 130 |
| mapping rules emitted | 130 |
| mapping rules missing | 0 |

| Corpus | Files | Result |
|---|---:|---|
| aozora-rs-adapter | 17,894 | 17,894 succeeded, 0 failed |
| aozora2html-adapter | 17,689 | 17,689 succeeded, 0 failed |
| aozora-epub3-adapter | 17,844 | 17,844 succeeded, 0 failed |

## Identity and Compatibility

Parser-IR outputs now carry `derived_from` with AAT producer identity and mapping
identity:

- `aat_version`, `aat_adapter`, and `aat_adapter_version` come from the AAT
  document.
- `mapping_id`, `mapping_version`, and `mapping_schema_hash` come from the
  mapping document.
- `mapping_hash` remains outside parser-IR and is supplied as a manifest input
  for ABC compatibility validation.

The full audit generated three ABC registry candidates in
`docs/superpowers/reports/2026-07-04-aat-parser-ir-compatibility-candidates.edn`.
ABC admitted the earlier `0.2.0` two-adapter entries in commit `4cb15df`; the
`0.2.1` candidates below still need ABC registry admission:

| adapter | adapter_version | files_succeeded | rules_emitted | rules_missing | unsupported_occurrences |
|---|---|---:|---:|---:|---:|
| aozora-epub3 | aozora-epub3-adapter 0.1.0 AozoraEpub3-JDK21-1.3.4-jdk21 | 17,844 | 64 | 66 | 13,234 |
| aozora-rs | aozora-rs-adapter 0.1.0 2b4e8d1 | 17,894 | 26 | 104 | 0 |
| aozora2html | aozora2html-adapter 0.1.0 gem-3.0.1 | 17,689 | 115 | 15 | 14,230 |

Adapter-version matching is exact. A future adapter-version tuple requires a
new measured conversion-audit entry rather than wildcard or prefix registry
matching.

## Source Authority Caveat

Parser-IR conversion evidence over four adapters is not a proof that AAT can
represent the Aozora source language. The source-authority inventory at
`docs/superpowers/reports/2026-07-04-source-authority-representability.md`
is the representability gate. Parser evidence remains triangulation only.

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

- ask ABC to admit the three `0.2.1` conversion-audit candidates,
- characterize `aozora-epub3` adapter-fidelity failures from
  `/db/ab-validator/aat-corpus/aozora-epub3-full-20260704T050652Z-300s`,
- use `aozora-epub3` as a fast second oracle while characterizing
  `visible_text_body_order`, `gaiji_resolution`, `ruby_completeness`, and
  parse-incomplete residuals.
