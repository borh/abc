# Ruby Reading Evidence Contract

**Date:** 2026-07-07
**Owner:** ab-validator

Ruby readings are analyzer-evaluation evidence. They do not replace parser-IR visible text, sentence splitting input, or tokenizer input in the publication pipeline.

## Evidence Row

The contract applies when `nway_region_oracle_evidence.oracle_source == "ruby"`.

| Field | Meaning |
|---|---|
| `projected_char_start`, `projected_char_end` | Ruby base span in projected visible text coordinates. |
| `classification` | `resolved`, `nonstandard_ruby`, or `no_comparable_reading`. |
| `evidence_detail.ruby_base` | Source ruby base text used for analyzer span alignment. |
| `evidence_detail.ruby_reading` | Editor-provided ruby reading. |
| `evidence_detail.ruby_reading_norm` | Canonicalized reading for comparison. |
| `evidence_detail.per_analyzer.*.reading` | Analyzer reading concatenated over the base span, or null. |
| `evidence_detail.per_analyzer.*.norm` | Canonicalized analyzer reading, or null. |
| `evidence_detail.per_analyzer.*.match` | Whether the analyzer reading equals the normalized ruby reading. |
| `evidence_detail.per_analyzer.*.align` | `exact`, `boundary-misalign`, or `no-reading`. |

## Boundary Rule

Analyzer span alignment uses the ruby base span only. A reading such as `名前《めいしょう》` can adjudicate whether an analyzer reading is better, but it must not change the sentence span, parser-IR node span, or tokenizer input text.
