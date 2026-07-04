# Post Parser-IR Conversion Sync

Date: 2026-07-04

## Verdict

`PARSER_IR_CONVERTER_CORPUS_CLEAN`

The `crates/ab-aat-to-parser-ir` converter has consumed the July 3
`CLI_READY_WITH_LOWER_BOUND_CAVEAT` gate, the July 4 `aozora-epub3` adapter
measurement, and the Level 3 parser-IR schema rotation. It now converts all
three measured local AAT corpora into ABC parser-IR with zero conversion
failures, full measured rule coverage, and release-smoke coverage through
Cargo and the shell smoke.

This sync supersedes the "Next Gate" section in
`docs/superpowers/reports/2026-07-03-post-measurement-abc-sync.md`. The residual
aozora2html runtime/oracle buckets remain adapter-fidelity work; they are not a
parser-IR protocol blocker.

## Evidence

- Converter crate: `crates/ab-aat-to-parser-ir`
- Mapping artifact: `data/aat-to-parser-ir-mapping-v1.json`
- Mapping version: `0.2.2`
- Mapping hash:
  `sha256:17fb33db137f23ae30325558545ae364d21488d773aed733df1af012af6658c0`
- Mapping schema hash:
  `sha256:38ec7f0e5affb10329b550a091cd3a6fb5a25e26fd469dfe9f8249970cf9adb4`
- Target parser-IR schema hash:
  `sha256:90c9c46c1e3048cf2559733d4ee7f3e37827756e2527548ba981f023a1232fa2`
- Full-corpus audit:
  `docs/superpowers/reports/2026-07-04-aat-parser-ir-full-corpus-conversion.md`
- Summary JSON:
  `docs/superpowers/reports/2026-07-04-aat-parser-ir-full-corpus-conversion.summary.json`

| Metric | Value |
|---|---:|
| files attempted | 53,427 |
| files succeeded | 53,427 |
| files failed | 0 |
| parser-IR nodes | 26,907,266 |
| divergence records | 674,102 |
| divergence occurrences | 42,482,637 |
| mapping rules total | 127 |
| mapping rules emitted | 127 |
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
`0.2.2` candidates below still need ABC registry admission:

| adapter | adapter_version | files_succeeded | rules_emitted | rules_missing | unsupported_occurrences |
|---|---|---:|---:|---:|---:|
| aozora-epub3 | aozora-epub3-adapter 0.1.0 AozoraEpub3-JDK21-1.3.4-jdk21 | 17,844 | 61 | 66 | 13,234 |
| aozora-rs | aozora-rs-adapter 0.1.0 2b4e8d1 | 17,894 | 25 | 102 | 0 |
| aozora2html | aozora2html-adapter 0.1.0 gem-3.0.1 | 17,689 | 112 | 15 | 14,230 |

## TEI-EAJ Structural Comparison

The Melos-only structural probe is superseded by the whole
TEI-EAJ/aozora_tei workset comparison:

- report:
  `docs/superpowers/reports/2026-07-04-tei-eaj-structural-expansion.md`
- summary:
  `docs/superpowers/reports/2026-07-04-tei-eaj-structural-expansion.summary.json`

| Metric | Value |
|---|---:|
| TEI-EAJ files | 62 |
| candidate work IDs | 50 |
| rows with AAT evidence | 55 |
| parser-IR gap rows | 0 |
| adapter gap rows | 17 |
| evidence gap rows | 7 |

Interpretation: Level 3 parser-IR paragraph/source-note representation is no
longer the row-level blocker for the TEI-EAJ workset. Remaining structural work
is adapter/evidence coverage: materialize missing ABC counterparts and improve
adapter paragraph/source-note preservation where TEI-EAJ exposes richer
structure than a specific adapter AAT.

Adapter-version matching is exact. A future adapter-version tuple requires a
new measured conversion-audit entry rather than wildcard or prefix registry
matching.

## Source Authority Caveat

Parser-IR conversion evidence over four adapters is not a proof that AAT can
represent the Aozora source language. The source-authority inventory at
`docs/superpowers/reports/2026-07-04-source-authority-representability.md`
is the representability gate, and the current run is failing:
`SOURCE_AUTHORITY_GATE_FAILING_REVIEW_REQUIRED`. Parser evidence remains
triangulation only until the source inventory has no unallowlisted unknown
markers and no reached `needs_research` representability rows.

## Release Surface

The converter is exposed as:

- Cargo package: `ab-aat-to-parser-ir`
- Flake package/app: `.#ab-aat-to-parser-ir`
- Flake check: `checks.<system>.aat-to-parser-ir-smoke`
- Just smoke target: `just aat-to-parser-ir-smoke`
- Just full-audit target: `just aat-to-parser-ir-full-audit 24`

The crate-specific usage notes live in `crates/ab-aat-to-parser-ir/README.md`.

## Next Work

Proceed with ABC admission and residual adapter/evidence characterization:

- ask ABC to admit the three `0.2.2` conversion-audit candidates,
- keep the whole TEI-EAJ/aozora_tei structural expansion as the Level 3
  comparison artifact instead of the Melos-only probe,
- characterize the 17 adapter gap rows and 7 evidence gap rows in the
  TEI-EAJ expansion,
- continue source-authority representability work before making stronger claims
  about all Aozora markdown constructs.
