# Post Parser-IR Conversion Sync

Date: 2026-07-04

## Verdict

`PARSER_IR_CONVERTER_CORPUS_CLEAN`

The `crates/ab-aat-to-parser-ir` converter has consumed the July 3
`CLI_READY_WITH_LOWER_BOUND_CAVEAT` gate, the July 4 `aozora-epub3` adapter
measurement, and the Level 3 parser-IR schema rotation. It now converts all
four measured local AAT inputs into ABC parser-IR with zero conversion failures,
full measured rule coverage, and release-smoke coverage through Cargo and the
shell smoke. Three inputs are full-corpus adapter runs; `aozora2` is currently a
Melos-scoped adapter input.

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
- Conversion audit:
  `docs/superpowers/reports/2026-07-04-aat-parser-ir-full-corpus-conversion.md`
- Summary JSON:
  `docs/superpowers/reports/2026-07-04-aat-parser-ir-full-corpus-conversion.summary.json`

| Metric | Value |
|---|---:|
| files attempted | 53,428 |
| files succeeded | 53,428 |
| files failed | 0 |
| parser-IR nodes | 26,907,268 |
| divergence records | 674,111 |
| divergence occurrences | 42,482,734 |
| mapping rules total | 127 |
| mapping rules emitted | 127 |
| mapping rules missing | 0 |

| Corpus | Files | Result |
|---|---:|---|
| aozora-rs-adapter | 17,894 | 17,894 succeeded, 0 failed |
| aozora2-adapter | 1 | 1 succeeded, 0 failed |
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

The audit generated four ABC registry candidates in
`docs/superpowers/reports/2026-07-04-aat-parser-ir-compatibility-candidates.edn`.
ABC admitted the earlier `0.2.0` two-adapter entries in commit `4cb15df`; the
four `0.2.2` candidates below are admitted in local ABC commit `59731aa`:

| adapter | adapter_version | files_succeeded | rules_emitted | rules_missing | unsupported_occurrences |
|---|---|---:|---:|---:|---:|
| aozora-epub3 | aozora-epub3-adapter 0.1.0 AozoraEpub3-JDK21-1.3.4-jdk21 | 17,844 | 61 | 66 | 13,234 |
| aozora-rs | aozora-rs-adapter 0.1.0 2b4e8d1 | 17,894 | 25 | 102 | 0 |
| aozora2 | aozora2-adapter 0.1.0 aozora-core-0.7.1 | 1 | 9 | 118 | 0 |
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
| rows with AAT evidence | 57 |
| parser-IR gap rows | 0 |
| adapter gap rows | 18 |
| evidence gap rows | 5 |

Interpretation: Level 3 parser-IR paragraph/source-note representation is no
longer the row-level blocker for the TEI-EAJ workset. Remaining structural work
is adapter/evidence coverage: materialize missing ABC counterparts and improve
adapter paragraph/source-note preservation where TEI-EAJ exposes richer
structure than a specific adapter AAT.

Generated TEI is now smoke-tested against a TEI-EAJ comparison row with:

```sh
just parser-ir-level3-melos-eaj-compare-smoke
```

The current Melos `aozora2html` path materializes ABC TEI that validates and
routes the final source attribution to `<back><note type="source-attribution">`.
The remaining measured gap is paragraph segmentation fidelity: generated TEI
has 74 body paragraphs for `data/complete/tei_lib_lv4/1567_tei.xml`, while
TEI-EAJ has 17 body paragraphs and 19 total `p` elements in the workset count.

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
- Just Level 3 TEI-EAJ comparison target:
  `just parser-ir-level3-melos-eaj-compare-smoke`

The crate-specific usage notes live in `crates/ab-aat-to-parser-ir/README.md`.

## Next Work

Proceed with residual adapter/evidence characterization:

- carry the local ABC `0.2.2` compatibility admission (`59731aa`) wherever the
  ABC registry is synced,
- keep the whole TEI-EAJ/aozora_tei structural expansion as the Level 3
  comparison artifact instead of the Melos-only probe,
- use `docs/superpowers/reports/2026-07-04-tei-eaj-structural-gap-analysis.md`
  as the current gap classification,
- add paragraph-count delta buckets to the TEI-EAJ expansion so non-collapsed
  adapter disagreement remains visible,
- continue source-authority representability work before making stronger claims
  about all Aozora markdown constructs.
