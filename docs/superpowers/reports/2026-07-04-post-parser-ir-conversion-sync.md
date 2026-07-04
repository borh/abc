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

Generated TEI is still smoke-tested against a single TEI-EAJ comparison row
with:

```sh
just parser-ir-level3-melos-eaj-compare-smoke
```

The same comparison runs over the TEI-EAJ workset in selected-candidate mode
with:

```sh
just parser-ir-level3-tei-eaj-generated-audit
```

Committed measurement:

- report:
  `docs/superpowers/reports/2026-07-04-tei-eaj-generated-comparison.md`
- summary:
  `docs/superpowers/reports/2026-07-04-tei-eaj-generated-comparison.summary.json`
- generated per-row TEI artifacts:
  `/db/ab-validator/parser-ir/tei-eaj-generated-comparison/`

Current result: 57 selected rows materialized through parser-IR and ABC TEI, 0
materialization failures, 5 rows skipped for missing/nonmaterializable evidence.
Paragraph deltas are 5 exact, 45 over-split, and 7 under-split. Strict
normalized body-base-text relation, after skipping notes/ruby `rp`/`rt`, is 3
equal, 1 generated-contains-TEI-EAJ, and 53 different.

The generated-TEI audit now also classifies alternate text surfaces. Best
surface-match buckets are 3 base-equal, 1 ruby-expanded-equal, 19
ruby-expanded-parenless-equal, 1 base-drop-parentheticals-equal, 5
ruby-expanded-parenless generated-contains-TEI-EAJ, 2 ruby-expanded-parenless
TEI-EAJ-contains-generated, and 26 still different. This makes the next Level 3
blocker a measured paragraph segmentation plus narrower TEI-EAJ text-policy and
content-alignment problem, not a parser-IR schema-validity or
source-note-placement problem.

The all-parser matrix comparison now runs over every materializable parser input
for each TEI-EAJ row with:

```sh
just parser-ir-level3-tei-eaj-generated-matrix-audit
```

Committed matrix measurement:

- report:
  `docs/superpowers/reports/2026-07-04-tei-eaj-generated-matrix-comparison.md`
- summary:
  `docs/superpowers/reports/2026-07-04-tei-eaj-generated-matrix-comparison.summary.json`
- generated per-row TEI artifacts:
  `/db/ab-validator/parser-ir/tei-eaj-generated-matrix-comparison/`

Current matrix result: 57 TEI-EAJ rows expanded to 173 parser-input rows, 173
materialized through parser-IR and ABC TEI, 0 materialization failures, and 5
rows skipped for missing/nonmaterializable evidence. Candidate coverage is 57
rows each for `aozora2html`, `aozora-epub3`, and `aozora-rs`, plus 2 rows for
`aozora2`.

Paragraph deltas by adapter:

| adapter | exact | over-split | under-split | collapsed |
|---|---:|---:|---:|---:|
| aozora2html | 5 | 45 | 7 | 0 |
| aozora-epub3 | 7 | 48 | 2 | 0 |
| aozora-rs | 6 | 35 | 0 | 16 |
| aozora2 | 0 | 0 | 0 | 2 |

Paragraph-origin classification across the matrix:

| origin | rows |
|---|---:|
| adapter over-segmented | 123 |
| adapter collapsed | 18 |
| adapter under-segmented | 7 |
| source-note back routing | 4 |
| page-break projection | 3 |
| aligned | 18 |

Paragraph origin by adapter:

| adapter | adapter over | adapter under | adapter collapsed | source-note back | page-break projection | aligned |
|---|---:|---:|---:|---:|---:|---:|
| aozora2html | 42 | 5 | 0 | 2 | 3 | 5 |
| aozora-epub3 | 46 | 2 | 0 | 2 | 0 | 7 |
| aozora-rs | 35 | 0 | 16 | 0 | 0 | 6 |
| aozora2 | 0 | 0 | 2 | 0 | 0 | 0 |

Best text-surface residuals by adapter:

| adapter | still different | strict base equal | ruby/parenthetical-policy explainable |
|---|---:|---:|---:|
| aozora2html | 26 | 3 | 28 |
| aozora-epub3 | 50 | 1 | 6 |
| aozora-rs | 28 | 2 | 27 |
| aozora2 | 0 | 0 | 2 |

TEI-EAJ structural profiles across the matrix:

| profile | rows |
|---|---:|
| plain prose | 81 |
| drama | 30 |
| Level 4 enrichment | 29 |
| notes | 21 |
| front/back matter | 6 |
| verse | 6 |

Interpretation: paragraph segmentation is primarily an adapter/source-structure
fidelity problem, not a parser-IR schema gap. AAT paragraph-block counts already
match parser-IR paragraph counts in the dominant path; 148/173 matrix rows have
their TEI-EAJ paragraph mismatch before IR. The prior 7-row renderer mismatch
bucket now resolves into 4 expected source-note back-matter routes and 3
aozora2html source-derived page-break projections; no generic ABC TEI paragraph
renderer loss remains in the matrix. Text alignment also differs by parser:
`aozora2html` and `aozora-rs` often reduce to ruby/parenthetical policy, while
`aozora-epub3` has the largest true residual body-text mismatch bucket. The
next Level 3 admission gate should be profile-aware: plain prose can be judged
on paragraph/source-note/text policy, while drama, verse, notes, front/back
matter, and Level 4 enrichment need separate policy decisions instead of raw
`<p>` count parity.

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
- Just Level 3 TEI-EAJ selected-candidate audit:
  `just parser-ir-level3-tei-eaj-generated-audit`
- Just Level 3 TEI-EAJ all-parser matrix audit:
  `just parser-ir-level3-tei-eaj-generated-matrix-audit`

The crate-specific usage notes live in `crates/ab-aat-to-parser-ir/README.md`.

## Next Work

Proceed with residual adapter/evidence characterization:

- carry the local ABC `0.2.2` compatibility admission (`59731aa`) wherever the
  ABC registry is synced,
- keep the whole TEI-EAJ/aozora_tei structural expansion, selected-candidate
  generated-TEI comparison, and all-parser generated-TEI matrix as the Level 3
  artifacts instead of the single-row probe,
- use `docs/superpowers/reports/2026-07-04-tei-eaj-structural-gap-analysis.md`
  as the current gap classification,
- fix adapter paragraph segmentation first, because 148/173 matrix rows already
  have the paragraph mismatch at AAT paragraph-block evidence before parser-IR,
- keep page-break projection classified separately from body paragraph
  segmentation, currently 3 matrix rows and 9 page-break nodes,
- use the generated-TEI matrix text buckets to prioritize source-text fidelity
  and TEI-EAJ text-policy alignment,
- continue source-authority representability work before making stronger claims
  about all Aozora markdown constructs.
