---
title: "Sustaining Aozora Bunko as Versioned Corpus Infrastructure"
subtitle: "Evidence-driven source mapping, parser comparison, and analytical views"
author: Bor Hodošček
date: July 2026
type: slides
aspect-ratio: 16-9
css: jadh-2026-wip-slides.css
bibliography: references/abstract-refs.bib
csl: references/digital_humanities_abstracts.csl
link-citations: true
---

<!--
Citation provenance:
- abstract-refs.bib sha256:ecac6b1d04171239d5cd1467a37802ff8e9fe8a5820f03ebbdb05cec196ae897
- digital_humanities_abstracts.csl sha256:51537ae9dd3a3a77a971757942c3769c722379aa78d7f2a0bec442e57197c1c9
Budget: 22 min talk + 5 min demo + 3 min buffer.
Canonical Pandoc backend: reveal.js (`--to revealjs --standalone --citeproc`).
-->

# Aozora Bunko Is Shared Research Infrastructure

- Public reading and literary scholarship
- Corpus linguistics, stylistics, and NLP
- Conversational and retrieval-based exploration
- One source collection, many research environments [@aozorabunko2026; @iwata2025]

**Infrastructure question:** can a researcher identify exactly what each derived corpus contains and how it was produced?

<!-- 1:00. Evidence: archived abstract, paragraph 1. -->

# One Source, Many Derived Corpora

```text
Aozora source
  ├─ reading platforms
  ├─ parser libraries
  ├─ TEI editions
  ├─ plaintext corpora
  ├─ tokenized datasets
  └─ LLM / RAG indexes
```

The transformation is often treated as disposable preprocessing.

**Soranoha treats it as a versioned scholarly process.**

# Parallel Work: TEI-EAJ aozora_tei

- Curated human encoding of Aozora works in TEI P5
- Encoding depth organized through Levels 2–5
- Shared workflow for headers, drafts, completed files, and enrichment
- Guidelines and visualization experiments that make TEI useful beyond XML

[Project repository](https://github.com/TEI-EAJ/aozora_tei) ·
[Project wiki](https://github.com/TEI-EAJ/aozora_tei/wiki)
[@teieaj2023; @okada2023]

**Parallel emphasis:** TEI-EAJ develops curated scholarly encodings; Soranoha measures and versions corpus-scale transformations.

<!--
Evidence path: abc/flake.nix; pinned input tei-eaj-aozora-tei.
Inspect/regenerate: nix run .#abc-tei-eaj-aozora-tei-source
Inputs: TEI-EAJ/aozora_tei@77a675fc2771936f9544505d922d4cd45075338c
Expected: store path containing README.md and data/complete/tei_lib_lv4/1567_tei.xml
Class: pinned upstream comparison source
-->

# Research Questions

1. What markup syntax does the authoritative source corpus contain?
2. What does each parser preserve, normalize, or lose?
3. How do publication and tokenizer choices change downstream research views?

**Contribution:** evidence that connects source syntax to versioned outputs.

# Readable Does Not Mean Plain

```text
吾輩《わがはい》
※［＃「口＋世」、U+546D］
青空［＃「青空」に傍点］
［＃ここから2字下げ］
```

- Ruby and gaiji
- Emphasis and headings
- Layout, notes, images, warigaki, and kunten
- Conventions whose meaning depends on documented syntax and context

# What Counts as Evidence?

## Source authority

- Official Aozora documentation
- The versioned source works themselves

## Supporting evidence

- Community notation specifications
- Parser outputs and adapter measurements
- TEI-EAJ comparison works [@teieaj2023]

**A parser cannot define away syntax that exists in the source.**

<!-- Evidence: parser study authority caveat and repository design boundaries. -->

# Building the Source-Authority Inventory

1. Extract markers directly from the pinned corpus snapshot.
2. Reconcile observed forms with official documentation.
3. Group 50 inventory rows into 10 presentation families.
4. Assign reviewed representation and TEI P5 projection targets.
5. Gate unknown or unrepresentable syntax.

<!-- Evidence: ab-validator/docs/superpowers/reports/2026-07-04-source-authority-representability.md -->

# What Is in the Corpus?

## 2026-04-25 source snapshot

- **17,894 works**
- **4.3 million** de-duplicated markup occurrences
- **50** source-inventory rows
- **10** presentation families
- **3,607,926 ruby annotations** dominate the corpus

Per-family counts overlap; rows do not sum to the de-duplicated total.

## Rare Syntax Still Matters

| Construct | Works |
| --- | ---: |
| Multicolumn layout | 26 |
| Tables | 7 |
| Quote blocks | 6 |

- Frequency tells us impact, not semantic importance.
- Rare constructs test whether a pipeline preserves the source's expressive range.
- Long-tail syntax needs explicit evidence, not optimistic fallback.

<!--
Evidence path: ab-validator/docs/superpowers/reports/2026-07-04-source-authority-representability.summary.json
Inspect/regenerate: jq '{works_scanned,markers_total,row_count:(.rows|length),selected:(.rows|with_entries(select(.key=="ruby.basic" or .key=="layout.multicolumn" or .key=="structure.table" or .key=="structure.quote_block")))}' ab-validator/docs/superpowers/reports/2026-07-04-source-authority-representability.summary.json
Inputs: source-authority extraction over the 17,894-work snapshot
Expected: markers_total 4323915; 50 rows; ruby 3607926 occurrences; multicolumn 26 works; tables 7; quote blocks 6
Class: checked-in generated source-authority summary
-->

# What Makes a Corpus Version Reproducible?

![Soranoha Reproducibility Architecture](../../abc/docs/figures/soranoha-reproducibility-architecture.svg){width=100%}

<!--
FINAL GATE: generated by abc/docs/superpowers/plans/2026-07-10-academic-presentation-diagrams.md.
Focus: identity binds sources, evidence, contracts, profiles, and recipes.
ArtifactID is not a materialized-file byte hash.
-->

# Five Parsers, Three Questions

- `aozora-pipeline`: measured reference candidate
- `aozora2html`: official XHTML converter [@aozorahack2026]
- `aozora2`: Rust format converter [@takahashi2026]
- `aozora-rs`: Rust parser [@kinokov2026]
- `AozoraEpub3`: Java/EPUB converter [@aozoraepub32026]

No parser is assumed correct because of its name or output format.

<!--
Crosswalk: aozora-pipeline = aozora / ab-aozora; aozora2 = aozora-core;
aozora-rs = aozora-rs-core when measured natively; AozoraEpub3 = aozora-epub3.
aozora-pipeline is P4suta/aozora, a measured candidate—not completed Soranoha consolidation.
-->

| Measurement | Question |
| --- | --- |
| Conformance breadth | Which curated constructs are recognized? |
| Frequency-weighted coverage | How much real-corpus markup mass is represented? |
| Fidelity / robustness | Given completion, how faithful is output—and does the parser complete? |

**Speed is a fourth engineering constraint, not a correctness score.**

# Conformance Breadth

- 127 third-party vectors across 24 feature families
- No parser passes all 25 `must` vectors
- `aozora-pipeline`: **22/25 must**; the three failures are diagnostic-only
- Raw adapter scores can confuse parser gaps with adapter serialization gaps
- Independent official-document seeds corroborate the family-level findings

**A conformance ranking is not a capability ranking.**

<!--
Evidence path: ab-validator/docs/superpowers/reports/2026-07-08-aozora-parser-comparison-study.md
Inspect/regenerate: sed -n '85,175p' ab-validator/docs/superpowers/reports/2026-07-08-aozora-parser-comparison-study.md
Inputs: 127 P4suta vectors; independent official-doc seed
Expected: 24 families; 25 must vectors; aozora-pipeline 22/25; no parser passes all must vectors
Class: checked-in comparison report; supporting evidence, not source authority
-->

# Frequency-Weighted Corpus Coverage

| Parser lane | Coverage |
| --- | ---: |
| `aozora-pipeline` | **0.969** |
| `aozora-rs` | 0.938 |
| `AozoraEpub3` | 0.926 |
| `aozora2` | 0.855 |
| `aozora2html` | 0.743 |

Ruby is about 90% of **coverage-weighted mass**: read the total as **ruby coverage adjusted by the long tail**.

<!--
Evidence path: ab-validator/docs/superpowers/reports/2026-07-08-aozora-parser-comparison-study.md
Inspect/regenerate: sed -n '219,345p' ab-validator/docs/superpowers/reports/2026-07-08-aozora-parser-comparison-study.md
Inputs: checked-in July 2026 parser study; pinned-denominator reconciliation report
Expected: coverage 0.969/0.938/0.926/0.855/0.743; robustness interpretation over 17,886 works
Class: checked-in report
-->

# Fidelity and Robustness Are Different

- `aozora2html`: highest per-work fidelity (**0.974**) on the common intersection
- It fails on **302 ruby-heavy works**, missing **24.7% of ruby mass**
- Its 0.983 work completion rate hides a mass-weighted robustness failure
- `aozora-pipeline` and `aozora-rs` complete all 17,886 pinned works—but `aozora-rs` has the lowest isolated fidelity
- In **ア、秋**: `aozora2` matches the one-paragraph reference; `aozora2html` produces **29 fragments**

**Per-work quality cannot compensate for systematically missing difficult works.**

<!--
Evidence path: ab-validator/docs/superpowers/reports/2026-07-08-fidelity-robustness-split.md
Inspect/regenerate: sed -n '140,175p' ab-validator/docs/superpowers/reports/2026-07-08-fidelity-robustness-split.md
Inputs: common completed-work intersection; pinned 17,886-work robustness denominator
Expected: aozora2html fidelity 0.974; 302 ruby-heavy failures; 24.7% ruby mass missed; work-count completion 0.983
Class: checked-in report
-->

# Parser Comparison: What We Can Conclude

- Highest frequency-weighted coverage: `aozora-pipeline`
- Highest isolated fidelity: `aozora2html`
- Fastest measured lane: `aozora-rs`
- Complementary strengths remain across feature families
- Engineering recommendation: use `aozora-pipeline` as the future parser base

**This is a multi-criterion recommendation—not one overall scholarly score.**

<!-- Consolidation is not complete. The recommended candidate still drops jizume and yokogumi. -->

# From Authoritative Source to Scholarly Views

![Soranoha Publication Pipeline](../../abc/docs/figures/soranoha-publication-pipeline.svg){width=100%}

<!--
FINAL GATE: generated by abc/docs/superpowers/plans/2026-07-10-academic-presentation-diagrams.md.
Stable contract: source -> validated parser process -> Parser-IR -> manifest -> scholarly outputs.
AAT is current producer detail.
-->

# Two Representation Boundaries

## AAT — descriptive waist

- Parser-shaped evidence
- Preserves raw markup and adapter provenance
- Makes heterogeneous parser outputs comparable

## Parser-IR — decisional waist

- Publication lanes and normalized semantics
- Typed loss taxonomy and ambiguity ledger
- One policy boundary for multiple scholarly outputs

# Worked Mapping: 傍点

| Stage | Representation | Decision |
| --- | --- | --- |
| Source | `青空［＃「青空」に傍点］` | authoritative marker |
| AAT | `style_type: boten` | preserve descriptive identity |
| Parser-IR | `emphasis` | normalize under rule `A-06` |
| TEI | `<hi>` plus preservation evidence | publish while retaining recoverability |

**Normalization does not have to mean silent loss.**

<!--
Evidence path: ab-validator/data/aat-to-parser-ir-mapping-v1.json
Inspect/regenerate: jq '.transform_rule_descriptions[] | select(.rule_id == "A-06"), .loss_taxonomy.AMBIGUITY' ab-validator/data/aat-to-parser-ir-mapping-v1.json
Inputs: mapping version 0.2.1
Expected: A-06 style -> emphasis; 130 observed style mappings; AMBIGUITY records_sidecar true
Class: checked-in generated mapping contract
-->

## Transformation Records Make Decisions Inspectable

| Category | Meaning |
| --- | --- |
| `LOSS` | AAT information has no Parser-IR field |
| `INVENTION` | Parser-IR requires a value AAT does not supply |
| `AMBIGUITY` | Related concepts differ in semantics or range |
| `UNSUPPORTED` | No Parser-IR representation exists |
| `STRUCTURAL` | Tree shape forces boundary or span loss |

Each category has an explicit action and sidecar policy.

<!-- Evidence: ab-validator/data/aat-to-parser-ir-mapping-v1.json loss_taxonomy. -->

# Actual TEI XML: Curated TEI-EAJ

## 走れメロス — Level 4

::: {.xml-example}
```xml
<p>
  <persName corresp="#メロス">メロス</persName>は激怒した。
  必ず、かの<persName corresp="#ディオニス">
    <ruby>
      <rb>邪智暴虐</rb>
      <rt>じゃちぼうぎゃく</rt>
    </ruby> の王
  </persName>を除かなければならぬと決意した。
  <persName corresp="#メロス">メロス</persName>には政治がわからぬ。
  <persName corresp="#メロス">メロス</persName>は、
  <roleName>村の牧人</roleName>である。
</p>
```
:::

[Pinned TEI-EAJ project source](https://github.com/TEI-EAJ/aozora_tei) [@teieaj2023; @okada2023]

<!--
Evidence path: pinned input data/complete/tei_lib_lv4/1567_tei.xml
Inspect/regenerate: tei_root="$(nix run .#abc-tei-eaj-aozora-tei-source)"; sed -n '57,67p' "$tei_root/data/complete/tei_lib_lv4/1567_tei.xml"
Inputs: TEI-EAJ/aozora_tei@77a675fc2771936f9544505d922d4cd45075338c
Expected: SHA-256 c2f6e43fbc3235e832feccf47e5b06c896959d8aa2a9798132e8dca12df5c7be; work 1567; curated Level 4
Class: pinned upstream curated TEI
-->

# Work-Matched XML Comparison: Readiness Gate

## What the paired excerpts will test

- Paragraph boundaries: curated structure versus parser-derived structure
- Ruby: shared TEI vocabulary and source-preserving detail
- Notes: body, back matter, and source-apparatus routing
- Headers: human enrichment versus generated provenance
- Named entities and speech: curated Level 4 depth versus transcription baseline

**Draft gate:** the real Soranoha 走れメロス publication bundle has not yet landed as tracked, reproducible evidence.

<!--
DRAFT XML GATE
Evidence path: target abc/paper/demo-melos-real/{parser-ir.aozora2html.json,metadata-record.json,source.manifest.json,tei.xml,tei.manifest.json,tei-validation-result.json}
Inspect/regenerate: rg --files abc | rg 'demo-melos-real/(parser-ir|metadata-record|source\.manifest|tei\.xml|tei\.manifest|tei-validation-result)'
Inputs: work ID 1567; tracked Parser-IR, metadata/persons, source manifest; generated-at 2026-07-04T00:00:00Z
Expected: work-matched TEI plus manifest identity and zero validation findings
Class: readiness gate; no Soranoha fixture is shown as a real edition
-->

# Publication Is Plural

- **TEI:** structured scholarly transcription and validation [@okada2023]
- **Visible plaintext:** body text only
- **Custom sidecars:** source apparatus, provenance, and policy-specific detail
- **Tokenized artifacts:** identified analytical views
- **Manifest:** binds identity-bearing inputs to each derived artifact

Parser-IR is a publication boundary, not a claim that every output should contain the same information.

<!-- Evidence: parser-ir-publication-policy-v0.json and publication-bundle reports. ArtifactID is not content hash. -->

# Tokenization Is a Scholarly Choice

- Segmentation changes the units available for counting and comparison.
- Lemmas and parts of speech depend on tokenizer and dictionary identity.
- Literary and historical forms expose differences hidden by contemporary prose.
- Soranoha publishes tokenizer-specific artifacts as parallel analytical views [@kanda2025; @worksapplications2026].

**There is no single authoritative tokenized corpus.**

# Two Regions, Different Analyses

| Region | Sudachi A/B | Sudachi C | Vibrato + UniDic CWJ |
| --- | --- | --- | --- |
| 求むる | 求むる（動詞） | 求むる（動詞） | 求｜む｜る（名詞／名詞／助動詞） |
| 竹馬の友 | 竹馬｜の｜友 | 竹馬の友（固有名詞） | 竹馬｜の｜友 |

Dictionary choice can change POS even when token boundaries look similar [@den2008; @daactools2026].

<!-- Evidence: tokenizer-comparison-view.md regions 15 and 208. Profiles: sudachi-20260116 and unidic-cwj-202512. -->

# Live Demo: Follow the Evidence

1. Inspect source constructs and authority-backed mappings.
2. Compare selected parser lanes.
3. Inspect AAT → Parser-IR transformation records.
4. Materialize TEI and visible plaintext.
5. Compare tokenization over one selected region.

<!--
DEMO BUDGET: 5:00; never run corpus-scale work live.

Preflight tracked demo artifacts:
  rg --files abc | rg 'demo-(rashomon|melos)-real/(parser-ir|divergence|tei|plain)'

Update this comment with actual tracked monorepo paths after the demo artifacts land.
Never use machine-local database paths or an untracked references/ directory.
Each live read-only jq/rg/display command must complete in under five seconds.

Fallback: narrate slides 19, 20, 21, and 23; do not improvise a corpus-scale run.
Provenance narrative: sibling archive paper/demo-trace.md; it is not a build input.
-->

# Current Limits and Next Work

- Long-tail gaps remain, including 字詰め and 横組 in the recommended candidate.
- Parser consolidation is recommended, not complete.
- TEI Levels 4–5 require editorial and enrichment work.
- Tokenizer suitability needs work-level and genre-aware evaluation.
- Every corpus, parser, dictionary, or profile change requires remeasurement.

**Work in progress means preserving the boundary between measured results and planned work.**

# Versioned, Inspectable, Comparable, Reproducible

- Inventory source syntax before transforming it.
- Measure parsers against authority-backed evidence.
- Record normalization and loss at representation boundaries.
- Publish parser- and tokenizer-specific scholarly views.
- Bind every view to explicit identities, contracts, and recipes.

**Corpus transformation is part of the scholarly method.**

[Soranoha repository](https://github.com/borh/soranoha)

# Appendix: Source Markup Inventory

| Family | Occurrences | Example |
| --- | ---: | --- |
| Ruby | 3,607,926 | `吾輩《わがはい》` |
| Emphasis | 157,494 | `［＃太字］` |
| Indentation | 266,540 | `［＃ここから2字下げ］` |
| Gaiji | 121,535 | `※［＃「口＋世」、U+546D］` |
| Headings | 94,111 | `［＃大見出し］` |
| Decorations | 208,333 | `［＃「語」に傍点］` |
| Kunten | 34,549 | `［＃レ］` |
| Layout | 24,442 | `［＃ページの左右中央］` |
| Annotations | 35,517 | `「text」の注記付き` |
| Warigaki | 6,607 | `［＃割り注］` |

<!-- Counts overlap by family; source: abstract Table 1. -->

# Appendix: Reading the Parser Numbers

- **Breadth:** curated constructs, equal weight per vector
- **Mass:** real-corpus occurrences, ruby-dominated
- **Fidelity:** representation quality on a common completed-work intersection
- **Robustness:** completion over the pinned corpus
- **Speed:** operational feasibility

No single column answers every research question.
