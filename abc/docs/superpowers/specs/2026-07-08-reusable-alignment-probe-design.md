# Reusable Alignment Probe Design

Status: Proposed design
Date: 2026-07-08
Owner: ABC / ab-validator comparison tooling

This spec deepens the TEI-EAJ comparison work by applying CollateX-style
separation of tokenization, normalization, alignment, and analysis, while
keeping the implementation local and reusable across ABC and `ab-validator`.

The design is not about integrating CollateX. It uses CollateX as prior art for
alignment workflow shape, then routes implementation through existing Soranoha
comparison machinery. In particular, it treats `ab-validator` as the home for a
shared comparison vocabulary so tokenizer, TEI, parser, and schema-version
diffs do not grow separate dialects.

## Problem

The current TEI-EAJ comparison answers a coarse but useful question: after ABC
and TEI-EAJ body text are normalized by the comparison policy, do the base texts
match, is one a subset of the other, or is the row a mismatch?

That is enough for a corpus gate. It is not enough for diagnosis.

When a mismatch appears, maintainers need to know whether it is:

- a tail addition or omission,
- an internal insertion or deletion,
- a substitution,
- a paragraph or sentence segmentation difference,
- materialized metadata that should remain TEI/XML metadata,
- likely moved or reordered content,
- or an unclassified text mismatch.

The same need exists outside TEI-EAJ:

- comparing parser-IR or AAT outputs across schema versions;
- showing minimal differences when a renderer changes;
- comparing token streams from different tokenizer profiles;
- explaining the smallest meaningful divergence between generated artifacts
  across source, parser, schema, or recipe revisions.

The current repo already has related machinery:

- `ab-morph-diff` aligns tokenizer/morpheme analyses over the same source text
  and classifies one-to-one, split, merge, resegment, and coverage-mismatch
  regions.
- `ab-compare` hashes and compares structural, semantic, visible-text, and
  normalized-visible-text summaries, then records first-difference snippets.
- `ab-diff-utils` owns small shared utilities such as first-difference and
  stable sequence hashing.

Those are valuable, but they do not yet provide a generic token-sequence
alignment kernel for comparing two different rendered text views where there is
no shared source span grid.

The missing piece is not "another TEI diff." It is a reusable comparison layer
that can say, consistently:

- use source-span alignment when both sides share a validated source coordinate
  system;
- use token-sequence alignment when only rendered or normalized strings are
  comparable;
- preserve the evidence level in the report so consumers do not treat weaker
  string alignment as equivalent to same-source span comparison.

## Evidence

| Claim | Type | Source | Confidence | Impact if wrong |
|---|---|---|---|---|
| TEI-EAJ comparison currently reports equality/subset/mismatch plus first difference, but not aligned divergence regions. | Observation | `abc/tools/tei_eaj_compare.py`, `abc/schemas/tei-eaj-comparison.schema.json` | High | This design would duplicate existing output. |
| CollateX separates tokenization, normalization, alignment, analysis, and visualization. | Observation | <https://collatex.net/doc/> | High | The prior-art argument weakens. |
| CollateX uses progressive alignment over variant graphs and includes Dekker, Needleman-Wunsch, and MEDITE algorithms. | Observation | <https://collatex.net/doc/> | High | Algorithm tradeoffs would need revision. |
| CollateX-style full variant graphs are more than the first TEI-EAJ diagnostic slice needs. | Inference | Current comparison is mostly pairwise work rows | Medium | A richer multi-witness design might be warranted earlier. |
| `ab-morph-diff` already models split/merge/resegment/coverage mismatch, but requires analyses over the same source text/span coordinate system. | Observation | `ab-validator/crates/ab-morph-diff/src/{lib.rs,align.rs,nway.rs,model.rs}` | High | We could reuse it directly instead of factoring a generic layer. |
| `ab-compare` already compares visible/normalized-visible text and semantic hashes, but its text diagnosis stops at first difference. | Observation | `ab-validator/crates/ab-compare/src/aat_diff.rs` | High | A reusable alignment probe may be unnecessary. |
| The reusable layer belongs below TEI-EAJ-specific code and below AAT/schema-version comparison adapters. | Design inference | Multiple consumers need the same aligned-region vocabulary | Medium-high | The first implementation could remain TEI-only, but would be harder to reuse later. |

## Non-Goals

- Do not add CollateX as a dependency.
- Do not implement full CollateX variant graphs in the first slice.
- Do not replace the existing TEI-EAJ equality/subset/missing-counterpart gate.
- Do not make alignment diagnosis identity-bearing for ABC artifacts.
- Do not change tokenizer artifact identity or analysis artifact identity.
- Do not rewrite `ab-morph-diff`; its same-source span alignment remains a
  specialized, valid path.
- Do not require every comparison report to emit full aligned tables. Output
  must stay bounded.

## Prior Art

CollateX's useful contribution is workflow shape:

```text
tokenization -> normalization -> alignment -> analysis -> visualization/output
```

It also offers three useful algorithmic reference points:

- **Needleman-Wunsch:** deterministic global alignment by dynamic programming.
  It is strong for ordinary insert/delete/substitution diagnosis but does not
  model moved blocks.
- **Dekker:** phrase-aware progressive alignment with transposition detection.
  This is closer to literary collation needs, but too broad for the first local
  kernel.
- **MEDITE:** maximal unique matches plus optimization, with move handling. The
  "anchor by maximal unique matches first" idea is useful for us; the full
  algorithm is not needed initially.

Soranoha prior art:

- `ab-morph-diff` provides the strongest existing region vocabulary:
  one-to-one, split, merge, resegment, coverage mismatch, compact examples, and
  stats.
- `ab-compare` provides the strongest current report shape for artifact
  comparison: hash summaries, normalized-visible buckets, semantic hashes, and
  bounded examples.
- `ab-diff-utils` is the right home for small shared, dependency-light diff
  primitives.

## Glossary

| Term | Meaning |
|---|---|
| Alignment kernel | A generic library function that aligns two token sequences and emits aligned regions. It knows nothing about TEI, AAT, parser-IR, or morphology. |
| Comparison adapter | Domain-specific code that extracts tokens from a source artifact and interprets aligned regions for that domain. |
| Witness | One side of a comparison. The name follows collation terminology but is only local vocabulary here. |
| Token | A comparison unit with raw text, normalized text, ordinal position, optional source/path context, and optional feature tags. |
| Anchor | A high-confidence matching token or n-gram used to divide a large alignment into smaller windows. |
| Region | A contiguous aligned span: equal, insertion, deletion, substitution, segmentation difference, likely move, or unclassified mismatch. |
| Probe | A bounded, diagnostic output attached to a comparison row. It is informational, not canonical identity. |

## Use Cases

| Actor | Objective | Current obstacle | Capability when solved |
|---|---|---|---|
| TEI maintainer | Understand why ABC and TEI-EAJ text differ | First-difference snippets do not classify the mismatch | See insertion/deletion/substitution/move/metadata diagnoses per row. |
| Parser/schema maintainer | Compare generated outputs across schema or renderer revisions | Hash or first-difference output does not show minimal semantic regions | Attach bounded aligned-region summaries to version-diff reports. |
| Tokenizer evaluator | Explain tokenization differences across profiles | `ab-morph-diff` works only when both tokenizations share source spans | Use same-source span alignment where possible, fall back to sequence alignment for rendered/normalized streams. |
| Release reviewer | Triage regressions after a source/parser/schema update | Full artifact diffs are noisy and unbounded | Review compact examples and counts by divergence class. |
| Tooling developer | Reuse comparison logic without TEI-specific coupling | Current TEI-EAJ comparator is Python and TEI-specific | Call a small generic alignment API from multiple adapters. |

## Design Direction

Add a reusable comparison layer under `ab-validator` and make TEI-EAJ alignment
diagnosis its first cross-rendered-text consumer.

```text
ABC TEI / TEI-EAJ TEI / AAT / parser-IR / token stream
        │
        ▼
domain adapter extracts ComparisonTokens
        │
        ▼
generic alignment kernel
        │
        ▼
domain adapter classifies/interprets regions
        │
        ▼
bounded probe in JSON report / Clojure schema fixture / human report
```

The comparison layer has two related but distinct strategies:

1. **Span-aware comparison:** use existing `ab-morph-diff` machinery when both
   sides are validated over the same source text and source-span coordinate
   system.
2. **Spanless sequence comparison:** use the new alignment kernel when the
   strongest shared coordinate is an ordered token sequence with normalized text.

Both strategies should emit report shapes that share names for counts, bounded
examples, truncation, and region classes where the concepts are actually the
same. They should not pretend to have the same evidentiary strength.

The first implementation should be small:

1. Define the generic model and deterministic pairwise alignment.
2. Add TEI-EAJ `alignment_probe_v1` for mismatched compared rows.
3. Extend the TEI-EAJ comparison schema and Clojure schema tests.
4. Leave AAT/schema-version adapters as follow-ups, but design the model so
   they do not require a rewrite.

## Alternatives

| Criterion | Status quo | TEI-only alignment in Python | Reuse `ab-morph-diff` directly | Recommended generic kernel + adapters |
|---|---|---|---|---|
| Diagnoses TEI-EAJ mismatches | Weak: first difference only. | Strong enough for TEI-EAJ. | Not directly; TEI-EAJ sides do not share source spans. | Strong enough and reusable. |
| Reuses existing thinking | Limited to first-diff utility shape. | Low; likely duplicates region vocabulary. | High where same-source spans exist. | High: reuses concepts, stats, bounded examples, and can later factor utilities. |
| Applies to schema/version diffs | No. | No, unless TEI logic leaks outward. | Only if artifacts can be projected onto common source spans. | Yes, via adapters. |
| Implementation size | None. | Small now, costly later. | Medium now, but forced fit. | Medium; better long-term boundary. |
| Risk | Poor diagnostics persist. | Creates a second comparison dialect. | Complects same-source span alignment with cross-text alignment. | Requires a new model but keeps concerns separate. |

## Component Design

### 1. `ab-diff-align` or `ab-diff-utils::align`

Primary type: **Deepen**.

Purpose: compare two ordered token sequences without knowing their domain.

Inputs:

- left witness id;
- right witness id;
- ordered left tokens;
- ordered right tokens;
- alignment configuration;
- output limits.

Outputs:

- alignment summary;
- bounded region examples;
- optional diagnostic flags for truncation or fallback.

Dependencies:

- no TEI/XML dependency;
- no tokenizer dependency;
- no JSON Schema dependency;
- reuse `ab-diff-utils` hashing/first-diff code when useful;
- reuse naming conventions from `ab-morph-diff` for region counts and compact
  examples when the semantics match;
- do not depend on `ab-morph-diff` internals unless that dependency stays
  acyclic and keeps `ab-morph-diff`'s same-source span model intact.

State / time / identity:

- pure value-in, value-out;
- deterministic order;
- no mutable global state;
- `algorithm_id = "abc-pairwise-token-align-v1"` appears in probe output;
- config is a value and must be serializable for report reproducibility.

Candidate token shape:

```text
ComparisonToken {
  ordinal: usize,
  text: String,
  normalized: String,
  char_span: Option<Range<usize>>,
  path: Option<String>,
  features: BTreeSet<String>
}
```

Candidate region shape:

```text
AlignmentRegion {
  kind:
    equal
    insertion
    deletion
    substitution
    segmentation
    likely_moved_block
    unclassified_mismatch,
  left_range: Range<usize>,
  right_range: Range<usize>,
  left_text_sample: String,
  right_text_sample: String,
  feature_hints: BTreeSet<String>
}
```

The model deliberately does not use `from`/`to` names. Some consumers compare
old/new versions; others compare ABC/reference. The adapter owns the naming.

Evidence level should be explicit:

```text
ComparisonEvidence {
  source_span_aligned,
  token_sequence_aligned,
  hash_only,
  first_difference_only
}
```

This lets release reports and future schema-version diffs put several
comparison mechanisms in one table without implying that every row was proven
with the same strength.

### 2. Alignment Algorithm

The first kernel should combine anchors with bounded dynamic programming.

1. Build normalized-token n-grams.
2. Select unique matching n-grams as anchors.
3. Sort anchors by source order and keep a non-crossing chain.
4. For gaps between anchors, run Needleman-Wunsch-style dynamic programming
   when the window is below configured token/character limits.
5. For oversized windows, emit an `unclassified_mismatch` region with
   truncation metadata rather than doing unbounded work.
6. After ordinary alignment, run a cheap move detector over unmatched regions:
   if one side's deleted normalized token sequence appears as an inserted
   sequence elsewhere, mark both as a `likely_moved_block` pair or add a
   `move_group_id`.

This is intentionally not full MEDITE. The MEDITE-inspired part is using
maximal/unique anchors to make alignment stable and cheap before local DP.

Default scoring:

| Event | Score / effect |
|---|---|
| Exact normalized token match | Strong positive |
| Near match | Disabled by default |
| Gap | Negative |
| Substitution | Negative, but preferred over insertion+deletion for one-token differences |
| Crossing anchors | Rejected; preserve sequence order first |

Near matching may be added later, but must be explicit in the algorithm/config
id because it can change diagnosis.

### 3. TEI-EAJ Adapter

Purpose: produce `alignment_probe_v1` for compared TEI-EAJ rows where current
base-text relation is not `equal`.

Inputs:

- ABC TEI path;
- TEI-EAJ TEI path;
- existing base-text normalization policy;
- output limits.

Outputs:

- same existing row fields;
- optional `alignment_probe`.

Token extraction:

- Prefer structural tokens in this order:
  1. paragraphs and headings;
  2. sentence elements if present;
  3. text runs under body;
  4. character windows only as a fallback for very small unmatched regions.
- Token `text` is rendered body text after the same ruby/note exclusion policy
  used by base-text comparison.
- Token `normalized` uses the same whitespace-removal policy as current
  `body_base_text_no_ws` unless the adapter declares a new normalization id.
- Token `features` records path hints such as `p`, `head`, `note`,
  `source-attribution`, `ruby-base`, or `body-text`.

Interpretation rules:

- tail-only insertion/deletion becomes `tail_addition` or `tail_omission`;
- insertion/deletion adjacent to `note` or source-attribution context becomes
  `metadata_materialized_as_text` when the text appears to come from apparatus
  rather than body prose;
- equal text with different paragraph token boundaries becomes
  `segmentation_only`;
- repeated matching blocks outside expected order become `likely_moved_block`;
- all other non-equal windows remain `substitution` or `unclassified_mismatch`.

The TEI-EAJ adapter is diagnostic. It must not alter `base_text_equal`,
`base_text_relation`, or missing-counterpart status.

### 4. AAT / Schema-Version Adapter

Purpose: compare two generated structured outputs across schema or renderer
versions and show minimal meaningful differences.

This should be a follow-up consumer, not part of the first TEI-EAJ slice.

Inputs:

- two AAT/parser-IR/renderer-output directories or per-work values;
- existing visible/normalized-visible projections from `ab-compare`;
- optional semantic hash buckets.

Outputs:

- current `ab-compare` summary fields;
- optional aligned-region probe for normalized-visible mismatches;
- semantic/context hints from existing block/inline/semantic counts.

This adapter should reuse `ab-compare`'s existing visible text projection and
semantic hash logic rather than create another projection.

For schema-version changes, the adapter should report the smallest stable unit
that a maintainer can act on:

- semantic hash change with equal normalized-visible text;
- normalized-visible text change localized to aligned regions;
- structural path change with equal rendered text;
- materialized-text change caused by renderer/schema policy;
- unclassified change when the adapter cannot localize safely.

This makes the same kernel useful for "what changed between schema versions?"
without making TEI-EAJ the center of the abstraction.

### 5. Morph/Tokenizer Adapter

Purpose: avoid duplicating `ab-morph-diff` while leaving a fallback path for
token streams that do not share source spans.

Rules:

- If both token streams are validated against the same source text and expose
  spans, keep using `ab-morph-diff`.
- If only rendered/normalized token strings are available, use the generic
  alignment kernel and mark the result as `spanless_token_sequence_alignment`.
- Do not collapse these two result types. Same-source span alignment is
  stronger evidence than string-sequence alignment.
- Prefer extracting shared report summaries from `ab-morph-diff` rather than
  translating its full model into the generic alignment model.
- If a future tokenizer comparison needs both span-aware and spanless evidence,
  emit two probes with distinct evidence levels and algorithm ids.

## Probe Output Contract

`alignment_probe_v1` should be optional and bounded.

Candidate JSON shape:

```json
{
  "schema_version": "alignment-probe-v1",
  "algorithm_id": "abc-pairwise-token-align-v1",
  "tokenization_id": "tei-body-structural-text-v1",
  "normalization_id": "tei-eaj-base-text-no-ws-v1",
  "left_witness": "abc",
  "right_witness": "tei_eaj",
  "summary": {
    "equal_regions": 12,
    "insertion_regions": 1,
    "deletion_regions": 0,
    "substitution_regions": 0,
    "segmentation_regions": 3,
    "likely_moved_block_regions": 0,
    "unclassified_mismatch_regions": 0
  },
  "diagnosis_counts": {
    "tail_addition": 1,
    "segmentation_only": 3
  },
  "samples": [
    {
      "kind": "insertion",
      "diagnosis": "tail_addition",
      "left_range": [18, 19],
      "right_range": [18, 18],
      "left_text": "（古伝説と、シルレルの詩から。）",
      "right_text": "",
      "left_path": "/TEI/text/body/p[1]",
      "right_path": null
    }
  ],
  "truncated": false,
  "limits": {
    "max_tokens_per_window": 512,
    "max_samples": 8,
    "max_sample_chars": 160
  }
}
```

The TEI-EAJ comparison schema should embed this as an optional property on
compared file rows. Other consumers may define their own wrapper schemas while
reusing the same probe schema.

## Data Lifecycle

- Alignment probes are generated report data, not canonical artifact identity.
- Probe schema versions are stable and schema-tested.
- Report fixtures should pin one or two minimal examples:
  - tail addition;
  - segmentation-only difference;
  - optional likely move once the move detector exists.
- Full aligned tables should not be committed for corpus runs.
- Consumers that need complete alignments can request a separate local debug
  output, but CI report outputs stay bounded.

## Design Review

### Hickey Simplicity Review

| Check | Finding |
|---|---|
| Problem separate from solution | The problem is mismatch diagnosis and reusable minimal diffs, not CollateX integration. |
| Components about one thing | Kernel aligns token sequences; adapters extract/domain-interpret; schemas validate outputs. |
| State/time/identity explicit | Kernel is pure. Algorithm/config ids are values in output. Probes are not identity-bearing. |
| Protocol boundary explicit | `alignment_probe_v1` is the wire/report contract. Token extraction contracts are versioned by adapter ids. |
| Trust boundary | External TEI-EAJ input remains comparison input, not authority over ABC identity. |
| Existing behavior preserved | Current equality/subset/missing-counterpart fields remain unchanged. |

### Risks

| Risk | Mitigation |
|---|---|
| A generic kernel becomes too abstract for the first need. | Keep first API pairwise and token-sequence-only. No graph API in v1. |
| TEI-specific classification leaks into shared code. | Shared regions use generic kinds; TEI diagnoses live in the TEI-EAJ adapter. |
| Same-source tokenization comparison gets weakened by string alignment. | Keep `ab-morph-diff` as the preferred span-aware path; string alignment is a lower-evidence fallback. |
| A second comparison vocabulary grows beside `ab-morph-diff`. | Share count/example naming where semantics match and record evidence level explicitly. |
| Alignment cost explodes on long works. | Anchor first, DP only inside bounded windows, emit truncated unclassified regions when limits are exceeded. |
| Output becomes too noisy for regular CI reports. | Store counts and bounded samples only. Full debug output is opt-in and local. |

## First Implementation Slice

1. Add a small reusable alignment model and pairwise alignment implementation
   under `ab-validator`, likely as a new `ab-diff-align` crate or an
   `ab-diff-utils::align` module.
   - Prefer `ab-diff-utils::align` if the first slice can stay small and avoid a
     new crate.
   - Split to `ab-diff-align` only when multiple binaries/crates need the model
     directly.
2. Add unit/property tests for:
   - equal sequences;
   - insertion/deletion;
   - substitution;
   - repeated tokens with stable ordering;
   - anchor-split windows;
   - bounded/truncated oversized windows.
3. Add a TEI-EAJ adapter in `abc/tools/tei_eaj_compare.py` or migrate the
   comparator to call a Rust CLI if the crate boundary is ready.
4. Extend `tei-eaj-comparison.schema.json` with optional
   `alignment_probe`.
5. Add Clojure schema tests for valid and invalid probe fixtures.
6. Regenerate the TEI-EAJ handoff JSON/report and show Melos as a tail-addition
   diagnosis rather than only a first-difference snippet.

## Follow-Ups

- Add an `ab-compare` adapter that attaches alignment probes to
  normalized-visible differences across AAT/parser-IR/schema-version outputs.
- Add a release-diff command that compares two schema or renderer versions and
  reports minimal changed regions using the same evidence-level vocabulary.
- Add optional near-match scoring with explicit algorithm/config id rotation.
- Add move-group ids once repeated-block detection is characterized.
- Consider a true multi-witness variant-table/graph only when comparing three
  or more versions of the same work becomes a regular workflow.

## Decision Log

| Decision | Status | Date | Reversibility | Evidence / alternatives rejected | Revisit trigger |
|---|---|---|---|---|---|
| Do not integrate CollateX as a dependency. | Accepted | 2026-07-08 | Reversible | User preference; GPL dependency and runtime integration are unnecessary for the local need. | If a future editorial workflow needs full multi-witness apparatus generation. |
| Add a reusable token-sequence alignment kernel rather than TEI-only Python logic. | Proposed | 2026-07-08 | Reversible before implementation | TEI-EAJ, schema-version diffs, and token stream diffs all need minimal aligned regions. | If implementation proves too broad for the first slice. |
| Keep `ab-morph-diff` as the authoritative same-source span comparison path. | Proposed | 2026-07-08 | Low-risk | It already models spans, segmentation, coverage, stats, and compact examples. | If generic alignment can preserve source-span evidence without loss. |
| Make alignment probes informational, not identity-bearing. | Proposed | 2026-07-08 | Reversible only with schema/ADR work | Probe algorithm changes should not rotate ABC artifact identity. | If probes are later published as canonical analysis artifacts. |
