# Parser-IR Sentence Segmentation and Orthographic TEI Propagation Design

**Date:** 2026-07-07
**Status:** follow-up design
**Supersedes:** Decision D3 in
`2026-07-07-ortho-sentence-annotation-design.md`. That earlier design said
ABC owns TEI sentence splitting. This follow-up corrects the boundary:
ab-validator/parser-IR owns sentence segmentation evidence; ABC owns TEI
rendering.
**Governs:** `ab-aat-to-parser-ir`, ABC parser-IR schema, ABC TEI renderer,
orthographic annotation propagation, and ruby/tokenization interaction rules.

## Problem

Commit `0fd0aa6` added the ab-validator producer-side wrapper for
`orthographic_annotations`, but it did not make the information appear in TEI.
It also left an awkward boundary: ABC would have to split text during TEI
generation and independently overlap sentences with orthographic annotation
ranges.

That is the wrong direction. Sentence splitting is source/text analysis. TEI
generation should render already-decided structure, not infer segmentation.
Otherwise morph evidence, parser-IR spans, and publication output can drift.

## Goal

Parser-IR carries sentence segmentation as first-class evidence, produced by
ab-validator during AAT -> parser-IR conversion. Orthographic annotations remain
the detector evidence, but ab-validator also joins them to sentence rows so ABC
can render `<s type="orthographic-katakana">` without re-running splitting or
range-overlap logic.

## Design

### Ownership

| Concern | Owner |
|---|---|
| Japanese sentence segmentation over visible source text | ab-validator parsing/conversion |
| Sentence IDs, byte spans, paragraph membership, node ranges | parser-IR contract |
| Orthographic detector output | ab-validator `ab-ortho-detect` |
| Joining orthographic annotations to sentence rows | ab-validator `ab-aat-to-parser-ir` |
| TEI `<s>` emission and `@type` mapping | ABC TEI renderer |
| TEI header/profile declaration for orthographic markup | ABC |
| Ruby readings as reading evidence | ruby oracle / future reading-evidence contract, not sentence segmentation |

### Parser-IR contract

Add two optional top-level parser-IR fields. They become required for
publication-grade parser-IR after the migration window.

```json
{
  "sentence_segmentation": {
    "schema_version": "sentence-segmentation-v1",
    "splitter_id": "ab-plaintext-japanese-v1",
    "coordinate_system": "decoded_utf8",
    "coverage": "body-paragraphs"
  },
  "sentences": [
    {
      "id": "s000000",
      "paragraph_id": "p000000",
      "span": {
        "start": 0,
        "end": 24,
        "coordinate_system": "decoded_utf8"
      },
      "node_range": {
        "start": 0,
        "end": 3
      },
      "tags": ["orthographic-katakana"],
      "orthographic_annotation_indices": [0]
    }
  ]
}
```

Field rules:

| Field | Rule |
|---|---|
| `sentence_segmentation.schema_version` | Literal `"sentence-segmentation-v1"` for this contract. |
| `sentence_segmentation.splitter_id` | Stable identifier for the algorithm and behavior. First value: `"ab-plaintext-japanese-v1"`. |
| `sentence_segmentation.coordinate_system` | Literal `"decoded_utf8"`; same byte coordinate system as node and paragraph spans. |
| `sentence_segmentation.coverage` | Literal `"body-paragraphs"`; source notes/front/back apparatus are not sentence-wrapped in v1. |
| `sentences[].id` | Stable sequential ID in body-text order: `s000000`, `s000001`, ... |
| `sentences[].paragraph_id` | Must match a body paragraph row in `paragraphs[]`. |
| `sentences[].span` | Half-open byte range in decoded visible source text. Must be contained by the paragraph span. |
| `sentences[].node_range` | Contiguous node range, aligned to parser-IR node boundaries. ABC must not slice strings. |
| `sentences[].tags` | Closed set for v1: currently only `"orthographic-katakana"`. Empty array when untagged. |
| `sentences[].orthographic_annotation_indices` | Indices into `orthographic_annotations.annotations` that overlap this sentence. Empty when no overlap. |

`orthographic_annotations` from the prior commit remains the detector evidence
bundle:

```json
{
  "orthographic_annotations": {
    "work_id": "000000",
    "work_content_hash": "sha256:1111111111111111111111111111111111111111111111111111111111111111",
    "coordinate_system": "decoded_utf8",
    "detector_id": "HeuristicV1",
    "annotations": [
      {
        "source_byte_range": { "start": 0, "end": 24 },
        "normalized_text": "吾輩は猫である。",
        "kind": "ScriptKatakanaToHiragana",
        "confidence": null
      }
    ]
  }
}
```

The sentence row is the renderer-facing projection. The annotation bundle is
the provenance. If a sentence partially overlaps one or more
`OrthoAnnotation.source_byte_range` values, the converter adds
`"orthographic-katakana"` to `tags` and records the matching annotation indices.
`detector_id` keeps the existing `OrthoDetectorId` JSON shape: the heuristic
detector is `"HeuristicV1"` and ML detectors use an object with their model
hash.

### Node-boundary invariant

ABC should not split text during TEI rendering. Therefore every sentence
`node_range` must align to parser-IR node boundaries.

The converter is responsible for splitting splittable parser-IR nodes at
sentence boundaries before emitting `sentences[]`.

Splittable in v1:

- `text`
- `quote` when represented as inline text
- `emphasis` and `layout-span` only when they have no `inline_children`

Atomic in v1:

- `ruby`
- `gaiji`
- `editor-note`
- `line-break`
- `page-break`
- `image`
- `caption`
- `source-note`
- any node with structured `inline_children`

If a sentence boundary falls inside an atomic node, conversion must fail in
sentence-enabled mode with a diagnostic naming the node type and byte boundary.
No silent boundary snapping. This keeps parser-IR honest and avoids TEI that
appears precise but changed the tokenizer/sentence decision.

### ABC TEI rendering

ABC consumes `sentences[]` when present:

1. Render paragraphs from `paragraphs[]`.
2. For each body paragraph, group sentence rows by `paragraph_id`.
3. Render each sentence row as `<s>`.
4. Render all nodes in `sentence.node_range` inside that `<s>`.
5. If `sentence.tags` contains `"orthographic-katakana"`, set
   `type="orthographic-katakana"`.
6. If a body paragraph has no `sentences[]` rows, fall back to the current
   paragraph renderer and record an omitted/preservation note.

Example:

```xml
<p>
  <s type="orthographic-katakana">吾輩ハ猫デアル。</s>
  <s><ruby type="furigana"><rb>名前</rb><rt>なまえ</rt></ruby>はまだ無い。</s>
</p>
```

ABC must not call `abc.text/split-japanese-sentence` for parser-IR TEI output
once `sentences[]` is present. That splitter may remain for legacy annotation
formats or non-parser-IR workflows.

### TEI header/profile

ABC adds an encoding declaration:

```xml
<encodingDesc>
  <editorialDecl>
    <normalization method="markup">
      <p>Sentences annotated with <gi>s</gi>
      <att>type</att>="orthographic-katakana" indicate text where the
      ab-validator ortho-detect layer identified katakana-dominant prose for
      tokenizer-facing katakana-to-hiragana normalization. The original text is
      not replaced.</p>
    </normalization>
  </editorialDecl>
</encodingDesc>
```

The TEI profile must allow `<s>` inside body paragraphs and allow
`@type="orthographic-katakana"` on `<s>`.

### Ruby and tokenization

Ruby readings are not source text and must not drive sentence splitting.

Rules:

- Sentence splitting uses visible base text only. For ruby nodes, that means
  `ruby.base`, not `ruby.reading`.
- Tokenizer input uses visible text, optionally transformed by orthographic
  normalization. Ruby reading is not substituted before tokenization.
- Ruby readings may be used as high-authority reading evidence after tokens are
  aligned to source spans.
- A ruby reading may override or annotate analyzer reading only when analyzer
  token spans exactly tile the ruby base span.
- If token spans do not tile the ruby base, record alignment evidence
  (`boundary-misalign`, `no-reading`, etc.); do not silently change
  tokenization.

This keeps three identities separate:

| Value | Identity |
|---|---|
| Source/base text | What TEI and parser-IR preserve. |
| Ortho-normalized text | Analyzer input view for better tokenization. |
| Ruby reading | Editorial/authorial reading evidence, not replacement source. |

### Migration

The parser-IR schema changes from `0.5.0` to `0.6.0`.

Required migration steps:

1. ABC schema accepts `sentence_segmentation`, `sentences`, and
   `orthographic_annotations`.
2. ABC validation adds cross-field coherence checks for sentence rows, because
   JSON Schema cannot verify paragraph/node-range relationships.
3. ab-validator mirrors the updated ABC schema.
4. ab-validator updates the AAT -> parser-IR mapping artifact target schema
   hash and mapping version.
5. ab-validator emits `sentences[]` for body paragraphs.
6. ab-validator joins `orthographic_annotations` to `sentences[].tags`.
7. ABC TEI renderer consumes `sentences[]` and emits `<s>`.

## Decisions

| # | Decision | Rationale |
|---|---|---|
| D1 | ab-validator owns sentence segmentation evidence | Splitting is parsing/text analysis, not TEI rendering. |
| D2 | ABC owns `<s>` rendering only | Keeps publication output deterministic and schema-driven. |
| D3 | Add parser-IR `sentence_segmentation` and `sentences` | Makes sentence spans a versioned contract rather than an implicit renderer behavior. |
| D4 | `sentences[].node_range` must align to node boundaries | Prevents ABC from slicing text/ruby/emphasis during rendering. |
| D5 | Converter fails on sentence boundary inside atomic nodes | Silent snapping would falsify segmentation evidence. |
| D6 | `sentences[].tags` carries `orthographic-katakana` | ABC should not reimplement orthographic overlap logic. |
| D7 | Keep `orthographic_annotations` as provenance | Sentence tags are derived; annotation bundle remains detector evidence. |
| D8 | Ruby readings do not affect splitting or tokenizer input | Ruby is reading evidence, not source text. |

## Open Questions

1. **Production ortho sidecar path:** The prior commit added
   `--ortho-annotations`, but the publication pipeline still needs a stable
   way to produce and pass that sidecar. This design assumes ab-validator
   generates the sidecar from the same decoded visible text used for AAT
   conversion.
2. **Atomic-boundary frequency:** We should measure how often sentence
   boundaries fall inside atomic nodes before making the failure mode less
   strict. If it occurs in real corpus data, design a lossless split for the
   affected node type instead of snapping.
3. **Legacy fallback window:** ABC may temporarily support parser-IR without
   `sentences[]`; publication-grade imports should eventually require it.

## Non-Goals

- Ruby-driven tokenization.
- `<choice><orig>/<reg>` rendering for orthographic normalization.
- Sentence segmentation for source notes or metadata apparatus in v1.
- A second sentence splitter in ABC for parser-IR TEI rendering.
