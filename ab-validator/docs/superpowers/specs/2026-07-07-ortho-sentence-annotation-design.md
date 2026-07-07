# Orthographic Sentence Annotation Design

**Date:** 2026-07-07
**Status:** superseded in part by the parser-IR sentence segmentation follow-up
**Governs:** `ab-ortho-detect` (annotations), `ab-aat-to-parser-ir`
(`orthographic_annotations` serialization), ABC parser-IR schema, downstream
ABC TEI renderer.
**Predecessors:** `2026-07-05-ortho-detect-design.md` (detector + ADR).
**Follow-up correction:** Decision D3 ("ABC owns sentence splitting") is
superseded by
`2026-07-07-parser-ir-sentence-segmentation-and-ortho-tei-design.md`.
ab-validator owns parser-IR sentence segmentation evidence; ABC renders it.

## Goal

ABC's TEI output marks sentences written in kanji-katakana-majiri style
so consumers know to normalize before tokenization. The original text is
never modified; the annotation is an attribute on the sentence element.
ab-validator provides the evidence (byte-range annotations); ABC's TEI
renderer produces the markup.

## Motivation

The ortho-detect layer normalizes text before tokenization and remaps
morpheme surfaces back to original coordinates. This works for the
morphological analysis pipeline. But downstream TEI output has no marker
telling consumers "this sentence was katakana-majiri — normalize it before
your own tokenization." Adding sentence-level annotations closes that gap.

The three-way human-annotated gold set (accept/normalize/reject) evaluates
detector recall. The semantic distinction (historical orthography vs
emphatic/stylistic katakana) is research metadata — the detector cannot
make this distinction with character-only features, so the output
vocabulary stays undifferentiated.

## Prior art

- **TEI `<normalization>`** (under `<editorialDecl>`): Declares normalization
  policy. `@method="markup"` signals markup is used.
- **TEI `<s>` with `@type`** (body): Sentence-level classification via
  `att.typed`. User-defined type values, documented in the header.
- **TEI `<choice><orig>/<reg>`**: Editorial replacement. Rejected — this is
  analytic annotation, not text replacement.
- **Hoshi (2021):** Japanese morphological adorner for TEI XML. Validates
  "annotate without modifying text." Uses standoff pointers.
- **No established Japanese vocabulary:** `@type` values are user-defined
  in TEI by design. We define `orthographic-katakana`.

## Design

### What the detector does (honest label)

The v1 detector performs mechanical katakana→hiragana conversion on
sentences where katakana is doing grammatical or stylistic work that would
degrade tokenization. It does NOT perform historical kana normalization
(歴史的仮名遣い→現代仮名遣い). That is `OrthoNormalization::HistoricalToModern`,
which no detector emits in v1.

The TEI annotation reflects what actually happened: this sentence was
identified as katakana-dominant prose where kata→hira conversion may
help. The label is `type="orthographic-katakana"` — no subtype. The
three-way gold labels (accept = historical, normalize = stylistic) are
evaluation metadata; the detector cannot distinguish them.

### TEI output (ABC-owned)

ABC's TEI renderer splits body text into `<s>` elements and sets
`@type="orthographic-katakana"` on sentences that overlap an
`OrthoAnnotation`:

```xml
<a:body>
  <a:p>
    <a:s type="orthographic-katakana">吾輩ハ猫デアル</a:s>
    <a:s>名前はまだ無い。</a:s>
    <a:s type="orthographic-katakana">名前ハマダ無イ</a:s>
  </a:p>
</a:body>
```

TEI header (note: `<normalization>` goes under `<editorialDecl>`,
matching the existing TEI profile):

```xml
<a:encodingDesc>
  <a:editorialDecl>
    <a:normalization method="markup">
      <a:p>Sentences annotated with <a:gi>s</a:gi>
         <a:att>type</a:att>="orthographic-katakana" indicate
         kanji-katakana-majiri text. The ab-validator ortho-detect
         layer identified these sentences as katakana-dominant prose
         where normalizing katakana→hiragana before tokenization may
         improve morphological analysis. This is mechanical script
         normalization, not historical kana normalization.</a:p>
    </a:normalization>
  </a:editorialDecl>
</a:encodingDesc>
```

### Data contract: parser-IR `orthographic_annotations`

A new optional top-level field in parser-IR JSON, carrying the detector's
output alongside the node/paragraph tree that ABC's renderer already
consumes. Shape:

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

Field breakdown:

| Field | Type | Source |
|---|---|---|
| `work_id` | String | `AAT.work_id`; guards against attaching annotations to the wrong work |
| `work_content_hash` | `sha256:<64 hex>` | `AAT.meta.source_hash`, also projected to `parser_ir.source.work_content_hash` |
| `coordinate_system` | `"decoded_utf8"` | Same byte-offset coordinate system as parser-IR spans |
| `detector_id` | `OrthoDetectorId` JSON | `OrthoDetector::detector_id()` |
| `annotations` | `[OrthoAnnotation]` | `detector.detect()` output |
| `annotations[].source_byte_range` | `{start, end}` (byte offsets in original doc) | `OrthoAnnotation.source_byte_range` |
| `annotations[].normalized_text` | String (kata→hira) | `OrthoAnnotation.normalized_text` |
| `annotations[].kind` | `"ScriptKatakanaToHiragana"` | `OrthoAnnotation.kind` |
| `annotations[].confidence` | `u8` or `null` | `OrthoAnnotation.confidence` |

`detector_id` uses the existing serde shape for `OrthoDetectorId`: the v1
heuristic serializes as `"HeuristicV1"`; the ML detector serializes as
`{"MlLogisticRegression": {"model_hash": "<sha256 hex>"}}`.

`OrthoAnnotation` is reused as the annotation record shape. The wrapper adds
detector provenance, source identity, and coordinate-system declaration, none
of which `OrthoAnnotation` itself carries. The source identity is part of the
contract because the annotation bundle is a standalone input to
`ab-aat-to-parser-ir`; the converter must reject a bundle whose `work_id` or
`work_content_hash` does not match the AAT being converted.

ABC schema change: add `orthographic_annotations` to parser-IR
`properties` and `$defs`. The follow-up parser-IR sentence design sets the
next schema version to `0.6.0` and includes the corresponding mapping artifact
hash update. Separate ABC task — not in ab-validator scope.

ab-validator must not emit parser-IR that claims the old schema hash while
carrying `orthographic_annotations`. If the loaded parser-IR schema does not
declare the field, the converter must fail with an explicit precondition error,
not skip output validation.

### Why not `AatProjection.ortho_normalizations`

- `AatProjection` is an internal Rust struct; ABC consumes parser-IR JSON.
- `ortho_normalizations` is documented for future `<choice><orig>/<reg>`
  rendering (editorial normalization, not sentence classification).
- Sharing one field for two different TEI semantics creates ambiguous
  consumers.

`AatProjection.ortho_normalizations` stays as-is for its intended
`<choice>` use. The new `orthographic_annotations` field in parser-IR
is a separate versioned contract for `<s>` annotation.

### Sentence splitting (superseded)

This section is superseded by
`2026-07-07-parser-ir-sentence-segmentation-and-ortho-tei-design.md`.
The earlier decision assigned sentence segmentation to ABC. The follow-up
design moves sentence segmentation evidence into parser-IR, produced by
ab-validator during parsing/conversion. ABC renders the supplied sentence rows
and does not split parser-IR text for TEI when `sentences[]` is present.

Historical pre-follow-up design:

ABC's renderer:
1. Splits body text into sentences using ABC's splitter
2. Wraps each sentence in `<s>`
3. For each `<s>`, checks if its byte range overlaps any
   `OrthoAnnotation.source_byte_range`
4. If yes: sets `@type="orthographic-katakana"`

In this historical design, sentence boundaries were ABC's decision.
ab-validator's annotations were byte ranges; ABC's splitter determined where
`<s>` tags went. An `<s>` carried `@type` if it overlapped an annotation
(partial overlap counted — a sentence that started in normal orthography and
ended in katakana-majiri still got the annotation).

### All-sentence wrapping

Recommend all sentences get `<s>` wrappers (consistent structure), with
`@type` only on annotated ones. Partial wrapping (only ortho sentences get
`<s>`) creates two shapes for the same concept. The follow-up spec makes
all-sentence rows part of parser-IR evidence.

### Boundary

| Concern | Owner |
|---|---|
| Ortho detection + annotation byte ranges | ab-validator (`ab-ortho-detect`) |
| Serialize annotations to parser-IR `orthographic_annotations` | ab-validator (`ab-aat-to-parser-ir` converter) |
| Parser-IR schema: add `orthographic_annotations` field | ABC (`abc/schemas/parser-ir.schema.json`) |
| Mapping artifact target schema hash update | ab-validator mirror of ABC schema/mapping inputs |
| Sentence segmentation evidence | Superseded: ab-validator parser-IR conversion |
| `<s>` wrapping + `@type` rendering | ABC (TEI renderer) |
| `<normalization>` header declaration | ABC |
| TEI ODD profile update (allow `<s>` + `@type`) | ABC |

### Non-goals

- **Three-way subtype in output:** Requires work-level features the
  character-only detector lacks. The three-way gold labels evaluate
  recall; the output says `orthographic-katakana` regardless.
- **`<choice>/<orig>/<reg>`:** Rejected. Annotation, not replacement.
- **New sentence splitter in ab-validator:** Superseded by follow-up design;
  parser-IR sentence segmentation is now planned in ab-validator conversion.
- **Paragraph-level aggregation:** Stay sentence-level. Consumers can
  aggregate from sentence annotations.

## Decisions

| # | Decision | Rationale |
|---|---|---|
| D1 | `type="orthographic-katakana"` only — no subtype | Detector does kata→hira, not historical kana normalization. Honest label. |
| D2 | New parser-IR field `orthographic_annotations: {work_id, work_content_hash, coordinate_system, detector_id, annotations: [OrthoAnnotation]}` | ABC-facing contract with enough identity to reject mismatched standalone bundles. `AatProjection.ortho_normalizations` stays internal (reserved for `<choice>`). New field is versioned in parser-IR schema. |
| D3 | Superseded: parser-IR owns sentence segmentation evidence | Follow-up spec `2026-07-07-parser-ir-sentence-segmentation-and-ortho-tei-design.md` moves splitting to ab-validator parsing/conversion and leaves ABC responsible for `<s>` rendering. |
| D4 | `<s>` overlap: partial overlap = annotated | A sentence that partially overlaps an annotation gets `@type`. Avoids edge-case gaps. |
| D5 | `<normalization>` under `<editorialDecl>` | Matches existing TEI profile structure. |
| D6 | Detector ID and source identity explicit in wrapper | `OrthoAnnotation` doesn't carry detector ID, coordinate-system, or source identity; the wrapper bundle adds them so ABC consumers know provenance and the converter can reject mismatches. |
| D7 | No validation skip | Output with `orthographic_annotations` must validate against a parser-IR schema that declares the field. Until ABC updates the schema and mapping hash, the converter fails early instead of emitting invalid parser-IR. |

## Open questions

The original open questions are superseded by
`2026-07-07-parser-ir-sentence-segmentation-and-ortho-tei-design.md`:

- All-sentence wrapping is represented as parser-IR `sentences[]` rows.
- The parser-IR schema version becomes `0.6.0`.
