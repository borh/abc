# Aozora Syntax Coverage And Projection Design

**Date:** 2026-04-26
**Status:** Draft design, approved direction

## Purpose

The validation harness must compare Aozora Bunko parsers against the whole
syntax surface of the corpus, not only against a lossy visible-text projection.
The essential artifact is a syntax coverage matrix derived from:

- `docs/superpowers/specs/2026-04-25-parser-validation-harness-design.md`
- `docs/superpowers/specs/2026-04-26-parser-neutral-ir-design.md`
- `references/aozorabunko/rules/*.html`
- `references/parsers/AozoraEpub3-JDK21/chuki_*.txt`
- observed corpus feature inventory from `ab-index`

The matrix is both a design artifact and a test-routing artifact. It says what
each source notation means, how it should be preserved in parser-neutral IR, how
it projects to export formats, and how parser implementations are compared.

## Relationship To Existing Artifacts

This design supersedes the current `ab-ir` ruby and gaiji shapes where they are
too flat to preserve syntax semantics.

Current `crates/ab-ir/src/lib.rs` has:

```rust
Inline::Ruby {
    base: String,
    reading: String,
    provenance: Provenance,
}
```

That is sufficient for current AAT projection and lossy text comparison, but it
cannot represent ruby over a gaiji node. The syntax matrix therefore requires a
breaking `ab-ir` migration to a structured ruby base:

```rust
Inline::Ruby {
    base: Vec<Inline>,
    reading: String,
    placement: RubyPlacement,
    provenance: Provenance,
}
```

The existing flat `base: String` may remain temporarily in the AAT projection.
It must not remain the canonical parser-neutral IR.

The current `data/feature-patterns.toml` is not replaced immediately. Its role is
feature detection and corpus routing. The new
`data/aozora-syntax-coverage.toml` is the semantic authority for syntax IDs,
projection policy, and validation policy. During migration:

- `feature-patterns.toml` keeps driving `ab-index`.
- Each feature key should map to one or more syntax matrix IDs.
- New validation and comparison code should cite matrix IDs, not raw feature
  keys.
- Once the matrix contains equivalent routing patterns, `feature-patterns.toml`
  can be generated from or subsumed by the matrix.

The existing parser-neutral IR design remains valid for its broad goal: move
adapter output into typed values before projection. This design refines that goal
by making ruby bases, gaiji subtypes, and named projections explicit.

## Current Problem

The current codebase still uses helpers named like `source_visible_text` for
several different jobs:

- parser smoke validation
- parser-to-parser comparison
- plaintext rendering
- AAT visible projection
- temporary fallback construction

Those jobs are not the same. Recent gaiji/ruby work made this visible:

```text
ことを、※［＃「口＋愛」、第3水準1-15-23］《おくび》にも
```

For a lossy plaintext comparison projection, dropping both unresolved gaiji and
its orphan ruby can be acceptable. For AST and TEI export, it is wrong: the
gaiji should resolve to a character reference when possible, and the ruby should
remain attached to that base.

The design goal is to stop treating one projection as truth.

## Non-Goals

- Do not choose the final parser implementation in this design.
- Do not promote `aozora2`, `aozora-rs`, AozoraEpub3, or any other adapter to
  oracle status.
- Do not require every syntax row to be fully implemented before comparison can
  continue.
- Do not replace AAT immediately. AAT remains a comparison and validation
  exchange format while parser-neutral IR evolves.

## Core Model

Use three explicit layers.

### 1. Source Syntax

This layer records what appears in the Aozora text source. It includes raw
marker text, decoded spans, and the official rule/source that describes the
notation.

Examples:

- `吾輩《わがはい》`
- `｜あのひと《...》`
- `※［＃「口＋愛」、第3水準1-15-23］`
- `［＃ここから２字下げ］ ... ［＃ここで字下げ終わり］`
- `［＃「...」に傍点］`

Source syntax is not an AST. It is the evidence parsers consume.

### 2. Parser-Neutral IR

This layer preserves semantic structure independently of export format. It is
the target for all parser adapters, including deliberately imperfect reference
adapters.

Important rules:

- Ruby is an annotation over a base node, not merely a string replacement.
- A gaiji can be a ruby base.
- Unresolved gaiji is still a first-class node.
- Styling, layout, and block annotations remain nodes even when plaintext export
  drops them.
- Parser-derived nodes and heuristic/supplemental nodes must be distinguishable.

Ruby over unresolved gaiji should be representable as a structured value, for
example:

```text
Inline::Ruby {
  base: [Inline::GaijiRef {
    source: "※［＃「口＋愛」、第3水準1-15-23］",
    resolved: None,
    description: "「口＋愛」、第3水準1-15-23",
    kind: GaijiKind::JisLevel {
      level: 3,
      row: 15,
      cell: 23
    }
  }],
  reading: "おくび"
}
```

If the gaiji can resolve, the same structure keeps the ruby and changes only the
gaiji resolution:

```text
Inline::Ruby {
  base: [Inline::GaijiRef { resolved: Some("..."), ... }],
  reading: "おくび"
}
```

This keeps the AST correct while allowing lossy projections to omit unresolved
content.

Gaiji should be typed enough for routing and quality checks:

```rust
enum GaijiKind {
    UnicodeCodepoint { value: char },
    JisCode { plane: Option<u8>, row: u8, cell: u8 },
    JisLevel { level: u8, row: u8, cell: u8 },
    Composition { description: String },
    DakutenVariant { base: String, mark: DakutenMark },
    Unknown { description: String },
}

struct GaijiRef {
    source: String,
    description: String,
    description_format: Option<String>,
    kind: GaijiKind,
    resolved: Option<String>,
    provenance: Provenance,
}
```

This keeps the existing `description_format` information while replacing the
current empty-string unresolved convention with `Option<String>`. AAT can still
project unresolved gaiji as `resolved: null` or an empty string if required by
its schema, but `ab-ir` should not encode absence as an empty string.

### 3. Projections

Projection names must state their purpose. No generic "visible text" function is
canonical.

| Projection | Purpose | Lossy? |
|------------|---------|--------|
| `ir_projection::aat` | Current validation/comparison exchange format | Partly |
| `ir_projection::semantic_summary` | Parser-to-parser semantic comparison | No for modeled syntax |
| `ir_projection::tei` | Preservation-oriented TEI XML export | No for modeled syntax |
| `ir_projection::plaintext_lossy` | Human-readable plain text or Markdown body | Yes |
| `ir_projection::comparison_lossy` | Stable cross-parser smoke comparison | Yes |
| `source_projection::*` | Heuristic checks against source text only | Yes, confidence-labeled |

The comparison projection may drop unresolved gaiji and attached ruby readings.
The TEI projection must preserve them.

Initial `source_projection` names:

- `source_projection::comparison_lossy_body`: current source-side behavior used
  by `visible_text_body_order`, renamed and documented as lossy.
- `source_projection::ruby_markers`: source-side ruby marker inventory for
  completeness checks.
- `source_projection::gaiji_markers`: source-side gaiji marker inventory for
  resolution and preservation checks.
- `source_projection::command_markers`: source-side `［＃...］` command inventory
  for syntax routing and coverage reporting.

These source projections are heuristics. They may route tests and detect likely
loss, but they are not AST truth.

`ir_projection::semantic_summary` is a structured, matrix-keyed value, not a
plain visible-text string. Initial shape:

```json
{
  "syntax": {
    "ruby.explicit_base": [
      {
        "base_projection": "あのひと",
        "reading": "...",
        "placement": "right",
        "provenance": "parser"
      }
    ],
    "gaiji.jis_level": [
      {
        "source": "※［＃「口＋愛」、第3水準1-15-23］",
        "description": "「口＋愛」、第3水準1-15-23",
        "kind": "jis_level",
        "resolved": null,
        "provenance": "parser"
      }
    ]
  },
  "unmodeled": [
    {
      "source": "［＃...］",
      "reason": "syntax_id_not_modeled"
    }
  ]
}
```

It preserves all matrix-modeled syntax needed for parser comparison. It may
include lossy display fields such as `base_projection`, but those fields are
derived summaries, not the canonical IR node.

Interim AAT projection for gaiji plus ruby is intentionally lossy because AAT
currently has `ruby.base: string`:

- If the gaiji resolves, project `ruby.base` to the resolved character and keep
  `ruby.reading`.
- If the gaiji is unresolved, emit a `gaiji` node for the base, do not emit an
  orphan `ruby` node, and attach a warning with syntax ID
  `gaiji_ruby.unresolved_base`.
- The semantic summary and TEI projections must still preserve the ruby
  relationship.

This prevents AAT from pretending it can represent nested ruby bases while still
making the data loss observable.

## TEI Direction

TEI should preserve ruby as ruby. TEI P5 defines:

- [`<ruby>`](https://tei-c.org/release/doc/tei-p5-doc/en/html/ref-ruby.html)
  as the ruby container.
- [`<rb>`](https://tei-c.org/release/doc/tei-p5-doc/en/html/ref-rb.html)
  as the ruby base.
- [`<rt>`](https://tei-c.org/release/doc/tei-p5-doc/en/html/ref-rt.html)
  as ruby text.
- `rt@place` can express placement when needed.

For Aozora ruby over gaiji, TEI should map the base into `<rb>` and the reading
into `<rt>`. If the gaiji is unresolved, the base should still carry an explicit
reference rather than disappearing.

Illustrative TEI shape:

```xml
<ruby>
  <rb>
    <g ref="#gaiji-kou-ai"/>
  </rb>
  <rt place="right">おくび</rt>
</ruby>
```

The exact gaiji declaration format belongs in the TEI export design, but the IR
must preserve enough data to produce it.

## Syntax Coverage Matrix

The matrix is a checked-in data artifact, not prose hidden in a design doc.
Recommended path:

```text
data/aozora-syntax-coverage.toml
```

Each row represents one syntax feature or feature family. The field name
`reference_sources` is used instead of `official_sources` because some useful
tables, such as AozoraEpub3 `chuki_*.txt`, are parser references rather than
official Aozora Bunko rules.

```toml
[[syntax]]
id = "ruby.explicit_base"
category = "inline_annotation"
reference_sources = [
  "references/parsers/AozoraEpub3-JDK21/chuki_tag.txt",
  "references/aozorabunko/rules/kijyunn.html"
]
source_examples = ["｜あのひと《...》"]
source_patterns = ["｜[^《\\n]+《[^》]+》"]
ir_nodes = ["Inline::Ruby"]
aat_nodes = ["ruby"]
tei_projection = "ruby/rb/rt"
plaintext_projection = "base_only"
comparison_projection = "base_only"
validation_properties = ["ruby_completeness", "semantic_ruby_sequence"]
adapter_expectations = ["must preserve reading", "must not duplicate base"]
status = "covered"
```

Required fields:

| Field | Meaning |
|-------|---------|
| `id` | Stable syntax identifier such as `gaiji.jis_code` |
| `category` | `inline_annotation`, `block`, `layout`, `glyph`, `metadata`, etc. |
| `reference_sources` | Local official docs, parser reference tables, or corpus-derived evidence |
| `source_examples` | Minimal examples, preferably real corpus examples |
| `source_patterns` | Detection patterns for routing, not parser grammar |
| `ir_nodes` | Parser-neutral IR node kinds required to preserve semantics |
| `aat_nodes` | Current AAT projection nodes, if representable |
| `tei_projection` | TEI element strategy |
| `plaintext_projection` | Plaintext/Markdown policy |
| `comparison_projection` | Cross-parser lossy comparison policy |
| `validation_properties` | Checks that apply to this syntax |
| `adapter_expectations` | What adapter summaries must report |
| `status` | `covered`, `partial`, `not_modeled`, or `needs_research` |

### Matrix Field Definitions

`source_patterns` are Rust `regex` crate patterns unless a row explicitly says
otherwise. They are used for corpus routing and fixture discovery only. They are
not grammar productions and must not be the source of AST truth. TOML should use
literal strings (`'...'`) when possible so backslashes keep regex meaning.

`status` values:

- `covered`: `ab-ir` can represent the syntax, at least one adapter projects it
  into the expected IR/AAT shape, and at least one validation or comparison test
  covers the row.
- `partial`: some evidence is detected or projected, but representation,
  validation, or adapter coverage is incomplete.
- `not_modeled`: the syntax is recognized as part of Aozora Bunko but has no
  parser-neutral IR representation yet.
- `needs_research`: the source notation or desired projection is not understood
  enough to specify behavior.

`adapter_expectations` are human-readable review assertions in the first
version. Machine-checkable requirements belong in `validation_properties`.

`validation_properties` reference property IDs in `ab-check` or planned property
IDs in this design. Existing property IDs may keep their current names for
compatibility, but their docs must say which matrix IDs they cover and whether
they are strict or heuristic.

## Initial Syntax Families

The first matrix should cover the priority 1 families before more parser fixes
are treated as semantic improvements. Priority 2 rows may be coarse initially
but need enough evidence to avoid losing known syntax.

| Priority | Family | Examples | IR Requirement |
|----------|--------|----------|----------------|
| 1 | Ruby | implicit base, explicit `｜`, left ruby | `Inline::Ruby { base: Vec<Inline>, reading }` |
| 1 | Gaiji | Unicode, JIS, composition descriptions, dakuten variants | `Inline::GaijiRef` with source, kind, and resolution fields |
| 1 | Gaiji + Ruby | gaiji marker followed by `《reading》` | ruby base can be a gaiji node |
| 1 | Headings | inline and block 大/中/小見出し | `Block::Heading { level }` |
| 1 | Emphasis | 傍点, 白ゴマ傍点, 傍線, 太字, 斜体 | inline style wrappers with variant |
| 1 | Indentation | single-line 字下げ, block 字下げ, 地付き, 字上げ | block/line layout attributes |
| 1 | Warichu | inline and block 割り注 | `Inline::Warichu` or block variant |
| 1 | Captions/Images | image insertion, captions | figure/media nodes plus caption |
| 1 | Page/Line Breaks | 改ページ, 改行, page positioning | break/milestone nodes |
| 2 | Font Size | 大きな文字, 小さな文字, start/end forms | inline/block style with magnitude |
| 2 | Quote/Box/Window | 引用, 罫囲み, 窓 | block wrappers |
| 2 | Writing Direction | 横組み, 縦中横 | direction/layout wrappers |
| 2 | Kanbun Marks | 返り点, レ点, okurigana side notes | annotation nodes, not plaintext |
| 2 | Accent/Latin Marks | bracket accent notation | normalized character or annotation |
| 2 | Editorial Notes | generic `［＃...］` notes | typed note or raw command |
| 2 | Metadata/Colophon | title, author, bottom text, input/proof fields | document metadata, body boundary |

Rows may start coarse, but each row needs a status and a reason. Coarse rows are
acceptable only when parsers do not yet disagree in a way that requires a finer
split.

## Adapter Comparison Semantics

Every adapter is a subject under comparison. In the current adapter protocol,
adapters emit AAT JSON on stdout. The syntax support summary and provenance
counts are computed by `ab-check` or `ab-compare` from AAT/IR artifacts unless a
future protocol explicitly adds sidecars.

Adapters should make it possible to compute:

- IR/AAT artifacts.
- A syntax support summary keyed by matrix `id`.
- Provenance counts: parser-owned, parser-normalized, heuristic supplement,
  fallback.
- Warnings keyed to syntax IDs when possible.

Comparison should report:

- pass/fail validation deltas
- syntax support deltas by matrix row
- semantic summary deltas
- projection deltas by projection name
- sample snippets for each changed syntax family

This avoids pretending that zero validation failures means no AST differences.

## Validation Properties

Existing properties remain useful but must be renamed or scoped where they are
lossy.

Recommended direction:

- Keep `visible_text_body_order` as `comparison_lossy_body_order` or document it
  as lossy.
- Add syntax-specific properties:
  - `semantic_ruby_sequence`
  - `semantic_gaiji_sequence`
  - `gaiji_resolution_quality`
  - `block_scope_balance`
  - `style_scope_balance`
  - `heading_structure`
  - `figure_caption_integrity`
  - `tei_projection_preserves_modeled_syntax`
- Every property declares:
  - input layer: source, IR, AAT, TEI, plaintext
  - confidence: strict, parser-reported, heuristic
  - syntax IDs it covers

## Data Flow

```text
Official rules + chuki tables + current feature patterns
  -> syntax coverage matrix

Corpus source
  -> ab-index feature routing
  -> feature keys mapped to syntax matrix IDs

Parser adapter
  -> parser-native output
  -> parser-neutral IR
  -> AAT / TEI / plaintext / semantic summary projections

ab-check
  -> matrix-keyed property reports

ab-compare
  -> parser comparison by validation, semantic summary, and projection
```

## Migration Path

1. Add `data/aozora-syntax-coverage.toml` with the priority 1 rows above and a
   mapping from existing `feature-patterns.toml` keys to syntax IDs.
2. Rename or wrap the current `source_visible_text` behavior as
   `source_projection::comparison_lossy_body` without changing behavior.
3. Port `visible_text_body_order` documentation to say it is a heuristic,
   lossy comparison property. Keep the existing property ID until report
   compatibility is deliberately broken.
4. Extend `ab-ir` ruby and gaiji types to support structured ruby bases and
   typed gaiji references. Add AAT projection compatibility for the current flat
   ruby shape.
5. Add `ir_projection::semantic_summary` and teach `ab-compare` to diff
   matrix-keyed semantic summaries before treating parser agreement as real AST
   agreement.
6. Add TEI projection tests for ruby, gaiji, and gaiji+ruby before adding broad
   TEI export.
7. Once the matrix can drive routing, generate or retire duplicated entries in
   `feature-patterns.toml`.

## Design Constraints

- Regex may route works and support heuristic checks. Regex must not become the
  source of AST truth.
- A lossy projection must never be used to justify discarding IR data.
- Official Aozora source text remains authoritative for source syntax.
- Official XHTML is a rendering reference, not an AST oracle.
- Parser choice remains undecided until matrix coverage, validation results, and
  performance data are compared across candidate parsers.

## Success Criteria

- A checked-in syntax coverage matrix exists and is referenced by docs.
- Each matrix row links source syntax to IR, AAT, TEI, plaintext, comparison,
  and validation behavior.
- At least ruby, gaiji, gaiji+ruby, headings, style annotations, indentation,
  warichu, image/caption, and page/line breaks have initial rows.
- Each priority 1 row has a declared relationship to existing
  `feature-patterns.toml` keys.
- The gaiji+ruby row defines different AAT, semantic summary, plaintext, and TEI
  projection behavior.
- Current lossy comparison behavior is explicitly named as such.
- Future parser fixes can cite syntax matrix row IDs rather than inventing
  behavior from local regex helpers.

## Review Notes

- This design deliberately does not decide whether the final parser is an
  existing parser, a fork, or a new parser.
- The current `ab-ir` and AAT `ruby.base: string` shape is too weak for ruby
  over gaiji. AAT can keep this shape for current comparison, but
  parser-neutral IR and TEI need nested ruby bases.
- The previous assertion that orphan ruby after unresolved gaiji projects to
  nothing is only a comparison/plaintext policy. It is not correct AST or TEI
  behavior.
