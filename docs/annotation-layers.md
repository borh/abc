# Research annotation layers

Base transcription preserves source paragraphs and rich inline nodes. Sentence
segmentation belongs to independently selected analysis layers; it neither splits
source nodes nor adds sentence wrappers to base TEI.

Analysis consumes an identified text view of TEI rather than an export's presentation
whitespace. The `body-v1` view selects ruby bases and corrected or regularized readings,
omits notes, figure descriptions and running matter, and retains semantic line/page breaks and block
separators. UTF-8 byte offsets are the coordinate unit. An unresolved empty glyph
occupies U+FFFC and is excluded from eligible ranges; it is never silently deleted.
The view identity binds the exact text and reading policy. Its DOM alignment is local
to the source document and is not part of the reusable analyzer input. Image
metadata is not principal text: `figDesc` preserves descriptions and a
`note` of type `caption-reference` preserves any quoted caption reference. The
separately transcribed visible caption remains body text, following the
[Aozora image convention](https://www.aozora.gr.jp/annotation/graphics.html).
Correcting previously included descriptions changes affected text identities;
image-free readings retain their existing `body-v1` identity.

`soranoha-annotation-layer/1` JSON stores each producer's claims separately. The envelope
contains the view identity, a vocabulary content hash, named producer input hashes,
actual dependency hashes, eligible spans, execution status and records. Records carry
an identifier, half-open byte range, label and string-valued features. Vocabulary and
producer identities identify the meaning asserted by that producer; envelope validation
does not certify its linguistic conclusions.

All offsets must fall on UTF-8 boundaries within eligible ranges. Producer inputs
include the code, model, dictionary and configuration actually used, as applicable.
Masks are computation inputs: changing eligibility can invalidate analysis even when
text remains equal. A completed empty result differs from failed, partial and unrun
execution. Failed and unrun layers cannot carry claims. Competing tokenizations or
entity labels are separate artifacts, not replacements of shared records.

The computation identity excludes resulting claims and source-edition attachment.
The layer identity includes its claims and execution outcome. Assemblies select exact
layer artifacts. When source correspondence changes but analyzer inputs do not, the
same layer may be attached to the new edition through its current TEI alignment.

Enriched TEI retains the original transcription, inserts anchors and appends stand-off
span groups. Each group references the separately stored layer by content identity;
record labels use `span/@n` and feature objects use JSON text inside the span. These
are analysis claims rather than source assertions. The base TEI and plaintext stages
do not depend on the selected analysis layers. The enrichment stage consumes only
the base TEI and ordered layer blob identities.

Anchors can separate text nodes without changing their concatenated content. TEI
glyphs remain atomic: assembly rejects an analysis boundary inside a multi-codepoint
glyph rather than splitting its realization. Existing XML identifiers are preserved;
an identifier collision or an incompatible text view is an error. File commands refuse
to overwrite an existing output.

## Markdown consumer

The `markdown/2` profile targets horizontal CommonMark with inline HTML. The
flake-pinned reference consumer, cmark, runs with HTML enabled (`--unsafe`). Tests
verify that text, ruby nesting, line breaks and generated `span` attributes survive
that consumer. They do not qualify a browser's typography or a hosting service's
sanitizer. Readers must permit `ruby`, `rb`, `rt`, `strong`, `em`, `br`, and `span`
with its generated `style` and `data-tei-rend` attributes. Source text is escaped for
both HTML and CommonMark, including text inside inline HTML.

The profile retains visible body text and ruby readings. Explicit TEI renditions
have these transformations:

| Rendition | Horizontal projection |
| --- | --- |
| Bold and italic | `strong` and `em` |
| Named sesame, circle, triangle, double-circle, bullseye and cross dots | CSS text emphasis preserving the named shape; source-right marks above, source-left below |
| Solid, double and wavy side lines | CSS text decoration; source-right below, source-left above, both on both sides |
| Inline horizontal text and vertical text combination | `writing-mode: horizontal-tb` and `text-combine-upright: all`; already horizontal text stays horizontal |
| An unqualified inline box | A solid one-pixel border; no claim of the source's exact stroke width |

Dot shapes and side-line distinctions follow the
[Aozora emphasis notation](https://www.aozora.gr.jp/annotation/emphasis.html).
Its horizontal XHTML examples use underlines for default side lines and overlines
for left-side lines. Dot placement follows the separate
[CSS text-emphasis convention](https://www.w3.org/TR/css-text-decor-3/#text-emphasis-position-property).
These are explicit horizontal projection choices, not a rotation or a facsimile of
vertical source typography. Generated spans retain the original `rend` in
`data-tei-rend`; projection reports classify these cases as `transformed`.

The profile intentionally omits source apparatus, alternative readings and layout
such as exact pagination. Generic `emphasis` does not identify a visual style and
remains unsupported. Unknown rendition tokens, additional TEI inline styles,
unsupported ruby placement, and unresolved glyphs also remain explicit in reports;
their text is retained. A `complete-for-profile` result applies to this declared
projection, not source-parser correctness or complete editorial fidelity.

## Source correspondence and reading variants

Canonical TEI attaches explicit decoded UTF-8 source ranges through local `@source`
references. The referenced notes contain the original range record and identify the
primary text by content hash when available. Equal range records share one local
identifier. Parser-text offsets are a different coordinate system and cannot supply
missing source correspondence.

A supplied reading that differs from the base text is represented as `app` with
`lem` for the supplied reading and `rdg type="base-text"` for the alternative. This
does not assert that the base text is erroneous. Body and Markdown views select the
supplied reading; Markdown reports the alternative as intentionally omitted.

Positive interpretation facts identify the exact source marker the interpreter
resolved. They do not cover every nested marker in a rendered node's provenance
range. Paired scopes claim their opener and closer separately when both are known;
independent lexical occurrences join those facts by exact decoded byte range.

Canonical `interpretation-problem` notes retain the source interpreter's structured
facts. Content uncertainty excludes the whole body from eligible analysis until
an influence range in the analysis view is established; a marker's source range
alone cannot establish that range. Layout-only problems retain text eligibility.
Eligibility expresses usability under these explicit known-uncertainty masks; it
does not certify source coverage or prove that no further uncertainty exists.
These masks are separate from reading identity, so unchanged analyzer text can be
reused while a layer must still satisfy the current eligibility mask.

Warichu uses `seg type="warichu" rend="two-line"`. An undivided source remains one
content sequence; nested `seg type="upper"` and `seg type="lower"` appear only when
the input establishes those divisions. Text projections retain their supplied
reading order and report the omitted two-line layout.

Canonical TEI distinguishes Gothic, italic, superscript and subscript renditions.
Font size retains either a relative step count or an absolute size category;
source line-right and line-left small writing remain separate side values. These
source axes are not inferred to mean horizontal superscript or subscript. The
Markdown profile reports unimplemented font and script renditions explicitly.
