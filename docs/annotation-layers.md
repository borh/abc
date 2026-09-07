# Research annotation layers

Analysis consumes an identified text view of TEI rather than an export's presentation
whitespace. The `body-v1` view selects ruby bases and corrected or regularized readings,
omits notes and running matter, and retains semantic line/page breaks and block
separators. UTF-8 byte offsets are the coordinate unit. An unresolved empty glyph
occupies U+FFFC and is excluded from eligible ranges; it is never silently deleted.
The view identity binds the exact text and reading policy. Its DOM alignment is local
to the source document and is not part of the reusable analyzer input.

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

The Markdown projection is tested with the flake-pinned CommonMark reference
consumer, cmark, with HTML enabled (`--unsafe`). This permits the writer's ruby,
emphasis and line-break elements; it is not a claim about a hosting service's
sanitizer. Source text is escaped for both HTML and CommonMark, including text
inside inline HTML elements.

The profile keeps ruby readings, bold and italic emphasis and visible body text.
It intentionally omits source apparatus, alternative readings in `choice`, and
layout such as exact pagination. Unsupported ruby placement, other emphasis
renditions and unresolved glyphs remain visible in projection reports. A report's
`complete-for-profile` result applies to that declared projection, not to correctness
of the source parser or complete editorial fidelity.
