# Gaiji Resolution Comparison Notes

**Date:** 2026-04-26
**Status:** Non-normative comparison notes

## Purpose

These notes summarize gaiji resolution capabilities to compare across parser
implementations. They are not syntax authority for the coverage matrix. Official
Aozora Bunko rules and observed corpus examples remain the source basis for
`data/aozora-syntax-coverage.toml`.

GPL parser implementation tables and source files in `references/parsers/` may
be inspected locally to understand adapter behavior, but they must not be copied
into project data or cited as `reference_sources` for syntax rows unless the
same material is available from a compatible non-GPL source. MIT, Apache, BSD,
public-domain, and similarly permissive files can be reused when the source path,
upstream URL or package, and license are recorded.

## Comparison Dimensions

Gaiji comparison should be reported as capabilities, not as a single pass/fail:

| Dimension | Meaning |
|-----------|---------|
| `unicode_codepoint_parse` | Parser recognizes explicit `U+XXXX`, `UCS-XXXX`, or equivalent Unicode notation. |
| `unicode_sequence_parse` | Parser recognizes multiple codepoints or base character plus variation selector / combining mark. |
| `jis_menkuten_parse` | Parser recognizes Aozora/JIS menkuten notation such as `1-15-23` or `第3水準1-15-23`. |
| `jis_to_unicode_resolution` | Parser maps menkuten notation to Unicode text. |
| `description_lookup` | Parser maps descriptive gaiji names to Unicode or alternatives. |
| `dakuten_variant_resolution` | Parser handles dakuten/handakuten variants as text, composed Unicode, or glyph fallback. |
| `ivs_support` | Parser preserves or emits Ideographic Variation Sequence information. |
| `image_fallback` | Parser can represent unresolved gaiji as an image reference. |
| `glyph_font_fallback` | Parser can preserve unresolved or variant glyphs through embedded glyph fonts. |
| `unresolved_policy` | Parser's behavior when resolution fails: `geta`, note span, source marker, image, warning, or drop. |
| `ruby_base_preservation` | Parser preserves ruby attached to gaiji, including unresolved gaiji. |

These dimensions should feed `semantic_summary` and comparison reports. They
should not force every parser into the same rendering strategy. A parser that
preserves structure without resolving characters can be preferable to a parser
that produces plausible text while losing source syntax.

## Observed Parser Families

The current reference set suggests these broad families:

- Structural parsers: recognize gaiji syntax but leave character resolution to a
  downstream phase.
- Text-normalizing parsers: resolve explicit Unicode and JIS notation to Unicode
  when possible, often falling back to the geta mark.
- HTML/rendering parsers: preserve unresolved gaiji as notes or image
  references for display.
- EPUB/rendering pipelines: may add alternative-character tables, image
  fallback, glyph fonts, and IVS-aware rendering.
- Corpus extractors: optimize for plain text and may convert common gaiji while
  serializing unresolved cases as textual notes.

The comparison harness should report which family of behavior each adapter
exhibits per work and per syntax matrix row. It should not treat any family as
the oracle.

## IR Implications

`Inline::GaijiRef` should preserve:

- raw source marker
- parsed description
- typed `GaijiKind`
- resolved text, if available
- one or more candidate resolutions, if a parser reports alternatives
- fallback representation, if the parser emits image/glyph/note output
- provenance and warning information

This lets TEI, plaintext, HTML, and comparison projections make different
choices without asking the parser to throw away information early.
