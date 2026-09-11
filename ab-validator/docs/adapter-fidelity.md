# Adapter fidelity and measurement limits

AAT schema validity does not establish fidelity to the source. Adapters that
project rendered HTML cannot recover distinctions already lost by the renderer.
The syntax matrix's `evidence` fields identify the supporting implementation or
measurement record; they do not establish publication eligibility.

## Adapter boundaries

- `aozora2html` projects rendered XHTML. Source-only distinctions need separate
  source evidence; an XHTML comparison is a rendered-body comparison.
- `aozora-epub3 --mode html` extracts and filters body XHTML from an EPUB.

## Historical parser observations

These findings describe the named parser versions and retained corpus runs.
They have not been remeasured against newer parser versions.

- AozoraEpub3 `1.3.4-jdk21` rejected half-width numerals in some annotations
  accepted with full-width numerals, including block indentation.
- The same version produced malformed warichu HTML with
  `[ERROR] 割り注終わりなし`. Rendered split spans did not provide the upper/lower
  rows required by the AAT warigaki representation.
- aozora-core `0.7.1` produced empty children for some implicit ruby bases after
  a gaiji-bearing ruby on the same line. The base remained in a preceding text
  node. The `aozora2-full-20260705` dump contained 3,541 such rubies across
  1,266 works; direct upstream reproduction separated parser behavior from
  adapter projection.
- The `aozora-epub3-full-20260704` dump contained 14 empty ruby bases across
  three works: 12 used image-rendered gaiji bases and two lost a plain kanji base.

The adapter-note records in `data/adapter-fidelity-notes.toml` retain these
population labels. Reusing a result for another input population requires a new
measurement; these counts are not current-corpus totals.
