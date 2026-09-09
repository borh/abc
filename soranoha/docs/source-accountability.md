# Source accountability and validation

`source-accountability.json` records lexical occurrences found directly in the
retained source. It binds the original bytes, decoded UTF-8 text and coverage
matrix by hash. Each occurrence retains its exact spelling, decoded byte span,
source region and recognized syntax families. Editorial legend examples remain
visible as front matter. Unrecognized markers and lossy decoding remain explicit.

This scanner runs independently of the parser. Its stage identity binds the
scanner executable and matrix; changes confined to the parser stage do not
invalidate its evidence. Changes to shared scanner dependencies can invalidate it.
The native command is `ab-source-inventory --source source.txt --matrix matrix.toml
--output-json accountability.json`. The publication wrapper supplies
`AB_SOURCE_INVENTORY_BIN` and `AB_AOZORA_SYNTAX_MATRIX`.

Lexical recognition does not establish a correct interpretation. Interpreter
claims describe what the parser recognized; missing claims remain
unaccounted. Neither source preservation nor an empty problem list proves semantic
coverage. Source conformance tests exercise expected values and annotation
placement for specific examples. TEI profile validation checks the exported
XML against the profile. These are separate kinds of evidence.

A report about one edition does not compare its exported values independently
against every source construction. Conformance tests support the examples they
exercise; structural validation and interpreter claims are not equivalent to a
per-edition source/export fidelity certificate.

`interpretation-coverage.json` joins this lexical evidence to explicit native
interpretation facts. Family counts distinguish `interpreter_claimed` from
`unaccounted`; a claimed occurrence has at least one explicit compatible aspect,
not necessarily every aspect. Each claim must match the exact span and a
compatible family of an independently scanned occurrence or nested component.
Nested markers retain their own component spans. Source apparatus is kept
separate. Lossy decoding prevents positive claim accounting. Reported
interpretation problems retain their original influence bounds alongside positive
claims. A positive claim is not evidence that an overlapping or document-wide
problem has been resolved.

The report states the counts and lists the exceptions. `families` and
`interpretation_problems` cover every occurrence; `occurrence_count` and
`unclassified_occurrence_count` give the totals; `unaccounted_occurrences` holds,
whole, each occurrence with an unaccounted family or no recognized family at all.
A claimed occurrence is not listed, because the correspondence is already
recorded from the other direction: the TEI element the claim produced carries
that occurrence's span in its `source` attribute. Nothing else the build writes
records which occurrences went unaccounted, so those are kept in full.

## Review exports

From the repository root, set these variables to absolute paths. The corpus must
be a clean Git checkout and the export directory must not exist:

```sh
nix run .#soranoha-kernel -- build \
  --root "$BUILD_STORE" \
  --aozora-root "$CORPUS_CHECKOUT" \
  --assets-root "$PWD/soranoha" \
  --out "$EXPORT_DIR"
```

Each work directory contains `tei.xml`, `plain.txt`, `text.md`,
`tei-validation.json`, `plaintext-projection.json`, `markdown-projection.json`,
`source-accountability.json` and `interpretation-coverage.json`. A top-level
`build.json` records artifact hashes and build results. Omitting `--out` retains
outputs in the computation store. Building exports does not publish them or accept
an assessment.

TEI retains ruby, gaiji declarations, headings, layout and source apparatus.
Supplied kunten uses `note type="kunten"`: `subtype="return-mark"` carries
subscript rendition and `subtype="okurigana"` carries superscript rendition.
Annotation text omits syntax parentheses; source spans retain exact notation.
These notes preserve their insertion position and enclosing context without
inferring a host character or spoken reading. The principal-body text view
excludes notes, including kunten, while TEI retains their supplied text.
Resolved gaiji contain Unicode text directly in `g`, including inside ruby bases;
`ref` and `charDecl` retain source mappings. Plaintext contains visible body text.
Paragraph-leading indentation is encoded as CSS `text-indent`; heading and
source-note continuation indentation uses `padding-inline-start`. Structural XML
is indented for inspection while mixed content retains its lexical whitespace.

Complete iteration notation `／＼` and `／″＼` retains its source spelling in
`choice/orig`; `choice/reg` contains the whole Unicode mark `〱` or `〲`.
The body view, plaintext and Markdown select that realization without expanding
the repeated reading. A mark inside ruby reading remains part of the ruby
annotation. Malformed notation and literal Unicode upper/lower halves remain
literal text; iteration marks are distinct from gaiji.

Supplied diacritic annotations apply to native-resolved target text. Their
`choice/orig` retains the source spelling and `choice/reg` contains the composed
Unicode text selected by the body view, plaintext and Markdown. An annotation
addressing an interior letter retains its original marker separately as source
apparatus. This is transcription of supplied marks, not an editorial correction
or an inferred transliteration. Unresolved selectors retain explicit uncertainty.

An emphasis selector may name the range to mark or the range to leave out.
`［＃「自律・自由・人格・性格」の「・」を除く部分に傍点］` marks every part of
the target the named run does not cover, once for every occurrence of that run
rather than the first alone, and the cut-out characters remain unmarked body
text in their source position. The claim covers the marker, not the ranges the
marker points at. A run the target does not contain, or one that covers the
whole target, leaves the marker unread rather than marking a range the source
did not select.

A marker with two operands can also state a position rather than a range.
`［＃「自」と「然」の間に白三角傍点］` places a mark at the junction of two
adjacent characters and marks neither of them, so it publishes a supplied mark
anchored between them rather than emphasis over a span. The mark's side and its
anchor stay separate: every such mark sits on the right, and `の間に` says where
along the line rather than which side of it. The note carries the source's own
word for the mark, with the extent that word occupies in the marker. Operands
the source names with anything between them, or in an order the text does not
have, locate no junction and leave the marker unread.

Supplied block geometry retains indentation, continuation indentation, line width,
and distance from the line end independently. Hanging indentation uses the
continuation padding and the difference between first and continuation indents.
TEI retains supplied line counts as `rend="line-count(N)"` and placement between
the physical left and right page edges as `rend="page-horizontal-center"`.
Page placement and text alignment remain independent.
Consecutive indentation instructions replace the preceding indentation scope.
An indentation close that restates its measure, as `［＃１字下げここまで］` and
`［＃１字下げ終わり］` do, ends the scope that measure opened. A restated
measure that disagrees with the open leaves the pair unmatched rather than
closing at a boundary the source did not name. A close that names scopes no
block opened, such as `［＃字下げ、地付きここまで］` following line-scoped
`次行は…で` instructions, stays retained source rather than ending scopes that
do not exist.
Page placement ends before the next supplied page break, which remains in TEI.
Frame styles and co-applied typography share the same formatting vocabulary on
TEI `div` and `hi` scopes. Supplied solid and dashed rules retain their border
style; framing alone does not establish an independent embedded text.
A supplied table role is retained on the scope the source supplies it for, both
on its own marker and as a clause of a compound indentation marker, alongside
that marker's independent indentation, direction and frame. TEI records it as
`div type="table"`, and `図表` as `type="figure-table"`, because that spelling
names an exhibit without choosing between a diagram and a table. The enclosed
lines keep their own spelling and spacing; no cell, row or column structure is
derived from them. A supplied `罫無し` is retained as `rend="table-rules(none)"`,
an explicit absence of table rules distinct from a marker that says nothing
about them, and independent of any frame drawn around the scope. A base-edition
table description remains source apparatus and is not executed as current
formatting. A standalone table opener carries the axes it names beside the role
in the same way: `ここから表罫囲み` an enclosure and `ここから横組みの表` a
writing direction, with no indentation invented for either. `罫仕切り` is not
read as a table: it states a reading convention over the lines that follow,
rather than delimiting the partitions it names, and the rules that bound them
are literal text in the source.
A supplied quotation or letter role is read the same way when the source writes
it ahead of the measure that scopes it, as `［＃ここから引用文、３字下げ］` and
`［＃これより手紙文、１字下げ］` do. The role and the indentation are both
retained on that one scope, and TEI records the role as `div type="quotation"`
or `div type="letter"`. The two stay apart because the source spells them apart:
a letter is a document the text reproduces, not a passage it quotes. Nothing
about the quoted passage's own source, or a letter's correspondents, is derived
from the marker. A quotation's close is matched only against a scope carrying
that role, so a closer never ends a plain indentation scope and thereby report
its lines as quoted matter. Clauses the role reading does not cover stay retained
source: a supplied vertical gap such as `３行アキ`, and a line-subset exception
such as `はじめの「一」のみ２字下げ`.
A line-scoped foot measure can carry a qualitative type size beside it, as
`［＃この行、下揃え、下から５字上げ、相対的に字が小さい］` and
`［＃この行はポイントを下げて、地より２字上げ］` do. The measure and the size are
retained on the same line, and no magnitude is derived for the size: the source
states a direction and no degree. A marker that names a target for the measure,
or that has lost its target and leaves a bare `は`, stays retained source, and so
does one that changes treatment at its own position inside the line rather than
stating the whole line's.
Qualitative font comparisons preserve direction and any supplied `やや` or
`ひとまわり` qualifier, without a numbered size level. TEI records these as
`font-size qualitative(smaller)` or `font-size qualitative(larger)` and an
optional `qualifier(...)`; explicit bold weight and gothic typeface remain
separate attributes.
Bare `下付き` preserves lowered placement without small type, while `指数`
preserves the exponent role without identifying a mathematical base. TEI marks
the original target glyphs with `baseline-lowered` or `exponent`; plaintext and
Markdown retain those glyphs unchanged. Markdown uses inline HTML with inherited
font size for the placement; explicit `下付き小文字` remains distinct.
Presentation supplied on an indentation opener can end before the indentation.
Alignment supplied within a horizontal-writing clause ends with that clause;
an unclosed indentation remains explicit uncertainty.
Relative placement keeps its source anchor and supplied character offset separately
from absolute indentation. TEI links the placed scope to the anchor's source-span
record with `corresp`, records `placement-below`, `anchor-kind(text)` or
`anchor-kind(horizontal-block)`, and any `anchor-offset-chars(N)`
in `rend`, and preserves supplied writing direction and alignment. A missing or
multiply matching literal anchor remains unresolved. A vertical label below an
established horizontal block ends that horizontal presentation without ending
independent surrounding indentation.

Validation provenance binds the ODD, RNG and Schematron hashes through
`schemas/tei-profile-generation.json`. See [TEI validation](tei-validation.md).
