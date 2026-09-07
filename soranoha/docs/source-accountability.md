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
claims describe what the parser believes it understood; missing claims remain
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
not necessarily every aspect. Individual claims list their established aspects
and native source extent. A compatible ruby fact can contain its lexical reading marker, while an
enclosing emphasis fact cannot account for that ruby. Source apparatus is kept
separate. Missing facts and unclassified markers remain visible. Lossy decoding
prevents positive claim accounting. Reported interpretation problems retain their
original influence bounds alongside positive claims: a claim is not evidence that
an overlapping or document-wide problem has been resolved.

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

Supplied block geometry retains indentation, continuation indentation, line width,
and distance from the line end independently. Hanging indentation uses the
continuation padding and the difference between first and continuation indents.
TEI retains supplied line counts as `rend="line-count(N)"` and page placement as
`rend="page-center"`; page placement does not imply centered text alignment.
Consecutive indentation instructions replace the preceding indentation scope.
Page placement ends before the next supplied page break, which remains in TEI.

Validation provenance binds the ODD, RNG and Schematron hashes through
`schemas/tei-profile-generation.json`. See [TEI validation](tei-validation.md).
