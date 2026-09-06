# Source fidelity and review exports

Soranoha builds TEI and visible-body plaintext as the research outputs. TEI retains ruby, gaiji declarations, headings, layout and source apparatus. Plaintext omits readings and source apparatus. The rights snapshot describes admission evidence; TEI schema validation checks structural validity; the separate source-fidelity report compares exports with the raw Aozora primary text.

From the repository root, set the three variables below to absolute paths. The corpus must be a clean Git checkout; the export directory must not exist:

```sh
nix run .#soranoha-kernel -- build \
  --root "$BUILD_STORE" \
  --aozora-root "$CORPUS_CHECKOUT" \
  --assets-root "$PWD/abc" \
  --out "$EXPORT_DIR"
```

Each work gets a directory named by its catalog slug containing `tei.xml`, `plain.txt`, `tei-validation.json` and `source-fidelity.json`. A top-level `build.json` records artifact hashes and build results. These are regular files, independent of the computation cache. Omitting `--out` retains the existing cache-only build behavior. A build never publishes or accepts an assessment.

TEI encodes paragraph-leading fullwidth indentation as `style="text-indent: 1em"` (scaled to the source count), and heading or source-note continuation indentation as `padding-inline-start`. Its `styleDefDecl` declares CSS. Source-note lines use `seg` and `lb`. Interior spaces remain text. The XML serializer indents structural containers for inspection. It leaves paragraphs, sentences, headings, ruby, notes, other mixed content, and `xml:space="preserve"` subtrees unchanged; indentation cannot become part of their lexical text.

The fidelity checker independently reads the raw source, rather than treating parser IR as ground truth. It compares visible text, block order, middle headings and indentation, ruby base/reading pairs, gaiji, paragraph indentation, source-note lines and closing-date layout. The report binds the exact source, TEI and plaintext digests. `passed` covers only these listed comparisons; `failed` reports an observed mismatch; `not-evaluated` means the checker does not recognize the source encoding, boundaries or markup.

Coverage is deliberately bounded to separator-delimited Aozora prose with a 底本 colophon, basic ruby, plane-1 third-level JIS gaiji and the supported layout forms. Blank-line spacing, title/author metadata and colophon fields after 入力 are not certified. Unsupported sources must not be read as passing. The report is internal review evidence, not a new signed publication artifact or publication gate; the existing include-and-flag validation policy is unchanged.

The fidelity stage depends only on primary-text, TEI and plaintext bytes plus its own checker version and runtime. Renderer changes increment the render-stage version; checker changes increment the fidelity-stage version. Neither changes rights evidence or parsing. Parser and converter executable hashes identify their own implementations. Extend the checker when another source form needs a verified comparison, without coupling it to assessment rules or duplicating the parser schema.
