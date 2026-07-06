# Source Reference Reconciliation

Verdict: `SOURCE_REFERENCE_RECONCILIATION_COMPLETE`

## Totals

| Metric | Count |
| --- | ---: |
| `syntax_rows` | `56` |
| `official_syntax_rows` | `16` |
| `source_inventory_rows` | `50` |
| `documented_observed` | `15` |
| `documented_unobserved` | `1` |
| `observed_without_syntax_row` | `0` |
| `p4suta_features` | `24` |
| `p4suta_feature_mapped` | `20` |
| `p4suta_feature_unmapped` | `0` |
| `p4suta_feature_unobserved` | `0` |
| `p4suta_feature_comparison_only` | `4` |

## Review Items

### Observed Source Rows Without Syntax Rows

| source_inventory_row | works | occurrences | sample_works |
| --- | --- | --- | --- |

### P4suta Features Without Local Mapping

| feature | vectors | levels | rows |
| --- | --- | --- | --- |

### Official Rows Documented But Unobserved

| syntax_id | source_inventory_row | status | references |
| --- | --- | --- | --- |
| ruby.double | None | None | references/aozorabunko/rules/kijyunn.html, references/parsers/aozora2html/lib/aozora2html/tag/ruby.rb |

## Mapped P4suta Features

| feature | syntax_rows | source_inventory_rows | occurrences |
| --- | --- | --- | --- |
| accent | accent.dotted_letter | accent.dotted_letter | 93 |
| annotation | annotation.bouki, annotation.chuuki, annotation.layout_note, glyph.variant_note, source.note_label, source.page_reference, source.reviewed_residual_command | annotation.bouki, annotation.chuuki, annotation.layout_note, glyph.variant_note, source.note_label, source.page_reference, source.reviewed_residual_command | 45193 |
| bouten | decoration.boten | decoration.boten | 129086 |
| break | break.line_explicit, break.page_line | break.line_explicit, break.page_line | 9390 |
| container | indentation.basic, indentation.burasage, indentation.chitsuki, indentation.jisage_block, indentation.jisage_oneline, indentation.jizume, layout.center_page, structure.quote_block | indentation.basic, indentation.burasage, indentation.chitsuki, indentation.jisage_block, indentation.jisage_oneline, indentation.jizume, layout.center_page, structure.quote_block | 267446 |
| emphasis | emphasis.basic, decoration.bold_italic, decoration.bousen, decoration.font_size, decoration.typeface | decoration.bold_italic, decoration.bousen, decoration.font_size, decoration.typeface, emphasis.basic | 235965 |
| gaiji | gaiji.marker, gaiji.jis_code, gaiji.un_embed, gaiji.unicode_codepoint, gaiji_ruby.inline_base | gaiji.jis_code, gaiji.marker, gaiji.un_embed, gaiji.unicode_codepoint, gaiji_ruby.inline_base | 134985 |
| heading | heading.basic, heading.dogyo, heading.mado | heading.basic, heading.dogyo, heading.mado | 94111 |
| horizontal | layout.yokogumi | layout.yokogumi | 3690 |
| kaeriten | kunten.kaeriten | kunten.kaeriten | 27987 |
| keigakomi | decoration.keigakomi | decoration.keigakomi | 717 |
| kunten | kunten.kaeriten, kunten.okurigana | kunten.kaeriten, kunten.okurigana | 34549 |
| layout | indentation.basic, indentation.burasage, indentation.chitsuki, indentation.jisage_block, indentation.jisage_oneline, indentation.jizume, layout.center_page | indentation.basic, indentation.burasage, indentation.chitsuki, indentation.jisage_block, indentation.jisage_oneline, indentation.jizume, layout.center_page | 267425 |
| ruby | ruby.basic, ruby.placement_directional, gaiji_ruby.inline_base | gaiji_ruby.inline_base, ruby.basic, ruby.placement_directional | 3621694 |
| sashie | figure.image_caption, figure.image_inline | figure.image_caption, figure.image_inline | 7654 |
| structural-marker | reference.frontref, source.note_label | reference.frontref, source.note_label | 5302 |
| tables_columns | layout.multicolumn, structure.table | layout.multicolumn, structure.table | 119 |
| tate_chu_yoko | layout.tcy | layout.tcy | 19794 |
| tcy | layout.tcy | layout.tcy | 19794 |
| warichu | warichu.basic | warichu.basic | 6605 |
