# Cross-Adapter AAT Oracle Summary

Report JSON: `/db/ab-validator/aat-fidelity/cross-adapter/report.json`
Oracle cases: `data/aat-oracle-cases.toml`

## Adapter Totals

| adapter | cases | schema pass | upstream faithful | oracle pass | oracle fail |
| --- | --- | --- | --- | --- | --- |
| aozora-rs | 46 | 46 | 46 | 46 | 0 |
| aozora2 | 46 | 46 | 46 | 46 | 0 |
| aozora2html | 46 | 46 | 46 | 46 | 0 |

## Failure Buckets

| adapter | bucket | count |
| --- | --- | --- |

## Failure Families

| adapter | case family | failures |
| --- | --- | --- |

## XHTML Source Evidence

XHTML DuckDB: `/db/ab-validator/aat-fidelity/cross-adapter/fidelity.duckdb`
XHTML report id: `upstream-xhtml-full`

| metric | value |
| --- | --- |
| both_missing_main_text | 1 |
| local_adapter_error | 58 |
| main_text_equal | 8910 |
| main_text_mismatch | 2179 |
| raw_equal | 6321 |
| rendered-body proxy eligible | 15231 |
| total observations | 17601 |
| upstream_missing_main_text | 132 |

- `aozora2html` source-level oracle failures should be interpreted beside rendered-XHTML evidence: `raw_equal` and `main_text_equal` rows support rendered-body proxy claims, while `main_text_mismatch`, adapter errors, and missing-main-text rows require separate triage.

## Syntax Row Coverage

| syntax row | adapter | pass | fail |
| --- | --- | --- | --- |
| accent.diacritic | aozora-rs | 1 | 0 |
| accent.diacritic | aozora2 | 1 | 0 |
| accent.diacritic | aozora2html | 1 | 0 |
| annotation.bouki | aozora-rs | 1 | 0 |
| annotation.bouki | aozora2 | 1 | 0 |
| annotation.bouki | aozora2html | 1 | 0 |
| annotation.chuuki | aozora-rs | 1 | 0 |
| annotation.chuuki | aozora2 | 1 | 0 |
| annotation.chuuki | aozora2html | 1 | 0 |
| break.line_explicit | aozora-rs | 1 | 0 |
| break.line_explicit | aozora2 | 1 | 0 |
| break.line_explicit | aozora2html | 1 | 0 |
| break.page_line | aozora-rs | 1 | 0 |
| break.page_line | aozora2 | 1 | 0 |
| break.page_line | aozora2html | 1 | 0 |
| caption.block | aozora-rs | 1 | 0 |
| caption.block | aozora2 | 1 | 0 |
| caption.block | aozora2html | 1 | 0 |
| caption.inline | aozora-rs | 1 | 0 |
| caption.inline | aozora2 | 1 | 0 |
| caption.inline | aozora2html | 1 | 0 |
| decoration.bold_italic | aozora-rs | 2 | 0 |
| decoration.bold_italic | aozora2 | 2 | 0 |
| decoration.bold_italic | aozora2html | 2 | 0 |
| decoration.boten | aozora-rs | 1 | 0 |
| decoration.boten | aozora2 | 1 | 0 |
| decoration.boten | aozora2html | 1 | 0 |
| decoration.bousen | aozora-rs | 1 | 0 |
| decoration.bousen | aozora2 | 1 | 0 |
| decoration.bousen | aozora2html | 1 | 0 |
| decoration.direction_override | aozora-rs | 1 | 0 |
| decoration.direction_override | aozora2 | 1 | 0 |
| decoration.direction_override | aozora2html | 1 | 0 |
| decoration.font_size | aozora-rs | 2 | 0 |
| decoration.font_size | aozora2 | 2 | 0 |
| decoration.font_size | aozora2html | 2 | 0 |
| decoration.keigakomi | aozora-rs | 1 | 0 |
| decoration.keigakomi | aozora2 | 1 | 0 |
| decoration.keigakomi | aozora2html | 1 | 0 |
| editor_note.unmapped | aozora-rs | 1 | 0 |
| editor_note.unmapped | aozora2 | 1 | 0 |
| editor_note.unmapped | aozora2html | 1 | 0 |
| emphasis.basic | aozora-rs | 1 | 0 |
| emphasis.basic | aozora2 | 1 | 0 |
| emphasis.basic | aozora2html | 1 | 0 |
| figure.image_caption | aozora-rs | 1 | 0 |
| figure.image_caption | aozora2 | 1 | 0 |
| figure.image_caption | aozora2html | 1 | 0 |
| figure.image_inline | aozora-rs | 1 | 0 |
| figure.image_inline | aozora2 | 1 | 0 |
| figure.image_inline | aozora2html | 1 | 0 |
| gaiji.dakuten_katakana | aozora-rs | 1 | 0 |
| gaiji.dakuten_katakana | aozora2 | 1 | 0 |
| gaiji.dakuten_katakana | aozora2html | 1 | 0 |
| gaiji.jis_code | aozora-rs | 1 | 0 |
| gaiji.jis_code | aozora2 | 1 | 0 |
| gaiji.jis_code | aozora2html | 1 | 0 |
| gaiji.marker | aozora-rs | 2 | 0 |
| gaiji.marker | aozora2 | 2 | 0 |
| gaiji.marker | aozora2html | 2 | 0 |
| gaiji.un_embed | aozora-rs | 1 | 0 |
| gaiji.un_embed | aozora2 | 1 | 0 |
| gaiji.un_embed | aozora2html | 1 | 0 |
| gaiji.unicode_codepoint | aozora-rs | 1 | 0 |
| gaiji.unicode_codepoint | aozora2 | 1 | 0 |
| gaiji.unicode_codepoint | aozora2html | 1 | 0 |
| gaiji_ruby.inline_base | aozora-rs | 1 | 0 |
| gaiji_ruby.inline_base | aozora2 | 1 | 0 |
| gaiji_ruby.inline_base | aozora2html | 1 | 0 |
| heading.basic | aozora-rs | 1 | 0 |
| heading.basic | aozora2 | 1 | 0 |
| heading.basic | aozora2html | 1 | 0 |
| heading.dogyo | aozora-rs | 1 | 0 |
| heading.dogyo | aozora2 | 1 | 0 |
| heading.dogyo | aozora2html | 1 | 0 |
| heading.inline_form | aozora-rs | 1 | 0 |
| heading.inline_form | aozora2 | 1 | 0 |
| heading.inline_form | aozora2html | 1 | 0 |
| heading.mado | aozora-rs | 1 | 0 |
| heading.mado | aozora2 | 1 | 0 |
| heading.mado | aozora2html | 1 | 0 |
| indentation.basic | aozora-rs | 1 | 0 |
| indentation.basic | aozora2 | 1 | 0 |
| indentation.basic | aozora2html | 1 | 0 |
| indentation.burasage | aozora-rs | 1 | 0 |
| indentation.burasage | aozora2 | 1 | 0 |
| indentation.burasage | aozora2html | 1 | 0 |
| indentation.chitsuki | aozora-rs | 1 | 0 |
| indentation.chitsuki | aozora2 | 1 | 0 |
| indentation.chitsuki | aozora2html | 1 | 0 |
| indentation.jisage_block | aozora-rs | 1 | 0 |
| indentation.jisage_block | aozora2 | 1 | 0 |
| indentation.jisage_block | aozora2html | 1 | 0 |
| indentation.jisage_oneline | aozora-rs | 1 | 0 |
| indentation.jisage_oneline | aozora2 | 1 | 0 |
| indentation.jisage_oneline | aozora2html | 1 | 0 |
| indentation.jizume | aozora-rs | 1 | 0 |
| indentation.jizume | aozora2 | 1 | 0 |
| indentation.jizume | aozora2html | 1 | 0 |
| iteration.kunoji | aozora-rs | 1 | 0 |
| iteration.kunoji | aozora2 | 1 | 0 |
| iteration.kunoji | aozora2html | 1 | 0 |
| kunten.kaeriten | aozora-rs | 1 | 0 |
| kunten.kaeriten | aozora2 | 1 | 0 |
| kunten.kaeriten | aozora2html | 1 | 0 |
| kunten.okurigana | aozora-rs | 1 | 0 |
| kunten.okurigana | aozora2 | 1 | 0 |
| kunten.okurigana | aozora2html | 1 | 0 |
| layout.tcy | aozora-rs | 2 | 0 |
| layout.tcy | aozora2 | 2 | 0 |
| layout.tcy | aozora2html | 2 | 0 |
| layout.yokogumi | aozora-rs | 1 | 0 |
| layout.yokogumi | aozora2 | 1 | 0 |
| layout.yokogumi | aozora2html | 1 | 0 |
| reference.frontref | aozora-rs | 1 | 0 |
| reference.frontref | aozora2 | 1 | 0 |
| reference.frontref | aozora2html | 1 | 0 |
| ruby.basic | aozora-rs | 2 | 0 |
| ruby.basic | aozora2 | 2 | 0 |
| ruby.basic | aozora2html | 2 | 0 |
| ruby.double | aozora-rs | 1 | 0 |
| ruby.double | aozora2 | 1 | 0 |
| ruby.double | aozora2html | 1 | 0 |
| ruby.nested_forbidden | aozora-rs | 1 | 0 |
| ruby.nested_forbidden | aozora2 | 1 | 0 |
| ruby.nested_forbidden | aozora2html | 1 | 0 |
| ruby.placement_directional | aozora-rs | 1 | 0 |
| ruby.placement_directional | aozora2 | 1 | 0 |
| ruby.placement_directional | aozora2html | 1 | 0 |
| warichu.basic | aozora-rs | 1 | 0 |
| warichu.basic | aozora2 | 1 | 0 |
| warichu.basic | aozora2html | 1 | 0 |
| warigaki.parenthetical | aozora-rs | 1 | 0 |
| warigaki.parenthetical | aozora2 | 1 | 0 |
| warigaki.parenthetical | aozora2html | 1 | 0 |

## Triage Notes

- `aozora-rs` passes schema, upstream-observation, and oracle axes for all reviewed cases.
- `aozora2` passes schema, upstream-observation, and oracle axes for all reviewed cases.
- `aozora2html` passes schema, upstream-observation, and oracle axes for all reviewed cases.
- Next implementation work should target one remaining adapter/family at a time, using these observations as the pre-fix upstream contract.
