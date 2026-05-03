# Cross-Adapter AAT Oracle Summary

Report JSON: `/db/ab-validator/aat-fidelity/cross-adapter/report.json`
Oracle cases: `data/aat-oracle-cases.toml`

## Adapter Totals

| adapter | cases | schema pass | upstream faithful | oracle pass | oracle fail |
| --- | --- | --- | --- | --- | --- |
| aozora-rs | 43 | 43 | 39 | 4 | 39 |
| aozora2 | 43 | 43 | 2 | 43 | 0 |
| aozora2html | 43 | 43 | 42 | 1 | 42 |

## Failure Buckets

| adapter | bucket | count |
| --- | --- | --- |
| aozora-rs | faithful-upstream-vs-oracle | 39 |
| aozora2html | faithful-rendered-output-vs-oracle | 42 |

## Failure Families

| adapter | case family | failures |
| --- | --- | --- |
| aozora-rs | accent | 1 |
| aozora-rs | annotation | 2 |
| aozora-rs | break | 2 |
| aozora-rs | caption | 2 |
| aozora-rs | decoration | 5 |
| aozora-rs | editor_note | 1 |
| aozora-rs | figure | 2 |
| aozora-rs | gaiji | 3 |
| aozora-rs | heading | 4 |
| aozora-rs | indentation | 5 |
| aozora-rs | iteration | 1 |
| aozora-rs | kunten | 2 |
| aozora-rs | layout | 2 |
| aozora-rs | reference | 1 |
| aozora-rs | ruby | 4 |
| aozora-rs | warichu | 1 |
| aozora-rs | warigaki | 1 |
| aozora2html | accent | 1 |
| aozora2html | annotation | 2 |
| aozora2html | break | 2 |
| aozora2html | caption | 2 |
| aozora2html | decoration | 6 |
| aozora2html | editor_note | 1 |
| aozora2html | emphasis | 1 |
| aozora2html | figure | 2 |
| aozora2html | gaiji | 4 |
| aozora2html | heading | 4 |
| aozora2html | indentation | 5 |
| aozora2html | iteration | 1 |
| aozora2html | kunten | 2 |
| aozora2html | layout | 2 |
| aozora2html | reference | 1 |
| aozora2html | ruby | 4 |
| aozora2html | warichu | 1 |
| aozora2html | warigaki | 1 |

## Syntax Row Coverage

| syntax row | adapter | pass | fail |
| --- | --- | --- | --- |
| accent.diacritic | aozora-rs | 0 | 1 |
| accent.diacritic | aozora2 | 1 | 0 |
| accent.diacritic | aozora2html | 0 | 1 |
| annotation.bouki | aozora-rs | 0 | 1 |
| annotation.bouki | aozora2 | 1 | 0 |
| annotation.bouki | aozora2html | 0 | 1 |
| annotation.chuuki | aozora-rs | 0 | 1 |
| annotation.chuuki | aozora2 | 1 | 0 |
| annotation.chuuki | aozora2html | 0 | 1 |
| break.line_explicit | aozora-rs | 0 | 1 |
| break.line_explicit | aozora2 | 1 | 0 |
| break.line_explicit | aozora2html | 0 | 1 |
| break.page_line | aozora-rs | 0 | 1 |
| break.page_line | aozora2 | 1 | 0 |
| break.page_line | aozora2html | 0 | 1 |
| caption.block | aozora-rs | 0 | 1 |
| caption.block | aozora2 | 1 | 0 |
| caption.block | aozora2html | 0 | 1 |
| caption.inline | aozora-rs | 0 | 1 |
| caption.inline | aozora2 | 1 | 0 |
| caption.inline | aozora2html | 0 | 1 |
| decoration.bold_italic | aozora-rs | 1 | 0 |
| decoration.bold_italic | aozora2 | 1 | 0 |
| decoration.bold_italic | aozora2html | 0 | 1 |
| decoration.boten | aozora-rs | 0 | 1 |
| decoration.boten | aozora2 | 1 | 0 |
| decoration.boten | aozora2html | 0 | 1 |
| decoration.bousen | aozora-rs | 0 | 1 |
| decoration.bousen | aozora2 | 1 | 0 |
| decoration.bousen | aozora2html | 0 | 1 |
| decoration.direction_override | aozora-rs | 0 | 1 |
| decoration.direction_override | aozora2 | 1 | 0 |
| decoration.direction_override | aozora2html | 0 | 1 |
| decoration.font_size | aozora-rs | 0 | 1 |
| decoration.font_size | aozora2 | 1 | 0 |
| decoration.font_size | aozora2html | 0 | 1 |
| decoration.keigakomi | aozora-rs | 0 | 1 |
| decoration.keigakomi | aozora2 | 1 | 0 |
| decoration.keigakomi | aozora2html | 0 | 1 |
| editor_note.unmapped | aozora-rs | 0 | 1 |
| editor_note.unmapped | aozora2 | 1 | 0 |
| editor_note.unmapped | aozora2html | 0 | 1 |
| emphasis.basic | aozora-rs | 1 | 0 |
| emphasis.basic | aozora2 | 1 | 0 |
| emphasis.basic | aozora2html | 0 | 1 |
| figure.image_caption | aozora-rs | 0 | 1 |
| figure.image_caption | aozora2 | 1 | 0 |
| figure.image_caption | aozora2html | 0 | 1 |
| figure.image_inline | aozora-rs | 0 | 1 |
| figure.image_inline | aozora2 | 1 | 0 |
| figure.image_inline | aozora2html | 0 | 1 |
| gaiji.dakuten_katakana | aozora-rs | 0 | 1 |
| gaiji.dakuten_katakana | aozora2 | 1 | 0 |
| gaiji.dakuten_katakana | aozora2html | 0 | 1 |
| gaiji.jis_code | aozora-rs | 0 | 1 |
| gaiji.jis_code | aozora2 | 1 | 0 |
| gaiji.jis_code | aozora2html | 0 | 1 |
| gaiji.marker | aozora-rs | 1 | 1 |
| gaiji.marker | aozora2 | 2 | 0 |
| gaiji.marker | aozora2html | 0 | 2 |
| gaiji.un_embed | aozora-rs | 0 | 1 |
| gaiji.un_embed | aozora2 | 1 | 0 |
| gaiji.un_embed | aozora2html | 0 | 1 |
| gaiji.unicode_codepoint | aozora-rs | 1 | 0 |
| gaiji.unicode_codepoint | aozora2 | 1 | 0 |
| gaiji.unicode_codepoint | aozora2html | 0 | 1 |
| gaiji_ruby.inline_base | aozora-rs | 0 | 1 |
| gaiji_ruby.inline_base | aozora2 | 1 | 0 |
| gaiji_ruby.inline_base | aozora2html | 0 | 1 |
| heading.basic | aozora-rs | 0 | 1 |
| heading.basic | aozora2 | 1 | 0 |
| heading.basic | aozora2html | 0 | 1 |
| heading.dogyo | aozora-rs | 0 | 1 |
| heading.dogyo | aozora2 | 1 | 0 |
| heading.dogyo | aozora2html | 0 | 1 |
| heading.inline_form | aozora-rs | 0 | 1 |
| heading.inline_form | aozora2 | 1 | 0 |
| heading.inline_form | aozora2html | 0 | 1 |
| heading.mado | aozora-rs | 0 | 1 |
| heading.mado | aozora2 | 1 | 0 |
| heading.mado | aozora2html | 0 | 1 |
| indentation.basic | aozora-rs | 0 | 1 |
| indentation.basic | aozora2 | 1 | 0 |
| indentation.basic | aozora2html | 0 | 1 |
| indentation.burasage | aozora-rs | 0 | 1 |
| indentation.burasage | aozora2 | 1 | 0 |
| indentation.burasage | aozora2html | 0 | 1 |
| indentation.chitsuki | aozora-rs | 0 | 1 |
| indentation.chitsuki | aozora2 | 1 | 0 |
| indentation.chitsuki | aozora2html | 0 | 1 |
| indentation.jisage_block | aozora-rs | 0 | 1 |
| indentation.jisage_block | aozora2 | 1 | 0 |
| indentation.jisage_block | aozora2html | 0 | 1 |
| indentation.jisage_oneline | aozora-rs | 0 | 1 |
| indentation.jisage_oneline | aozora2 | 1 | 0 |
| indentation.jisage_oneline | aozora2html | 0 | 1 |
| indentation.jizume | aozora-rs | 0 | 1 |
| indentation.jizume | aozora2 | 1 | 0 |
| indentation.jizume | aozora2html | 0 | 1 |
| iteration.kunoji | aozora-rs | 0 | 1 |
| iteration.kunoji | aozora2 | 1 | 0 |
| iteration.kunoji | aozora2html | 0 | 1 |
| kunten.kaeriten | aozora-rs | 0 | 1 |
| kunten.kaeriten | aozora2 | 1 | 0 |
| kunten.kaeriten | aozora2html | 0 | 1 |
| kunten.okurigana | aozora-rs | 0 | 1 |
| kunten.okurigana | aozora2 | 1 | 0 |
| kunten.okurigana | aozora2html | 0 | 1 |
| layout.tcy | aozora-rs | 0 | 1 |
| layout.tcy | aozora2 | 1 | 0 |
| layout.tcy | aozora2html | 0 | 1 |
| layout.yokogumi | aozora-rs | 0 | 1 |
| layout.yokogumi | aozora2 | 1 | 0 |
| layout.yokogumi | aozora2html | 0 | 1 |
| reference.frontref | aozora-rs | 0 | 1 |
| reference.frontref | aozora2 | 1 | 0 |
| reference.frontref | aozora2html | 0 | 1 |
| ruby.basic | aozora-rs | 1 | 1 |
| ruby.basic | aozora2 | 2 | 0 |
| ruby.basic | aozora2html | 1 | 1 |
| ruby.double | aozora-rs | 0 | 1 |
| ruby.double | aozora2 | 1 | 0 |
| ruby.double | aozora2html | 0 | 1 |
| ruby.nested_forbidden | aozora-rs | 0 | 1 |
| ruby.nested_forbidden | aozora2 | 1 | 0 |
| ruby.nested_forbidden | aozora2html | 0 | 1 |
| ruby.placement_directional | aozora-rs | 0 | 1 |
| ruby.placement_directional | aozora2 | 1 | 0 |
| ruby.placement_directional | aozora2html | 0 | 1 |
| warichu.basic | aozora-rs | 0 | 1 |
| warichu.basic | aozora2 | 1 | 0 |
| warichu.basic | aozora2html | 0 | 1 |
| warigaki.parenthetical | aozora-rs | 0 | 1 |
| warigaki.parenthetical | aozora2 | 1 | 0 |
| warigaki.parenthetical | aozora2html | 0 | 1 |

## Triage Notes

- `aozora2` is the current oracle baseline for the reviewed AAT cases.
- `aozora-rs` now has upstream observations for every reviewed oracle failure. Its remaining failures are faithful-to-observed-output vs oracle-correctness divergences.
- `aozora2html` now has rendered-output observations for every reviewed oracle failure. Its remaining failures are faithful-to-XHTML vs source-level oracle divergences.
- The next implementation work should target one adapter/family at a time, using these observations as the pre-fix upstream contract.
