# Aozora Syntax Coverage Report

Generated: 2026-04-28 from `0ac3ed0` against the full corpus.

- Corpus: 17 613 works (281 unreadable zips skipped, 2 aozora2html crashes excluded).
- Parsers compared: `aozora2`, `aozora-rs`, `aozora2html`.
- Syntax rows: 45.
- Wall-clock (warm cache, 32 workers): 6.4 s.
- Source data: [`data/aozora-syntax-coverage.toml`](../data/aozora-syntax-coverage.toml).
- Methodology: [`docs/superpowers/specs/2026-04-28-syntax-coverage-methodology.md`](superpowers/specs/2026-04-28-syntax-coverage-methodology.md).

## 1. Parser recognition matrix

What each parser does when it encounters this syntax.

Legend: `parsed` ✓ — `normalised` ≈ — `unrecognised` ✗ — `aborts` ‼ — `unknown` ?

| id | category | aozora2 | aozora-rs | aozora2html |
|---|---|---|---|---|
| `heading.basic` | block | ✓ | ✓ | ✓ |
| `heading.dogyo` | block | ✓ | ✗ | ✓ |
| `heading.inline_form` | block | ✓ | ✗ | ✓ |
| `heading.mado` | block | ✓ | ✗ | ✓ |
| `accent.diacritic` | glyph | ✓ | ✗ | ✓ |
| `gaiji.dakuten_katakana` | glyph | ✓ | ✗ | ✓ |
| `gaiji.jis_code` | glyph | ✓ | ✗ | ≈ |
| `gaiji.marker` | glyph | ✓ | ✗ | ≈ |
| `gaiji.un_embed` | glyph | ✓ | ✗ | ✓ |
| `gaiji.unicode_codepoint` | glyph | ✓ | ✗ | ≈ |
| `iteration.kunoji` | glyph | ✗ | ✗ | ✓ |
| `annotation.bouki` | inline_annotation | ✓ | ✗ | ✓ |
| `annotation.chuuki` | inline_annotation | ✓ | ✗ | ✓ |
| `decoration.bold_italic` | inline_annotation | ✓ | ✓ | ✓ |
| `decoration.boten` | inline_annotation | ✓ | ✓ | ✓ |
| `decoration.bousen` | inline_annotation | ✓ | ✓ | ✓ |
| `decoration.direction_override` | inline_annotation | ✓ | ✗ | ✓ |
| `decoration.font_size` | inline_annotation | ✓ | ✓ | ✓ |
| `decoration.keigakomi` | inline_annotation | ✓ | ✗ | ✓ |
| `emphasis.basic` | inline_annotation | ✓ | ✓ | ✓ |
| `gaiji_ruby.inline_base` | inline_annotation | ✓ | ✗ | ✓ |
| `kunten.kaeriten` | inline_annotation | ✓ | ✗ | ✓ |
| `kunten.okurigana` | inline_annotation | ✓ | ✗ | ✓ |
| `ruby.basic` | inline_annotation | ✓ | ✓ | ✓ |
| `ruby.double` | inline_annotation | ≈ | ≈ | ✓ |
| `ruby.nested_forbidden` | inline_annotation | ≈ | ≈ | ‼ |
| `ruby.placement_directional` | inline_annotation | ✓ | ✗ | ✓ |
| `warichu.basic` | inline_annotation | ✓ | ✓ | ✓ |
| `warigaki.parenthetical` | inline_annotation | ✓ | ✗ | ✓ |
| `indentation.basic` | layout | ✓ | ✓ | ✓ |
| `indentation.burasage` | layout | ✓ | ✓ | ✓ |
| `indentation.chitsuki` | layout | ✓ | ✓ | ✓ |
| `indentation.jisage_block` | layout | ✓ | ✓ | ✓ |
| `indentation.jisage_oneline` | layout | ✓ | ✗ | ✓ |
| `indentation.jizume` | layout | ✓ | ✗ | ✓ |
| `layout.tcy` | layout | ✓ | ✗ | ✓ |
| `layout.yokogumi` | layout | ✓ | ✗ | ✓ |
| `caption.block` | media | ✓ | ✗ | ✓ |
| `caption.inline` | media | ✓ | ✗ | ✓ |
| `figure.image_caption` | media | ✓ | ✓ | ✓ |
| `figure.image_inline` | media | ✓ | ✓ | ✓ |
| `break.line_explicit` | milestone | ✗ | ≈ | ✓ |
| `break.page_line` | milestone | ✗ | ✓ | ✓ |
| `editor_note.unmapped` | reference | ✓ | ≈ | ✓ |
| `reference.frontref` | reference | ≈ | ✗ | ✓ |

## 2. Adapter AAT fidelity matrix

What survives in each adapter's AAT JSON.

Legend: `preserved` ◉ — `lossy` ◐ — `dropped` ◌ — `synthesised` ✱ — `not_applicable` — — `unknown` ?

| id | aozora2 | aozora-rs | aozora2html |
|---|---|---|---|
| `heading.basic` | ◌ | ◉ | ◉ |
| `heading.dogyo` | ◌ | — | ◉ |
| `heading.inline_form` | ◌ | — | ◉ |
| `heading.mado` | ◌ | — | ◉ |
| `accent.diacritic` | ◌ | — | ◐ |
| `gaiji.dakuten_katakana` | ◌ | ✱ | ◌ |
| `gaiji.jis_code` | ◐ | ✱ | ◐ |
| `gaiji.marker` | ◉ | ✱ | ◌ |
| `gaiji.un_embed` | ◌ | ✱ | ◌ |
| `gaiji.unicode_codepoint` | ◉ | ✱ | ◉ |
| `iteration.kunoji` | — | — | ◌ |
| `annotation.bouki` | ◌ | — | ◌ |
| `annotation.chuuki` | ◌ | — | ◌ |
| `decoration.bold_italic` | ◌ | ◉ | ◉ |
| `decoration.boten` | ◌ | ◐ | ◉ |
| `decoration.bousen` | ◌ | ◐ | ◉ |
| `decoration.direction_override` | ◌ | — | ◌ |
| `decoration.font_size` | ◌ | ◐ | ◉ |
| `decoration.keigakomi` | ◌ | — | ◐ |
| `emphasis.basic` | ◌ | ◉ | ◉ |
| `gaiji_ruby.inline_base` | ◐ | ✱ | ◉ |
| `kunten.kaeriten` | ◌ | — | ◌ |
| `kunten.okurigana` | ◌ | — | ◌ |
| `ruby.basic` | ◉ | ◉ | ◉ |
| `ruby.double` | ◐ | ◐ | ◌ |
| `ruby.nested_forbidden` | — | ◌ | — |
| `ruby.placement_directional` | ◌ | — | ◉ |
| `warichu.basic` | ◌ | ◉ | ◐ |
| `warigaki.parenthetical` | ◌ | — | ◐ |
| `indentation.basic` | ◌ | ◐ | ◉ |
| `indentation.burasage` | ◌ | ◐ | ◌ |
| `indentation.chitsuki` | ◌ | ◉ | ◉ |
| `indentation.jisage_block` | ◌ | ◐ | ◉ |
| `indentation.jisage_oneline` | ◌ | — | ◉ |
| `indentation.jizume` | ◌ | — | ◌ |
| `layout.tcy` | ◌ | — | ◌ |
| `layout.yokogumi` | ◌ | — | ◉ |
| `caption.block` | ◌ | — | ◐ |
| `caption.inline` | ◌ | — | ◐ |
| `figure.image_caption` | ◌ | ◐ | ◐ |
| `figure.image_inline` | ◌ | ◐ | ◐ |
| `break.line_explicit` | — | ◌ | ◉ |
| `break.page_line` | — | ◌ | ◉ |
| `editor_note.unmapped` | ◌ | ◌ | ◌ |
| `reference.frontref` | — | — | ◌ |

## 3. Corpus prevalence (max across parsers)

Sorted by `works_with_feature` descending. `works_with_feature` counts distinct works exhibiting the feature; `total_occurrences` sums all hits.

| id | works | total | top samples |
|---|---:|---:|---|
| `ruby.basic` | 14 190 | 10 493 869 | `001529_50685`, `000118_1745`, `000148_56923` |
| `editor_note.unmapped` | 13 834 | 682 387 | `001670_55342`, `001025_50909`, `001185_45210` |
| `indentation.basic` | 8 084 | 133 045 | `001025_50909`, `000294_1858`, `001670_55342` |
| `gaiji.marker` | 6 715 | 189 770 | `JISTABLE`, `000311_2012`, `000129_2084` |
| `indentation.jisage_block` | 6 714 | 165 543 | `000294_1858`, `001310_51839`, `001025_50909` |
| `emphasis.basic` | 6 467 | 310 208 | `000281_3597`, `001161_43624`, `001670_55342` |
| `decoration.boten` | 6 201 | 128 569 | `001161_43624`, `000305_43619`, `000048_48803` |
| `gaiji.jis_code` | 5 662 | 116 905 | `000129_2084`, `001562_52417`, `001562_56146` |
| `gaiji.un_embed` | 5 515 | 109 961 | `000311_2012`, `001161_43624`, `001161_48122` |
| `figure.image_inline` | 5 459 | 58 586 | `001764_55990`, `000866_3039`, `001185_45210` |
| `gaiji_ruby.inline_base` | 4 689 | 46 471 | `000129_2084`, `001930_58400`, `001562_52417` |
| `heading.basic` | 3 643 | 76 796 | `001562_33224`, `001097_43785`, `000286_49178` |
| `iteration.kunoji` | 3 601 | 80 988 | `001529_50685`, `000118_1745`, `000989_350` |
| `indentation.chitsuki` | 2 172 | 7 931 | `001025_50909`, `000740_49078`, `000150_46615` |
| `break.page_line` | 826 | 7 804 | `000042_1684`, `001529_546`, `001670_55342` |
| `decoration.font_size` | 762 | 11 561 | `001977_60827`, `002168_51895`, `001569_56726` |
| `reference.frontref` | 757 | 3 956 | `001404_49966`, `000076_45641`, `000146_44913` |
| `indentation.burasage` | 718 | 5 233 | `001099_46996`, `001097_43785`, `001402_49940` |
| `gaiji.unicode_codepoint` | 625 | 7 122 | `001021_4852`, `000129_2084`, `001930_58400` |
| `gaiji.dakuten_katakana` | 447 | 3 757 | `000311_2012`, `000207_47131`, `000320_2168` |
| `layout.yokogumi` | 407 | 1 842 | `000096_2110`, `000034_55507`, `000106_49617` |
| `warichu.basic` | 357 | 6 360 | `001930_58400`, `001094_42603`, `001930_58401` |
| `decoration.bousen` | 316 | 35 489 | `000019_4376`, `000933_47023`, `001191_55276` |
| `decoration.bold_italic` | 281 | 8 257 | `001784_56525`, `000748_46576`, `000035_52380` |
| `caption.inline` | 147 | 2 680 | `001574_52529`, `001054_18371`, `001404_49966` |
| `heading.dogyo` | 141 | 11 617 | `001402_49940`, `001566_58764`, `001341_51302` |
| `decoration.keigakomi` | 135 | 458 | `000160_45621`, `001095_42626`, `001779_58486` |
| `figure.image_caption` | 103 | 1 777 | `001764_55990`, `001185_45210`, `001574_52529` |
| `layout.tcy` | 90 | 542 | `000150_46616`, `000879_21`, `002265_62126` |
| `break.line_explicit` | 47 | 165 | `002240_61619`, `002177_60663`, `000866_3039` |
| `ruby.placement_directional` | 31 | 397 | `001930_58400`, `001930_58401`, `001395_49905` |
| `caption.block` | 29 | 620 | `001574_52529`, `001492_51195`, `001548_52232` |
| `annotation.bouki` | 12 | 127 | `000491_2698`, `000287_3062`, `000287_3061` |
| `heading.inline_form` | 11 | 20 | `001670_55342`, `001524_51918`, `001524_51919` |
| `annotation.chuuki` | 9 | 12 | `001266_51368`, `000908_51450`, `000146_49258` |
| `heading.mado` | 6 | 1 680 | `001404_49966`, `000296_1864`, `001520_54957` |
| `decoration.direction_override` | 6 | 65 | `001242_46444`, `000866_3039`, `001094_42603` |
| `indentation.jisage_oneline` | 1 | 2 | `000284_2227` |
| `warigaki.parenthetical` | 1 | 1 | `000034_519` |
| `ruby.double` | 0 | 0 |  |
| `ruby.nested_forbidden` | 0 | 0 |  |
| `accent.diacritic` | 0 | 0 |  |
| `kunten.kaeriten` | 0 | 0 |  |
| `kunten.okurigana` | 0 | 0 |  |
| `indentation.jizume` | 0 | 0 |  |

## 4. Per-parser headline counts

`works_with_feature` per row, side-by-side. Highlights where parsers disagree.

| id | aozora2 | aozora-rs | aozora2html |
|---|---:|---:|---:|
| `heading.basic` | 3 642 | 3 642 | 3 643 |
| `heading.dogyo` | 141 | 141 | 141 |
| `heading.inline_form` | 11 | 11 | 11 |
| `heading.mado` | 6 | 6 | 6 |
| `accent.diacritic` | 0 | 0 | 0 |
| `gaiji.dakuten_katakana` | 447 | 447 | 353 |
| `gaiji.jis_code` | 5 662 | 5 662 | 5 662 |
| `gaiji.marker` | 6 583 | 6 651 | 6 715 |
| `gaiji.un_embed` | 5 386 | 5 515 | 5 203 |
| `gaiji.unicode_codepoint` | 625 | 625 | 625 |
| `iteration.kunoji` | 3 601 | 3 601 | 3 601 |
| `annotation.bouki` | 12 | 12 | 12 |
| `annotation.chuuki` | 9 | 9 | 9 |
| `decoration.bold_italic` | 55 | 92 | 281 |
| `decoration.boten` | 7 | 2 757 | 6 201 |
| `decoration.bousen` | 313 | 313 | 316 |
| `decoration.direction_override` | 6 | 6 | 6 |
| `decoration.font_size` | 0 | 159 | 762 |
| `decoration.keigakomi` | 106 | 106 | 135 |
| `emphasis.basic` | 6 458 | 6 461 | 6 467 |
| `gaiji_ruby.inline_base` | 4 689 | 2 940 | 2 725 |
| `kunten.kaeriten` | 0 | 0 | 0 |
| `kunten.okurigana` | 0 | 0 | 0 |
| `ruby.basic` | 14 055 | 14 132 | 14 190 |
| `ruby.double` | 0 | 0 | 0 |
| `ruby.nested_forbidden` | 0 | 0 | 0 |
| `ruby.placement_directional` | 31 | 31 | 31 |
| `warichu.basic` | 357 | 357 | 357 |
| `warigaki.parenthetical` | 1 | 1 | 1 |
| `indentation.basic` | 8 084 | 8 084 | 8 084 |
| `indentation.burasage` | 718 | 718 | 718 |
| `indentation.chitsuki` | 2 172 | 2 172 | 2 172 |
| `indentation.jisage_block` | 4 049 | 4 049 | 6 714 |
| `indentation.jisage_oneline` | 1 | 1 | 1 |
| `indentation.jizume` | 0 | 0 | 0 |
| `layout.tcy` | 1 | 90 | 1 |
| `layout.yokogumi` | 87 | 87 | 407 |
| `caption.block` | 29 | 29 | 29 |
| `caption.inline` | 137 | 137 | 147 |
| `figure.image_caption` | 103 | 103 | 103 |
| `figure.image_inline` | 499 | 499 | 5 459 |
| `break.line_explicit` | 47 | 47 | 47 |
| `break.page_line` | 826 | 826 | 826 |
| `editor_note.unmapped` | 13 834 | 13 834 | 13 834 |
| `reference.frontref` | 757 | 757 | 757 |

## 5. Cross-parser disagreements (recognition only)

Rows where the three parsers do not agree on `recognition`.

| id | aozora2 | aozora-rs | aozora2html |
|---|---|---|---|
| `accent.diacritic` | parsed | unrecognised | parsed |
| `annotation.bouki` | parsed | unrecognised | parsed |
| `annotation.chuuki` | parsed | unrecognised | parsed |
| `break.line_explicit` | unrecognised | normalised | parsed |
| `break.page_line` | unrecognised | parsed | parsed |
| `caption.block` | parsed | unrecognised | parsed |
| `caption.inline` | parsed | unrecognised | parsed |
| `decoration.direction_override` | parsed | unrecognised | parsed |
| `decoration.keigakomi` | parsed | unrecognised | parsed |
| `editor_note.unmapped` | parsed | normalised | parsed |
| `gaiji.dakuten_katakana` | parsed | unrecognised | parsed |
| `gaiji.jis_code` | parsed | unrecognised | normalised |
| `gaiji.marker` | parsed | unrecognised | normalised |
| `gaiji.un_embed` | parsed | unrecognised | parsed |
| `gaiji.unicode_codepoint` | parsed | unrecognised | normalised |
| `gaiji_ruby.inline_base` | parsed | unrecognised | parsed |
| `heading.dogyo` | parsed | unrecognised | parsed |
| `heading.inline_form` | parsed | unrecognised | parsed |
| `heading.mado` | parsed | unrecognised | parsed |
| `indentation.jisage_oneline` | parsed | unrecognised | parsed |
| `indentation.jizume` | parsed | unrecognised | parsed |
| `iteration.kunoji` | unrecognised | unrecognised | parsed |
| `kunten.kaeriten` | parsed | unrecognised | parsed |
| `kunten.okurigana` | parsed | unrecognised | parsed |
| `layout.tcy` | parsed | unrecognised | parsed |
| `layout.yokogumi` | parsed | unrecognised | parsed |
| `reference.frontref` | normalised | unrecognised | parsed |
| `ruby.double` | normalised | normalised | parsed |
| `ruby.nested_forbidden` | normalised | normalised | aborts |
| `ruby.placement_directional` | parsed | unrecognised | parsed |
| `warigaki.parenthetical` | parsed | unrecognised | parsed |

---

_Regenerate: re-run `AB_COV_OUT=scratch/ab-coverage-tuned AB_COV_MERGE=1 bash benchmarks/run-coverage.sh`, then run this script._
