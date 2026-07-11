# Aozora Notation-Spec Comparison

- vectors_dir: `/home/bor/Projects/soranoha/.worktrees/parser-fork-phase4/ab-validator/reports/parser-conformance/official-docs-seed`
- rows: 210

| vector | feature | level | adapter | status | failures | warnings | skips |
| --- | --- | --- | --- | --- | --- | --- | --- |
| official_accent_decomposition | accent | should | aozora | warning |  | nodes: expected [{'kind': 'accent', 'span': {'start': 0, 'end': 16}}], got [] |  |
| official_accent_decomposition | accent | should | aozora-adapter | warning |  | nodes: kind sequence differs
      expected: ['accent']
      got:      ['unparsed-source-gap'] |  |
| official_accent_decomposition | accent | should | ab-aozora | warning |  | nodes: kind sequence differs
      expected: ['accent']
      got:      ['unparsed-source-gap'] |  |
| official_accent_decomposition | accent | should | aozora2 | warning |  | nodes: kind sequence differs
      expected: ['accent']
      got:      ['emphasis'] |  |
| official_accent_decomposition | accent | should | aozora2html | warning |  | nodes: kind sequence differs
      expected: ['accent']
      got:      ['gaiji'] |  |
| official_accent_decomposition | accent | should | aozora-rs | warning |  | nodes: kind sequence differs
      expected: ['accent']
      got:      ['emphasis'] |  |
| official_accent_decomposition | accent | should | aozora-epub3 | warning |  | nodes: kind sequence differs
      expected: ['accent']
      got:      [] |  |
| official_bold | emphasis | should | aozora | warning |  | nodes: expected [{'kind': 'emphasis', 'span': {'start': 0, 'end': 39}}], got [{'kind': 'emphasis', 'span': {'start': 0, 'end': 36}}] |  |
| official_bold | emphasis | should | aozora-adapter | pass |  |  |  |
| official_bold | emphasis | should | ab-aozora | pass |  |  |  |
| official_bold | emphasis | should | aozora2 | pass |  |  |  |
| official_bold | emphasis | should | aozora2html | pass |  |  |  |
| official_bold | emphasis | should | aozora-rs | pass |  |  |  |
| official_bold | emphasis | should | aozora-epub3 | warning |  | nodes: kind sequence differs
      expected: ['emphasis']
      got:      [] |  |
| official_bousen | bousen | should | aozora | warning |  | nodes: expected [{'kind': 'emphasis', 'span': {'start': 0, 'end': 75}}], got [{'kind': 'bouten', 'span': {'start': 12, 'end': 48}}] |  |
| official_bousen | bousen | should | aozora-adapter | warning |  | nodes: kind sequence differs
      expected: ['emphasis']
      got:      ['bouten'] |  |
| official_bousen | bousen | should | ab-aozora | warning |  | nodes: kind sequence differs
      expected: ['emphasis']
      got:      ['bouten'] |  |
| official_bousen | bousen | should | aozora2 | pass |  |  |  |
| official_bousen | bousen | should | aozora2html | pass |  |  |  |
| official_bousen | bousen | should | aozora-rs | warning |  | nodes: kind sequence differs
      expected: ['emphasis']
      got:      [] |  |
| official_bousen | bousen | should | aozora-epub3 | warning |  | nodes: kind sequence differs
      expected: ['emphasis']
      got:      [] |  |
| official_bousen_double | bousen | should | aozora | warning |  | nodes: expected [{'kind': 'emphasis', 'span': {'start': 0, 'end': 36}}], got [{'kind': 'bouten', 'span': {'start': 0, 'end': 36}}] |  |
| official_bousen_double | bousen | should | aozora-adapter | warning |  | nodes: kind sequence differs
      expected: ['emphasis']
      got:      ['bouten'] |  |
| official_bousen_double | bousen | should | ab-aozora | warning |  | nodes: kind sequence differs
      expected: ['emphasis']
      got:      ['bouten'] |  |
| official_bousen_double | bousen | should | aozora2 | pass |  |  |  |
| official_bousen_double | bousen | should | aozora2html | pass |  |  |  |
| official_bousen_double | bousen | should | aozora-rs | pass |  |  |  |
| official_bousen_double | bousen | should | aozora-epub3 | warning |  | nodes: kind sequence differs
      expected: ['emphasis']
      got:      [] |  |
| official_bousen_left | bousen | should | aozora | warning |  | nodes: expected [{'kind': 'emphasis', 'span': {'start': 0, 'end': 36}}], got [{'kind': 'bouten', 'span': {'start': 0, 'end': 36}}] |  |
| official_bousen_left | bousen | should | aozora-adapter | warning |  | nodes: kind sequence differs
      expected: ['emphasis']
      got:      ['bouten'] |  |
| official_bousen_left | bousen | should | ab-aozora | warning |  | nodes: kind sequence differs
      expected: ['emphasis']
      got:      ['bouten'] |  |
| official_bousen_left | bousen | should | aozora2 | pass |  |  |  |
| official_bousen_left | bousen | should | aozora2html | pass |  |  |  |
| official_bousen_left | bousen | should | aozora-rs | warning |  | nodes: kind sequence differs
      expected: ['emphasis']
      got:      [] |  |
| official_bousen_left | bousen | should | aozora-epub3 | warning |  | nodes: kind sequence differs
      expected: ['emphasis']
      got:      [] |  |
| official_bousen_wave | bousen | should | aozora | warning |  | nodes: expected [{'kind': 'emphasis', 'span': {'start': 0, 'end': 30}}], got [{'kind': 'bouten', 'span': {'start': 0, 'end': 30}}] |  |
| official_bousen_wave | bousen | should | aozora-adapter | warning |  | nodes: kind sequence differs
      expected: ['emphasis']
      got:      ['bouten'] |  |
| official_bousen_wave | bousen | should | ab-aozora | warning |  | nodes: kind sequence differs
      expected: ['emphasis']
      got:      ['bouten'] |  |
| official_bousen_wave | bousen | should | aozora2 | pass |  |  |  |
| official_bousen_wave | bousen | should | aozora2html | pass |  |  |  |
| official_bousen_wave | bousen | should | aozora-rs | warning |  | nodes: kind sequence differs
      expected: ['emphasis']
      got:      [] |  |
| official_bousen_wave | bousen | should | aozora-epub3 | warning |  | nodes: kind sequence differs
      expected: ['emphasis']
      got:      [] |  |
| official_bouten | bouten | must | aozora | pass |  |  |  |
| official_bouten | bouten | must | aozora-adapter | pass |  |  |  |
| official_bouten | bouten | must | ab-aozora | pass |  |  |  |
| official_bouten | bouten | must | aozora2 | pass |  |  |  |
| official_bouten | bouten | must | aozora2html | pass |  |  |  |
| official_bouten | bouten | must | aozora-rs | pass |  |  |  |
| official_bouten | bouten | must | aozora-epub3 | fail | nodes: kind sequence differs
      expected: ['bouten']
      got:      [] |  |  |
| official_bouten_circle | bouten | should | aozora | pass |  |  |  |
| official_bouten_circle | bouten | should | aozora-adapter | pass |  |  |  |
| official_bouten_circle | bouten | should | ab-aozora | pass |  |  |  |
| official_bouten_circle | bouten | should | aozora2 | pass |  |  |  |
| official_bouten_circle | bouten | should | aozora2html | warning |  | nodes: kind sequence differs
      expected: ['bouten']
      got:      ['emphasis'] |  |
| official_bouten_circle | bouten | should | aozora-rs | warning |  | nodes: kind sequence differs
      expected: ['bouten']
      got:      [] |  |
| official_bouten_circle | bouten | should | aozora-epub3 | warning |  | nodes: kind sequence differs
      expected: ['bouten']
      got:      [] |  |
| official_bouten_double_circle | bouten | should | aozora | pass |  |  |  |
| official_bouten_double_circle | bouten | should | aozora-adapter | pass |  |  |  |
| official_bouten_double_circle | bouten | should | ab-aozora | pass |  |  |  |
| official_bouten_double_circle | bouten | should | aozora2 | pass |  |  |  |
| official_bouten_double_circle | bouten | should | aozora2html | warning |  | nodes: kind sequence differs
      expected: ['bouten']
      got:      ['emphasis'] |  |
| official_bouten_double_circle | bouten | should | aozora-rs | warning |  | nodes: kind sequence differs
      expected: ['bouten']
      got:      [] |  |
| official_bouten_double_circle | bouten | should | aozora-epub3 | warning |  | nodes: kind sequence differs
      expected: ['bouten']
      got:      [] |  |
| official_bouten_fisheye | bouten | should | aozora | pass |  |  |  |
| official_bouten_fisheye | bouten | should | aozora-adapter | pass |  |  |  |
| official_bouten_fisheye | bouten | should | ab-aozora | pass |  |  |  |
| official_bouten_fisheye | bouten | should | aozora2 | pass |  |  |  |
| official_bouten_fisheye | bouten | should | aozora2html | warning |  | nodes: kind sequence differs
      expected: ['bouten']
      got:      ['emphasis'] |  |
| official_bouten_fisheye | bouten | should | aozora-rs | warning |  | nodes: kind sequence differs
      expected: ['bouten']
      got:      [] |  |
| official_bouten_fisheye | bouten | should | aozora-epub3 | warning |  | nodes: kind sequence differs
      expected: ['bouten']
      got:      [] |  |
| official_bouten_left | bouten | should | aozora | pass |  |  |  |
| official_bouten_left | bouten | should | aozora-adapter | pass |  |  |  |
| official_bouten_left | bouten | should | ab-aozora | pass |  |  |  |
| official_bouten_left | bouten | should | aozora2 | pass |  |  |  |
| official_bouten_left | bouten | should | aozora2html | pass |  |  |  |
| official_bouten_left | bouten | should | aozora-rs | pass |  |  |  |
| official_bouten_left | bouten | should | aozora-epub3 | warning |  | nodes: kind sequence differs
      expected: ['bouten']
      got:      [] |  |
| official_bouten_saltire | bouten | should | aozora | pass |  |  |  |
| official_bouten_saltire | bouten | should | aozora-adapter | pass |  |  |  |
| official_bouten_saltire | bouten | should | ab-aozora | pass |  |  |  |
| official_bouten_saltire | bouten | should | aozora2 | pass |  |  |  |
| official_bouten_saltire | bouten | should | aozora2html | warning |  | nodes: kind sequence differs
      expected: ['bouten']
      got:      ['emphasis'] |  |
| official_bouten_saltire | bouten | should | aozora-rs | warning |  | nodes: kind sequence differs
      expected: ['bouten']
      got:      [] |  |
| official_bouten_saltire | bouten | should | aozora-epub3 | warning |  | nodes: kind sequence differs
      expected: ['bouten']
      got:      [] |  |
| official_bouten_white_sesame | bouten | should | aozora | pass |  |  |  |
| official_bouten_white_sesame | bouten | should | aozora-adapter | pass |  |  |  |
| official_bouten_white_sesame | bouten | should | ab-aozora | pass |  |  |  |
| official_bouten_white_sesame | bouten | should | aozora2 | pass |  |  |  |
| official_bouten_white_sesame | bouten | should | aozora2html | pass |  |  |  |
| official_bouten_white_sesame | bouten | should | aozora-rs | pass |  |  |  |
| official_bouten_white_sesame | bouten | should | aozora-epub3 | warning |  | nodes: kind sequence differs
      expected: ['bouten']
      got:      [] |  |
| official_gaiji_description_only | gaiji | must | aozora | pass |  |  |  |
| official_gaiji_description_only | gaiji | must | aozora-adapter | pass |  |  |  |
| official_gaiji_description_only | gaiji | must | ab-aozora | pass |  |  |  |
| official_gaiji_description_only | gaiji | must | aozora2 | pass |  |  |  |
| official_gaiji_description_only | gaiji | must | aozora2html | pass |  |  |  |
| official_gaiji_description_only | gaiji | must | aozora-rs | pass |  |  |  |
| official_gaiji_description_only | gaiji | must | aozora-epub3 | fail | nodes: kind sequence differs
      expected: ['gaiji']
      got:      [] |  |  |
| official_gaiji_jisx0213 | gaiji | must | aozora | pass |  |  |  |
| official_gaiji_jisx0213 | gaiji | must | aozora-adapter | pass |  |  |  |
| official_gaiji_jisx0213 | gaiji | must | ab-aozora | pass |  |  |  |
| official_gaiji_jisx0213 | gaiji | must | aozora2 | pass |  |  |  |
| official_gaiji_jisx0213 | gaiji | must | aozora2html | pass |  |  |  |
| official_gaiji_jisx0213 | gaiji | must | aozora-rs | pass |  |  |  |
| official_gaiji_jisx0213 | gaiji | must | aozora-epub3 | fail | nodes: kind sequence differs
      expected: ['gaiji']
      got:      [] |  |  |
| official_gaiji_jisx0213_kana | gaiji | must | aozora | pass |  |  |  |
| official_gaiji_jisx0213_kana | gaiji | must | aozora-adapter | pass |  |  |  |
| official_gaiji_jisx0213_kana | gaiji | must | ab-aozora | pass |  |  |  |
| official_gaiji_jisx0213_kana | gaiji | must | aozora2 | pass |  |  |  |
| official_gaiji_jisx0213_kana | gaiji | must | aozora2html | pass |  |  |  |
| official_gaiji_jisx0213_kana | gaiji | must | aozora-rs | pass |  |  |  |
| official_gaiji_jisx0213_kana | gaiji | must | aozora-epub3 | fail | nodes: kind sequence differs
      expected: ['gaiji']
      got:      [] |  |  |
| official_gaiji_unicode | gaiji | must | aozora | pass |  |  |  |
| official_gaiji_unicode | gaiji | must | aozora-adapter | pass |  |  |  |
| official_gaiji_unicode | gaiji | must | ab-aozora | pass |  |  |  |
| official_gaiji_unicode | gaiji | must | aozora2 | pass |  |  |  |
| official_gaiji_unicode | gaiji | must | aozora2html | pass |  |  |  |
| official_gaiji_unicode | gaiji | must | aozora-rs | pass |  |  |  |
| official_gaiji_unicode | gaiji | must | aozora-epub3 | fail | nodes: kind sequence differs
      expected: ['gaiji']
      got:      [] |  |  |
| official_heading_dogyo | heading | should | aozora | warning |  | nodes: expected [{'kind': 'heading', 'span': {'start': 0, 'end': 54}}], got [{'kind': 'headingHint', 'span': {'start': 9, 'end': 54}}] |  |
| official_heading_dogyo | heading | should | aozora-adapter | pass |  |  |  |
| official_heading_dogyo | heading | should | ab-aozora | pass |  |  |  |
| official_heading_dogyo | heading | should | aozora2 | pass |  |  |  |
| official_heading_dogyo | heading | should | aozora2html | pass |  |  |  |
| official_heading_dogyo | heading | should | aozora-rs | pass |  |  |  |
| official_heading_dogyo | heading | should | aozora-epub3 | warning |  | nodes: kind sequence differs
      expected: ['heading']
      got:      [] |  |
| official_heading_large | heading | should | aozora | warning |  | nodes: expected [{'kind': 'heading', 'span': {'start': 0, 'end': 66}}], got [{'kind': 'headingHint', 'span': {'start': 18, 'end': 66}}] |  |
| official_heading_large | heading | should | aozora-adapter | pass |  |  |  |
| official_heading_large | heading | should | ab-aozora | pass |  |  |  |
| official_heading_large | heading | should | aozora2 | pass |  |  |  |
| official_heading_large | heading | should | aozora2html | pass |  |  |  |
| official_heading_large | heading | should | aozora-rs | pass |  |  |  |
| official_heading_large | heading | should | aozora-epub3 | warning |  | nodes: kind sequence differs
      expected: ['heading']
      got:      [] |  |
| official_heading_large_range | heading | should | aozora | warning |  | nodes: expected [{'kind': 'heading', 'span': {'start': 0, 'end': 63}}], got [{'kind': 'containerOpen', 'span': {'start': 0, 'end': 21}}, {'kind': 'containerClose', 'span': {'start': 33, 'end': 63}}] |  |
| official_heading_large_range | heading | should | aozora-adapter | warning |  | nodes: kind sequence differs
      expected: ['heading']
      got:      ['containerOpen', 'containerClose'] |  |
| official_heading_large_range | heading | should | ab-aozora | warning |  | nodes: kind sequence differs
      expected: ['heading']
      got:      ['containerOpen', 'containerClose'] |  |
| official_heading_large_range | heading | should | aozora2 | warning |  | nodes: kind sequence differs
      expected: ['heading']
      got:      ['containerClose'] |  |
| official_heading_large_range | heading | should | aozora2html | pass |  |  |  |
| official_heading_large_range | heading | should | aozora-rs | warning |  | nodes: kind sequence differs
      expected: ['heading']
      got:      ['heading', 'heading'] |  |
| official_heading_large_range | heading | should | aozora-epub3 | warning |  | nodes: kind sequence differs
      expected: ['heading']
      got:      [] |  |
| official_heading_mado | heading | should | aozora | warning |  | nodes: expected [{'kind': 'heading', 'span': {'start': 0, 'end': 51}}], got [{'kind': 'headingHint', 'span': {'start': 9, 'end': 51}}] |  |
| official_heading_mado | heading | should | aozora-adapter | pass |  |  |  |
| official_heading_mado | heading | should | ab-aozora | pass |  |  |  |
| official_heading_mado | heading | should | aozora2 | pass |  |  |  |
| official_heading_mado | heading | should | aozora2html | pass |  |  |  |
| official_heading_mado | heading | should | aozora-rs | warning |  | nodes: kind sequence differs
      expected: ['heading']
      got:      [] |  |
| official_heading_mado | heading | should | aozora-epub3 | warning |  | nodes: kind sequence differs
      expected: ['heading']
      got:      [] |  |
| official_heading_medium | heading | should | aozora | warning |  | nodes: expected [{'kind': 'heading', 'span': {'start': 0, 'end': 36}}], got [{'kind': 'headingHint', 'span': {'start': 3, 'end': 36}}] |  |
| official_heading_medium | heading | should | aozora-adapter | pass |  |  |  |
| official_heading_medium | heading | should | ab-aozora | pass |  |  |  |
| official_heading_medium | heading | should | aozora2 | pass |  |  |  |
| official_heading_medium | heading | should | aozora2html | pass |  |  |  |
| official_heading_medium | heading | should | aozora-rs | warning |  | nodes: kind sequence differs
      expected: ['heading']
      got:      [] |  |
| official_heading_medium | heading | should | aozora-epub3 | warning |  | nodes: kind sequence differs
      expected: ['heading']
      got:      [] |  |
| official_heading_small | heading | should | aozora | warning |  | nodes: expected [{'kind': 'heading', 'span': {'start': 0, 'end': 42}}], got [{'kind': 'headingHint', 'span': {'start': 6, 'end': 42}}] |  |
| official_heading_small | heading | should | aozora-adapter | pass |  |  |  |
| official_heading_small | heading | should | ab-aozora | pass |  |  |  |
| official_heading_small | heading | should | aozora2 | pass |  |  |  |
| official_heading_small | heading | should | aozora2html | pass |  |  |  |
| official_heading_small | heading | should | aozora-rs | warning |  | nodes: kind sequence differs
      expected: ['heading']
      got:      [] |  |
| official_heading_small | heading | should | aozora-epub3 | warning |  | nodes: kind sequence differs
      expected: ['heading']
      got:      [] |  |
| official_italic | emphasis | should | aozora | pass |  |  |  |
| official_italic | emphasis | should | aozora-adapter | pass |  |  |  |
| official_italic | emphasis | should | ab-aozora | pass |  |  |  |
| official_italic | emphasis | should | aozora2 | pass |  |  |  |
| official_italic | emphasis | should | aozora2html | pass |  |  |  |
| official_italic | emphasis | should | aozora-rs | pass |  |  |  |
| official_italic | emphasis | should | aozora-epub3 | warning |  | nodes: kind sequence differs
      expected: ['emphasis']
      got:      [] |  |
| official_jisage_ageage | layout | should | aozora | warning |  | nodes: expected [{'kind': 'indent', 'span': {'start': 0, 'end': 60}}], got [{'kind': 'alignEnd', 'span': {'start': 0, 'end': 30}}] |  |
| official_jisage_ageage | layout | should | aozora-adapter | warning |  | nodes: kind sequence differs
      expected: ['indent']
      got:      ['emphasis'] |  |
| official_jisage_ageage | layout | should | ab-aozora | warning |  | nodes: kind sequence differs
      expected: ['indent']
      got:      ['emphasis'] |  |
| official_jisage_ageage | layout | should | aozora2 | warning |  | nodes: kind sequence differs
      expected: ['indent']
      got:      [] |  |
| official_jisage_ageage | layout | should | aozora2html | warning |  | nodes: kind sequence differs
      expected: ['indent']
      got:      ['emphasis'] |  |
| official_jisage_ageage | layout | should | aozora-rs | warning |  | nodes: kind sequence differs
      expected: ['indent']
      got:      [] |  |
| official_jisage_ageage | layout | should | aozora-epub3 | warning |  | nodes: kind sequence differs
      expected: ['indent']
      got:      [] |  |
| official_pagebreak_kaicho | break | should | aozora | warning |  | nodes: expected [{'kind': 'pageBreak', 'span': {'start': 0, 'end': 15}}], got [{'kind': 'sectionBreak', 'span': {'start': 0, 'end': 15}}] |  |
| official_pagebreak_kaicho | break | should | aozora-adapter | warning |  | nodes: kind sequence differs
      expected: ['pageBreak']
      got:      ['sectionBreak'] |  |
| official_pagebreak_kaicho | break | should | ab-aozora | warning |  | nodes: kind sequence differs
      expected: ['pageBreak']
      got:      ['sectionBreak'] |  |
| official_pagebreak_kaicho | break | should | aozora2 | warning |  | nodes: kind sequence differs
      expected: ['pageBreak']
      got:      ['directive'] |  |
| official_pagebreak_kaicho | break | should | aozora2html | warning |  | nodes: kind sequence differs
      expected: ['pageBreak']
      got:      ['emphasis'] |  |
| official_pagebreak_kaicho | break | should | aozora-rs | warning |  | nodes: kind sequence differs
      expected: ['pageBreak']
      got:      [] |  |
| official_pagebreak_kaicho | break | should | aozora-epub3 | warning |  | nodes: kind sequence differs
      expected: ['pageBreak']
      got:      [] |  |
| official_pagebreak_kaipage | break | should | aozora | pass |  |  |  |
| official_pagebreak_kaipage | break | should | aozora-adapter | pass |  |  |  |
| official_pagebreak_kaipage | break | should | ab-aozora | pass |  |  |  |
| official_pagebreak_kaipage | break | should | aozora2 | warning |  | nodes: kind sequence differs
      expected: ['pageBreak']
      got:      [] |  |
| official_pagebreak_kaipage | break | should | aozora2html | warning |  | nodes: kind sequence differs
      expected: ['pageBreak']
      got:      [] |  |
| official_pagebreak_kaipage | break | should | aozora-rs | warning |  | nodes: kind sequence differs
      expected: ['pageBreak']
      got:      [] |  |
| official_pagebreak_kaipage | break | should | aozora-epub3 | warning |  | nodes: kind sequence differs
      expected: ['pageBreak']
      got:      [] |  |
| official_ruby_alphabet_base | ruby | must | aozora | fail | nodes: expected [{'kind': 'ruby', 'span': {'start': 0, 'end': 39}}], got [{'kind': 'ruby', 'span': {'start': 0, 'end': 27}}] |  |  |
| official_ruby_alphabet_base | ruby | must | aozora-adapter | pass |  |  |  |
| official_ruby_alphabet_base | ruby | must | ab-aozora | pass |  |  |  |
| official_ruby_alphabet_base | ruby | must | aozora2 | pass |  |  |  |
| official_ruby_alphabet_base | ruby | must | aozora2html | pass |  |  |  |
| official_ruby_alphabet_base | ruby | must | aozora-rs | pass |  |  |  |
| official_ruby_alphabet_base | ruby | must | aozora-epub3 | fail | nodes: kind sequence differs
      expected: ['ruby']
      got:      [] |  |  |
| official_ruby_explicit | ruby | must | aozora | fail | nodes: expected [{'kind': 'ruby', 'span': {'start': 0, 'end': 39}}], got [{'kind': 'ruby', 'span': {'start': 0, 'end': 27}}] |  |  |
| official_ruby_explicit | ruby | must | aozora-adapter | pass |  |  |  |
| official_ruby_explicit | ruby | must | ab-aozora | pass |  |  |  |
| official_ruby_explicit | ruby | must | aozora2 | pass |  |  |  |
| official_ruby_explicit | ruby | must | aozora2html | pass |  |  |  |
| official_ruby_explicit | ruby | must | aozora-rs | pass |  |  |  |
| official_ruby_explicit | ruby | must | aozora-epub3 | fail | nodes: kind sequence differs
      expected: ['ruby']
      got:      [] |  |  |
| official_ruby_implicit | ruby | must | aozora | pass |  |  |  |
| official_ruby_implicit | ruby | must | aozora-adapter | pass |  |  |  |
| official_ruby_implicit | ruby | must | ab-aozora | pass |  |  |  |
| official_ruby_implicit | ruby | must | aozora2 | pass |  |  |  |
| official_ruby_implicit | ruby | must | aozora2html | pass |  |  |  |
| official_ruby_implicit | ruby | must | aozora-rs | pass |  |  |  |
| official_ruby_implicit | ruby | must | aozora-epub3 | fail | nodes: kind sequence differs
      expected: ['ruby']
      got:      [] |  |  |
