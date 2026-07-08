# Aozora Notation-Spec Comparison

- vectors_dir: `reports/parser-conformance/official-docs-seed`
- rows: 44

| vector | feature | level | adapter | status | failures | warnings | skips |
| --- | --- | --- | --- | --- | --- | --- | --- |
| official_bold | emphasis | should | aozora | warning |  | nodes: expected [{'kind': 'emphasis', 'span': {'start': 0, 'end': 39}}], got [{'kind': 'emphasis', 'span': {'start': 0, 'end': 36}}] |  |
| official_bold | emphasis | should | ab-aozora | pass |  |  |  |
| official_bold | emphasis | should | aozora2 | pass |  |  |  |
| official_bold | emphasis | should | aozora-rs | pass |  |  |  |
| official_bousen | bousen | should | aozora | warning |  | nodes: expected [{'kind': 'emphasis', 'span': {'start': 0, 'end': 75}}], got [{'kind': 'bouten', 'span': {'start': 12, 'end': 48}}] |  |
| official_bousen | bousen | should | ab-aozora | warning |  | nodes: kind sequence differs
      expected: ['emphasis']
      got:      ['bouten'] |  |
| official_bousen | bousen | should | aozora2 | pass |  |  |  |
| official_bousen | bousen | should | aozora-rs | warning |  | nodes: kind sequence differs
      expected: ['emphasis']
      got:      [] |  |
| official_bouten | bouten | must | aozora | pass |  |  |  |
| official_bouten | bouten | must | ab-aozora | pass |  |  |  |
| official_bouten | bouten | must | aozora2 | pass |  |  |  |
| official_bouten | bouten | must | aozora-rs | pass |  |  |  |
| official_gaiji_jisx0213 | gaiji | must | aozora | pass |  |  |  |
| official_gaiji_jisx0213 | gaiji | must | ab-aozora | pass |  |  |  |
| official_gaiji_jisx0213 | gaiji | must | aozora2 | pass |  |  |  |
| official_gaiji_jisx0213 | gaiji | must | aozora-rs | pass |  |  |  |
| official_heading_large | heading | should | aozora | warning |  | nodes: expected [{'kind': 'heading', 'span': {'start': 0, 'end': 66}}], got [{'kind': 'headingHint', 'span': {'start': 18, 'end': 66}}] |  |
| official_heading_large | heading | should | ab-aozora | pass |  |  |  |
| official_heading_large | heading | should | aozora2 | pass |  |  |  |
| official_heading_large | heading | should | aozora-rs | pass |  |  |  |
| official_italic | emphasis | should | aozora | pass |  |  |  |
| official_italic | emphasis | should | ab-aozora | pass |  |  |  |
| official_italic | emphasis | should | aozora2 | pass |  |  |  |
| official_italic | emphasis | should | aozora-rs | pass |  |  |  |
| official_jisage_ageage | layout | should | aozora | warning |  | nodes: expected [{'kind': 'indent', 'span': {'start': 0, 'end': 60}}], got [{'kind': 'alignEnd', 'span': {'start': 0, 'end': 30}}] |  |
| official_jisage_ageage | layout | should | ab-aozora | warning |  | nodes: kind sequence differs
      expected: ['indent']
      got:      ['emphasis'] |  |
| official_jisage_ageage | layout | should | aozora2 | warning |  | nodes: kind sequence differs
      expected: ['indent']
      got:      [] |  |
| official_jisage_ageage | layout | should | aozora-rs | warning |  | nodes: kind sequence differs
      expected: ['indent']
      got:      [] |  |
| official_pagebreak_kaicho | break | should | aozora | warning |  | nodes: expected [{'kind': 'pageBreak', 'span': {'start': 0, 'end': 15}}], got [{'kind': 'sectionBreak', 'span': {'start': 0, 'end': 15}}] |  |
| official_pagebreak_kaicho | break | should | ab-aozora | warning |  | nodes: kind sequence differs
      expected: ['pageBreak']
      got:      ['sectionBreak'] |  |
| official_pagebreak_kaicho | break | should | aozora2 | warning |  | nodes: kind sequence differs
      expected: ['pageBreak']
      got:      ['directive'] |  |
| official_pagebreak_kaicho | break | should | aozora-rs | warning |  | nodes: kind sequence differs
      expected: ['pageBreak']
      got:      [] |  |
| official_pagebreak_kaipage | break | should | aozora | pass |  |  |  |
| official_pagebreak_kaipage | break | should | ab-aozora | pass |  |  |  |
| official_pagebreak_kaipage | break | should | aozora2 | warning |  | nodes: kind sequence differs
      expected: ['pageBreak']
      got:      [] |  |
| official_pagebreak_kaipage | break | should | aozora-rs | warning |  | nodes: kind sequence differs
      expected: ['pageBreak']
      got:      [] |  |
| official_ruby_explicit | ruby | must | aozora | fail | nodes: expected [{'kind': 'ruby', 'span': {'start': 0, 'end': 39}}], got [{'kind': 'ruby', 'span': {'start': 0, 'end': 27}}] |  |  |
| official_ruby_explicit | ruby | must | ab-aozora | pass |  |  |  |
| official_ruby_explicit | ruby | must | aozora2 | pass |  |  |  |
| official_ruby_explicit | ruby | must | aozora-rs | pass |  |  |  |
| official_ruby_implicit | ruby | must | aozora | pass |  |  |  |
| official_ruby_implicit | ruby | must | ab-aozora | pass |  |  |  |
| official_ruby_implicit | ruby | must | aozora2 | pass |  |  |  |
| official_ruby_implicit | ruby | must | aozora-rs | pass |  |  |  |
