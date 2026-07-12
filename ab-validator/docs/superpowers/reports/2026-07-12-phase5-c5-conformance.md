# Aozora Notation-Spec Comparison

Note (Phase 5 closure): this file's `.summary.json` companion
(`2026-07-12-phase5-c5-conformance.summary.json`) is byte-identical to
`2026-07-12-phase4-retirement-conformance.summary.json` — expected from a
deterministic pipeline run over unchanged legacy comparison-lane binaries
after the Phase 4 retirement dropped 2 adapters from the wider Phase 4
comparison set down to the 5 lanes reported here; the `ab-aozora` rows in
this report independently verify zero row drift against the true C4
baseline (`2026-07-12-phase5-c5-conformance-gate.summary.json`), not
against this retirement snapshot.

- vectors_dir: `/nix/store/55sq8wlgl97rfdyqvnncxyz4j33pp6l9-upstream-aozora-notation-spec/conformance/vectors`
- rows: 635

| vector | feature | level | adapter | status | failures | warnings | skips |
| --- | --- | --- | --- | --- | --- | --- | --- |
| accent_decomposition_applied | accent | may | ab-aozora | pass |  |  | nodes: vector has no expected.nodes<br>pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only) |
| accent_decomposition_applied | accent | may | aozora2 | skip |  |  | nodes: vector has no expected.nodes<br>pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| accent_decomposition_applied | accent | may | aozora2html | skip |  |  | nodes: vector has no expected.nodes<br>pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| accent_decomposition_applied | accent | may | aozora-rs | skip |  |  | nodes: vector has no expected.nodes<br>pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| accent_decomposition_applied | accent | may | aozora-epub3 | skip |  |  | nodes: vector has no expected.nodes<br>pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| accent_dot_above | emphasis | should | ab-aozora | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only) |
| accent_dot_above | emphasis | should | aozora2 | warning |  | nodes: kind sequence differs
      expected: ['emphasis']
      got:      ['directive'] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| accent_dot_above | emphasis | should | aozora2html | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| accent_dot_above | emphasis | should | aozora-rs | warning |  | nodes: kind sequence differs
      expected: ['emphasis']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| accent_dot_above | emphasis | should | aozora-epub3 | warning |  | nodes: kind sequence differs
      expected: ['emphasis']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| accent_dot_below | emphasis | should | ab-aozora | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only) |
| accent_dot_below | emphasis | should | aozora2 | warning |  | nodes: kind sequence differs
      expected: ['emphasis']
      got:      ['directive'] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| accent_dot_below | emphasis | should | aozora2html | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| accent_dot_below | emphasis | should | aozora-rs | warning |  | nodes: kind sequence differs
      expected: ['emphasis']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| accent_dot_below | emphasis | should | aozora-epub3 | warning |  | nodes: kind sequence differs
      expected: ['emphasis']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| accent_dot_cluster | emphasis | should | ab-aozora | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only) |
| accent_dot_cluster | emphasis | should | aozora2 | warning |  | nodes: kind sequence differs
      expected: ['emphasis']
      got:      ['directive'] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| accent_dot_cluster | emphasis | should | aozora2html | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| accent_dot_cluster | emphasis | should | aozora-rs | warning |  | nodes: kind sequence differs
      expected: ['emphasis']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| accent_dot_cluster | emphasis | should | aozora-epub3 | warning |  | nodes: kind sequence differs
      expected: ['emphasis']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| accent_dot_dangyou_unknown | emphasis | should | ab-aozora | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only) |
| accent_dot_dangyou_unknown | emphasis | should | aozora2 | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| accent_dot_dangyou_unknown | emphasis | should | aozora2html | warning |  | nodes: kind sequence differs
      expected: ['directive']
      got:      ['emphasis'] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| accent_dot_dangyou_unknown | emphasis | should | aozora-rs | warning |  | nodes: kind sequence differs
      expected: ['directive']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| accent_dot_dangyou_unknown | emphasis | should | aozora-epub3 | warning |  | nodes: kind sequence differs
      expected: ['directive']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| accent_dot_former_latter | emphasis | should | ab-aozora | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only) |
| accent_dot_former_latter | emphasis | should | aozora2 | warning |  | nodes: kind sequence differs
      expected: ['emphasis']
      got:      ['directive'] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| accent_dot_former_latter | emphasis | should | aozora2html | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| accent_dot_former_latter | emphasis | should | aozora-rs | warning |  | nodes: kind sequence differs
      expected: ['emphasis']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| accent_dot_former_latter | emphasis | should | aozora-epub3 | warning |  | nodes: kind sequence differs
      expected: ['emphasis']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| accent_dot_multi_clause | emphasis | should | ab-aozora | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only) |
| accent_dot_multi_clause | emphasis | should | aozora2 | warning |  | nodes: kind sequence differs
      expected: ['emphasis']
      got:      ['directive'] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| accent_dot_multi_clause | emphasis | should | aozora2html | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| accent_dot_multi_clause | emphasis | should | aozora-rs | warning |  | nodes: kind sequence differs
      expected: ['emphasis']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| accent_dot_multi_clause | emphasis | should | aozora-epub3 | warning |  | nodes: kind sequence differs
      expected: ['emphasis']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| accent_dot_ordinal | emphasis | should | ab-aozora | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only) |
| accent_dot_ordinal | emphasis | should | aozora2 | warning |  | nodes: kind sequence differs
      expected: ['emphasis']
      got:      ['directive'] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| accent_dot_ordinal | emphasis | should | aozora2html | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| accent_dot_ordinal | emphasis | should | aozora-rs | warning |  | nodes: kind sequence differs
      expected: ['emphasis']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| accent_dot_ordinal | emphasis | should | aozora-epub3 | warning |  | nodes: kind sequence differs
      expected: ['emphasis']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| accent_dot_ruby_base_unknown | emphasis | should | ab-aozora | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only) |
| accent_dot_ruby_base_unknown | emphasis | should | aozora2 | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| accent_dot_ruby_base_unknown | emphasis | should | aozora2html | warning |  | nodes: kind sequence differs
      expected: ['ruby', 'directive']
      got:      ['ruby', 'emphasis'] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| accent_dot_ruby_base_unknown | emphasis | should | aozora-rs | warning |  | nodes: kind sequence differs
      expected: ['ruby', 'directive']
      got:      ['ruby'] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| accent_dot_ruby_base_unknown | emphasis | should | aozora-epub3 | warning |  | nodes: kind sequence differs
      expected: ['ruby', 'directive']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| accent_dot_tortoise | emphasis | should | ab-aozora | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only) |
| accent_dot_tortoise | emphasis | should | aozora2 | warning |  | nodes: kind sequence differs
      expected: ['emphasis']
      got:      ['directive'] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| accent_dot_tortoise | emphasis | should | aozora2html | warning |  | nodes: kind sequence differs
      expected: ['emphasis']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| accent_dot_tortoise | emphasis | should | aozora-rs | warning |  | nodes: kind sequence differs
      expected: ['emphasis']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| accent_dot_tortoise | emphasis | should | aozora-epub3 | warning |  | nodes: kind sequence differs
      expected: ['emphasis']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| accent_dot_uppercase | emphasis | should | ab-aozora | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only) |
| accent_dot_uppercase | emphasis | should | aozora2 | warning |  | nodes: kind sequence differs
      expected: ['emphasis']
      got:      ['directive'] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| accent_dot_uppercase | emphasis | should | aozora2html | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| accent_dot_uppercase | emphasis | should | aozora-rs | warning |  | nodes: kind sequence differs
      expected: ['emphasis']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| accent_dot_uppercase | emphasis | should | aozora-epub3 | warning |  | nodes: kind sequence differs
      expected: ['emphasis']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| align_end_container | container | should | ab-aozora | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only) |
| align_end_container | container | should | aozora2 | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| align_end_container | container | should | aozora2html | warning |  | nodes: kind sequence differs
      expected: ['containerOpen', 'containerClose']
      got:      ['emphasis', 'directive'] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| align_end_container | container | should | aozora-rs | warning |  | nodes: kind sequence differs
      expected: ['containerOpen', 'containerClose']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| align_end_container | container | should | aozora-epub3 | warning |  | nodes: adapter error: xhtml parse failed: expected 'body' tag, not 'div' at 15:1

Stack backtrace:
   0: anyhow::error::<impl anyhow::Error>::msg
   1: anyhow::__private::format_err
   2: aozora_epub3_adapter::xhtml_mapper::map_to_aat
   3: aozora_epub3_adapter::main
   4: std::sys::backtrace::__rust_begin_short_backtrace
   5: std::rt::lang_start::{{closure}}
   6: std::rt::lang_start_internal
   7: main
   8: __libc_start_call_main
   9: __libc_start_main_alias_2
  10: _start | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| angle_quote | angle_quote | must | ab-aozora | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only) |
| angle_quote | angle_quote | must | aozora2 | fail | nodes: kind sequence differs
      expected: ['angleQuote']
      got:      [] |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| angle_quote | angle_quote | must | aozora2html | fail | nodes: kind sequence differs
      expected: ['angleQuote']
      got:      [] |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| angle_quote | angle_quote | must | aozora-rs | fail | nodes: kind sequence differs
      expected: ['angleQuote']
      got:      [] |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| angle_quote | angle_quote | must | aozora-epub3 | fail | nodes: kind sequence differs
      expected: ['angleQuote']
      got:      [] |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| annotation | annotation | must | ab-aozora | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only) |
| annotation | annotation | must | aozora2 | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| annotation | annotation | must | aozora2html | fail | nodes: kind sequence differs
      expected: ['directive']
      got:      ['emphasis'] |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| annotation | annotation | must | aozora-rs | fail | nodes: kind sequence differs
      expected: ['directive']
      got:      [] |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| annotation | annotation | must | aozora-epub3 | fail | nodes: kind sequence differs
      expected: ['directive']
      got:      [] |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| body_end | structural-marker | must | ab-aozora | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only) |
| body_end | structural-marker | must | aozora2 | fail | nodes: kind sequence differs
      expected: ['bodyEnd']
      got:      ['directive'] |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| body_end | structural-marker | must | aozora2html | fail | nodes: kind sequence differs
      expected: ['bodyEnd']
      got:      [] |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| body_end | structural-marker | must | aozora-rs | fail | nodes: kind sequence differs
      expected: ['bodyEnd']
      got:      [] |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| body_end | structural-marker | must | aozora-epub3 | fail | nodes: kind sequence differs
      expected: ['bodyEnd']
      got:      [] |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| boki | kunten | should | ab-aozora | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only) |
| boki | kunten | should | aozora2 | warning |  | nodes: kind sequence differs
      expected: ['marginNote']
      got:      ['ruby'] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| boki | kunten | should | aozora2html | warning |  | nodes: kind sequence differs
      expected: ['marginNote']
      got:      ['ruby'] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| boki | kunten | should | aozora-rs | warning |  | nodes: kind sequence differs
      expected: ['marginNote']
      got:      ['ruby'] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| boki | kunten | should | aozora-epub3 | warning |  | nodes: kind sequence differs
      expected: ['marginNote']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| bold_block | emphasis | should | ab-aozora | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only) |
| bold_block | emphasis | should | aozora2 | warning |  | nodes: kind sequence differs
      expected: ['containerOpen', 'containerClose']
      got:      ['emphasis'] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| bold_block | emphasis | should | aozora2html | warning |  | nodes: kind sequence differs
      expected: ['containerOpen', 'containerClose']
      got:      ['emphasis'] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| bold_block | emphasis | should | aozora-rs | warning |  | nodes: kind sequence differs
      expected: ['containerOpen', 'containerClose']
      got:      ['emphasis'] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| bold_block | emphasis | should | aozora-epub3 | warning |  | nodes: kind sequence differs
      expected: ['containerOpen', 'containerClose']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| bold_forward | emphasis | should | ab-aozora | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only) |
| bold_forward | emphasis | should | aozora2 | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| bold_forward | emphasis | should | aozora2html | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| bold_forward | emphasis | should | aozora-rs | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| bold_forward | emphasis | should | aozora-epub3 | warning |  | nodes: kind sequence differs
      expected: ['emphasis']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| bold_forward_gothic_no_referent | emphasis | should | ab-aozora | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only) |
| bold_forward_gothic_no_referent | emphasis | should | aozora2 | warning |  | nodes: kind sequence differs
      expected: ['emphasis']
      got:      ['directive'] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| bold_forward_gothic_no_referent | emphasis | should | aozora2html | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| bold_forward_gothic_no_referent | emphasis | should | aozora-rs | warning |  | nodes: kind sequence differs
      expected: ['emphasis']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| bold_forward_gothic_no_referent | emphasis | should | aozora-epub3 | warning |  | nodes: kind sequence differs
      expected: ['emphasis']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| bold_forward_interior | emphasis | should | ab-aozora | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only) |
| bold_forward_interior | emphasis | should | aozora2 | warning |  | nodes: kind sequence differs
      expected: ['emphasis', 'emphasis']
      got:      ['emphasis'] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| bold_forward_interior | emphasis | should | aozora2html | warning |  | nodes: kind sequence differs
      expected: ['emphasis', 'emphasis']
      got:      ['emphasis'] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| bold_forward_interior | emphasis | should | aozora-rs | warning |  | nodes: kind sequence differs
      expected: ['emphasis', 'emphasis']
      got:      ['emphasis'] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| bold_forward_interior | emphasis | should | aozora-epub3 | warning |  | nodes: kind sequence differs
      expected: ['emphasis', 'emphasis']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| bold_forward_no_referent | emphasis | should | ab-aozora | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only) |
| bold_forward_no_referent | emphasis | should | aozora2 | warning |  | nodes: kind sequence differs
      expected: ['emphasis']
      got:      ['directive'] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| bold_forward_no_referent | emphasis | should | aozora2html | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| bold_forward_no_referent | emphasis | should | aozora-rs | warning |  | nodes: kind sequence differs
      expected: ['emphasis']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| bold_forward_no_referent | emphasis | should | aozora-epub3 | warning |  | nodes: kind sequence differs
      expected: ['emphasis']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| bold_inline | emphasis | should | ab-aozora | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only) |
| bold_inline | emphasis | should | aozora2 | warning |  | nodes: kind sequence differs
      expected: ['emphasis']
      got:      ['containerOpen', 'containerClose'] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| bold_inline | emphasis | should | aozora2html | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| bold_inline | emphasis | should | aozora-rs | warning |  | nodes: kind sequence differs
      expected: ['emphasis']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| bold_inline | emphasis | should | aozora-epub3 | warning |  | nodes: kind sequence differs
      expected: ['emphasis']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| bouten | bouten | must | ab-aozora | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only) |
| bouten | bouten | must | aozora2 | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| bouten | bouten | must | aozora2html | fail | nodes: kind sequence differs
      expected: ['bouten']
      got:      ['emphasis'] |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| bouten | bouten | must | aozora-rs | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| bouten | bouten | must | aozora-epub3 | fail | nodes: kind sequence differs
      expected: ['bouten']
      got:      [] |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| bouten_black_triangle | bouten | should | ab-aozora | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only) |
| bouten_black_triangle | bouten | should | aozora2 | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| bouten_black_triangle | bouten | should | aozora2html | warning |  | nodes: kind sequence differs
      expected: ['bouten']
      got:      ['emphasis'] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| bouten_black_triangle | bouten | should | aozora-rs | warning |  | nodes: kind sequence differs
      expected: ['bouten']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| bouten_black_triangle | bouten | should | aozora-epub3 | warning |  | nodes: kind sequence differs
      expected: ['bouten']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| bouten_chain_line | bouten | should | ab-aozora | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only) |
| bouten_chain_line | bouten | should | aozora2 | warning |  | nodes: kind sequence differs
      expected: ['bouten']
      got:      ['emphasis'] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| bouten_chain_line | bouten | should | aozora2html | warning |  | nodes: kind sequence differs
      expected: ['bouten']
      got:      ['emphasis'] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| bouten_chain_line | bouten | should | aozora-rs | warning |  | nodes: kind sequence differs
      expected: ['bouten']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| bouten_chain_line | bouten | should | aozora-epub3 | warning |  | nodes: kind sequence differs
      expected: ['bouten']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| bouten_forward_interior | bouten | should | ab-aozora | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only) |
| bouten_forward_interior | bouten | should | aozora2 | warning |  | nodes: kind sequence differs
      expected: ['bouten', 'bouten']
      got:      ['bouten'] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| bouten_forward_interior | bouten | should | aozora2html | warning |  | nodes: kind sequence differs
      expected: ['bouten', 'bouten']
      got:      ['emphasis'] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| bouten_forward_interior | bouten | should | aozora-rs | warning |  | nodes: kind sequence differs
      expected: ['bouten', 'bouten']
      got:      ['bouten'] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| bouten_forward_interior | bouten | should | aozora-epub3 | warning |  | nodes: kind sequence differs
      expected: ['bouten', 'bouten']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| bouten_forward_no_referent | bouten | should | ab-aozora | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only) |
| bouten_forward_no_referent | bouten | should | aozora2 | warning |  | nodes: kind sequence differs
      expected: ['bouten']
      got:      ['directive'] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| bouten_forward_no_referent | bouten | should | aozora2html | warning |  | nodes: kind sequence differs
      expected: ['bouten']
      got:      ['emphasis'] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| bouten_forward_no_referent | bouten | should | aozora-rs | warning |  | nodes: kind sequence differs
      expected: ['bouten']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| bouten_forward_no_referent | bouten | should | aozora-epub3 | warning |  | nodes: kind sequence differs
      expected: ['bouten']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| bouten_range | bouten | should | ab-aozora | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only) |
| bouten_range | bouten | should | aozora2 | warning |  | nodes: kind sequence differs
      expected: ['bouten', 'bouten', 'bouten']
      got:      ['containerOpen', 'containerClose', 'containerOpen', 'containerClose', 'containerOpen', 'containerClose'] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| bouten_range | bouten | should | aozora2html | warning |  | nodes: kind sequence differs
      expected: ['bouten', 'bouten', 'bouten']
      got:      ['emphasis', 'emphasis', 'emphasis'] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| bouten_range | bouten | should | aozora-rs | warning |  | nodes: kind sequence differs
      expected: ['bouten', 'bouten', 'bouten']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| bouten_range | bouten | should | aozora-epub3 | warning |  | nodes: kind sequence differs
      expected: ['bouten', 'bouten', 'bouten']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| bouten_range_chain_line | bouten | should | ab-aozora | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only) |
| bouten_range_chain_line | bouten | should | aozora2 | warning |  | nodes: kind sequence differs
      expected: ['bouten']
      got:      ['containerOpen', 'containerClose'] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| bouten_range_chain_line | bouten | should | aozora2html | warning |  | nodes: kind sequence differs
      expected: ['bouten']
      got:      ['emphasis'] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| bouten_range_chain_line | bouten | should | aozora-rs | warning |  | nodes: kind sequence differs
      expected: ['bouten']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| bouten_range_chain_line | bouten | should | aozora-epub3 | warning |  | nodes: kind sequence differs
      expected: ['bouten']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| bouten_target_ambiguous | bouten | should | ab-aozora | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only) |
| bouten_target_ambiguous | bouten | should | aozora2 | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| bouten_target_ambiguous | bouten | should | aozora2html | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| bouten_target_ambiguous | bouten | should | aozora-rs | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| bouten_target_ambiguous | bouten | should | aozora-epub3 | warning |  | nodes: kind sequence differs
      expected: ['bouten']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| box_enclosure | emphasis | should | ab-aozora | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only) |
| box_enclosure | emphasis | should | aozora2 | warning |  | nodes: kind sequence differs
      expected: ['emphasis']
      got:      ['directive'] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| box_enclosure | emphasis | should | aozora2html | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| box_enclosure | emphasis | should | aozora-rs | warning |  | nodes: kind sequence differs
      expected: ['emphasis']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| box_enclosure | emphasis | should | aozora-epub3 | warning |  | nodes: kind sequence differs
      expected: ['emphasis']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| box_enclosure_no_referent | emphasis | should | ab-aozora | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only) |
| box_enclosure_no_referent | emphasis | should | aozora2 | warning |  | nodes: kind sequence differs
      expected: ['emphasis']
      got:      ['directive'] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| box_enclosure_no_referent | emphasis | should | aozora2html | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| box_enclosure_no_referent | emphasis | should | aozora-rs | warning |  | nodes: kind sequence differs
      expected: ['emphasis']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| box_enclosure_no_referent | emphasis | should | aozora-epub3 | warning |  | nodes: kind sequence differs
      expected: ['emphasis']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| bracketed_kaeriten_no_pair | kunten | should | ab-aozora | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only) |
| bracketed_kaeriten_no_pair | kunten | should | aozora2 | warning |  | nodes: kind sequence differs
      expected: ['kaeriten']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| bracketed_kaeriten_no_pair | kunten | should | aozora2html | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| bracketed_kaeriten_no_pair | kunten | should | aozora-rs | warning |  | nodes: kind sequence differs
      expected: ['kaeriten']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| bracketed_kaeriten_no_pair | kunten | should | aozora-epub3 | warning |  | nodes: kind sequence differs
      expected: ['kaeriten']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| break_in_single_line_container | break | should | ab-aozora | warning |  | nodes: kind sequence differs
      expected: ['alignEnd', 'pageBreak']
      got:      ['emphasis', 'pageBreak'] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only) |
| break_in_single_line_container | break | should | aozora2 | warning |  | nodes: kind sequence differs
      expected: ['alignEnd', 'pageBreak']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| break_in_single_line_container | break | should | aozora2html | warning |  | nodes: kind sequence differs
      expected: ['alignEnd', 'pageBreak']
      got:      ['emphasis', 'emphasis'] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| break_in_single_line_container | break | should | aozora-rs | warning |  | nodes: kind sequence differs
      expected: ['alignEnd', 'pageBreak']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| break_in_single_line_container | break | should | aozora-epub3 | warning |  | nodes: kind sequence differs
      expected: ['alignEnd', 'pageBreak']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| center_page | container | should | ab-aozora | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only) |
| center_page | container | should | aozora2 | warning |  | nodes: kind sequence differs
      expected: ['center']
      got:      ['directive'] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| center_page | container | should | aozora2html | warning |  | nodes: kind sequence differs
      expected: ['center']
      got:      ['emphasis'] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| center_page | container | should | aozora-rs | warning |  | nodes: kind sequence differs
      expected: ['center']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| center_page | container | should | aozora-epub3 | warning |  | nodes: kind sequence differs
      expected: ['center']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| columns_container | tables_columns | should | ab-aozora | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only) |
| columns_container | tables_columns | should | aozora2 | warning |  | nodes: kind sequence differs
      expected: ['containerOpen', 'containerClose']
      got:      ['directive', 'directive'] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| columns_container | tables_columns | should | aozora2html | warning |  | nodes: kind sequence differs
      expected: ['containerOpen', 'containerClose']
      got:      ['emphasis', 'emphasis'] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| columns_container | tables_columns | should | aozora-rs | warning |  | nodes: kind sequence differs
      expected: ['containerOpen', 'containerClose']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| columns_container | tables_columns | should | aozora-epub3 | warning |  | nodes: kind sequence differs
      expected: ['containerOpen', 'containerClose']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| correction_sic | annotation | should | ab-aozora | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only) |
| correction_sic | annotation | should | aozora2 | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| correction_sic | annotation | should | aozora2html | warning |  | nodes: kind sequence differs
      expected: ['directive']
      got:      ['emphasis'] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| correction_sic | annotation | should | aozora-rs | warning |  | nodes: kind sequence differs
      expected: ['directive']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| correction_sic | annotation | should | aozora-epub3 | warning |  | nodes: kind sequence differs
      expected: ['directive']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| correction_textual_note | annotation | should | ab-aozora | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only) |
| correction_textual_note | annotation | should | aozora2 | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| correction_textual_note | annotation | should | aozora2html | warning |  | nodes: kind sequence differs
      expected: ['directive']
      got:      ['emphasis'] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| correction_textual_note | annotation | should | aozora-rs | warning |  | nodes: kind sequence differs
      expected: ['directive']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| correction_textual_note | annotation | should | aozora-epub3 | warning |  | nodes: kind sequence differs
      expected: ['directive']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| editor_note | annotation | should | ab-aozora | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only) |
| editor_note | annotation | should | aozora2 | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| editor_note | annotation | should | aozora2html | warning |  | nodes: kind sequence differs
      expected: ['directive']
      got:      ['emphasis'] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| editor_note | annotation | should | aozora-rs | warning |  | nodes: kind sequence differs
      expected: ['directive']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| editor_note | annotation | should | aozora-epub3 | warning |  | nodes: kind sequence differs
      expected: ['directive']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| emphasis_mixed | emphasis | should | ab-aozora | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only) |
| emphasis_mixed | emphasis | should | aozora2 | warning |  | nodes: kind sequence differs
      expected: ['emphasis', 'emphasis', 'bouten']
      got:      ['containerOpen', 'containerClose', 'containerOpen', 'containerClose', 'bouten'] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| emphasis_mixed | emphasis | should | aozora2html | warning |  | nodes: kind sequence differs
      expected: ['emphasis', 'emphasis', 'bouten']
      got:      ['emphasis', 'emphasis', 'emphasis'] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| emphasis_mixed | emphasis | should | aozora-rs | warning |  | nodes: kind sequence differs
      expected: ['emphasis', 'emphasis', 'bouten']
      got:      ['bouten'] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| emphasis_mixed | emphasis | should | aozora-epub3 | warning |  | nodes: kind sequence differs
      expected: ['emphasis', 'emphasis', 'bouten']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| empty_ruby_reading | ruby | should | ab-aozora | pass |  |  | nodes: vector has no expected.nodes<br>pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only) |
| empty_ruby_reading | ruby | should | aozora2 | skip |  |  | nodes: vector has no expected.nodes<br>pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| empty_ruby_reading | ruby | should | aozora2html | skip |  |  | nodes: vector has no expected.nodes<br>pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| empty_ruby_reading | ruby | should | aozora-rs | skip |  |  | nodes: vector has no expected.nodes<br>pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| empty_ruby_reading | ruby | should | aozora-epub3 | skip |  |  | nodes: vector has no expected.nodes<br>pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| font_size_absolute_small_forward | emphasis | should | ab-aozora | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only) |
| font_size_absolute_small_forward | emphasis | should | aozora2 | warning |  | nodes: kind sequence differs
      expected: ['emphasis']
      got:      ['directive'] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| font_size_absolute_small_forward | emphasis | should | aozora2html | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| font_size_absolute_small_forward | emphasis | should | aozora-rs | warning |  | nodes: kind sequence differs
      expected: ['emphasis']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| font_size_absolute_small_forward | emphasis | should | aozora-epub3 | warning |  | nodes: kind sequence differs
      expected: ['emphasis']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| font_size_bare_range | container | should | ab-aozora | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only) |
| font_size_bare_range | container | should | aozora2 | warning |  | nodes: kind sequence differs
      expected: ['containerOpen', 'containerClose']
      got:      ['emphasis'] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| font_size_bare_range | container | should | aozora2html | warning |  | nodes: kind sequence differs
      expected: ['containerOpen', 'containerClose']
      got:      ['emphasis'] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| font_size_bare_range | container | should | aozora-rs | warning |  | nodes: kind sequence differs
      expected: ['containerOpen', 'containerClose']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| font_size_bare_range | container | should | aozora-epub3 | warning |  | nodes: kind sequence differs
      expected: ['containerOpen', 'containerClose']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| font_size_block | emphasis | should | ab-aozora | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only) |
| font_size_block | emphasis | should | aozora2 | warning |  | nodes: kind sequence differs
      expected: ['containerOpen', 'containerClose']
      got:      ['emphasis'] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| font_size_block | emphasis | should | aozora2html | warning |  | nodes: kind sequence differs
      expected: ['containerOpen', 'containerClose']
      got:      ['emphasis'] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| font_size_block | emphasis | should | aozora-rs | warning |  | nodes: kind sequence differs
      expected: ['containerOpen', 'containerClose']
      got:      ['emphasis'] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| font_size_block | emphasis | should | aozora-epub3 | warning |  | nodes: adapter error: xhtml parse failed: expected 'body' tag, not 'div' at 15:1

Stack backtrace:
   0: anyhow::error::<impl anyhow::Error>::msg
   1: anyhow::__private::format_err
   2: aozora_epub3_adapter::xhtml_mapper::map_to_aat
   3: aozora_epub3_adapter::main
   4: std::sys::backtrace::__rust_begin_short_backtrace
   5: std::rt::lang_start::{{closure}}
   6: std::rt::lang_start_internal
   7: main
   8: __libc_start_call_main
   9: __libc_start_main_alias_2
  10: _start | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| font_size_larger_forward | emphasis | should | ab-aozora | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only) |
| font_size_larger_forward | emphasis | should | aozora2 | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| font_size_larger_forward | emphasis | should | aozora2html | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| font_size_larger_forward | emphasis | should | aozora-rs | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| font_size_larger_forward | emphasis | should | aozora-epub3 | warning |  | nodes: kind sequence differs
      expected: ['emphasis']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| font_size_larger_no_referent | emphasis | should | ab-aozora | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only) |
| font_size_larger_no_referent | emphasis | should | aozora2 | warning |  | nodes: kind sequence differs
      expected: ['emphasis']
      got:      ['directive'] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| font_size_larger_no_referent | emphasis | should | aozora2html | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| font_size_larger_no_referent | emphasis | should | aozora-rs | warning |  | nodes: kind sequence differs
      expected: ['emphasis']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| font_size_larger_no_referent | emphasis | should | aozora-epub3 | warning |  | nodes: kind sequence differs
      expected: ['emphasis']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| font_size_smaller_forward | emphasis | should | ab-aozora | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only) |
| font_size_smaller_forward | emphasis | should | aozora2 | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| font_size_smaller_forward | emphasis | should | aozora2html | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| font_size_smaller_forward | emphasis | should | aozora-rs | warning |  | nodes: kind sequence differs
      expected: ['emphasis']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| font_size_smaller_forward | emphasis | should | aozora-epub3 | warning |  | nodes: kind sequence differs
      expected: ['emphasis']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| forced_break | structural-marker | must | ab-aozora | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only) |
| forced_break | structural-marker | must | aozora2 | fail | nodes: kind sequence differs
      expected: ['forcedBreak']
      got:      [] |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| forced_break | structural-marker | must | aozora2html | fail | nodes: kind sequence differs
      expected: ['forcedBreak']
      got:      [] |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| forced_break | structural-marker | must | aozora-rs | fail | nodes: kind sequence differs
      expected: ['forcedBreak']
      got:      [] |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| forced_break | structural-marker | must | aozora-epub3 | fail | nodes: kind sequence differs
      expected: ['forcedBreak']
      got:      [] |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| fraction_forward | emphasis | should | ab-aozora | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only) |
| fraction_forward | emphasis | should | aozora2 | warning |  | nodes: kind sequence differs
      expected: ['emphasis']
      got:      ['directive'] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| fraction_forward | emphasis | should | aozora2html | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| fraction_forward | emphasis | should | aozora-rs | warning |  | nodes: kind sequence differs
      expected: ['emphasis']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| fraction_forward | emphasis | should | aozora-epub3 | warning |  | nodes: kind sequence differs
      expected: ['emphasis']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| fraction_forward_no_referent | emphasis | should | ab-aozora | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only) |
| fraction_forward_no_referent | emphasis | should | aozora2 | warning |  | nodes: kind sequence differs
      expected: ['emphasis']
      got:      ['directive'] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| fraction_forward_no_referent | emphasis | should | aozora2html | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| fraction_forward_no_referent | emphasis | should | aozora-rs | warning |  | nodes: kind sequence differs
      expected: ['emphasis']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| fraction_forward_no_referent | emphasis | should | aozora-epub3 | warning |  | nodes: kind sequence differs
      expected: ['emphasis']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| gaiji | gaiji | must | ab-aozora | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only) |
| gaiji | gaiji | must | aozora2 | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| gaiji | gaiji | must | aozora2html | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| gaiji | gaiji | must | aozora-rs | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| gaiji | gaiji | must | aozora-epub3 | fail | nodes: kind sequence differs
      expected: ['gaiji']
      got:      [] |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| gaiji_composed_glyph | gaiji | should | ab-aozora | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only) |
| gaiji_composed_glyph | gaiji | should | aozora2 | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| gaiji_composed_glyph | gaiji | should | aozora2html | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| gaiji_composed_glyph | gaiji | should | aozora-rs | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| gaiji_composed_glyph | gaiji | should | aozora-epub3 | warning |  | nodes: kind sequence differs
      expected: ['gaiji']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| gaiji_composed_seiji | gaiji | should | ab-aozora | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only) |
| gaiji_composed_seiji | gaiji | should | aozora2 | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| gaiji_composed_seiji | gaiji | should | aozora2html | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| gaiji_composed_seiji | gaiji | should | aozora-rs | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| gaiji_composed_seiji | gaiji | should | aozora-epub3 | warning |  | nodes: kind sequence differs
      expected: ['gaiji']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| gaiji_double_paren | gaiji | should | ab-aozora | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only) |
| gaiji_double_paren | gaiji | should | aozora2 | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| gaiji_double_paren | gaiji | should | aozora2html | warning |  | nodes: kind sequence differs
      expected: ['gaiji', 'gaiji']
      got:      ['gaiji'] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| gaiji_double_paren | gaiji | should | aozora-rs | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| gaiji_double_paren | gaiji | should | aozora-epub3 | warning |  | nodes: kind sequence differs
      expected: ['gaiji', 'gaiji']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| head_flush_single | container | should | ab-aozora | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only) |
| head_flush_single | container | should | aozora2 | warning |  | nodes: kind sequence differs
      expected: ['containerOpen']
      got:      ['indent'] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| head_flush_single | container | should | aozora2html | warning |  | nodes: kind sequence differs
      expected: ['containerOpen']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| head_flush_single | container | should | aozora-rs | warning |  | nodes: kind sequence differs
      expected: ['containerOpen']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| head_flush_single | container | should | aozora-epub3 | warning |  | nodes: kind sequence differs
      expected: ['containerOpen']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| head_flush_wrap_indent | container | should | ab-aozora | warning |  | nodes: kind sequence differs
      expected: ['containerOpen', 'containerClose']
      got:      ['indent'] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only) |
| head_flush_wrap_indent | container | should | aozora2 | warning |  | nodes: kind sequence differs
      expected: ['containerOpen', 'containerClose']
      got:      ['indent'] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| head_flush_wrap_indent | container | should | aozora2html | warning |  | nodes: kind sequence differs
      expected: ['containerOpen', 'containerClose']
      got:      ['emphasis'] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| head_flush_wrap_indent | container | should | aozora-rs | warning |  | nodes: kind sequence differs
      expected: ['containerOpen', 'containerClose']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| head_flush_wrap_indent | container | should | aozora-epub3 | warning |  | nodes: kind sequence differs
      expected: ['containerOpen', 'containerClose']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| heading | heading | must | ab-aozora | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only) |
| heading | heading | must | aozora2 | fail | nodes: kind sequence differs
      expected: ['directive', 'directive']
      got:      ['directive', 'containerClose'] |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| heading | heading | must | aozora2html | fail | nodes: kind sequence differs
      expected: ['directive', 'directive']
      got:      [] |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| heading | heading | must | aozora-rs | fail | nodes: kind sequence differs
      expected: ['directive', 'directive']
      got:      [] |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| heading | heading | must | aozora-epub3 | fail | nodes: kind sequence differs
      expected: ['directive', 'directive']
      got:      [] |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| heading_block | heading | should | ab-aozora | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only) |
| heading_block | heading | should | aozora2 | warning |  | nodes: kind sequence differs
      expected: ['containerOpen', 'containerClose']
      got:      ['heading', 'containerClose'] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| heading_block | heading | should | aozora2html | warning |  | nodes: kind sequence differs
      expected: ['containerOpen', 'containerClose']
      got:      ['emphasis', 'directive'] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| heading_block | heading | should | aozora-rs | warning |  | nodes: kind sequence differs
      expected: ['containerOpen', 'containerClose']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| heading_block | heading | should | aozora-epub3 | warning |  | nodes: kind sequence differs
      expected: ['containerOpen', 'containerClose']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| heading_forward_no_referent | heading | should | ab-aozora | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only) |
| heading_forward_no_referent | heading | should | aozora2 | warning |  | nodes: kind sequence differs
      expected: ['headingHint']
      got:      ['directive'] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| heading_forward_no_referent | heading | should | aozora2html | warning |  | nodes: kind sequence differs
      expected: ['headingHint']
      got:      ['emphasis'] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| heading_forward_no_referent | heading | should | aozora-rs | warning |  | nodes: kind sequence differs
      expected: ['headingHint']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| heading_forward_no_referent | heading | should | aozora-epub3 | warning |  | nodes: kind sequence differs
      expected: ['headingHint']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| heading_hint | heading | should | ab-aozora | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only) |
| heading_hint | heading | should | aozora2 | warning |  | nodes: kind sequence differs
      expected: ['headingHint']
      got:      ['heading'] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| heading_hint | heading | should | aozora2html | warning |  | nodes: kind sequence differs
      expected: ['headingHint']
      got:      ['emphasis'] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| heading_hint | heading | should | aozora-rs | warning |  | nodes: kind sequence differs
      expected: ['headingHint']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| heading_hint | heading | should | aozora-epub3 | warning |  | nodes: kind sequence differs
      expected: ['headingHint']
      got:      ['heading'] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| heading_paired_window | heading | should | ab-aozora | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only) |
| heading_paired_window | heading | should | aozora2 | warning |  | nodes: kind sequence differs
      expected: ['containerOpen', 'containerClose']
      got:      ['containerClose'] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| heading_paired_window | heading | should | aozora2html | warning |  | nodes: kind sequence differs
      expected: ['containerOpen', 'containerClose']
      got:      ['emphasis'] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| heading_paired_window | heading | should | aozora-rs | warning |  | nodes: kind sequence differs
      expected: ['containerOpen', 'containerClose']
      got:      ['heading', 'heading'] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| heading_paired_window | heading | should | aozora-epub3 | warning |  | nodes: kind sequence differs
      expected: ['containerOpen', 'containerClose']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| heading_promoted | heading | should | ab-aozora | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only) |
| heading_promoted | heading | should | aozora2 | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| heading_promoted | heading | should | aozora2html | warning |  | nodes: kind sequence differs
      expected: ['heading']
      got:      ['emphasis'] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| heading_promoted | heading | should | aozora-rs | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| heading_promoted | heading | should | aozora-epub3 | warning |  | nodes: kind sequence differs
      expected: ['heading']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| heading_ruby_hint | heading | should | ab-aozora | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only) |
| heading_ruby_hint | heading | should | aozora2 | warning |  | nodes: kind sequence differs
      expected: ['ruby', 'ruby', 'headingHint']
      got:      ['heading', 'ruby', 'ruby'] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| heading_ruby_hint | heading | should | aozora2html | warning |  | nodes: kind sequence differs
      expected: ['ruby', 'ruby', 'headingHint']
      got:      ['heading', 'ruby', 'ruby'] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| heading_ruby_hint | heading | should | aozora-rs | warning |  | nodes: kind sequence differs
      expected: ['ruby', 'ruby', 'headingHint']
      got:      ['ruby', 'ruby'] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| heading_ruby_hint | heading | should | aozora-epub3 | warning |  | nodes: kind sequence differs
      expected: ['ruby', 'ruby', 'headingHint']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| heading_same_line | heading | should | ab-aozora | warning |  | nodes: kind sequence differs
      expected: ['headingHint']
      got:      ['heading'] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only) |
| heading_same_line | heading | should | aozora2 | warning |  | nodes: kind sequence differs
      expected: ['headingHint']
      got:      ['heading'] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| heading_same_line | heading | should | aozora2html | warning |  | nodes: kind sequence differs
      expected: ['headingHint']
      got:      ['emphasis'] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| heading_same_line | heading | should | aozora-rs | warning |  | nodes: kind sequence differs
      expected: ['headingHint']
      got:      ['heading'] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| heading_same_line | heading | should | aozora-epub3 | warning |  | nodes: kind sequence differs
      expected: ['headingHint']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| heading_window | heading | should | ab-aozora | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only) |
| heading_window | heading | should | aozora2 | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| heading_window | heading | should | aozora2html | warning |  | nodes: kind sequence differs
      expected: ['heading']
      got:      ['emphasis'] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| heading_window | heading | should | aozora-rs | warning |  | nodes: kind sequence differs
      expected: ['heading']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| heading_window | heading | should | aozora-epub3 | warning |  | nodes: kind sequence differs
      expected: ['heading']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| horizontal_container | horizontal | should | ab-aozora | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only) |
| horizontal_container | horizontal | should | aozora2 | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| horizontal_container | horizontal | should | aozora2html | warning |  | nodes: kind sequence differs
      expected: ['containerOpen', 'containerClose']
      got:      ['emphasis', 'directive'] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| horizontal_container | horizontal | should | aozora-rs | warning |  | nodes: kind sequence differs
      expected: ['containerOpen', 'containerClose']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| horizontal_container | horizontal | should | aozora-epub3 | warning |  | nodes: kind sequence differs
      expected: ['containerOpen', 'containerClose']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| horizontal_inline_forward | horizontal | should | ab-aozora | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only) |
| horizontal_inline_forward | horizontal | should | aozora2 | warning |  | unmapped AAT node kinds: ['yokogumi']<br>nodes: kind sequence differs
      expected: ['emphasis']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| horizontal_inline_forward | horizontal | should | aozora2html | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| horizontal_inline_forward | horizontal | should | aozora-rs | warning |  | nodes: kind sequence differs
      expected: ['emphasis']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| horizontal_inline_forward | horizontal | should | aozora-epub3 | warning |  | nodes: kind sequence differs
      expected: ['emphasis']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| horizontal_inline_forward_no_referent | horizontal | should | ab-aozora | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only) |
| horizontal_inline_forward_no_referent | horizontal | should | aozora2 | warning |  | nodes: kind sequence differs
      expected: ['emphasis']
      got:      ['directive'] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| horizontal_inline_forward_no_referent | horizontal | should | aozora2html | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| horizontal_inline_forward_no_referent | horizontal | should | aozora-rs | warning |  | nodes: kind sequence differs
      expected: ['emphasis']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| horizontal_inline_forward_no_referent | horizontal | should | aozora-epub3 | warning |  | nodes: kind sequence differs
      expected: ['emphasis']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| indent_center | container | should | ab-aozora | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only) |
| indent_center | container | should | aozora2 | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| indent_center | container | should | aozora2html | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| indent_center | container | should | aozora-rs | warning |  | nodes: kind sequence differs
      expected: ['containerOpen', 'containerClose']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| indent_center | container | should | aozora-epub3 | warning |  | nodes: kind sequence differs
      expected: ['containerOpen', 'containerClose']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| indent_compound_styled | layout | must | ab-aozora | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only) |
| indent_compound_styled | layout | must | aozora2 | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| indent_compound_styled | layout | must | aozora2html | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| indent_compound_styled | layout | must | aozora-rs | fail | nodes: kind sequence differs
      expected: ['containerOpen', 'containerClose']
      got:      [] |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| indent_compound_styled | layout | must | aozora-epub3 | fail | nodes: kind sequence differs
      expected: ['containerOpen', 'containerClose']
      got:      [] |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| indent_container | container | must | ab-aozora | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only) |
| indent_container | container | must | aozora2 | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| indent_container | container | must | aozora2html | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| indent_container | container | must | aozora-rs | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| indent_container | container | must | aozora-epub3 | fail | nodes: kind sequence differs
      expected: ['containerOpen', 'containerClose']
      got:      [] |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| indent_from_top | container | should | ab-aozora | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only) |
| indent_from_top | container | should | aozora2 | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| indent_from_top | container | should | aozora2html | warning |  | nodes: kind sequence differs
      expected: ['indent']
      got:      ['containerOpen', 'containerClose'] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| indent_from_top | container | should | aozora-rs | warning |  | nodes: kind sequence differs
      expected: ['indent']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| indent_from_top | container | should | aozora-epub3 | warning |  | nodes: kind sequence differs
      expected: ['indent']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| indent_line_kumi | container | must | ab-aozora | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only) |
| indent_line_kumi | container | must | aozora2 | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| indent_line_kumi | container | must | aozora2html | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| indent_line_kumi | container | must | aozora-rs | fail | nodes: kind sequence differs
      expected: ['containerOpen', 'containerClose']
      got:      [] |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| indent_line_kumi | container | must | aozora-epub3 | fail | nodes: adapter error: exit 2 |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| indent_line_width_compound | container | must | ab-aozora | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only) |
| indent_line_width_compound | container | must | aozora2 | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| indent_line_width_compound | container | must | aozora2html | fail | nodes: kind sequence differs
      expected: ['containerOpen', 'containerClose']
      got:      [] |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| indent_line_width_compound | container | must | aozora-rs | fail | nodes: kind sequence differs
      expected: ['containerOpen', 'containerClose']
      got:      [] |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| indent_line_width_compound | container | must | aozora-epub3 | fail | nodes: kind sequence differs
      expected: ['containerOpen', 'containerClose']
      got:      [] |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| italic_block | emphasis | should | ab-aozora | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only) |
| italic_block | emphasis | should | aozora2 | warning |  | nodes: kind sequence differs
      expected: ['containerOpen', 'containerClose']
      got:      ['emphasis'] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| italic_block | emphasis | should | aozora2html | warning |  | nodes: kind sequence differs
      expected: ['containerOpen', 'containerClose']
      got:      ['emphasis'] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| italic_block | emphasis | should | aozora-rs | warning |  | nodes: kind sequence differs
      expected: ['containerOpen', 'containerClose']
      got:      ['emphasis'] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| italic_block | emphasis | should | aozora-epub3 | warning |  | nodes: kind sequence differs
      expected: ['containerOpen', 'containerClose']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| italic_forward | emphasis | should | ab-aozora | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only) |
| italic_forward | emphasis | should | aozora2 | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| italic_forward | emphasis | should | aozora2html | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| italic_forward | emphasis | should | aozora-rs | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| italic_forward | emphasis | should | aozora-epub3 | warning |  | nodes: kind sequence differs
      expected: ['emphasis']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| italic_forward_no_referent | emphasis | should | ab-aozora | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only) |
| italic_forward_no_referent | emphasis | should | aozora2 | warning |  | nodes: kind sequence differs
      expected: ['emphasis']
      got:      ['directive'] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| italic_forward_no_referent | emphasis | should | aozora2html | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| italic_forward_no_referent | emphasis | should | aozora-rs | warning |  | nodes: kind sequence differs
      expected: ['emphasis']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| italic_forward_no_referent | emphasis | should | aozora-epub3 | warning |  | nodes: kind sequence differs
      expected: ['emphasis']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| italic_inline | emphasis | should | ab-aozora | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only) |
| italic_inline | emphasis | should | aozora2 | warning |  | nodes: kind sequence differs
      expected: ['emphasis']
      got:      ['containerOpen', 'containerClose'] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| italic_inline | emphasis | should | aozora2html | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| italic_inline | emphasis | should | aozora-rs | warning |  | nodes: kind sequence differs
      expected: ['emphasis']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| italic_inline | emphasis | should | aozora-epub3 | warning |  | nodes: kind sequence differs
      expected: ['emphasis']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| kaeriten | kaeriten | should | ab-aozora | warning |  | nodes: kind sequence differs
      expected: ['directive']
      got:      ['kaeriten'] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only) |
| kaeriten | kaeriten | should | aozora2 | warning |  | nodes: kind sequence differs
      expected: ['directive']
      got:      ['kaeriten'] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| kaeriten | kaeriten | should | aozora2html | warning |  | nodes: kind sequence differs
      expected: ['directive']
      got:      ['kaeriten'] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| kaeriten | kaeriten | should | aozora-rs | warning |  | nodes: kind sequence differs
      expected: ['directive']
      got:      ['kaeriten'] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| kaeriten | kaeriten | should | aozora-epub3 | warning |  | nodes: kind sequence differs
      expected: ['directive']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| kaeriten_outside_kanbun | kunten | may | ab-aozora | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only) |
| kaeriten_outside_kanbun | kunten | may | aozora2 | warning |  | nodes: kind sequence differs
      expected: ['kaeriten']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| kaeriten_outside_kanbun | kunten | may | aozora2html | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| kaeriten_outside_kanbun | kunten | may | aozora-rs | warning |  | nodes: kind sequence differs
      expected: ['kaeriten']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| kaeriten_outside_kanbun | kunten | may | aozora-epub3 | warning |  | nodes: kind sequence differs
      expected: ['kaeriten']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| keigakomi_container | container | must | ab-aozora | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only) |
| keigakomi_container | container | must | aozora2 | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| keigakomi_container | container | must | aozora2html | fail | nodes: kind sequence differs
      expected: ['containerOpen', 'containerClose']
      got:      [] |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| keigakomi_container | container | must | aozora-rs | fail | nodes: kind sequence differs
      expected: ['containerOpen', 'containerClose']
      got:      [] |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| keigakomi_container | container | must | aozora-epub3 | fail | nodes: kind sequence differs
      expected: ['containerOpen', 'containerClose']
      got:      [] |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| keigakomi_inline_forward | keigakomi | should | ab-aozora | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only) |
| keigakomi_inline_forward | keigakomi | should | aozora2 | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| keigakomi_inline_forward | keigakomi | should | aozora2html | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| keigakomi_inline_forward | keigakomi | should | aozora-rs | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| keigakomi_inline_forward | keigakomi | should | aozora-epub3 | warning |  | nodes: kind sequence differs
      expected: ['emphasis']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| keigakomi_inline_framed | keigakomi | should | ab-aozora | warning |  | nodes: kind sequence differs
      expected: ['emphasis']
      got:      ['directive']<br>diagnostics: expected [], got [{'code': 'non-canonical-directive', 'severity': 'warning', 'span': {'start': 12, 'end': 51}}] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only) |
| keigakomi_inline_framed | keigakomi | should | aozora2 | warning |  | nodes: kind sequence differs
      expected: ['emphasis']
      got:      ['directive'] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| keigakomi_inline_framed | keigakomi | should | aozora2html | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| keigakomi_inline_framed | keigakomi | should | aozora-rs | warning |  | nodes: kind sequence differs
      expected: ['emphasis']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| keigakomi_inline_framed | keigakomi | should | aozora-epub3 | warning |  | nodes: kind sequence differs
      expected: ['emphasis']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| left_ruby | kunten | should | ab-aozora | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only) |
| left_ruby | kunten | should | aozora2 | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| left_ruby | kunten | should | aozora2html | warning |  | nodes: kind sequence differs
      expected: ['ruby']
      got:      ['emphasis'] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| left_ruby | kunten | should | aozora-rs | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| left_ruby | kunten | should | aozora-epub3 | warning |  | nodes: kind sequence differs
      expected: ['ruby']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| left_ruby_pair | annotation | should | ab-aozora | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only) |
| left_ruby_pair | annotation | should | aozora2 | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| left_ruby_pair | annotation | should | aozora2html | warning |  | nodes: kind sequence differs
      expected: ['directive', 'directive']
      got:      ['emphasis', 'emphasis'] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| left_ruby_pair | annotation | should | aozora-rs | warning |  | nodes: kind sequence differs
      expected: ['directive', 'directive']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| left_ruby_pair | annotation | should | aozora-epub3 | warning |  | nodes: kind sequence differs
      expected: ['directive', 'directive']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| line_bold_single | layout | should | ab-aozora | warning |  | nodes: kind sequence differs
      expected: ['lineBold']
      got:      ['lineGothic'] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only) |
| line_bold_single | layout | should | aozora2 | warning |  | nodes: kind sequence differs
      expected: ['lineBold']
      got:      ['directive'] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| line_bold_single | layout | should | aozora2html | warning |  | nodes: kind sequence differs
      expected: ['lineBold']
      got:      ['emphasis'] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| line_bold_single | layout | should | aozora-rs | warning |  | nodes: kind sequence differs
      expected: ['lineBold']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| line_bold_single | layout | should | aozora-epub3 | warning |  | nodes: kind sequence differs
      expected: ['lineBold']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| line_font_size_bold_single | layout | should | ab-aozora | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only) |
| line_font_size_bold_single | layout | should | aozora2 | warning |  | nodes: kind sequence differs
      expected: ['lineFontSize']
      got:      ['directive'] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| line_font_size_bold_single | layout | should | aozora2html | warning |  | nodes: kind sequence differs
      expected: ['lineFontSize']
      got:      ['emphasis'] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| line_font_size_bold_single | layout | should | aozora-rs | warning |  | nodes: kind sequence differs
      expected: ['lineFontSize']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| line_font_size_bold_single | layout | should | aozora-epub3 | warning |  | nodes: kind sequence differs
      expected: ['lineFontSize']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| line_font_size_single | layout | should | ab-aozora | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only) |
| line_font_size_single | layout | should | aozora2 | warning |  | nodes: kind sequence differs
      expected: ['lineFontSize']
      got:      ['directive'] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| line_font_size_single | layout | should | aozora2html | warning |  | nodes: kind sequence differs
      expected: ['lineFontSize']
      got:      ['emphasis'] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| line_font_size_single | layout | should | aozora-rs | warning |  | nodes: kind sequence differs
      expected: ['lineFontSize']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| line_font_size_single | layout | should | aozora-epub3 | warning |  | nodes: kind sequence differs
      expected: ['lineFontSize']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| line_width_container | container | must | ab-aozora | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only) |
| line_width_container | container | must | aozora2 | fail | nodes: kind sequence differs
      expected: ['containerOpen', 'containerClose']
      got:      ['indent'] |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| line_width_container | container | must | aozora2html | fail | nodes: kind sequence differs
      expected: ['containerOpen', 'containerClose']
      got:      ['emphasis', 'directive', 'directive'] |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| line_width_container | container | must | aozora-rs | fail | nodes: kind sequence differs
      expected: ['containerOpen', 'containerClose']
      got:      [] |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| line_width_container | container | must | aozora-epub3 | fail | nodes: adapter error: xhtml parse failed: expected 'body' tag, not 'div' at 16:1

Stack backtrace:
   0: anyhow::error::<impl anyhow::Error>::msg
   1: anyhow::__private::format_err
   2: aozora_epub3_adapter::xhtml_mapper::map_to_aat
   3: aozora_epub3_adapter::main
   4: std::sys::backtrace::__rust_begin_short_backtrace
   5: std::rt::lang_start::{{closure}}
   6: std::rt::lang_start_internal
   7: main
   8: __libc_start_call_main
   9: __libc_start_main_alias_2
  10: _start |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| mismatched_bouten_container | bouten | should | ab-aozora | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only) |
| mismatched_bouten_container | bouten | should | aozora2 | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| mismatched_bouten_container | bouten | should | aozora2html | warning |  | nodes: kind sequence differs
      expected: ['containerOpen', 'containerClose']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| mismatched_bouten_container | bouten | should | aozora-rs | warning |  | nodes: kind sequence differs
      expected: ['containerOpen', 'containerClose']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| mismatched_bouten_container | bouten | should | aozora-epub3 | warning |  | nodes: kind sequence differs
      expected: ['containerOpen', 'containerClose']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| mismatched_container_close | container | should | ab-aozora | warning |  | nodes: kind sequence differs
      expected: ['containerOpen', 'containerClose']
      got:      ['containerOpen', 'containerClose', 'containerClose'] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only) |
| mismatched_container_close | container | should | aozora2 | warning |  | nodes: kind sequence differs
      expected: ['containerOpen', 'containerClose']
      got:      ['containerClose', 'containerOpen'] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| mismatched_container_close | container | should | aozora2html | warning |  | nodes: kind sequence differs
      expected: ['containerOpen', 'containerClose']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| mismatched_container_close | container | should | aozora-rs | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| mismatched_container_close | container | should | aozora-epub3 | warning |  | nodes: kind sequence differs
      expected: ['containerOpen', 'containerClose']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| mixed_ruby_bouten | composite | must | ab-aozora | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only) |
| mixed_ruby_bouten | composite | must | aozora2 | fail | nodes: kind sequence differs
      expected: ['ruby', 'bouten']
      got:      ['bouten', 'ruby'] |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| mixed_ruby_bouten | composite | must | aozora2html | fail | nodes: kind sequence differs
      expected: ['ruby', 'bouten']
      got:      ['ruby', 'emphasis'] |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| mixed_ruby_bouten | composite | must | aozora-rs | fail | nodes: kind sequence differs
      expected: ['ruby', 'bouten']
      got:      ['ruby'] |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| mixed_ruby_bouten | composite | must | aozora-epub3 | fail | nodes: kind sequence differs
      expected: ['ruby', 'bouten']
      got:      [] |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| nested_containers | composite | should | ab-aozora | warning |  | nodes: kind sequence differs
      expected: ['containerOpen', 'containerOpen', 'containerClose', 'containerClose']
      got:      ['containerOpen', 'containerClose', 'containerOpen', 'containerClose', 'containerClose'] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only) |
| nested_containers | composite | should | aozora2 | warning |  | nodes: kind sequence differs
      expected: ['containerOpen', 'containerOpen', 'containerClose', 'containerClose']
      got:      ['containerOpen', 'containerClose'] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| nested_containers | composite | should | aozora2html | warning |  | nodes: kind sequence differs
      expected: ['containerOpen', 'containerOpen', 'containerClose', 'containerClose']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| nested_containers | composite | should | aozora-rs | warning |  | nodes: kind sequence differs
      expected: ['containerOpen', 'containerOpen', 'containerClose', 'containerClose']
      got:      ['containerOpen', 'containerClose', 'containerOpen', 'containerClose'] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| nested_containers | composite | should | aozora-epub3 | warning |  | nodes: kind sequence differs
      expected: ['containerOpen', 'containerOpen', 'containerClose', 'containerClose']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| nested_ruby | ruby | should | ab-aozora | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only) |
| nested_ruby | ruby | should | aozora2 | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| nested_ruby | ruby | should | aozora2html | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| nested_ruby | ruby | should | aozora-rs | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| nested_ruby | ruby | should | aozora-epub3 | warning |  | nodes: kind sequence differs
      expected: ['ruby']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| page_break | break | must | ab-aozora | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only) |
| page_break | break | must | aozora2 | fail | nodes: kind sequence differs
      expected: ['pageBreak']
      got:      [] |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| page_break | break | must | aozora2html | fail | nodes: kind sequence differs
      expected: ['pageBreak']
      got:      [] |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| page_break | break | must | aozora-rs | fail | nodes: kind sequence differs
      expected: ['pageBreak']
      got:      [] |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| page_break | break | must | aozora-epub3 | fail | nodes: kind sequence differs
      expected: ['pageBreak']
      got:      [] |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| pageful | composite | may | ab-aozora | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only) |
| pageful | composite | may | aozora2 | warning |  | nodes: kind sequence differs
      expected: ['ruby', 'gaiji', 'containerOpen', 'containerClose', 'pageBreak']
      got:      ['ruby', 'gaiji', 'containerOpen', 'containerClose'] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| pageful | composite | may | aozora2html | warning |  | nodes: kind sequence differs
      expected: ['ruby', 'gaiji', 'containerOpen', 'containerClose', 'pageBreak']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| pageful | composite | may | aozora-rs | warning |  | nodes: kind sequence differs
      expected: ['ruby', 'gaiji', 'containerOpen', 'containerClose', 'pageBreak']
      got:      ['ruby', 'gaiji', 'containerOpen', 'containerClose'] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| pageful | composite | may | aozora-epub3 | warning |  | nodes: kind sequence differs
      expected: ['ruby', 'gaiji', 'containerOpen', 'containerClose', 'pageBreak']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| plain_text | plain | must | ab-aozora | pass |  |  | nodes: vector has no expected.nodes<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only) |
| plain_text | plain | must | aozora2 | skip |  |  | nodes: vector has no expected.nodes<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| plain_text | plain | must | aozora2html | skip |  |  | nodes: vector has no expected.nodes<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| plain_text | plain | must | aozora-rs | skip |  |  | nodes: vector has no expected.nodes<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| plain_text | plain | must | aozora-epub3 | skip |  |  | nodes: vector has no expected.nodes<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| pua_collision | recovery | must | ab-aozora | pass |  |  | nodes: vector has no expected.nodes<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only) |
| pua_collision | recovery | must | aozora2 | skip |  |  | nodes: vector has no expected.nodes<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| pua_collision | recovery | must | aozora2html | skip |  |  | nodes: vector has no expected.nodes<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| pua_collision | recovery | must | aozora-rs | skip |  |  | nodes: vector has no expected.nodes<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| pua_collision | recovery | must | aozora-epub3 | skip |  |  | nodes: vector has no expected.nodes<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| roman_numeral | gaiji | should | ab-aozora | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only) |
| roman_numeral | gaiji | should | aozora2 | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| roman_numeral | gaiji | should | aozora2html | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| roman_numeral | gaiji | should | aozora-rs | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| roman_numeral | gaiji | should | aozora-epub3 | warning |  | nodes: kind sequence differs
      expected: ['gaiji']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| ruby_attached | annotation | should | ab-aozora | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only) |
| ruby_attached | annotation | should | aozora2 | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| ruby_attached | annotation | should | aozora2html | warning |  | nodes: kind sequence differs
      expected: ['directive']
      got:      ['emphasis'] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| ruby_attached | annotation | should | aozora-rs | warning |  | nodes: kind sequence differs
      expected: ['directive']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| ruby_attached | annotation | should | aozora-epub3 | warning |  | nodes: kind sequence differs
      expected: ['directive']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| ruby_explicit | ruby | must | ab-aozora | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only) |
| ruby_explicit | ruby | must | aozora2 | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| ruby_explicit | ruby | must | aozora2html | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| ruby_explicit | ruby | must | aozora-rs | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| ruby_explicit | ruby | must | aozora-epub3 | fail | nodes: kind sequence differs
      expected: ['ruby']
      got:      [] |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| ruby_implicit | ruby | must | ab-aozora | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only) |
| ruby_implicit | ruby | must | aozora2 | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| ruby_implicit | ruby | must | aozora2html | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| ruby_implicit | ruby | must | aozora-rs | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| ruby_implicit | ruby | must | aozora-epub3 | fail | nodes: kind sequence differs
      expected: ['ruby']
      got:      [] |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| ruby_retarget | annotation | should | ab-aozora | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only) |
| ruby_retarget | annotation | should | aozora2 | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| ruby_retarget | annotation | should | aozora2html | warning |  | nodes: kind sequence differs
      expected: ['directive']
      got:      ['emphasis'] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| ruby_retarget | annotation | should | aozora-rs | warning |  | nodes: kind sequence differs
      expected: ['directive']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| ruby_retarget | annotation | should | aozora-epub3 | warning |  | nodes: kind sequence differs
      expected: ['directive']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| sashie | sashie | must | ab-aozora | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only) |
| sashie | sashie | must | aozora2 | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| sashie | sashie | must | aozora2html | fail | nodes: kind sequence differs
      expected: ['illustration']
      got:      ['emphasis'] |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| sashie | sashie | must | aozora-rs | fail | nodes: kind sequence differs
      expected: ['illustration']
      got:      [] |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| sashie | sashie | must | aozora-epub3 | fail | nodes: kind sequence differs
      expected: ['illustration']
      got:      [] |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| sashie_caption | sashie | should | ab-aozora | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only) |
| sashie_caption | sashie | should | aozora2 | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| sashie_caption | sashie | should | aozora2html | warning |  | nodes: kind sequence differs
      expected: ['illustration']
      got:      ['emphasis'] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| sashie_caption | sashie | should | aozora-rs | warning |  | nodes: kind sequence differs
      expected: ['illustration']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| sashie_caption | sashie | should | aozora-epub3 | warning |  | nodes: kind sequence differs
      expected: ['illustration']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| sashie_dimensions | sashie | should | ab-aozora | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only) |
| sashie_dimensions | sashie | should | aozora2 | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| sashie_dimensions | sashie | should | aozora2html | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| sashie_dimensions | sashie | should | aozora-rs | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| sashie_dimensions | sashie | should | aozora-epub3 | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| section_break | break | must | ab-aozora | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only) |
| section_break | break | must | aozora2 | fail | nodes: kind sequence differs
      expected: ['sectionBreak']
      got:      ['directive'] |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| section_break | break | must | aozora2html | fail | nodes: kind sequence differs
      expected: ['sectionBreak']
      got:      ['emphasis'] |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| section_break | break | must | aozora-rs | fail | nodes: kind sequence differs
      expected: ['sectionBreak']
      got:      [] |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| section_break | break | must | aozora-epub3 | fail | nodes: kind sequence differs
      expected: ['sectionBreak']
      got:      [] |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| side_glyph_left | emphasis | should | ab-aozora | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only) |
| side_glyph_left | emphasis | should | aozora2 | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| side_glyph_left | emphasis | should | aozora2html | warning |  | nodes: kind sequence differs
      expected: ['emphasis']
      got:      ['kaeriten'] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| side_glyph_left | emphasis | should | aozora-rs | warning |  | nodes: kind sequence differs
      expected: ['emphasis']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| side_glyph_left | emphasis | should | aozora-epub3 | warning |  | nodes: kind sequence differs
      expected: ['emphasis']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| side_glyph_left_no_referent | emphasis | should | ab-aozora | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only) |
| side_glyph_left_no_referent | emphasis | should | aozora2 | warning |  | nodes: kind sequence differs
      expected: ['emphasis']
      got:      ['directive'] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| side_glyph_left_no_referent | emphasis | should | aozora2html | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| side_glyph_left_no_referent | emphasis | should | aozora-rs | warning |  | nodes: kind sequence differs
      expected: ['emphasis']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| side_glyph_left_no_referent | emphasis | should | aozora-epub3 | warning |  | nodes: kind sequence differs
      expected: ['emphasis']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| side_glyph_left_range | emphasis | should | ab-aozora | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only) |
| side_glyph_left_range | emphasis | should | aozora2 | warning |  | nodes: kind sequence differs
      expected: ['emphasis']
      got:      ['containerOpen', 'containerClose'] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| side_glyph_left_range | emphasis | should | aozora2html | warning |  | nodes: kind sequence differs
      expected: ['emphasis']
      got:      ['kaeriten'] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| side_glyph_left_range | emphasis | should | aozora-rs | warning |  | nodes: kind sequence differs
      expected: ['emphasis']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| side_glyph_left_range | emphasis | should | aozora-epub3 | warning |  | nodes: kind sequence differs
      expected: ['emphasis']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| side_glyph_right | emphasis | should | ab-aozora | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only) |
| side_glyph_right | emphasis | should | aozora2 | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| side_glyph_right | emphasis | should | aozora2html | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| side_glyph_right | emphasis | should | aozora-rs | warning |  | nodes: kind sequence differs
      expected: ['emphasis']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| side_glyph_right | emphasis | should | aozora-epub3 | warning |  | nodes: kind sequence differs
      expected: ['emphasis']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| side_glyph_right_no_referent | emphasis | should | ab-aozora | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only) |
| side_glyph_right_no_referent | emphasis | should | aozora2 | warning |  | nodes: kind sequence differs
      expected: ['emphasis']
      got:      ['directive'] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| side_glyph_right_no_referent | emphasis | should | aozora2html | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| side_glyph_right_no_referent | emphasis | should | aozora-rs | warning |  | nodes: kind sequence differs
      expected: ['emphasis']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| side_glyph_right_no_referent | emphasis | should | aozora-epub3 | warning |  | nodes: kind sequence differs
      expected: ['emphasis']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| side_glyph_right_range | emphasis | should | ab-aozora | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only) |
| side_glyph_right_range | emphasis | should | aozora2 | warning |  | nodes: kind sequence differs
      expected: ['emphasis']
      got:      ['containerOpen', 'containerClose'] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| side_glyph_right_range | emphasis | should | aozora2html | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| side_glyph_right_range | emphasis | should | aozora-rs | warning |  | nodes: kind sequence differs
      expected: ['emphasis']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| side_glyph_right_range | emphasis | should | aozora-epub3 | warning |  | nodes: kind sequence differs
      expected: ['emphasis']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| side_note_left | kunten | should | ab-aozora | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only) |
| side_note_left | kunten | should | aozora2 | warning |  | nodes: kind sequence differs
      expected: ['marginNote']
      got:      ['directive'] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| side_note_left | kunten | should | aozora2html | warning |  | nodes: kind sequence differs
      expected: ['marginNote']
      got:      ['emphasis'] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| side_note_left | kunten | should | aozora-rs | warning |  | nodes: kind sequence differs
      expected: ['marginNote']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| side_note_left | kunten | should | aozora-epub3 | warning |  | nodes: kind sequence differs
      expected: ['marginNote']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| standalone_gaiji | gaiji | should | ab-aozora | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only) |
| standalone_gaiji | gaiji | should | aozora2 | warning |  | nodes: kind sequence differs
      expected: ['gaiji']
      got:      ['directive'] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| standalone_gaiji | gaiji | should | aozora2html | warning |  | nodes: kind sequence differs
      expected: ['gaiji']
      got:      ['emphasis'] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| standalone_gaiji | gaiji | should | aozora-rs | warning |  | nodes: kind sequence differs
      expected: ['gaiji']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| standalone_gaiji | gaiji | should | aozora-epub3 | warning |  | nodes: kind sequence differs
      expected: ['gaiji']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| subscript_forward | emphasis | should | ab-aozora | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only) |
| subscript_forward | emphasis | should | aozora2 | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| subscript_forward | emphasis | should | aozora2html | warning |  | nodes: kind sequence differs
      expected: ['emphasis']
      got:      ['kaeriten'] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| subscript_forward | emphasis | should | aozora-rs | warning |  | nodes: kind sequence differs
      expected: ['emphasis']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| subscript_forward | emphasis | should | aozora-epub3 | warning |  | nodes: kind sequence differs
      expected: ['emphasis']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| subscript_forward_no_referent | emphasis | should | ab-aozora | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only) |
| subscript_forward_no_referent | emphasis | should | aozora2 | warning |  | nodes: kind sequence differs
      expected: ['emphasis']
      got:      ['directive'] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| subscript_forward_no_referent | emphasis | should | aozora2html | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| subscript_forward_no_referent | emphasis | should | aozora-rs | warning |  | nodes: kind sequence differs
      expected: ['emphasis']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| subscript_forward_no_referent | emphasis | should | aozora-epub3 | warning |  | nodes: kind sequence differs
      expected: ['emphasis']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| superscript_forward | emphasis | should | ab-aozora | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only) |
| superscript_forward | emphasis | should | aozora2 | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| superscript_forward | emphasis | should | aozora2html | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| superscript_forward | emphasis | should | aozora-rs | warning |  | nodes: kind sequence differs
      expected: ['emphasis']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| superscript_forward | emphasis | should | aozora-epub3 | warning |  | nodes: kind sequence differs
      expected: ['emphasis']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| superscript_forward_no_referent | emphasis | should | ab-aozora | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only) |
| superscript_forward_no_referent | emphasis | should | aozora2 | warning |  | nodes: kind sequence differs
      expected: ['emphasis']
      got:      ['directive'] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| superscript_forward_no_referent | emphasis | should | aozora2html | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| superscript_forward_no_referent | emphasis | should | aozora-rs | warning |  | nodes: kind sequence differs
      expected: ['emphasis']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| superscript_forward_no_referent | emphasis | should | aozora-epub3 | warning |  | nodes: kind sequence differs
      expected: ['emphasis']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| table_container | tables_columns | should | ab-aozora | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only) |
| table_container | tables_columns | should | aozora2 | warning |  | nodes: kind sequence differs
      expected: ['containerOpen', 'containerClose']
      got:      ['directive', 'directive'] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| table_container | tables_columns | should | aozora2html | warning |  | nodes: kind sequence differs
      expected: ['containerOpen', 'containerClose']
      got:      ['emphasis', 'emphasis'] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| table_container | tables_columns | should | aozora-rs | warning |  | nodes: kind sequence differs
      expected: ['containerOpen', 'containerClose']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| table_container | tables_columns | should | aozora-epub3 | warning |  | nodes: kind sequence differs
      expected: ['containerOpen', 'containerClose']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| tate_chu_yoko | tcy | must | ab-aozora | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only) |
| tate_chu_yoko | tcy | must | aozora2 | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| tate_chu_yoko | tcy | must | aozora2html | fail | nodes: kind sequence differs
      expected: ['directive']
      got:      ['emphasis'] |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| tate_chu_yoko | tcy | must | aozora-rs | fail | nodes: kind sequence differs
      expected: ['directive']
      got:      [] |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| tate_chu_yoko | tcy | must | aozora-epub3 | fail | nodes: kind sequence differs
      expected: ['directive']
      got:      [] |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| tate_chu_yoko_found | tate_chu_yoko | must | ab-aozora | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only) |
| tate_chu_yoko_found | tate_chu_yoko | must | aozora2 | fail | nodes: kind sequence differs
      expected: ['combineUpright', 'combineUpright']
      got:      ['combineUpright'] |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| tate_chu_yoko_found | tate_chu_yoko | must | aozora2html | fail | nodes: kind sequence differs
      expected: ['combineUpright', 'combineUpright']
      got:      ['emphasis'] |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| tate_chu_yoko_found | tate_chu_yoko | must | aozora-rs | fail | nodes: kind sequence differs
      expected: ['combineUpright', 'combineUpright']
      got:      [] |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| tate_chu_yoko_found | tate_chu_yoko | must | aozora-epub3 | fail | nodes: kind sequence differs
      expected: ['combineUpright', 'combineUpright']
      got:      [] |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| unclosed_bracket | recovery | must | ab-aozora | pass |  |  | nodes: vector has no expected.nodes<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only) |
| unclosed_bracket | recovery | must | aozora2 | skip |  |  | nodes: vector has no expected.nodes<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| unclosed_bracket | recovery | must | aozora2html | skip |  |  | nodes: vector has no expected.nodes<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| unclosed_bracket | recovery | must | aozora-rs | skip |  |  | nodes: vector has no expected.nodes<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| unclosed_bracket | recovery | must | aozora-epub3 | skip |  |  | nodes: vector has no expected.nodes<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| unrecognised_container_directive | container | should | ab-aozora | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only) |
| unrecognised_container_directive | container | should | aozora2 | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| unrecognised_container_directive | container | should | aozora2html | warning |  | nodes: kind sequence differs
      expected: ['directive']
      got:      ['emphasis'] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| unrecognised_container_directive | container | should | aozora-rs | warning |  | nodes: kind sequence differs
      expected: ['directive']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| unrecognised_container_directive | container | should | aozora-epub3 | warning |  | nodes: kind sequence differs
      expected: ['directive']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| unresolved_gaiji | gaiji | should | ab-aozora | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only) |
| unresolved_gaiji | gaiji | should | aozora2 | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| unresolved_gaiji | gaiji | should | aozora2html | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| unresolved_gaiji | gaiji | should | aozora-rs | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| unresolved_gaiji | gaiji | should | aozora-epub3 | warning |  | nodes: kind sequence differs
      expected: ['gaiji']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| warichu_inline | warichu | must | ab-aozora | pass |  |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only) |
| warichu_inline | warichu | must | aozora2 | fail | nodes: kind sequence differs
      expected: ['directive', 'directive']
      got:      [] | unmapped AAT node kinds: ['warigaki'] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| warichu_inline | warichu | must | aozora2html | fail | nodes: kind sequence differs
      expected: ['directive', 'directive']
      got:      [] | unmapped AAT node kinds: ['warigaki'] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| warichu_inline | warichu | must | aozora-rs | fail | nodes: kind sequence differs
      expected: ['directive', 'directive']
      got:      [] | unmapped AAT node kinds: ['warigaki'] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| warichu_inline | warichu | must | aozora-epub3 | fail | nodes: kind sequence differs
      expected: ['directive', 'directive']
      got:      [] |  | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| wrap_indent | container | should | ab-aozora | warning |  | nodes: kind sequence differs
      expected: ['containerOpen', 'containerClose']
      got:      ['indent'] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only) |
| wrap_indent | container | should | aozora2 | warning |  | nodes: kind sequence differs
      expected: ['containerOpen', 'containerClose']
      got:      ['indent'] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| wrap_indent | container | should | aozora2html | warning |  | nodes: kind sequence differs
      expected: ['containerOpen', 'containerClose']
      got:      ['indent'] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| wrap_indent | container | should | aozora-rs | warning |  | nodes: kind sequence differs
      expected: ['containerOpen', 'containerClose']
      got:      ['indent'] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
| wrap_indent | container | should | aozora-epub3 | warning |  | nodes: kind sequence differs
      expected: ['containerOpen', 'containerClose']
      got:      [] | pairs: not comparable for AAT adapter (kind-sequence only)<br>serialize: not comparable for AAT adapter (kind-sequence only)<br>html: not comparable for AAT adapter (kind-sequence only)<br>diagnostics: not comparable for AAT adapter (kind-sequence only) |
