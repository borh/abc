## 2026-07-06 IR Publication Coverage final review fixes

- Added top-level `field_coverage` to the JSON summary and Markdown report, with explicit node/paragraph/contract field facts for gaiji, ruby, heading, emphasis, paragraph, source-note, caption, quote, mapping, divergence, and source pointer coverage.
- Fixed construct classification so clean parser-IR pointers win when present, `caption` is classified as `tei_policy_projection`, and `image.src` evidence lands under supported `image` coverage instead of falling through as an unsupported `figure`/filename token.
- Tightened parser evidence counting to require adapter selection, parser-IR schema hash, numeric node count, and passed materialization when materialization exists; source-delta `missing_parsers` now forces `FIVE_PARSER_EVIDENCE_INCOMPLETE`.
- Reduced local custom-contract policy: readable JSON is now `CUSTOM_CONTRACT_CANDIDATE_PROVIDED` evidence only, invalid JSON remains `CUSTOM_CONTRACT_INVALID`, and the headline verdict stays blocked on missing ABC-owned contract integration.
- Regenerated `docs/superpowers/reports/2026-07-06-ir-publication-coverage.summary.json` and `docs/superpowers/reports/2026-07-06-ir-publication-coverage.md`.
- Updated the ABC handoff to state that a supplied contract candidate path does not unblock admission.
