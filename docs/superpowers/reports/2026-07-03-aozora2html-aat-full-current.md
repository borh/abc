# aozora2html Current-Adapter Full-Corpus AAT Run

Date: 2026-07-03

Run directory: `/db/ab-validator/aat-corpus/aozora2html-full-20260703T020301Z`

## Summary

| Metric | Value |
|---|---:|
| indexed works | 17886 |
| check reports | 17886 |
| persisted AAT files | 17689 |
| schema invalid or missing reports | 197 |
| reports with failures | 1012 |
| total failures | 1287 |
| parse incomplete or missing reports | 302 |
| reports with source-derived nodes | 4778 |
| source-derived node observations | 33773 |
| warigaki works | 243 |
| warigaki nodes | 4050 |
| kunten works with AAT observations | 472 |
| kunten AAT observations | 22504 |

## Interpretation

- Warigaki policy evidence: aozora2html emitted 4050 warigaki observations across 243 works in this current-adapter run.
- Kunten Gap B evidence: current AAT output contains 22504 kunten observations across 472 works after the real-spelling detector fix.
- Schema-only adapter-boundary evidence: aozora2html produced 17689 persisted AAT files without depending on workspace `ab-*` crates.
- Style/source-derived evidence: the triage recorded 33773 source-derived node observations for follow-up unsupported/style analysis.
- Completeness caveat: 197 reports were schema-invalid or missing, and 302 reports were parse-incomplete or missing; policy conclusions should account for those failures.

## Reproduction

- command: `just aozora2html-aat-full "" 24 180s "aozora2html-full-2026-07-03"`
- repo head: `3b48a1524845c3f3ca11e76179f5df329c3fc64e`
- adapter version: `aozora2html-adapter 0.1.0 gem-3.0.1`
- worktree status captured at metadata time: `M flake.nix
?? crates/README.md
?? docs/handoffs/`
