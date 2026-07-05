## 2026-07-06 IR Publication Coverage final review fixes

- Added top-level `field_coverage` to the JSON summary and Markdown report, with explicit node/paragraph/contract field facts for gaiji, ruby, heading, emphasis, paragraph, source-note, caption, quote, mapping, divergence, and source pointer coverage.
- Fixed construct classification so clean parser-IR pointers win when present, `caption` is classified as `tei_policy_projection`, and `image.src` evidence lands under supported `image` coverage instead of falling through as an unsupported `figure`/filename token.
- Tightened parser evidence counting to require adapter selection, parser-IR schema hash, numeric node count, and passed materialization when materialization exists; source-delta `missing_parsers` now forces `FIVE_PARSER_EVIDENCE_INCOMPLETE`.
- Reduced local custom-contract policy: readable JSON is now `CUSTOM_CONTRACT_CANDIDATE_PROVIDED` evidence only, invalid JSON remains `CUSTOM_CONTRACT_INVALID`, and the headline verdict stays blocked on missing ABC-owned contract integration.
- Regenerated `docs/superpowers/reports/2026-07-06-ir-publication-coverage.summary.json` and `docs/superpowers/reports/2026-07-06-ir-publication-coverage.md`.
- Updated the ABC handoff to state that a supplied contract candidate path does not unblock admission.

## 2026-07-06 IR Publication Coverage final re-review fixes

- Added required top-level `scope` to the JSON summary with reproducibility boundary fields: `kind`, `source_authority_works_scanned`, `required_parsers`, `matrix_rows_attempted`, `mapping_id`, `mapping_version`, `parser_ir_schema_hash`, and `custom_contract_required`.
- Tightened the smoke harness to assert `.scope.kind == "ir_publication_coverage"` and to require the five active parsers under `.scope.required_parsers`.
- Replaced the hardcoded source-construct unsupported owner with `unsupported_owner(rule, construct)`, mapping categories to owners as requested: `UNSUPPORTED -> parser_ir_schema`, `LOSS -> custom_schema`, `STRUCTURAL -> aat_to_parser_ir_converter`, `AMBIGUITY -> policy`, fallback `-> evidence`.
- Added unsupported ownership coverage in the smoke test with explicit fixture rules for each owner category and regenerated `docs/superpowers/reports/2026-07-06-ir-publication-coverage.summary.json` plus `docs/superpowers/reports/2026-07-06-ir-publication-coverage.md`.
- The regenerated summary now reports `unsupported_gaps.counts_by_owner`, confirming owners are no longer forced to a single constant.

## 2026-07-06 IR Publication Coverage final re-review follow-up fixes

- Added top-level `diagnostic_coverage` to the report summary and Markdown output, with first-class classifications for `warnings[]`, `warnings[].code`, `warnings[].severity`, `warnings[].message`, `warnings[].span`, `errors[]`, `errors[].code`, `errors[].severity`, `errors[].message`, and `errors[].span`, all owned as `custom_sidecar` publication facts.
- Routed mapping rules that point at diagnostic parser-IR fields into `diagnostic_coverage` so warning/error pointers no longer fall through as generic unsupported source constructs.
- Normalized custom-schema ownership tokens to `custom_schema` anywhere this report emits custom-schema unsupported owners, including unknown node kinds and unsupported field/diagnostic placeholders.
- Extended the smoke harness with assertions for `diagnostic_coverage.by_field."warnings[].code"` and an unknown parser-IR node fixture that requires `.node_coverage.unsupported[0].owner == "custom_schema"`.
- Regenerated `docs/superpowers/reports/2026-07-06-ir-publication-coverage.summary.json` and `docs/superpowers/reports/2026-07-06-ir-publication-coverage.md` via `just parser-ir-publication-coverage-report`.

## 2026-07-06 IR Publication Coverage final re-review closing fixes

- Added `field_key_for_pointer()` alongside `diagnostic_field_key()` and now skip known `FIELD_COVERAGE_SPECS` pointers during `source_construct_coverage`, so field facts like `heading.level`, `gaiji.raw_marker`, and `ruby.direction` stay in `field_coverage` instead of leaking into unsupported construct gaps.
- Removed the non-spec unsupported owner fallback token `evidence`; unknown rule categories now fall back to the allowed `policy` owner, while the explicit category-to-owner mappings remain `parser_ir_schema`, `custom_schema`, `aat_to_parser_ir_converter`, and `policy`.
- Extended unsupported gap items with prevalence metadata: rule-based gaps parse `Observed N occurrences` from rule descriptions into `observed_occurrences` with `prevalence_source: "rule_description"`, and schema/node-based gaps now emit `observed_occurrences: null` plus `prevalence_source: "unavailable"`.
- Tightened the smoke harness to assert that `heading.level` and `gaiji.raw_marker` do not appear in unsupported gaps, that no unsupported owner is `evidence`, and that both rule-based and node-based unsupported gaps expose the expected prevalence shape.
