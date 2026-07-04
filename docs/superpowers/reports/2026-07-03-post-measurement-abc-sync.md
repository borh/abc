# Post-Measurement ABC Sync

Date: 2026-07-03

## 2026-07-04 Update

This gate has been consumed by the implemented `crates/ab-aat-to-parser-ir`
converter. The current release-sync artifact is
`docs/superpowers/reports/2026-07-04-post-parser-ir-conversion-sync.md`, and
the full-corpus conversion audit is
`docs/superpowers/reports/2026-07-04-aat-parser-ir-full-corpus-conversion.md`.

## Verdict

`CLI_READY_WITH_LOWER_BOUND_CAVEAT`

The generated AAT-to-parser-IR mapping evidence is ready, and the refreshed aozora2html source-feature gap measurement no longer has clean adapter-obligation or unknown clean gaps. The follow-up `crates/ab-aat-to-parser-ir` CLI plan may start, scoped to the generated mapping artifact and the v1 drop-sidecar behavior for unsupported critical constructs.

Reviewer decision: accept the 32 `source_index_only_candidates` listed below as outside the body adapter obligation for this gate. These works remain a lower-bound caveat for full-source feature accounting, not a blocker for the v1 AAT-to-parser-IR CLI.

## Mapping Evidence

- mapping artifact: `data/aat-to-parser-ir-mapping-v1.json`
- mapping version: `0.1.1`
- mapping schema hash: `sha256:38ec7f0e5affb10329b550a091cd3a6fb5a25e26fd469dfe9f8249970cf9adb4`
- target parser-IR schema hash: `sha256:41c43f0c88a66c31ae4fbf9b9eeb04de92756082acaaaa1c2e21f1a5bf74a396`
- aozora-rs measured corpus: 17,894 files scanned, 0 files with `UNSUPPORTED`
- combined generated mapping corpus: 35,583 files scanned across aozora-rs plus current aozora2html evidence
- generated mapping rules: 118
- generated mapper policy: `ruby.direction` projects directly, `style` maps to parser-IR `emphasis`, `windows-31j-lossy` maps to `source.encoding = Shift_JIS` with an `AMBIGUITY` ledger entry, measured aozora2html warigaki emits `UNSUPPORTED`, and missing-span fallback emits `AMBIGUITY`

Do not hand-copy the historical 27-rule table. The mapping artifact is generated from executable policy and measured folded corpus buckets.

## Aozora2html Measurement

Baseline run: `/db/ab-validator/aat-corpus/aozora2html-full-20260703T020301Z`

Final targeted retry run: `/db/ab-validator/aat-corpus/aozora2html-final-four-gap-20260703T091523Z`

Final targeted retry workset: `/db/ab-validator/aat-corpus/aozora2html-full-20260703T020301Z/source-feature-gap-worksets/source-feature-gap-final-four-retry-38.json` (38 works)

Current clean adapter-obligation workset: `/db/ab-validator/aat-corpus/aozora2html-full-20260703T020301Z/source-feature-gap-worksets/source-feature-gap-adapter-obligation-union.json` (0 works)

Full-run metrics before trust classification:

| Metric | Value |
|---|---:|
| indexed works | 17,886 |
| check reports | 17,886 |
| persisted AAT files | 17,689 |
| schema invalid or missing reports | 197 |
| parse incomplete or missing reports | 302 |
| warigaki nodes | 4,050 |
| kunten AAT observations | 22,504 |

Source feature counts from the trust audit:

| Source set | Works |
|---|---:|
| warigaki | 359 |
| kaeriten | 480 |
| okurigana | 254 |
| kunten union | 635 |
| policy union | 878 |

Merged trust buckets after the final retry:

| Family | observed_in_aat | adapter_timeout_or_protocol_error | schema_invalid_or_no_aat | parse_incomplete | report_failed_other_property | source_feature_without_aat_observation |
|---|---:|---:|---:|---:|---:|---:|
| warigaki | 277 | 32 | 0 | 5 | 16 | 29 |
| kunten | 435 | 36 | 0 | 10 | 151 | 3 |

Clean source-feature bucket before/final-after:

| Family | Before | Final after | Delta |
|---|---:|---:|---:|
| warigaki | 65 | 29 | -36 |
| kunten | 3 | 3 | 0 |
| union | 68 | 32 | -36 |

The final retry added two warigaki observations that were previously missing in clean adapter-obligation works: `000296_47149` and `001313_47601`. The remaining two reviewed works, `001231_46296` and `001231_46297`, are now classified as title-next-line source notes, not body adapter obligations.

## Clean-Gap Classification

Source-feature gap classification report: `docs/superpowers/reports/2026-07-03-aozora2html-source-feature-gap-classification.md`

Current classifier-backed clean-gap status:

- adapter-obligation candidates: 0 warigaki, 0 kunten, 0 union
- source-index-only candidates: 32
- unknown clean gaps: 0
- marker classification counts: `warigaki.compact_start_end=63`, `warigaki.koko_start_end=3`, `warigaki.with_source_line_break=3`, `kunten.kaeriten.compact=7`, `kunten.okurigana.parenthesized=5`
- context hint counts: `base_text_note=31`, `notation_example=21`, `publication_or_editor_note=22`

Accepted source-index-only workset: `/db/ab-validator/aat-corpus/aozora2html-full-20260703T020301Z/source-feature-gap-worksets/source-feature-gap-source-index-only-candidates.json`

Accepted source-index-only work IDs:

`000106_57905`, `000121_45086`, `000416_47410`, `000712_52955`, `000712_52957`, `000754_48376`, `000933_47549`, `001095_43203`, `001095_43204`, `001095_43205`, `001095_43206`, `001095_43207`, `001095_43208`, `001095_43209`, `001095_43210`, `001095_43211`, `001095_43212`, `001095_43213`, `001095_43214`, `001095_43215`, `001095_43216`, `001095_43217`, `001095_43218`, `001095_43219`, `001095_43220`, `001095_43221`, `001095_43222`, `001095_43223`, `001231_46296`, `001231_46297`, `001383_56944`, `001383_58152`

## Residual Buckets

Generated worksets:

- warigaki incomplete: `/db/ab-validator/aat-corpus/aozora2html-full-20260703T020301Z/worksets/warigaki-incomplete.json` (12 works)
- kunten incomplete: `/db/ab-validator/aat-corpus/aozora2html-full-20260703T020301Z/worksets/kunten-incomplete.json` (22 works)
- policy incomplete union: `/db/ab-validator/aat-corpus/aozora2html-full-20260703T020301Z/worksets/policy-incomplete-union.json` (32 works)

Residual triage report: `docs/superpowers/reports/2026-07-03-aozora2html-policy-residual-triage.md`

Residual bucket counts:

- warigaki `adapter_timeout_or_protocol_error`: 32 works
- warigaki `parse_incomplete`: 5 works
- warigaki `report_failed_other_property`: 16 works
- warigaki `source_feature_without_aat_observation`: 29 works, all accepted source-index-only
- kunten `adapter_timeout_or_protocol_error`: 36 works
- kunten `parse_incomplete`: 10 works
- kunten `report_failed_other_property`: 151 works
- kunten `source_feature_without_aat_observation`: 3 works, all accepted source-index-only

The timeout/protocol, parse-incomplete, and report-failed buckets remain follow-up adapter/runtime/oracle characterization work. They do not change the clean body adapter-obligation conclusion above; policy counts for aozora2html remain lower bounds until those buckets are separately resolved.

## Failure Overlap

| Family | adapter_timeout | adapter_protocol_error | parse_completeness | visible_text_body_order | gaiji_resolution | ruby_completeness |
|---|---:|---:|---:|---:|---:|---:|
| warigaki | 33 | 1 | 7 | 114 | 31 | 14 |
| kunten | 37 | 0 | 11 | 268 | 34 | 16 |

## Sample Evidence

Sample report: `docs/superpowers/reports/2026-07-03-aozora2html-policy-samples.md`

| Family | observed_in_aat | adapter_timeout_or_protocol_error | schema_invalid_or_no_aat | parse_incomplete | report_failed_other_property | source_feature_without_aat_observation |
|---|---:|---:|---:|---:|---:|---:|
| warigaki | 10 | 10 | 0 | 5 | 10 | 10 |
| kunten | 10 | 10 | 0 | 10 | 10 | 3 |

## Next Gate

The next allowed implementation plan is `crates/ab-aat-to-parser-ir`, using the generated `data/aat-to-parser-ir-mapping-v1.json` artifact and validating against ABC's mapping schema. Keep manifest identity and compatibility-registry hardening behind that generated mapping artifact and its release-smoke checks.

Separate follow-up work should characterize timeout/protocol, parse-incomplete, and report-failed buckets. Do not bundle those runtime/oracle investigations into the parser-IR CLI implementation.
