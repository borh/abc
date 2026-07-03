# Post-Measurement ABC Sync

Date: 2026-07-03

## Verdict

`MEASUREMENT_BLOCKED`

The generated AAT-to-parser-IR mapping evidence is ready, but the current aozora2html policy measurement is still not decision-complete after the source-feature-gap retry. Do not start the owned `crates/ab-aat-to-parser-ir` CLI plan yet.

The gate stays blocked because clean adapter-obligation candidates still remain in `source_feature_without_aat_observation` after retry. The source-index-only residuals can be recorded as a lower-bound caveat, but that does not override the remaining clean adapter-obligation gap.

## Mapping Evidence

- mapping artifact: `data/aat-to-parser-ir-mapping-v1.json`
- mapping schema hash: `sha256:38ec7f0e5affb10329b550a091cd3a6fb5a25e26fd469dfe9f8249970cf9adb4`
- target parser-IR schema hash: `sha256:41c43f0c88a66c31ae4fbf9b9eeb04de92756082acaaaa1c2e21f1a5bf74a396`
- aozora-rs measured corpus: 17,894 files scanned, 0 files with `UNSUPPORTED`
- generated mapping rules: 25
- policy changes in the generated mapper: `ruby.direction` projects directly, `style` maps to parser-IR `emphasis`, and `windows-31j-lossy` maps to `source.encoding = Shift_JIS` with an `AMBIGUITY` ledger entry

Do not hand-copy the historical 27-rule table. The mapping artifact is generated from executable policy and measured folded corpus buckets.

## Aozora2html Measurement

Baseline run: `/db/ab-validator/aat-corpus/aozora2html-full-20260703T020301Z`

Targeted retry run: `/db/ab-validator/aat-corpus/aozora2html-source-feature-gap-20260703T082427Z`

Targeted retry workset: `/db/ab-validator/aat-corpus/aozora2html-full-20260703T020301Z/source-feature-gap-worksets/source-feature-gap-adapter-obligation-union.json` (38 works)

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

Merged trust buckets after retry:

| Family | observed_in_aat | adapter_timeout_or_protocol_error | schema_invalid_or_no_aat | parse_incomplete | report_failed_other_property | source_feature_without_aat_observation |
|---|---:|---:|---:|---:|---:|---:|
| warigaki | 275 | 32 | 0 | 5 | 16 | 31 |
| kunten | 435 | 36 | 0 | 10 | 151 | 3 |

Clean source-feature bucket before/after:

| Family | Before | After | Delta |
|---|---:|---:|---:|
| warigaki | 65 | 31 | -34 |
| kunten | 3 | 3 | 0 |
| union | 68 | 34 | -34 |

Classifier-backed clean-gap status after retry:

- marker classification counts: `warigaki.koko_start_end=110`, `warigaki.compact_start_end=63`, `warigaki.with_source_line_break=11`, `kunten.kaeriten.compact=7`, `kunten.okurigana.parenthesized=5`
- adapter-obligation candidates: 38 before retry, 4 still clean after retry
- source-index-only candidates: 30, all still present as clean residuals
- unknown clean gaps: 0 before retry, 0 after retry

The retry cleared 34 of the 38 targeted clean adapter-obligation candidates. Four clean adapter-obligation works still remain in `source_feature_without_aat_observation`: `000296_47149`, `001231_46296`, `001231_46297`, and `001313_47601`.

## Blocking Buckets

Generated worksets:

- warigaki incomplete: `/db/ab-validator/aat-corpus/aozora2html-full-20260703T020301Z/worksets/warigaki-incomplete.json` (12 works)
- kunten incomplete: `/db/ab-validator/aat-corpus/aozora2html-full-20260703T020301Z/worksets/kunten-incomplete.json` (22 works)
- policy incomplete union: `/db/ab-validator/aat-corpus/aozora2html-full-20260703T020301Z/worksets/policy-incomplete-union.json` (32 works)

Blocking residual buckets:

- warigaki `adapter_timeout_or_protocol_error`: 32 works
- warigaki `parse_incomplete`: 5 works
- warigaki `report_failed_other_property`: 16 works
- warigaki `source_feature_without_aat_observation`: 31 works
- kunten `adapter_timeout_or_protocol_error`: 36 works
- kunten `parse_incomplete`: 10 works
- kunten `report_failed_other_property`: 151 works
- kunten `source_feature_without_aat_observation`: 3 works

Residual triage report: `docs/superpowers/reports/2026-07-03-aozora2html-policy-residual-triage.md`

The `source_feature_without_aat_observation` buckets remain the clean policy-specific adapter-emission blocker. After retry they contain 31 warigaki works and 3 kunten works with reports present, AAT present, and clean check reports, but no matching family observation in AAT.

Source-index-only scope decision:

- accepted scope: the 30 `source_index_only_candidates` are outside the body adapter obligation for this gate and may be carried as an explicit lower-bound caveat list
- non-accepted scope: the 4 remaining adapter-obligation candidates are still in scope for measurement completion, so the verdict cannot move to `CLI_READY_WITH_LOWER_BOUND_CAVEAT`

The `report_failed_other_property` buckets remain characterized separately. They changed only slightly from 17 to 16 warigaki works and from 153 to 151 kunten works. Their failures are still dominated by `visible_text_body_order` with small `gaiji_resolution` and `ruby_completeness` tails, so they remain adapter-oracle characterization work.

The parse-incomplete buckets are unchanged at 5 warigaki and 10 kunten works. The timeout/protocol buckets did not stay flat; they rose from 7 to 32 warigaki works and from 12 to 36 kunten works in the merged audit, so they remain separate runtime stability work rather than evidence for a lower-bound acceptance.

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

Before a follow-up `crates/ab-aat-to-parser-ir` implementation plan starts, the 4 remaining clean adapter-obligation works in `source_feature_without_aat_observation` must be resolved and the audit, sample, and residual triage reports must be regenerated.

A human reviewer still needs to record the lower-bound decision that explicitly names the 30 accepted `source_index_only_candidates` and separately assigns follow-up work for the 4 still-blocking adapter-obligation works and the timeout/runtime buckets, but that decision is only the accepted caveat for source-index-only scope and does not open the CLI plan while the 4 adapter-obligation gaps remain.

Until then, manifest identity and compatibility-registry hardening remain paused.
