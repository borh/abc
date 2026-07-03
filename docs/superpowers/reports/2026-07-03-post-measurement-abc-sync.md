# Post-Measurement ABC Sync

Date: 2026-07-03

## Verdict

`MEASUREMENT_BLOCKED`

The generated AAT-to-parser-IR mapping evidence is ready, but the current aozora2html policy measurement is still not decision-complete after targeted retry. Do not start the owned `crates/ab-aat-to-parser-ir` CLI plan yet.

The allowed next work is to resolve the aozora2html residual policy buckets below, or get an explicit human reviewer decision that accepts a lower-bound caveat. The executor must not choose `CLI_READY_WITH_LOWER_BOUND_CAVEAT` automatically.

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

Targeted retry run: `/db/ab-validator/aat-corpus/aozora2html-policy-retry-20260703T055526Z`

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
| warigaki | 265 | 7 | 0 | 5 | 17 | 65 |
| kunten | 457 | 12 | 0 | 10 | 153 | 3 |

Targeted retry added 24 warigaki-observed works and 23 kunten-observed works. It did not eliminate the blocking residuals.

## Blocking Buckets

Generated worksets:

- warigaki incomplete: `/db/ab-validator/aat-corpus/aozora2html-full-20260703T020301Z/worksets/warigaki-incomplete.json` (12 works)
- kunten incomplete: `/db/ab-validator/aat-corpus/aozora2html-full-20260703T020301Z/worksets/kunten-incomplete.json` (22 works)
- policy incomplete union: `/db/ab-validator/aat-corpus/aozora2html-full-20260703T020301Z/worksets/policy-incomplete-union.json` (32 works)

Blocking residual buckets:

- warigaki `adapter_timeout_or_protocol_error`: 7 works
- warigaki `parse_incomplete`: 5 works
- warigaki `report_failed_other_property`: 17 works
- warigaki `source_feature_without_aat_observation`: 65 works
- kunten `adapter_timeout_or_protocol_error`: 12 works
- kunten `parse_incomplete`: 10 works
- kunten `report_failed_other_property`: 153 works
- kunten `source_feature_without_aat_observation`: 3 works

Residual triage report: `docs/superpowers/reports/2026-07-03-aozora2html-policy-residual-triage.md`

The `source_feature_without_aat_observation` buckets are the clean policy-specific adapter-emission blocker. The residual triage found 65 warigaki works and 3 kunten works with reports present, AAT present, and clean check reports, but no matching family observation in AAT. That pattern is consistent with source markers being detected in the index but not emitted as warigaki/kunten observations by the adapter.

The `report_failed_other_property` buckets are now characterized separately. All 17 warigaki works and all 153 kunten works have reports and AAT present, but they are not clean enough to classify as source-feature-only emission gaps because their reports fail other properties. Their failures are dominated by `visible_text_body_order` with small `gaiji_resolution` and `ruby_completeness` tails, so they remain adapter-oracle characterization work. They still require a reviewer decision before any lower-bound caveat can unblock CLI work.

The retry-backed incomplete family buckets remain runtime or parse-completeness work: 7 warigaki and 12 kunten timeout/protocol bucket entries have no AAT, while 5 warigaki and 10 kunten parse-incomplete entries have AAT but fail `parse_completeness`, `gaiji_resolution`, and `ruby_completeness`.

## Failure Overlap

| Family | adapter_timeout | adapter_protocol_error | parse_completeness | visible_text_body_order | gaiji_resolution | ruby_completeness |
|---|---:|---:|---:|---:|---:|---:|
| warigaki | 33 | 1 | 7 | 114 | 31 | 14 |
| kunten | 37 | 0 | 11 | 268 | 34 | 16 |

## Sample Evidence

Sample report: `docs/superpowers/reports/2026-07-03-aozora2html-policy-samples.md`

| Family | observed_in_aat | adapter_timeout_or_protocol_error | schema_invalid_or_no_aat | parse_incomplete | report_failed_other_property | source_feature_without_aat_observation |
|---|---:|---:|---:|---:|---:|---:|
| warigaki | 10 | 7 | 0 | 5 | 10 | 10 |
| kunten | 10 | 10 | 0 | 10 | 10 | 3 |

## Next Gate

Before a follow-up `crates/ab-aat-to-parser-ir` implementation plan starts, one of these must happen:

1. The source-feature emission gap is fixed or explicitly accepted for the 65 warigaki and 3 kunten `source_feature_without_aat_observation` works, then the audit, sample, and residual triage reports are regenerated.
2. A human reviewer records `CLI_READY_WITH_LOWER_BOUND_CAVEAT` with a `Reviewer decision:` line naming the accepted known-gap list.

Until then, manifest identity and compatibility-registry hardening remain paused.
