# Aozora2html Measurement Trust Audit

- run_dir: `/db/ab-validator/aat-corpus/aozora2html-full-20260703T020301Z`
- report_id: `aozora2html-full-2026-07-03`

## Source Counts

- source warigaki works: 359
- source kunten works: 635
- source policy union works: 878

## AAT Observed Counts

| Metric | Value |
|---|---:|
| warigaki works | 241 |
| warigaki nodes | 4050 |
| kunten works | 464 |
| kunten nodes | 22504 |
| kunten semantic observations | 23174 |
| kunten observations | 22504 |

## warigaki Trust Buckets

| Bucket | Works |
|---|---:|
| observed_in_aat | 241 |
| adapter_timeout_or_protocol_error | 34 |
| schema_invalid_or_no_aat | 0 |
| parse_incomplete | 5 |
| report_failed_other_property | 16 |
| source_feature_without_aat_observation | 63 |

## warigaki Failure Overlap

| Property | Works |
|---|---:|
| adapter_protocol_error | 1 |
| adapter_timeout | 33 |
| gaiji_resolution | 31 |
| parse_completeness | 7 |
| ruby_completeness | 14 |
| visible_text_body_order | 114 |

## kunten Trust Buckets

| Bucket | Works |
|---|---:|
| observed_in_aat | 434 |
| adapter_timeout_or_protocol_error | 37 |
| schema_invalid_or_no_aat | 0 |
| parse_incomplete | 10 |
| report_failed_other_property | 151 |
| source_feature_without_aat_observation | 3 |

## kunten Failure Overlap

| Property | Works |
|---|---:|
| adapter_timeout | 37 |
| gaiji_resolution | 34 |
| parse_completeness | 11 |
| ruby_completeness | 16 |
| visible_text_body_order | 268 |

## Generated Worksets

- warigaki incomplete: `/db/ab-validator/aat-corpus/aozora2html-full-20260703T020301Z/worksets/warigaki-incomplete.json`
- kunten incomplete: `/db/ab-validator/aat-corpus/aozora2html-full-20260703T020301Z/worksets/kunten-incomplete.json`
- policy incomplete union: `/db/ab-validator/aat-corpus/aozora2html-full-20260703T020301Z/worksets/policy-incomplete-union.json`
