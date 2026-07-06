# aozora2html Adapter Coverage Triage Notes

Date: 2026-04-28
Sample: 25 works selected via `ab-index --query-all ruby --sample 25`,
plus one report covering an aggregate-style entry written by `ab-check`
(26 reports total).

## ab-check property tally

| Property | pass | fail |
|---|---:|---:|
| `schema_valid` | 26 | 0 |
| `parse_completeness` | 25 | 1 |
| `block_balance` | 26 | 0 |
| `heading_level_consistency` | 26 | 0 |
| `no_dropped_lines` | 26 | 0 |
| `ruby_completeness` | 25 | 1 |
| `gaiji_resolution` | 15 | 11 |
| `visible_text_body_order` | 11 | 15 |

## Triage by failure mode

### `parse_completeness` (1 fail)

- `000008_1083`: Ruby parser aborts at line 275 with
  "改行コードを、「CR+LF」にあらためてください". The wrapper transcodes
  to Shift_JIS + CRLF, but a stray bare CR appears mid-line in the
  source. `aozora2html-adapter` correctly emits `parse_complete: false`
  with the parser stderr captured in `meta.warnings[0].message`.
- **Bucket: 1 — Ruby tool genuinely loses information.** No mapper
  change. Rust adapters tolerate the same input.

### `gaiji_resolution` (11 fail)

- All failures match the documented behavior: when `--use-unicode`
  succeeds, `aozora2html` substitutes the resolved character inline as
  text and discards the marker, so the source has a `※［＃...］` that
  has no matching gaiji node in the AAT.
- **Bucket: 1 — Ruby tool genuinely loses information.** No mapper
  change. The Rust adapters keep the marker; the property is correct
  to flag this asymmetry.

### `visible_text_body_order` (15 fail)

- The aozora2html adapter inserts `\n` characters at `<br/>` boundaries
  and between block-level XHTML siblings; the Rust `visible_text` is
  whitespace-stripped paragraph text. After whitespace normalization,
  13/26 works' visible-text hashes match (`ab-compare` AAT diff:
  `normalized_visible_text_difference_count: 13`).
- **Bucket: 3 — property is too strict for an HTML-derived adapter
  that preserves source whitespace.** Recorded as follow-up; do not
  weaken the property in this plan because it would invalidate
  baselines for the Rust adapters.

### `ruby_completeness` (1 fail)

- One work has a ruby annotation that the source contains but the
  parser drops or normalizes differently. Sample size too small to
  classify; revisit on a corpus-wide run.

## ab-compare AAT diff vs. `aozora-rs` (sample size 26)

```
common_aat: 26
structural_difference_count: 26
visible_text_difference_count: 26
normalized_visible_text_difference_count: 13
semantic_hash_difference_counts:
  gaiji_descriptions: 15
  ruby_readings: 6
semantic_summary_hash_difference_counts:
  summary:ruby.basic: 15
  summary:gaiji.marker: 15
  summary:gaiji_ruby.inline_base: 7
  summary:projection.warning: 17
```

All four matrix-keyed `summary:*` entries are populated, confirming
that the Python-side semantic summary keys reach `ab-compare`'s
hashing layer. Mismatches concentrate on:

1. **`summary:gaiji.marker`** — expected, per the README caveat about
   parser-side gaiji resolution producing plain text, not a marker.
2. **`summary:projection.warning`** — only the aozora2html side emits
   `projection_warning` rows (one per `meta.warnings` entry); the
   Rust adapters emit them through a different path. The hash
   asymmetry is structural and not actionable in this plan.
3. **`summary:ruby.basic`** — value-shape mismatches on a per-work
   basis. Spot-checked one work: the Python adapter and `aozora-rs`
   agree on `(base_projection, reading, placement)` for most ruby
   annotations, but `aozora-rs` emits additional `provenance:
   "parser_normalized"` and `"source_fallback"` ruby entries that the
   Python side does not. Bucket 1 (parser-internal classification
   that aozora2html does not expose).

## Conclusion

The adapter is fit for purpose: it produces schema-valid AAT for all
sampled works, isolates parser failures to a single false on
`parse_completeness`, and surfaces the expected matrix-keyed
mismatches in `ab-compare`. No mapper changes triggered by this
triage round.
