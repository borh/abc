# v1 Lock Report — 2026-07-06

Executes the calibration campaign's final step: the owner's blind labels
returned, spec step 8's gate passed, and spec Open Question 4 (rank
scope) resolved in favor of `global`. This report records what changed,
why, and how it was verified.

Branch: `v1-lock`. Worktree:
`/home/bor/Projects/ab-validator/.worktrees/v1-lock`.

## 1. Code changes

`crates/ab-morph-run/src/summary/interesting.rs`:

- `SCORE_VERSION: u32 = 1` -> `2`, with an expanded doc comment recording
  the v2 semantics: default `rank_scope` flipped to global per the
  2026-07-06 calibration (spec Open Question 4 resolution); `v1` numbers
  remain reproducible via `--rank-scope within-kind`, but the *artifact's*
  `score_version` field is still whatever the binary stamps (`2`) —
  cross-version comparison stays forbidden regardless of which
  `--rank-scope` value produced a given run.
- `WarehouseInterestingOptions::default().rank_scope`: `RankScope::WithinKind`
  -> `RankScope::Global`.
- `RankScope` doc comment updated to describe `Global` as the v2 default
  and `WithinKind` as the v1 default retained for reproduction.
- Left untouched, as instructed: the serde `default_rank_scope()`
  deserialization fallback (still returns `"within-kind"` — correct,
  since pre-calibration artifacts predate the field and were always
  within-kind), `READER_MAX_SCHEMA_VERSION`, the `RankScope` enum itself,
  and the empty-run early return (it already derives its
  `score_version` block from `options`, so it picked up the new default
  with no code change).

`crates/ab-morph-run/src/main.rs`:

- `SummarizeWarehouseInteresting`'s `--rank-scope` CLI arg:
  `default_value_t = ab_morph_run::RankScope::WithinKind` -> `Global`.
- `parses_summarize_warehouse_interesting_command` test: updated its
  default-path assertion from `RankScope::WithinKind` to `RankScope::Global`.
  `parses_summarize_interesting_scoring_knobs` (which passes
  `--rank-scope global` explicitly) and `parses_summarize_interesting_score_mode_and_sample_seed`
  needed no changes, as expected — they don't exercise the default.

## 2. Tests fixed, with hand-derived arithmetic

Four tests broke from the default flip; all four failures were the
*expected* class per the task brief (default-path goldens/assertions),
not semantic breakage. No test that passes `rank_scope` explicitly
(the within-kind hand-computed pin, the global-pooling test) needed any
change — confirming the ranking semantics themselves are untouched, only
the default.

### `lexical_only_filter_excludes_punctuation_only_patterns`

Fixture `write_fixture` (after `--filter lexical-only` excludes the
punctuation-only segmentation pattern) leaves exactly one pattern per
kind: a segmentation pattern (今日 split, occurring in src-a region 0 and
src-b region 0 — 2 occurrences, `rarity_total = 2` distinct source ids
since there's no `aozora_works.parquet` sidecar), a coverage pattern
(src-a region 1, chars 2..5, 1 occurrence, `has_coverage_mismatch = true`
on its own region), and a feature pattern (src-a region 2, chars 5..6,
`pos1` 名詞/動詞, 1 occurrence).

Under the old within-kind default every signal trivially ranked 1st
within a kind-pool of size 1, so all three patterns tied at
`rrf_score = 1/61 = 0.016393`. Under the new global default,
`Coverage`/`Rarity`/`Span` pool ranks across all three patterns instead of
trivially ranking 1-of-1 (`Impact`'s pool stays feature-only, size 1, so
it is unaffected). Raw signal values, hand-computed from the fixture:

| Pattern | Coverage raw | Rarity raw | Span raw | Impact raw |
|---|---|---|---|---|
| coverage (pid `bda0d6…`) | `log2(1+1) = 1.0` | `log2((2+1)/(1+1)) = log2(1.5) = 0.584963` | `3` (chars 2..5) | n/a |
| feature (pid `fc1912…`) | `log2(1+0) = 0.0` | `log2(1.5) = 0.584963` | `1` (chars 5..6) | `impact_weight("pos1") = 4.0` |
| segmentation (pid `aa9e4b…`) | `log2(1+0) = 0.0` | `log2((2+1)/(2+1)) = log2(1) = 0.0` | `2` (chars 0..2) | n/a |

Ranking each signal's pool descending, ties broken by `pattern_id`
ascending:

- **Coverage** pool {coverage=1.0, feature=0.0, segmentation=0.0}: coverage
  rank 1. Feature vs segmentation tie at 0.0; `"aa9e4b…" < "fc1912…"`, so
  segmentation rank 2, feature rank 3.
- **Rarity** pool {coverage=0.584963, feature=0.584963, segmentation=0.0}:
  coverage vs feature tie at 0.584963; `"bda0d6…" < "fc1912…"`, so coverage
  rank 1, feature rank 2. Segmentation rank 3 (worst, unique tag).
- **Span** pool {coverage=3, segmentation=2, feature=1}: no ties — coverage
  rank 1, segmentation rank 2, feature rank 3.
- **Impact** pool {feature=4.0} (only applicable pattern): rank 1.

RRF term per rank: `1/(60+rank)` — rank1 = `1/61 = 0.0163934426…`, rank2 =
`1/62 = 0.0161290323…`, rank3 = `1/63 = 0.0158730159…`.

- **coverage**: `(1/61 + 1/61 + 1/61) / 3 = 0.0163934426… -> round6 = 0.016393`
- **feature**: `(1/63[cov] + 1/62[rar] + 1/61[imp] + 1/63[span]) / 4`
  `= (0.0158730159 + 0.0161290323 + 0.0163934426 + 0.0158730159) / 4`
  `= 0.0642685067 / 4 = 0.0160671267 -> round6 = 0.016067`
- **segmentation**: `(1/62[cov] + 1/63[rar] + 1/62[span]) / 3`
  `= (0.0161290323 + 0.0158730159 + 0.0161290323) / 3`
  `= 0.0481310805 / 3 = 0.0160436935 -> round6 = 0.016044`

Ordering (`rrf_score` desc): coverage (0.016393) > feature (0.016067) >
segmentation (0.016044). Test updated to assert this order and these
three values by name instead of asserting a single shared score with a
`source_count`-based tie-break (there is no longer a tie to break). These
numbers were independently derived by hand *before* being checked against
the actual computed output (obtained via a temporary `eprintln!` debug
pass, then removed) — they matched exactly.

### `anomaly_channel_surfaces_regions_below_cutoff`

Same fixture, `limit: 1`, lexical-only filter. Top-1 is now the coverage
pattern (`rrf_score = 0.016393`, highest of the three per the table
above), not the segmentation pattern as under the old default. The
anomaly channel excludes regions owned by a top-`k` pattern, so the
coverage pattern's own region (chars 2..5, `has_coverage_mismatch = true`,
previously scoring `5.0*1 + log2(4) = 7.0` and surfacing as an anomaly
under the old within-kind default) is now suppressed instead, and the
segmentation pattern's two region-0 occurrences (previously suppressed as
the old top-1) now surface:

- src-a/txt-a region 0 (chars 0..2, length 2, no coverage mismatch):
  `anomaly_score = 0 + log2(1+2) = log2(3) = 1.5849625007… -> round6 = 1.584963`
- src-b/txt-b region 0 (chars 0..2, length 2, no coverage mismatch):
  same computation, `1.584963`
- src-a/txt-a region 2 (feature pattern's region, chars 5..6, length 1,
  no coverage mismatch): `log2(1+1) = 1.0`

Tie between the two region-0 rows breaks on `source_id` ascending
(`"src-a" < "src-b"`), giving `[(0, 1.584963), (0, 1.584963), (2, 1.0)]`
— exactly the sequence the code produces. Test updated with this
arithmetic in a comment and the new expected vector.

### `tsv_writer_emits_rows_and_anomaly_section`

Same fixture/options as the anomaly test above. TSV rank-1 line changes
from `1\tsegmentation\t0.016393` to `1\tcoverage\t0.016393` (coverage is
now top-1, at the *same* score value — 0.016393 was already the top
score under the old default too, just attributed to the wrong pattern
after the flip). The literal `"7.000000"` assertion (the coverage
region's own anomaly score, now suppressed since coverage is top-1) is
replaced with checks for `"1.584963"` and `"1.000000"`, matching the
anomaly test's derivation above.

### `json_output_is_byte_stable_across_invocations`

Trivial: literal string assertion `"\"score_version\": 1"` ->
`"\"score_version\": 2"`, tracking the `SCORE_VERSION` bump. No ranking
arithmetic involved.

## 3. Full test run

```
cargo test -p ab-morph-run --features test-analyzer
```

Result: **234 passed, 0 failed** (199 in `lib`, 35 in `main.rs`'s test
module). `cargo clippy -p ab-morph-run --features test-analyzer
--all-targets`: clean, no warnings.

## 4. Data provenance and scorer reproduction

1. Copied the owner's filled labels:
   `/home/bor/labels.tsv` ->
   `reports/morph-warehouse/calibration/2026-07-06/labels/labels-filled.tsv`
   (164 lines, matches `labels.tsv`'s row count; only whitespace
   differences in the comment header from the owner's editor).
2. Reproduced scores:
   ```
   cargo run --release -p ab-morph-run -- score-interesting-labels \
     --labels reports/morph-warehouse/calibration/2026-07-06/labels/labels-filled.tsv \
     --mapping reports/morph-warehouse/calibration/2026-07-06/labels/mapping.json \
     > reports/morph-warehouse/calibration/2026-07-06/labels/scores.json
   ```
3. **Verified exact match against the owner-reported numbers** — no
   discrepancy, no BLOCKED condition:

   | Method | p@50 (reported / reproduced) | nDCG@50 (reported / reproduced) |
   |---|---|---|
   | frequency | 0.16 / 0.16 | 0.2898142842602241 / 0.2898142842602241 |
   | random | 0.56 / 0.56 | 0.6521337943910488 / 0.6521337943910488 |
   | rrf-global | 0.7 / 0.7 | 0.7388054023634453 / 0.7388054023634453 |
   | rrf-within | 0.64 / 0.64 | 0.7191302568437826 / 0.7191302568437826 |

   Verdict counts (155 labeled rows): bug 8, corpus-artifact 6,
   expected-dictionary 65, expected-policy 40, noise 32, unclear 4 — all
   exact matches.

## 5. Docs updated

- `reports/morph-warehouse/calibration/2026-07-06/report.md`: intro
  reframed from "conditional lock" to "final lock"; §4 (rank scope)
  filled in with the measured p@50/nDCG@50 table, the resolution, and a
  margin-fragility caveat (45/50 shared patterns between the two RRF
  variants' top-50s means the entire 0.70-vs-0.64 gap is decided by 3
  rows out of the 5-vs-5 patterns unique to each side, some flagged noisy
  by the owner); §5's table flipped from "pending labels" to fully
  locked, including `score_version = 2`; new §8 "Outcome (measured,
  2026-07-06)" with the step-8 gate verdict (PASSED, >4x margin over
  frequency under either rank scope), the honest random-baseline caveat
  (p@50=0.56 is a property of the rarity-heavy, dictionary/policy-heavy
  labeled pool, not a weak baseline; RRF's edge over random is +0.08 to
  +0.14 p@50, its decisive win is over frequency), the verdict histogram,
  and the final locked-defaults table.
- `docs/superpowers/specs/2026-07-05-interestingness-ranking-design.md`:
  - §Scoring step 2 (the within-kind ranking sentence, Open Question 4):
    appended a "Resolved 2026-07-06" paragraph with the measured numbers
    and the rule that fired; original rationale left intact.
  - §Calibration Plan step 3: appended the 2026-07-06 result inline
    (resolved, global wins, numbers, margin-fragility note).
  - §Calibration Plan status line: rewritten from "steps 1-5/9 done,
    labels/gate pending" to "steps 1-9 complete, labels received, gate
    PASSED, v1 LOCKED", with the final locked-defaults summary
    (`score_version` now 2).
  - §Score Versioning example JSON block: `"score_version": 2`,
    `"rank_scope": "global"`, with a parenthetical noting
    `"within-kind"` remains valid for reproducing v1 artifacts.
  - `rank_scope` field bullet: reworded to lead with `"global"` as the
    v2 default, `"within-kind"` as the retained v1 option; clarified
    that the serde deserialization default intentionally still targets
    old artifacts, not the current default.
  - Decision 6 (within-kind ranking decision): appended the 2026-07-06
    resolution to the rationale cell (status annotated "Superseded
    (rank-scope half)"), noting the `λ_missing` half of the decision is
    unaffected and still locked.
  - Phase 1 implementation-phases bullet ("RRF computation: per-signal
    ranking within-kind…"): appended a historical parenthetical noting
    the later default flip, consistent with the existing historical-note
    style used elsewhere on the same line (granularity harmonization).

## 6. Process notes

- `git status` showed no modification to the `dictionary` symlink at any
  point in this session; no `git checkout -- dictionary` was needed.
- Two commits, as directed:
  1. `feat(interesting)!: default rank scope global, SCORE_VERSION 2 (calibration lock)`
     — code + tests only.
  2. `docs(calibration): v1 lock record — gate passed, global scope wins, labels + scores committed`
     — report.md, spec doc, `labels-filled.tsv`, `scores.json`, this
     report.

## 7. Concerns / things worth the owner's attention

- The rank-scope margin is thin and mechanically fragile (§4 caveat,
  repeated in the spec and report): 3 rows out of 50 decide it, sourced
  from a 5-vs-5 unique-pattern set, and some of those swing labels were
  flagged as noisy by the owner during labeling. The pre-registered rule
  correctly resolves this as "global wins," but a future re-label pass
  focused on just the unique rows could conceivably flip it back. Nothing
  to do about this now — it's disclosed, not hidden — but it's not a
  rock-solid empirical mandate, just the mechanical output of the
  pre-registered decision procedure operating on a small, partly-disputed
  evidence set.
- `Decision 6`'s status field now reads "Superseded (rank-scope half)
  2026-07-06" — a slightly unusual half-superseded status since the
  decision bundled two independent choices (within-kind ranking +
  explicit missing-signal handling) and only one flipped. Flagging in
  case the owner prefers splitting Decision 6 into two decisions instead
  of the inline annotation used here.
