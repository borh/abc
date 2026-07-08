# Corpus-Scale Sentence Projection Audit

**Date:** 2026-07-08
**Owner:** ab-validator
**Governs:** parser-IR sentence segmentation readiness for publication; input to
the atomic-node lossless-split decision (Open Question 2 of
`2026-07-07-parser-ir-sentence-segmentation-and-ortho-tei-design.md`) and to the
legacy-fallback retirement decision.

## What was run

`ab-aat-to-parser-ir audit-corpus` over two checked-out subsets of the aozora2html
adapter corpus, using the checked-in mapping (`data/aat-to-parser-ir-mapping-v1.json`)
and ABC schemas:

| Subset | Files | Succeeded | Failed | Failure rate |
|---|---|---|---|---|
| `calib-smoke-100` | 100 | 92 | 8 | 8.0% |
| `calib-triage-1000` | 1000 | 903 | 97 | 9.7% |

The rate is stable across the two samples. Every recorded failure is a
sentence-projection failure; no other conversion-stage errors appeared in these
subsets.

## Failure breakdown (calib-triage-1000)

`sentence_projection_failures` aggregate:

| Class | Count | Detail |
|---|---|---|
| Atomic-boundary | 65 | `emphasis` 54, `layout-span` 10, `gaiji` 1 |
| Other | 32 | "no sentence spans for non-empty byte span" (dominant) + off-by-N tiling gaps |

### Atomic-boundary failures — answers Open Question 2

Sentence terminals fall inside atomic nodes in **~6.5% of works**, dominated by
`emphasis` (54/65). This is frequent enough that the hard-`bail!` failure mode is
a real publication blocker for this corpus, not a theoretical edge. The spec's
Open Question 2 asked whether this occurs in real corpus data before relaxing the
failure mode — it does, and the answer points to designing a lossless split for
inline-container nodes rather than keeping the hard fail or snapping boundaries.

### "Other" failures — same root cause

Two representative works, mechanism confirmed by inspecting the offending
paragraphs:

- **`001585` — off-by-1 tiling** (`sentence spans end at 495, expected paragraph
  end 496`, `sentences.rs:411`): the body paragraph ends with a trailing
  `{"kind":"text","value":"\n"}` node after `style`/unmapped-div wrappers. The
  trailing newline is not tiled into any sentence span.
- **`001091` — no sentence spans** (`body paragraph has no sentence spans for
  non-empty byte span`, `sentences.rs:395`): the paragraph's entire visible text
  lives inside a `font_size` inline-container node, so projection produces zero
  sentence rows for a non-empty span.

## Single root cause

All three failure families reduce to one gap: **sentence projection does not
recurse into inline-container nodes** (`emphasis` / `layout-span` / `style` /
`font_size` carrying `inline_children`). Body text wrapped in such a container
either hard-fails (a boundary lands inside it) or disappears from segmentation (a
paragraph made only of one yields no rows). `is_splittable_text_node`
(`sentences.rs:338`) treats any node with `inline_children` as atomic.

Because ~9–10% of publication-grade works currently fail conversion, **all
publication-grade parser-IR is not yet regenerated cleanly** — this gates Issue 4
(legacy-fallback retirement), which assumed a clean regeneration.

## Tooling finding (FIXED): `audit-corpus` silently skipped symlinked corpus entries

The `calib-*` subsets are directories of **symlinks** into the main corpus.
`collect_json_files` filtered on `entry.file_type().is_file()`, which does not
follow symlinks, so a symlink farm audited as **0 files** with a success exit
code — the initial `/db` runs reported "0 succeeded, 0 failed" for this reason
(not, as first suspected, a sandbox `/db` read restriction; the binary reads
`/db` fine).

Fixed in `audit.rs`: `collect_json_files` now resolves entries with
`fs::metadata` (following symlinks; broken links are skipped), and
`collect_inputs` fails loudly when a passed `--aat-dir` collects zero files.
Covered by `collect_json_files_follows_symlinked_entries` and
`collect_inputs_errors_on_directory_with_no_aat_files`. The fixed binary audits
the `/db` symlink subsets directly with no `cp -L` staging.

## Regression fixtures added (Issue 6)

The five-scenario regression matrix is now covered:

| Scenario | Status |
|---|---|
| (a) no-ortho sentence rows | pre-existing |
| (b) multiple ortho annotations in one sentence | **added** — `integration.rs::ortho_indices_cover_multiple_annotations_in_one_sentence` |
| (c) one annotation spanning two sentences | **added** — `integration.rs::ortho_annotation_spanning_two_sentences_tags_both` |
| (d) ruby inside tagged sentences | pre-existing |
| (e) atomic boundary failure | **added** — shared fixture `tests/fixtures/atomic-boundary-emphasis-input.aat.json` + `integration.rs::rejects_sentence_boundary_inside_atomic_emphasis_from_aat` |

The (e) fixture reproduces the dominant real-corpus failure end-to-end
(AAT → parser-IR): an `emphasis` node with mixed inline `inline_children` and an
interior terminal, failing with `sentence boundary falls inside atomic node
emphasis at byte 6`.

## Recommended next steps

1. **Design a lossless split for inline-container nodes** (emphasis / layout-span /
   style / font_size with `inline_children`) so a sentence boundary inside them
   splits the container rather than failing. This clears the majority (~6.5%) of
   failures and is the substantive follow-up the corpus evidence justifies.
2. **Fix trailing whitespace/newline tiling** so trailing text nodes are absorbed
   into the preceding sentence span (clears the off-by-N "other" failures).
3. **Cover all-container paragraphs** so a paragraph whose text is entirely inside
   a container still yields sentence rows (clears "no sentence spans").
4. **Re-run the audit at full-adapter scale** once (1)–(3) land, and only then
   proceed with Issue 4 (fallback retirement), which requires a clean corpus.
5. Fix the `audit-corpus` symlink-skipping / zero-file-silent-success behavior.

## Reproduction

```
target/release/ab-aat-to-parser-ir audit-corpus \
  --aat-dir /db/ab-validator/aat-corpus/subsets/calib-triage-1000 \
  --mapping data/aat-to-parser-ir-mapping-v1.json \
  --summary-json triage1000-summary.json --report-md triage1000-report.md \
  --abc-root ../abc --jobs 12
```
