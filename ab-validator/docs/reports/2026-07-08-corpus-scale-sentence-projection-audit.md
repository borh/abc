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

### "Other" failures — a distinct (whitespace) cause, not the container gap

Two representative works, mechanism confirmed by inspecting the offending
paragraphs and reproduced with the converter:

- **`001585` — off-by-N tiling** (`sentence spans end at 495, expected paragraph
  end 496`, `sentences.rs:411`): the body paragraph ends with a trailing
  `{"kind":"text","value":"\n"}` node after `style`/unmapped-div wrappers. The
  trailing newline is not tiled into any sentence span.
- **`001091` — no sentence spans** (`body paragraph has no sentence spans for
  non-empty byte span`, `sentences.rs:395`): the actual failing paragraph is a
  body paragraph (block 109) with content `[text, figure]` whose visible text is
  just `"\n"` — whitespace-only, while the byte span is non-empty.
  `ab_plaintext::sentence_split` returns zero spans for whitespace-only input, so
  the tiling assert bails. Reproduced directly: a paragraph whose only content is
  `"　\n"` fails identically. (An earlier draft of this report attributed this to
  a `font_size` inline container; that was wrong — the failing paragraph is
  `[text, figure]` with whitespace-only visible text, not a container.)

## Root causes — two distinct gaps

The failures split into two unrelated mechanisms, not one:

1. **Inline-container atomic-boundary (dominant):** sentence projection cannot
   split `emphasis` / `layout-span` carrying `inline_children`
   (`is_splittable_text_node`, `sentences.rs:338`, treats any `inline_children`
   node as atomic), so a boundary inside such a container hard-fails.
2. **Whitespace-only visible text (the "other" failures):** a body paragraph
   whose visible text is whitespace-only/empty but whose byte span is non-empty,
   plus trailing whitespace nodes not tiled into the last sentence. This is a
   whitespace-handling gap, independent of `inline_children`.

A residual `gaiji` atomic-boundary (1/97) exists too: a `gaiji` whose visible text
carries an interior terminal. `gaiji` is a single glyph and stays atomic, so this
is a separate, rare residual, not covered by the container fix.

## Phase B validation (2026-07-08)

After implementing Phase B (B1 recursive inline-container split + B2 whitespace
handling; see the design spec), re-running the same `calib-triage-1000` audit:

| Metric | Before (Phase A) | After (Phase B) |
|---|---|---|
| succeeded | 903 | **999** |
| failed | 97 | **1** |
| emphasis / layout-span atomic-boundary | 64 | **0** |
| whitespace ("other") | 32 | **0** |
| `gaiji` atomic-boundary | 1 | 1 (accepted, B-D7) |

The single remaining failure is the accepted `gaiji` hard-fail
(`sentence boundary falls inside atomic node gaiji at byte 37532`); no new failure
classes appeared. This satisfies U2 — publication-grade parser-IR for this corpus
is now clean except for the accepted `gaiji` residual, which unblocks Issue 4 (B4).

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

1. **Design a lossless split for `emphasis`/`layout-span` with `inline_children`**
   so a sentence boundary inside them splits the container into siblings rather
   than failing. This clears the dominant (~6.5%) atomic-boundary failures. See
   `docs/superpowers/specs/2026-07-08-parser-ir-inline-container-lossless-split-design.md`.
2. **Fix whitespace handling** (distinct from 1): absorb trailing whitespace/newline
   text nodes into the last sentence (off-by-N), and stop bailing on body
   paragraphs whose visible text is whitespace-only while the byte span is
   non-empty (the "no sentence spans" case).
3. **`gaiji` atomic-boundary residual** (1/97): accepted as a rare hard-fail
   (single glyph, no lossless split). Keep it classified separately in the audit
   so it never masks new regressions.
4. **Re-run the audit at full-adapter scale** once (1)–(3) land, and only then
   proceed with Issue 4 (fallback retirement), which requires a clean corpus.
5. ~~Fix the `audit-corpus` symlink-skipping / zero-file-silent-success behavior.~~
   Done (see the FIXED tooling section above).

## Reproduction

```
target/release/ab-aat-to-parser-ir audit-corpus \
  --aat-dir /db/ab-validator/aat-corpus/subsets/calib-triage-1000 \
  --mapping data/aat-to-parser-ir-mapping-v1.json \
  --summary-json triage1000-summary.json --report-md triage1000-report.md \
  --abc-root ../abc --jobs 12
```
