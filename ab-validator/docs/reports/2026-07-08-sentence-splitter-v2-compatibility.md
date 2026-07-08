# Sentence Splitter v2 Compatibility Report

**Date:** 2026-07-08
**Splitter:** `ab-plaintext-japanese-v2`
**Schema:** `parser-ir.schema.json` hash `sha256:a1e1b5069fdec17cbb1f94eb5e9a582d1b109dd95c07257f4da7d9b76c82cfa2`
**Mapping artifact:** `ab-validator/data/aat-to-parser-ir-mapping-v1.json` v0.2.8

## Scope

This report compares the legacy `split_sentences` / `sentence_split` behavior
(rolled out under `splitter_id: ab-plaintext-japanese-v1`) against the
fragment-aware splitter introduced alongside nested-sentence fragmentation.
Both v1 and v2 share the same `split_sentences_with_options` core; v2 is the
label under which the splitter is admitted to published parser-IR after the
nested-sentence-fragmentation design slice. The core split rules are unchanged
relative to the 2026-07-07 compatibility matrix; the only behavioral change is
that the converter now splits body text at quote-open (`「`, `『`) and
quote-close (`」`, `』`) markers before re-running the splitter inside each
region, so terminals inside an inner quote are no longer suppressed by an
upcoming closing bracket that belongs to an outer region.

## Fixture Matrix

The shared fixture matrix from `2026-07-07-sentence-splitter-compatibility.md`
carries over unchanged for the v2 core:

| Input | Expected Sentences | v1 Result | v2 Result | Divergence |
|---|---|---|---|---|
| `吾輩は猫である。名前はまだ無い。` | `吾輩は猫である。` / `名前はまだ無い。` | pass | pass | none |
| `え！？本当。` | `え！？` / `本当。` | pass | pass | none |
| `これは3.14です。終わり。` | `これは3.14です。` / `終わり。` | pass | pass | none |
| `彼は言った。）次。` | `彼は言った。）次。` | pass | pass | none |
| `一行目\n二行目。` | `一行目\n二行目。` | pass | pass | none |

All 39 `ab-plaintext` lib tests (covering CJK numbered lists, exclamation
continuation, closing-bracket depth, Western closing quotes, end-of-input
terminals, and adjacent-terminal grouping) pass under `splitter_id: v2`.

## Intentional Divergences

The nested-sentence-fragmentation design slice introduces a new pre-split
phase in the converter (`map_text_node_with_quotes` in
`ab-aat-to-parser-ir`). This is converter-level behavior and is not a change
to the `ab-plaintext` split core; v1 and v2 call the identical core function.
The divergences below therefore describe the converter, not `split_sentences`.

1. **Quote-region pre-split.** A paragraph text node like
   `先生は言った。「綺麗だ」といった。` is first split at the
   quote markers into `先生は言った。`, `「綺麗だ」`, `といった。`, then
   each region is sentence-split independently. The inner terminal is
   suppressed by the closing `」`, so the inner quote stays one fragment; the
   outer terminals (`先生は言った。`, `といった。`) each become fragments.
   The result is a fragmented sentence (I/M/F chain) rather than a single
   merged sentence row. This is required by the design so the outer
   sentence's framing punctuation is distributed to the inner fragments.

2. **Framing punctuation redistribution.** When a quote marker sits at a
   sentence boundary, the framing punctuation (`。「`/`。」`/`。」`) is
   redistributed so the terminal stays on the sentence that owns it and the
   marker stays attached to the inner quote fragment. v1 had no notion of
   framing punctuation; v2 emits the design's expected TEI exactly.

3. **No change to non-quote corpora.** Paragraphs without `「`/`『`/`」`/`』`
   markers produce identical splits under v1 and v2.

## Schema Backward Compatibility

- `splitter_id` is now an enum accepting both `ab-plaintext-japanese-v1` and
  `ab-plaintext-japanese-v2`, so previously produced v1 parser-IR artifacts
  still validate against the schema.
- The v6 conversion-audit compatibility-registry entries (measured against the
  pre-fragmentation parser-IR schema hash `0b495bb5c12c4d76…`) remain the
  authority for adapter coverage. They are retained as legacy-accepted hashes
  (`41c43f0c`, `8e568719`, `da916a3a`, `c081f236`, `0ab6f07e`, `0b495bb5`,
  `40d7ff66`) so existing manifests and registry evidence continue to
  validate.
- The new schema hash `a1e1b506…` records the additive change: four optional
  sentence fields (`part`, `fragment_group`, `next_id`, `prev_id`) and the
  `splitter_id` enum widening.

## Regression Coverage

| Suite | Command | Status |
|---|---|---|
| ab-plaintext lib | `cargo test -p ab-plaintext --lib` | 39 passed |
| ab-aat-to-parser-ir integration | `cargo test -p ab-aat-to-parser-ir` | 53 passed |
| abc Clojure focused tests | `nix build .#checks.x86_64-linux.abc-clj-nix-focused-tests` | 392 passed |
| abc clj-kondo/cljfmt | `nix build .#checks.x86_64-linux.abc-clj-kondo` | 0 errors |
| cargo check / clippy / fmt | `nix build .#checks.x86_64-linux.ab-validator-cargo-*` | pass |
