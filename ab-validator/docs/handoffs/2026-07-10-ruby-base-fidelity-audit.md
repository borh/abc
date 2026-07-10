# Ruby-base fidelity audit across adapters (2026-07-10)

Follow-up to the abc annotation join-stats run
(`abc/docs/handoffs/2026-07-10-annotation-join-stats-run.md`), which flagged
an adapter ruby-base defect (source `を吹《ふ》` surfacing in parser-IR as
base `を吹`). This audit root-causes that defect and checks ruby-base
fidelity across all five adapters. Seed test: the を吹 case.

## Headline findings

1. **The を吹 defect was a coordinate bug, already fixed.** At the revision
   that generated the `aozora-full-20260705T015007Z` dump, the aozora
   adapter fed the *raw* decoded source to the upstream parser but sliced
   ruby markers using spans the parser computes over its *internally
   sanitized* text. CRLF/header drift shifted every span; most drifted
   slices fail the ruby regex (falling back to raw nodes) and the few that
   happen to end at a `》` matched with corrupted bases (`を吹`, `、親`,
   `ま》あ見`, …). Fixed by `8fb55a32` ("fix: align aozora adapter span
   coordinates", 2026-07-05 15:43) — the dump predates the fix by ~13.5
   hours. Both upstream parser pins (5df2cfa5 and 1a4f864) produce correct
   spans; the parser was never at fault.
2. **The stale dump lost 99.1% of all ruby.** Corpus-wide, the 20260705 dump
   carries 32,812 ruby nodes vs 3,556,636 in the repin dump
   (`aozora-full-repin-1a4f864`, 2026-07-08, repo head `6d29b565`): 14,301
   of 17,886 works undercount. Every ruby-derived number from the 20260705
   dump is invalid, including the join-stats run's alignment rates. The
   repin dump is clean for the seed work (1,441 rubies, base 吹, zero empty
   bases corpus-wide) — **join-stats can re-run against it as-is.**
3. **aozora-rs dropped non-kanji implicit bases (fixed here).** Its
   implicit-ruby scanner only recognized kanji, so katakana/Latin bases
   (`ガラス《がらす》`, `ＮＨＫ《エヌエイチケー》`) came out empty with the
   base text left behind as plain text. Fixed in this branch: the base is
   now the trailing run of the same script class as the character before
   `《` (kanji / hiragana / katakana / Latin), matching the pinned upstream
   aozora parser and the aozora/aozora2 adapters on the audit battery.
4. **aozora2 loses the implicit base after a gaiji-bearing ruby — upstream
   bug, still live.** In upstream `aozora-core` 0.7.1 (takahashim/aozora2
   `93420b53`), once a line contains a gaiji-bearing ruby
   (`※［＃…］籠《とりかご》`), the *next* bar-less ruby on the line parses
   as `Ruby { children: [] }`: base text stays in the preceding `Text`
   node. Reproduced against the upstream crate directly; the adapter maps
   nodes faithfully. Scale: 3,541 empty bases across 1,266 works (0.12% of
   3,077,027 rubies) in the aozora2-full-20260705 dump. Recorded as an
   active adapter-fidelity note.
5. **aozora2html and aozora-epub3 are clean-ish.** All 4,418 aozora2html
   empty string-bases carry the gaiji in `base_content` (correct
   representation). aozora-epub3 has 14 marginal drops across 3 works
   (12 image-rendered gaiji bases, 2 lost kanji bases); recorded as an
   active fidelity note.

## Method

- Seed reproduction: work `000050_50770` (card 000050, 一夕話) from the raw
  corpus zip through each adapter build; AAT ruby (base, reading) pairs
  compared against the committed dumps and raw source markup.
- Root-cause bisection: pinned upstream parser (nix store, `1a4f864`) and a
  local build of the previous pin (`5df2cfa5`) probed on the same inputs
  (isolated line, full raw work, sanitized work) via `aozora inspect
  nodes -`; adapter code diffed dump-revision → HEAD.
- Cross-adapter battery: two synthetic Aozora-shaped works (CRLF, header,
  legend, footer) covering: implicit base after hiragana (seed), multi-kanji
  runs (田圃 / 東隣), explicit bar with mixed-script base, katakana run
  (窓ガラス《がらす》→ ガラス), kanji run after katakana (ガラス窓《まど》→
  窓), fullwidth Latin (ＮＨＫ), ASCII (pen), long-vowel katakana (ビール),
  iteration marks (山々), hiragana boundary (大人しい → base しい). Digits
  and halfwidth katakana take no implicit base (upstream emits no ruby
  node); the aozora-rs fix mirrors that.
- Corpus-scale checks: byte-scans of all AAT dumps for `"kind": "ruby"`
  counts and `"base": ""` (split by whether `base_content` carries a gaiji).

## Changes in this branch

- `adapters/aozora-rs/src/aat.rs`: implicit-base scanner rewritten from a
  kanji-only character test to a script-class run rule
  (`implicit_ruby_base_class`); covers both the source-fallback take and the
  supplement path (`implicit_ruby_base_before_marker`). Two new unit tests
  (non-kanji script runs; class-boundary stops).
- `adapters/aozora/tests/integration.rs`: seed regression
  `attaches_implicit_ruby_to_kanji_run_under_crlf_header_drift` pinning
  base 吹 for を吹《ふ》 on a CRLF corpus-shaped work.
- `data/adapter-fidelity-notes.toml`: two new active notes
  (`aozora2-upstream-implicit-ruby-base-lost-after-gaiji-ruby`,
  `aozora-epub3-implicit-ruby-base-drops`).

## Implications for the annotation pipeline (abc)

- The join-stats conflict class did exactly its job: it detected adapter
  infidelity, not alignment behavior. The 17.6% conflict rate is an
  artifact of the stale dump.
- Re-run `soranoha annotation-join-stats` against parser-IR converted from
  `aozora-full-repin-1a4f864` before reading any rate as linguistics.
- D7 recommendation stands: `span_preservation` = coordinate preservation
  only; base-extent fidelity is an ab-validator property, now audited here.

## Follow-ups

1. Re-run the corpus join statistics from the repin dump (replaces the
   "regenerate the AAT dump" follow-up — the dump already exists; note it
   still records a dev-build adapter path in metadata, so regenerate under
   store-pinned identity when the wrapper-adapter identity work lands).
2. File the aozora-core implicit-ruby-after-gaiji bug upstream
   (takahashim/aozora2) with the minimal repro from this audit
   (`※［＃…］籠《とりかご》のまわりを繞《めぐ》る。`).
3. Consider an ab-check property: no ruby node with empty `base` AND empty
   `base_content` (would have caught both live defects mechanically).
