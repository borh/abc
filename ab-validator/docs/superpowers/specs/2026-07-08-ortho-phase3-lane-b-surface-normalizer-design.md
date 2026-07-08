# Ortho Phase-3 Lane B — Historical→Modern Surface Normalizer — Design

**Date:** 2026-07-08
**Status:** DESIGN → DECIDED (2026-07-08, human partner): **mechanism M2**
(`kindai-bungo` segmentation oracle + rule-based kana rewrite, `pron` as digraph
oracle), and **probe coverage first** before locking the rule set / writing the
normalizer. The M2 cross-tokenizer oracle coupling (sudachi's input derived via a
vibrato dict) is accepted for the sake of identical modernized input across
analyzers. M1/M3 recorded below as the considered alternatives.
**Parent:** `2026-07-08-ortho-phase3-historical-lane-investigation.md` (direction:
Both A and B). Lane A (dictionary selection) is landed; this designs Lane B.
**Governing contract:** `2026-07-08-ortho-historical-scope-and-determinism-tier-design.md`
(U3 — I2-D17 detector-id binds a `dictionary_hash` + validation coupling;
I2-D17b metadata pre-selection outside normalization; token-granular remap;
determinism tier). And Issue 2's source-preserving-remap invariant.

## Why Lane B exists (and where it does not)

Lane A tokenizes old-kana works under a historical UniDic (`kindai-bungo`) and
reads the modern `lemma` — no surface rewrite. That solves **vibrato**. It does
**not** help analyzers with no historical dictionary in the repo — **sudachi**
mis-segments 使ひ→使+ひ, てふ→て+ふ and does not map けふ→今日 (see the parent
investigation's matrix). Lane B is the surface normalizer: rewrite historical
kana to modern kana **in the input text**, tokenize the modernized text with any
contemporary analyzer, and remap spans back to source. Its two jobs:

1. **Lift analyzers that lack a historical dictionary** (sudachi) onto old-kana text.
2. **Cross-analyzer comparability**: feed *every* analyzer the *same* modernized
   input, so a vibrato-vs-sudachi comparison on old-kana text is not confounded
   by one side using a different dictionary (the A-vs-B tension from the parent).

## The core difficulty — no modern-surface field exists

A surface normalizer must emit the modern **surface** (使ひ→使い, けふ→きょう) — kanji
kept, historical kana modernized. But UniDic (even `kindai-bungo`) exposes:

| field | 使ひ | けふ | meaning |
|---|---|---|---|
| `lemma` | 使う | 今日 | modern **dictionary** form (not the inflected surface; kanji) |
| `pron` | ツカイ | キョー | modern **reading**, katakana (loses kanji) |
| `orth`/`orthBase`/`kana` | 使ひ / 使ふ / ツカヒ | けふ / けふ / ケフ | **historical** spelling |

So there is **no field that is the modern surface**. `lemma` is the dictionary
form (使う, not 使い); `pron` is a katakana reading (loses the kanji 使). The modern
surface must be **constructed**: keep the kanji, rewrite only the historical
*kana* to modern kana. That kana rewrite is the linguistic problem, and it is
not a flat table like kata→hira (which was a near-bijective codepoint map).

## The three transformation classes

1. **Obsolete-kana swaps (context-free).** ゐ→い, ゑ→え, ヰ→イ, ヱ→エ; and their
   dakuten/handakuten forms. Always apply. (Note: `script.rs`'s `kata_to_hira`
   already *produces* ゐ/ゑ from obsolete katakana — Lane B would *consume* them.)
2. **Word-medial kana (grammar-dependent).** Word-internal は/ひ/ふ/へ/ほ →
   わ/い/う/え/お (使ひ→使い, かは→かわ, おもふ→おもう) — but the **particles** は/へ/を
   **stay** (modern orthography keeps them). Distinguishing a medial kana from a
   particle needs word boundaries + POS.
3. **Digraph long vowels (rules with exceptions).** au/iu/eu → ō/yū/yō
   (けふ→きょう, てふ→ちょう, かう→こう), くゎ→か, ぐゎ→が, and yotsugana づ→ず, ぢ→じ.
   Regular sound-changes, but with lexical irregularities.

Class 1 is easy; classes 2–3 need morphology and carry exceptions. This is why
U3 called it "word-level, dictionary-backed, not reversible."

## The chicken-and-egg that drives the mechanism choice

Classes 2–3 need **reliable segmentation + POS** of the *historical* text (to
protect particles, to know word-medial position). But the probe shows contemporary
dictionaries (cwj, sudachi) **mis-segment historical text** — the very input Lane
B processes. So a rules-only normalizer that relies on a contemporary analyzer's
POS to guard particles is standing on unreliable segmentation. The historical
UniDic (`kindai-bungo`) is the one component that segments historical text
correctly and yields modern `lemma`/`pron`. That observation drives the fork.

## Candidate mechanisms

**M1 — Rules + contemporary-POS (self-contained, deterministic).**
Apply class-1/2/3 rules; use a contemporary analyzer (the existing `OrthoTokenizer`
seam, as the heuristic detector already does) for the particle guard.
- Pro: no historical dictionary in the loop; deterministic; a checked-in rules
  table hashes to the `dictionary_hash` I2-D17 requires. Simplest identity.
- Con: the particle guard rides on contemporary segmentation of historical text,
  which is exactly what's unreliable — expect particle mis-protection and
  medial-kana errors on the hardest cases. Lowest quality.

**M2 — `kindai-bungo` segmentation oracle + rule-based kana rewrite (hybrid).**
Tokenize under `kindai-bungo` (reliable segmentation + POS + modern `pron`), then
per token rewrite only its historical kana to modern kana by rule, using POS to
protect particles and `pron` as a cross-check/oracle for the digraph cases
(けふ's `pron=キョー` confirms きょう). Emit one token-granular annotation per changed
token.
- Pro: correct segmentation resolves the chicken-and-egg; `pron` disambiguates
  digraphs; keeps kanji, rewrites only kana. Best quality achievable in-repo
  without an external lexicon.
- Con: the normalization oracle is itself a tokenizer+dict — sudachi's input is
  derived using a *vibrato* dictionary (architectural note below). Surface↔reading
  alignment (which surface kana map to which reading morae) is a real sub-problem
  for okurigana. Identity binds *both* the `kindai-bungo` `archive_hash` and the
  rules hash.

**M3 — External historical-kana lexicon/converter.**
Import a curated 歴史的仮名遣い→現代仮名遣い mapping/library (prior-art converters exist).
- Pro: can encode irregular exceptions directly; potentially highest recall.
- Con: new external dependency (licensing, provenance, maintenance); its hash
  binds identity; coverage still needs measurement. Heaviest supply-chain surface.

## Recommendation (provisional) — M2, with a rules-only fallback

Recommend **M2**: `kindai-bungo` as the segmentation/POS/reading oracle plus a
small, deterministic, checked-in kana-rewrite rule set (classes 1–3), `pron` as
the digraph oracle. It is the only option that resolves the chicken-and-egg with
components already in the repo, and it produces modern *surfaces* (not just
lemmas). Keep **M1 (rules-only)** as a documented fallback if the cross-tokenizer
oracle dependency is judged unacceptable. Treat **M3** as out-of-scope unless
coverage measurement shows M2's rules miss too much.

## Architectural note the human partner should weigh

Under M2, **the normalization oracle is a tokenizer+dictionary**, so a work's
modernized input (consumed by *all* analyzers, including sudachi) is derived using
the `kindai-bungo` vibrato dictionary. For **comparability** this is arguably
correct — every analyzer sees the identical modernized text, produced once by a
fixed oracle, and the oracle is pinned into identity. But it does mean "sudachi's
input was shaped by a vibrato dictionary," which is a coupling worth an explicit
decision. (M1 avoids the coupling at a quality cost; M3 replaces the vibrato
oracle with an external lexicon.)

## Mapping onto the U3 contract

- **I2-D17 (detector id binds a hash + validation coupling):** add an
  `OrthoDetectorId::HistoricalRewrite { dictionary_hash, rules_hash }` (or, for
  M1, just `rules_hash`) variant, and the validation coupling that rejects a
  `HistoricalToModern` policy unless the required hash(es) are present. Under M2
  the `dictionary_hash` is `kindai-bungo`'s `archive_hash`.
- **I2-D17b (metadata pre-selection outside normalization):** unchanged — Lane B
  runs on the old-kana slice (Lane A's `--works-parquet`/`--orthographic-style`
  filter), which is eligibility, not a normalization input.
- **Token-granular annotations / remap:** emit one `OrthoAnnotation` per changed
  token so each length-changing span (けふ 6B→きょう 9B) exactly covers its offset-map
  entry and remaps cleanly; route any residual cross-boundary case to the existing
  honest `ortho_remap_crosses_boundary` error row (never silent).
- **Determinism tier:** M1/M2 are deterministic given pinned rules (+ pinned
  `kindai-bungo`), so a Lane-B policy contributes `exact` per the U4 effective-tier
  rule — provided the oracle dict + rules are bound into the policy hash.

## Open sub-questions (settle during implementation, not blocking this design)

1. **Surface↔reading alignment (M2).** For okurigana (使ひ→使い), how to map which
   *surface* kana modernize while kanji stay — pure positional rule on the kana
   suffix, or align to `pron` morae? Recommend: rewrite only the trailing/interior
   *kana runs* by rule, leave kanji untouched; use `pron` only to disambiguate
   all-kana digraph tokens (けふ, てふ). A disposable probe over a sample of real
   old-kana sentences should measure rule accuracy before locking the rule set.
2. **Coverage bar.** What error rate is acceptable, and measured how? Needs a
   labeled old-kana sample (the calibration corpus has candidates).
3. **Rules-only vs oracle-assisted (M1 vs M2).** Decide per the architectural
   note + a quality probe.
4. **Reversibility / provenance.** Historical→modern is many-to-one; the offset
   map maps *byte ranges*, not text inversion, so the source is always recoverable
   from spans — confirm no consumer needs to invert the text itself.

## Non-goals

- 旧字体→新字体 (kanji-form) modernization — out of scope (separate axis).
- Changing Lane A — dictionary selection stays the vibrato path.
- Building M3 (external lexicon) now.

## Decision forks (for the human partner)

1. **Mechanism:** M2 (kindai-bungo oracle + rules) [recommended], M1 (rules-only,
   simpler, lower quality, no cross-tokenizer coupling), or M3 (external lexicon)?
2. **Cross-tokenizer oracle coupling (if M2):** acceptable that sudachi's input is
   derived using a vibrato dictionary, given it buys identical modernized input
   for all analyzers?
3. **Sequencing:** run the accuracy probe (measure rule coverage on real old-kana
   sentences) before writing the normalizer, or implement M2 behind the existing
   honest-failure path and iterate on coverage?
