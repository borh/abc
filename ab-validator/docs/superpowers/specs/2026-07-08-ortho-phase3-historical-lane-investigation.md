# Ortho Phase-3 Historical-Kana Lane — Investigation

**Date:** 2026-07-08
**Status:** INVESTIGATION (provisional). The evidence reframes the problem; one
direction fork is open for the human partner before any build.
**Parent:** `2026-07-08-ortho-historical-scope-and-determinism-tier-design.md`
(U3/U4) and `2026-07-08-ortho-normalized-tokenizer-input-policy-design.md`
(Issue 2). This is the Phase-3 issue those specs deferred.
**Enabling infra (landed this branch):** vibrato dictionary resolution by name
(`AB_VIBRATO_DICT_DIR`) — see "Enabling infra" below; needed to run the matrix.

## Headline finding (the premise mostly does not hold)

The reserved `HistoricalToModern` lane was scoped as a **dictionary-backed
surface normalizer** (歴史的仮名遣い → 現代仮名遣い, けふ→きょう). An empirical probe
across the dictionaries this repo builds **disconfirms that framing as the main
need**:

- **UniDic already maps historical-kana surfaces to a MODERN lemma and a MODERN
  reading** — even the contemporary CWJ dictionary. けふ → `lemma=今日`,
  `pron=キョー`; 使ひ → `lemma=使う`; ゐる → `lemma=居る`; 思ふ → `lemma=思う`. The
  historical spelling is preserved in `orth`/`kana`; the modern form is already
  exposed in `lemma` (orthography) and `pron` (reading).
- The **real** differentiator between dictionaries is **segmentation**, not
  lemma exposure. CWJ mis-splits てふ → て+ふ; the historical
  `unidic-kindai-bungo` / `unidic-qkana` keep てふ as one token
  (`lemma=ちょう`/`蝶`).
- Therefore a bespoke historical→modern **surface** rewrite is largely redundant
  for getting modern token identity under UniDic. The value is in **dictionary
  selection** (which is already possible), not a new normalization kind.

Sudachi is the exception and the one place a surface normalizer would help — see
the matrix.

## Evidence — the dictionary × surface matrix

Probe: tokenize each surface under each dictionary and read the modern-form
fields (vibrato: `lemma`/`pron`; sudachi: `normalized_form`). Run via a
disposable example (`historical_probe`, not committed); reproducible with
`AB_VIBRATO_DICT_DIR` + `AB_SUDACHI_DICT` set to the nix outputs.

| surface | vibrato **cwj** | vibrato **kindai-bungo** | vibrato **qkana** | **sudachi-C** |
|---|---|---|---|---|
| けふ | `今日` / キョー ✓ | `今日` / キョー ✓ | `今日` / キョー ✓ | `けふ` (no norm) ✗ |
| 使ひ | `使う` ✓ | `使う` ✓ | 名詞 `使い` | **split** 使+ひ ✗ |
| てふ | **split** て+ふ ✗ | `ちょう` ✓ (1 token) | `蝶` ✓ (1 token) | **split** て+ふ ✗ |
| ゐる | `居る` ✓ | `居る` ✓ | `居る` ✓ | `居る` ✓ (normalized) |
| 思ふ | `思う` ✓ | `思う` ✓ | `思う` ✓ | `思う` ✓ (normalized) |
| 吾輩はゐる。けふは寒し。 | all correct ✓ | all correct ✓ | all correct ✓ | ゐる/寒し normalized; **けふ not→今日** ✗ |

Reading (each dictionary loaded once; identical rows collapsed):
- **All UniDic variants** expose modern `lemma`/`pron` for historical kana they
  recognize; even CWJ handled the whole sample sentence correctly.
- **Historical UniDic** (`kindai-bungo`, `qkana`) additionally **segments**
  historical constructs correctly (てふ), where CWJ splits.
- **Sudachi** (contemporary SudachiDict, the only sudachi dict in-repo) is
  weakest: `normalized_form` handles some variants (ゐる→居る, 寒し→寒い) but
  **not** けふ→今日, and it **mis-segments** 使ひ and てふ. There is no historical
  SudachiDict available here.

## What this means — three lanes, not one

**Lane A — dictionary selection (vibrato).** For a 旧仮名 work, tokenize under
`unidic-kindai-bungo` (or `qkana`) and read the modern `lemma`/`pron`. This needs
**no** `HistoricalToModern` normalization, no offset-map remap, no surface
rewrite. It is available **today** (`--analyzer vibrato:unidic-kindai-bungo`),
now that dictionary resolution is fixed (below). What it needs to become a real
lane: (i) pre-select old-kana works by `orthographic_style` (`新字旧仮名`/`旧字旧仮名`,
per U3 I2-D17b, a run-eligibility filter); (ii) record the chosen dictionary's
`archive_hash` in identity (already a tokenizer coordinate); (iii) test coverage
across the matrix.

**Lane B — surface normalization (sudachi / cross-analyzer comparability).** The
`HistoricalToModern` enum's original frame: rewrite 使ひ→使い, けふ→きょう in the
input, tokenize with a contemporary dictionary, remap spans back. This is the
*only* way to help analyzers that have no historical dictionary (sudachi), and
the only way to feed **all** analyzers the **same** normalized input for a fair
cross-analyzer comparison. But it is the hard linguistic problem (word-level,
dictionary-backed, not reversible — U3), and it is largely **redundant for
UniDic**, which already yields modern lemmas without it.

**Lane C — do nothing new yet.** UniDic already carries modern identity; the
segmentation win is a dictionary-selection knob that now works. Park the
`HistoricalToModern` normalizer as reserved (its U3 contract stands) until a
concrete consumer needs Lane B.

### The tension between A and B

ab-validator's purpose is **comparing analyzers on the same input**. Lane A
improves vibrato *alone* and, by giving vibrato a different dictionary than
sudachi, **confounds** cross-analyzer comparison (different tokenizations from
different inputs *and* different dictionaries). Lane B gives every analyzer the
same modernized input — better comparability and it lifts sudachi — but is the
expensive lane and mostly wasted on UniDic. Which matters more is a **product
decision**, captured as the fork below.

## How this maps onto the resolved U3 contract

- **I2-D17 (detector id binds `dictionary_hash`)**: under Lane A the
  "dictionary" is the UniDic variant itself; its `archive_hash` (from
  `flake.nix`) is already an identity coordinate — no new `OrthoDetectorId`
  needed, because there is no normalization. Under Lane B the historical
  dictionary/rules table is what must bind a hash, and the validation coupling
  applies.
- **I2-D17b (metadata pre-selection outside normalization)**: applies to both
  lanes as a run-eligibility filter on `新字旧仮名`/`旧字旧仮名`.
- **Whole-span remap warning**: only relevant to Lane B (Lane A does no remap).

## Enabling infra — vibrato dictionary resolution (landed this branch)

Running the matrix required fixing the "dictionary linking dance." Previously
every non-default UniDic had to be symlinked into `dictionary/compiled/` by a
pwd-relative `just dictionary-build-*` (stray dirs, GC-dangling links, easy to
forget). Now:

- `AB_VIBRATO_DICT_DIR` (a `:`-separated dir list) resolves dictionaries by name
  straight from the flake's combined `vibrato-dictionaries` nix output — no
  symlinking. The dev shell exports it; `just dictionary-env` prints it for CLI;
  `morph-warehouse-run-with-analyzers` sets it from the combined output.
- The decompression cache is decoupled from the source
  (`from_zstd_with_options` + `AB_VIBRATO_CACHE_DIR`, default the project-local
  `dictionary/compiled/.cache`), so a read-only nix-store source works while the
  multi-GB cache stays on the roomy project filesystem (not `$HOME`).

This is a prerequisite for Lane A being usable at all, and for testing any lane
across the dictionary matrix.

## Open fork (needs a decision before building)

1. **Direction.** Lane A (dictionary selection), Lane B (surface normalization),
   both, or Lane C (park it)?
2. **Cross-analyzer comparability.** Is comparing analyzers on old-kana text a
   real goal? If yes, Lane B is on the table; if no, Lane A/C suffice.
3. **Coverage bar.** For Lane A, is `kindai-bungo`'s segmentation good enough on
   the actual corpus (measure on 旧仮名 works), or is `qkana` better for some
   sub-genre?

**Provisional recommendation:** Lane A + park the surface normalizer (effectively
A over C). UniDic already gives modern identity; the segmentation win is a
dictionary-selection knob that now works with no new normalization code or
identity risk. Build Lane B only if cross-analyzer comparability on old-kana text
becomes a stated requirement — that is the one scenario the evidence does not
already cover.

## Non-goals / deferred

- Building the dictionary-backed historical→modern **surface** normalizer now
  (Lane B) — deferred pending the fork.
- 旧字体→新字体 (kanji-form) normalization — out of scope (a separate axis;
  `orthographic_style` records it, nothing normalizes it).
- A committed dictionary-comparison tool — the probe was disposable; formalize
  only if the matrix needs to be re-run regularly.

## Follow-ups if Lane A is chosen

- Corpus measurement: count `新字旧仮名`/`旧字旧仮名` works (needs an importer run —
  metadata is not committed) and score `kindai-bungo` segmentation on them.
- Wire `orthographic_style` pre-selection into run configuration.
- Record the selected dictionary `archive_hash` in the tokenized/input-view
  identity for old-kana works (confirm it already flows via `dictionary`).
