# Authority Correction: chuki_tag.txt is Not Authoritative

> Re-evaluates the taxonomy generator's source model after the user flagged
> (correctly) that `chuki_tag.txt` is not from the aozorabunko repo and
> `annotation/*.html` may not be comprehensive. Both confirmed by measurement.
> This handoff corrects `taxonomy-generator-done.md`'s premise.

## The premise error

`docs/handoffs/taxonomy-generator-done.md` treated `chuki_tag.txt` as a second
canonical source alongside `annotation/*.html`. **It is not.**

`references/parsers/AozoraEpub3-JDK21/chuki_tag.txt` is the internal marker→HTML
**conversion table of AozoraEpub3** — a third-party epub converter project. It
enumerates what that one converter *handles*, not what Aozora Bunko *defines*.
The authoritative source is `annotation/*.html` from aozora.gr.jp itself
(present locally at `/home/bor/Dependencies/aozorabunko/annotation/`).

## Measured: real-corpus coverage by authority

| Source | Authority | Distinct markers (normalized) | Covers real corpus |
|---|---|---:|---:|
| `annotation/*.html` | **Authoritative** (aozora.gr.jp own spec) | 263 | **84%** (27/32) |
| Real corpus (8k-file sample) | Ground truth (what authors write) | 32 | — |
| `chuki_tag.txt` | **Non-authoritative** (3rd-party converter) | 293 | **53%** (17/32) |
| Authoritative ∪ chuki | | | 84% (27/32 — **no improvement**) |

**Key finding:** chuki_tag.txt adds **0 real-corpus constructs** beyond the
authoritative `annotation/*.html`. Its 160 chuki-only entries (38% of the
generated 423-feature taxonomy) are converter-specific forms that the real
Aozora corpus never emits. The generated taxonomy is **38% inflated** by a
third-party converter's operational table.

## The 5 real constructs NEITHER source covers (the true spec gap)

1. `［＃「○○」、U+NBN、N-N］` — Unicode-codepoint gaiji form
2. `［＃「○○」の「○○」に代えて「○○」、第N水準…］` — nested substitution gaiji
3. `［＃図（figN_N.png、横N×縦N）入る］` — figure with explicit dimensions
4. `［＃（ツ）］`, `［＃（フ）］` — kunten 返り点 markers

These are Aozora constructs that authors actually write but neither the
authoritative prose manual nor the converter table spells out as `［＃...］`
examples. This is the *real* spec gap — and it can only be discovered by
reading the corpus itself, which neither spec source is comprehensive enough
to surface.

## The corrected source model

A taxonomy faithful to Aozora Bunko should be grounded in **two sources,
ranked by authority**, with the third demoted:

| Rank | Source | Role in the taxonomy |
|---|---|---|
| 1 (authoritative) | `annotation/*.html` | Defines the documented feature set — what Aozora says exists |
| 2 (ground truth) | real `cards/*.txt` corpus | Reveals what authors actually write — closes the 16% documentation gap |
| 3 (advisory, NOT a taxonomy source) | `chuki_tag.txt` | At most an "implementation coverage" column — does converter X handle feature Y — never a feature source |

## Required corrections to the generator

The committed `generate_taxonomy` bin (`crates/ab-coverage/src/bin/generate_taxonomy.rs`)
and its output (`data/generated-feature-taxonomy.md`) need to be revised:

1. **Demote `chuki_tag.txt` from a source to an advisory column.** A feature
   should enter the taxonomy only if it appears in `annotation/*.html` OR the
   real corpus. `chuki_tag.txt` membership becomes a `chuki-covered: bool`
   advisory column, not a feature source.

2. **Add the real corpus as a ground-truth source.** Walk `cards/*/txt`
   (zipped SHIFT_JIS), extract `［＃...］` markers, normalize, and union with
   the authoritative manual. This closes the 16% doc gap by construction.

3. **Re-derive the verdict column** with only authoritative + real-corpus
   evidence:
   - `DOCUMENTED` — in `annotation/*.html`
   - `OBSERVED` — in real corpus but not documented (the 5 gaps above)
   - `DOCUMENTED-AND-OBSERVED` — both
   - `CHUKI-COVERED` (advisory only) — whether AozoraEpub3 handles it

4. **Expected output size drop.** From 423 features → roughly 263
   (authoritative) + a handful of real-only gaps (≤~10 after broader corpus
   sampling), so ~270, with the 160 chuki-only entries removed from the
   taxonomy proper and retained only as advisory coverage annotations.

## What this does NOT invalidate

- The generator's *machinery* (Rust bin in ab-coverage, deterministic
  `--write`, drift-gateable) is still the right approach — the bug is the
  source model, not the tool.
- The verifier's correctness logic — the L13 段組み finding, the
  normalization — remains valid; what changes is which sources count as
  taxonomy authority.
- The overall direction (mechanical generation + Nix drift gate) is
  unaffected; only the `chuki_tag` role changes.

## Remaining work, re-ordered

1. **Rewrite the generator's source model** (the correction above). ~2 hrs.
   The bin stays; the source-loading + verdict logic changes. Re-generate
   `data/generated-feature-taxonomy.md` from the corrected sources and confirm
   the 160 chuki-only entries disappear from the taxonomy (retained as
   advisory only).
2. **Nix drift gate** (unchanged) — once the corrected generator is
   byte-stable, wire `checks.taxonomy-drift` mirroring `tei-profile-drift`.
3. **Tier 2 empirical coverage probe** (unchanged, in Clojure/abc) — but now
   its role is clearer: it provides the *ground-truth real-corpus* dimension
   the corrected generator needs. The Clojure probe that walks the 17,894
   AATs and the Rust generator can share the real-corpus evidence.

## Process lesson

I should have checked `chuki_tag.txt`'s provenance before treating it as
canonical — the skill's "calibrate, don't reflex" rule applies to sources
too, not just mutation patterns. The user's two-line question ("is this
authoritative? is the manual comprehensive?") was worth more than my
423-feature generated artifact. The measurement above is the response that
should have come first.

---

*Measured 2026-07-02. Sources: `/home/bor/Dependencies/aozorabunko/annotation/*.html`
(authoritative), `/home/bor/Dependencies/aozorabunko/cards/*/txt` (real corpus,
8k-file sample), `references/parsers/AozoraEpub3-JDK21/chuki_tag.txt`
(non-authoritative).*
