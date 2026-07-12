# Keigakomi 44-marker denominator residual — attribution

**Date:** 2026-07-12
**Tool:** `reports/aat-fidelity/keigakomi-residual-attribution.py`, run on
hinoki (`hinoki.hyakutake-barbel.ts.net`, branch `feat/parser-fork-phase5`
@ `463fcb9a`, `--jobs 28`) against the pinned nix corpus store path
`/nix/store/sdr1imwrxfldvlwzs2d2fhs11vxncgpx-aozorabunko-corpus` — the
identical store path recorded as `source_inventory.corpus` in the frozen
fidelity summary (`2026-07-09-corpus-adapter-fidelity.summary.json`).
**Output:** `2026-07-12-keigakomi-residual-attribution.summary.json`
(script output, verbatim).
**Extends:** `2026-07-11-keigakomi-yokogumi-denominator-attribution.md`,
which established the 44-marker gap (matrix-exact 673 vs. frozen
denominator 717) and ruled out UTF-8 decoding artifacts (no work in the
corpus is UTF-8-encoded).

## Both frozen figures reproduced exactly

The instrument implements two independent counting modes over the exact
17,886-work production universe (`reports/lib/corpus_reader.py`,
`work` class only; the run fails exit 2 otherwise):

- **matrix mode** — `re.finditer` over the union alternation of
  `decoration.keigakomi`'s 13 `source_patterns`
  (`data/aozora-syntax-coverage.toml:2471`), applied directly to raw work
  text. This is the technique that produced the 2026-07-11 report's 673.
- **rust-scanner mode** — a faithful Python port of the production Rust
  scanner's counting unit: `crates/ab-source-syntax/src/lib.rs`'s marker
  tokenizer (`scan_markers`/`scan_next_marker`, ported case-for-case,
  characterized against 8 of its own Rust unit-test fixtures in
  `reports/aat-fidelity/tests/test_keigakomi_residual.py`), followed by
  `crates/ab-coverage/src/source_inventory.rs`'s `matching_rows` /
  `append_composite_matching_rows`: a row earns at most one credit per
  tokenized marker (`Regex::is_match` = substring search against the
  marker's own raw text), plus one additional credit per pair of
  textually-adjacent markers whose *combined* raw text matches but
  neither half does alone.

| mode | figure | frozen target | reproduced |
| --- | ---: | ---: | :---: |
| matrix (`re.finditer` over raw text) | **673** | 673 (matrix-exact, 2026-07-11) | **yes** |
| rust-scanner (per-marker port) | **717** | 717 (frozen denominator, `source_inventory.corpus`) | **yes** |
| residual (rust − matrix) | **44** | 44 | **yes** |

`works_scanned: 17886` (candidate classes: `work` 17,886, `non_work` 5,
`recovered_extra` 2, `unreadable` 2 — the same classification as every
other Task-11-family instrument on this corpus).

Both figures reproducing exactly on a fresh, independent reimplementation
of each counting technique is itself evidence: it confirms the 44-marker
gap is a genuine property of the *counting method*, not an artifact of
either script's own bugs, corpus drift, or a different work universe.

## The mechanism: `re.finditer` non-overlap consumption vs. per-marker tokenization

`decoration.keigakomi`'s 13 `source_patterns` are not uniform in how
tightly their wildcard content is bounded:

- Pattern 4, `「[^\n]+」[のは]罫囲み`, has **no bracket delimiters at
  all** — the only bound on its content is end-of-line (`[^\n]`).
- Patterns 8–13 (e.g. pattern 8,
  `［＃「[^\n]+」は[^］]*罫囲み］`) bound their *middle* wildcard only by
  the next literal `］` — **not** by end-of-line, and not by the
  marker's own bracket pair.

When these patterns are applied via `re.finditer` directly against raw
corpus text (matrix mode), a match that starts at one
`［＃「word₁」は罫囲み］` marker can, via one of these two wildcard
shapes, extend greedily **past that marker's own closing `］`**, through
any intervening plain text, and swallow one or more *additional*,
independently well-formed `［＃「wordₙ」は罫囲み］` markers before finally
completing at the **last** reachable `罫囲み］`/`」［の|は］罫囲み` on the
line (pattern 4) or at the first reachable `］` after crossing into a
subsequent line (patterns 8–13, whose `[^］]*` does not exclude `\n`).
`re.finditer`'s non-overlapping match consumption then credits this
**entire swallowed span as ONE occurrence**, not N.

The Rust production scanner is structurally immune to this: it
tokenizes the whole document into discrete markers *first*
(`scan_next_marker`, including nested-bracket-aware
`command_end_on_same_line`, which correctly finds each command's own
closing `］` even when its body embeds unrelated nested
`［＃…］`/`※［＃…］`), and only then tests the 13 keigakomi patterns
against each marker's own already-bounded raw text. Every well-formed
`［＃「wordₙ」は罫囲み］` marker is therefore its own tokenizer unit,
regardless of how many sibling keigakomi markers sit nearby — it earns
its own row credit independent of the others.

Concrete example (`ho_urakara.txt`, lines 43–45, one continuous prose
run): the real, individually-bracketed markers `［＃「たゝる」は罫囲み］`
(line 44), `［＃「たゝり」は罫囲み］` (line 44, later in the same
paragraph), and `［＃「たゝふ」は罫囲み］` (line 45, after crossing a
`\r\n`) are three *separate*, well-formed Aozora commands. Rust-scanner
mode counts all three. Matrix mode's `re.finditer` produces **one**
match starting at `たゝる`'s opening bracket and ending at `たゝふ`'s
closing `］`, swallowing `たゝり` and crossing the line break to `たゝふ`
along the way — netting **one** credit for what are unambiguously three
distinct editorial commands.

### Composite adjacency checked and ruled out as a contributor

The rust-scanner mode also implements the Rust scanner's *composite*
credit (two textually-adjacent markers earning one credit jointly when
neither matches alone). A corpus-wide check (every candidate containing
any of 罫囲/枠囲/枠線/罫線/オモテケイ/ミシン罫, 194 works) found **zero**
composite-kind occurrences anywhere for `decoration.keigakomi` — every
one of the 717 credits is a direct, single-marker match. The 44-marker
gap is fully and exclusively explained by the swallowing mechanism
above; composite adjacency plays no part.

## The 44, fully localized: 8 named works

The corpus-wide deficit (`rust_count − matrix_count`, summed per work)
is non-zero in exactly 8 of the 157 works touched by any keigakomi form,
and sums to exactly 44:

| work | work_id | matrix_count | rust_count | deficit |
| --- | --- | ---: | ---: | ---: |
| `cards/000933/files/47177_ruby_36849.zip::ho_urakara.txt` | 000933_47177 | 14 | 30 | 16 |
| `cards/000933/files/47176_ruby_36848.zip::tokoyoto_marebitoto.txt` | 000933_47176 | 16 | 26 | 10 |
| `cards/000125/files/1317_ruby_22263.zip::kokushikan_satsujin_jiken.txt` | 000125_1317 | 7 | 13 | 6 |
| `cards/001344/files/54437_ruby_49129.zip::eta_genryuko.txt` | 001344_54437 | 1 | 5 | 4 |
| `cards/000933/files/47174_ruby_36846.zip::shijimakara.txt` | 000933_47174 | 5 | 9 | 4 |
| `cards/001021/files/49269_ruby_49545.zip::yokaigaku.txt` | 001021_49269 | 4 | 6 | 2 |
| `cards/000933/files/47196_ruby_35629.zip::nihonbungakuno_hassei1.txt` | 000933_47196 | 1 | 2 | 1 |
| `cards/000908/files/51960_ruby_41164.zip::101keio_sannen_kugatsu.txt` | 000908_51960 | 7 | 8 | 1 |
| **total** | | | | **44** |

Five of the eight are Origuchi Shinobu (折口信夫) ethnolinguistic essays
from card 000933 (`ho_urakara`, `tokoyoto_marebitoto`, `shijimakara`,
`nihonbungakuno_hassei1`, plus the card's own repeated-gloss style) —
their prose style repeatedly keigakomi-flags individual dialect/loan
words in dense succession within a single paragraph (e.g.
`tokoyoto_marebitoto.txt` line 24: `まれびと`, `まらひと`, `まらうど`,
three separate markers on one line), which is exactly the density
pattern 4's line-bound greediness swallows. The remaining three
(`kokushikan_satsujin_jiken.txt`, `eta_genryuko.txt`, `101keio_sannen_
kugatsu.txt`) show the same pattern with repeated masked/blanked text
(`「　　」は罫囲み`, full-width-space placeholders for redacted names) or
a document-count `の` genitive form —the underlying mechanism is
identical in every case: multiple independently-bracketed
`［＃「word」は罫囲み］` (or `に/に枠…`) markers sitting close enough
together that one of the construct's own bracket-less/weakly-bounded
patterns spans across more than one of them in a single `re.finditer`
pass. Every diff entry (128 total: 45 `matrix_only` giant spans, 83
`rust_only` individually-bracketed markers) is confined to these 8
works; no other work in the 17,886-work universe contributes to the
residual.

## Reading: which figure to cite

**717 (rust-scanner / per-marker semantics) is the semantically correct
count of distinct, individually-delimited `decoration.keigakomi`
editorial commands in the pinned corpus** — it is what the production
source-authority scanner (`crates/ab-coverage/src/bin/
source_inventory.rs`) computes and what `denominator: 717` means in the
frozen fidelity summary. **673 (matrix-alternation / raw-text
`re.finditer` semantics) undercounts by exactly 44, for a fully named
reason**: two of keigakomi's own 13 `source_patterns` are bounded by
end-of-line or by the next literal `］` rather than by the matching
marker's own bracket pair, so a naive flat-text regex scan (not a
per-tokenized-marker scan) can swallow multiple real, independently
well-formed markers into one non-overlapping match. This is not an
equally-valid alternative denominator under a different definition — it
is a measured counting artifact of applying these particular patterns
outside the tokenizer they were designed to be tested against.

Future keigakomi-rate readers should still name which figure and
counting method they cite (per the general spirit of the ADR's
Contract-4 discipline), since the two numbers are not interchangeable
and a rate computed against 673 vs. 717 differs by ~6.5%. This report's
recommendation, absent a reason to prefer otherwise: **cite 717**, and
cite this report for why 673 is 44 short of it.

## Verification

- `matrix_total: 673`, `rust_total: 717`, `residual_rust_minus_matrix:
  44`, `residual_matches_44: true`, `matrix_reproduces_frozen_673: true`,
  `rust_reproduces_frozen_717: true` (all in the summary JSON, verbatim
  script output).
- `works_scanned: 17886` (exit-2 gate did not fire).
- `reports/aat-fidelity/tests/test_keigakomi_residual.py`: 15 tests,
  8 characterizing the ported marker tokenizer against transcribed Rust
  `ab-source-syntax` fixtures, 7 covering pattern-union construction,
  per-row dedup semantics, composite adjacency (a synthetic case
  demonstrating the exact mechanism above with a minimal 2-marker
  fixture), and the structural matrix-mode/rust-mode asymmetry over
  bracket-less pattern 4 in free prose. Full `reports/` suite: 266
  passed (was 251 before this task's 15 additions).
