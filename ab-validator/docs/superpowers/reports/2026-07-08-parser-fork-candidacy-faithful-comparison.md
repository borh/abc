# Faithful Rust-parser fork-candidacy comparison

**Date:** 2026-07-08
**Question:** Which Aozora parser should a new/forked parser be based on? A new
parser will be written in **Rust**, so only Rust parsers are candidates.
**Why this supersedes the raw conformance matrix:** the
`2026-07-08-parser-conformance-comparison.md` matrix is **apples-to-oranges** —
the reference was measured at parser-native granularity (`inspect`), every other
parser through the lossy AAT lens, and one candidate through a *broken* adapter.
This report puts the Rust candidates on equal footing and attributes each
divergence to its real cause.

> P4suta remains **corroborating, not authoritative** (per standing decision).

## Method

- **Fork-candidacy facts** (language/license/maintenance/forkability) gathered
  from repo Cargo.tomls + upstream crates.io/GitHub.
- **Divergence attribution** (`reports/parser-conformance/attribute-divergences.py`,
  reusing the harness's exact projection) buckets every non-passing AAT vector:
  - `typed-mismatch` — AAT has typed nodes but the sequence differs → **mapper**-level
  - `raw-preserved` — AAT keeps the marker as a `raw` node → parser saw it, unclassified → **mapper/IR**-level
  - `dropped-to-text` — only bare text remains → **true parser capability gap**
  - `pass + typed-mismatch + raw-preserved` = *parser-capable or mapper-recoverable*.

## Eligibility: Rust candidates only

| Parser (crate) | Behind adapter | Lang | License | Maintenance | Forkability |
| --- | --- | --- | --- | --- | --- |
| **aozora-pipeline** | `aozora` (reference) | Rust | MIT/Apache-2.0 | **Active** — 508 commits, v0.4.1 2026-06-15 | 4-phase lexer + arena AST; 22-crate umbrella (fork lifts several sibling crates) |
| **aozora-core** | `aozora2` | Rust | MIT | **Stalled** — ~56 commits, v0.7.1 2026-01-04, no tags | Small self-contained 3-crate workspace; **easiest to lift wholesale** |
| **aozora-rs-core** | `aozora-rs` | Rust | Apache-2.0 | Active — 151 commits, v0.6.0 2026-04-29 | Fast; **build friction** (gaiji X0213 network fetch + pdfium, already patched in `third_party/`) |
| ~~aozora2html~~ | `aozora2html` | **Ruby** | — | — | **Not a candidate** (non-Rust) |
| ~~AozoraEpub3~~ | `aozora-epub3` | **Java** | — | — | **Not a candidate** (non-Rust) |
| *build-fresh* | — (on `ab-source-syntax`) | Rust | in-repo | — | In-house lexer exists; parser tiers to be written |

Licensing rules out none of the three Rust parsers (all permissive).

## The faithfulness correction — two distortions in the raw matrix

**1. `aozora-core` was badly under-measured by its lossy mapper.**
Attributed over 122 nodes-bearing vectors:

| bucket | count | meaning |
| --- | ---: | --- |
| pass | 48 | conformant as-is |
| raw-preserved | 46 | parser saw the marker, adapter emitted `raw` → mapper-recoverable |
| typed-mismatch | 22 | classified, wrong kind/order → mapper-recoverable |
| **dropped-to-text** | **6** | **true capability gap** |

**95% (116/122) is parser-capable or mapper-recoverable; only 4% is a real gap.**
The adapter maps `aozora_core::Node` directly and is rich (`Ruby`, `Gaiji`,
`Style`, `Tcy`, `Keigakomi`, `Yokogumi`, `Caption`, `FontSize`, `Midashi`,
`Warigaki`, `Accent`, `Img`, `Kaeriten`, `BlockStart{block_type}`…), but flattens
`Node::Note` and several `BlockStart` block-types to `raw`/text
(`adapters/aozora2/src/lib.rs:974`, `:968`). So aozora-core's **12/25 must** in
the raw matrix is largely a *mapper* artifact — the parser is far more capable
than that number implies.

The genuine `aozora-core` capability gaps (only 6, 3 of them `must`):
`angle_quote` (must), `forced_break` (must), `page_break` (must),
`bracketed_kaeriten_no_pair`, `kaeriten_outside_kanbun`,
`break_in_single_line_container`.

**2. `aozora-rs`'s adapter measures a lexer fallback — but the parser, measured
directly, is genuinely weak on this corpus anyway.**
The `aozora-rs` adapter projects its AAT from the **source-lexer fallback**
(`aat.rs` `build_fallback_from_events`) whenever its typed projection does not
round-trip — which, on the conformance vectors, is nearly always
(`fallback=True` on every vector probed). So its raw conformance (20 pass) does
*not* reflect `aozora-rs-core`.

To measure the parser directly, a `--mode retokenized` dump was added
(`adapters/aozora-rs/src/dump.rs`) that emits `aozora-rs-core`'s own token stream,
bypassing the fidelity gate; `reports/parser-conformance/measure-aozora-rs-core.py`
projects it to spec kinds. **Result (faithful): 21/122 pass; the parser recognizes
*any* construct in only 34/122 (27%); it produces empty/text streams for the other
88 (72%).** So the fallback's ~20 was, coincidentally, about right — not because it
faithfully projects the parser, but because `aozora-rs-core` genuinely does not
recognize most of these constructs. Its rich 23-variant `Deco` enum
(Bold/Italic/Ruby/Boten/Bosen/Indent/Hanging/Grounded/LowFlying/A|B|C-Head/
HinV/Mama/Smaller/Bigger/VHCentre/Warichu/HorizontalLayout/Kerning/Sub/Sup) *is*
exercised for the constructs it covers (bouten, bold/italic w/ referent, ruby,
indent, headings, tcy, figure), but accent-dots, box-enclosure, angle-quote,
page/forced-break, most annotations, and "no-referent" decoration forms are simply
outside what it recognizes here.

Caveats on this measurement: gaiji is resolved in a separate `aozora-rs-gaiji`
layer *not* present in the `Retokenized` stream, so the dump is blind to gaiji
(≈7 vectors under-counted); and a valid full-document wrapper was not confirmed
(the parser keys body-selection off separator lines), so document context might
lift recognition somewhat. Even generously, `aozora-rs-core` is the **weakest of
the three Rust parsers on notation coverage** — its appeal is speed, not coverage.

## Faithful per-candidate assessment

| candidate | faithful capability signal | maintenance | fork cost | verdict |
| --- | --- | --- | --- | --- |
| **aozora-pipeline** | **22/25 must** via its own `inspect` (3 fails are `diagnostics`-only; nodes conformant) — the only parser measured at native granularity | best (active, June 2026) | higher (multi-crate umbrella) | **strongest, most-proven base** |
| **aozora-core** | parses ~95% of constructs; only 6 true gaps (3 `must`); AAT score is a mapper artifact | red flag (stalled since Jan 2026) | lowest (small, MIT, self-contained) | **capable + cheapest to fork, but maintenance-stalled** |
| **aozora-rs-core** | **21/122** faithfully (via `retokenized` dump); recognizes only 27% of constructs on this corpus | active | medium (gaiji/pdfium build friction) | **weakest on notation coverage; appeal is speed, not coverage** |
| build-fresh | lexer covers ruby/gaiji/commands/accent/notes w/ spans; needs block-assembly + semantics + SJIS/gaiji-codepoint tiers | n/a | highest (write parser tiers) | **viable long game; most control, most work** |

## Build-fresh option (`crates/ab-source-syntax`)

A production-grade in-house Rust lexer already exists: 6 `SourceEventKind` + 18
`SourceMarkerKind`, zero-copy `&str`, byte spans, nested-bracket-aware command
scanning, explicit/implicit ruby, fullwidth+ASCII gaiji/commands, accent
notation, editorial-correction notes, malformed-marker recovery, ~30 tests. It
is a **lexer, not a parser** — no block structure, no command semantics, no
Shift_JIS decode, no gaiji code-point resolution. A fresh parser would adopt it
as the lexing tier and add block-assembly + semantic + encoding layers. The
`aozora-rs` fallback numbers above are a rough empirical ceiling for
"lexer-only, naive projection": ruby/gaiji pass, everything semantic drops.

## Synthesis (for the fork decision — not yet a commitment)

- The raw conformance ranking (aozora2 "12/25") is **not** the capability ranking.
  Faithfully: **aozora-pipeline** is the most-proven parser; **aozora-core** is far
  more capable than its adapter showed (~95% reachable); **aozora-rs-core**,
  measured directly, is the **weakest on notation** (27% recognition) — its draw is
  speed. The faithful capability order is **aozora-pipeline ≥ aozora-core ≫
  aozora-rs-core**.
- The real trade-off is **aozora-pipeline (most capable + active, multi-crate fork
  cost)** vs **aozora-core (nearly as reachable + trivially forkable, but upstream
  looks abandoned)** — with **build-fresh-on-`ab-source-syntax`** as the long play.
  `aozora-rs-core` is not a fork base unless per-work speed dominates coverage.
- No candidate is conformant out of the box: even the best (aozora-pipeline)
  misses 3 `must` (diagnostics), and aozora-core misses 3 `must` (angleQuote,
  forcedBreak, pageBreak) at the parser level.

## What is still unmeasured (honest gaps)

1. **aozora-rs-core, gaiji + document-context** — measured directly here (21/122,
   27% recognition), but the `retokenized` dump is blind to gaiji (separate layer)
   and a valid full-document wrapper was not confirmed. Repairing the adapter's
   typed projection (scheduled follow-up) would let the *production* artifact
   reflect the parser and settle these residuals.
2. **Performance** — not compared here; there is a separate
   `parser-performance-all-parsers` recipe. aozora-rs advertises speed; that may
   matter for the corpus-scale acceptance gate.
3. **Parser-IR / admission fitness** — this report measures *notation
   conformance*, which is one input to the `2026-07-06` admission criteria (which
   also require parser-IR validity, publication accounting, perf evidence).

## Reproduce

```
python reports/parser-conformance/attribute-divergences.py \
  "$(nix build --no-link --print-out-paths .#upstream-aozora-notation-spec)/conformance/vectors"
# → 2026-07-08-parser-divergence-attribution.json
```
