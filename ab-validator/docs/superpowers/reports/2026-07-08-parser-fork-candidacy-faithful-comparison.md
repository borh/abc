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

**2. `aozora-rs` was measured through a broken adapter that bypasses the parser.**
The `aozora-rs` adapter is flagged **"not building (2026-07-03), intentionally
left in a broken state"** (`adapters/aozora-rs/README.md`). It *does* invoke
`aozora-rs-core` (`parser.rs:41` `tokenize/retokenize/scopenize`), but its AAT is
built from the **source-lexer fallback** (`aat.rs` `build_fallback_from_events`
over `ab_source_syntax::source_events`) because the typed projection from the
parser's tree is the part that fails to compile. So its conformance
(20 pass, 69% dropped-to-text) reflects a **lexer-level fallback shim, not
`aozora-rs-core`**. The parser is effectively **unmeasured** here; its 69% "drop"
must not be read as parser incapability. (Usefully, that fallback ≈ "the
`ab-source-syntax` lexer, naively projected" — see build-fresh below.)

## Faithful per-candidate assessment

| candidate | faithful capability signal | maintenance | fork cost | verdict |
| --- | --- | --- | --- | --- |
| **aozora-pipeline** | **22/25 must** via its own `inspect` (3 fails are `diagnostics`-only; nodes conformant) — the only parser measured at native granularity | best (active, June 2026) | higher (multi-crate umbrella) | **strongest, most-proven base** |
| **aozora-core** | parses ~95% of constructs; only 6 true gaps (3 `must`); AAT score is a mapper artifact | red flag (stalled since Jan 2026) | lowest (small, MIT, self-contained) | **capable + cheapest to fork, but maintenance-stalled** |
| **aozora-rs-core** | **unmeasured** (broken adapter); fast, actively maintained | active | medium (gaiji/pdfium build friction) | **can't be judged without repairing the adapter first** |
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
  Faithfully, **aozora-pipeline** is the most-proven parser and **aozora-core** is
  far more capable than its adapter showed; **aozora-rs-core** is a genuine
  unknown behind a broken adapter.
- The real trade-off is **aozora-pipeline (most capable + active, multi-crate fork
  cost)** vs **aozora-core (nearly as reachable + trivially forkable, but
  upstream looks abandoned)** — with **repair-then-measure aozora-rs** and
  **build-fresh-on-ab-source-syntax** as the two longer plays.
- No candidate is conformant out of the box: even the best (aozora-pipeline)
  misses 3 `must` (diagnostics), and aozora-core misses 3 `must` (angleQuote,
  forcedBreak, pageBreak) at the parser level.

## What is still unmeasured (honest gaps)

1. **aozora-rs-core's true capability** — needs the adapter's typed projection
   repaired (or the parser measured directly) before it can be ranked.
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
