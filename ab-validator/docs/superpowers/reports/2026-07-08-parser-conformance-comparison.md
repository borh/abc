# Aozora parser conformance comparison (spec-anchored, per-construct)

**Date:** 2026-07-08
**Instrument:** P4suta `upstream-aozora-notation-spec` conformance suite — 127
canonical vectors, 24 feature families, levels {must 25 / should 99 / may 3}.
**Harness:** `reports/parser-conformance/run-aozora-notation-spec.py`
(`just aozora-notation-spec-comparison`).
**Summary data:** `2026-07-08-aozora-notation-spec-comparison.summary.json`
(762 rows = 127 vectors × 6 adapters).

> **Authority.** The P4suta suite is **corroborating evidence, not
> authoritative** (the official 青空文庫 documentation and the works themselves
> are the authority — see memory `aozora-syntax-map-initiative`). Read every
> "pass" below as *"conforms to the P4suta reference vector,"* not *"correct."*

## Headline

**No parser passes all 25 `must`-level vectors** — not even the reference
`aozora` parser. Through the AAT lens the best genuinely-distinct parser
(`aozora2`) conforms on **12/25** must vectors; the others land far lower. The
parsers' strengths are **complementary, not nested** — no single existing parser
dominates the construct space. That measured gap is the empirical justification
for a new or forked parser, and its acceptance criterion is now concrete: green
across the 127 vectors (starting with the 25 `must`).

## What is compared, and how (method)

Two adapter modes are scored side by side:

- **`aozora` (inspect)** — the reference parser, compared **exactly** on the
  spec's `nodes`, `pairs`, and `diagnostics` projections (spans included).
- **AAT-mode adapters** (`ab-aozora`, `aozora2`, `aozora-rs`, `aozora2html`,
  `aozora-epub3`) — run `--mode aat`, their `blocks` tree flattened
  depth-first into an ordered **spec-`kind` sequence**, compared to the `kind`
  sequence of `expected.nodes`.

**Why kind-sequence and not spans:** production AAT adapters emit **no spans**
(confirmed against real output; see `docs/aat-contract.md`, `docs/aat-span-audit.md`),
so span-level comparison is impossible. The ordered `kind` sequence is the honest
common denominator across adapters.

**Projection policy (uniform across adapters):** an AAT node's spec `kind` is
taken from (1) an `x-source-marker-kind` field if the adapter declares one, else
(2) `style_type` (`boten`/`bouten`→`bouten`, `kaeriten`→`kaeriten`, indent
family→`indent`, other decoration→`emphasis`), else (3) the node `kind`
(`ruby`,`gaiji`,`tcy`→`combineUpright`,`figure`→`illustration`, container blocks
and `raw BlockStart/BlockEnd`→`containerOpen`/`containerClose`, other `raw`→
`directive`). We honor spec-kinds an adapter *declares*; we do **not** re-derive a
parser's directive semantics. A parser whose AAT self-classifies markers
therefore conforms better than one emitting raw text — which is a real,
intended fidelity signal, not a scoring quirk.

**Validation (self-check oracle):** `ab-aozora` is the reference `aozora` parser
in AAT mode. Its projected sequences reproduce `aozora`-inspect's `nodes` on
**115/122** nodes-bearing vectors (0 must-fails); the 7 residual differences are
genuine AAT-adapter quirks (e.g. marker classified `indent` vs spec
`containerOpen`, `heading` vs `headingHint`, nested-container ordering), not map
errors. This anchors the projection as faithful, so divergences from the *other*
adapters are real.

**Severity (unchanged):** a `must` sequence mismatch → `fail`; a `should`/`may`
mismatch → `warning`; the 5 vectors without `expected.nodes` and the projections
an adapter cannot answer are explicit **`skip`**s (never a silent pass).

## Overall matrix

| adapter | mode | pass | warning | fail | skip | note |
| --- | --- | ---: | ---: | ---: | ---: | --- |
| `aozora` | inspect | 113 | 11 | 3 | 0 | reference baseline |
| `ab-aozora` | aat | 115 | 7 | 0 | 5 | reference parser via AAT (self-check) |
| `aozora2` | aat | 48 | 64 | 10 | 5 | strongest distinct AAT parser |
| `aozora2html` | aat | 49 | 57 | 16 | 5 | emphasis specialist |
| `aozora-rs` | aat | 20 | 85 | 17 | 5 | broadly lossy through AAT |
| `aozora-epub3` | aat | 1 | 99 | 22 | 5 | 4 crashes; see caveat |

(Prior committed run was **0 pass / 759 warning / 3 fail** — AAT adapters were
skipped entirely. This run scores them.)

## `must`-level conformance — the parser-motivation result

| adapter | must pass | must fail | must skip |
| --- | ---: | ---: | ---: |
| `aozora` (inspect) | 22 / 25 | 3 | 0 |
| `ab-aozora` (aat) | 22 / 25 | 0 | 3 |
| `aozora2` | 12 / 25 | 10 | 3 |
| `aozora2html` | 6 / 25 | 16 | 3 |
| `aozora-rs` | 5 / 25 | 17 | 3 |
| `aozora-epub3` | 0 / 25 | 22 | 3 |

The reference's 3 `must`-fails are all **`diagnostics`** mismatches
(`pua_collision`, `tate_chu_yoko`, `unclosed_bracket`) — the parser's emitted
diagnostic codes/spans differ from the suite's expected ones; its `nodes` are
conformant. (`ab-aozora` does not score `diagnostics`, hence 0 must-fails / 3
skips instead.)

## Per-feature strengths (where each parser conforms / diverges)

Distinct AAT parsers, `pass / scored` per family (higher = more conformant):

| family (vectors) | aozora2 | aozora2html | aozora-rs | aozora-epub3 |
| --- | --- | --- | --- | --- |
| emphasis (40) | 10 | **29** | 3 | 0 |
| annotation (7) | **7** | 0 | 0 | 0 |
| container (15) | **8** | 3 | 2 | 0 |
| gaiji (7) | **6** | 5 | **6** | 0 |
| ruby (3) | **3** | **3** | **3** | 0 |
| sashie/illustration (3) | **3** | 1 | 1 | 1 |
| bouten (9) | **4** | 1 | 2 | 0 |
| heading (9) | **2** | 0 | 1 | 0 |
| keigakomi (2) | 1 | **2** | 1 | 0 |
| kunten (5) | 1 | **2** | 1 | 0 |

**Reading it:** strengths are **complementary**. `aozora2` owns
annotation/container/heading/bouten and ties on ruby/gaiji; `aozora2html` owns
emphasis (29/40) and the enclosure families (keigakomi/kunten); `aozora-rs` ties
only where the construct is a clean typed node (ruby, gaiji) and drops most
inline decoration. No parser is a superset of another — the motivation for a
consolidated parser is that **each already-existing parser covers a different
slice** of the spec.

Families where **every** AAT parser diverges (0 passes): `break`,
`structural-marker`, `tate_chu_yoko`, `warichu`, `composite`, `tables_columns`,
`angle_quote`, `kaeriten` — high-value targets for a new parser.

## Caveats (do not over-read the numbers)

1. **AAT is a lossy lens.** These scores measure conformance *as observable
   through each adapter's AAT output*, not raw parser capability. An adapter that
   parses a construct correctly but flattens it in AAT (no span, generic node)
   will under-score. The gap between `aozora`-inspect (113 pass) and `ab-aozora`
   -aat (115 pass, but 5 skips + coarser) is the size of that lens effect for the
   *same* parser.
2. **`aozora-epub3` is disadvantaged by construction.** It is a full-document
   EPUB converter; the vectors are headerless one-line snippets, which it maps to
   near-empty AAT (1 pass) and crashes on 4 (`align_end_container`,
   `font_size_block`, `indent_line_kumi`, `line_width_container`). Its column is a
   floor, not a verdict on the parser.
3. **`should`/`may` warnings are divergences, not failures** — many emphasis/
   container rows are `warning` (not `fail`) only because the vector is
   `should`-level. The per-feature `pass/scored` fractions above count those
   non-passes.
4. **P4suta is corroborating, not authoritative** (see top). A divergence from a
   vector is a lead to investigate against the official docs, not a proven bug.

## Conclusion

A new or forked Aozora parser is **empirically warranted**: no existing parser
(reference included) is green across the 25 `must` vectors, and the distinct
parsers cover **complementary, non-nested** slices of the construct space. The
suite doubles as the new parser's acceptance target — **127 green vectors, must
first** — and this harness is the ready-made scorer to track progress against it.

## Reproduce

```
just aozora-notation-spec-comparison
# → docs/superpowers/reports/2026-07-08-aozora-notation-spec-comparison.{summary.json,md}
```
