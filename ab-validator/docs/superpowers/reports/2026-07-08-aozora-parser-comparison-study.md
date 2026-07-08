# A spec-anchored, pipeline-aware comparison of Aozora-bunko parsers

**Status:** draft toward a research-publishable artifact. Consolidates and hardens
the 2026-07-08 conformance, attribution, and fork-candidacy reports into one
study with an explicit methodology and threats-to-validity section.
**Scope:** notation *conformance* and *pipeline fidelity*, not full publication
admission (which is a separate measured gate).

## Abstract

We compare five Aozora-bunko parsers three ways: **conformance** to a 127-vector
suite (breadth), **faithful capability** (each measured at native granularity with
divergences attributed to parser vs adapter), and — most decision-relevant —
**frequency-weighted corpus coverage** (how much of the markup that *actually
occurs* across ~17,800 real works each parser represents). The three views
disagree in instructive ways: conformance favors edge-case breadth; corpus
coverage is dominated by ruby (~90% of all markup mass). Normalizing AAT
representation variants (a fix that belongs in the AAT contract) is required to
measure coverage at all. The consistent result across breadth **and** mass **and**
speed is that **`aozora-pipeline` leads** (corpus coverage 0.972, 22/25 must,
1.25 s median); `aozora-rs` is a strong second on corpus coverage (0.937) and
fastest (~90×), its low conformance being a headerless-vector artifact; `aozora2`
(0.840) and `aozora2html` (0.700) trail on real texts.

> **Authority caveat (load-bearing).** The conformance suite is the third-party
> P4suta `upstream-aozora-notation-spec`. By standing project decision it is
> **corroborating evidence, not authoritative** — the official 青空文庫 documentation
> and the works themselves are the authority. Every "pass" means *"conforms to the
> P4suta reference vector,"* never *"correct."*

## 1. Research question

No prior artifact established whether any existing Aozora parser is complete
enough to serve as the basis (fork or reference) for a consolidated parser, nor
compared the parsers on a common, spec-anchored footing. This study answers:
(a) which parsers conform to which constructs; (b) where each *genuinely* diverges
(parser gap) versus where its adapter merely under-serializes; and (c) what that
implies for building or forking a Rust parser.

## 2. Pipeline context — why parser fidelity matters beyond conformance

Parsers do not emit TEI directly. The pipeline is:

```
parser (N) ──▶ [AAT] ──▶ (loss-accounted mapping) ──▶ [parser-IR] ──▶ [publication lanes] (M)
             descriptive                                decisional        TEI P5 / ABC-ext /
             adapter waist                              policy waist       sidecar / plaintext
```

- **AAT** (Aozora Adapter Tree, `data/aat-schema.json`) is the *normative adapter
  output* — descriptive, parser-shaped, preserving raw markup via a `raw` escape
  hatch. It is the input waist: N heterogeneous parsers normalize to one schema.
- **parser-IR** (`ab-aat-to-parser-ir`, `data/aat-to-parser-ir-mapping-v1.json`) is
  *decisional* — it classifies into publication lanes and carries an
  encoding-ambiguity ledger. It is the output waist: one IR serves M lanes.

**Worked example — `bouten` (傍点) end to end.** This traces one construct through
every hop and pressure-tests the descriptive/decisional separation:

| Stage | Representation | Role |
| --- | --- | --- |
| Source | `青空［＃「青空」に傍点］` | — |
| AAT | inline `style`, `style_type: "boten"` | **descriptive** — keeps sesame-dot identity |
| AAT→IR | rule `A-06`: `style → emphasis`, category **AMBIGUITY**, `records_sidecar: true` | **decisional** — generalizes to `emphasis`; diverts boten-specificity to a sidecar |
| IR→TEI | `<hi>` + rendition; ABC source-marker attribute (from the sidecar) recovers 傍点 | publication — recovers what TEI vocab can't name |

The AAT→IR mapping records a **typed loss taxonomy** — `LOSS`, `INVENTION`,
`AMBIGUITY`, `UNSUPPORTED`, `STRUCTURAL` — each with a default action and a sidecar
flag. `boten → emphasis` is `AMBIGUITY` with `records_sidecar: true`: the
generalization does **not** destroy the specific identity, it relocates it to a
channel appropriate for "detail the policy generalized away but publication may
want back."

**Consequence for this study.** AAT-lens loss (a parser/adapter emitting generic
`style`/`raw` instead of a precise typed node) is the *same phenomenon* as the
AAT→IR `AMBIGUITY`/sidecar loss. A higher-fidelity parser+adapter produces a
less-lossy pipeline end to end. So conformance is a proxy for pipeline fidelity,
and "which parser feeds the least-lossy pipeline" is the decision-relevant metric —
not "which passes the most vectors."

## 3. Methodology

### 3.1 Instrument
127 vectors (24 feature families; levels must 25 / should 99 / may 3), each with
`source` and `expected.nodes` (an ordered `{kind, span}` sequence; present on
122/127). Harness: `reports/parser-conformance/run-aozora-notation-spec.py`.

### 3.2 The faithfulness problem and per-parser measurement
Production AAT adapters omit spans and vary in completeness, so a single
AAT-serialization comparison is **apples-to-oranges**. Each parser is therefore
measured at its own native granularity, and the method is stated per parser:

| Parser | Native measurement | Rationale |
| --- | --- | --- |
| `aozora`/`aozora-pipeline` (reference) | its own `inspect` projection (nodes/pairs/diagnostics), exact | only parser exposing inspect |
| `aozora-core` (aozora2) | AAT + **divergence attribution** over its `Node` tree | adapter maps `Node` directly; attribution separates mapper loss from parser gap |
| `aozora-rs-core` | new `--mode retokenized` dump of its own token stream, projected to spec kinds | adapter is fallback-dominant; dump bypasses it |
| `aozora2html` (Ruby), `aozora-epub3` (Java) | AAT only | context/reference; **not Rust fork candidates** |

### 3.3 Divergence attribution (`attribute-divergences.py`)
Every non-passing AAT vector is bucketed: `typed-mismatch` (classified, wrong
sequence → mapper), `raw-preserved` (marker kept as `raw` → parser saw it,
unclassified → mapper/IR), `dropped-to-text` (only bare text → genuine parser
capability gap). `pass + typed-mismatch + raw-preserved` = parser-capable or
mapper-recoverable.

### 3.4 Validation oracle
`ab-aozora` is the reference parser run through the AAT path. Its projected
sequences reproduce `aozora`-inspect on 115/122 vectors (0 must-fails); the 7
residuals are genuine AAT-adapter quirks. This anchors the AAT projection as
faithful, so divergences from *other* adapters are trustworthy.

## 4. Results

### 4.1 Raw conformance matrix (762 rows = 127 × 6)
`pass 346 / warning 323 / fail 68 / skip 25` (was `0 / 759 / 3` before AAT adapters
were scored). Per adapter: `aozora` 113/11/3/0; `ab-aozora` 115/7/0/5; `aozora2`
48/64/10/5; `aozora2html` 49/57/16/5; `aozora-rs` 20/85/17/5; `aozora-epub3`
1/99/22/5. **This ranking is not the capability ranking** (§4.3).

### 4.2 `must`-level headline
No parser passes all 25 `must` vectors. `aozora`/`ab-aozora` 22/25 (the 3 fails are
`diagnostics`-only; nodes conformant); `aozora2` 12; `aozora2html` 6; `aozora-rs` 5;
`aozora-epub3` 0. **Empirical justification for a new/forked parser, measured.**

### 4.3 Faithful Rust-candidate capability (the corrected ranking)

| Candidate | Faithful signal | Measured via | Fork economics |
| --- | --- | --- | --- |
| `aozora-pipeline` | **22/25 must**, nodes conformant | native `inspect` | active, MIT/Apache; multi-crate umbrella |
| `aozora-core` | **~95% parser-capable**, 6 true gaps (3 must: angleQuote/forcedBreak/pageBreak) | `Node` attribution | MIT, trivially forkable; upstream **stalled** since Jan 2026 |
| `aozora-rs-core` | **21/122; 27% recognition** | `retokenized` dump | active/fast; **weakest on notation** |

Capability order: **`aozora-pipeline ≥ aozora-core ≫ aozora-rs-core`**.

Two distortions the naive matrix hid: (1) `aozora-core`'s "12/25" is largely a
*mapper* artifact — its adapter flattens `Node::Note` and block-types to `raw`; the
parser reaches ~95%. (2) `aozora-rs`'s "20" measures a lexer fallback, not the
parser — but measuring the parser directly *confirms* it is genuinely weak on this
corpus (it recognizes bouten/bold/ruby/indent/headings/tcy but not accent/box/
angle-quote/page-break/most annotations). A prior in-session hypothesis that
`aozora-rs-core` was a hidden-strong candidate was **disproved by direct
measurement**.

### 4.4 Complementary, non-nested strengths
Among distinct AAT parsers: `aozora2html` leads emphasis (29/40); `aozora2` leads
annotation (7/7), container (8/15), gaiji/ruby; `aozora-rs` ties only on clean
typed nodes (ruby, gaiji). No parser is a superset of another — the motivation for
a consolidated parser is that each covers a different slice. Families where *every*
AAT parser diverges (high-value targets): break, structural-marker, tate_chu_yoko,
warichu, composite, tables_columns, angle_quote, kaeriten.

### 4.5 Independent-instrument corroboration (official-docs seed)
To reduce reliance on the single P4suta instrument, an independent **seed** of 11
vectors was authored from the *official* 青空文庫 annotation documentation
(`aozora.gr.jp/annotation`, CC-BY 4.0; several verbatim examples), each provenanced
to a specific official page (`reports/parser-conformance/author-official-seed.py`,
`official-docs-seed/`). Scored by the same harness (AAT kind-sequence; the
`inspect` column is invalid here — nominal spans):

- **Core constructs replicate as universal**: ruby (explicit + implicit), bouten,
  gaiji (JIS X 0213), heading (大見出し), bold, italic **pass on all** AAT parsers
  (ab-aozora, aozora2, aozora-rs). This confirms the low P4suta scores stem from
  *edge cases*, not core recognition.
- **Weak families replicate independently**: page-break (改丁/改ページ) diverges for
  aozora2/aozora-rs; single-line 地上げ diverges for all — matching P4suta's
  `break`/`layout` weakness.

So the headline findings are **not P4suta artifacts**. Caveats: the seed is small
(11) and uses *clean canonical* forms (P4suta stresses edge cases), so it
reproduces the *relative* family weaknesses, not the low absolute pass rates; and
it reuses the shared kind vocabulary for expected labels (independent cases, shared
scale). Report: `2026-07-08-official-docs-seed-comparison.md`.

### 4.6 Performance
`full_adapter` wall-time and peak RSS over a 6-work sample of the real corpus
(`aozorabunko-corpus`), 90 s per-work limit
(`2026-07-08-parser-performance-sample.{md,json}`):

| parser | median wall | max wall | timeouts | peak RSS |
| --- | ---: | ---: | ---: | ---: |
| `aozora-rs` | **0.19 s** | 0.75 s | 0/6 | 630 MB |
| `aozora-epub3` | 0.96 s | 1.40 s | 0/6 | 351 MB |
| `aozora` (pipeline) | 1.25 s | 3.18 s | 0/6 | 503 MB |
| `aozora2html` | 6.77 s | 28.6 s | 0/6 | 540 MB |
| `aozora-core` (aozora2) | **16.9 s** | **77.0 s** | **2/6** | 152 MB |

**This flips the fork economics.** `aozora-rs-core` is fastest by ~7× over the next
Rust parser and ~90× over `aozora-core` — confirming speed as its real advantage.
`aozora-core`, the "trivially forkable, ~95%-capable" candidate, is **catastrophically
slow: 16.9 s median and 2/6 timeouts on real works.** At corpus scale that is close
to disqualifying unless the pathology is fixable. `aozora-pipeline` is both the most
capable *and* ~14× faster than `aozora-core`, with no timeouts — it now leads on
both axes. (Memory trades inversely with speed: `aozora-rs` is fastest but heaviest;
`aozora-core` lightest but slowest.)

### 4.7 Corpus coverage — how much of *real* markup each parser represents
Conformance (§4.1–4.6) measures **breadth** on curated construct sets weighted
toward edge cases. The decision-relevant question is **mass**: across the real
corpus, weighted by how often each construct actually occurs, what fraction can
each parser represent? Measured over each parser's full-corpus AAT (~17,800 works)
with **fair normalized recognition** (`reports/aat-fidelity/normalized-corpus-coverage.py`).

**Fairness method.** Recognition signatures are derived from a *full-corpus
vocabulary audit* of every adapter's real output
(`2026-07-08-adapter-aat-vocabulary-audit.txt`), not from guesses. Each construct's
signature is the **union** of every adapter's actual encoding; the encodings are
disjoint across adapters (ab-aozora emits only `x-source-marker-kind`; aozora2html
emits HTML-flavoured `style_type`s), so the union credits each adapter for its own
form without cross-contamination. Counts are 1-per-occurrence; rates are capped at
1.0 in the weighted sum.

| construct | corpus freq | aozora | aozora2 | aozora-rs | aozora2html | aozora-epub3 |
| --- | ---: | ---: | ---: | ---: | ---: | ---: |
| ruby | 3,607,926 | 0.99 | 0.85 | 0.99 | 0.74 | 0.96 |
| boten | 129,086 | ~1.0 | 0.97 | 0.95 | 0.89 | 0.98 |
| jisage_block | 94,993 | 0.97 | 0.38 | 0.46 | ~1.0 | ~1.0 |
| heading | 80,592 | 0.84 | 0.91 | 0.24 | 0.77 | 0.84 |
| font_size | 51,508 | 0.00‡ | 0.33 | 0.00‡ | 0.25 | 0.00‡ |
| tcy | 19,794 | 0.95 | 0.86 | 0.00‡ | 0.00‡ | 0.69 |
| bousen | 18,127 | 0.00† | 0.95 | 0.18 | 0.79 | 0.81 |
| figure | 5,877 | 0.99 | 0.88 | 0.91 | 0.70 | 0.05‡ |
| **freq-weighted coverage** | **4.0 M** | **0.972** | **0.840** | **0.937** | **0.745** | **0.940** |

**The audit caught real unfairness** — and fixing it did *not* change the ranking,
which is the point of checking. `aozora2html` had been scored **boten 0.00,
bousen 0.13, heading 0.10**; the audit showed it emits boten as `sesame_dot`
(+`white_circle`/`black_circle`/`bullseye`/… dot variants), bousen as
`underline_solid`, and most headings as `unmapped-h4`/`h5` styles — so the fair
figures are **0.89 / 0.79 / 0.77**, raising its total 0.700 → 0.745. Separately,
counting only `kind=="ruby"` would have reported the *reference* parser near-zero on
ruby, since ab-aozora emits ruby ~99% as `raw`+marker. This normalization belongs in
the AAT contract (canonical `style_type` enum; typed-node ≡ marker-kind); it would
also shrink downstream IR `AMBIGUITY` sidecars (§2).

**Findings (robust despite caveats below):**
- **`aozora-pipeline` leads corpus coverage (0.972)** — best on *both* mass and
  breadth, and fast. The fork base is confirmed, not a toss-up.
- **`aozora-rs` (0.937) is near the top on real texts** — its §4.3 "27%" was a
  headerless-vector artifact; with real document structure it nearly matches the
  leader. Corpus context, not curated vectors, is where it shows its strength.
- **`aozora2` (0.840) trails `aozora-rs`** despite leading AAT *conformance* —
  because ruby is ~90% of all markup mass and aozora2's ruby (0.85) lags.
- The ranking is **ruby-dominated** (ruby = 90% of weighted occurrences); read the
  weighted number as "ruby coverage, adjusted by the long tail," and the
  per-construct row for where parsers genuinely diverge (heading, tcy, bousen,
  font_size are the real differentiators).

Caveats (documented, not fairness bugs): **‡** marks a **verified-real 0** — the
vocabulary audit confirms the adapter emits *no* node for that construct (aozora-rs
`tcy`: 1 node in the entire corpus; aozora2html `tcy`: none; aozora/aozora-rs/epub3
`font_size`: none; epub3 `figure`: 305, it emits images as separate files). **†**
`aozora` bousen is **folded**, not dropped — ab-aozora emits a generic `emphasis`
marker with no distinct bousen (it keeps the span, loses the type); scored 0 for
*distinct* representation, 0.45% of mass. A few rates exceed 1.0 (a parser emits
>1 node per occurrence, e.g. epub3's per-line `jisage_block`) — **capped at 1.0**,
so coverage is not inflated; and ab-aozora's `jisage_block` is approximated by
`containerOpen`, which slightly over-attributes (it covers all block containers).
Adapters completed different work counts (aozora2html 17,689 of 17,886), so
numerators over completed works are lightly (~1%) under the corpus-wide denominator.
`aozora2html`'s ruby 0.74 is a *real* node-count ratio (verified recursive), the one
cell most worth a targeted follow-up since ruby dominates the weighted score. Fine
gaiji sub-constructs remain excluded (adapter-dropped fields — the residual
granularity wall).

## 5. Threats to validity / limitations

1. **Instrument authority.** P4suta is corroborating, not authoritative; a
   divergence is a lead to check against official docs, not a proven defect.
2. **Methodological non-uniformity.** Parsers are measured three ways (§3.2). Each
   is defensible and the reason is stated, but cross-parser numbers are not a
   single scale; read them with the method attached.
3. **`aozora-epub3` is disadvantaged by construction** (full-document EPUB
   converter fed headerless one-line vectors; 1 pass, 4 crashes) — a floor, not a
   verdict.
4. **`aozora-rs-core` residuals.** The `retokenized` dump is blind to gaiji
   (resolved in a separate `aozora-rs-gaiji` layer, ≈7 vectors under-counted), and
   a valid full-document wrapper was not confirmed (its body-selection keys off
   separator lines), so document context might lift recognition somewhat.
5. **Performance sample is small** (§4.6): 6 works, one machine, 90 s limit. The
   ranking is stark enough to be directionally trustworthy, but `aozora-core`'s
   2/6 timeouts suggest *work-specific* pathology (some inputs blow up) rather than
   a uniform constant — a larger sample would separate "slow everywhere" from
   "occasionally catastrophic." Memory figures are peak RSS, not steady-state.
6. **Conformance ≠ admission.** The 2026-07-06 acceptance criteria gate on measured
   publication accounting, valid parser-IR, zero unknown-markup counters, and perf —
   conformance is one input.

## 6. Conclusions

A new or forked Rust parser is empirically warranted: no existing parser is green
across the 25 `must` vectors, and coverage is complementary, not nested. With
performance folded in (§4.6), the two-way fork call **resolves toward
`aozora-pipeline`**: it leads on *both* capability (22/25 must) and speed (~14×
faster than `aozora-core`, no timeouts), and is actively maintained. `aozora-core`'s
trivially-forkable appeal is undercut by catastrophic performance (16.9 s median,
2/6 timeouts) — viable only if that pathology proves cheaply fixable.
`aozora-rs-core` remains a speed outlier (~90× faster) but weakest on coverage —
a candidate only if per-work throughput dominates. Build-fresh on `ab-source-syntax`
stays the long play. Because parser fidelity governs loss through the whole pipeline
(§2), the acceptance target doubles as a pipeline-quality target: precise AAT ⇒
fewer AMBIGUITY sidecars ⇒ higher-fidelity TEI.

**Recommendation:** base the new parser on `aozora-pipeline`. It is now the clear
choice on *every* axis measured — corpus coverage (0.972, highest), conformance
breadth (22/25 must), and speed (1.25 s, no timeouts) — while being actively
maintained and permissively licensed. `aozora-core`'s earlier "trivially forkable"
appeal is undercut on two independent axes (mediocre corpus coverage 0.840 *and*
catastrophic speed with 2/6 timeouts). `aozora-rs` is the credible alternative
*only* if raw throughput dominates and its heading/tcy/bousen gaps are acceptable
or cheaply closable. Concrete first steps: contribute the 3 `diagnostics`
must-fixes upstream where possible and fork for the rest; and normalize the AAT
`style_type`/marker representation (§4.7) to sharpen both measurement and the
downstream IR mapping.

## 7. Reproducibility

```
just aozora-notation-spec-comparison            # matrix + summary
python reports/parser-conformance/attribute-divergences.py  "$VECTORS"  # cause attribution
python reports/parser-conformance/measure-aozora-rs-core.py "$VECTORS"  # aozora-rs-core, faithful
# $VECTORS = "$(nix build --no-link --print-out-paths .#upstream-aozora-notation-spec)/conformance/vectors"
```

Backing data: `2026-07-08-aozora-notation-spec-comparison.summary.json`,
`2026-07-08-parser-divergence-attribution.json`,
`2026-07-08-aozora-rs-core-capability.json`. Companion reports:
`2026-07-08-parser-conformance-comparison.md`,
`2026-07-08-parser-fork-candidacy-faithful-comparison.md`.

## 8. Open work toward publication

- Expand the performance sample (§4.6) beyond 6 works and pinpoint `aozora-core`'s
  timeout pathology (which inputs, and whether it is a cheap fix).
- Resolve the `aozora-rs-core` gaiji blind spot and confirm a valid document
  wrapper (or repair its adapter's typed projection — deferred).
- Expand the official-docs seed (§4.5) from 11 clean cases toward edge-case
  coverage and precise spans, so it reproduces absolute rates, not just relative
  family weakness — a full independent instrument, not a seed.
- Uniform per-parser methodology write-up (or a normalization that puts all
  candidates on one scale).
