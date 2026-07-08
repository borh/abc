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
speed is that **`aozora-pipeline` leads** (corpus coverage 0.969, 22/25 must,
1.25 s median); `aozora-rs` is a clean second on corpus coverage (0.938) and
fastest (~90×), its low conformance being a headerless-vector artifact; `aozora2`
(0.855) and `aozora2html` (0.743) trail on real texts. Decomposing corpus coverage
into **fidelity** (representation quality on works a parser completes) and
**robustness** (whether it completes them) sharpens the picture without changing the
call: aozora2html is actually the *most faithful* per work (0.974) but disqualified
by catastrophic robustness, and aozora-rs's coverage rank was flattered by robustness
(lowest fidelity, 0.950) — leaving `aozora-pipeline` the only parser strong on both.

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

> **Update 2026-07-09 — the pathology is pinpointed** (`2026-07-09-aozora-core-perf-pathology.md`).
> A controlled contrast (aozora-core on the 30 works it fails at corpus scale vs. 30
> *larger* works it completes) shows the slowness is **work-specific and ruby-density
> driven, not size-driven**: the larger controls all finish (median 1.25 s, max 16 s,
> 0 timeouts) while the ruby-dense giants take 22–173 s with 12/30 timeouts. Giants
> carry ~9× the ruby density (21.4 vs 2.4 ruby/KB). All failures are timeouts, never
> crashes → a superlinear ruby-handling algorithm. So aozora-core is "occasionally
> catastrophic on ruby-dense inputs," not "slow everywhere" — confirming §4.8's claim
> that the robustness gap and the perf pathology are the same defect. A cheap fix is
> *plausible* (algorithmic) but unproven; even fixed, aozora-core still trails on
> coverage, so the recommendation is unchanged.

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
| boten | 129,086 | ~1.0∗ | 0.97 | 0.95 | 0.89 | 0.98 |
| jisage (harmonized) | 94,993 | 0.97 | ~1.0∗ | 0.46 | ~1.0∗ | ~1.0∗ |
| heading | 80,592 | 0.84 | 0.92 | 0.24 | 0.77 | 0.84 |
| gaiji | 62,355 | 0.77 | 0.82 | ~1.0∗ | 0.57 | **0.00‡** |
| font_size | 51,508 | 0.00‡ | 0.33 | 0.00‡ | 0.25 | 0.00‡ |
| tcy | 19,794 | 0.95 | 0.86 | 0.00‡ | 0.00‡ | 0.69 |
| bousen | 18,127 | 0.00† | 0.95 | 0.18 | 0.79 | 0.81 |
| figure | 5,877 | 0.99 | 0.88 | 0.91 | 0.70 | 0.05‡ |
| **freq-weighted coverage** | **4.07 M** | **0.969** | **0.855** | **0.938** | **0.743** | **0.926** |

∗ rate exceeds 1.0 (adapter emits >1 node per source occurrence — aozora boten 1.19×;
jisage per-line by aozora2/epub3; gaiji by aozora-rs 1.79×) — **capped at 1.0**, so
coverage is not inflated. **Indentation is harmonized**: aozora2 uses `jisage_line` as
a primary 字下げ encoding, folded into the signature (0.38 → ~1.0; no adapter loses).
**gaiji** (62k occ) is added; it exposes that **aozora-epub3 emits no gaiji at all**.

**The audit caught real unfairness** — and fixing it did *not* change the ranking,
which is the point of checking. `aozora2html` had been scored **boten 0.00,
bousen 0.13, heading 0.10**; the audit showed it emits boten as `sesame_dot`
(+`white_circle`/`black_circle`/`bullseye`/… dot variants), bousen as
`underline_solid`, and most headings as `unmapped-h4`/`h5` styles — so the fair
figures are **0.89 / 0.79 / 0.77**. A later vocabulary re-audit (see §4.8) added two
more normalizations: **indentation** (aozora2 emits 字下げ as per-line `jisage_line`,
folding it in lifts aozora2's jisage 0.38 → ~1.0) and **gaiji** (62k occ, previously
excluded — exposing that aozora-epub3 emits none). Separately, counting only
`kind=="ruby"` would have reported the *reference* parser near-zero on ruby, since
ab-aozora emits ruby ~99% as `raw`+marker. This normalization belongs in the AAT
contract (canonical `style_type` enum; typed-node ≡ marker-kind); it would also shrink
downstream IR `AMBIGUITY` sidecars (§2).

**Findings (robust despite caveats below):**
- **`aozora-pipeline` leads corpus coverage (0.969)** — best on *both* mass and
  breadth, and fast. The fork base is confirmed, not a toss-up.
- **`aozora-rs` (0.938) is a clean second on real texts** — its §4.3 "27%" was a
  headerless-vector artifact; with real document structure it nearly matches the
  leader. Corpus context, not curated vectors, is where it shows its strength.
- **`aozora2` (0.855) trails** despite leading AAT *conformance* — because ruby is
  ~90% of all markup mass and aozora2's ruby (0.85) lags (the indentation fix lifted
  it from 0.840); and **aozora-epub3 (0.926) slips below `aozora-rs`** once its
  zero-gaiji gap is counted.
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
Fine gaiji sub-constructs remain excluded (adapter-dropped fields — the residual
granularity wall).

**Closure on `aozora2html` ruby 0.74 (coverage conflates *fidelity* and
*robustness*).** A per-work diagnostic (`/tmp` script, method in commit) shows the
0.74 is **not** ruby lossiness: on the works aozora2html actually processes, its
per-work ruby ratio vs aozora-rs is **median 1.000 / mean 0.998** — near-perfect
fidelity. The gap is that aozora2html **fails entirely on ~197 works** (plus ~105 it
emits empty), and those are **ruby-heavy large works** (§4.8 makes this precise: 302
missed works holding **24.7% of all ruby mass**). So corpus coverage mixes two axes:
*fidelity* (markup captured *given the adapter ran*) and *robustness* (whether it
completes the work). aozora2html's fidelity is ~1.0 on ruby; its low coverage is a
**robustness** problem (a different, more fixable failure than dropping constructs).
The other adapters completed different work counts too (aozora/aozora-rs ~17,600–17,900,
epub3 17,844, aozora2 17,856), so each carries a smaller version of this. **A pure
fidelity comparison recomputed over the intersection of works *all* adapters
completed is now §4.8**; the numbers above are honest "total-mass" coverage, not
isolated fidelity.

### 4.8 Fidelity vs robustness — decomposing corpus coverage
Corpus coverage (§4.7) multiplies two independent axes: **robustness** (did the
adapter produce output at all?) and **fidelity** (*given it ran*, how faithfully did
it represent the markup?). Separating them
(`reports/aat-fidelity/fidelity-robustness-split.py`; full report
`2026-07-08-fidelity-robustness-split.md`) recomputes coverage over **I = the
intersection of works all five parsers completed** (17,518 works), scoring every
parser on an identical input set. The per-work counts are asserted to reconcile with
§4.7's full-corpus numerators, so it is the same instrument; the unit is the source
input file (basenames are shared across adapters), and fidelity is reported under two
denominators — *reference* (aozora-pipeline; primary — immune to other adapters'
over-emission) and *best-attested* (per-work max; adapter-neutral cross-check, but
inflated on gaiji/indentation where node counts are incommensurable). Both give the
same ranking; ruby (86% of mass) drives it and is proxy-insensitive.

**Robustness (completion over 17,886 works).** aozora-pipeline and aozora-rs complete
**every** work; aozora-epub3 0.996; aozora2 0.998; aozora2html 0.983. The rate hides
the damage because missed works are the *largest*: aozora2's 30 missing works hold
**13.6% of all ruby** (the timeout-on-giants pathology of §4.6 resurfacing), and
aozora2html's 302 missing works (197 no-output + 105 empty) hold **24.7% of ruby**.

**Fidelity (weighted, reference denominator):** aozora2html **0.974** > aozora2 0.973
> aozora-epub3 0.952 > aozora-rs **0.950** (aozora 1.000 by construction); the
best-attested cross-check agrees (aozora2html 0.960 top, aozora-rs 0.925 bottom, aozora
0.953). On ruby (86% of mass) all five sit at 0.97–0.999 — the entire §4.7 ruby spread
was missing works, not lossiness.

**Two inversions vs §4.7 corpus coverage — the finding:**
- **aozora2html: 0.743 (5th) → 0.974 fidelity (1st), Δ+0.231.** Its coverage deficit
  is *entirely* robustness; per work it is the most faithful parser measured.
- **aozora-rs: 0.938 (2nd) → last on fidelity (0.950 ref / 0.925 best-attested).** Its
  coverage rank was flattered by perfect robustness; on equal inputs it is the *least*
  faithful (tcy 0.000, bousen 0.152, heading 0.224 — genuine parser gaps).

**This strengthens the pick.** The axes are orthogonal and **aozora-pipeline is the
only parser top-tier on both** — fidelity ~0.97 (tied with aozora2html for the lead)
*and* robustness 1.000 *and* fastest. aozora2html's best-in-class fidelity is
disqualified by
catastrophic robustness (and lives in non-canonical encodings needing normalization);
aozora-rs is confirmed a throughput-only candidate; aozora2's robustness gap *is* its
timeout pathology (a third axis undercutting it). See the companion report for the
full per-construct table and threats.

### 4.9 Parity/support audit — the constructs coverage doesn't score
§4.7 scores the 9 highest-mass constructs; the source-authority summary lists **21**.
A systematic sweep of the remaining scoreable constructs
(`reports/aat-fidelity/parity-support-audit.py`,
`2026-07-08-parity-support-audit.json`) — signatures vocabulary-audited per adapter as
in §4.7 — surfaces where a parser drops a construct *entirely* while others represent
it (✗ = rate < 0.05). aozora is the re-measured HEAD parser (§7; control confirms
these are parser-invariant).

| construct | src occ | aozora | aozora2 | aozora-rs | aozora2html | aozora-epub3 |
| --- | ---: | ---: | ---: | ---: | ---: | ---: |
| chitsuki 地付き | 20,358 | 0.90 | ~1.0 | **✗** | **✗** | **✗** |
| burasage ぶら下げ | 13,278 | 0.93 | 0.49 | 0.39 | **✗** | **✗** |
| bold/italic | 8,830 | 0.62 | 0.39 | 0.41 | 0.56 | 0.78 |
| warichu 割注 | 6,605 | 0.58 | 0.43 | 0.46 | 0.29 | 0.44 |
| yokogumi 横組 | 3,690 | **✗** | 0.07 | **✗** | 0.37 | **✗** |
| jizume 字詰め | 3,239 | **✗** | 0.37 | **✗** | **✗** | **✗** |

**Findings:**
- **Every parser drops at least one construct entirely** — support is complementary,
  not nested, reinforcing §4.2 finding #1 across a wider construct set.
- **The recommended `aozora-pipeline` drops `jizume` (字詰め) and `yokogumi` (横組)
  outright** — two concrete, named gaps for the fork's backlog (small mass, ~0.2% of
  the corpus, but real).
- **`chitsuki` is dropped by 3 of 5** (aozora-rs, aozora2html, aozora-epub3);
  **`burasage` by 2** (aozora2html, aozora-epub3). `aozora2html`/`epub3` (the HTML/EPUB
  converters) systematically lose bottom-alignment/hanging-indent as distinct nodes.
- **`bold`/`italic` and `warichu` are represented by all five** — no parity gap.
- **Unscoreable:** `keigakomi` (罫囲み, box) is emitted by aozora-rs/aozora2html/epub3
  but has *no* source-authority denominator, so it can't be rate-scored (a gap in the
  denominators, not the parsers); `kunoji` (くの字, 10,701) is emitted as a gaiji node,
  already inside `gaiji.marker`.

These six constructs total ~56k source occurrences (~1.4% of mass) — too small to move
the §4.7 weighted ranking, which is why they were out of the headline scope; the audit
records them as the completeness frontier and the recommended parser's specific gaps.

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
   *Resolved 2026-07-09* (`2026-07-09-aozora-core-perf-pathology.md`): a 60-work
   controlled contrast confirms the pathology is work-specific and ruby-density
   driven — larger control works all complete (median 1.25 s) while ruby-dense
   giants time out (12/30). "Occasionally catastrophic on ruby-dense inputs," not
   "slow everywhere."
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
choice on *every* axis measured — corpus coverage (0.969, highest), conformance
breadth (22/25 must), and speed (1.25 s, no timeouts) — while being actively
maintained and permissively licensed. `aozora-core`'s earlier "trivially forkable"
appeal is undercut on two independent axes (mediocre corpus coverage 0.855 *and*
catastrophic speed with 2/6 timeouts). `aozora-rs` is the credible alternative
*only* if raw throughput dominates and its heading/tcy/bousen gaps are acceptable
or cheaply closable. Concrete first steps: contribute the 3 `diagnostics`
must-fixes upstream where possible and fork for the rest; and normalize the AAT
`style_type`/marker representation (§4.7) to sharpen both measurement and the
downstream IR mapping.

## 7. Reproducibility

**Measured versions (pinned in `ab-validator/flake.lock`).** The parsers are
fast-moving; every number here is a snapshot of these exact revisions:

| parser | source | rev | locked |
| --- | --- | --- | --- |
| aozora-pipeline (reference / recommended base) | `P4suta/aozora` | `1a4f864` (HEAD; §4.7/§4.8 numbers measured on `5df2cfa5`) | 2026-07-08 |
| aozora-core (aozora2) | `takahashim/aozora2` | `93420b53` | 2026-01-04 |
| aozora-rs | `kinoko0518/aozora-rs` | `2b2b8f64` | 2026-05-07 |
| conformance vectors | `P4suta/aozora-notation-spec` | `b60665fd` | 2026-07-02 |
| corpus | `aozorabunko/aozorabunko` | `0e9ea3e5` | 2026-04-24 |

> **Staleness — resolved by a controlled re-measure.** `P4suta/aozora` is under heavy
> active development; the §4.7/§4.8 numbers were measured on `5df2cfa5` (2026-07-03),
> and `flake.lock` is now re-pinned to HEAD `1a4f864` (2026-07-08, ~14 commits ahead:
> dependency bumps plus a marker→typed-node output restructuring and the schemaVersion
> 1→2 envelope bump). A **controlled experiment** — old vs new parser over the *same*
> pinned corpus — isolates the parser effect: **≤0.05 on any construct (only `tcy`,
> −0.05); every other tracked construct changes by ≤0.001.** The output restructuring
> is representation-only and absorbed by the union signatures. **Conclusions are
> unaffected.** Two infrastructure fixes were needed to re-measure on the pinned
> corpus and are now in place (so future re-pins are a ~5-min recompute): `ab-index`
> now indexes the symlinked nix corpus (it previously skipped every symlinked work
> file → 0 works), and the adapter accepts inspect `schemaVersion` 2 (verified a pure
> version bump). *Corpus caveat — resolved 2026-07-09* (§1,
> `2026-07-09-full-nix-denominator-recompute.md`): a full `ab-source-inventory` run on
> the pinned corpus shows the source-authority denominators are **corpus-invariant** —
> 17 of 21 constructs byte-identical to the local extraction, the only movement being
> `gaiji.marker` −4 and `gaiji.un_embed` −3 (an 8-work count difference); `jisage` is
> identical. The earlier "phantom gaiji/jisage drop" was a *numerator* artifact (naive
> nix re-measure before the `ab-index` symlink fix), not denominator drift. The
> fully-pinned re-run (repin numerators + pinned denominators) leaves all rankings
> unchanged (aozora 0.957 vs the mixed-corpus 0.969, the delta being the repin dump).

```
just aozora-notation-spec-comparison            # matrix + summary
python reports/parser-conformance/attribute-divergences.py  "$VECTORS"  # cause attribution
python reports/parser-conformance/measure-aozora-rs-core.py "$VECTORS"  # aozora-rs-core, faithful
# $VECTORS = "$(nix build --no-link --print-out-paths .#upstream-aozora-notation-spec)/conformance/vectors"
```

```
# §4.8 fidelity/robustness split (reconciles with §4.7; ~3 min over the AAT dumps)
python3 reports/aat-fidelity/fidelity-robustness-split.py \
  docs/superpowers/reports/2026-07-08-normalized-corpus-coverage.json \
  > docs/superpowers/reports/2026-07-08-fidelity-robustness-split.json
```

Backing data: `2026-07-08-aozora-notation-spec-comparison.summary.json`,
`2026-07-08-parser-divergence-attribution.json`,
`2026-07-08-aozora-rs-core-capability.json`,
`2026-07-08-fidelity-robustness-split.json`. Companion reports:
`2026-07-08-parser-conformance-comparison.md`,
`2026-07-08-parser-fork-candidacy-faithful-comparison.md`,
`2026-07-08-fidelity-robustness-split.md`.

## 8. Open work toward publication

- Expand the performance sample (§4.6) beyond 6 works and pinpoint `aozora-core`'s
  timeout pathology (which inputs, and whether it is a cheap fix).
- Resolve the `aozora-rs-core` gaiji blind spot and confirm a valid document
  wrapper (or repair its adapter's typed projection — deferred).
- ~~Split coverage into **fidelity** and **robustness**~~ — **done (§4.8**,
  `2026-07-08-fidelity-robustness-split.{md,json}`): aozora2html's fidelity is the
  highest measured (0.974) and its coverage deficit is pure robustness; aozora-rs's
  coverage rank was flattered by robustness (fidelity 0.950 ref / 0.925 best-attested,
  lowest). Also harmonized indentation and added gaiji to the shared signatures. The
  split reinforces the aozora-pipeline pick (only parser top-tier on both axes).
- Expand the official-docs seed (§4.5) from 11 clean cases toward edge-case
  coverage and precise spans, so it reproduces absolute rates, not just relative
  family weakness — a full independent instrument, not a seed.
- Uniform per-parser methodology write-up (or a normalization that puts all
  candidates on one scale).
- ~~Re-pin `P4suta/aozora` to HEAD and re-measure~~ — **done (§7)**: HEAD `1a4f864` is
  measurement-equivalent to `5df2cfa5` (parser effect ≤0.05, `tcy` only); `flake.lock`
  re-pinned; `ab-index` symlink + adapter schemaVersion-2 fixes landed so re-pins are
  now a ~5-min recompute.
- **Fully-nix-reproducible re-measure:** recompute the §4.7 source-authority
  denominators on the pinned `aozorabunko` corpus (they are currently from the
  original local extraction; the pinned corpus has fewer `gaiji`/`jisage` source
  occurrences). Rescales those two rows without changing rankings.
- **Close the recommended parser's construct gaps** surfaced by the parity audit
  (§4.9): `aozora-pipeline` drops `jizume` (字詰め) and `yokogumi` (横組) entirely, and
  the source-authority summary has no `keigakomi` (罫囲み) denominator to score it.
