# Parser-RQ Corpus Tiering Design

Date: 2026-07-26 (revised 2026-07-27)
Status: The three-tier **architecture** is proposed for acceptance. Of the five
items previously open, **Q11 and D7 are decided** and **Q2 is closed by
measurement**; closing Q2 corrected a wrong instrument attribution in D6, and the
successor probe **Q12** then closed as well.

**The headline is no longer the architecture.** Q12 measured
`source-span-coverage` — the predicate the architecture would carry to tier 2 —
against 299 real works and **not one reaches its declared `:= 1.0`** (fold
0.9640, worst 0.2466), while the governed corpus sits at exactly 1.0. This is
**confirmed through the built `ab-parser-rq-source-accountability` binary**, not
inferred: same binary, same taxonomy, same policy hash, 177/177 bytes recognized
on the governed corpus and 12,597,285/13,068,347 on real source. The current
predicate and the proposed tier 2 population are incompatible, and 177 bytes of
corpus is why nobody could see it. That is tracked as **Q14**; it gates the
confirmatory campaign the whole governance path builds toward, and it now needs a
decision rather than more measurement.

The rest blocks something specific rather than everything: **Q3**
(LOSS/AMBIGUITY untraced) blocks *predicting* outcomes from D6, not measuring
them; **D8** (repetition count is a schema constant) blocks tier-relative
repetitions; **Q9** (population equality, unproven) blocks calling tier 2 a
census — its prerequisite Q11 is settled, so the equality target is fixed. See
*What each open item blocks*.
Depends on: `docs/adr/decisions.edn` slugs `custom-parser-release-qualification`,
`governed-qualification-corpus-location`, `parser-release-instrument-bindings`

## Purpose

Separate the incompatible jobs currently carried by one 177-byte qualification
corpus, so that corpus size stops being an accidental ceiling — without
pretending the corpus-design decision disappears.

## Current Fault

`data/parser-release-qualification-corpus.edn` holds three synthetic works
totalling **177 bytes** (`1_ruby` 43, `2_gaiji` 58, `3_both` 76). All nine
predicates in `data/parser-release-qualification-predicates.edn` are observed
over that corpus, and `custom-parser-release-qualification` — which carries
`:release-authority :publication` — declares `:validation-scope :smoke-corpus`.

Two consequences:

- The parser was **selected** on `:full-corpus` evidence
  (`aozora-parser-selection`, `parser-fork-hard-detach`) and is
  **release-qualified** on 177 bytes. No policy explains why the narrow
  observation suffices for publication.
- One predicate, `wall-time ≤ 300 s`, is a function of corpus cardinality, so
  growing the corpus breaks it for a reason unrelated to the parser.

## Evidence

All figures below are measured, single-host, on the pinned `aozorabunko-src`
checkout at rev `0e9ea3e`. Method and limits are stated because several are
extrapolations, and one earlier measurement in this investigation was invalid
for a method reason (noted inline).

### Corpus inventory

Deterministic walk of `cards/**/*.zip`, first `.txt` member per archive,
sorted by archive path.

| Quantity | Value |
|---|---|
| Works inventoried | 17,878 |
| Archives unreadable by stock `zipfile` | 4 |
| Total source bytes | 568,169,767 (568 MB) |
| Mean / median source bytes | 31,780 / 10,879 |
| Largest **text** work | 2,116,173 (2.1 MB) |
| Max ruby / gaiji / single-line / lines | 75,892 / 1,062 / 28,245 / 19,549 |
| Max annotation — `annot_markers` / `annot_non_gaiji` | 8,191 / **8,118** |

Both annotation maxima are given because they are different metrics: every gaiji
marker `※［＃` contains the annotation opener `［＃`, so `annot_markers`
double-counts gaiji. **The selector ranks on `annot_non_gaiji` (8,118)**, so that
is the figure describing correctness-tail membership; `annot_markers` (8,191) is
the raw opener count.

The 4 unreadable archives are the same ones already known to the corpus
evidence: `cards/001154/.../chihobunkano_shinkensetsu.zip`,
`cards/001393/files/50710_ruby_36965.zip`,
`cards/001505/files/58100_txt_60357.zip`,
`cards/001562/files/56151_ruby_60063.zip`. ABC's admission path recovers them;
the inventory does not reimplement that recovery.

Note: `max_member_bytes` in `data/source-bundle/aozorabunko-0e9ea3e-summary.json`
is 12,631,833, which is the largest member **of any type** (not text). The
largest text work is 2.1 MB. Reconciling 17,878 against the summary's
`admitted_zip_count` 17,880 and the audit's `files_scanned` 17,886 is **Q9, a
prerequisite for the tier 2 population definition** — not a minor item. Three
tools counting one snapshot differently is the evidence that these are three
different populations.

### Cost decomposition

| Layer | Measurement | Method |
|---|---|---|
| Parse, serial | **6.57 ms/work**, 4.6 MB/s | 400 works, `random.seed(20260726)`, one `ab-aozora --mode aat` per work; sample mean 30,332 B vs true mean 31,780 B |
| Process spawn floor | ~0.99 ms | 50 × `ab-aozora` on empty input |
| `ab-check --jobs 1` | fixed **327 ms**, per-work **25.12 ms** | two-point fit over N = 50, 200, 800 |
| `systemd-run --user --wait --collect` | **20.3 ms/work** (median 15.6, max 38.0) | 20 × `true`, with the resource policy's own properties |
| **RQ capture layer** (`capture-corpus`) | fixed **7,410 ms**, per-work **232.4 ms** | Q1; least-squares fit over N = 50, 101, 200, 299, cold store per run, best of 2 |

Parse is **26%** of `ab-check`'s per-work cost; the remaining **74%** is zip
open/read, adapter spawn, AAT deserialize, checks, and report write.

### Q1, closed — the capture layer dominates everything below it

Q1 asked whether the layer above `ab-aozora` and `ab-check` — evidence writing,
hashing, envelope construction — "could add materially to 25.12 ms/work". It
does not add to it; **it dwarfs it by an order of magnitude.**

| Component of `capture-corpus` | ms/work | Share |
|---|---|---|
| Re-parse done in-process (AAT + diagnostics) | 5.11 | 2.2% |
| P1 node-span accountability (`analyze-corpus`) | 9.80 | 4.2% |
| **Capture generation, recognition, aggregates** | **222.57** | **95.8%** |
| **Total** | **232.37** | — |
| *Reference:* all of `ab-check --jobs 1` | *25.12* | — |

**The capture layer is 9.25× the entire `ab-check` per-work cost**, and only
2.2% of it is parsing. Note the re-parse: `capture_generation_from_bytes_*` runs
the AAT and diagnostics parsers again in-process, so that 5.11 ms is work the
6.57 ms/work parse figure already counted — it is small enough not to matter, and
that is itself the finding. Note also that P1 — the node-span basis the *D6
correction* shows is not authoritative for any predicate — costs 9.80 ms/work,
more than the parse.

Marginal cost is **byte-proportional, not per-work**: net of the fixed 7,410 ms,
the measured cost is ~5,400 ms per MB of decoded source across N = 101, 200, 299
(5,383 / 5,417 / 5,441), i.e. ~**0.185 MB/s**. Extrapolating a single serial
capture pass to the full snapshot, by total source bytes (×64.7) and by work
count (×59.8) respectively:

| Path | Full-snapshot serial |
|---|---|
| `ab-check --jobs 1`, ×1 | 449 s (7.5 min) |
| `ab-check --jobs 1`, ×3 repetitions | 1,348 s (22.5 min) |
| **RQ capture layer, one pass** | **≈ 4,700–5,100 s (78–85 min)** |

**One capture pass is roughly 3.5× the entire three-repetition `ab-check`
campaign the design previously treated as the cost model.** Any tier 2 wall-time
budget derived from `ab-check` alone is wrong by an order of magnitude, which
sharpens the *Wall-Time Reshape*: the confounded 300 s predicate is not merely
measuring the wrong things, it is measuring the cheap layer.

**Q6 falls out of the same run.** The content-addressed store holds
**662 KB/work** — a **15.2× expansion over decoded source** — dominated by the
AAT `parser_output` blob, consistent with the 15.7× AAT expansion in *Tail
behaviour*. At 17,878 works that is **≈ 12 GB** per capture generation. P1's own
records are negligible by comparison (2,178 B/work). Retention and
closed-membership cost remain unmeasured, so Q6 is narrowed, not closed.

Method limits: serial by construction, so this is governed serial cost and not
achievable throughput — `capture-corpus` was not tested under concurrency, and
the 26× figure in D5 is an `ab-check` result that must not be transferred to it.
Single host, cold store per run, best of 2. Same 299-work stride sample and
synthesized identity as *Q12, closed*; the sample's mean source size (29,355 B)
is 0.92× the corpus mean (31,780 B), which is why the extrapolation is given
both ways.

Extrapolated to 17,878 works:

| Path | ×1 | ×3 repetitions |
|---|---|---|
| `ab-check --jobs 1` | 449 s (7.5 min) | **1,348 s (22.5 min)** |
| `systemd-run` overhead alone | 363 s (6.0 min) | — |

The same 800 works at `--jobs 32` take **0.78 s** against 20.42 s serial — a
**26× speedup** at 28.0 MB/s, implying ~20 s for the full corpus. Serialization
is a measurement choice, not an inherent cost.

### Tail behaviour

25 works selected by rank on six dimensions (top 5 each), 22.2 MB total. Only
**5 of 30 slots overlap**, so the extremes lie on largely independent axes.

| Quantity | Value |
|---|---|
| Worst per-work parse | 1.339 s against a 60 s per-work timeout — **45× headroom** |
| Failures at the extremes | none; all 25 exit 0 |
| AAT output expansion | up to **15.7×** source bytes |
| Peak RSS, isolated per work | 539 / 305 / 389 / 33 MB |
| Peak RSS vs source | 200–600×; model ≈ **30 MB + ~250× source** |
| Worst observed peak vs 2 GiB threshold | 539 MB — **4× headroom** |

At that model a work of roughly **8 MB source would breach the 2 GiB memory
threshold**. The largest text work today is 2.1 MB, so the margin is real but
not large, and it is composition-dependent.

Method note: an initial RSS measurement was invalid because
`getrusage(RUSAGE_CHILDREN).ru_maxrss` is a monotonic high-water mark across all
reaped children and cannot be attributed per work. The figures above are
re-measured in isolated processes.

## Diagnosis

### D1 — One corpus carries three incompatible jobs

A per-PR **gate** wants small, fast, adversarial, expected-output-bearing. A
**qualification** corpus wants exhaustive exposure. A **resource envelope**
wants extremes, not averages. The three current works are defensible as feature
fixtures and inadequate as either of the other two. Both "3 is deliberate" and
"3 is far too few" are true, of different jobs.

They are not, however, credible as a general regression gate: the feature
universe is not limited to ruby and gaiji, and all three declare
`:expected_status :parsed` with `:expected_diagnostics []`, so **no adversarial
case is exercised**.

### D7 — An adversarial fixture tier is incompatible with `fatal-failures ≤ 0`

This blocks the tier as proposed, and it is a semantics decision rather than an
authoring task.

`parser_rq_core_attempt.clj:218` computes

```clojure
:fatal_failures (count (filter #(= "fatal_error" (:disposition %)) records))
```

with **no reference to the corpus member's `:expected_status`**.
`allowed_dispositions` merely permits `fatal_error` to travel through the capture
protocol; it does not make an *expected* fatal rejection pass. So a fixture that
deliberately expects fatal rejection increments `fatal_failures` and fails
`fatal-failures ≤ 0`.

The adversarial population the fixture tier promises therefore cannot be adopted
under the current predicate. Three ways out, to be chosen explicitly:

1. **Redefine as `unexpected-fatal-failures ≤ 0`**, comparing each observed
   disposition against a governed expected disposition. Conceptually strongest —
   it makes the predicate say what it means — but it rotates instrument semantics
   and `predicate_set_hash`, hence `qualification_identity_ref`.
2. **Keep malformed-input conformance in a separate negative-test lane** that does
   not feed the release fatal-failure aggregate. Cheapest; leaves the release
   predicate untouched but splits where parser conformance is asserted.
3. **Require every supported malformed input to produce a non-fatal
   parsed-and-diagnosed result**, and state that requirement explicitly. Keeps one
   aggregate, but constrains what the fixture tier may contain.

**Status: DECIDED 2026-07-27 — Option 1, `unexpected-fatal-failures ≤ 0`.**
The predicate is to be redefined to compare each observed disposition against a
governed expected outcome, so that it says what it means. This is a decision to
adopt, not an implementation: the prerequisites below are unchanged and must be
discharged first, in this order —

1. **untangle `allowed_dispositions`** (it lists the *status* value
   `protocol_error` in a *disposition* vocabulary; nothing can be compared
   against it until the two levels are separated);
2. specify the two-level expected-outcome model and its closed,
   schema-enforced vocabularies, including which values are never expectable;
3. adopt the safe initial contract below — expectations restricted to
   `parsed` and `fatal_error`;
4. map corpus expectations to wire values, authenticate exact membership
   between expectations and observed records, and specify behaviour for a
   missing or unknown expectation.

Sequence with the `wall-time` reshape so one identity rotation covers both.
Until step 4 lands, the fixture tier still cannot carry adversarial cases —
**the decision unblocks the work, it does not unblock the tier.**

The rationale for the choice is recorded below and stands as written. It is
not merely a matter of reading an existing field: `:expected_status` is in
`corpus-entry-identity-keys`, so it already moves corpus identity, but **no schema
validates its vocabulary** and nothing compares it against core-attempt
dispositions. Adopting `unexpected-fatal-failures ≤ 0` first requires:

- an **expected-outcome model, not merely a disposition vocabulary**. The work
  schema already separates two levels, and a comparator over dispositions alone
  cannot address the first:

  | Level | Schema enum |
  |---|---|
  | record `status` | `measured`, `protocol_error`, `unavailable` |
  | measured `disposition` | `parsed`, `fatal_error`, `adapter_timeout` |

  Note the policy's `allowed_dispositions` is
  `["parsed", "fatal_error", "adapter_timeout", "protocol_error"]` — it **already
  conflates the two levels**, listing a *status* value in a *disposition*
  vocabulary. That confusion has to be resolved before anything compares against
  it;
- a closed, schema-enforced vocabulary at each level, plus which values are
  **never expectable**;
- a mapping from corpus expectations to wire values;
- exact-membership authentication between expectations and observed records;
- specified behaviour for a missing or unknown expectation.

A safe initial contract: allow only **`parsed` and `fatal_error`** as
expectations, keeping `adapter_timeout`, `protocol_error`, and `unavailable`
unconditional failures — an expected timeout would make the timeout predicate
vacuous, and an expected protocol error would let a harness fault masquerade as a
governed outcome.

Until those are specified, the fixture tier cannot contain the adversarial cases
this design promises.

### D8 — Repetition count is a protocol constant, not an analyzer detail

Making repetitions tier-relative is a **versioned core-attempt protocol
migration**, not a code change. `repetitions` is fixed as
`{"const": 3}` in `schemas/parser-rq-core-attempt-policy.schema.json:16` — the
schema itself forbids any other value — and the constant recurs across:

- the capture-authorization, core-attempt policy, work, index, and aggregate
  schemas;
- `parser_rq_core_attempt.clj` — `(range 1 4)` in `index-errors` expected-pair
  construction (line 68), an explicit `(not= 3 (:repetitions policy))` check, and
  `(range 1 4)` again in `derive-aggregate`;
- `parser_rq_campaign.clj` campaign construction;
- `data/parser-rq-core-attempt-policy-v1.json`.

Because the schemas carry the constant, changing it rotates schema hashes, hence
the policy's `schema_hashes`, hence `policy_hash`, hence
`instrument_policy_hashes`, hence `qualification_identity_ref` — and it makes
existing captured evidence protocol-incompatible rather than merely stale.

### D2 — Corpus sensitivity is not confined to cardinality

An earlier formulation of this design claimed eight predicates were
"size-invariant." That is false and is corrected here.

Only the *thresholds* of eight predicates are syntactically independent of
corpus size. Their observed values and evidential strength are not:

- `fatal-failures ≤ 0`, `silent-drops ≤ 0`, `timeout-policy ≤ 0` become harder
  to satisfy as exposure grows.
- Ratios at exactly `1.0` likewise become harder as more works and constructs
  enter the denominator.
- `memory` is the **maximum across work records**
  (`parser_rq_resource.clj:31`, `(apply max …)`), not an intrinsically per-work
  invariant. A larger corpus is more likely to contain the work that sets the
  maximum. `acceptable?` additionally admits `"ceiling_clipped"`, so a work that
  saturates the 3 GiB cgroup ceiling is accepted and its clipped value enters
  the maximum — the predicate then fails while the true peak is unmeasurable.

The correct statement:

> Eight predicates do not mechanically accumulate with corpus cardinality;
> nevertheless all nine depend on corpus composition, and most become more
> discriminating as exposure grows. Total wall time alone confounds a
> performance property with aggregate workload size by construction.

### D3 — "Deterministic" is conflated with "repo-tracked"

The corpus file requires members be "small, deterministic, workspace-tracked
… (no machine-local /db paths)." The genuine requirement is the parenthetical.
A pinned flake input is content-addressed exactly as strongly — rev plus
narHash — and `source-bundle-corpus` already proves the full corpus is
deterministically reachable in a Nix sandbox. The size ceiling follows from the
conflation, not from reproducibility.

Removing it does **not** remove the corpus-design decision.
`governed-qualification-corpus-location` binds `:source_path` as a
corpus-entry identity key, so membership still determines which expectations are
exercised, which extremes are included, what can falsify qualification,
evidence volume, identity hashes, and repeatability under upstream drift.

### D4 — Evidence asymmetry, not simple scope inversion

Full-corpus parser comparison and release qualification prove different
propositions: the former establishes relative behaviour such as parse
completion, the latter exact owned-contract predicates. Full-corpus evidence
for one does not satisfy the other. The precise defect is:

> Publication authority depends on an exact but extremely narrow owned-contract
> observation, while broader development decisions depend on much wider but
> semantically weaker observations. The evidence is complementary, yet no policy
> explains why the narrow observation is sufficient for publication.

### D5 — Wall time confounds three quantities, and is not redundant

`wall-time` measures the maximum over three complete serial `ab-check`
campaigns (`parser_rq_core_attempt.clj:206`, `derive-aggregate`). It confounds:

1. aggregate workload size (cardinality);
2. parser cost against harness overhead, at a measured **26 / 74** split;
3. a **governed serialization** whose cost may not suit tier 2.

On the third: a 26× parallel speedup shows concurrency changes campaign duration.
It does **not** show serial execution is accidental. `--jobs 1` and `serial: true`
control interference, stabilise measurement, and are what make per-work resource
attribution possible at all. Parallelism also changes peak aggregate memory and
host contention, so it cannot be substituted without rotating the protocol. The
finding is that one number is being asked to serve three purposes, which should be
separated into reproducible performance qualification, operational campaign
throughput, and per-work resource isolation.

It is **not** redundant with `timeout-policy`. A per-work timeout proves no
single work exceeded 60 s; it does not constrain aggregate throughput. A parser
taking 59 s per work over 17,880 works yields `timeouts = 0` and a campaign of
≈12.2 days. Campaign duration is a real operational property and must not be
retired by accident.

### D6 — Representational loss can coexist with complete span coverage

**Scope of this trace: `UNSUPPORTED` and `STRUCTURAL` emission sites only.**
`LOSS` and `AMBIGUITY` are untraced (Q3). The one previously unresolved
`UNSUPPORTED` arm is now measured — see *Q2, closed* — and its premise was
wrong. An earlier draft of this section claimed an exhaustive trace
and concluded globally that the audit categories "predict no predicate failure."
That claim exceeded its evidence and is withdrawn; the narrower finding below is
what the trace supports.

All six `UNSUPPORTED` emission sites in
`ab-validator/crates/ab-aat-to-parser-ir/src/convert.rs`:

| Site | Behaviour | Coverage effect |
|---|---|---|
| block container (631) | records the lost boundary, recurses `map_block` over children | covered |
| `caption` (1188) | records, then `visible_content_text`, `end = offset + utf8_len(&text)` | covered |
| synthetic-span site (1457) | emits `"span": synthetic_span(offset, end)` | covered |
| `map_raw_to_nodes` (1599) | `PageBreak` / `SourceNote` push nodes; `ParserResidue` emits none | covered / covered / **emits no node over real source bytes — see Q2, closed** |
| `map_warigaki_to_nodes` (1793) | `map_inline_content` for upper and lower | covered |
| `warigaki` in visible-content projection (1958) | records, then `append_visible_content_text` for upper and lower into `out` | **different output** — appends to the visible-text projection, not to `nodes` |

Site 1958 is worth separating: it writes the visible-content projection rather
than the node list, so its relationship to span coverage is different in kind
from the other five. Lines 2031/2034 are a `has_rule` query in
`warigaki_target`, not an emission.

#### D6 correction — this section named the wrong instrument

An earlier draft of this section asserted:

> `source_span_coverage` is computed over parser-IR node spans
> (`CoverageBasis { NodeSpans => "parser_ir.nodes[*].span" }`, consuming
> `input.parser_ir_bytes`).

**That is false, and it was the premise of the whole section.** It describes
`ab-parser-rq-source-accountability/src/analyze.rs`, which is a different
analyzer in the same crate. The predicate declares instrument
**`parser-rq-source-recognition-v1`**
(`data/parser-release-qualification-predicates.edn`), which is
`recognition.rs`: it builds `recognized` from **source-recognition ledger
entries**, each validated against the decoded source itself
(`end > decoded.len()`, `decoded.is_char_boundary(start)`), and partitions on
each entry's `disposition` — `preserved_opaque` entries land in `accounted`
but not in `recognized`.

`parser_release_qualification.clj:190` states the supersession outright:

```clojure
(defn install-source-recognition-observation
  "Install ledger-authoritative R1 evidence while retaining the former
  Parser-IR node-span observation under its explicit supporting name."
  [measurements recognition-envelope]
  (cond-> (assoc measurements :source_span_coverage recognition-envelope)
    (contains? measurements :source_span_coverage)
    (assoc :parser_ir_node_span_coverage (:source_span_coverage measurements))))
```

So the node-span observation is **demoted to `:parser_ir_node_span_coverage`**,
retained as supporting evidence, and the predicate is bound to the ledger
envelope. The accountability analyzer's `NodeSpans` basis is not the instrument
of any of the nine predicates.

What survives the correction, and what does not:

- **Survives:** the trace of the six `UNSUPPORTED` emission sites, and the
  finding that representational loss can coexist with complete coverage. The
  ledger basis makes that *easier* to satisfy, not harder — a `preserved_opaque`
  disposition accounts bytes without recognizing them, and a recognized interval
  says nothing about whether the node covering it says the right thing.
- **Does not survive:** the stated *mechanism*. "`source_span_coverage = 1.0`
  can be satisfied by a text node standing in for a lost `warigaki`, because the
  bytes are covered" describes node-span arithmetic that no longer computes the
  predicate. The claim must be re-derived over ledger dispositions, and it has
  not been.
- **Unaffected:** the conclusion that the nine predicates do not imply fidelity,
  and the case for a tenth predicate. That argument never depended on which
  coverage basis is authoritative.

**Measured aside — the retained supporting observation is not a coverage
ratio.** Parser-IR node spans do not address the decoded-source coordinate on
real works. Their extents are the byte lengths of each node's own *emitted*
text, accumulated as a running offset, while carrying
`"coordinate_system": "decoded_utf8"`. On `cards/000005/files/53194_ruby_44732.zip`
(`hatsukoi.txt`, 259,320 decoded bytes) the 8,137 nodes run contiguously to
224,010 and the source at any given node's offset is unrelated text — node 8000
spans 135 bytes, exactly the UTF-8 length of its own text. Reconstructing
`analyze.rs`'s union over that gives 0.865, and over the three corpus works
0.58 / 0.43 / 0.33. Method limit: this reconstructs the analyzer in Python from
`ab-aozora --mode aat | ab-aat-to-parser-ir convert` (the production path, per
`soranoha_build_publication.clj:241`); the built instrument was not run, and
**no ledger-basis coverage was measured here**, so nothing in this aside
predicts whether a tier 2 campaign passes.

The governed ignored-regions taxonomy `data/parser-rq-ignored-regions-v1.json`
is `{"rules":[]}` — nothing is exempt. `silent-drops` partitions the resulting
source gaps into diagnosed and silent.

The defensible finding, stated at the strength the trace supports:

> The inspected unsupported paths demonstrate that representational loss **can
> coexist with complete span coverage**. Therefore the nine predicates do not
> imply fidelity. Whether *all* current divergence paths preserve coverage
> remains open under Q3, and the *mechanism* by which loss and coverage coexist
> must be re-derived over ledger dispositions (Q12) rather than node spans.

So `rules_emitted: 54` of `690` and `unsupported_occurrences: 195,437` document a
major known fidelity exposure in a dimension the predicate set cannot detect.
They do **not** license waiving full-corpus uncertainty: this section must not be
cited to conclude that a full-snapshot campaign will pass.

`source_span_coverage = 1.0` **can be** satisfied while a `warigaki` is lost,
because recognition and fidelity are different propositions — but per the *D6
correction* the node-span mechanism previously offered for this is not the one
the predicate computes, and the ledger-basis mechanism has not been derived. No
captured `warigaki` witness has been produced showing the predicate actually
returning `1.0` over such a work. Attaching one — a fixture or a real capture —
would turn this from a conjecture into a demonstration, and is now the *only*
route to the claim, since the mechanism argument has been withdrawn. The nine
predicates measure coverage,
validity, diagnosis, and resources — not whether the node covering a byte says
the right thing about it.

### Q2, closed — the residue arm drops real source bytes

Q2 asked whether any `ParserResidue` occurrence carries an eligible source span,
reasoning that selection on `x-provenance == "parser-derived"` *should* mean it
does not. **Measured, that premise is false.**

Over a deterministic 300-archive stride sample of the pinned `0e9ea3e` checkout
(299 measured, 1 unreadable by stock `zipfile`):

| Quantity | Value |
|---|---|
| `ParserResidue` occurrences | **2,421** |
| — via the `x-provenance == "parser-derived"` arm | 2,421 (**100%**) |
| — via the `is_parser_raw_residue` string heuristic | **0** |
| Occurrences carrying a `span` | 2,421 (**100%**) |
| Occurrences with a **non-zero** span extent | 2,421 (**100%**) |
| Total residue source bytes | **61,002** |

Span extent equals source-text length for every occurrence. The witnesses are
unambiguously real source, not parser artefacts:
`［＃ページの左右中央］`, `［＃４字下げ］`, `［＃改丁］`, `［＃割り注］`,
`［＃挿絵１（fig55882_01.png、横657×縦776）入る］`, and
`≪愛さでやまぬ胸なれば≫` — the last being body text, not an annotation.

So `x-provenance: "parser-derived"` marks *how the AAT emitter derived the
node*, *not* the absence of a source extent, and `map_raw_to_nodes`
(`convert.rs:1639`) emits no parser-IR node for any of them.

Two corrections follow, and neither is the one Q2 anticipated:

1. **The exposure is the provenance arm, not the heuristic.** Q4 flags
   `is_parser_raw_residue` as a string-inspection heuristic that could change
   coverage without a policy hash moving. On this sample **it never fired
   once** — every classification came from the provenance test. Q4 remains a
   real latent hazard; it is not the operative one.
2. **This does not establish a coverage hole.** Per the *D6 correction*,
   `source_span_coverage` is computed from the recognition ledger, not from
   parser-IR nodes, so "no node emitted" does not by itself mean "byte
   unrecognized" — it depends on the ledger entry and its disposition, which
   was not measured here. The dropped bytes are also far too few to explain the
   node-span deficit: on `hatsukoi.txt` residue accounts for **249 bytes**
   against a 34,570-byte shortfall, which is markup overhead, not residue.

What Q2 now leaves open is narrower and better posed: **do the residue bytes
appear in the recognition ledger, and with what disposition?** That is a ledger
question, answerable without a full campaign.

Method limit: this replicates `raw_recovery_class` / `is_parser_raw_residue` in
Python over `ab-aozora --mode aat` output, and approximates admission the way
`tools/corpus_inventory.py` does. It is an exploratory observation on 299 works,
not a census, and not durable evidence — the same status the *Deterministic Tail
Set* section assigns its own figures.

### Q12, closed — the residue is mostly recognized, and coverage fails anyway

Q12 asked whether the dropped residue bytes reach the recognition ledger. They
mostly do. Answering it also measured the predicate itself, and **that** is the
consequential result.

First the formula, since D6 previously got the instrument wrong. Coverage is

```clojure
;; parser_rq_source_accountability.clj:704
(exact-display-ratio (:recognized_bytes aggregate) (:eligible_bytes aggregate))
```

and `recognition.rs:461` routes every `preserved_opaque` entry to `accounted`
but **not** to `recognized`. So `source_span_coverage = recognized / eligible`,
and there are two distinct ways to lose: an entry dispositioned
`preserved_opaque`, or a byte with **no ledger entry at all**.

#### The residue question, answered

| Residue intervals (299 works) | Count | Bytes |
|---|---|---|
| Overlapping only **recognized** entries | **2,154** | 44,638 |
| Overlapping only `preserved_opaque` entries | 47 | 5,864 |
| **No ledger entry at all** | **220** | 10,500 |

The recognized-side constructs are `kaeriten` (1,164), `indent` (206),
`container_close`/`container_open` (196/192), `page_break` (136), `heading_hint`
and `margin_note` (51 each), `section_break` (46), `warichu_open` (35),
`illustration` (31), `align_end` (23), `center` (15), `body_end` (6).

**This is D6's conclusion, finally demonstrated on the right instrument.** The
ledger recognizes constructs the parser-IR converter emits no node for — so
representational loss genuinely does coexist with recognition, and the nine
predicates cannot detect it. The mechanism withdrawn in the *D6 correction* is
hereby replaced by a measured one. The 220 entry-less intervals are a separate,
smaller exposure.

#### The predicate, measured

| Quantity | Value |
|---|---|
| Works measured | 299 (stride sample, 1 unreadable by stock `zipfile`) |
| **Works reaching `source_span_coverage = 1.0`** | **0** |
| Works carrying unaccounted bytes | **299** |
| Corpus fold, `recognized / eligible` | **0.9640** |
| Worst single work | **0.2466** |
| Eligible / recognized / accounted bytes | 13,068,347 / 12,597,285 / 12,727,595 |
| Semantic gap (`eligible − recognized`) | **471,062** |
| — of which `preserved_opaque` | 130,310 (28%) |
| — of which **unaccounted, no entry** | **340,752 (72%)** |

The `preserved_opaque` share is `recovered_verbatim` (41,506 occurrences /
125,382 bytes) and `unknown_directive` (47 / 5,864 bytes) — both under role
`unrecognized_source_form`.

**Confirmed through the built instrument.** The figures above were first
reconstructed in Python; they have since been reproduced by running
`ab-parser-rq-source-accountability capture-corpus` over the same 299 works,
with the ledger, recognition records, and both aggregates produced by the binary
itself. The two agree **to the byte on every quantity**:

| | Instrument | Python | Δ |
|---|---|---|---|
| `eligible_bytes` | 13,068,347 | 13,068,347 | **0** |
| `recognized_bytes` | 12,597,285 | 12,597,285 | **0** |
| `accounted_bytes` | 12,727,595 | 12,727,595 | **0** |
| `semantic_gap_bytes` | 471,062 | 471,062 | **0** |
| `unaccounted_bytes` | 340,752 | 340,752 | **0** |
| `recognized / eligible` | **0.9639539721435313** | 0.9639539721435313 | **0** |

The aggregate carries `"status": "ok"` and an empty `errors` list — this is a
valid measurement, not an error path. `accounted / eligible` is **0.9739253939**.

**Control, same binary.** Over the three governed corpus works the instrument
returns `eligible_bytes` **177**, `recognized_bytes` **177**,
`semantic_gap_bytes` **0**, `unaccounted_bytes` **0** — coverage exactly
**1.0** — which is why the predicate passes today. One binary, one identity, one
taxonomy, one policy hash: **1.0 on 177 governed bytes, 0.9640 on 13 MB of real
source.** The corpus is not merely small; it is the only population on which this
predicate has ever been satisfiable.

#### What this means for the design

`source-span-coverage` is declared `{:comparator := :value 1.0}` — exact. On this
evidence **a tier 2 campaign at snapshot scale fails that predicate on every
work**, by a wide margin, and not because of anything the parser did wrong: 72%
of the gap is source the ledger never claims to describe.

Three readings, and the design does not choose between them here:

1. The **classified-source policy is incomplete** — 31 rules do not cover
   Aozora's construct inventory, so real works accumulate unclassified bytes.
   Then the fix is policy work, and it rotates `policy_hash`.
2. The **threshold is wrong for a real population** — `:= 1.0` was predeclared
   against a 177-byte corpus where it is trivially satisfiable. Then the
   confirmatory threshold must be set from the exploratory campaign, exactly as
   *Governance Path* step 5 anticipates.
3. **`preserved_opaque` is being counted wrongly** — if the intent is "the parser
   accounted for this byte", the ratio arguably wants `accounted / eligible`
   (0.9740 here), not `recognized / eligible`. That is a predicate-semantics
   question of the same kind as D7.

All three are live. What is now settled is that **the current predicate and the
proposed tier 2 population are incompatible**, and that this was invisible at
177 bytes. This is the sharpest instance of D4's evidence asymmetry in the whole
document: publication authority rests on a predicate that has never been
observed against a population capable of falsifying it.

Method limits, now narrower. The instrument objection is discharged. What
remains:

- **Sample, not census.** One deterministic 300-archive stride sample (299
  measured), taking the name-sorted first `.txt` member via stock `zipfile`
  rather than ABC's semantic primary-text selection. A different member
  selection could move individual archives, though it cannot plausibly move a
  471 KB aggregate gap to zero.
- **Synthesized qualification identity.** The identity was built to mirror the
  real parser-IR `derived_from` so per-work records return `ok` rather than
  identity-mismatched. It is not a campaign identity, and the
  `qualification_identity_ref` these runs carry is meaningless outside them.
  This measures coverage; it authenticates nothing.
- **Still a disposable observation.** Per the *Deterministic Tail Set* rule,
  nothing here is durable evidence until it carries snapshot identity and an
  expected digest.

Reproduction needs a 15-line `examples/dump_ledger.rs` in `ab-aozora-capture`
piping stdin through `classified_source_ledger_from_bytes`, plus
`capture-corpus` over an extracted source tree and its converted parser-IR. The
example was **deliberately not committed** — a permanent example in the
release-gated capture crate is governed surface for a one-off probe, and this
document's own rule is that these tools must not accrete into governance inputs.

### D6 and D4

This bears directly on D4. A green full-corpus report must not be read as "the
parser is faithful." Closing it requires a **tenth predicate on representational
fidelity**, instrumented from the divergence ledger. That is a predicate-set
question, not a corpus question, and it does not block tiering.

## Proposed Tiers

| Tier | Population | Purpose | Cadence |
|---|---|---|---|
| **1. Fixture / conformance** | Small synthetic works, expected-output-bearing, adversarial, including malformed inputs and exact diagnostic contracts | Feature interactions and negative cases that natural data may never exercise | Every PR |
| **2. Publication-workload snapshot** | Publication's successfully-selected source projection, carried with a declared denominator (`candidates_considered`, `derive_failures`, `rejected`) — **bound by Q11; equality proof pending Q9** | Exhaustive exposure of owned-contract predicates | Release / high-risk change |
| **3. Stress / resource envelope** | Governed extremes selected by rank on notation properties | Resource envelope and pathological-input behaviour | Release, plus on parser change |

Tier 2 cannot replace tier 1: naturally occurring data need not contain
malformed input, exact diagnostic expectations, or isolated feature
interactions. Tier 1 cannot replace tier 2: 23 hand-picked works do not
establish exposure.

### What tier 2 claims

Tier 2 is **intended** to census the pinned publication-source population.

**The required equality is with publication's selected-source projection alone.**
An earlier draft asked for "exact equality with the admitted and
publication-selected populations," which is the wrong acceptance condition: the
count spread is *evidence* that admission, conversion audit, and publication
selection are three different populations, and there is no reason publication
selection should equal source-bundle admission. Demanding it would either be
unsatisfiable or would push tier 2 back to every admitted archive purely to
manufacture the equality.

Three populations, three different obligations. **Q11 is now decided**, so the
relation below is settled:

| Population | Relation to tier 2 | Obligation |
|---|---|---|
| Source-bundle admission universe | superset | **accounted set difference** — every excluded archive explained |
| **Publication-selected projection** | **identical** | exact set equality — authenticate that tier 2 was projected from `source_selection_hash`, not re-derived |
| Conversion-audit population | overlapping | **accounted set difference** — reconciled, not equalised |

Tier 2's population is *derived from publication source selection* rather than
from an independent walk of the checkout, and its relation to every other
population is authenticated rather than assumed.

**Set equality needs a common identity coordinate before it means anything.** The
three populations are keyed differently today — the source-bundle census by
archive and member, publication selection by work ID, person ID, archive relpath,
primary member and hashes, the conversion audit by converted file entry. Counts
establish neither equality nor accounted difference. Q9 must therefore predeclare
a canonical join key — at minimum archive relpath plus primary-text member, with
source hashes where available — together with normalization rules, treatment of
duplicate and card-person cases, one-to-many conversion entries, missing keys, and
collision rejection. Only then can the reconciliation authenticate equality or
emit typed difference classes.

An earlier draft asserted that "corpus and workload are the same object at the
same rev." That is not established by the cited code. `official-source`
(`soranoha_build_publication.clj:410`) records identity for each *selected* work,
and the Git check refuses release when "official source Git state is unprovable"
(line 728) — together these prove **shared source state at a shared revision, not
equal membership**. `materialize-selected-sources!` (line 583) additionally:

- keeps only candidates with a resolvable relpath;
- requires a catalog row matched by **basename** (`rows-by-basename`);
- filters through `select-candidate`, splitting results into `selected` (`:ok`)
  and `derive-failures` (`:failed`);
- may skip archive admission failures under `continue_on_failure`;
- reports `selected`, `rejected`, and derivation-failed populations separately.

The three-way count spread is direct evidence the populations differ: this
design's exploratory inventory finds **17,878**, source-bundle admission reports
**17,880**, and the conversion audit reports **17,886**.

Therefore the tier 2 population must be **defined from the same governed
selection projection publication uses** — ideally its content-addressed
source-selection identity — with set equality proven, not from an independent
walk of the checkout. Q9 is prerequisite evidence for this claim.

**Open: which selection projection.** `continue_on_failure` can omit derivation
failures from `selected`, so binding tier 2 to the resulting set lets a
*source-selection* regression shrink the qualification denominator while every
ratio predicate stays at `1.0`.

**That shrinkage is local to the qualification report and does not confer
publication authority.** Selection failures enter the snapshot-index failure set,
and `publication-release/failure-problems` treats any non-empty failure set as
inadmissible — *"A release closes over a complete corpus: any recorded failure is
inadmissible"* — so a best-effort partial root is non-release-admissible and exits
nonzero. An earlier draft implied the denominator could move silently toward a
release; it cannot.

The real trade-off is narrower:

- **successful-selection binding** — exactly matches the workload that publishes;
  the qualification report itself can stay green on a shrunken corpus, with the
  *independent* release gate catching the partial corpus;
- **pre-attempt catalog-backed candidate population**, selection failures retained
  as qualification failures — makes qualification independently expose the same
  failure rather than relying on the downstream release boundary; qualifies works
  publication did not emit.

#### Q11, decided 2026-07-27 — successful selection, with a declared denominator

**Tier 2 binds to publication's successfully-selected projection**, and the tier
is named the **publication-workload snapshot**. This was decided on
constructibility, not preference: the pre-attempt population *cannot supply
corpus-entry identity for exactly the members that distinguish it.*

A corpus entry's identity is `corpus-entry-identity-keys`
(`parser_release_qualification.clj:40`) — `:work_id :source_path :source_sha256
:category :expected_status :expected_diagnostics` — and `corpus-snapshot-hash`
is the sha256 over the sorted `:source_sha256` of **every** entry. Now compare
what each candidate population actually carries:

| | Pre-attempt candidate | Successfully selected |
|---|---|---|
| Shape | `{:file :relpath :row}` (`materialize-selected-sources!`, line 583) | full identity row (`inspect-selected-work!`, line 439) |
| `:source_sha256` | **absent** | `archive_hash` / `primary_text_hash` |
| Primary-text member | **absent** | `primary_text_member` |
| Content-addressed identity | **none** | `source-selection-hash` over `source-selection-identity-object` |

Every one of those missing fields is produced by `source-bundle/inspect-zip`
*inside* `inspect-selected-work!` — which is precisely the fallible step
`select-candidate` wraps in `try`/`catch`. So a derivation-failed member has no
source hash **because it failed**, and it cannot be hashed into
`corpus-snapshot-hash` without inventing an absent-hash representation that
would weaken the pinning for every other entry. The same gap defeats Q9: the
canonical join key that section demands is "archive relpath plus primary-text
member", and the pre-attempt population has the relpath but not the member.

The successful projection, by contrast, is *already* content-addressed exactly
as this design asked for — `source-selection-identity-object` is built from
`selected` alone, from "official Git/catalog/archive/bundle/primary-text/
metadata facts only — never from parser output", and hashed to the
`corpus_snapshot_hash` the source and publication manifests share.

**The shrinkage objection is answered by attestation, not by the release gate.**
Binding to `selected` alone would leave the qualification report green on a
corpus a `continue_on_failure` regression had quietly shrunk, with only the
independent `publication-release/failure-problems` gate to notice. Rather than
accept that coupling, tier 2's report must carry its own denominator:

- `candidates_considered` — `(count selected-candidates)`, the catalog-backed
  pre-attempt count;
- the `derive_failures` set and `derive_failed_count`;
- the `rejected` set with its reasons.

All three are already computed and emitted by `materialize-selected-sources!`,
so this is a **projection of data publication already produces**, not new
machinery — which is the second reason this option is the simpler one. A
non-empty failure set makes the qualification report self-describing about its
own denominator, independently of the downstream gate.

Minor observation, recorded rather than acted on: `rejected`'s `:else
"not-selected"` reason appears unreachable — a candidate is admitted to
`selected-candidates` iff it has a non-nil relpath *and* a `rows-by-basename`
row, so any removed candidate satisfies one of the two preceding branches. It is
harmless, but a reason string that can never be emitted should not be read as
evidence that a "selected but not chosen" class exists.

**Consequences for Q9.** The equality target is now fixed: exact set equality
between tier 2 and publication's selected projection — which is near-tautological
by construction, since tier 2 *is* that projection, and reduces to authenticating
that the corpus was projected from `source_selection_hash` rather than
re-derived. The substantive Q9 work is unchanged: the **accounted set
difference** against the source-bundle admission universe and the
conversion-audit population, which is where the 17,878 / 17,880 / 17,886 spread
has to be explained. Q9 remains open.

Once proven, the claim would be **a census of one snapshot** coinciding with **an
operational claim about the publication workload**. It is in no case a statistical
claim about future Aozora works, nor a language-conformance claim. The census
expires when `aozorabunko-src` is bumped; `upstream-ingest-drift-awareness`
governs that.

## Predicate-to-Tier Assignment

Predicates are assigned to the tier whose population can actually falsify them,
rather than all being observed over one corpus.

| Predicate | Tier(s) | Rationale |
|---|---|---|
| `fatal-failures` | 1 + 2 | Zero-tolerance; exposure is the point |
| `source-span-coverage` | 1 + 2 | Strictest predicate; every **eligible decoded-source byte** must fall under a parser-IR node span. The instrument works in the `decoded_utf8` coordinate system over `eligible_bytes` — after decoding and after any governed ignored-region treatment — not over raw archive bytes or original-encoding bytes |
| `silent-drops` | 1 + 2 | Gap partition needs real gaps; tier 1 supplies contrived ones |
| `diagnostic-completeness` | 1 primarily | Requires *expected* diagnostics, which only authored fixtures carry; currently vacuous at 3/3 against empty expectations |
| `parser-ir-schema-validation` | 1 + 2 | Size-independent threshold, composition-sensitive value |
| `publication-structure` | 1 + 2 | Same |
| `timeout-policy` | every attempted work, all tiers | Per-work property; 45× headroom measured |
| `memory` | **2 authoritative**, 3 for regression | See below — a rank-selected set cannot be assumed to contain the true maximum |
| **wall-time** | see reshape below | Currently confounded |

`max_line` is reassigned from tier 3 to tier 1: its extremes are the *fastest*
works measured (0.004–0.077 s; a 28,245-character line parses in 33 ms), so it
stresses correctness and buffer handling, not the resource envelope.

**Memory must stay authoritative at tier 2 while the full run is affordable.**
The rank selection is a *hypothesis* about which notation properties drive peak
memory, and the four-point model (≈ 30 MB + ~250× source) does not prove its axes
contain the actual maximum — an unmodelled construct or interaction could peak
elsewhere. So tier 2 establishes the snapshot's true maximum and tier 3 supplies
concentrated regression and future-envelope evidence.

Falsifier for the selector: if a tier 2 run reports a peak memory higher than any
archive in the tier 3 resource projection, the selection axes are incomplete and
must be revised. That comparison should be recorded on every full-census run,
which is also what keeps the selector honest as the snapshot moves.

## Wall-Time Reshape

Split the one confounded predicate into two honest ones:

1. **Parser throughput** — bytes/second or per-work seconds, under a declared
   **governed host class**, isolating the parser from harness overhead. Given
   the 26/74 split, "throughput" must state *which* throughput; the two differ
   by roughly 4×.
2. **Campaign duration budget, per tier** — so `300 s` becomes explicitly "the
   fixture tier's budget" and tier 2 carries its own declared number. The
   corpus-dependence becomes explicit instead of implicit.

`repetitions` is likewise tier-relative. Three passes exist for measurement
stability on 3 works; at 17,878 works aggregate timing is already stable and
three passes cost 3× (1,348 s against 449 s) for little gain.

**Q1 changes the budget arithmetic.** Both numbers above are `ab-check` alone,
and the capture layer above it costs ~78–85 min for a single pass — roughly 3.5×
the whole ×3 `ab-check` campaign. A tier 2 campaign-duration budget derived from
`ab-check` extrapolations would be wrong by an order of magnitude. Whichever
number tier 2 declares, it must be measured over the layer that actually
dominates, and the declared **governed host class** (Q5) matters correspondingly
more.

## Deterministic Tail Set

Selection is by **rank on measured notation properties**, not sampling: the same
inventory yields the same works every time, with ties broken on archive path.

Metrics are **volumes, not densities** — raw occurrence counts with no
normalization by length — because total work is what the resource envelope
responds to. A per-byte density would select short, dense works, a different
question selecting a different tail. Two projections are emitted rather than one
union, so an archive selected for a correctness reason cannot be read as
resource-envelope evidence:

| Metric | Projection | Failure mode targeted |
|---|---|---|
| `bytes` | resource | Parse throughput and peak memory |
| `ruby` | resource | Inline span emission volume |
| `lines` | resource | Per-line overhead accumulation |
| `max_line` | correctness | Line-oriented buffer behaviour |
| `gaiji` | correctness | External-character resolution |
| `annot_non_gaiji` | correctness | Editor-note handling |

At top-5 the projections are **12 resource archives (14.3 MB)** and **15
correctness archives (10.3 MB)**, overlapping in 2. Because only 5 of 30 slots
overlap, **a "largest N archives" selection is not a substitute** —
`cards/000168/files/909_ruby_518.zip` is 57 KB and carries the
28,245-character line.

The four archives stock `zipfile` cannot open are emitted as
`stock_zipfile_skipped_archives`, deliberately **not** as a
"deliberately-damaged" or "recovery" category. That observation means only "stock
`zipfile` could not open this." It does not establish that ABC admits the archive
through recovery, that the archive is in publication selection, or that all four
share one failure class — and the source-bundle summary already distinguishes
damaged paths, declared/actual size mismatch, admitted, and rejected-unreadable.
Only the authoritative projection may classify an archive as recovered,
rejected, or tier-eligible.

Every gaiji marker `※［＃` contains the annotation opener `［＃`, so a bare opener
count double-counts gaiji. Both are emitted (`annot_markers`,
`annot_non_gaiji`); the selector ranks on `annot_non_gaiji`. Measured, gaiji is
**8.9%** of raw openers and excluding them does not change the top-5 ranking on
this snapshot — but the metrics differ and could select differently on another.

Reproduced by `tools/corpus_inventory.py` and `tools/corpus_tail_set.py`, both
labelled non-authoritative.

The `corpus-exploratory-tool-tests` flake check protects **ordering, metric
definitions, and projection separation** against synthetic archives. It does
**not** regenerate or pin any real-snapshot measurement. Specifically unverified
by any check: the 17,878 archive count, 568,169,767 bytes, the five maxima, the
12/15 projection membership, the 14.3/10.3 MB totals, and the two-archive
overlap. Those remain **disposable observations** recorded in this design, not
durable evidence.

If any of those figures is to become durable, it needs either a generated
measurement record carrying snapshot identity and an expected digest — the shape
`data/source-bundle/aozorabunko-0e9ea3e-summary.json` already uses — or a
separate real-input evidence check. Neither exists.

**These tools must not become governance tools.** The inventory takes the
name-sorted first `.txt` member via stock `zipfile`, whereas ABC admission decodes
legacy entry names, applies semantic primary-text selection, rejects zero or
multiple primary members, enforces archive limits and collision rules, and
recovers trailing-garbage archives. Governed tier 2 and tier 3 membership must be
projected from the authoritative source-bundle census and publication's own
source-selection projection. Growing a second approximation of one population
into a governance input is the drift this repository binds hashes to prevent.

## Governance Path

Corpus and predicate changes rotate identity, so ordering matters.

### What each open item blocks

Stated explicitly, because "blocker" without a scope stalls everything equally.

| Item | Blocks | Does **not** block |
|---|---|---|
| **Q3** (LOSS/AMBIGUITY untraced) | **Predicting** outcomes from D6; claiming all divergence paths preserve coverage; inferring fidelity from a green report; using D6 as a substitute for measurement | Accepting the architecture; running the exploratory campaign; **reporting directly measured predicate observations from either campaign**, exploratory or confirmatory |
| **D7 prerequisites** (decided; unimplemented) | The fixture tier containing adversarial or malformed inputs, until `allowed_dispositions` is untangled and the expected-outcome model specified | The tier architecture; tier 2 and tier 3 work; the `wall-time` reshape |
| **Q9** (population equality) | Describing tier 2 as a census; the accounted difference against the admission and conversion-audit populations | Accepting that a snapshot tier should exist; tier 2's population definition and name, both now fixed by Q11 |
| **D8** (repetition protocol) | Tier-relative repetition counts | Everything else; tier 2 can run at the current fixed 3 |
| **Q14** (coverage predicate vs tier 2) | **The confirmatory tier 2 campaign** — it cannot pass `source-span-coverage := 1.0` as predicated; and reading any tier 2 coverage observation as a parser verdict | Accepting the architecture; tier 1 and tier 3; the exploratory campaign, whose *purpose* is to measure exactly this |
| ~~**Q2**~~ (closed 2026-07-27) | — | — · closing it *corrected* D6's instrument attribution; see *D6 correction* |
| ~~**Q12**~~ (closed 2026-07-27) | — | — · it demonstrated D6's conclusion on the right instrument, and raised Q14 |
| ~~**Q11**~~ (decided 2026-07-27) | — | — |

So the three-tier **architecture** is acceptable now. What is not yet available
is a confirmatory qualification at tier 2, an adversarial tier 1, or a census
claim.

**The exploratory campaign does not by itself close Q3 or Q12.** A green campaign
does not show that the residue bytes are ledger-recognized, nor that every
`LOSS`/`AMBIGUITY` path preserves coverage — measured outcomes are not the same
proposition as the mechanism that produced them. Closing Q12 needs the ledger
entries over the residue intervals inspected; closing Q3 needs class-specific
ledger and code-path analysis. The campaign's role is to **supply the evidence
population and witnesses** for those dedicated probes.

Q2 is the worked example of why that distinction matters. It was closed by
direct measurement of 299 works, not by a campaign — and it did not merely
answer its own question, it falsified its premise *and* surfaced that D6 named
the wrong instrument. A dedicated probe found in one pass what a green
full-snapshot report would have concealed.

Current blocker status, plainly:

- **B1 — D6.** **Q2 and Q12 closed; Q3 still open.** Q2's closure found that this
  section named the wrong instrument: `source_span_coverage` is
  ledger-authoritative (`parser-rq-source-recognition-v1`), not node-span based.
  Q12 then re-derived the section's mechanism over ledger dispositions and
  **confirmed its conclusion** — 2,154 residue intervals are recognized by the
  ledger while the converter emits no node for them. Substantially resolved.
- **B5 — Q14. NEW, and the most serious open item.** `source-span-coverage :=
  1.0` fails on 299 of 299 real works — **confirmed through the built
  instrument**, byte-identical to the reconstruction, against exactly 1.0 on the
  governed corpus from the same binary. Until it is resolved, tier 2 has a
  population it cannot qualify against, and *step 5 of the governance path cannot
  complete*. It does not block accepting the architecture or running the
  exploratory campaign. What it needs now is a decision, not more measurement.
- **B2 — D7.** **Decided** (Option 1, `unexpected-fatal-failures ≤ 0`).
  **Unimplemented:** `allowed_dispositions` is still level-confused and the
  expected-outcome model is unspecified, so the adversarial fixture tier remains
  blocked on the work, not on the decision.
- **B3 — Q9.** **Unproven**, but its prerequisite Q11 is decided, so the equality
  target is now fixed: set equality with the selected projection (tautological by
  construction, reducing to authenticating the projection), plus the substantive
  accounted difference against the admission and conversion-audit populations. No
  canonical join key is predeclared yet.
- **B4 — D8.** **Open.** Tier-relative repetitions are still proposed in the
  wall-time reshape, so this is in scope, not incidental. If the migration is not
  wanted, the accepted architecture must instead **explicitly retain three fixed
  repetitions at every tier** — which is a legitimate choice, since tier 2 can
  run at the current constant.

Then:

1. **Settle Q14 first — it now gates step 5.** The measurement is confirmed
   through the built instrument, so what is left is the choice among policy
   completion, threshold predeclaration, and `preserved_opaque` semantics — a
   governance decision, not more measurement. Widening the sample toward a census
   would sharpen the number but cannot change its sign. **Q1 is measured** — the
   capture layer, not the parser, sets campaign cost — so what remains here is
   Q5's host class and the other open questions.
2. Discharge the D7 prerequisites in order — untangle `allowed_dispositions`
   first — and define tier semantics and authority **before** changing any hash.
3. Reshape `wall-time`, apply the D7 decision, carry the D8 protocol migration if
   tier-relative repetitions are kept, and, if adopted, add the fidelity
   predicate — **one identity rotation covering all four**. They rotate the
   identity by two different routes, and conflating them was an error in an
   earlier draft:

   - **`wall-time`, D7, and fidelity** change the predicate declarations, so they
     rotate **`predicate_set_hash`**.
   - **D8** changes core-attempt protocol schemas and policy, so it rotates the
     schema hashes, the core policy hash, and **`instrument_policy_hashes`**. It
     need not touch the predicate declaration or `predicate_set_hash` at all.

   Both routes terminate in `qualification_identity_ref`, so a combined final
   campaign is still one requalification, not four. D8 alone additionally makes
   prior captured evidence protocol-incompatible rather than merely stale.
4. Run a **non-authoritative exploratory campaign** at full-snapshot scale under
   its own predicate-set identity, and publish the observations.
5. Predeclare the confirmatory thresholds and rotate the qualification identity.

`custom-parser-release-qualification` claim `c2` requires thresholds "fixed
before measurement." That forbids presenting post-hoc thresholds as
preregistered confirmation; it does not forbid exploratory measurement. A
distinct exploratory campaign followed by a newly identified confirmatory
predicate set is methodologically honest, and the identity machinery makes the
distinction machine-checkable rather than a matter of trust.

Every governance edit to `decisions.edn` is authored by hand.

## Non-Goals

- Deciding whether the parser is faithful. D6 shows the current predicate set
  cannot answer that; this design records the gap rather than closing it.
- Changing published manifests or any existing snapshot identity.
- Retiring the three-work corpus. It becomes tier 1's seed, not its whole.
- Making corpus membership a non-decision. D3 explicitly rejects that framing.

## Open Questions

- **Q1 — CLOSED 2026-07-27, and the answer is large.** The capture layer costs
  **232.4 ms/work** (fixed 7,410 ms) against `ab-check`'s entire 25.12 ms/work —
  **9.25×** — with only 2.2% of it parsing and 4.2% the non-authoritative P1
  analysis. Cost is byte-proportional at ~5,400 ms/MB decoded, so one serial
  full-snapshot pass is **78–85 min**, ~3.5× the whole ×3 `ab-check` campaign.
  See *Q1, closed*.
- **Q2 — CLOSED 2026-07-27, premise falsified.** All 2,421 `ParserResidue`
  occurrences in a 299-work sample carry a real, non-zero source span (61,002
  bytes); `x-provenance == "parser-derived"` does **not** mean "no eligible
  source span". The string heuristic never fired. See *Q2, closed*. The
  successor question — whether those bytes appear in the recognition ledger and
  with what disposition — is tracked as **Q12**.
- **Q3 — LOSS and AMBIGUITY untraced.** D6 traced `UNSUPPORTED` and
  `STRUCTURAL`. The bulk of 4,938,800 divergences may be `LOSS`, whose coverage
  behaviour is unchecked.
- **Q4 — A heuristic sits in the conversion path.** `heuristic_enabled` is
  threaded through `map_block`, and `is_parser_raw_residue` classifies by string
  inspection. A heuristic that decides whether a node is emitted can change
  coverage without any policy hash moving.
- **Q5 — Host class undefined.** All figures are single-host. A throughput
  predicate requires a declared host class to be meaningful.
- **Q6 — Evidence volume at tier 2. NARROWED 2026-07-27, not closed.** Measured:
  **662 KB/work**, a **15.2× expansion over decoded source**, dominated by the
  AAT `parser_output` blob — **≈ 12 GB** at 17,878 works per capture generation.
  Retention across generations and closed-membership verification cost remain
  unmeasured. See *Q1, closed*.
- **Q7 — Fidelity predicate shape.** If adopted, what does it assert — rules
  emitted over rules total, divergence occurrences per node, or per-category
  ceilings? Unresolved.
- **Q8 — Tier 1 membership.** The v0 design
  (`docs/v0-design-bundle/ci-smoke-corpus.md`) specifies 9 buckets totalling 23
  works; 2 buckets and 3 works exist. Which adversarial cases are authored, and
  what their expected diagnostics are, is unspecified.
- **Q9 — Population reconciliation. PREREQUISITE for the tier 2 *census claim*,
  not a minor item.** Its prerequisite Q11 is decided, so the target is fixed.
  17,878 inventoried against `admitted_zip_count` 17,880 and audit
  `files_scanned` 17,886. Resolving it means proving **set equality with
  publication's selected projection only** — now near-tautological, reducing to
  authenticating that tier 2 was projected from `source_selection_hash` rather
  than re-derived — and producing an **accounted set difference** — not
  equality — against the source-bundle admission universe and
  the conversion-audit population, explaining `rejected`, `derive-failures`, and
  `continue_on_failure` skips. Requiring equality against admission would be the
  wrong condition and would push tier 2 back to every admitted archive purely to
  satisfy it. Until proven, tier 2 is "intended to census the publication-source
  population," not a census.
- **Q11 — DECIDED 2026-07-27.** Tier 2 binds to publication's
  successfully-selected projection and is named the **publication-workload
  snapshot**, with a declared denominator. Decided on constructibility: the
  pre-attempt population carries no `:source_sha256` and no primary-text member
  for exactly its distinguishing members, so it can supply neither
  `corpus-snapshot-hash` nor Q9's join key. See *Q11, decided*.
- **Q12 — CLOSED 2026-07-27.** The residue bytes are mostly ledger-recognized
  (2,154 of 2,421 intervals), which demonstrates D6's conclusion on the correct
  instrument; 220 intervals have no ledger entry. Closing it also measured the
  predicate: **0 of 299 real works reach `source_span_coverage = 1.0`** (fold
  0.9640, worst 0.2466), against exactly 1.0 on all three governed corpus works.
  See *Q12, closed*. The successor is **Q14**.
- **Q14 — `source-span-coverage := 1.0` and tier 2 are incompatible. Which
  gives?** Q12 shows the predicate fails on every real work, 72% of the gap being
  source with no ledger entry at all. Either the classified-source policy is
  incomplete (31 rules, rotates `policy_hash`), or the exact `1.0` threshold was
  an artefact of a 177-byte corpus and must be predeclared from the exploratory
  campaign, or `preserved_opaque` is mis-counted and the ratio wants
  `accounted / eligible` (0.9739) — a predicate-semantics question of D7's kind.
  **This now gates the confirmatory tier 2 campaign.** Confirmed through the
  built `ab-parser-rq-source-accountability` binary, agreeing with the Python
  reconstruction to the byte, with the governed corpus returning exactly 1.0 on
  the same binary — so the remaining uncertainty is the sample, not the method.
- **Q13 — Is `:parser_ir_node_span_coverage` safe to retain?** It is kept as
  supporting evidence beside the ledger-authoritative observation, but on real
  works it is not a source-coverage ratio at all: node spans are a running offset
  over emitted text carrying a `decoded_utf8` label (*D6 correction*, measured
  aside). A retained number that looks like a ratio, is named like a ratio, and
  measures something else invites exactly the misreading D6 warns against. Either
  re-derive what it means, rename it, or drop it.
- **Q10 — Fidelity across corpus growth.** If the fidelity predicate (Q7) is
  adopted, its observed value depends on corpus composition like the others, so
  its threshold cannot be set from the 3-work corpus either. It has to be
  predeclared from the exploratory full-snapshot campaign.
