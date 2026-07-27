# Parser-RQ Corpus Tiering Design

Date: 2026-07-26
Status: The three-tier **architecture** is proposed for acceptance. Five items
remain open, each blocking something specific rather than everything: **Q2/Q3**
(incomplete divergence trace) block *predicting* outcomes from D6, not measuring
them; **D7** (expected-outcome semantics, unresolved) blocks an adversarial
fixture tier; **D8** (repetition count is a schema constant) blocks tier-relative
repetitions; **Q11** (which selection projection binds tier 2) blocks that tier's
population definition and its name; **Q9** (population equality, unproven, and
prerequisite on Q11) blocks calling tier 2 a census. See *What each open item
blocks*.
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

Parse is **26%** of `ab-check`'s per-work cost; the remaining **74%** is zip
open/read, adapter spawn, AAT deserialize, checks, and report write.

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

**Status: UNRESOLVED.** Option 1 is the best design and is *not selected*. It is
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

Until those are specified and chosen, the fixture tier cannot contain the
adversarial cases this design promises. Sequence with the `wall-time` reshape so
one identity rotation covers both.

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
`LOSS` and `AMBIGUITY` are untraced (Q3), and one `UNSUPPORTED` arm is
unresolved (Q2). An earlier draft of this section claimed an exhaustive trace
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
| `map_raw_to_nodes` (1608) | `PageBreak` / `SourceNote` push nodes; `ParserResidue` emits none | covered / covered / **open, see Q2** |
| `map_warigaki_to_nodes` (1793) | `map_inline_content` for upper and lower | covered |
| `warigaki` in visible-content projection (1958) | records, then `append_visible_content_text` for upper and lower into `out` | **different output** — appends to the visible-text projection, not to `nodes` |

Site 1958 is worth separating: it writes the visible-content projection rather
than the node list, so its relationship to span coverage is different in kind
from the other five. Lines 2031/2034 are a `has_rule` query in
`warigaki_target`, not an emission.

`source_span_coverage` is computed over parser-IR node spans
(`CoverageBasis { NodeSpans => "parser_ir.nodes[*].span" }`, consuming
`input.parser_ir_bytes`), and the governed ignored-regions taxonomy
`data/parser-rq-ignored-regions-v1.json` is `{"rules":[]}` — nothing is exempt.
`silent-drops` partitions the resulting source gaps into diagnosed and silent.

The defensible finding, stated at the strength the trace supports:

> The inspected unsupported paths demonstrate that representational loss **can
> coexist with complete span coverage**. Therefore the nine predicates do not
> imply fidelity. Whether *all* current divergence paths preserve coverage
> remains open under Q2 and Q3.

So `rules_emitted: 54` of `690` and `unsupported_occurrences: 195,437` document a
major known fidelity exposure in a dimension the predicate set cannot detect.
They do **not** license waiving full-corpus uncertainty: this section must not be
cited to conclude that a full-snapshot campaign will pass.

`source_span_coverage = 1.0` **can be** satisfied by a text node standing in for
a lost `warigaki`, because the bytes are covered. The trace shows the mechanism
by which that happens; no captured `warigaki` witness has been produced showing
the predicate actually returning `1.0` over such a work. Attaching one — a
fixture or a real capture — would turn this from a mechanism into a
demonstration. The nine predicates measure coverage,
validity, diagnosis, and resources — not whether the node covering a byte says
the right thing about it.

This bears directly on D4. A green full-corpus report must not be read as "the
parser is faithful." Closing it requires a **tenth predicate on representational
fidelity**, instrumented from the divergence ledger. That is a predicate-set
question, not a corpus question, and it does not block tiering.

## Proposed Tiers

| Tier | Population | Purpose | Cadence |
|---|---|---|---|
| **1. Fixture / conformance** | Small synthetic works, expected-output-bearing, adversarial, including malformed inputs and exact diagnostic contracts | Feature interactions and negative cases that natural data may never exercise | Every PR |
| **2. Snapshot qualification** *(name pending Q11)* | The governed qualification population derived from publication source selection — **exact binding pending Q11, equality proof pending Q9** | Exhaustive exposure of owned-contract predicates | Release / high-risk change |
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

Three populations, three different obligations. **The tier 2 row is one of two
candidate designs and is settled by Q11**, so the relation below is provisional:

| Population | Relation to tier 2 | Obligation |
|---|---|---|
| Source-bundle admission universe | superset | **accounted set difference** — every excluded archive explained |
| **Publication-selected projection** | *if Q11 picks successful selection:* **identical** · *if Q11 picks the pre-attempt candidate population:* **authenticated accounted projection** | exact set equality, or a typed accounted difference — **pending Q11** |
| Conversion-audit population | overlapping | **accounted set difference** — reconciled, not equalised |

What is fixed regardless of Q11: tier 2's population is *derived from publication
source selection* rather than from an independent walk of the checkout, and its
relation to every other population is authenticated rather than assumed.

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

Tracked as Q11. The tier's name follows the decision: "full-snapshot
qualification" is apt for the pre-attempt population, whereas the successful
population is more precisely a **publication-workload snapshot** — and neither
should imply the source-bundle admission universe.

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
| **Q2 / Q3** (incomplete divergence trace) | **Predicting** outcomes from D6; claiming all divergence paths preserve coverage; inferring fidelity from a green report; using D6 as a substitute for measurement | Accepting the architecture; running the exploratory campaign; **reporting directly measured predicate observations from either campaign**, exploratory or confirmatory |
| **D7** (expected-failure semantics) | The fixture tier containing adversarial or malformed inputs | The tier architecture; tier 2 and tier 3 work; the `wall-time` reshape |
| **Q11** (which selection projection) | Tier 2's population definition, its relation to publication selection, and the tier's name | Accepting that a snapshot tier should exist |
| **Q9** (population equality; depends on Q11) | Describing tier 2 as a census; defining its governed membership | Accepting that a snapshot tier should exist |
| **D8** (repetition protocol) | Tier-relative repetition counts | Everything else; tier 2 can run at the current fixed 3 |

So the three-tier **architecture** is acceptable now. What is not yet available
is a confirmatory qualification at tier 2, an adversarial tier 1, or a census
claim.

**The exploratory campaign does not by itself close Q2/Q3.** A green campaign
does not show that every `ParserResidue` lacks an eligible source span, nor that
every `LOSS`/`AMBIGUITY` path preserves coverage — measured outcomes are not the
same proposition as the mechanism that produced them. Closing Q2 needs the actual
`ParserResidue` occurrences inspected or instrumented; closing Q3 needs
class-specific ledger and code-path analysis. The campaign's role is to **supply
the evidence population and witnesses** for those dedicated probes.

Current blocker status, plainly:

- **B1 — D6 scope.** Wording corrected; **Q2/Q3 still open.** Partially resolved.
- **B2 — D7.** **Unresolved.** Option 1 recommended but not selected, and its
  expected-outcome model is unspecified.
- **B3 — Q9, gated on Q11.** **Unproven.** The document no longer overclaims, but
  the equality proof does not exist, no canonical join key is predeclared, and
  Q11 must be decided first — the equality target itself depends on it.
- **B4 — D8.** **Open.** Tier-relative repetitions are still proposed in the
  wall-time reshape, so this is in scope, not incidental. If the migration is not
  wanted, the accepted architecture must instead **explicitly retain three fixed
  repetitions at every tier** — which is a legitimate choice, since tier 2 can
  run at the current constant.

Then:

1. Measure the remaining harness layer (Q1) and settle the other open questions.
2. Define tier semantics and authority **before** changing any hash.
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

- **Q1 — The RQ capture layer is unmeasured.** Measurements cover `ab-aozora`
  and `ab-check`, not evidence writing, hashing, and envelope construction above
  them. That third layer could add materially to 25.12 ms/work.
- **Q2 — `ParserResidue` spans unverified.** It is the only non-covering arm. It
  is selected on `x-provenance == "parser-derived"`, which *should* mean no
  eligible source span, but this is not confirmed. If any occurrence carries a
  real span, it is a coverage hole.
- **Q3 — LOSS and AMBIGUITY untraced.** D6 traced `UNSUPPORTED` and
  `STRUCTURAL`. The bulk of 4,938,800 divergences may be `LOSS`, whose coverage
  behaviour is unchecked.
- **Q4 — A heuristic sits in the conversion path.** `heuristic_enabled` is
  threaded through `map_block`, and `is_parser_raw_residue` classifies by string
  inspection. A heuristic that decides whether a node is emitted can change
  coverage without any policy hash moving.
- **Q5 — Host class undefined.** All figures are single-host. A throughput
  predicate requires a declared host class to be meaningful.
- **Q6 — Evidence volume at tier 2.** 17,878 per-work records against a store
  designed around 3; size, retention, and closed-membership cost unmeasured.
- **Q7 — Fidelity predicate shape.** If adopted, what does it assert — rules
  emitted over rules total, divergence occurrences per node, or per-category
  ceilings? Unresolved.
- **Q8 — Tier 1 membership.** The v0 design
  (`docs/v0-design-bundle/ci-smoke-corpus.md`) specifies 9 buckets totalling 23
  works; 2 buckets and 3 works exist. Which adversarial cases are authored, and
  what their expected diagnostics are, is unspecified.
- **Q9 — Population reconciliation. PREREQUISITE for the tier 2 claim, not a
  minor item.** 17,878 inventoried against `admitted_zip_count` 17,880 and audit
  `files_scanned` 17,886. Resolving it means proving **set equality with
  publication's selected projection only**, and producing an **accounted set
  difference** — not equality — against the source-bundle admission universe and
  the conversion-audit population, explaining `rejected`, `derive-failures`, and
  `continue_on_failure` skips. Requiring equality against admission would be the
  wrong condition and would push tier 2 back to every admitted archive purely to
  satisfy it. Until proven, tier 2 is "intended to census the publication-source
  population," not a census.
- **Q11 — Which selection projection binds tier 2? Prerequisite to Q9, and it
  determines the tier's name.** The successfully selected publication population
  (exactly the published workload; qualification can stay green on a shrunken
  denominator while the independent release gate rejects the partial corpus), or
  the pre-attempt catalog-backed candidate population with selection failures
  retained as qualification failures (qualification exposes the failure itself,
  but qualifies works publication did not emit). Until decided, tier 2's relation
  to the publication-selected projection is *provisional*, and the tier is named
  "snapshot qualification" rather than "full-snapshot" so it cannot be read as the
  admission universe.
- **Q10 — Fidelity across corpus growth.** If the fidelity predicate (Q7) is
  adopted, its observed value depends on corpus composition like the others, so
  its threshold cannot be set from the 3-work corpus either. It has to be
  predeclared from the exploratory full-snapshot campaign.
