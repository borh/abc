# Partition the Source into Body and Metadata Populations

## Implementation Status

**Tasks 1–5 implemented, 2026-07-27, then reviewed and corrected. Task 6
(predeclare both thresholds) deliberately not executed.** The task sequence is
in `docs/superpowers/plans/2026-07-27-q15-region-partition.md`.

Measured through the built binary on three real works (850,980 decoded bytes):
body recognition **0.991129**, metadata attribution **0.784180**, cross-region
conservation exact. Reproducible in
`docs/reports/parser-rq-region-partition-exploratory-v1.md`, which is
non-authoritative and says so.

The measurement qualifies the premise this record inherited.
`parser-rq-instrument-before-threshold` c2 holds that the shortfall was
*entirely* packaging never being lexed. The header and tail do behave that way,
but two thirds of the total gap lies inside the body, and moving to a body
denominator moves the fold only from 0.9869 to 0.9911. Populations differ — c2
rests on a 299-work sample — so this does not refute the sampled figure, only
the qualitative claim that the coordinate mismatch is the whole story. The
partition is still worth its cost, because it makes the two failure modes
separable; it is not the fix that reaches the declared threshold.

Three classifiers landed 2026-07-27 and took metadata attribution from 0.126768
to 0.500786 (editorial legend), 0.616815 (bibliographic block and colophon
continuations) and 0.784180 (distribution notice). Each is new coverage, not a
correction to a reported number; what remains unattributed is enumerated under
*Decision*.

**Measured across a 597-work random sample of the pinned corpus** (25,080,140
decoded bytes, every work returning `ok`, conservation exact): body recognition
folds to 0.987916 and metadata attribution to 0.810653, both close to the
three-work control. The distributions matter more than the folds, because
clearance is per work:

| | median | max | works at 1.0 |
|---|---|---|---|
| body recognition | 0.9934 | 1.0000 | 105 / 597 |
| metadata attribution | 0.8757 | 0.9494 | **0 / 597** |

**This settles what `:= 1.0` would mean for either measure.** It fails 492 of
597 works on the body and every work on metadata. On the metadata side it is
not merely unmet but unreachable: line terminators and layout whitespace can
never attribute, which caps that measure at 0.9270. The body literal was
declared against the old whole-file denominator and cannot simply be carried
across; the metadata measure has no declared threshold and must not inherit
that one. Both
figures are in
`docs/reports/parser-rq-region-partition-exploratory-v1.md`, which also records
that the sample is the classifier's own design population and therefore not a
generalization test.

Review of the implementation found four defects, all now closed and all of a
piece with what this record says about invariants that cannot fire: malformed
region sets underflowed the aggregate validator rather than being rejected; the
colophon producer also classified the header's notation legend, which is a
`key：value` line by shape; metadata attribution counted facts that are not
attribution (see *Decision*); and the changed measure still declared
`parser-rq-source-recognition-v1`, so the gate scored a body denominator against
a threshold predeclared for a whole-file one.

## Context

[parser-rq-instrument-before-threshold](parser-rq-instrument-before-threshold.md)
established that `source_span_coverage` divides a body-derived numerator by a
whole-file denominator, and that the instrument is fixed rather than the
threshold. It did not say what the fixed instrument measures.

Two answers were put to the owner. Reduce the denominator to the body projection
the parser was actually given; or extend capture so the header and colophon
produce facts in the same coordinate as body facts. Both are coherent, and they
mean different things by `source_span_coverage = 1.0`.

**Both framings were rejected.** They present the body and the packaging metadata
as competing denominators for one ratio. They are two populations. The body is
the work — prose and annotations. The header and `底本：` colophon are metadata
*about* the work: a notation legend and bibliographic provenance of the
transcription. A single ratio over their union averages parser fidelity against
packaging attribution and can mean neither.

The evidence for that is the history of the number itself. The observed 0.9640
could not distinguish "the parser missed prose" from "packaging is never lexed",
and establishing that it was entirely the latter took three attempts.

## Decision

Partition the decoded source into declared regions and measure the populations
separately, with conjunctive clearance.

| Region | Extent | Measured by |
| --- | --- | --- |
| header | `[0, body_start)` | metadata attribution |
| body | `[body_start, body_end)` | `source_span_coverage` |
| tail | `[body_end, len)` | metadata attribution |

Three regions, no fourth, no unassigned byte. Note the tail begins at `body_end`
and **not** at `aozora_body_range`'s `tail_start` — see *Consequences*.

- Region authority is `aozora_body_range` **alone**. No separator or `底本：`
  heuristic is maintained anywhere else.
- The regions are pairwise disjoint and their union is the whole decoded file.
  The instrument asserts that conservation and fails closed.
- `source_span_coverage` becomes body-projection coverage. It is not a whole-file
  measure and must not be read as one.
- Metadata attribution is a separate predicate over the header and tail in their
  own coordinate. Header and tail are recorded as distinct regions so a failure
  localizes to one end of the file; they qualify under one conjunctive predicate
  rather than one predicate each.
- **A work clears only when both clear.**
- Neither threshold is carried forward by default. Build, measure under an
  explicitly non-authoritative exploratory campaign, then predeclare.

### What attributes a metadata byte

Decided 2026-07-27, after review found the first implementation counted every
accounted interval and so measured something other than attribution.

A metadata byte is **attributed** when a fact covers it whose `source_role`
appears in the classified-source policy's closed `metadata_attributing_roles`,
and whose disposition is not `preserved_opaque`. Nothing else attributes.

Both exclusions are load-bearing.

`preserved_opaque` is the disposition that records "these bytes are carried
through and nothing is claimed about them." Counting it would let the measure
improve by *declining* to classify, which is not a weakness of degree but a
contradiction of what the number reports.

A role outside the attributing set can still **account** for a byte without
attributing it. Every metadata line ending in a CRLF source carries a
`crlf_normalization` fact under `structural_newline`, because the sanitizer
walks the whole text rather than the body. That fact is true, and it is not
knowledge about the packaging: it holds of every line in the file whether or not
anything understands it. Counting it gave CRLF works several percent of
attribution for free and made the number move when line endings changed rather
than when packaging became understood.

The attributing role set is **governed data, not a constant in the instrument**,
so widening what counts as understood packaging rotates the policy hash and,
through `instrument_policy_hashes`, the qualification identity. That is the same
attribution route `parser-rq-classified-source-policy-binding` exists to create.

The published field names are `attributed`/`unattributed`, distinct from the
body population's `accounted`/`unaccounted`, so no reader can average a
recognition ratio and an attribution ratio into a single whole-file number —
which is the failure this whole record exists to prevent.

### What attributes: the classifiers that exist

Four producers, each bound to a region and a position within it, and never to
a line's shape — with one deliberate exception, named below.

| Producer | Region | Constructs |
|---|---|---|
| colophon | tail | `publication_metadata_line`, `publication_metadata_continuation` |
| editorial legend | header, inside its fence | `editorial_separator_rule`, `editorial_legend_heading`, `editorial_legend_entry`, `editorial_legend_example`, `editorial_legend_note` |
| bibliographic block | header, before the legend | `bibliographic_header_line` |
| distribution notice | tail | `distribution_notice_line` |

**Region, not shape, is what separates them.** A legend entry `《》：ルビ` and a
colophon field `底本：…` are the same shape — non-empty key, fullwidth colon,
value — carrying entirely different meanings. Nothing in either line's text
distinguishes it. A producer that lost its region bound would relabel one as the
other and attach the wrong role, which is exactly what the first colophon
implementation did.

The legend producer is bound twice over: to the header, and within it to a
closed pair of separator rules enclosing a `【...】` heading. All three are
required. Measured over a 597-work random sample of the pinned corpus, 557
headers carry such a block, every fence is a run of ASCII hyphens, and no fenced
pair was found without a heading — so a fence with no heading is a shape this
producer has never seen, and it declines the block rather than guessing.

Lines inside the block matching none of the four forms stay unattributed: 70 of
3,631 non-blank block lines in the sample, all of them transcriber prose
(`＊濁点付きの…`, bare URLs, `※底本では…`). Claiming them would be claiming to
understand a sentence.

The bibliographic block is the header's opening run, ending at the first blank
line, separator rule or bracketed editorial heading. It claims block membership
and not which item a line is: line 1 is usually the title and the last is
usually a translator, but original titles and subtitles break the ordering, and
reading the item off the position would be a guess dressed as a fact.

The colophon continuation is an indented line under a field, reaching it
without crossing a blank. That bound is what keeps the tail's other unindented
lines — the distribution notice, the file's own dating lines, the transcriber's
`※` remarks — out of the claim.

**Shape must not be consulted inside a bound block, and the sample says so
twice.** One work's title is
`※［＃「氓のへん／（虫＋虫）」、第3水準1-91-58］の囁き`, and one colophon
continuation is a citation whose title opens the same way. A leading `※`
elsewhere in these regions marks transcriber prose, so a producer that declined
`※` lines by shape would have declined a real title and a real citation. The
block is the claim's justification; within it, every line belongs.

**The distribution notice is the one sentence recognized, and it is recognized
as a constant rather than read.** `このファイルは、…青空文庫…で作られました。…`
is generated by the archive, not written by a transcriber, and it states in
prose what the `入力：` and `校正：` fields state as fields. A closed set of
literals was measured over all 17,913 works and rejected: the notice appears in
29 forms whose tail is transcription noise — `インターネツト`, `あたつた`,
`みんなさん`, `www.aozora.gr.p`, a fullwidth `：` in the URL scheme. So the test
is the two fixed anchors the sentence opens with and names, under which every
one of the 17,680 tail lines beginning `このファイルは、` corpus-wide is this
notice. It carries its own role so that the predicate's owner can exclude
boilerplate without excluding the colophon.

**What still has no producer**, measured across the 597-work sample: 42.8% of
the remaining unattributed bytes are transcriber `※` remarks, 26.1% are line
terminators and 12.5% is layout whitespace that the attribution contract
structurally excludes, 12.2% are the file's own dating lines, and the rest is
other prose plus 0.5% of Creative Commons licence statements. Those licence
lines state terms rather than provenance, so folding them into the notice
construct would make it mean two things; they are declined for now.

**The contract's own ceiling is 0.9270**, because terminators and layout
whitespace can never attribute. The instrument measures 0.810653 against it. If
prose is never attributable, the reachable ceiling is lower still, which the
predicate's owner must know before fixing a threshold.

## Consequences

**This is not the denominator reduction Q11 rejected.** No byte leaves the
accounting; the metadata bytes move to a different *accounted* region. The
conservation identity is what keeps that honest, and it is the reason the
partition is worth its cost.

**Conservation becomes assertable, and that is the point.** Every defect traced
in this area shares one property: no invariant existed that could catch it. The
node-span analyzer compares a coordinate label against a value the emitter
hard-codes, so its check can never fire. `Interval::new` bounds-checks and
nothing else. `source_span_coverage` divided across coordinates and returned a
believable 0.9640. A declared partition with a conservation check is the first
structure here that breaks loudly instead of returning a plausible number.

**The cost is both earlier options combined**, plus a partition schema and its
checks. This is not a middle path, and it should not be adopted believing it is.

**`aozora_body_range`'s return value is not itself a partition**, so the region
derivation does not use it verbatim. It returns `(body_start..body_end,
tail_start)` where `body_end` is trim-adjusted for trailing newlines and
`tail_start` is not, so whenever a tail exists at least one byte lies between
them.

The tail region is therefore `[body_end, len)`, extending backward to meet the
body and absorbing the blank lines that separate them. This is symmetric with the
header, which already absorbs its adjacent blank lines because `skip_blank_lines`
advances `body_start` past them.

Two alternatives were rejected. Widening `body_end` *forward* to `tail_start`
puts transcriber whitespace into the body population, making body coverage depend
on how many blank lines a transcriber left; metadata attribution over packaging
whitespace is trivially satisfiable by comparison, since the ledger vocabulary
already carries a `newline` disposition. Changing `aozora_body_range` itself is
rejected because `sanitized.body` is the projection handed to the parser, so
moving that boundary would move `source_span_coverage`'s numerator as a side
effect of a partition fix.

This gap was found by writing the plan rather than by running the instrument —
and an earlier draft of this record asserted whole-file conservation over three
regions while separately conceding a fourth was needed, which review caught. Both
are the argument for declaring the partition explicitly before relying on it.

**Sequencing.** Retiring the parser-IR node-span coverage path removes
`analyze.rs`, `aggregate.rs`, two schemas and four test files from this change
set, so that question is settled first. Mapping the twelve unmapped
`DirectiveKind` variants in `node_policy` is body-side work and a separate
reviewable deliverable, not blocked by the partition.

**Identity.** The predicate contract changes, so `predicate_set_hash` moves.
Prior captured evidence is stale.

**The instrument is quarantined at v2 until the predicate set catches up.**
Observed 2026-07-27: the measure changed while the instrument still declared
`parser-rq-source-recognition-v1`, and on the governed corpus the gate reported
`pass` for `source-span-coverage` against `:= 1.0` — a threshold predeclared for
a denominator that no longer exists. Wire versioning did not catch this, because
the wire format was not what changed.

The instrument is now `parser-rq-source-recognition-v2` and the predicate set is
deliberately left naming v1. `instrument_versions` is taken from the predicate
set's declared `:instrument`, and both readers require the record's
`instrument_version` to match it, so the two disagree and the observation is
`:unavailable`. Verified against the promoted identity of run `24d61fc7…`: it is
refused. This is the intended transitional state. Qualification is unavailable
rather than wrong, and it stays unavailable until the predicate's owner declares
a contract for what is now measured. Bumping the predicate set to v2 without
redeclaring the threshold would restore exactly the defect this quarantine
exists to stop.

## Evidence

The implementation is held by tests in `ab-validator/crates/`, which the
governance schema cannot reference: it admits only `test/`, `fixtures/`, `nix/`
and `docs/evidence/external/` prefixes. That is a gap in what this corpus can
cite, not an absence of evidence, and it is worth naming rather than working
around. The same gap is recorded in
[parser-rq-retire-node-span-coverage](parser-rq-retire-node-span-coverage.md).

`docs/reports/parser-rq-region-partition-exploratory-v1.md` binds the source
hashes, governed document hashes, code revision and command behind the measured
folds, but it is explicitly non-authoritative: its qualification identity is
synthesized and its corpus is three hand-picked works.

Promotion to Accepted requires evidence for each claim, the governed three-work
corpus as a control, and a real-source *sample* — not three works — measured
through the built binary under an identity that authenticates something.

Plan: `docs/superpowers/plans/2026-07-27-q15-region-partition.md`.
Design: `docs/superpowers/specs/2026-07-26-parser-rq-corpus-tiering-design.md`.
