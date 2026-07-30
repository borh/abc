# Region Partition — Exploratory Measurement, 2026-07-27

**Non-authoritative.** This report is not a campaign capture, is not admissible
evidence for any predicate, and must not be cited as one. Its qualification
identity is synthesized and authenticates nothing; its corpus is three works
chosen by hand. It exists because the numbers that drove the region-partition
decisions lived only in plan and ADR prose, where nothing reproduces them and
nothing detects them going stale.

What it does bind is enough to re-run: the exact source bytes, the exact
governed documents, the exact code revision, and the command.

## What was measured

Two ratios over one decoded file, in two populations that partition it:

- **body recognition** — `recognized_bytes / body_bytes`. Of the body region,
  how much did the parser recognize?
- **metadata attribution** — `metadata.attributed_bytes /
  metadata.eligible_bytes`. Of the header and tail, how much is attributed to an
  understood packaging construct?

They answer different questions and are deliberately not combined. A single
whole-file ratio over their sum is the average this partition exists to remove.

Attribution counts a byte only when a fact whose `source_role` appears in the
classified-source policy's `metadata_attributing_roles` covers it, and never
when the disposition is `preserved_opaque`. See
[parser-rq-source-region-partition](../adr/parser-rq-source-region-partition.md).

## Provenance

| | |
|---|---|
| repository revision | `507fc2e7` plus the residue classifiers of this commit |
| corpus | `aozorabunko` at `0e9ea3e586eb0aa34039fabfc85a407d2f98b165`, resolved through the flake input |
| toolchain | `rustc 1.96.1 (31fca3adb 2026-06-26)` |
| instrument | `parser-rq-source-recognition-v2` |
| work record wire schema | `abc/parser-rq-source-recognition-work/v2` |

Governed documents, by raw file bytes:

| document | sha256 |
|---|---|
| `data/parser-rq-ab-aozora-classified-source-v1.json` | `3212d69119b7687c43a21720d14f9c56cc3068f1aeec1c42b930683160c9f05b` |
| `schemas/parser-rq-classified-source-ledger.schema.json` | `6e8db5c3b2b0d18a95e95ecece75fc211c4e954dc3d599716fd7db47edee26d9` |
| `schemas/parser-rq-source-recognition-work.schema.json` | `600f7c195e99c8bac5b6102eaadf59d9ff334759dbdaeb7ad1148d4a031c9474` |
| `schemas/parser-rq-source-recognition-aggregate.schema.json` | `b1c43492208185989fe9e028fc2710ce64f2a4a90952072efc847a101209d1f8` |

Run identities, as the instrument emitted them:

| | |
|---|---|
| `policy_hash` | `sha256:42949d48…e1850bee` |
| `qualification_identity_ref` | `sha256:228669fe…1eae1365` (**synthesized**) |
| `membership_ref` | `sha256:e5268dea…ccb0962c` |
| `corpus_generation_ref` | `sha256:2d3e7eff…53be1914` |

The qualification identity is a probe fixture with placeholder mapping, corpus
and predicate-set hashes. It is what makes this report exploratory rather than
evidentiary, and it is why the identity ref is recorded but not treated as
meaningful.

## Corpus

| work | original source sha256 |
|---|---|
| `hashire_merosu` | `sha256:03def0d5f4322d1cf1bb812a7cca4e490464cb95090182a34a6e2665dde22ba7` |
| `hatsukoi` | `sha256:296cd806ac46e93855dd25c2aebe7b22fd163a278f98daf9f43c40808d6ade94` |
| `kokoro` | `sha256:d0124c6a71ca3c8f6898dc84578134789fcbdf6a1133121d205ebdaf52274594` |

Three works is not a sample. It cannot support a claim about the corpus, and no
threshold should be set from it.

## Regions

Instrument-derived, in `decoded_utf8` coordinates. Header, body and tail
partition the file exactly.

| work | decoded | header | body | tail |
|---|---|---|---|---|
| `hashire_merosu` | 32,148 | 505 | 30,985 | 658 |
| `hatsukoi` | 259,320 | 495 | 258,077 | 748 |
| `kokoro` | 559,512 | 621 | 558,100 | 791 |
| **fold** | **850,980** | 1,621 | 847,162 | 2,197 |

## Body recognition

| work | body | recognized | gap | fold |
|---|---|---|---|---|
| `hashire_merosu` | 30,985 | 30,613 | 372 | 0.987994 |
| `hatsukoi` | 258,077 | 255,194 | 2,883 | 0.988829 |
| `kokoro` | 558,100 | 553,840 | 4,260 | 0.992367 |
| **fold** | **847,162** | **839,647** | **7,515** | **0.991129** |

The whole-file fold this replaced was 0.9869. Moving the denominator to the body
gains 0.0042 — it does not approach 1.0. Two thirds of the total gap is inside
the body, so the residue is parser limitation and not a coordinate artifact.
This is the measurement that qualifies claim c2 of
`parser-rq-instrument-before-threshold`, which says the shortfall was entirely
packaging never being lexed.

## Metadata attribution

| work | metadata | attributed | fold |
|---|---|---|---|
| `hashire_merosu` | 1,163 | 1,101 | 0.946690 |
| `hatsukoi` | 1,243 | 1,179 | 0.948512 |
| `kokoro` | 1,412 | 1,344 | 0.951841 |
| **fold** | **3,818** | **3,624** | **0.949188** |

Nine constructs contribute across four producers: the colophon in the tail with
its field, continuation, dating, remark and licence forms; the fenced editorial
legend block in the header; the bibliographic block above it; and the archive's
own distribution notice. The measure has moved five times, and the moves are
different in kind.

| | metadata fold | |
|---|---|---|
| colophon producer, first draft | 0.3002 | **superseded, wrong** |
| after the attribution contract | 0.126768 | correction |
| after the legend classifier | 0.500786 | new coverage |
| after the bibliographic classifier | 0.616815 | new coverage |
| after the distribution notice | 0.784180 | new coverage |
| after the residue classifiers | **0.949188** | new coverage |

The **0.3002** figure counted two things that are not attribution: the
line-ending normalizations the sanitizer emits across the whole file, which are
true of every line whether or not anything understands the packaging and which
made the number depend on whether a work used CRLF; and header lines matching
the colophon field shape, including the standard notation legend `《》：ルビ`,
which is a `key：value` line by shape and is not publication metadata. Confining
attribution to the tail and to policy-named roles put the colophon's real
contribution at 0.126768.

The moves to 0.500786, 0.616815, 0.784180 and **0.949188** are the legend,
bibliographic, distribution-notice and residue classifiers. All four are
coverage the instrument did not have before, rather than corrections to numbers
it already reported.

The last move is also the largest, and part of it is not new coverage but a
corrected span. A line fact now covers its whole line, indentation included.
The earlier producers trimmed layout whitespace off the span as well as off the
text they tested, which left a colophon continuation claiming everything about
itself except the indentation that identified it. That accounted for 16,470 of
the sample's unattributed bytes and could never have been classified away,
because no construct was permitted to cover them.

Body recognition is unchanged at 0.991129 across every move. That is the check
that the metadata producers stayed inside their own population; it is the one
number that must not move when a metadata producer is added.

## Conservation

847,162 + 3,818 = 850,980, equal to the decoded total, with no overlap between
the body and metadata interval sets. The instrument refuses to emit a record
where this fails.

## The 597-work sample

Three works cannot support a claim about the corpus. This section can, within
the limits stated at the end of it.

**Population.** Every `.zip` under the pinned checkout, path-sorted (17,913 of
them), sampled with Python's `random.sample(zips, 600)` under `random.seed(20260727)`.
Three of the 600 are index archives carrying no `.txt` member and are recorded
as skipped rather than silently dropped, leaving **597 works, 25,080,140 decoded
bytes**. This is the same seed and the same sorted list the legend classifier's
line forms were designed against, so these are the works the design saw — a
held-out sample would be a stronger test and this is not one.

**Method.** Each work's raw Shift-JIS bytes through
`capture_generation_from_bytes_for_identity_and_work` and then
`analyze_recognition`, which is the real capture producer and the real
measurement path. Nothing is reconstructed. Parser-IR is not involved, because
the per-work recognition record does not require it.

**Every one of the 597 returned `status: ok`.** No capture failure, no
unavailable record, no conservation rejection across 25 MB.

| | bytes | fold |
|---|---|---|
| body recognized / eligible | 24,088,179 / 24,382,817 | **0.987916** |
| metadata attributed / eligible (region denominator) | 662,859 / 697,323 | 0.950577 |
| metadata attributed / content (**the ratio**) | 662,859 / 662,859 | **1.000000** |
| body + metadata | 25,080,140 | = decoded, exactly |

Metadata attribution was 0.512965 before the bibliographic classifier, 0.630518
before the distribution notice and 0.810653 before the residue classifiers;
body recognition is byte-identical across all three changes.

### The distribution, which is what a per-work predicate needs

Clearance is conjunctive and per work, so an aggregate fold is not what a
threshold would be tested against. The distributions are:

| | min | p05 | median | p95 | max | at 1.0 |
|---|---|---|---|---|---|---|
| body recognition | 0.6588 | 0.9689 | 0.9934 | 1.0000 | 1.0000 | **105 / 597** |
| metadata, region denominator (v2) | 0.9264 | 0.9421 | 0.9486 | 0.9581 | 0.9833 | 0 / 597 |
| metadata attribution (**v3**) | 1.0000 | 1.0000 | 1.0000 | 1.0000 | 1.0000 | **597 / 597** |

**Both rows are the same numerator over two different denominators**, and the
distance between them is the whole finding of this report. Five rounds of
classifiers moved the v2 median from 0.5384 to 0.9486 and its minimum from
0.2114 to 0.9264 without moving one work to 1.0 — and by the end the reason was
no longer that packaging was unclassified. It was that the denominator counted
bytes no construct is allowed to claim. See *What the unattributed bytes are*
below.

**105 of 597 works reach 1.0 on body recognition**, and the median is 0.9934. A
`:= 1.0` body threshold — the literal currently in the predicate set, declared
for the old whole-file denominator — would fail 492 of 597 works, 82% of them.

### Classifier coverage

| construct | lines | bytes | works |
|---|---|---|---|
| `distribution_notice_line` | 590 | 125,613 | 590 |
| `publication_metadata_line` | 2,972 | 116,175 | 591 |
| `publication_metadata_continuation` | 1,625 | 76,154 | 589 |
| `editorial_legend_entry` | 1,404 | 75,547 | 557 |
| `editorial_separator_rule` | 1,114 | 61,265 | 557 |
| `editorial_legend_example` | 1,403 | 59,301 | 557 |
| `editorial_remark_line` | 411 | 52,924 | 243 |
| `editorial_legend_heading` | 557 | 28,407 | 557 |
| `bibliographic_header_line` | 1,256 | 20,529 | 557 |
| `editorial_legend_note` | 197 | 19,087 | 197 |
| `file_dating_line` | 734 | 16,125 | 591 |
| `editorial_remark_continuation` | 20 | 6,666 | 9 |
| `editorial_legend_line` | 70 | 4,086 | 61 |
| `licence_statement_line` | 4 | 854 | 4 |
| `licence_statement_continuation` | 1 | 126 | 1 |

557 of 597 works carry a recognized legend block and a recognized bibliographic
block, and 591 carry a recognized colophon. Every absence was checked rather
than assumed:

- The **40 works with no legend block** were re-scanned for a fenced pair and a
  `【...】` heading in the header. **Zero** have one. Every absence is genuine;
  the classifier missed nothing.
- Those same 40 works have a **zero-byte header**, which is why they carry no
  bibliographic block either. There is nothing there to classify.
- The **6 works with no colophon** have a tail region of exactly 0 bytes.
- The **8 works with no continuation run** either have no colophon at all or a
  colophon whose fields are each complete on their own line.
- The **7 works with no distribution notice** are those same 6 plus one, and
  every one was opened. The 6 have no `底本：` line at all, so their tail is
  empty and their notice sits in the body region, where the parser lexes it and
  body recognition counts it. The seventh writes the notice as the value of a
  `青空文庫作成ファイル：` field on one line, so it is attributed — as a
  colophon field, which is what that line is.

The bibliographic block is 1 to 6 lines, 437 works carrying exactly the two of
title and author. No sampled header holds a further non-blank line between that
block's terminating blank and the legend fence, so the blank line is a real
terminator rather than a convenient one.

Every construct count above matches an independent reconstruction of the same
sample line by line, which is the cross-check that the instrument's region
derivation and the design analysis agree.

### One defect, found by running this

The two counts initially disagreed by one: 196 notes from the instrument against
197 from the reconstruction. The cause was a legend line carrying a **trailing
space**. `region_lines` stripped indentation but not trailing whitespace, so
`）` was no longer the line's last character, the note test failed, and the line
fell through every form to unattributed.

It is one line in 597 works and 85 bytes, so its effect on the fold is nil. Its
cause was not: whether a line is recognized should never depend on whether a
transcriber left a trailing space. Trimming is now symmetric, and the figures in
this section are post-fix. A single unexplained one-line discrepancy is exactly
the shape every earlier defect in this area took, which is the argument for
chasing it rather than rounding it away.

### The distribution notice, measured over the whole corpus

The notice is the one sentence this instrument recognizes, so how it is
recognized was settled against all 17,913 works of the pinned checkout rather
than against the sample. **A closed set of literals was measured and rejected.**
The notice appears in **29 distinct forms**, and the variation is not only the
URL scheme:

| | works |
|---|---|
| `…（http://www.aozora.gr.jp/）で作られました。…` | 14,159 |
| the same with `https` | 3,178 |
| the same with no trailing slash | 118 |
| `…著作権者自らの意思により…に収録されています。` | 107 |
| the remaining 25 forms | 236 |

The tail of that distribution is transcription noise: `インターネツト` for
`インターネット`, `あたつた` for `あたった`, `みんなさん` for `皆さん`,
`www.aozora.gr.p` for `.jp`, halfwidth `()` for `（）`, a fullwidth `：` inside
the URL scheme, and truncations that drop the closing `。`. Enumerating those
would be enumerating typos, and the thirtieth arrives with the next corpus
revision.

So the test is the two fixed anchors the sentence opens with and names: it
begins `このファイルは、` and it names `青空文庫`. Measured corpus-wide,
**every one of the 17,680 tail lines beginning `このファイルは、` is this
notice**, and not one of them lacks `青空文庫` — so the second anchor rejects
nothing that exists today and is kept as a guard, with a test, against a
transcriber remark that opens the same way.

**One ordering defect was found by this and is fixed.** The variant writing
`http：//` with a fullwidth colon makes the whole sentence a non-empty key, a
fullwidth colon and a value — a colophon field by `is_colophon_field` — and it
was classified as `publication_metadata_line`. The notice is now tested before
the field, because the construct a sentence gets must not turn on a typo inside
it.

### What the unattributed bytes are — and why 1.0 is not reachable

**Every content byte of the header and tail of all 597 works is now attributed.
The entire remaining residue is line structure.**

| | bytes | share |
|---|---|---|
| line terminators and blank layout, tail | 17,416 | 50.5% |
| line terminators and blank layout, header | 17,048 | 49.5% |
| **anything carrying a non-whitespace character** | **0** | **0.0%** |

That table is derived by splitting every published
`metadata_unattributed_intervals` entry at line boundaries, so it is exhaustive
and sums to the 34,464-byte residue exactly. There is no category of prose left
in it, no "other", and no long tail.

The five rounds of classification took the residue from 339,621 bytes to 34,464
and moved it from mostly-content to entirely-structure:

| after | residue | of which content |
|---|---|---|
| the legend classifier | 339,621 | 288,687 |
| the bibliographic classifier | 257,648 | 206,714 |
| the distribution notice | 132,036 | 81,102 |
| the residue classifiers | **34,464** | **0** |

**So the measure cannot reach 1.0, and that is now a property of the
denominator rather than of the classifiers.** `metadata_eligible_bytes` is the
header and tail regions whole, line terminators included. A terminator is not
packaging that can be understood or misunderstood; it is the delimiter between
packaging items, present in identical quantity whether the instrument
recognizes every construct or none. No construct claims one, so 34,464 bytes of
the 697,323 can never enter the numerator. **The ceiling is 0.950577 and the
instrument is sitting on it.**

The earlier version of this section computed that ceiling as 0.9270 and gave
two reasons: line terminators and layout whitespace. **The second reason was a
defect, not a ceiling.** Layout whitespace was unattributable only because
`region_lines` trimmed indentation off the fact's span, which meant a colophon
continuation — a construct recognized *by* its indentation — declined to cover
the evidence that identified it. A line fact now covers its line, and those
16,470 bytes are attributed. The remaining ceiling has one cause, not two.

**The measure is currently sensitive to line-ending convention, in the wrong
direction.** A CRLF work carries two terminator bytes per line where an LF work
carries one. Those bytes are in the denominator and can never be in the
numerator, so a CRLF work scores strictly lower than a byte-identical LF work
whose packaging is understood exactly as well. The `structural_newline` role
was kept out of `metadata_attributing_roles` precisely to stop the measure
moving "when line endings changed rather than when packaging becomes
understood" — but excluding terminators from the numerator while keeping them
in the denominator does not remove that sensitivity, it inverts it.

Three resolutions were available:

| | reaches | keeps region conservation | line-ending invariant |
|---|---|---|---|
| **A.** leave it: the measure caps below 1 | 0.950577 | yes | no |
| **B.** a construct's span includes its own line terminator | ~0.9900 | yes | approximately |
| **C.** terminators leave the attribution denominator | 1.000000 | yes, if published alongside | yes |

**C was taken, and the instrument now implements it as
`parser-rq-source-recognition-v3`.** It is the only option that makes the
measure independent of line-ending convention, and the only one under which
1.0 means something achievable — which matters because `:= 1.0` is the literal
the predicate set already carries.

It does not reduce the accounting. `metadata.eligible_bytes` is still the
regions whole, still published, and still the term in
`eligible + metadata.eligible == decoded`; terminators keep their
`structural_newline` facts and their place in `metadata.unattributed`. What was
added beside them is `metadata.content_bytes` — the same regions with line
terminators and wholly blank lines removed — and the attribution ratio is now
taken over that. Both denominators are in the record, so a reader can compute
either and neither is inferred.

A wholly blank line leaves the denominator with the terminators, and a line of
nothing but layout whitespace leaves with it. Counting one and not the other
would make the measure depend on whether a transcriber's editor stripped
trailing spaces, which is the failure this instrument already fixed once in
`region_lines`.

Three invariants guard the new denominator, and each can fail:
`attributed <= content <= eligible`, `attributed + unattributed_content ==
content`, and set containment of the attributed intervals inside the content
intervals. A producer that ever claimed a terminator or a blank line would
trip the first or the last, rather than quietly scoring above 1.

B was rejected for a subtler reason than its arithmetic. It would attribute a
terminator whenever the line above it was understood, which is defensible, but
it leaves the blank-line terminators — 4,688 bytes of the sample are the
four-line gap every file puts between its body and its colophon — and reaching
1.0 would then require claiming *those*, which is claiming to understand a gap.
The number would arrive by widening claims until nothing is left to decline,
which is the failure mode this instrument was built to avoid.

### Under v3 the measure reads 1.000000, and that is a warning as much as a result

**597 of 597 works, and 0 unattributed content bytes.** A saturated measure
cannot, by itself, distinguish "the instrument covers the archive" from "the
instrument was fitted to these 597 works". The held-out sample below is what
separates those two, and it comes out on the first side.

But the predicate owner should read the number for what it is. Under the v2
denominator, `:= 1.0` was unreachable by construction and therefore obviously
the wrong threshold. Under v3 it is met by every work of both samples, which
makes it *look* right and means something different: **a threshold that
currently refuses nothing.** Its entire value is in what it will refuse when
the archive produces packaging these classifiers do not know — a header with no
fence, a colophon field in a form not in the corpus today, a new boilerplate
sentence. That is a real and useful property, and it is not the same property
as "the instrument has been validated at 1.0".

## The held-out sample

Everything above is measured on the works the classifiers were designed
against. This section is not.

**Population.** The same 17,913 path-sorted archives, sampled with
`random.sample(zips, 600)` under `random.seed(20260728)` — a different seed. 16
of the 600 also appear in the design sample and were removed, leaving **584
works, none of which any classifier was designed against**.

| | design sample | **held-out sample** |
|---|---|---|
| works | 597 | 583 |
| decoded bytes | 25,080,140 | 31,494,853 |
| body recognition | 0.987916 | **0.988187** |
| metadata, region denominator (v2) | 0.950577 | **0.950575** |
| metadata attribution (**v3**) | 1.000000 | **1.000000** |
| works at metadata 1.0 (v3) | 597 / 597 | **583 / 583** |
| content bytes unattributed | **0** | **0** |

**Metadata attribution differs between the two samples in the seventh decimal
place under the region denominator, and is exactly 1.000000 under the content
one**, with the residue on unseen works again 100% line terminators and blank
layout — 33,807 bytes, not one of which carries a non-whitespace character. The corpus-wide anchor measurement did its job: the classifiers
describe the archive's packaging conventions rather than 597 works' worth of
them.

The one number that moved is the metadata minimum, 0.9264 to 0.8908. That is a
work with an unusually high ratio of blank lines to content in its metadata
regions, which under the current denominator reads as lower attribution — the
same denominator artefact described above, seen from the other end.

### The held-out sample found what the design sample could not

**One work still yields no measurement, and a second defect behind it.** One of
the 584 originally failed capture outright with `duplicate classified-source
entry` over `〔George Innes, 1825―1894.〔Albert Biersta\`dt〕` — an unclosed
accent bracket containing a closed one. That was fixed; the work now captures
and then returns `status: unavailable, errors: ["ledger-evidence-invalid"]`,
because its accent normalization proof does not round-trip.

Chasing that surfaced the more serious finding. **The accent decomposition is
applied to whole `〔...〕` spans rather than to the sequences that motivate
them**, so ordinary punctuation is consumed by the mappings: `Innes, ` becomes
`Inneş` and `hot,` becomes `hoţ`, because `s,`→`ş` and `t,`→`ţ` ate a comma
that was sentence punctuation. The proof carries
`inverse_rule: "accent_decomposition"` under `lossless_normalization` — a claim
that nothing is lost — and it loses a comma. **The round-trip check cannot
catch it, because the recognition side reproduces the same mapping**; both
sides make the identical mistake.

**Zero of the 597 design-sample works do this. Four of the 584 held-out works
do, and three of those four return `status: ok`** — the corrupting
normalization is accepted and published. Neither defect is caused by the
classifier work and neither is fixed here; both are recorded in
`plans/2026-07-28-pre-existing-defects-handoff.md`.

This is what the held-out sample was for. Both defects sit in packaging shapes
the design sample does not contain, and no amount of re-measuring the design
sample would have produced either.

The corpus-wide scan both defects called for was run on 2026-07-30, over all
17,887 archives of the same population under the same first-text-member
approximation: **26 works produce no record at all** (23 duplicate
classified-source entries, every one a construct nested inside a `〔…〕`
accent span; 3 `lossy source decoding`), and **73 works publish accent
normalizations that lose prose characters** — a lower bound, from a
comma-loss criterion with the genuine cedilla decompositions
(`Franc,ois` → `François`, 27 works) separated out by aligning each proof's
source and normalized forms. Figures and method are recorded in
`plans/2026-07-28-pre-existing-defects-handoff.md`, *Corpus-wide scope*.

## Reproducing

Stage the three works, their parser-IR documents, a probe qualification identity
and the ignored-regions taxonomy in a scratch directory — none of which belong
in the repository — then:

```
cargo run -p ab-parser-rq-source-accountability -- capture-corpus \
  --corpus <corpus.json> --source-root <src> --parser-ir-root <ir> \
  --qualification <identity.json> --taxonomy <taxonomy.json> \
  --store-root <store> --output-dir <out>
```

The folds above are read from `<out>/source-recognition-aggregate.json`, whose
`eligible_bytes`/`recognized_bytes` and `metadata_eligible_bytes`/
`metadata_attributed_bytes` are the four numbers involved. Per-work rows come
from the records the index in `<out>/source-recognition-index.json` addresses.

## What this does not establish

**No threshold.** That is a governance act for the predicate's owner, and this
report is input to it, not a substitute for it. What the sample does establish
is that the literal currently in the predicate set is not a candidate for the
body measure: `:= 1.0` fails 492 of 597 works. On the metadata side the answer
now depends on a denominator decision that has not been taken — see *What the
unattributed bytes are*. Under the denominator as it stands, `:= 1.0` fails all
597 and is unreachable by construction; under resolution C it passes all 597,
which is a different reason to be careful of it.

**The held-out test has now been run, and it passes.** See *The held-out
sample*: a second 597-work draw under a different seed, disjoint from the first,
reproduces metadata attribution to the seventh decimal and leaves the same
all-terminator residue. What that establishes is that the classifiers are not
fitted to their design population. What it does not establish is coverage of
packaging conventions absent from both samples — the bibliographic block's
terminator set is still the weakest point, because every sampled header in both
draws separates bibliography from legend with a blank line, so the
separator-rule and bracketed-heading terminators remain untested against a
header that omits it.

**Not an authoritative campaign.** The qualification identity is synthesized,
the run is not a capture under the promoted identity, and the probe binary and
sampling script are scratch state rather than committed tooling — deliberately,
since neither should become a governance input. Reproducing the sample requires
re-running them, which the *Reproducing* section describes but does not
automate.

**Not a claim about the corpus.** 597 of 17,913 works is 3.3%, drawn at random
but from a single snapshot, and the aozorabunko population is not homogeneous —
old-orthography works, accent-decomposition works and modern transcriptions
carry different packaging conventions. Nothing here is stratified.
