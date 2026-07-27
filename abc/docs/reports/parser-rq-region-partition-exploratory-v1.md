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
| repository revision | `21e4422f` plus the distribution-notice classifier of this commit |
| corpus | `aozorabunko` at `0e9ea3e586eb0aa34039fabfc85a407d2f98b165`, resolved through the flake input |
| toolchain | `rustc 1.96.1 (31fca3adb 2026-06-26)` |
| instrument | `parser-rq-source-recognition-v2` |
| work record wire schema | `abc/parser-rq-source-recognition-work/v2` |

Governed documents, by raw file bytes:

| document | sha256 |
|---|---|
| `data/parser-rq-ab-aozora-classified-source-v1.json` | `73d8a6ec5eb239201621d39e52eb1360aac45cbc6f5e92b147edd7931c30677c` |
| `schemas/parser-rq-classified-source-ledger.schema.json` | `2da2384cf836d73cf1d3c90f498a4513dc6c6e085f02fdb79523e92dc482d3dc` |
| `schemas/parser-rq-source-recognition-work.schema.json` | `600f7c195e99c8bac5b6102eaadf59d9ff334759dbdaeb7ad1148d4a031c9474` |
| `schemas/parser-rq-source-recognition-aggregate.schema.json` | `b1c43492208185989fe9e028fc2710ce64f2a4a90952072efc847a101209d1f8` |

Run identities, as the instrument emitted them:

| | |
|---|---|
| `policy_hash` | `sha256:2e63e45e…5115bf75` |
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
| `hashire_merosu` | 1,163 | 1,030 | 0.885641 |
| `hatsukoi` | 1,243 | 909 | 0.731295 |
| `kokoro` | 1,412 | 1,055 | 0.747167 |
| **fold** | **3,818** | **2,994** | **0.784180** |

Four producers contribute: the colophon in the tail, the fenced editorial
legend block in the header, the bibliographic block above it, and the archive's
own distribution notice. The measure has moved four times, and the moves are
different in kind.

| | metadata fold | |
|---|---|---|
| colophon producer, first draft | 0.3002 | **superseded, wrong** |
| after the attribution contract | 0.126768 | correction |
| after the legend classifier | 0.500786 | new coverage |
| after the bibliographic classifier | 0.616815 | new coverage |
| after the distribution notice | **0.784180** | new coverage |

The **0.3002** figure counted two things that are not attribution: the
line-ending normalizations the sanitizer emits across the whole file, which are
true of every line whether or not anything understands the packaging and which
made the number depend on whether a work used CRLF; and header lines matching
the colophon field shape, including the standard notation legend `《》：ルビ`,
which is a `key：value` line by shape and is not publication metadata. Confining
attribution to the tail and to policy-named roles put the colophon's real
contribution at 0.126768.

The moves to 0.500786, 0.616815 and **0.784180** are the legend, bibliographic
and distribution-notice classifiers. All three are coverage the instrument did
not have before, rather than corrections to numbers it already reported.

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
| metadata attributed / eligible | 565,287 / 697,323 | **0.810653** |
| body + metadata | 25,080,140 | = decoded, exactly |

Metadata attribution was 0.512965 before the bibliographic classifier and
0.630518 before the distribution notice; body recognition is byte-identical
across both changes.

### The distribution, which is what a per-work predicate needs

Clearance is conjunctive and per work, so an aggregate fold is not what a
threshold would be tested against. The distributions are:

| | min | p05 | median | p95 | max | at 1.0 |
|---|---|---|---|---|---|---|
| body recognition | 0.6588 | 0.9689 | 0.9934 | 1.0000 | 1.0000 | **105 / 597** |
| metadata attribution | 0.2114 | 0.6589 | 0.8757 | 0.9085 | 0.9494 | **0 / 597** |

**No work reaches 1.0 on metadata attribution.** The best is 0.9494. A `:= 1.0`
metadata threshold would fail every work in the sample, and it would do so for
reasons that are mostly not defects — see the residue below. Four classifiers
have now moved the median from 0.5384 to 0.8757 and the maximum from 0.8449 to
0.9494 **without moving a single work to 1.0**, and the maximum has not moved at
all since the bibliographic classifier. That is the shape of a measure
approaching a ceiling rather than a target, and the ceiling is computed below.

**105 of 597 works reach 1.0 on body recognition**, and the median is 0.9934. A
`:= 1.0` body threshold — the literal currently in the predicate set, declared
for the old whole-file denominator — would fail 492 of 597 works, 82% of them.

### Classifier coverage

| construct | lines | bytes | works |
|---|---|---|---|
| `distribution_notice_line` | 590 | 125,612 | 590 |
| `publication_metadata_line` | 2,969 | 115,971 | 591 |
| `editorial_legend_entry` | 1,404 | 75,493 | 557 |
| `publication_metadata_continuation` | 1,622 | 61,451 | 589 |
| `editorial_separator_rule` | 1,114 | 61,265 | 557 |
| `editorial_legend_example` | 1,403 | 59,247 | 557 |
| `editorial_legend_heading` | 557 | 28,407 | 557 |
| `bibliographic_header_line` | 1,256 | 20,528 | 557 |
| `editorial_legend_note` | 197 | 17,313 | 197 |

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

### What the unattributed 132,036 bytes are

Every interval split at line boundaries and categorized, so the table is
exhaustive and sums to the residue exactly:

| | bytes | share |
|---|---|---|
| transcriber remarks (`※…`, `＊…`, `●…`) | 56,532 | 42.8% |
| line terminators | 34,464 | 26.1% |
| layout whitespace | 16,470 | 12.5% |
| the file's own dating lines (`2007年4月2日作成`) | 16,125 | 12.2% |
| other prose and unclassified lines | 7,554 | 5.7% |
| Creative Commons licence statements | 615 | 0.5% |
| bare URLs | 276 | 0.2% |

**The previous version of this table was wrong and is corrected here.** It
claimed a 322,389-byte residue of which 77.7% was "title, author and colophon
continuation lines". The residue was 339,621 bytes — the table did not sum to
it — and the bibliographic forms were 24.1% of it, which is what the classifier
in fact took. The error was in the report's own line-level reconstruction, not
in the instrument: the aggregate's `metadata_eligible_bytes` minus
`metadata_attributed_bytes` was 339,621 then and is 257,648 now, and both agree
with the per-work unattributed intervals to the byte. The categorization above
is now derived from those intervals rather than reconstructed alongside them.

**Two rows are structurally unattributable, not merely unclassified.** Line
terminators carry `structural_newline` facts, and that role is deliberately
outside `metadata_attributing_roles` — counting it would make the measure move
when line endings changed. Layout whitespace is the indentation the producers
trim off a continuation before claiming it, because a claim should cover content
and not the space in front of it. Together they are 50,934 bytes, so **the
attribution contract alone caps this measure at 0.9270** no matter how many
classifiers are added. At 0.810653 the instrument now sits **0.116 below its own
ceiling**, not 0.189 below 1.0.

Of what is left, the file's own dating lines are the only remaining typed form;
classifying them would reach 0.8338. Everything else is prose — 42.8%
transcriber remarks and 5.7% other — plus 615 bytes of Creative Commons licence
statements, which are a different act from the distribution notice (they state
terms rather than provenance) and are declined for now rather than folded into
a construct that would then mean two things. Corpus-wide they are 118 lines.

Every classifier so far has moved the measure without moving any work to 1.0,
and the per-work maximum has been static at 0.9494 for two classifiers running.
No amount of further classification reaches the literal the predicate set
currently carries for the body measure.

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
is that the literal currently in the predicate set is not a candidate for either
measure: `:= 1.0` fails 492 of 597 works on the body and all 597 on metadata,
and on the metadata side it is not reachable at all — the attribution contract
caps that measure at 0.9270.

**Not a held-out test of the classifiers.** Both the legend classifier's line
forms and the bibliographic classifier's block bounds were designed against this
same sample under this same seed. The distribution-notice anchors are the
exception: they were measured over all 17,913 works, so their coverage is a
corpus-wide figure rather than a design-population one. The coverage figures are therefore a
description of the design population, not a prediction about unseen works. A
second sample under a different seed would be the honest generalization test and
has not been run. This matters more for the bibliographic block than for the
legend: its terminator set was chosen because every sampled header separates
bibliography from legend with a blank line, and a header that does not would be
carried only by the separator-rule and bracketed-heading terminators, which the
sample never exercised.

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
