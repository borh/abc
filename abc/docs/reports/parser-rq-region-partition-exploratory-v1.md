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
| repository revision | `d81d5409d3085ac7fa7c3e6cee6936c33369edd8` |
| corpus | `aozorabunko` at `0e9ea3e586eb0aa34039fabfc85a407d2f98b165`, resolved through the flake input |
| toolchain | `rustc 1.96.1 (31fca3adb 2026-06-26)` |
| instrument | `parser-rq-source-recognition-v2` |
| work record wire schema | `abc/parser-rq-source-recognition-work/v2` |

Governed documents, by raw file bytes:

| document | sha256 |
|---|---|
| `data/parser-rq-ab-aozora-classified-source-v1.json` | `f16e27e11da484bc8b9fb3afa0b199af44c614c9897f8fdc29c6f43d95f90598` |
| `schemas/parser-rq-classified-source-ledger.schema.json` | `b1139dd9f6dcf11ab998a7ad658c038c5f3a4b3c44deec550bb60564f8ad2731` |
| `schemas/parser-rq-source-recognition-work.schema.json` | `600f7c195e99c8bac5b6102eaadf59d9ff334759dbdaeb7ad1148d4a031c9474` |
| `schemas/parser-rq-source-recognition-aggregate.schema.json` | `b1c43492208185989fe9e028fc2710ce64f2a4a90952072efc847a101209d1f8` |

Run identities, as the instrument emitted them:

| | |
|---|---|
| `policy_hash` | `sha256:3805facf…0b7231f1` |
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
| `hashire_merosu` | 1,163 | 660 | 0.567498 |
| `hatsukoi` | 1,243 | 545 | 0.438455 |
| `kokoro` | 1,412 | 707 | 0.500708 |
| **fold** | **3,818** | **1,912** | **0.500786** |

Two producers contribute: the colophon fields in the tail, and the fenced
editorial legend block in the header. The measure has moved twice, and the two
moves are different in kind.

| | metadata fold | |
|---|---|---|
| colophon producer, first draft | 0.3002 | **superseded, wrong** |
| after the attribution contract | 0.126768 | correction |
| after the legend classifier | **0.500786** | new coverage |

The **0.3002** figure counted two things that are not attribution: the
line-ending normalizations the sanitizer emits across the whole file, which are
true of every line whether or not anything understands the packaging and which
made the number depend on whether a work used CRLF; and header lines matching
the colophon field shape, including the standard notation legend `《》：ルビ`,
which is a `key：value` line by shape and is not publication metadata. Confining
attribution to the tail and to policy-named roles put the colophon's real
contribution at 0.126768.

The move from 0.126768 to **0.500786** is the legend classifier, which is
coverage the instrument did not have before rather than a correction to a
number it already reported.

Body recognition is unchanged at 0.991129 across both moves. That is the check
that the metadata producers stayed inside their own population; it is the one
number that must not move when a metadata producer is added.

### What the remaining 1,809 bytes are

Measured, not estimated, over the same three works:

| | bytes | share |
|---|---|---|
| title, author and colophon continuation lines | 1,320 | 73.0% |
| free-text transcriber remarks (`※…`, `＊…`, bare URLs) | 411 | 22.7% |
| whitespace and line terminators | 78 | 4.3% |

The largest share is a real typed form with no producer yet: `走れメロス`,
`太宰治`, and colophon continuation lines like
`1988（昭和63）年10月25日初版発行` that sit under a field rather than carrying
their own `key：`. Bibliographic attribution is the obvious next classifier.

The transcriber remarks are prose. They are not a typed form and arguably
should never be attributed — claiming them would be claiming to understand a
sentence. If that judgement holds, this instrument's ceiling is below 1.0 by
construction, which is itself something the predicate's owner needs to know
before fixing a threshold.

## Conservation

847,162 + 3,818 = 850,980, equal to the decoded total, with no overlap between
the body and metadata interval sets. The instrument refuses to emit a record
where this fails.

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

No threshold. Three hand-picked works under a synthesized identity cannot
support one, and the metadata instrument still leaves half of packaging bytes
unattributed — most of it a bibliographic form that has no producer yet, and
some of it free prose that may never have one. A threshold fixed here would
encode the current classifier inventory as a permanent governed allowance.
Predeclaration belongs to the predicate's owner, after the instrument measures
what its predicate would claim.

Nor does it establish that the legend classifier generalizes. Its line forms
were designed against a 597-work random sample of the pinned corpus, in which
557 headers carry a fenced legend block and the four classified forms cover
3,561 of 3,631 non-blank block lines; but the *folds* above are three works, and
the sampling script is exploratory scratch work that is not committed. Running
the classifier across that sample and publishing the coverage is the next
measurement worth taking.
