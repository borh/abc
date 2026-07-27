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
| repository revision | `4d7e5b5c2f437027ab3c75aa98d63da12d6e4c4f` |
| corpus | `aozorabunko` at `0e9ea3e586eb0aa34039fabfc85a407d2f98b165`, resolved through the flake input |
| toolchain | `rustc 1.96.1 (31fca3adb 2026-06-26)` |
| instrument | `parser-rq-source-recognition-v2` |
| work record wire schema | `abc/parser-rq-source-recognition-work/v2` |

Governed documents, by raw file bytes:

| document | sha256 |
|---|---|
| `data/parser-rq-ab-aozora-classified-source-v1.json` | `4d7728c8cbdd2822400681e1841d39c548fba84dbad59149c802c19081eb5da0` |
| `schemas/parser-rq-source-recognition-work.schema.json` | `600f7c195e99c8bac5b6102eaadf59d9ff334759dbdaeb7ad1148d4a031c9474` |
| `schemas/parser-rq-source-recognition-aggregate.schema.json` | `b1c43492208185989fe9e028fc2710ce64f2a4a90952072efc847a101209d1f8` |

Run identities, as the instrument emitted them:

| | |
|---|---|
| `policy_hash` | `sha256:69eefe9f…a227e5b1` |
| `qualification_identity_ref` | `sha256:228669fe…1eae1365` (**synthesized**) |
| `membership_ref` | `sha256:e5268dea…ccb0962c` |
| `corpus_generation_ref` | `sha256:26d41797…3fe9d57e` |

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
| `hashire_merosu` | 1,163 | 211 | 0.181427 |
| `hatsukoi` | 1,243 | 123 | 0.098954 |
| `kokoro` | 1,412 | 150 | 0.106232 |
| **fold** | **3,818** | **484** | **0.126768** |

**This supersedes the 0.3002 recorded during Q15 task 5.** That figure counted
two things that are not attribution:

- the line-ending normalizations the sanitizer emits across the whole file,
  which are true of every line whether or not anything understands the
  packaging, and which made the number depend on whether a work used CRLF; and
- header lines that matched the colophon field shape, including the standard
  notation legend `《》：ルビ`, which is a `key：value` line by shape and is not
  publication metadata.

Attribution is now confined to the tail and to policy-named roles, so 0.126768
is what the colophon alone accounts for. The drop is a correction, not a
regression.

Body recognition is unchanged at 0.991129 across that correction, which is the
check that the metadata producer stayed inside its own population.

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
support one, and the metadata instrument still leaves roughly seven eighths of
packaging bytes unclassified because the editorial legend block and the
separator rules have no classifier. A threshold fixed against this instrument
would encode "colophon fields are classified and nothing else is" as a permanent
governed allowance. Predeclaration belongs to the predicate's owner, after the
instrument measures what its predicate would claim.
