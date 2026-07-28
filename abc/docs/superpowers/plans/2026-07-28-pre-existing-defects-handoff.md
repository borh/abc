# Handoff: two pre-existing defects found on 2026-07-28

**Defect 2 was resolved on 2026-07-28. Defect 1 was partly resolved — the
capture no longer fails, but the work still yields no measurement, and chasing
it surfaced a third and more serious defect recorded below as issue 3.** The investigation notes are kept below as the context the fixes and
their regression tests were built from; each issue's *Resolution* records what
was done. Neither defect was caused by the parser-RQ classifier work on this
branch. Both were found by running that work's verification wider than the
change required — one by a held-out corpus sample, one by a property test that
happened to draw a failing case.

## 1. Capture fails closed on a nested accent bracket

**Where.** `ab-aozora-capture`, the `recovered_verbatim` fact path. Reproduces
at `507fc2e7`, before any of this branch's classifier work.

**Symptom.** `capture_generation_from_bytes_for_identity_and_work` returns

```
duplicate classified-source entry: {"construct_id":"recovered_verbatim", ...
 "start":623357,"end":623409,
 "source_form":"〔George Innes, 1825―1894.〔Albert Biersta`dt〕"}
```

**Cause, as far as it was traced.** An unclosed `〔` whose span contains a
closed `〔...〕`. The accent-decomposition recovery emits two facts with
identical spans, construct and witness, and the ledger builder rejects the
duplicate rather than deduplicating it.

**Why it was not fixed here.** Rejecting a duplicate fact is the designed
behaviour and the right one — silently collapsing two facts into one would hide
exactly the class of defect this ledger exists to expose. The question is which
of the two facts should not have been emitted, and that is a question about the
recovery rule, not about the ledger. Answering it needs someone to decide
whether a nested `〔` opens a new recovery or is content of the outer one.

**Why it matters.** The work yields no capture, no record and no measurement.
A qualification campaign over the full corpus will meet works that produce
nothing at all, and the campaign's own accounting has to say what it does with
them. One work in a 584-work held-out sample; the corpus-wide count is not
known and would be worth a scan before the campaign is designed.

**Resolution, partial.** Accent reconciliation now treats a nested tortoise
delimiter as content of the sanitizer-owned outer recovery span. The ledger
continues to reject genuine duplicate facts, and a capture-generation
regression covers the reported source form. **Capture succeeds; the work is
still not measurable.** Re-run through the real recognition path it now returns

```
status: unavailable, errors: ["ledger-evidence-invalid"]
```

because the accent normalization proof over that span does not round-trip. The
failure moved one stage later rather than closing. What it exposed is issue 3,
which is the reason it does not round-trip and is worth more than this work
is.

**To reproduce.** The work is the sole `.txt` of the archive whose sample
`work_id` is `9d75d3d65ed3308c`, in the `random.seed(20260728)` draw described
in `docs/reports/parser-rq-region-partition-exploratory-v1.md`. Feed its raw
Shift-JIS bytes to the capture entry point.

## 2. Incremental parse diverges from a full parse

**Where.** `ab-aozora-facade`, `incremental::oracle_proptests::
pieceseq_multi_edit_matches_full_parse`, asserting at
`crates/ab-aozora-facade/src/incremental.rs:2347`.

**Symptom.** `assertion left == right failed: source_nodes length; left: 0,
right: 1` — an incremental multi-edit reparse produces no source node where a
full parse of the same final text produces one.

**Shrunk counterexample**, as proptest saved it:

```
doc = "《ん。｜漢字《かんじ》\n\n※［＃ばける、第3水準1-15-94］\n\n｜漢字《かんじ》》"
edits = [(220, 0, 1)]
```

**Status when found.** The seed was written to
`crates/ab-aozora-facade/proptest-regressions/incremental.txt` by the failing
run and then reverted, so the suite passed again by not drawing that case.

**Why it matters.** It is a correctness divergence between the incremental and
full parse paths, which is the one property incremental parsing exists to
preserve. A randomized test found it, which means it is reachable, and the
saved seed means it need not be found again.

**Resolution.** `NestedRuby` is now classified as a whole-document-scoped
diagnostic by `is_whole_document_scoped`, so the unsafe local splice is
declined and the caller falls back to a full parse. The shrunk edit is pinned
as `pieceseq_stray_ruby_close_edit_matches_full_parse` — a named deterministic
test rather than a proptest seed, so it runs on every invocation instead of
whenever the generator happens to redraw it.

## 3. Accent decomposition is applied to prose that is not accent-decomposed

**Found 2026-07-28**, while diagnosing why issue 1's work still failed. Not
previously recorded anywhere.

**Symptom.** A `〔...〕` span is treated as accent-decomposed Western text and
the accent mappings are applied to all of it, including ordinary punctuation.
Two normalization proofs from one work:

```
〔George Innes, 1825―1894.〔Albert Biersta`dt〕
  -> 〔George Inneş 1825―1894.〔Albert Bierstàdt〕

〔... Pease porridge hot,\nPease porridge cold, ...〕
  -> 〔... Pease porridge hoţ\nPease porridge cold, ...〕
```

`Innes, ` becomes `Inneş` and `hot,` becomes `hoţ`: the mapping `s,`→`ş` and
`t,`→`ţ` consumed a comma that was sentence punctuation, and in the first case
a following space with it. The `a\`` → `à` in the same span is a genuine accent
decomposition, which is why the span was entered at all.

**Why this is worse than it looks.** The proof is recorded with
`inverse_rule: "accent_decomposition"` under the `lossless_normalization`
disposition — a claim that the transformation loses nothing. It loses a comma.
And the round-trip check passes, because `decompose_accent` on the recognition
side reproduces the same mapping: both sides make the identical mistake, so the
check that exists to catch exactly this cannot fire.

**Scope, measured.** Zero of the 597 works of the design sample. **Four of the
584 works of the held-out sample.** Three of those four return `status: ok` —
the corrupting normalization is accepted and published. The fourth is issue
1's work, which fails for the additional nested-bracket reason.

Corpus-wide the count is not known; it is worth a scan, since the design sample
contains none and the held-out sample contains four.

**What the fix probably is, and why it was not taken here.** The span entered
accent recovery because it contains one real accent sequence. The mappings are
then applied to the whole span rather than to the sequences that motivated it.
Deciding whether accent decomposition applies per-span or per-sequence is a
question about the recovery rule and the notation it models, not a question
about the ledger, and it should be answered by whoever owns
`accent_mappings` — the same call issue 1 needs.
