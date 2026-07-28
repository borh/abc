# Handoff: two pre-existing defects found on 2026-07-28

**Both defects were resolved on 2026-07-28, after this note was first
written.** The investigation notes are kept below as the context the fixes and
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

**Resolution.** Accent reconciliation now treats a nested tortoise delimiter
as content of the sanitizer-owned outer recovery span. The ledger continues to
reject genuine duplicate facts. A capture-generation regression uses the
reported source form and verifies the resulting generation.

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
