# Handoff: two pre-existing defects found on 2026-07-28

**Status 2026-07-30: issues 1 and 2 are fully resolved; issue 3's accounting
layer is resolved and its text-corruption layer is in progress.** The
diagnosis that unlocked issue 1 is recorded in
`2026-07-30-accent-exact-accounting.md`: the sanitize stage's accent rewrite
recorded ONE whole-span offset-map edit per rewritten `〔…〕`, which collapsed
every classifier fact inside the span onto the whole bracketed range — two
same-typed constructs became byte-identical ledger entries and the ledger
failed closed, which was the entire duplicate class (23 works corpus-wide).
The rewrite now records one edit and one diagnostic per digraph substitution
site, the whole-span reconciliation workaround is deleted, accent
normalization proofs are per-digraph (`s,` → `ş` at its own offsets, so a
proof can never span a line ending), and the recognition instrument is
`parser-rq-source-recognition-v4`: an `accent_decomposition` proof must be
exactly one row of the policy's `accent_mappings`. Re-run corpus-wide, all
23 duplicate-failure works and all 3 multi-line-span works capture and
validate with zero failing entries; only the 3 `lossy source decoding`
works remain refused, which is the designed fail-closed on undecodable
bytes.

The investigation notes are kept below as the context the fixes and
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
them. One work in a 584-work held-out sample; the corpus-wide count is 26 —
see *Corpus-wide scope* below.

**Resolution (2026-07-30, superseding the partial one).** The root cause was
not in the recovery rule at all: the sanitize offset map's whole-span accent
edit collapsed every fact inside a rewritten span onto the span start, and
"two facts with identical spans" was that collapse, not a double emission.
With per-site offset-map edits the collapse is gone, the interim
nested-tortoise reconciliation (`916d422f`) is deleted as moot, and this work
captures and validates end to end — the `ledger-evidence-invalid` it still
returned after the interim fix was the whole-span proof conflating the accent
rewrite with CRLF normalization, which per-digraph proofs end by
construction. See `2026-07-30-accent-exact-accounting.md`.

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

Corpus-wide the count is 73 works, a lower bound — see *Corpus-wide scope*
below.

**What the fix probably is, and why it was not taken here.** The span entered
accent recovery because it contains one real accent sequence. The mappings are
then applied to the whole span rather than to the sequences that motivated it.
Deciding whether accent decomposition applies per-span or per-sequence is a
question about the recovery rule and the notation it models, not a question
about the ledger, and it should be answered by whoever owns
`accent_mappings` — the same call issue 1 needs.

## Corpus-wide scope, measured 2026-07-30

The scan both issues asked for was run over every `cards/` archive of the
pinned checkout — 17,887 archives, the name-sorted first `.txt` member of
each, the same approximation and population as
`docs/reports/parser-rq-region-partition-exploratory-v1.md`. The probe is the
same scratch binary that produced the 0/597 and 4/584 sample figures, built
against the tree at `3c929a09` (issue 1's nested-bracket fix included). Five
archives carry no text member and four are unreadable zips; 17,878 works were
attempted. Exploratory, not a governance input; the scan scripts live in
session scratch and are not committed.

**Issue 1 generalizes: 23 works fail capture on a duplicate classified-source
entry, and every one involves a construct nested inside a `〔…〕` accent
span.** The inner construct is emitted twice — once by the normal path, once
under the accent recovery — and the ledger fails closed on the duplicate,
which remains the right behaviour. By duplicated construct: ruby 9, gaiji 5,
plain text 2, emphasis 1, tate-chū-yoko 1, plus 5 `recovered_verbatim`
duplicates where the inner construct is itself ruby or another bracket
(`〔ve'rite'《ヴェリテ》 vraie《ヴレイ》.〕`). The nested-tortoise fix covered
exactly the nested-`〔` shape — all 4 such works corpus-wide now capture — but
the general shape is *any* construct inside the span, and that still fails.
Separately, 3 works fail `lossy source decoding`, a different and designed
fail-closed path. **Total: 26 of 17,878 works produce no record.**

**Issue 3: 73 works publish corrupted accent normalizations.** 504 works
carry `accent_decomposition` proofs; the comma-loss criterion used on the
held-out sample flags 100 of them, 263 proofs. Aligning each proof's source
and normalized forms and classifying every lost comma by what follows it
separates the two populations cleanly: **73 works (203 sites) lose a comma
that is sentence punctuation** (`Paris, Les Presses` → `Pariş`,
`Der Geist des Films, 1930` → `Der Geist des Filmş`), and 27 works lose only
commas that are genuine cedilla notation (`Franc,ois` → `François`,
`garc,on` → `garçon`) — correct decompositions the criterion cannot tell
apart without alignment. All 73 capture successfully, so the corrupting
proofs publish under `lossless_normalization`. 73 is a lower bound: the
criterion only counts commas, and the flagged proofs show a second shape it
misses — an élision apostrophe consumed as an acute accent,
`〔L'art, mes enfents` → `〔Ĺarţ mes enfentş` — which no comma-based count
reaches.

For campaign accounting: 26 works (0.15% of 17,878) yield no record at all,
and 73 works (0.41%) yield a published normalization that loses prose
characters. Both classes trace to the same per-span accent recovery, and both
end at the same owner's decision.
