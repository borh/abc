# Plan: resolve the accent-recovery defects (2026-07-30)

Resolves the three issues of `2026-07-28-pre-existing-defects-handoff.md`, whose
corpus-wide scope was measured earlier today. Authorized by the repository
owner's instruction to proceed; the per-span/per-sequence decision deferred to
the `accent_mappings` owner in that handoff is taken here, from evidence.

## What the diagnosis added today

Three mechanisms, all in the sanitize stage's accent rewrite, none in the
ledger:

1. **The offset map lies about accent spans.**
   `rewrite_accent_spans_collecting_core` records ONE whole-span `MapEdit` per
   rewritten `〔…〕`, so `OffsetMap::to_source_offset` collapses every byte
   inside the span to the span start. Every classified-source fact wholly
   inside — ruby, gaiji, plain text — rebases to the identical whole-span
   decoded range. Two same-typed constructs inside one span become
   byte-identical ledger entries and the ledger fails closed (correctly).
   That is the entire duplicate class: 23 works. `decompose_fragment_edits`
   exists precisely to record per-digraph deltas; the sanitize step ignores it.
   `reconcile_accent_edit_facts` (and the nested-tortoise special case added
   at `916d422f`) is a workaround for this collapse, not a fix.

2. **Whole-span proofs conflate two normalizations.** The accent
   normalization proof's `source_form` is the decoded slice (CRLF intact);
   `normalized_form` is sanitized text (LF). Recognition's `decompose_accent`
   applies only accent mappings, so any accent span containing a line break in
   a CRLF work fails round-trip → `ledger-evidence-invalid`. 3 works
   corpus-wide (4363, 45210, 55990) — including the nested-bracket work, which
   is why the `916d422f` fix still left it unmeasurable.

3. **`decompose_fragment` is unguarded**, so prose punctuation inside `〔…〕`
   is consumed (`Films,` → `Filmş`; `L'art` → `Ĺart`). 5,031 substitution
   sites corpus-wide; the comma-loss criterion alone flags 73 works of prose
   corruption, and the `'`/`:` markers corrupt more (élision, prose colons)
   that no comma count reaches.

**The reference behaviour is per-sequence.** Aozora's own published XHTML for
the same works composes `Bala'zs`→Balázs, `a\``→à, `e^`→ê, `i:`→ï, `c,`→ç as
gaiji images while leaving `Innes,`, `hot,`, `L'art` literal *in the same
spans*. The 〔…〕+digraph text notation is a lossy projection of a master that
knows which sequences are notation; a whole-span greedy rewrite is not what
the archive itself does. The paired HTML is a per-site oracle (~5k labeled
sites) for validating any guard.

One important correction to the handoff: the proof layer's losslessness claim
is true in the byte sense — CP932 cannot encode any accent-table target
character, so the inverse map (ş→`s,`) is total over decoded sources. The
corruption is in the reader-visible emitted text, not in invertibility.

## Step A — exact accounting (this changes no emitted text)

- `accent.rs`: new `decompose_fragment_sites` returning every substitution
  (offset, in_len, out_len), including length-preserving ones;
  `decompose_fragment_edits` becomes its length-changing filter.
- `sanitize.rs`: per-substitution `accent_decomposition_applied` diagnostics
  (span = the replacement character, output coords) and per-substitution
  `MapEdit`s. No whole-span edit: unedited bytes inside a span now translate
  exactly.
- `classified_source.rs`: delete `reconcile_accent_edit_facts`. With exact
  rebasing the inner facts are correct as emitted; rewritten spans become
  uniform with never-rewritten spans (delimiters stay individual 3-byte
  `recovered_verbatim` facts, interior stays typed/plain). `sanitizer_entries`
  unchanged in code — the per-digraph diagnostics make it emit per-digraph
  proofs (`s,`→`ş`), which cannot span a line ending.
- `recognition.rs`: the `accent_decomposition` round-trip becomes: the proof
  pair is exactly one row of the policy's `accent_mappings`. Stronger than the
  old re-derivation, and guard-independent. Instrument semantics change →
  `parser-rq-source-recognition-v4`, work/aggregate schema consts, ABC
  instrument-version string, fixtures, qualification-identity refs.

## Step B — guard the decomposition (this changes emitted text)

Per-digraph context guard in `try_match`, derived from and validated against
the XHTML oracle; fail toward leaving source bytes verbatim. Known from
site classification: `,` composes only before an ASCII letter; `'` on
consonant bases collides with élision; `:` collides with prose colons and is
sometimes locally undecidable (`Hai:kai:` vs `Lotze:`) — the oracle decides
what rule earns its residual. Recognition needs no further change: per-digraph
proofs verify against the mapping table regardless of which sites fire.

## Verification

Per-crate tests first, then: full `cargo test --workspace`, clippy, fmt,
Clojure suite, fixture regenerations, and the scratch corpus probes re-run —
the 26 capture failures and 3 round-trip failures must all become measurable,
with zero new failures, before Step B lands; after Step B, guard agreement
with the oracle is reported corpus-wide in the handoff.
