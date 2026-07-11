# Gate: C4 append-confinement audit (Phase 4, `ab-aozora` C4 vs C3 dump)

**Date:** 2026-07-12
**Authority:** `.superpowers/sdd/task-15-brief.md`, Phase 4 design spec
(source-note append confinement contract),
`reports/aat-fidelity/audit-aat-delta.py` (`source-note-append` mode,
introduced at `966001ed`).
**Verdict:** `PASS`

## Gate definition

Phase 4 (C4) turns on `source_note` emission for terminal provenance
(底本 tails): the parser now appends `source_note` blocks (placement
`back`, region_class `terminal_provenance`) after the body. This gate
proves corpus-wide that the C4 dump differs from the C3 dump by exactly
that append and nothing else (fail-closed): after stripping
`/meta/adapter_version`, the candidate's block list must carry the
baseline's block list as an exact prefix (zero body drift), every
appended block must be a well-formed terminal-provenance `source_note`
(text-only content, terminator-preserving values, positive spans, block
span anchored to the first content span), and `meta` — including
`warnings` — must be byte-equal.

Exit 0 = PASS; ANY unclassified difference exits 2 (fail-closed, no exit 1).

Binding: `source_note_appended` must equal
`works_with_terminal_provenance` from the Task 12 terminal-provenance /
colophon split instrument — the independent Python measurement of which
corpus texts carry a 底本 tail. The no-tail works must land in
`identical`.

## Candidate identity

Built on hinoki, detached clean worktree at C4, rev-injected
(`AB_AOZORA_GIT_REV`), identity asserted before any run:

- **commit:** `27772b1b75c9ceeb0b724095bbbb47f774f3a275`
- **`--version` (verbatim):** `ab-aozora 0.5.0 aat-schema 2 facade 0.3.0 wire-schema 3 (git 27772b1b75c9ceeb0b724095bbbb47f774f3a275)`
- **binary sha256:** `641b5eaefbaa67f6c157484979d1c9e02db3e1c610d8eccae42895fd670c87c6`

The same triple is carried by the conformance and perf gate summaries and
by the perf runner JSON's `bins.candidate` block.

## Dumps

- Baseline: `/db/ab-validator/aat-corpus/ab-aozora-phase4-c3-c2b9b39/aat/ab-aozora`
  (the frozen C3 dump, Task 10).
- Candidate: `/db/ab-validator/aat-corpus/ab-aozora-phase4-c4-27772b1/aat/ab-aozora`
  (this task; 17,886 files, run 2026-07-12 ~05:30–05:45 JST; retained —
  the activation run-set entry points at it).

## Instrument note (Task 12 split binding, Revision 2)

The audit itself passed on its first run (exit 0, every appended block
well-formed), but its class split — `source_note_appended = 17735`,
`identical = 151` — disagreed with the then-frozen Task 12 split binding
(`works_with_terminal_provenance = 17733`, `works_without_tail = 153`)
by 2 works in each direction. Per the standing rule this was escalated
rather than reconciled locally. A read-only sweep isolated the full delta
to exactly 6 corpus entries, and the controller investigation found the
divergence was in the Task 12 Python scanner, not in the parser or the
confinement audit: `read_entry_text` dispatched zip-vs-plain by filename
extension only, so it (a) read stray/truncated duplicate files instead of
the real corpus entries for 2 work ids, and (b) failed to read 2
`.txt`-named files that are actually valid zip archives with real 底本
tails (plus 2 mirror-case entries its zipfile reader could not open at
all, which the Rust pipeline reads fine and which genuinely have no
tail). The normative `classify_tail` rule itself was zero-diff.

The split evidence was regenerated in place (Revision 2, reports-only
commit `b8930eaa2b9f4f95d60b1eedce907c45dacd4374`; C4 UNCHANGED at
`27772b1b`; no rebuild, no re-dump). The Revision 2 binding is
`works_with_terminal_provenance = 17735`, `works_without_tail = 151` —
exactly what this audit measured.

## Result (exit 0)

`~/phase4-c4-confinement.json` on hinoki, mirrored verbatim into
`2026-07-12-phase4-c4-confinement.summary.json` `details`:

```json
{
  "mode": "source-note-append",
  "compared": 17886,
  "classes": {
    "identical": 151,
    "source_note_appended": 17735
  },
  "verdict": "PASS"
}
```

- **compared = 17886** — full corpus, file sets identical.
- **source_note_appended = 17735** — equals the split binding
  `works_with_terminal_provenance = 17735` (Revision 2). ✓
- **identical = 151** — equals the split binding
  `works_without_tail = 151` (Revision 2). ✓
- Class sum: 17735 + 151 = 17886 = compared. ✓
- Every appended block passed the shape contract (source_note / back /
  terminal_provenance / terminator-preserving text / positive spans /
  anchored block span); `meta.warnings` and all other meta keys byte-equal
  corpus-wide.

Independent corroboration from the conversion audit (same dump): the two
mapping rules newly emitted at C4 (`L-469`, `S-12`) each fire on exactly
17,735 files (17,737 occurrences — a handful of works carry more than one
appended `source_note` block), with every other rule's counts unchanged
from C3.
