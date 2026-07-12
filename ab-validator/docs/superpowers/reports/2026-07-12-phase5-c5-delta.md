# Gate: C5 bare-toggle adoption delta (Phase 5, `ab-aozora` C5 vs C4 dump)

**Date:** 2026-07-12
**Authority:** `.superpowers/sdd/task-9-brief.md`, Phase 5 design spec
(bare-toggle inline containers, Contract 1),
`reports/aat-fidelity/audit-aat-delta.py` (`bare-toggle-adoption` mode),
expectation rebinding `a443af0cd9b372f92b3e03c8d6645997c24b76a1`.
**Verdict:** `PASS`

## Gate definition

Phase 5 (C5) turns on bare-toggle adoption: well-paired same-line
`［＃横組み］…［＃横組み終わり］` / `［＃罫囲み］…［＃罫囲み終わり］` raw
marker pairs become typed `yokogumi` / `keigakomi` inline containers. The
audit proves corpus-wide, fail-closed, that the C5 dump differs from the
C4 dump by exactly those adoptions and nothing else:

1. **Diff grammar** — re-expanding every candidate toggle container back
   to `[raw open, *content, raw close]` (spans recovered verbatim from
   the baseline) must reproduce the C4 document byte-for-byte.
2. **Per-line independence (review P5-4, plan amendment 0d323a72)** — on
   every marker-carrying line, observed adoptions must equal the audit's
   OWN derivation (`classify_tokens` over the C4 baseline's raw markers,
   never the placement report, never the candidate).
3. **Preregistered totals binding** — the corpus totals must equal the
   preregistered expectations.

Exit 0 = PASS; ANY unclassified difference exits 2 (fail-closed).

## Candidate identity

Built on hinoki, detached clean worktree at C5, rev-injected
(`AB_AOZORA_GIT_REV`), identity asserted before any run:

- **commit:** `004deaf548f34a36abbc17d0f7a162df010a6292`
- **`--version` (verbatim):** `ab-aozora 0.6.0 aat-schema 2 facade 0.3.0 wire-schema 3 (git 004deaf548f34a36abbc17d0f7a162df010a6292)`
- **binary sha256:** `00066926b8cb035c2b2a55691cfcb99d26c8450b87d960d058855a69afedd24d`

The same triple is carried by the conformance and perf gate summaries and
by the perf runner JSON's `bins.candidate` block. A superseded first C5
candidate, `1f14e3696d2c993bb77f4e01bee1bfdb480e77fa`, was blocked by this
gate (83 works with jizume-context reflow beyond pure adoption) and fixed
by plan amendment 2 (`004deaf5`: bare-toggle pairing runs
post-block-classification).

## Dumps

- Baseline: `/db/ab-validator/aat-corpus/ab-aozora-phase4-c4-27772b1/aat/ab-aozora`
  (the frozen C4 dump; run-set `content_hash`
  `sha256:a19a3829125dde95a94da0077d5197d5b5645d64b4101dc3fb7c599a41f5ff80`).
- Candidate: `/db/ab-validator/aat-corpus/ab-aozora-phase5-c5-004deaf/aat/ab-aozora`
  (this task; 17,886 files; content hash
  `sha256:f89d36c7bb0d0ce7344840abf3e82538f54b3e515c23ac00c14b1c26bca5a484`).

## Instrument note (expectation rebinding, parser-visible universe)

The originally preregistered totals (adopted 1582 yokogumi / 25 keigakomi,
declined 24, reasons {orphan_open 10, orphan_close 0, reopen_rollback 14,
interleave 0}) were derived by the placement instrument from SOURCE TEXT.
On the first run against the 004deaf dump the audit's per-work checks
(diff grammar + per-line independence) passed for ALL 17,886 works, and
the exit 2 fired only at the totals binding: found 1552 yokogumi, not
1582. Per the standing rule this was escalated, not reconciled locally.

The controller investigation (independently verified, reports-only commit
`a443af0c`; C5 UNCHANGED at `004deaf5`; no rebuild, no re-dump) reconciled
the 70-marker gap to exactly three parser-invisible source regions —
annotation-legend `（例）` lines (44 markers: 17 pairs + all 10
orphan_opens), 底本 tails emitted as `source_note` text (12 markers /
6 pairs), and unparsed-source-gap raw text (14 markers / 7 pairs, all in
`001111_42789`) — and rebound the gate to the parser-visible universe:
**adopted 1552 / 25, declined 14, reasons {orphan_open 0, orphan_close 0,
reopen_rollback 14, interleave 0}** (see
`2026-07-12-bare-toggle-visibility-reconciliation.summary.json`).

## Result (exit 0)

`~/phase5-delta.summary.json` on hinoki, mirrored verbatim into
`2026-07-12-phase5-c5-delta.summary.json` `details`:

```json
{
  "mode": "bare-toggle-adoption",
  "compared": 17886,
  "classes": {
    "identical": 17544,
    "toggle_adopted": 342
  },
  "verdict": "PASS",
  "details": {
    "adopted_yokogumi_pairs": 1552,
    "adopted_keigakomi_pairs": 25,
    "declined_markers": 14,
    "declined_by_reason": {
      "orphan_open": 0,
      "orphan_close": 0,
      "reopen_rollback": 14,
      "interleave": 0
    }
  }
}
```

- **compared = 17886** — full corpus, file sets identical.
- **toggle_adopted = 342** — works carrying ≥ 1 adoption;
  identical = 17886 − 342 = 17544. ✓
- **adopted = 1552 / 25, declined = 14 {0, 0, 14, 0}** — equals the
  rebound parser-visible binding exactly; every per-line observed
  adoption equals the audit's independent baseline derivation. ✓
- Every difference corpus-wide re-expanded byte-exactly to the C4
  baseline (no reflow, no span drift, no newline drift — the failure
  mode of the superseded 1f14e369 candidate is gone).

Independent corroboration from the conversion audit (same dump): the
raw-marker divergence rules shrink exactly where adoption removes raw
nodes (e.g. `U-28` 175,209 → 171,901 occurrences; `A-104` 37,253 →
36,691; `I-02` 3,556,724 → 3,556,598), with every other rule's counts
unchanged from C4 and zero conversion failures.
