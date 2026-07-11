# Gate: C3 migration delta audit (Phase 4, `ab-aozora` C3 vs Phase 3 span dump)

**Date:** 2026-07-12
**Authority:** `.superpowers/sdd/task-10-brief.md`, Phase 4 design spec
(v2-migration delta taxonomy), `reports/aat-fidelity/audit-aat-delta.py`
(`v2-migration` mode).
**Verdict:** `PASS`

## Gate definition

Phase 4 (C3) moves the AAT document to schema version 2: mechanical layout-key
renames (`x-indent` → `indent`, etc.), the compound jizume rewrite, and the
left-ruby structural rewrite. This gate proves that corpus-wide, every AAT
difference between the Phase 3 span-rotation dump and the C3 candidate dump is
exactly the v2-migration grammar's forward rewrite of the baseline (FORWARD
REWRITE + DEEP EQUALITY, fail-closed): a work lands in `migrated` at minimum
(every document changes mechanically — root version bump), plus
`jizume_rewritten` and/or `ruby_left_rewritten` where those features fire.

Exit 0 = PASS; ANY unclassified difference exits 2 (fail-closed, no exit 1).

Note (per plan): at C3, `jizume_rewritten` counts ONLY compound wraps —
standalone `jizume_block` emission was removed by `4190c26b` (the §6.6
line-width-container form stays raw), so the 3,239-marker matrix reference
from earlier planning no longer applies to this class.

## Candidate identity

Built on hinoki, detached clean worktree at C3, rev-injected
(`AB_AOZORA_GIT_REV`), identity asserted before any run:

- **commit:** `c2b9b396271755bc0b898fdaed17c9c9cbe4666d`
- **`--version` (verbatim):** `ab-aozora 0.4.0 aat-schema 2 facade 0.3.0 wire-schema 3 (git c2b9b396271755bc0b898fdaed17c9c9cbe4666d)`
- **binary sha256:** `008790f777b05e00692d2e740ec1d2e7736830c7cdefa0661cf13a050d761e4d`

The same triple is carried by the conformance and perf gate summaries and by
the perf runner JSON's `bins.candidate` block.

## Dumps

- Baseline: `/db/ab-validator/aat-corpus/ab-aozora-phase3-span-a3f91f5/aat/ab-aozora`
  (Phase 3 span rotation, the frozen Phase 3 baseline).
- Candidate: `/db/ab-validator/aat-corpus/ab-aozora-phase4-c3-c2b9b39/aat/ab-aozora`
  (this task; 17,886 files, run 2026-07-12 03:49–04:00 JST; retained —
  Task 15's baseline).

Both paths point one level past `aat/` at the adapter subdirectory, per the
established layout (`{aat_dir}/{adapter_id}/*.json`).

## Instrument note (auditor grammar fidelity, C3 unchanged)

The first audit run at C3 FAILED closed (exit 2) at
`000933_47550-70e0b2b8bbb0.json`: the auditor's forward rewrite of the
baseline could not reproduce the candidate. A read-only evidence sweep
isolated the failure to exactly 3 of 17,886 works
(`000933_47550-70e0b2b8bbb0`, `001395_49891-5d6f782302d9`,
`001395_49905-45ce8e5a2e75`), all in the left-ruby rewrite area (no jizume
involvement; candidate emitted more `ruby` nodes than the auditor's grammar
predicted: 359→367, 942→952, 1347→1363). Escalation per spec found the
divergence was in the audit instrument's grammar mirror, not in the parser
(parser CORRECT in all three cases): two fidelity gaps, fixed in the
reports-only commit `22030d58ee0d31b59515151a31f0d1e5c24aa809` —

1. gaiji-base left-ruby: a left-ruby BASE that is or contains an embedded
   gaiji reference `※［＃「…」、…］` requires the structural parse the parser
   applies, not the literal-text treatment the mirror used;
2. chitsuki left-ruby: line re-merge ordering around chitsuki blocks.

C3 was NOT moved (the fix touched only the instrument). The audit was then
re-run in full against the same dumps.

## Result (final run, exit 0)

`~/phase4-c3-delta.json` on hinoki, mirrored verbatim into
`2026-07-12-phase4-c3-delta.summary.json` `details`:

```json
{
  "mode": "v2-migration",
  "compared": 17886,
  "classes": {
    "migrated": 17864,
    "jizume_rewritten": 4,
    "ruby_left_rewritten": 18
  },
  "verdict": "PASS",
  "details": {
    "both": 0,
    "compound_jizume_adopted": 6
  }
}
```

- **compared = 17886** — full corpus, file sets identical.
- **migrated = 17864** — mechanical migration only.
- **jizume_rewritten = 4** — compound-wrap works only (see note above); the
  6 `compound_jizume_adopted` markers sit within these 4 works (a work can
  adopt more than one compound).
- **ruby_left_rewritten = 18** — first corpus-wide left-ruby measurement at
  v2 (includes the 3 works the pre-`22030d58` mirror could not classify).
- **both = 0** — no work needed both feature rewrites.
- Class sum: 17864 + 4 + 18 = 17886 = compared. ✓
