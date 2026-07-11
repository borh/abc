# Gate: rotation A delta audit (Phase 3 capability, `ab-aozora` C1 vs stage 0)

**Date:** 2026-07-11
**Authority:** `.superpowers/sdd/task-10-brief.md`, Phase 3 design spec
(three-class delta taxonomy), `reports/aat-fidelity/audit-aat-delta.py`
(`container-rewrite` mode).
**Verdict:** `PASS`

## Gate definition

Rotation A (Tasks 4–9) added wire schema 3, `--mode diagnostics`, and the
keigakomi/yokogumi block classifiers. This gate proves that corpus-wide,
every AAT difference between the stage 0 dump (C0 =
`c3cb16908b0134ca03856735f10445eeb2a92957`) and the rotation A candidate
dump (C1) is explained by the fail-closed three-class taxonomy:

1. identity pointers (`/meta/adapter_version`),
2. works whose baseline carries well-paired keigakomi/yokogumi container
   markers, verified by FORWARD REWRITE + DEEP EQUALITY (an independent
   reimplementation of the classifier grammar over the baseline JSON must
   reproduce the candidate exactly — no invariant-weakening), and
3. everything else byte-identical after identity substitution.

Exit 0 = PASS; ANY unclassified difference exits 2 (fail-closed, no exit 1).

## Candidate identity

Built on hinoki, detached clean worktree at C1, rev-injected
(`AB_AOZORA_GIT_REV`), identity asserted before any run:

- **commit:** `a81edf066ecc0e9ac12e04c4ccc2551c27009161`
- **`--version` (verbatim):** `ab-aozora 0.2.0 aat-schema 1 facade 0.2.0 wire-schema 3 (git a81edf066ecc0e9ac12e04c4ccc2551c27009161)`
- **binary sha256:** `c3cec69c7dd4261bd9068bab1de06666c54584a77c15783ccc17c8cea355e916`

The candidate dump's `metadata.json` (`repo_head`, `repo_dirty: false`,
`adapter_bin_override.{sha256,version}`) independently records the same
triple. The same triple is carried by the conformance and perf gate
summaries and by the perf runner JSON's `bins.candidate` block.

## Dumps

- Baseline: `/db/ab-validator/aat-corpus/ab-aozora-phase3-stage0-c3cb169/aat/ab-aozora`
  (stage 0, Task 3; byte-identical to the Phase 2 dump modulo identity).
- Candidate: `/db/ab-validator/aat-corpus/ab-aozora-phase3-capability-a81edf0/aat/ab-aozora`
  (this task; 17,886 files, `workflow-run.json` `status: passed`, run
  2026-07-11T06:39:26Z–06:42:48Z; retained — Task 17's rotation B baseline).

Both paths point one level past `aat/` at the adapter subdirectory, per the
established layout (`{aat_dir}/{adapter_id}/*.json`).

## Instrument note (auditor grammar fidelity, C1 unchanged)

The first audit run FAILED closed (exit 2) at
`000067_1789-aae6d58f40b1.json`: the auditor's forward rewrite of the
baseline could not reproduce the candidate. Escalation per spec found the
divergence was in the audit instrument's grammar mirror, not in the parser:
two fidelity gaps, fixed in two iterations documented in
`.superpowers/sdd/task-9-report.md` —

1. `010e5340` — mirror the Rust strip ordering through block-assembly
   wrappers (the parser applies `strip_boundary_newlines` to the flat
   inline stream *before* inline wrappers such as `style` are assembled;
   the 5-work strip-ordering pattern, e.g. container-final `"著者\n"` →
   `"著者"` inside a `style` node), plus jisage abort and post-close flag
   fidelity;
2. `b4db35e5` — admit later opens after an unadmitted one (the nested-pair
   `001558` pattern).

Both commits touch `reports/aat-fidelity/` only (instrument-side);
**C1 is unchanged** at `a81edf066ecc0e9ac12e04c4ccc2551c27009161` and the
candidate dump was not regenerated. The passing audit ran with instrument
commit `b4db35e52010ebe7d698c2d4e8fa1e5caefdf267`, recorded as
`instrument_commit` in the gate summary.

## Audit invocation and result

```bash
cd ~/Projects/soranoha/.worktrees/parser-fork-phase3/ab-validator
python3 reports/aat-fidelity/audit-aat-delta.py container-rewrite \
  /db/ab-validator/aat-corpus/ab-aozora-phase3-stage0-c3cb169/aat/ab-aozora \
  /db/ab-validator/aat-corpus/ab-aozora-phase3-capability-a81edf0/aat/ab-aozora \
  --summary-json ~/phase3-capability-delta.json
```

Exit 0. Summary (verbatim; also embedded in
`2026-07-11-phase3-capability-delta.summary.json` `details`):

```json
{
  "mode": "container-rewrite",
  "compared": 17886,
  "classes": {
    "identical": 17723,
    "rewritten": 163,
    "span_confined": 0
  },
  "verdict": "PASS"
}
```

## Class-count review

- `rewritten = 163`: on the order of the keigakomi (106) + yokogumi (88)
  backlog work sets with overlap (106 + 88 = 194; 163 implies ~31 works
  carrying both/overlapping or unpaired markers) — magnitude as expected.
- `identical = 17723`: everything else, including all jizume-only works
  (Task 8's jizume recognition is emission-free at rotation A; any
  jizume-only AAT change would have been an unclassified difference and
  failed the audit closed).
- `span_confined = 0`: rotation-B class, correctly empty in
  `container-rewrite` mode.

## Verdict

**`PASS`** — all 17,886 works classified: 17,723 identical after identity
substitution, 163 exactly reproduced by the independent forward rewrite of
the container grammar, zero unclassified differences.
