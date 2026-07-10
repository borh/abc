# Gate B: fork conformance parity (127-vector suite + 30-vector official-docs seed)

**Date:** 2026-07-10
**Authority:** ADR 0031 (hard detach), `.superpowers/sdd/task-7-brief.md`,
`docs/handoffs/2026-07-10-parser-fork-provenance.md`.
**Verdict:** `FORK_CONFORMANCE_PARITY_CONFIRMED`

## What was compared

The pinned upstream `aozora` CLI vs the fork shim `ab-aozora-cli`
(3+1-kind `inspect` dispatch over the lifted `ab-aozora-facade`), run
through the shared conformance harness
(`reports/parser-conformance/run-aozora-notation-spec.py` via the
justfile recipe `aozora-notation-spec-comparison`) on **both
instruments**:

1. the 127-vector P4suta notation-spec suite
   (`nix build .#upstream-aozora-notation-spec` → `conformance/vectors`), and
2. the 30-vector official-docs seed
   (`reports/parser-conformance/official-docs-seed`).

Each run scores all six adapters; the aozora binary feeds **two** of
them — `aozora` (inspect mode, binary invoked directly) and `ab-aozora`
(AAT mode, binary invoked by `adapters/aozora` via `AB_AOZORA_BIN`).
The other four adapters (aozora2, aozora-rs, aozora2html, aozora-epub3)
are fixed across legs and act as in-run controls.

## Binary identities

| leg | binary | identity |
| --- | --- | --- |
| baseline | pinned upstream store binary | `/nix/store/kps3xj67p1lvjvn55k62z2p2bsifjy8b-upstream-parser-aozora-0.1.0/bin/aozora`, sha256 `ae2dffa3b3cc26f65042000407de4e553d265706e53f99355365c67aed9bdec7`, `--version` → `aozora 0.4.1` |
| shim | fork shim | `target/release/ab-aozora-cli` built from the shim sources at git rev `1fa0316cbfc6d6aa5b03b534d96d932d2e7674c7` (branch `feat/parser-fork-phase1`; the shim-fix commit below), sha256 `bf6493fee3f183c7bf809c0f6301cf8e25733996ee359c3a29bc710d125620ab`, `--version` → `ab-aozora-cli 0.1.0 (fork of P4suta/aozora @ 1a4f864, ADR 0031)` |

Upstream detach point (unchanged): P4suta/aozora @
`1a4f864603970983719655aa4af4525958ac2d38`.

## How the binary was controlled in each leg (both instruments)

Control is **explicit and parameterized in both legs — no ambient env**:
the recipe's `AOZORA_BIN` parameter (Task 6) substitutes the binary path
directly into the two adapter command lines
(`--adapter "aozora=inspect:<path> inspect"` and
`--adapter "ab-aozora=aat:env AB_AOZORA_BIN=<path> …aozora-adapter --mode aat"`).
Verified two ways:

1. `just --dry-run` capture of each seed invocation shows
   `aozora_bin="/…/ab-validator/target/release/ab-aozora-cli"` (shim leg)
   vs the `nix build …#upstream-parser-aozora` + `$aozora_pkg/bin/aozora`
   resolution (baseline leg, no override).
2. Positive attribution from output: an earlier shim leg (run before the
   `pairs` fix below) produced 124 `aozora`-adapter rows carrying the
   string `pairs: usage: ab-aozora-cli inspect {nodes|diagnostics|gaiji} - | --version`
   — a string that exists only in the shim binary. The instrument
   demonstrably executes the binary passed via `AOZORA_BIN` and is
   sensitive to its behavior on the inspect channel.

## Shim defect found and fixed (the Gate working as intended)

The first shim run on the 127-suite was **not** identical: the harness
probes `inspect pairs` on inspect adapters, upstream `aozora` supports
`InspectKind::Pairs => json::pairs(&tree)`, but the Task 5 shim
dispatched only `nodes|diagnostics|gaiji` and exited 64 with a usage
message. Statuses and totals were unchanged (both legs warned on the
`pairs` channel), but 124 `aozora`-adapter rows differed in warning
text:

```
<                 "pairs: unsupported inspect envelope",
---
>                 "pairs: usage: ab-aozora-cli inspect {nodes|diagnostics|gaiji} -  |  --version",
```

Per the brief ("any scoring diff = lift/shim defect; return to Task
3/5, fix, re-run"), the shim gained the `pairs` arm
(`json::pairs(&Document::new(source).parse())` — the exact upstream
mapping), the golden test matrix was extended to 4 kinds x 3 samples
(all 12 cells byte-identical to the pinned upstream binary), and the
shim leg was re-run from scratch. Commit: `1fa0316c`
`fix(parser): add inspect pairs to the ab-aozora-cli shim`.

## Results

### Instrument 1: 127-vector P4suta suite (127 vectors x 6 adapters = 762 rows)

- baseline: `2026-07-10-fork-parity-conformance.baseline.summary.json`
  sha256 `2b50378eb6fd4522b769807cc05fd520673543958ea211f1402df0488e16daba`
- shim: `2026-07-10-fork-parity-conformance.shim.summary.json`
  sha256 `2b50378eb6fd4522b769807cc05fd520673543958ea211f1402df0488e16daba`

```
$ diff <(python3 -m json.tool ….baseline.summary.json) <(python3 -m json.tool ….shim.summary.json)
$ echo $?
0
$ cmp ….baseline.summary.json ….shim.summary.json && echo BYTE-IDENTICAL
BYTE-IDENTICAL
```

The diff output is **empty** — not even identity-only keys differ (the
summary schema records no binary paths/versions; `vectors_dir` is the
same store path in both legs). The narrative `.md` reports were also
byte-identical between legs and were deleted as redundant per the brief
(summary.json evidence retained).

Totals (identical both legs): 762 rows — 231 pass / 416 warning /
90 fail / 25 skip. Aozora-fed rows: `aozora` (inspect) 102 warning +
25 fail; `ab-aozora` (AAT) 113 pass + 9 warning + 5 skip.

### Instrument 2: 30-vector official-docs seed (30 x 6 = 180 rows)

- baseline: `2026-07-10-fork-parity-official-seed.baseline.summary.json`
  sha256 `268c03fdc24601e1bd72f2e0beacb7b119e33597fec6351413c71949a22f6e9e`
- shim: `2026-07-10-fork-parity-official-seed.shim.summary.json`
  sha256 `268c03fdc24601e1bd72f2e0beacb7b119e33597fec6351413c71949a22f6e9e`

Same procedure, same result: `diff` empty (exit 0), files
byte-identical. Totals (identical both legs): 180 rows — 84 pass /
80 warning / 16 fail / 0 skip. Aozora-fed rows: `aozora` (inspect)
22 warning + 8 fail; `ab-aozora` (AAT) 22 pass + 8 warning.

## Caveats

- **Inspect-channel envelope drift (identical in both legs, pre-existing):**
  the harness accepts only `schemaVersion == 1` inspect envelopes;
  the pinned upstream binary (0.4.1) emits `schemaVersion: 2`, so most
  `aozora`-adapter nodes/pairs/diagnostics comparisons degrade to
  identical "unsupported inspect envelope" warnings in BOTH legs
  (the shim reproduces the v2 envelopes byte-for-byte — golden tests +
  direct spot-check `inspect pairs` output identical). This reduces the
  inspect channel's discriminating power but does not weaken the parity
  claim: (a) the `ab-aozora` AAT channel drives the same binary and
  scores real kind-sequence content (113/9/5 and 22/8 splits, identical
  both legs), and (b) the pre-fix run proved the inspect channel still
  detects binary behavioral differences (it caught the missing `pairs`
  kind). Harness-vs-binary schema drift is pre-existing on `main`
  (the 2026-07-08 report shows the same warnings) and out of Phase 1
  scope.
- **No adapter failed to resolve**: all six adapters (including
  aozora-epub3 via its nix-built jar) resolved and ran in both legs of
  both instruments, so no identically-failing-adapter caveat is needed.
- Absolute totals differ from the dated 2026-07-08 report (different
  harness/adapters state on this branch); irrelevant to this gate,
  which compares two same-day legs under identical conditions.

## Verdict

Byte-identical scoring on both instruments, with the aozora binary as
the only variable between legs and positive attribution that the
intended binary ran in each leg:

**`FORK_CONFORMANCE_PARITY_CONFIRMED`**

Evidence files (kept):
- `docs/superpowers/reports/2026-07-10-fork-parity-conformance.baseline.summary.json`
- `docs/superpowers/reports/2026-07-10-fork-parity-conformance.shim.summary.json`
- `docs/superpowers/reports/2026-07-10-fork-parity-official-seed.baseline.summary.json`
- `docs/superpowers/reports/2026-07-10-fork-parity-official-seed.shim.summary.json`
