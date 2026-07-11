# Phase 3 rotation B — span-confinement gate (PASS)

Stage: `rotation-b` · Gate: `confinement` · Verdict: **PASS**
Machine summary: `2026-07-11-phase3-span-confinement.summary.json`

## Candidate identity (C2, shared by all three rotation-B gate summaries)

- commit: `a3f91f53fcae9bc18f577ea5b746f3be7f228fcb`
- bin_sha256: `9a94dcc8daf9c38a74611667f316e0e024a999d9f41ec41a3721d493dae277dc`
- version: `ab-aozora 0.3.0 aat-schema 1 facade 0.2.0 wire-schema 3 (git a3f91f53fcae9bc18f577ea5b746f3be7f228fcb)`

Identity cross-verified byte-for-byte across the hinoki build assertion
(`--version` + `sha256sum`), the corpus dump's
`metadata.json.adapter_bin_override`, and the perf runner's
`bins.candidate` block.

## Escalation history: C2 moved once (5afe01c9 → a3f91f53)

The first rotation-B candidate (`5afe01c9fc871e140cea04487307deb35aabe40b`,
ledger "Phase 3 C2" after Task 16) FAILED this gate: the auditor's
line-synthesis tripline fired (exit 2) on
`000081_454-3eaff4172fd3.json` (181 spans, all `line 1/1`), with a
read-only full-corpus scan finding exactly one more trip,
`000081_460-f98b1e3acad2.json` (503 spans), and zero non-span or
invalid-span differences anywhere else in the 17886 works.

Root cause (parser bug, not an instrument bug): `line_starts()` in
`crates/ab-aozora-aat/src/lib.rs` indexed only literal `\n` over the
decoded text. Both trip works are classic-Mac bare-CR sources (e.g. card
000081's `dokumomi_rubi.txt`: 90 CR, 0 LF — source bytes sha256-matched
to `meta.source_hash` before diagnosis), so the line index degenerated to
`[0]` and every span got synthesized `line 1/1` — the exact ADR 0024
violation the tripline exists to catch.

Fix: commit `a3f91f53` (`fix(ab-aozora-aat): line index honors bare-CR
and CRLF terminators (ADR 0024)`, `crates/ab-aozora-aat/src/lib.rs` only,
+86/−6) — line boundaries are now `\n`, `\r\n` (one boundary after the
pair), and bare `\r`; the boundary set is proven byte-identical to the
old index for any input without bare CR (goldens and the byte-exact
tripwire pass without regeneration). Full derivation in
`.superpowers/sdd/task-14-report.md`, "Fix appendix (2026-07-11): line
index honors bare-CR and CRLF terminators".

Per the candidate rule this MOVED C2; the whole rotation-B stage was
re-run from the detached build step at the new C2. The superseded first
dump (`ab-aozora-phase3-span-5afe01c`) was referenced by no committed
evidence and was deleted after the replacement run's file count checked
out.

## Corpus run

Full-corpus run on hinoki at C2 (detached checkout, clean tree,
`AB_AOZORA_GIT_REV` injected): **17886** files into
`/db/ab-validator/aat-corpus/ab-aozora-phase3-span-a3f91f5/aat/ab-aozora`
(retained — expected Phase 4 baseline). `workflow-run.json` status
`passed`; the usual four pre-existing malformed zip/txt corpus warnings,
unrelated.

## Audit

```
python3 reports/aat-fidelity/audit-aat-delta.py span-confinement \
  /db/ab-validator/aat-corpus/ab-aozora-phase3-capability-a81edf0/aat/ab-aozora \
  /db/ab-validator/aat-corpus/ab-aozora-phase3-span-a3f91f5/aat/ab-aozora \
  --summary-json ~/phase3-span-confinement.json
```

Result (exit 0; summary written only on full PASS by design):

```json
{"mode": "span-confinement", "compared": 17886,
 "classes": {"identical": 0, "rewritten": 0, "span_confined": 17886},
 "verdict": "PASS"}
```

All 17886 works are span-confined: after masking span objects (and
warning line values, and the deleted legacy synthesized-span warning),
every candidate document is deeply equal to its rotation-A baseline —
zero non-span leaks — and every candidate span satisfies the AAT field
invariants (`byte_end >= byte_start`, `line_end >= line_start >= 1`).
`identical: 0` is expected: every work's spans moved from
sanitized-body-relative offsets with synthesized `line 1/1` to
decoded-source offsets with real lines, so no file is byte-identical to
rotation A. The tripline fired nowhere; the two formerly-degenerate
bare-CR works now carry real multi-line spans (000081_454: 31 distinct
`line_start` values, lines 14–78; 000081_460: 56 distinct, lines 1–71).

Instrument: `reports/aat-fidelity/audit-aat-delta.py` unmodified since
rotation A — `git log -1 --format=%H -- reports/aat-fidelity/audit-aat-delta.py`
= `b4db35e52010ebe7d698c2d4e8fa1e5caefdf267` (recorded as
`instrument_commit` in the gate summary).

## Span-deviation manifest — the report documents, the manifest authorizes

The committed manifest
(`reports/parser-conformance/span-deviation-manifest.json`, landed with
Task 16 at `5afe01c9`) contains exactly ONE vector. Every entry, with
before/after offsets:

| vector | level | code | span before (vector's own `original_expected`) | span after (authorized decoded span) | why |
|---|---|---|---|---|---|
| `accent_decomposition_applied` | may | `accent-decomposition-applied` | `{start: 3, end: 13}` | `{start: 3, end: 14}` | Third-party vector's hand-derived span is off by one: for source `前〔ae&on〕後`, `bytes[3:13]` truncates `〕`'s 3-byte UTF-8 encoding mid-character (not valid UTF-8, cannot be a `decoded_utf8` span per ADR 0024); `bytes[3:14]` decodes cleanly to the full `〔ae&on〕` construct. Hand byte-arithmetic derivation independent of any adapter output in `.superpowers/sdd/task-16-report.md` ("Row 1: accent_decomposition_applied — manifest material (hand-derived)"), guarded by `source_sha256` `dd6ad350…2087`. |

This report documents the deviation; the manifest (pre-committed, hash-
and staleness-guarded by the Task 16 scorer support) is what authorizes
the scorer to accept it. The conformance gate's programmatic subset check
(see `2026-07-11-phase3-span-conformance-gate.summary.json`,
`details.manifest_subset_check`) confirms the set of rows differing from
rotation A is a subset of this manifest's vector set — in fact empty on
both suites, since the manifest override makes the accent row pass at
rotation B exactly as it passed natively at rotation A.

## Hygiene

- Rotation-A baseline dump `ab-aozora-phase3-capability-a81edf0` read
  only, retained.
- New dump `ab-aozora-phase3-span-a3f91f5` retained (Phase 4 baseline).
- Superseded first-C2 dump `ab-aozora-phase3-span-5afe01c` deleted under
  explicit coordinator authorization (no committed evidence referenced
  it; verified by grep over `docs/superpowers/reports/` before deletion).
- No frozen (≤2026-07-10) report touched.
