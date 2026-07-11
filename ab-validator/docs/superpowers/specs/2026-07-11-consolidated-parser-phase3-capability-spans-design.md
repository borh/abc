# Consolidated Parser Phase 3 — Capability and Span Semantics (Design)

**Date:** 2026-07-11
**Parent:** `ab-validator/docs/superpowers/specs/2026-07-10-consolidated-parser-design.md`
(Phase 3 section) as hardened by
`2026-07-10-consolidated-parser-phase2-absorption-design.md`.
**Inputs:**
- `ab-validator/docs/superpowers/reports/2026-07-09-aozora-pipeline-construct-gap-backlog.md`
  (the three classifier gaps, reference counts, denominator items)
- `ab-validator/docs/handoffs/2026-07-10-parser-fork-provenance.md`
  ("Phase 3 follow-ups": the sanitize-dependency retirement)
- ADR 0024 (span contract: offsets against the full decoded source,
  `decoded_utf8`), ADR 0030 (selection; selection ≠ admission), ADR 0032
  (hard detach; upstream engagement replaced), ADR 0023 (exact-match
  registry).

Phase 2 landed `ab-aozora` (candidate `9cbb7b7b`, merge `0160046d`) as a
byte-parity-proven measurement lane with an unadmitted registry row. Phase 3
delivers the capability obligations named by ADR 0030 and the span-semantics
contract of ADR 0024 — still entirely under unadmitted identities. Registry
activation remains the atomic Phase 4 step; the legacy `--adapter aozora`
lane stays the measurement default throughout.

## Decisions (settled in brainstorming, 2026-07-11)

1. **One spec, two rotations.** Phase 3 is one plan with three ordered
   stages: sanitize swap (no identity rotation), capability fixes
   (rotation A), span semantics (rotation B).
2. **Upstream obligation closed, fork-only.** ADR 0030's upstream-first
   engagement model was already replaced by ADR 0032 ("upstream
   contributions are no longer part of the parser plan"). No upstream PRs;
   this spec records the closure by citation. No new governance is needed.
3. **Diagnostics proven on the production lane.** The 3 diagnostics `must`
   fails are fixed in the fork and scored on the `ab-aozora` lane — the
   instrument that survives Phase 4's legacy retirement. The frozen legacy
   `aozora` binary keeps its historical 3 inspect-lane fails; they are
   unfixable by design (ADR 0032 freeze) and are documented as such.
4. **jizume: parser-typed, AAT-raw until Phase 4.** The fork classifies
   `jizume_block` (with the chars-per-line integer and the compound
   container form), but AAT emission keeps today's raw container markers
   for jizume. AAT schema v1 has no `jizume_block` kind, and the next AAT
   schema version — including jizume layout metadata — is Phase 4's
   rotation.

Two mechanism corrections discovered while grounding the approved design in
the current code; both preserve the decisions above:

- **There is no fork inspect lane.** The `ab-aozora-cli` shim (which carried
  `inspect` dispatch) was deleted in Phase 2, and the inspect scorer
  compares envelopes by exact equality, so an additive field cannot pass it
  anyway. Diagnostics conformance is therefore carried by a new
  `ab-aozora --mode diagnostics` projection (see Stage 1), not by an
  inspect surface.
- **AAT schema v1's `warning` object is closed** (`additionalProperties:
  false`; only `message`/`line`/`path`). Diagnostic `code`/`severity`/span
  therefore cannot ride in AAT `meta.warnings` under schema v1; they ride
  in the wire diagnostics envelope behind `--mode diagnostics`. AAT
  `meta.warnings` keeps its current lossy shape until Phase 4's schema
  rotation. Consequence: rotation A's corpus AAT delta is confined to
  exactly the typed keigakomi/yokogumi containers.

## Scope

In scope:

- Retire the last live crates.io upstream dependency inside the fork's
  owning crate: `ab-aozora-aat`'s `aozora-pipeline = "=0.4.1"` import of
  `lexer::sanitize`, swapped for the fork's
  `ab_aozora_pipeline::lexer::sanitize::sanitize`.
- The 3 diagnostics `must` fixes (`pua_collision`, `tate_chu_yoko`,
  `unclosed_bracket`): stable kebab-case diagnostic `code`s emitted by the
  fork and scored on the `ab-aozora` lane.
- Container classifiers for `keigakomi_block`, `yokogumi_block`, and
  (parser-side) `jizume_block`, including the compound container form
  (`［＃ここから６字下げ、折り返して７字下げ、２１字詰め］`).
- Instrument work the parent spec assigns to Phase 3: AAT-lane diagnostics
  scoring, keigakomi source-authority denominator seed (106 works /
  200 start-annotations), yokogumi denominator audit (3,690 recorded vs
  183 observed).
- Span semantics per ADR 0024: byte offsets into the full decoded source
  and real line coordinates, as its own identity-rotated step.
- Two identity rotations with full ceremony each: adapter version bump,
  full-corpus run, conformance re-score, perf gate, conversion audit, new
  **unadmitted** registry row, tripwire and golden re-baselines.

Out of scope (Phase 4): AAT schema next version (jizume surfacing, layout
metadata, `source_note`, `ruby.direction`, enriched warning shape), mapping
next version, admission, atomic registry activation, legacy-lane
retirement, `:corpus` label normalization. Also out of scope: any upstream
PR, and any change to the frozen `adapters/aozora` or the pinned legacy
binary.

## Architecture: three ordered stages, two rotations

The order is forced by dependencies, not preference:

- The span fix needs sanitize to emit an offset map. `SanitizeOutput` today
  is `{ text: Cow<str>, diagnostics }` — no map — and we can only extend
  the fork's copy. So the **sanitize swap precedes the span work**.
- Doing the swap first, before any behavior change, makes the
  0.4.1-vs-`1a4f864` equivalence question empirically answerable for free:
  full-corpus byte-identity against the retained Phase 2 dump
  (`/db/ab-validator/aat-corpus/ab-aozora-phase2-9cbb7b7b`), identity
  pointers substituted by the existing `compare-aat-dumps.py --bytes`
  machinery.
- Capability changes (rotation A) land before span changes (rotation B) so
  that rotation B's confinement gate — "nothing but spans moved" — has a
  single clean baseline (the rotation-A dump).

Each stage has its own candidate commit (C0, C1, C2), built detached with
`AB_AOZORA_GIT_REV` injected, `--version` asserted to embed the commit, and
its own gate-summary JSON in the Phase 2 evidence schema
(`{gate, candidate: {commit, bin_sha256, version}, verdict, details}`) plus
a `stage` field. A fail-closed `verify-phase3-checkpoint.py` (generalizing
`verify-phase2-checkpoint.py`) checks per-stage internal consistency and
stage ordering before the phase is declared complete.

## Stage 0 — sanitize swap (no rotation)

**Change.** In `crates/ab-aozora-aat`: replace
`use aozora_pipeline::lexer::sanitize` with the fork's
`ab_aozora_pipeline::lexer::sanitize::sanitize`; remove the
`aozora-pipeline = "=0.4.1"` dependency entirely (with it, the ADR 0032
comment block that justified the exact pin). No version bumps, no AAT
output change expected. The fork's sanitize has four steps (BOM strip,
CR/LF normalization, accent decomposition inside `〔…〕`, decorative-rule
isolation); whether crates.io `0.4.1` behaves identically is exactly what
the gate answers.

**Gates (candidate C0):**

- Workspace tests green. The always-on exact-bytes tripwire in
  `ab-aozora-aat` doubles as a free equivalence canary: it hardcodes
  Phase 2 output bytes and must pass **unmodified** at this stage.
- Full-corpus run on hinoki; `compare-aat-dumps.py --bytes` against the
  retained `ab-aozora-phase2-9cbb7b7b` dump: 17,886 compared, 0 missing,
  0 byte-diverged (identity pointers `/meta/adapter` and
  `/meta/adapter_version` substituted; C0's git rev differs from
  `9cbb7b7b`, which is the substitution's job).
- Conformance re-run (P4suta 127 + official seed 30), scores identical to
  the Phase 2 echo baselines: `ab-aozora` lane 113/9/0/5 and 22/8/0/0.
- Perf workset per the parent protocol (≥5 measured runs; >10% median
  regression blocks; new timeouts block unconditionally).

**Divergence resolution rule (fail closed).** Any byte-diverged work stops
the stage. Classify the divergence: if the fork's sanitize is *more
correct* (e.g. a post-0.4.1 upstream fix present in the `1a4f864` lift),
the swap is re-scoped as a behavior-bearing change and moves into
rotation A with its own delta-taxonomy entry and report section; if it is a
lift defect, fix the fork and re-run. Never widen the comparator.

**Evidence:** `docs/superpowers/reports/2026-07-11-phase3-stage0-sanitize-parity.{md,summary.json}`.
The stage-0 dump is deletable after rotation A passes (it is byte-identical
to the Phase 2 dump modulo identity); the Phase 2 dump retention rule is
unchanged.

## Stage 1 — capability (rotation A)

### Diagnostics codes and the `--mode diagnostics` projection

- Every fork diagnostic gains a stable kebab-case `code`, the 1:1 kebab
  form of the existing snake_case `kind` (the three `must` vectors confirm
  the correspondence: `source_contains_pua` → `source-contains-pua`,
  `tcy_target_not_found` → `tcy-target-not-found`, `unclosed_bracket` →
  `unclosed-bracket`). The `code` is emitted by the parser (facade entries
  layer), not synthesized by any instrument: the code value in scored
  output must originate in fork code.
- The wire diagnostics envelope (facade `json` feature) carries the new
  `code` field alongside the existing fields. Wire `SCHEMA_VERSION` bumps
  **2 → 3**; the `const _: () = assert!(json::SCHEMA_VERSION == …)`
  tripwire in `ab-aozora-aat` and every harness gate that names the wire
  schema are updated in the same commit (the Phase 2 lesson: the pin-bump
  drift where the adapter gate moved and the Python harness didn't).
- The permanent binary gains `--mode diagnostics`: read stdin, emit the
  wire diagnostics envelope as one JSON line. Exit codes unchanged (0
  success, 1 failure, 2 still reserved); `--mode aat` and `--version`
  behavior unchanged. This is a recorded, additive amendment to the
  executable-boundary contract of the Phase 2 spec.

### Classifiers

All three mirror the existing `jisage_block` path — "classify an existing
`containerOpen`/`containerClose` marker into a typed node":

- **`keigakomi_block`** from `［＃ここから罫囲み］` … `［＃ここで罫囲み終わり］`.
  AAT emission: schema-v1 `block_container` kind `keigakomi_block`.
  Reference: 106 works / 200 start-annotations, exactly cross-validated by
  aozora-rs.
- **`yokogumi_block`** from `［＃ここから横組み］` … `［＃ここで横組み終わり］`.
  AAT emission: schema-v1 `block_container` kind `yokogumi_block`.
  Reference: 88 works / 183 start-annotations. Content is frequently Latin
  text or math; no special handling beyond faithful children.
- **`jizume_block`** from `［＃ここからN字詰め］` … `［＃ここで字詰め終わり］`,
  carrying N (chars per line) as a typed attribute — **parser vocabulary
  only**. AAT emission for jizume is unchanged (raw container markers), so
  jizume works stay byte-stable in the corpus. The typed node is proven by
  parser-level unit tests and the wire projection; AAT surfacing is
  Phase 4.
- **Compound containers must parse**: `［＃ここから６字下げ、折り返して７字下げ、
  ２１字詰め］` (about 6 occurrences / 4 works) yields the correct composed
  classification (jisage + orikaeshi + jizume) rather than falling back to
  an untyped marker. A regression test uses the exact compound string.

Unclosed/mismatched container markers keep today's recovery behavior
(untyped raw markers plus the existing diagnostics); classification applies
only to well-paired markers, matching the `jisage_block` precedent.

### Instrument work

- **AAT-lane diagnostics scoring** in
  `reports/parser-conformance/run-aozora-notation-spec.py`: for adapters
  that declare a diagnostics command (only `ab-aozora` initially), replace
  the blanket "diagnostics: not comparable for AAT adapter" skip with:
  run `--mode diagnostics`, project each actual entry to
  `{code, severity, span}` (key selection only — never value rewriting),
  and exact-compare against `expected.diagnostics`. Vectors without
  `expected.diagnostics` are unaffected. Unit tests cover pass, fail,
  missing-code, and projection behavior.
- **keigakomi denominator**: seed the source-authority denominator with
  the backlog's verified figure (106 works / 200 start-annotations on the
  pinned corpus) so keigakomi becomes rate-scoreable; per the backlog,
  this means adding `罫囲み` to the source-representability classifier /
  `build-inputs.json` `occ` path, citing the backlog report.
- **yokogumi denominator audit**: resolve the 3,690-vs-183 discrepancy
  (the recorded denominator is from the original local extraction, not the
  pinned nix corpus). Deliverable: a recomputed pinned-corpus figure with
  the counting unit stated, or a documented decision to carry the old
  figure as provisional with the discrepancy explained. The audit result
  is part of rotation A's report either way.

### Rotation A ceremony (candidate C1)

- Versions: `ab-aozora-aat` and `ab-aozora` 0.1.0 → 0.2.0; facade
  0.1.0 → 0.2.0 (its parser vocabulary and wire schema changed); wire
  schema 3. Expected version string:
  `ab-aozora 0.2.0 aat-schema 1 facade 0.2.0 wire-schema 3 (git <C1>)`.
- Tripwire re-baselined to the new exact bytes (version string changed even
  where content didn't).
- **Conformance gate:** on the `ab-aozora` lane, all 25 `must` vectors
  scored (no must-level skip) and passing — this is the "25/25 must" claim,
  now carried by the lane that survives Phase 4. The three named vectors
  pass via the diagnostics comparison. Any additional vectors whose
  `expected.diagnostics` becomes comparable must pass at `must` level or be
  fixed; `should`/`may`-level movements are recorded in the report.
  Baseline for comparison: 113/9/0/5 (P4suta) and 22/8/0/0 (seed).
- **Delta audit (new, fail-closed):** `reports/aat-fidelity/audit-aat-delta.py`
  compares the rotation-A full-corpus dump against the stage-0 dump under
  an explicit allowed-change taxonomy:
  1. identity pointers (`/meta/adapter_version`; substitution reused from
     `compare-aat-dumps.py`);
  2. works whose source carries keigakomi/yokogumi container markers may
     differ **only** by the typed-container restructuring, verified by:
     schema validation, typed-container counts consistent with marker-pair
     counts, and concatenated text content (all `text` node `value`s in
     document order) unchanged;
  3. every other work byte-identical after identity substitution.
  Anything unclassified is exit 2. Expected magnitudes recorded against the
  backlog references (keigakomi ~106 works, yokogumi ~88, overlap
  possible); jizume works must appear in class 3 (byte-identical).
- Conversion audit on hinoki against mapping 0.2.8; new **unadmitted**
  registry row in `abc/data/aat-parser-ir-compatibility.edn` for the 0.2.0
  identity, citing the audit's own coordinates (the Phase 2 rule: fresh
  evidence, no carried-over entries; `:corpus` label follows the Phase 2
  row's convention).
- Perf gate per the parent protocol.
- Evidence: `2026-07-11-phase3-capability-conformance.{md,summary.json}`,
  `2026-07-11-phase3-capability-delta.{md,summary.json}`,
  `2026-07-11-phase3-capability-perf.{md,summary.json}`,
  `2026-07-11-ab-aozora-phase3-capability-conversion-audit.{md,summary.json}`.

## Stage 2 — span semantics (rotation B)

**Contract (ADR 0024).** Every emitted span is byte offsets into the full
decoded source (`decoded_utf8`) with real 1-based `line_start`/`line_end`.
The per-work synthesized warning ("aozora upstream spans are
sanitized-source byte offsets; line_start and line_end are synthesized as
1") is deleted. AAT warning entries' `line` field becomes the real line of
the diagnostic's span start (schema v1 allows any integer ≥ 1).

**Mechanism.**

- `ab_aozora_pipeline::lexer::sanitize` gains an offset map alongside
  `text`: a segment list mapping sanitized-text ranges to source-text
  ranges (insertions from decorative-rule isolation map to zero-width
  source anchors; replacements from accent decomposition and CR/LF
  normalization map to their source ranges). Additive API — existing
  callers are unaffected.
- `ab-aozora-aat` composes the chain: parser span (offsets in the
  body-selected sanitized text) → + body-selection offset (position of the
  body slice within the sanitized text) → sanitize offset map → offsets in
  the decoded source; a precomputed line index over `decoded_utf8` yields
  line coordinates. `span_json` stops synthesizing `1`.
- Property tests over the map: for arbitrary inputs exercising all four
  sanitize steps, every mapped span's decoded-source slice must equal the
  sanitized-text slice it translates wherever the step is
  content-preserving, and must cover the transformed source range at
  transformation sites; map segments are contiguous, monotone, and cover
  the sanitized text exactly.

### Rotation B ceremony (candidate C2)

- Versions: `ab-aozora-aat` and `ab-aozora` 0.2.0 → 0.3.0; facade stays
  0.2.0 (untouched); `ab-aozora-pipeline` takes its own crate-version bump
  for the sanitize API addition. Expected version string:
  `ab-aozora 0.3.0 aat-schema 1 facade 0.2.0 wire-schema 3 (git <C2>)`.
- Golden re-baseline: the four `reference_parity` samples (including the
  real-SJIS one) get hand-verified expected spans — offsets checked
  against the decoded source, line numbers checked against the actual
  files. The tripwire is re-baselined with its expected bytes derived the
  same way. Hand verification is the point: the goldens must not be
  regenerated from the code under test and rubber-stamped.
- **Span-confinement audit** (same `audit-aat-delta.py`, span mode) against
  the rotation-A dump: after identity substitution, masking every span
  object, masking warning `line` values, and dropping the synthesized-span
  warning from the baseline side, the documents must be semantically
  identical — no node kind, value, structure, or count may move. Candidate
  spans additionally satisfy field invariants (`byte_end ≥ byte_start`,
  `line_end ≥ line_start ≥ 1`) and per-work sanity (multi-line works may
  not have all `line_start == 1`). Fail closed on anything else.
- Conformance re-score, both suites. **Deviation rule for diagnostic
  spans:** third-party vector `expected.diagnostics` spans are
  sanitized-text offsets; after rotation B the fork emits decoded-source
  offsets. For most vectors (no BOM/CRLF/accent content) the two coincide.
  Where they differ, our ADR 0024 contract governs — the instrument is
  corroborating, not authoritative (ADR 0030's own framing). Each such
  vector is listed in the rotation-B report with both offset values and
  scored via a declared, span-translated comparison: the expected
  sanitized-text offsets are translated through the same fork offset map
  (computed for the vector's source) before the exact compare, and the
  translation is only applied to vectors on the report's named list —
  never by silently editing vectors or weakening the scorer for everyone.
- Conversion audit + second unadmitted registry row (0.3.0 identity), perf
  gate — same ceremony as rotation A.
- Evidence: `2026-07-11-phase3-span-confinement.{md,summary.json}`,
  `2026-07-11-phase3-span-conformance.{md,summary.json}`,
  `2026-07-11-phase3-span-perf.{md,summary.json}`,
  `2026-07-11-ab-aozora-phase3-span-conversion-audit.{md,summary.json}`.
- Dump retention: the rotation-A and rotation-B dumps are retained on
  hinoki (`ab-aozora-phase3-capability-<C1>`, `ab-aozora-phase3-span-<C2>`);
  rotation B's dump is the expected Phase 4 baseline.

## Identity and evidence discipline

- Three candidate commits C0, C1, C2, each recorded in the progress ledger
  when fixed. Gate builds are detached clean checkouts with
  `AB_AOZORA_GIT_REV=$(git rev-parse HEAD)`; every gate asserts
  `ab-aozora --version | grep -F <candidate>` before running. A
  "git unknown" build never becomes evidence.
- Gate summaries reuse the Phase 2 schema plus `stage`; frozen once
  written, never edited.
- `verify-phase3-checkpoint.py` (fail-closed, unit-tested) verifies before
  phase completion: every stage's summaries agree internally on
  commit/bin_sha256/version, all verdicts PASS, version strings embed
  their commit and the expected version/wire-schema values per stage, and
  the stage set is complete. The Phase 3 branch merges only after
  `CHECKPOINT OK`.
- Both registry rows are compatibility evidence under ADR 0023 —
  **unadmitted**. Nothing in Phase 3 changes the measurement default, the
  admission state, or the legacy lane.

## Testing

- Unit: classifier tests per construct (simple form, close form, compound
  form, unpaired-marker recovery); diagnostics `code` presence and kebab
  correspondence for every diagnostic kind the fork can emit; offset-map
  property tests (all four sanitize steps, composition with body
  selection); `--mode diagnostics` wire tests alongside the existing
  `tests/wire.rs`.
- Instrument: unit tests for the scorer's diagnostics path and for
  `audit-aat-delta.py` (both modes, including fail-closed behavior on an
  unclassified diff); `reports/**` pytest already runs in CI.
- Integration: `reference_parity` goldens (env-gated) at each rotation;
  `ab-aat-to-parser-ir` integration suite must stay green at both rotations
  (keigakomi/yokogumi block kinds are schema-v1, so mapping 0.2.8 must
  already handle them — any converter change is out of contract and a
  finding).
- Always-on: exact-bytes tripwire, re-baselined at each rotation, unchanged
  at stage 0 by construction; `just preserve-order-hazard-check` stays
  green throughout (guard script + canary).

## Risks

- **Stage 0 non-equivalence** (0.4.1 vs `1a4f864` sanitize): covered by the
  fail-closed classify-then-route rule; the plausible divergence sites are
  steps 3 and 4 (accent decomposition scope, decorative-rule isolation),
  both post-0.4.1-active areas upstream.
- **Scorer widening surfaces new must-level diagnostics mismatches** beyond
  the three named vectors: the gate demands they be fixed, not skipped;
  budgetary risk only.
- **Offset-map complexity**: steps 3 and 4 are length-changing; the
  property-test suite is the primary defense, the hand-verified goldens the
  secondary.
- **Diagnostic-span drift vs third-party vectors after rotation B**:
  handled by the declared deviation rule; worst case is a documented,
  per-vector translated comparison, never a silent instrument weakening.
- **Warning-count blowup at rotation A** is designed out: AAT
  `meta.warnings` is untouched at rotation A (closed schema), so the corpus
  delta stays confined to the two typed constructs.

## Acceptance criteria

- [ ] `ab-aozora-aat` has no `aozora-pipeline` (crates.io) dependency; the
      workspace has no live upstream parser crate outside the frozen legacy
      comparator surface.
- [ ] Stage 0 parity: 17,886/0/0 byte-parity vs the Phase 2 dump; tripwire
      unmodified and green; conformance scores identical to Phase 2 echo.
- [ ] Rotation A: 25/25 `must` scored and passing on the `ab-aozora` lane;
      delta audit PASS under the three-class taxonomy; conversion audit +
      unadmitted 0.2.0 registry row; perf gate PASS.
- [ ] `ab-aozora --mode diagnostics` emits wire-schema-3 envelopes with
      parser-originated kebab `code`s; exit codes unchanged.
- [ ] `jizume_block` typed in parser vocabulary with N attribute and
      compound-form parsing; jizume corpus AAT byte-stable.
- [ ] keigakomi denominator seeded (106/200, backlog-cited); yokogumi
      denominator audit resolved or documented-provisional.
- [ ] Rotation B: span-confinement audit PASS (only spans, warning lines,
      and the synthesized warning moved); hand-verified goldens; conversion
      audit + unadmitted 0.3.0 registry row; perf gate PASS; diagnostic-span
      deviations (if any) documented per the deviation rule.
- [ ] `verify-phase3-checkpoint.py` reports CHECKPOINT OK over all frozen
      stage summaries before merge.
- [ ] Legacy lane untouched: `--adapter aozora` remains the measurement
      default; no admission, no activation, no frozen-evidence edits.
