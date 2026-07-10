# Consolidated Aozora parser — design (fork of aozora-pipeline)

**Status:** Accepted design, pending implementation plan
**Date:** 2026-07-10
**Authority:** ADR 0030 (`abc/docs/adr/0030-aozora-parser-selection.md`) selected
`P4suta/aozora` as the fork base; this design decides the fork's shape.
**Acceptance bar:** `docs/superpowers/specs/2026-07-06-comprehensive-parser-acceptance-criteria.md`
(source coverage, parser-IR emission, publication bundle, bounded performance).
The parser consumes the next-work ledger; it does not replace it.
**Predecessor:** `docs/superpowers/specs/2026-07-09-parser-comparison-followups-handoff.md`
("start the parser design" was the remaining forward move).

## Decisions (settled in brainstorming, 2026-07-10)

1. **Component shape: library fork as workspace crates.** The consolidated
   parser is first-party code in the `ab-validator` workspace that emits AAT
   natively as a library, absorbing `adapters/aozora`. Not an evolved
   subprocess adapter, not a tracking patch-queue.
2. **Fork scope: minimal parser core.** Lift only the dependency closure that
   parsing-to-AAT needs — expected `aozora-encoding`, `aozora-syntax`,
   `aozora-cst`, `aozora-pipeline` plus whatever sibling crates their closure
   pulls in. The closure, not this list, is normative; everything outside it
   (CLI, LSP, WASM, py/go/pandoc bindings, render, playground, bench, book)
   stays out.
3. **v1 output contract: parity + Level-3 structures.** Beyond parity with
   today's adapter AAT (nodes/diagnostics/gaiji with `decoded_utf8` spans):
   the three container classifiers (`jizume`, `yokogumi`, `keigakomi`,
   including compound containers), the 3 diagnostics `must` fixes, and the
   producer-preserved publication structures TEI Level 3 is blocked on —
   `paragraphs[]`, `source_note` nodes, layout metadata, `ruby.direction`.
   First-class warigaki/kunten stay deferred (ADR 0023) and need their own
   vocabulary ADR before any implementation.
4. **Upstream relationship: hard detach at `1a4f864`.** No future merges from
   `P4suta/aozora`; the fork is fully ours from the measured pin. This
   replaces ADR 0030's upstream-first engagement model and therefore requires
   a short amending ADR (Phase 0) — ADR Decisions are immutable in place.
5. **Placement and naming: `ab-validator/crates/`, `ab-*` names.** Lifted
   crates are renamed (`ab-aozora-encoding`, `ab-aozora-syntax`,
   `ab-aozora-cst`, `ab-aozora-pipeline`), live inside the existing workspace
   gates (fmt/clippy/deny/tests), and preserve upstream attribution: the
   upstream `NOTICE` travels with the code and each crate records the source
   repository and rev `1a4f864603970983719655aa4af4525958ac2d38` in a
   provenance header. Upstream is dual `MIT OR Apache-2.0`, matching the
   workspace license.

## Migration architecture: strangler / parity-first (Approach B)

Rejected alternatives: big-bang absorption (first measurable artifact appears
only after everything has moved — a lift/rename/projection/capability
regression would be unattributable) and CST-first re-projection (discards the
`inspect` projection path the comparison study validated on 115/122 vectors,
re-proving fidelity from scratch). Approach B converts the fork into a
sequence of individually falsifiable steps, each gated by measurement
instruments that already exist (`reports/aat-fidelity/normalized-corpus-coverage.py`,
the conformance harness, golden corpus diffs, the perf sample harness).

### New components

- `crates/ab-aozora-encoding|syntax|cst|pipeline` — verbatim lifts,
  rename-only in Phase 1, zero semantic edits.
- `crates/ab-aozora-cli` — **throwaway shim** binary reproducing the upstream
  `aozora inspect {nodes,diagnostics,gaiji} -` protocol at `schemaVersion` 2,
  byte-compatible, so the unchanged `adapters/aozora` (983 LOC) runs against
  the fork for the parity gate. Deleted at the end of Phase 2.
- `crates/ab-aozora-aat` (Phase 2) — the target component: native AAT
  emission as a library, absorbing the adapter's Shift_JIS/CP932 encoding
  shims and AAT assembly. The subprocess/JSON hop disappears.

### Untouched

- `crates/ab-source-syntax` — in-house lexer serving source-inventory and
  source-region-coverage instrumentation. It is measurement infrastructure,
  not the parser; it stays independent so source authority does not share
  code with the parser under test.
- All other adapters (`aozora2`, `aozora-rs`, `aozora2html`, `aozora-epub3`)
  remain comparison lanes with their existing fidelity notes.
- `crates/ab-aat-to-parser-ir` — continues to own AAT→parser-IR; Phase 4
  extends its mapping, it is not restructured.

### Retired

- The `upstream-aozora-src` flake input and `AB_AOZORA_BIN` indirection, once
  the shim passes parity (the workspace builds its own parser binary).
- The adapter's crates.io `aozora-pipeline 0.4.1` dependency, at absorption.

## Phases and gates

Each phase lands independently on `main` with its gate evidence committed.

### Phase 0 — governance

- ADR 0031 (abc ADR series): amends ADR 0030 — hard detach at `1a4f864`
  replaces upstream-first; fork provenance, attribution, and naming policy
  recorded.
- Fork-provenance handoff note in `ab-validator/docs/handoffs/`.

### Phase 1 — lift + shim + parity

- Lift the dependency closure, rename, keep upstream unit tests green under
  the workspace gates. Review the inherited cargo dependency tree under
  `deny.toml` (licenses, advisories); record any newly-vendored transitive
  surface.
- Gate A (corpus): full-corpus AAT via `adapters/aozora` + `ab-aozora-cli`
  is identical to the `aozora-full-repin-1a4f864` dump modulo adapter-path
  metadata. Same code, so the expectation is byte-equality of AAT content;
  any diff is a lift defect.
- Gate B (conformance): the 127-vector P4suta suite plus the 30-vector
  official-docs seed score identically to the pinned upstream binary.

### Phase 2 — absorption

- `ab-aozora-aat` emits AAT natively; `adapters/aozora` logic (decode shims,
  envelope handling, AAT assembly) moves into it.
- Gate: corpus-wide golden diff — new-library AAT ≡ adapter AAT. Then delete
  `ab-aozora-cli`, rewire the measurement harness (`run-aozora-aat-full.sh`
  path), and retire the flake input.
- Identity rotation is part of the phase: new adapter identity (`ab-aozora`
  + version) means fresh conversion-audit evidence and exact-match registry
  rows in `abc/data/aat-parser-ir-compatibility.edn` (ADR 0023). No wildcard
  or carried-over entries.

### Phase 3 — capability

- The 3 diagnostics `must` fixes (nodes were already conformant; fails are
  diagnostics-only).
- Classifiers for `jizume`, `yokogumi`, `keigakomi`: each is "classify an
  existing `containerOpen`/`containerClose` marker into a typed node",
  mirroring the `jisage_block` path; the compound-container form
  (`［＃ここから６字下げ、折り返して７字下げ、２１字詰め］`) must parse. Reference
  counts from `2026-07-09-aozora-pipeline-construct-gap-backlog.md`
  (keigakomi: 106 works / 200 starts, exact aozora-rs cross-validation).
- Gate per change: conformance re-score + corpus re-measure; effects confined
  to the named constructs (everything else byte-stable), instrument
  denominators updated where the backlog flagged them (keigakomi denominator,
  yokogumi denominator audit).

### Phase 4 — Level-3 structures

- AAT schema extension (`data/aat-schema.json` vNext): `paragraphs[]` range
  table, `source_note` node with placement, layout metadata
  (jisage/burasage/chitsuki/jizume/line-jisage), `ruby.direction`.
- Mapping vNext in `ab-aat-to-parser-ir` with divergence accounting for the
  new structures; parser-IR population per
  `abc/docs/handoffs/parser-ir-level3-structure-delta.md`.
- Full ADR 0023 rotation ceremony: new mapping document hash, fresh producer
  reports, new registry rows, regenerated `examples/ab-validator-output/`
  fixtures, ABC design-bundle validation.
- Gate: the 2026-07-06 acceptance criteria wholesale —
  `unsupported_body_markup_occurrences == 0`,
  `unknown_region_occurrences == 0`, `unknown_unreviewed_occurrences == 0`,
  parser-IR schema-valid with every source-observed fact in an admitted
  publication lane, representative then full-scope bundle validation, and
  named next-work ledger items cited and closed.

### Performance gate (all phases)

Bounded-budget measurement with recorded DNF, never open-ended retries.
Regression bar: the study's measured profile (median 1.25 s, zero timeouts on
the 6-work sample; corpus-scale run ~5–10 min at 32 jobs). Any phase that
regresses median wall time or introduces timeouts blocks on a perf
investigation before landing.

## Data flow and identity

```
source bytes
  → ab-aozora-encoding (Shift_JIS/CP932 decode, decoded_utf8 spans)
  → ab-aozora-syntax / ab-aozora-cst
  → ab-aozora-pipeline projections
  → ab-aozora-aat (native AAT emission)
  → AAT JSON (data/aat-schema.json, vNext in Phase 4)
  → ab-aat-to-parser-ir (mapping vNext) → parser-IR + divergence bundle
  → ABC registry admission (ADR 0023) → publication lanes (TEI P5 / ABC-ext /
    sidecar / plaintext)
```

AAT remains the normative adapter contract; the fork is the first parser to
emit it natively. Parser identity lives in adapter/mapping coordinates
exactly as today; manifest identity rules are untouched (ADR 0001/0023).

## Diagnostics and error handling

- Upstream's diagnostics envelope is preserved and extended, never bypassed.
- Stable codes and severities with source spans (ADR 0002 gates).
- Unsupported syntax is emitted as structured warnings or raw-preserved
  nodes — zero silent drops.
- Critical divergence stays mode-bound: development records and continues;
  release-smoke fails (no operator strictness flag).

## Testing

- Lifted upstream unit tests, kept green as the inherited baseline.
- Both conformance instruments (127 P4suta vectors + 30 official-docs seed)
  wired into fork CI via the existing harness.
- Corpus-scale golden AAT diffs as phase gates (Phases 1–3).
- Existing perf sample harness for the performance gate.
- `aozora-proptest` is not lifted in v1; property testing is an optional
  later add.

## Risks and open points

- **Dependency closure may exceed four crates.** Discovered at lift time; the
  closure is normative. Anything large or unwanted in the closure triggers a
  scoping decision before code lands.
- **Inherited cargo dependency tree** enters the lock/audit surface; reviewed
  under `deny.toml` at Phase 1.
- **Phase 4 is the largest non-parser chunk** (schema + mapping + registry +
  fixture rotation). It is deliberately last and deliberately whole — the
  rotation ceremony must not be split across partial identities.
- **Hard detach forfeits upstream fixes.** Accepted deliberately; the
  amending ADR records it. If upstream lands something we want later, it is
  ported as a reviewed patch with re-measurement, not a merge.
- **Warigaki/kunten remain out of contract** until a vocabulary ADR; the
  fork must keep raw-preserving them (they already flow through the
  raw/marker escape hatch) so no evidence is lost meanwhile.
