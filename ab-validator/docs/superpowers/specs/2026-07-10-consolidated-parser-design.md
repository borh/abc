# Consolidated Aozora parser — design (fork of aozora-pipeline)

**Status:** Accepted design, pending implementation plan
**Date:** 2026-07-10 (revised same day after design review; see Revision Notes)
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
   today's adapter AAT (nodes/diagnostics/gaiji with spans): the three
   container classifiers (`jizume`, `yokogumi`, `keigakomi`, including
   compound containers), the 3 diagnostics `must` fixes, genuine
   producer-emitted AAT block structure (paragraph and container blocks; see
   Paragraph Authority below), `source_note` emission under a full-document
   region contract, layout metadata, and `ruby.direction`. First-class
   warigaki/kunten stay deferred (ADR 0023) and need their own vocabulary ADR
   before any implementation.
4. **Upstream relationship: hard detach at `1a4f864`.** No future merges from
   `P4suta/aozora`; the fork is fully ours from the measured pin. This
   replaces ADR 0030's upstream-first engagement model and requires a short
   amending ADR (ADR 0031) — folded into the implementation plan as its first
   task, not a design prerequisite.
5. **Placement and naming: `ab-validator/crates/`, `ab-*` names.** Lifted
   crates are renamed (`ab-aozora-encoding`, `ab-aozora-syntax`,
   `ab-aozora-cst`, `ab-aozora-pipeline`), live inside the existing workspace
   gates (fmt/clippy/deny/tests), and preserve upstream attribution: the
   upstream `NOTICE` travels with the code and each crate records the source
   repository and rev `1a4f864603970983719655aa4af4525958ac2d38` in a
   provenance header. Upstream is dual `MIT OR Apache-2.0`, matching the
   workspace license.

## Protocol contracts (review-driven; bind Phases 2–4)

### Paragraph authority — AAT blocks are the single source of truth

AAT already represents paragraph and container structure as block nodes
(`docs/aat-contract.md` Block Nodes: `paragraph`, `heading`, `jisage_block`,
`quote_block`, `keigakomi_block`, `yokogumi_block`, `caption_block`), and
`ab-aat-to-parser-ir` already derives parser-IR `paragraphs[]` from `/blocks`
(`crates/ab-aat-to-parser-ir/src/convert.rs`). This design adds **no**
top-level AAT paragraph range table. The fork's Level-3 paragraph work is to
emit *genuine* block structure from the parse (today's adapter synthesizes
blocks from inline content in `blocks_from_inline_content`), and the
converter remains the only producer of parser-IR paragraph ranges.
Coherence invariant: parser-IR `paragraphs[]` is derived exclusively from AAT
blocks; no second representation of paragraph boundaries may enter AAT.

### Executable boundary — a permanent `ab-aozora` binary

Corpus measurement and performance tooling require a stdin→AAT process
boundary (`reports/aat-fidelity/run-aat-full.sh` drives adapter binaries;
`reports/aat-fidelity/measure-parser-performance.py` invokes "every parser as
a stdin → AAT process"). The library absorption therefore ships with a
**permanent** thin binary, `ab-aozora`, over `ab-aozora-aat`:

- stdin: raw source bytes (Shift_JIS/CP932 as in the corpus); stdout: one AAT
  JSON document.
- exit codes: `0` success, `2` success-with-warnings, `1` fatal — matching
  the existing adapter wire contract
  (`docs/handoffs/adding-aozora2-aozoraepub3-parser-support.md`).
- `--version` emits the identity fields the evidence chain needs: adapter id
  (`ab-aozora`), adapter version, AAT schema version, and build identity
  (crate versions + git rev when available).
- errors are structured on stderr; no partial AAT on stdout after a fatal.

What disappears in Phase 2 is the *internal* subprocess hop between parser
and AAT assembly (the `inspect` JSON protocol), not the process boundary the
harness owns.

### Source-region contract — full-document region model before `source_note`

Current body selection removes the notation legend and truncates the tail
(`adapters/aozora/src/lib.rs` `aozora_body_text`), while terminal provenance
and colophon classes still await ABC disposition
(`docs/handoffs/source-region-coverage-abc-integration.md`). `source_note`
emission is therefore governed by a region contract, specified and accepted
**before Phase 4 implementation starts**:

- The parser models the full document as regions: `front_legend`, `body`,
  `terminal_provenance` (底本： block), `colophon_metadata`,
  `body_end_boundary`, `malformed_residue`.
- ab-validator measures regions (what and where); **ABC assigns disposition
  and placement** per its existing owner boundary — which regions become
  `source_note` nodes with which `placement`, which become custom
  preservation records, which stay diagnostics.
- Non-body regions re-enter *parsing* (they are measured and preserved), but
  the visible-body-only plaintext rule is explicitly unchanged: no region
  content leaks into plaintext output.
- Until the contract is accepted, the fork preserves current body-selection
  behavior byte-for-byte.

### Span semantics — parity first, then one identity-rotated fix

Current spans are byte offsets into sanitized/body-selected text with
`line_start`/`line_end` synthesized as `1` (the adapter says so in its own
warning). The Phase 2 parity gate deliberately preserves this legacy
behavior. The claimed contract — offsets against the **full decoded source**
(`decoded_utf8`, per ADR 0024) with real line coordinates — lands afterwards
as a **separate, identity-rotated change** (its own adapter-version bump,
golden re-baseline, and conversion-audit refresh), so span semantics are
never conflated with the lift or the absorption in the evidence chain.

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
  the fork for the parity gate. Deleted at the end of Phase 2, superseded by
  the permanent `ab-aozora` binary.
- `crates/ab-aozora-aat` (Phase 2) — the target component: native AAT
  emission as a library, absorbing the adapter's Shift_JIS/CP932 encoding
  shims and AAT assembly, fronted by the permanent `ab-aozora` binary
  (contract above).

### Untouched

- `crates/ab-source-syntax` — in-house lexer serving source-inventory and
  source-region-coverage instrumentation. It is measurement infrastructure,
  not the parser; it stays independent so source authority does not share
  code with the parser under test.
- All other adapters (`aozora2`, `aozora-rs`, `aozora2html`, `aozora-epub3`)
  remain comparison lanes with their existing fidelity notes.
- `crates/ab-aat-to-parser-ir` — continues to own AAT→parser-IR; Phase 4
  extends its mapping, it is not restructured.

### Legacy lane retention (rollback safety)

The pinned upstream lane (`upstream-aozora-src` flake input + `AB_AOZORA_BIN`
+ the crates.io `aozora-pipeline 0.4.1` adapter path) is **demoted to
comparison-only at Phase 2, not deleted**. It remains reproducible until the
fork passes Phase 4 admission; registry activation of the fork's identity is
the atomic final step, after which the legacy lane and flake input are
retired in a follow-up change. Schema, mapper, reports, and fixtures may land
earlier under an unadmitted identity.

## Phases and gates

Each phase lands independently on `main` with its gate evidence committed.

### Phase 0 — governance (first task of the implementation plan)

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
  envelope handling, AAT assembly) moves into it; the permanent `ab-aozora`
  binary fronts it per the executable-boundary contract.
- Gate: corpus-wide golden diff — new-library AAT ≡ adapter AAT (legacy span
  semantics preserved for this gate). Then delete `ab-aozora-cli`, point the
  measurement harness at `ab-aozora`, and demote (not delete) the legacy
  lane per Legacy Lane Retention.
- Identity rotation is part of the phase: new adapter identity (`ab-aozora`
  + version) means fresh conversion-audit evidence and exact-match registry
  rows in `abc/data/aat-parser-ir-compatibility.edn` (ADR 0023). No wildcard
  or carried-over entries.

### Phase 3 — capability and span semantics

- The 3 diagnostics `must` fixes (nodes were already conformant; fails are
  diagnostics-only).
- Classifiers for `jizume`, `yokogumi`, `keigakomi`: each is "classify an
  existing `containerOpen`/`containerClose` marker into a typed node",
  mirroring the `jisage_block` path; the compound-container form
  (`［＃ここから６字下げ、折り返して７字下げ、２１字詰め］`) must parse. Reference
  counts from `2026-07-09-aozora-pipeline-construct-gap-backlog.md`
  (keigakomi: 106 works / 200 starts, exact aozora-rs cross-validation).
- **Span-semantics fix as its own identity-rotated step** (see Span
  Semantics): full-decoded-source offsets, real line coordinates, golden
  re-baseline.
- Gate per change: conformance re-score + corpus re-measure; effects confined
  to the named constructs (everything else byte-stable), instrument
  denominators updated where the backlog flagged them (keigakomi denominator,
  yokogumi denominator audit).

### Phase 4 — Level-3 structures

- Prerequisite: the source-region contract (above) accepted by ABC — the
  region→lane disposition table is a policy input, not something this phase
  invents.
- AAT schema extension (`data/aat-schema.json` next version): `source_note`
  node kind with region-derived placement, layout metadata on container
  blocks (jisage/burasage/chitsuki/jizume/line-jisage), `ruby.direction`.
  **No paragraph range table** — paragraph fidelity ships as genuine block
  emission per the Paragraph Authority contract.
- Mapping next version in `ab-aat-to-parser-ir` with divergence accounting
  for the new structures; parser-IR population per
  `abc/docs/handoffs/parser-ir-level3-structure-delta.md` (parser-IR
  `paragraphs[]` remains converter-derived from AAT blocks).
- Full ADR 0023 rotation ceremony: new mapping document hash, fresh producer
  reports, new registry rows, regenerated `examples/ab-validator-output/`
  fixtures, ABC design-bundle validation. **Registry activation is the
  atomic final step**; everything before it lands under an unadmitted
  identity, and the legacy lane stays reproducible until it passes.
- Gate: the 2026-07-06 acceptance criteria wholesale —
  `unsupported_body_markup_occurrences == 0`,
  `unknown_region_occurrences == 0`, `unknown_unreviewed_occurrences == 0`,
  parser-IR schema-valid with every source-observed fact in an admitted
  publication lane, representative then full-scope bundle validation, and
  named next-work ledger items cited and closed.

### Performance gate (all phases)

Bounded-budget measurement with recorded DNF, never open-ended retries.
Measurement protocol (replaces the earlier any-median-regression rule, which
was noise-gated):

- **Fixed workset**: the committed perf workset (the existing 6-work sample
  extended to a named, hash-pinned workset file at Phase 1).
- **Build profile**: `--release`, sccache disabled, recorded toolchain.
- **Repetitions**: 1 warm-up + ≥5 measured runs per work; report median and
  spread.
- **Machine identity**: host, CPU model, and core count recorded alongside
  results; comparisons only within the same recorded identity.
- **Material-regression threshold**: >10% median wall-time regression on the
  workset blocks the phase pending investigation; anything under threshold is
  recorded, not blocking.
- **New timeouts are an unconditional blocker** at any threshold.

Reference profile: the study's measured 1.25 s median, zero timeouts;
corpus-scale run ~5–10 min at 32 jobs.

## Data flow and identity

```
source bytes
  → ab-aozora-encoding (Shift_JIS/CP932 decode, decoded_utf8 spans)
  → ab-aozora-syntax / ab-aozora-cst
  → ab-aozora-pipeline projections
  → ab-aozora-aat (native AAT emission; ab-aozora binary at the harness edge)
  → AAT JSON (data/aat-schema.json, next version in Phase 4)
  → ab-aat-to-parser-ir (mapping next version) → parser-IR + divergence bundle
  → ABC registry admission (ADR 0023) → publication lanes (TEI P5 / ABC-ext /
    sidecar / plaintext)
```

AAT remains the normative adapter contract; the fork is the first parser to
emit it natively. Parser identity lives in adapter/mapping coordinates
exactly as today; manifest identity rules are untouched (ADR 0001/0023).

## Diagnostics and error handling

- Upstream's diagnostics envelope is preserved and extended, never bypassed.
- Stable codes and severities with source spans (ADR 0002 gates); span
  coordinates follow the Span Semantics contract (legacy until the Phase 3
  rotation, `decoded_utf8` full-source afterwards).
- Unsupported syntax is emitted as structured warnings or raw-preserved
  nodes — zero silent drops.
- Critical divergence stays mode-bound: development records and continues;
  release-smoke fails (no operator strictness flag).

## Testing

- Lifted upstream unit tests, kept green as the inherited baseline.
- Both conformance instruments (127 P4suta vectors + 30 official-docs seed)
  wired into fork CI via the existing harness.
- Corpus-scale golden AAT diffs as phase gates (Phases 1–3).
- Perf measurement per the protocol above.
- `aozora-proptest` is not lifted in v1; property testing is an optional
  later add.

## Risks and open points

- **Dependency closure may exceed four crates.** Discovered at lift time; the
  closure is normative. Anything large or unwanted in the closure triggers a
  scoping decision before code lands.
- **Inherited cargo dependency tree** enters the lock/audit surface; reviewed
  under `deny.toml` at Phase 1.
- **Phase 4 is the largest non-parser chunk** (schema + mapping + registry +
  fixture rotation) and now has a policy prerequisite (the source-region
  disposition table from ABC). It is deliberately last; the rotation
  ceremony must not be split across partial identities, and registry
  activation is atomic.
- **Hard detach forfeits upstream fixes.** Accepted deliberately; the
  amending ADR records it. If upstream lands something we want later, it is
  ported as a reviewed patch with re-measurement, not a merge.
- **Warigaki/kunten remain out of contract** until a vocabulary ADR; the
  fork must keep raw-preserving them (they already flow through the
  raw/marker escape hatch) so no evidence is lost meanwhile.

## Revision Notes (2026-07-10 design review)

Incorporated from the post-draft design review:

1. Dropped the AAT `paragraphs[]` range table (would have created a second
   paragraph authority next to AAT blocks); added the Paragraph Authority
   contract.
2. Defined the permanent `ab-aozora` stdin→AAT binary (the harness process
   boundary survives absorption; only the internal inspect hop dies).
3. Added the full-document source-region contract as a Phase 4 policy
   prerequisite with ABC owning disposition/placement; visible-body-only
   plaintext explicitly preserved.
4. Split span semantics out of parity: legacy spans hold through the Phase 2
   gate; the `decoded_utf8`/real-lines fix is its own identity-rotated step.
5. Legacy pinned lane retained comparison-only until Phase 4 admission;
   registry activation is the atomic cutover.
6. Replaced the any-median-regression perf rule with a repetition/threshold
   protocol; new timeouts stay unconditionally blocking.
7. ADR 0031 folded into the implementation plan as its first task.
