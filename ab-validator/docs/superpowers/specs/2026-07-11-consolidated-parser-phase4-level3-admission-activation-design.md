# Consolidated Parser Phase 4 — Level-3 Structures, Admission, Activation

- **Status:** Draft for review
- **Date:** 2026-07-11
- **Parent design:** `2026-07-10-consolidated-parser-design.md` (Phase 4 section)
- **Predecessor:** Phase 3 merged to main `c18254fa`
  (`docs/handoffs/2026-07-10-parser-fork-provenance.md`, Phase 3 closure)
- **Acceptance bar:** `2026-07-06-comprehensive-parser-acceptance-criteria.md`,
  applied wholesale at the activation gate

## Goal

Make `ab-aozora` the publication-lane parser: extend the AAT contract to
schema v2 (Level-3 structures), rotate the AAT→parser-IR mapping, pass the
ADR 0023 admission ceremony with fresh evidence, atomically activate the
fork as the publication default, and retire the legacy pinned lane.

## Decisions locked at brainstorm (2026-07-11)

1. **Bare-toggle marker forms are deferred past Phase 4.** The
   `［＃横組み］…［＃横組み終わり］` (~3,188 occurrences) and bare `［＃罫囲み］`
   (~25) toggle forms stay raw-preserved. They do not affect
   `unsupported_body_markup_occurrences == 0` (raw preservation counts as
   covered) and remain the named classifier ceiling on the ledger.
2. **Two identity rotations**: C3 (schema v2 + in-body reshaping), C4
   (source_note emission). The ADR 0023 ceremony happens exactly once, at C4.
3. **Legacy-lane retirement is the final Phase 4 task**, executed only after
   the activation checkpoint is OK. The phase ends with the fork as sole
   publication lane and no dangling follow-up.
4. **Performance is measure-first.** Standard per-rotation gate (>10% median
   wall-time regression on the pinned workset blocks; new timeouts block at
   any threshold). No up-front optimization task; one is added only if a
   gate blocks or the C4 candidate measures slower than the legacy lane at
   median.
5. **AAT rotates to a true v2** (new `$id`, `version` const 2), with a
   byte-frozen v1 schema file retained for the comparison lanes.
6. **Layout metadata is typed in place** — existing node topology is
   preserved; `x-` layout fields are promoted to typed schema fields. No
   container restructuring of chitsuki/burasage.
7. **`source_note` emission covers `terminal_provenance` only** (placement
   `back`), exactly matching ABC source-region publication policy v0.2.0.
   The placement enum stays full-width for future classes.

## Scope

- AAT schema v2: `jizume_block`, typed layout fields, enriched warnings,
  `source_note` node kind; frozen v1 schema copy for comparison lanes.
- Parser emission for all of the above across two identity rotations, each
  with the full Phase 3-style evidence set (delta audit, conformance, perf,
  conversion audit) on the full corpus.
- Terminal-provenance/colophon **measurement split** instrument (the
  `needs_measurement_split` item named by ABC policy v0.2.0), landed before
  any source_note emission.
- Mapping next version (`0.3.0`, `source_aat_version 2`, new mapping id)
  with divergence accounting for the new structures; converter dispatches
  on AAT document version, v1 path byte-stable.
- Full ADR 0023 rotation ceremony: fresh producer reports, new registry
  rows (`:aat_version 2`), regenerated `examples/ab-validator-output/`
  fixtures, ABC design-bundle validation (representative, then full-scope).
- **Atomic activation commit** flipping the publication default to
  `ab-aozora`; 2026-07-06 acceptance criteria applied wholesale;
  `verify-phase4-checkpoint.py` binding the whole chain.
- Legacy-lane retirement (final task).
- Owed housekeeping: `ab-aozora-pipeline` crate version `0.1.0 → 0.2.0`
  (Phase 3 record-only Minor; the `sanitize_mapped` API addition), stale
  `crates/ab-aat-to-parser-ir/README.md` mapping coordinates (still cites
  0.2.6 / `sha256:e36508c3…`), and the fork-provenance handoff one-liner for
  the three ledger items Phase 3's final review noted as omitted
  (bare-toggle ceiling, keigakomi 44-marker residual, `:corpus` label trap).

## Non-goals

- Bare-toggle classifiers (decision 1).
- Warigaki/kunten vocabulary — awaits its own vocabulary ADR; both remain
  raw-preserved so no evidence is lost.
- Resolving the keigakomi 44-marker denominator residual (673 matrix vs 717
  frozen, `2026-07-11-keigakomi-yokogumi-denominator-attribution.md`). It
  stays a named ledger item; nothing in Phase 4 depends on it.
- Any behavior change in the frozen comparison adapters (`aozora2`,
  `aozora-rs`, `aozora2html`, `aozora-epub3`) or in frozen evidence.
- Plaintext semantics: the visible-body-only rule is byte-preserved
  throughout; no region content enters plaintext-relevant byte accounting.
- Parser-IR schema changes: parser-IR already carries `source-note`,
  `paragraphs[]`, and paragraph `layout`
  (`abc/docs/handoffs/parser-ir-level3-structure-delta.md`); Phase 4
  populates them via the mapping, it does not extend them.

## Identity plan

Baseline: Phase 3 C2 = `a3f91f53fcae9bc18f577ea5b746f3be7f228fcb`
(`ab-aozora 0.3.0 aat-schema 1 facade 0.2.0 wire-schema 3`), corpus dump
`/db/ab-validator/aat-corpus/ab-aozora-phase3-span-a3f91f5` (retained,
never delete).

| Candidate | Version join key | Content |
|---|---|---|
| C3 | `ab-aozora 0.4.0 aat-schema 2 facade 0.2.0 wire-schema 3` | Schema v2 + in-body reshaping: jizume_block emission, typed layout, warning enrichment, ruby.direction emission |
| C4 | `ab-aozora 0.5.0 aat-schema 2 facade 0.2.0 wire-schema 3` | source_note emission (terminal_provenance, placement back). Admission/activation candidate |

- Candidate-commit discipline as in Phase 3: detached hinoki builds,
  `AB_AOZORA_GIT_REV` injected, `--version` asserted before every gate run;
  code commits move the candidate, `reports/**`-only commits do not.
- `wire-schema` stays 3: nothing in Phase 4 touches the diagnostics wire
  envelope. The facade version moves only if facade code changes (not
  expected; if it does, the join key records it).
- The `ab-aozora-pipeline` `0.1.0 → 0.2.0` bump lands in the first C3
  commit wave, before any C3 evidence is gathered.
- Corpus dumps per rotation: `ab-aozora-phase4-c3-<sha7>` and
  `ab-aozora-phase4-c4-<sha7>` under `/db/ab-validator/aat-corpus/`,
  layout `<dump>/aat/ab-aozora/`. C2's dump is C3's delta baseline; C3's
  dump is C4's.

## AAT schema v2

`data/aat-schema.json` becomes the v2 contract: `$id`
`https://abc.local/schemas/aat-v2.json`, `"version": { "const": 2 }`. A
byte-frozen copy of the current v1 file lands as `data/aat-schema-v1.json`
in the same commit; every validator that can see comparison-lane AAT
selects the schema file by the document's `version` field. The comparison
lanes continue to emit v1 and are untouched.

Changes in v2 (all typed-in-place; node topology unchanged):

### jizume_block

- `block_container.kind` enum gains `jizume_block`.
- `jizume_block` requires `width` (integer, minimum 1) alongside the
  container keys (`kind`, `children`, optional `span`).
- Emission wires the Phase 3 Task 8 recognizers (`jizume_open_chars`,
  `is_jizume_close` in `crates/ab-aozora-aat/src/lib.rs`) into block
  emission, mirroring the keigakomi/yokogumi paired-only classifier path.
  Compound jisage+jizume (`［＃ここから６字下げ、折り返して７字下げ、２１字詰め］`)
  nests `jizume_block` per the existing container-nesting semantics; the
  pinned compound case (`Some(21)`, burasage `(6,7)`) must parse and emit.
- Reference universe: the 2026-07-11 attribution report's matrix-exact
  paired-jizume counts (matrix 3,239; the 1,373 `block_end`-form question
  from the backlog remains open and out of scope — unpaired forms stay
  raw-preserved).

### Typed layout (replacing `x-` layout fields)

- `jisage_block` gains required `indent` (integer, minimum 0), replacing
  `x-indent`.
- The chitsuki `style` node (`style_type: "chitsuki"`) gains required
  `align` (const `"right"`) and `offset_from_end` (integer, minimum 0),
  replacing `x-align`/`x-offset`.
- The burasage `style` node (`style_type: "burasage"`) gains required
  `indent_first` and `indent_rest` (integers, minimum 0), replacing
  `x-indent-first`/`x-indent-rest`.
- Line-jisage keeps its current node shape with a typed `indent`.
- The replaced `x-` fields are dropped from fork emission in the same
  rotation. `patternProperties ^x-` remains legal in v2; provenance fields
  (`x-provenance`, `x-source-marker-kind`) are unchanged.
- `heading` gains an optional typed `indent` (integer, minimum 0 —
  optional because heading indentation is conditional), replacing the
  `x-indent` that heading emission adds today
  (`crates/ab-aozora-aat/src/lib.rs:872`); the converter already consumes
  heading indentation as a separate concern, so this is a promotion, not a
  new fact.
- The field inventory is fixed by a **repository-wide inventory of every
  emitted `x-*` field** in `crates/ab-aozora-aat/src` (not a line-range
  read — the heading field above sits outside the block-emission region
  and proves ranges under-count). The inventory is committed as evidence
  in the schema task; every emitted `x-*` field is either promoted to a
  typed field under this rule or explicitly classified as provenance
  (`x-provenance`, `x-source-marker-kind` are the known provenance
  fields, unchanged in v2). No third category.

### Warning enrichment

v2 `warning`:

```json
{
  "required": ["code", "severity", "message"],
  "properties": {
    "code":     { "type": "string" },          // kebab-case, stable
    "severity": { "enum": ["error", "warning", "note"] },
    "message":  { "type": "string" },
    "span":     { "$ref": "#/$defs/span" },    // optional, ADR 0024 semantics
    "path":     { "type": "string" }           // optional, retained
  }
}
```

- `line` is dropped (subsumed by `span.line_start`).
- **Severity vocabulary is the producer's, verbatim**: the façade's
  `severity_str` (`crates/ab-aozora-facade/src/json.rs`) emits exactly
  `error`, `warning`, `note` (with `error` as the non-exhaustive default
  arm). The schema enum matches it one-to-one; no normalization layer.
  `error` matters: `parse_complete` already keys off error-severity
  diagnostics, so collapsing it would lose a tested signal.
- **`code` is the façade's stable kebab code passed through verbatim**
  for every façade-diagnostic-derived warning. The façade already
  computes it (`Diagnostic::code()` → kebab); the adapter today discards
  it and retains only kind/severity/span — v2 emission carries it through
  unchanged. Adapter-origin warning sites (those not backed by a façade
  diagnostic) get their own stable kebab codes under the same convention.
  No site may fall through to a placeholder code; the plan enumerates all
  warning emission sites and classifies each as façade-passthrough or
  adapter-origin.

### source_note

New block kind (top-level `block` oneOf member):

```json
{
  "required": ["kind", "placement", "region_class", "content"],
  "properties": {
    "kind":         { "const": "source_note" },
    "placement":    { "enum": ["front", "body", "back", "unknown"] },
    "region_class": { "enum": ["front_legend", "terminal_provenance",
                                "colophon_metadata", "body_end_boundary",
                                "malformed_residue"] },
    "content":      { "type": "array", "items": { "$ref": "#/$defs/inline" } },
    "span":         { "$ref": "#/$defs/span" }
  }
}
```

C4 emits only (`placement: "back"`, `region_class: "terminal_provenance"`).
The enums are deliberately wider than C4's emission: the schema is the
contract for future region classes; the delta audit and conformance gates
pin actual emission to terminal_provenance only.

### ruby.direction

Already present in v1 (`enum ["right", "left"]`); v2 is unchanged here.
Recognition is **confirmed present** in the lifted grammar
(`ab-aozora-syntax/src/lib.rs:188` `RubySide` with `Left`; the pipeline
constructs left ruby), but **the adapter cannot currently observe it**:
the façade's `AozoraNode` exposes only kind and span, and the adapter's
`ruby_node` reparses source text with a regex that recognizes only
ordinary `｜base《reading》` ruby. C3 therefore includes a **named plumbing
task**: carry ruby side — and structured base/reading, retiring the regex
reparse for ruby — through the façade (or another structured boundary),
then emit `direction: "left"` where side is Left. The task carries its
own reference counts (left-ruby forms from the construct-gap backlog) and
tests at both the façade boundary and AAT emission. This is emission
plumbing, not a recognition question; no descope path is needed.

## Rotation C3 — in-body reshaping

All schema v2 emission except source_note. Gates (all on the full 17,886-
work corpus, hinoki):

1. **Schema validation**: every C3 AAT document validates against v2.
2. **Delta audit** (`reports/aat-fidelity/audit-aat-delta.py`, extended):
   C2-dump → C3-dump comparison, exit 0/2 only, fail-closed. New rewrite
   classes, each a forward rewrite mirroring `lib.rs` semantics with deep
   equality (never invariant-only, never corpus-fitted):
   - jizume container formation (paired opens/closes become
     `jizume_block`),
   - `x-` → typed layout field migration (field-level rewrite; tree
     topology must be identical),
   - warning reshape (`meta.warnings` only),
   - ruby restructuring: ruby emission moves from regex reparse to
     structured façade facts, so the class asserts byte-identical
     base/reading for ordinary right ruby (deep equality — the structured
     path must reproduce the regex path's values exactly) and admits only
     `direction: "left"` additions on left-ruby nodes.
   Works outside these classes must be byte-identical modulo the version
   join key.
3. **Conformance**: both suites (127-vector P4suta + 30-vector seed) via
   the production lane. Where typed fields legitimately change vector
   rows, the vector expectations are updated in the same change and every
   row diff is classified (Phase 3 Task 10 precedent: 127 diffs, all
   classified). Baselines at C2: P4suta 118/9/0/0 (25/25 must), seed
   22/8/0/0. The span-deviation manifest (1 entry, P4suta-scoped) carries
   over unchanged unless a typed-layout change touches its vector, in
   which case the manifest follows the fail-closed amendment rules from
   Phase 3 (pre-committed, reviewed, staleness-checked).
4. **Perf**: pinned 6-work workset, 1 warm-up + ≥5 measured runs, median +
   spread, same recorded machine identity (hinoki). >10% median regression
   blocks; new timeouts block. Result recorded against the −4.09% Phase 3
   margin either way.
5. **Conversion audit**: mapping 0.3.0 (see below) over the C3 dump,
   17886/17886/0 required; `cargo test -p ab-aat-to-parser-ir` green.
6. **Registry row** for C3 (`:aat_version 2`, adapter version join key
   verbatim, evidence copied from the audit tool's own emitted output) —
   inert until activation, same as Phase 3's 0.2.0/0.3.0 rows.

Golden tests (`crates/ab-aozora-aat/tests/goldens.rs`) are re-baselined at
C3 under the Phase 3 tripwire discipline; `verify-golden-spans.py` remains
LF/CRLF-only unless a bare-CR golden is added, in which case it is extended
first.

## Rotation C4 — source_note

### Prerequisite: terminal_provenance / colophon measurement split

ABC policy v0.2.0 admits `terminal_provenance` and `colophon_metadata` but
records `measurement_status: "needs_measurement_split"` — prevalence claims
for the two classes are not yet separable. Before any parser emission:

- An instrument task (report-side, building on the source-region coverage
  scanner) fixes the byte-exact boundary rule between the 底本： block
  (terminal_provenance) and colophon lines (入力：/校正： etc.), publishes a
  report + summary JSON with per-class counts, and updates the coverage
  contract's compatibility notes if counters move.
- Reference: 609 `terminal_provenance_occurrences` vs 89,416
  `colophon_metadata_occurrences` in the 2026-07-06 coverage report; the
  split instrument either confirms or corrects these, and its rule is the
  **normative boundary** the parser implements.
- The policy note in `abc/data/source-region-publication-policy-v0.json`
  is updated ABC-side (measurement_status → measured) as part of the
  ceremony's ABC validation, citing the split report.

### Parser change

- The parser already locates the tail boundary during body selection; C4
  stops discarding the located tail, classifies the terminal_provenance
  block per the split rule, and appends `source_note` nodes after the last
  body block: placement `back`, region_class `terminal_provenance`,
  `content` as text inlines with real decoded-source spans (ADR 0024),
  one source_note per contiguous terminal-provenance block.
- Colophon lines and all other tail content remain outside AAT (they stay
  measured by the source-region instrument and preserved via ABC's custom
  sidecar per policy v0.2.0).
- Body selection for *parsing* is byte-identical to C3: the same body
  bytes feed the lexer; `validation_body_bytes` and `parser_body_bytes`
  are asserted unchanged work-by-work in the delta audit. No region
  content enters plaintext-relevant byte accounting.

### Gates

Same instrument set as C3, with the delta audit running in a new
**append-only confinement** mode: for every work, all pre-existing blocks
byte-identical and in order, new nodes only as trailing top-level
`source_note` nodes (schema-valid, placement/region_class pinned to the C4
emission set), plus bounded `meta` deltas (adapter_version join key,
metrics, warnings only where a documented emission-site change exists).
Works without terminal provenance must be byte-identical modulo the join
key. Conformance re-scores both suites (expected: zero row drift — vectors
contain no terminal-provenance regions; any drift blocks). Perf and
conversion audit per protocol; C4 registry row appended (inert).

## Mapping and converter

- **The shipped v1 mapping is retained byte-frozen**, not rotated in
  place: `data/aat-to-parser-ir-mapping-v1.json` stays exactly as shipped
  (0.2.8, hash `sha256:952620ce…` — the frozen bytes keep the historical
  registry rows verifiable). The v2 mapping is a **new file**,
  `data/aat-to-parser-ir-mapping-v2.json`: `mapping_id`
  `https://w3id.org/abc/mappings/aat-v2-to-parser-ir-v1/generated-probe`,
  `mapping_version` `0.3.0`, `source_aat_version` 2. `mapping_hash` is
  computed, never hand-written; the new registry rows carry the fresh
  hash.
- **Version-tuple selection precedes converter preparation.** Today
  `PreparedConverter` binds exactly one mapping to one `SchemaSet`, and
  `SchemaSet::load` hardcodes `data/aat-schema.json`. Phase 4 defines a
  version-keyed tuple table resolved from the AAT document's `version`
  field *before* preparation:
  - `1` → (`data/aat-schema-v1.json`, `data/aat-to-parser-ir-mapping-v1.json`),
  - `2` → (`data/aat-schema.json` [v2], `data/aat-to-parser-ir-mapping-v2.json`).
  Fail-closed cases, each with a test: an AAT document with an unknown
  `version` is a hard error (no default tuple); a mismatched pair —
  `mapping.source_aat_version` differing from the schema's `version`
  const or from the document's declared version — fails preflight. The
  `source_aat_version == 1` hardcode in
  `crates/ab-aat-to-parser-ir/src/mapping.rs` is replaced by this
  tuple-consistency check.
- New/changed rules with divergence accounting through the existing
  recorder (no new divergence structures):
  - `jizume_block` → parser-IR paragraph `layout` `jizume width(N)`,
  - typed layout pointers replace the `x-` layout pointer rules
    (chitsuki/burasage/jisage/line-jisage tokens unchanged on the
    parser-IR side),
  - `source_note` → parser-IR `source-note` node per the explicit rules
    below; `placement: "unknown"` must never map to ordinary body content,
  - warning enrichment accounted in the loss taxonomy (all five categories
    LOSS/INVENTION/AMBIGUITY/UNSUPPORTED/STRUCTURAL remain present).
- Converter (`ab-aat-to-parser-ir`) dispatches on the AAT document
  `version` field: v1 path byte-stable (pinned by existing fixtures and
  the frozen comparison-lane evidence), v2 path implements the new rules.
  Parser-IR `paragraphs[]` stays converter-derived from AAT blocks
  (Paragraph Authority contract; no AAT paragraph range table).

### Source-note authority: explicit v2 replaces the v1 heuristic

The v1 converter synthesizes a source note heuristically: the final
top-level paragraph, when `source_attribution_text` matches, becomes a
`source-note` node with `classification: "heuristic"`
(`crates/ab-aat-to-parser-ir/src/convert.rs:260–287`). Left ambiguous,
C3 would still synthesize source notes and C4 would duplicate or
re-classify them. The contract:

- **v1 path retains the heuristic byte-for-byte** (frozen fixtures pin it).
- **v2 path never applies the heuristic** — at any rotation. v2
  `source-note` nodes arise **only** from explicit AAT `source_note`
  blocks.
- Consequence, stated so audits read it correctly: at C3 (v2 documents,
  no `source_note` blocks yet) works whose final parenthetical paragraph
  previously triggered the heuristic legitimately convert to ordinary
  body paragraphs; the C3 conversion audit accounts for this as the
  heuristic's retirement (divergence-recorded), and the explicit
  replacement arrives at C4. No duplication path exists because the
  heuristic is dead in v2 from the first v2 document.
- Explicit v2 mapping rules (mirroring the v1 node shape so parser-IR
  consumers see one contract):
  - `type: "source-note"`,
  - `note_type: "source-attribution"` for
    `region_class: "terminal_provenance"` (the only C4 class),
  - `placement` copied verbatim from the AAT block,
  - `classification: "direct"`,
  - `source_pointer`: the structural pointer of the originating AAT
    `source_note` block (same pointer scheme the v1 heuristic uses for
    its paragraph),
  - `text`: flattened from `content` via the existing
    `visible_content_text` rule — text-inline values concatenated in
    order; non-text inlines divergence-recorded (C4 emits text-only, so
    this is a guard, not a path),
  - `span`: mapped from the AAT block span via the existing `map_span`
    rule,
  - paragraph row: each `source_note` block yields its own
    `paragraphs[]` row, `role: "source-note"`,
    `classification: "direct"`, `node_range` covering exactly its
    source-note node (satisfying ABC's paragraph-coherence rule that
    direct source-note paragraphs contain a source-note node).
- `crates/ab-aat-to-parser-ir/README.md` mapping coordinates refreshed
  (currently stale at 0.2.6 / `sha256:e36508c3…` vs shipped 0.2.8 /
  `sha256:952620ce…`) as part of the mapping rotation commit.

## Admission ceremony and atomic activation

Admission mechanics (verified against
`abc/src/abc/tools/aat_parser_ir_compat.clj`): a candidate is admitted iff
the registry contains a row whose nine match-keys (`:aat_version`,
`:aat_adapter`, `:aat_adapter_version`, `:mapping_id`, `:mapping_version`,
`:mapping_hash`, `:mapping_schema_hash`, `:parser_ir_schema_id`,
`:parser_ir_schema_hash`) match **and** whose full row — including
`:evidence_scope` — equals the candidate byte-exactly; a match-key hit with
different evidence is `:conflict`. Consequences the ceremony enforces:

- **Registry rows copy the producing tool's emitted evidence verbatim** —
  field names, numbers, and the `:corpus` label exactly as emitted, no
  prose paraphrase. This closes the Phase 3 `:corpus` trap by rule.
- Any evidence re-run that changes a number requires re-writing the row in
  the same change; the checkpoint verifier re-derives the comparison.

Ceremony order (each step blocks the next):

1. **Fresh producer reports at C4**: ir-publication-coverage (fork lane),
   source-reference reconciliation (must stay
   `SOURCE_REFERENCE_RECONCILIATION_COMPLETE`), full-corpus conversion
   audit (mapping 0.3.0), perf measurement, updated next-work ledger.
2. **C4 registry row appended** to
   `abc/data/aat-parser-ir-compatibility.edn` (`:aat_version 2`). The C3
   row was already committed with the C3 gate evidence (rotation step 6)
   — this step appends the C4 row and **re-validates both** Phase 4 rows:
   registry schema (Malli) validation green; `admission-report` for the
   C4 candidate tuple returns `:admitted` (empirically run, output
   captured as evidence).
3. **ABC design-bundle validation**: representative bundle first, then
   full-scope, with fork parser-IR; parser-IR schema hash taken fresh from
   the synced `data/abc-schemas/schemas/parser-ir.schema.json` at ceremony
   time (the 2026-07-04 handoff's hash is historical, not normative).
4. **The activation commit** — the atomic step, one commit:
   - `reports/aat-fidelity/run-sets/current.json`: `ab-aozora` becomes the
     publication lane,
   - `docs/handoffs/ir-publication-coverage-contract.md`: required-evidence
     lane set updated,
   - justfile lane wiring: default measurement/publication recipes point
     at `ab-aozora`,
   - `examples/ab-validator-output/` fixtures regenerated at C4.
   Everything before this commit leaves the legacy lane as default;
   everything after it treats `ab-aozora` as the publication parser.
   **The activation commit must not reach `main` until steps 5 and 6
   pass.** It is authored on the phase branch; the wholesale gate and the
   checkpoint run against it there. If a post-activation gate fails, the
   activation commit (and anything stacked on it) is reverted or dropped
   on the branch before any merge — `main` never sees an activated state
   whose wholesale gate did not pass.
5. **Acceptance gate wholesale** (2026-07-06 criteria):
   `unsupported_body_markup_occurrences == 0`,
   `unknown_region_occurrences == 0`,
   `unknown_unreviewed_occurrences == 0`, reconciliation COMPLETE,
   parser-IR schema-valid with every source-observed fact in an admitted
   publication lane, representative + full-scope bundle validation green,
   and the next-work ledger items **cited and closed, not replaced**.
6. **`verify-phase4-checkpoint.py`** (new, in the Phase 3 verifier's
   mold, with tests): binds C3/C4 shas and version join keys, verifies all
   gate summaries (per-rotation delta/conformance/perf), both conversion
   audits, the measurement-split report, the captured `admission-report`
   output, the activation-commit content (run-set + contract + fixtures
   actually flipped), and the acceptance-criteria numbers — substantive
   checks, not existence checks; independently re-runnable; pairwise-
   distinct candidates enforced.

## Legacy-lane retirement (final task)

Only after the checkpoint is OK:

- Delete `adapters/aozora` (crate + tests).
- Remove the `upstream-aozora-src` flake input and all `AB_AOZORA_BIN`
  bindings (`flake.nix` 1747/1761/2002 at the Phase 3 tip) and any script
  references (`run-aat-full.sh` adapter menu, cross-adapter wiring keeps
  only live lanes).
- Update the coverage contract and handoffs to record the retirement.
- **Never touched**: frozen dumps under `/db/ab-validator/aat-corpus/`
  (`aozora-full-repin-1a4f864`, `aozora-fork-parity-2263b92a`,
  `ab-aozora-phase2-9cbb7b7b`, `ab-aozora-phase3-capability-a81edf0`,
  `ab-aozora-phase3-span-a3f91f5`, plus the Phase 4 dumps), frozen dated
  reports, historical registry rows (v1 rows for all adapters stay —
  history is never rewritten).
- Rollback stance: after this task the legacy lane is no longer
  reproducible from HEAD (the flake input is gone); rollback means `git
  revert` of the retirement + activation commits. The user accepts this by
  approving the phase design; the plan places the task last so every gate
  has already passed.

## Performance

Protocol unchanged from the master spec: pinned 6-work workset, `--release`
without sccache, recorded toolchain and machine identity (hinoki), 1
warm-up + ≥5 measured runs, median + spread. >10% median regression blocks
the rotation; new timeouts block unconditionally. Watch item carried in:
Phase 3 rotation B ended at −4.09% median (candidate faster) with 3/6 works
individually regressed (worst +34.77%). Both rotations re-measure; results
are recorded against this margin. If C4 measures slower than the legacy
lane at median, an optimization task is inserted before the activation
commit (decision 4) — activation never ships on a candidate that lost the
median to the lane it replaces.

## Testing summary

- Workspace suites + `clippy -D warnings` clean at every task (Phase 3
  discipline; lint debt is fixed in the wave that creates it).
- Goldens re-baselined per rotation; preserve_order guard + canary + exact-
  bytes tripwire maintained (tripwire re-baselines with each candidate).
- Both conformance suites per rotation with classified row diffs.
- Delta audit extended per rotation (new classes above), grammar fidelity
  derived from `lib.rs` semantics only; 20-test baseline in
  `test_audit_aat_delta.py` grows with each new class.
- Conversion audit per rotation; converter unit/integration suites green
  (89 tests at the Phase 3 tip, growing with v2 rules).
- New instrument tests: measurement-split report, checkpoint verifier.
- ABC-side: registry Malli validation, `admission-report` capture, kaocha
  suite, ADR governance check, design-bundle validations.

## Risks and open points

- **Byte-exact admission equality** is the sharpest edge: any drift
  between emitted evidence and the registry row is `:conflict`. Mitigated
  by the verbatim-copy rule and by the checkpoint verifier re-deriving the
  comparison from the artifacts.
- **Source_note boundary correctness** rides on the measurement split;
  that instrument lands first and its rule is normative for the parser.
- **Perf erosion**: v2 emission adds work in the span-heavy path that
  already thinned to −4.09%. Measure-first per decision 4; the activation
  bar (not slower than the legacy lane at median) is a hard floor.
- **Converter dual-version regressions**: the v1 path is pinned by
  existing fixtures; any v1 output change in the conversion suites blocks.
- **ABC plaintext text vs policy (resolved as an assertion + stale doc)**:
  the 2026-07-04 Level-3 delta handoff says plaintext appends
  back-placement source notes after the body, while policy v0.2.0 says
  `plaintext_projection: "omit"` for terminal_provenance — and ABC's
  current code already omits source-note from plaintext, matching the
  policy and the acceptance criteria. Phase 4 therefore (a) makes
  **plaintext omission an explicit activation assertion** — the bundle
  validation step asserts no source-note text appears in plaintext output
  — and (b) tracks the delta handoff's plaintext sentence as **stale
  documentation** for an ABC-side correction. ab-validator's own
  instruments exclude source_note from body byte accounting regardless.
- **Ruby plumbing regressions**: retiring the regex reparse in favor of
  structured façade facts touches every ruby node in the corpus; the
  delta-audit ruby class (byte-identical base/reading for right ruby)
  is the guard, and the façade-boundary tests pin the new surface.
- **Registry `:aat_version 2` schema**: abc's Malli entry schema may pin
  `:aat_version` to 1; if so, the ABC-side schema update is part of step 2
  of the ceremony, validated by the registry check.

## References

- Master design: `docs/superpowers/specs/2026-07-10-consolidated-parser-design.md`
- Acceptance criteria: `docs/superpowers/specs/2026-07-06-comprehensive-parser-acceptance-criteria.md`
- Coverage contract: `docs/handoffs/ir-publication-coverage-contract.md`
- Source-region policy: `abc/data/source-region-publication-policy-v0.json` (v0.2.0)
- Level-3 parser-IR delta: `abc/docs/handoffs/parser-ir-level3-structure-delta.md`
- Attribution report: `docs/superpowers/reports/2026-07-11-keigakomi-yokogumi-denominator-attribution.md`
- Phase 3 closure: `docs/handoffs/2026-07-10-parser-fork-provenance.md`
- Registry + admission code: `abc/data/aat-parser-ir-compatibility.edn`,
  `abc/src/abc/tools/aat_parser_ir_compat.clj`
- ADRs: 0023 (registry/admission), 0024 (span semantics), 0032 (fork
  provenance), 0001 (manifest identity)
