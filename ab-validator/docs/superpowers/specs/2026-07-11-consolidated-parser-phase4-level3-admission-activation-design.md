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
- The exact field inventory is fixed at plan time by reading the current
  emission sites in `crates/ab-aozora-aat/src/lib.rs` (jisage
  495–510, chitsuki 583–592, burasage 614–623, indent helpers 628–672 at
  the Phase 3 tip); any `x-` layout field found there beyond the list above
  is promoted under the same rule, not left behind.

### Warning enrichment

v2 `warning`:

```json
{
  "required": ["code", "severity", "message"],
  "properties": {
    "code":     { "type": "string" },          // kebab-case, stable
    "severity": { "enum": ["warning", "info"] },
    "message":  { "type": "string" },
    "span":     { "$ref": "#/$defs/span" },    // optional, ADR 0024 semantics
    "path":     { "type": "string" }           // optional, retained
  }
}
```

- `line` is dropped (subsumed by `span.line_start`).
- Codes follow the kebab convention established by wire-schema 3
  diagnostics; every current warning emission site gets a stable code.
  The plan enumerates the sites and their codes from `lib.rs`; no site may
  fall through to a placeholder code.
- Severity vocabulary is fixed at plan time from the actual emission sites;
  if only `warning` is ever emitted, the enum still ships both values (the
  schema is the contract, emission may cover a subset).

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

Already present in v1 (`enum ["right", "left"]`); v2 is unchanged here. The
Phase 4 work is emission: C3 emits `direction: "left"` for left-ruby
constructs the CST already distinguishes. Whether the lifted grammar
recognizes left ruby is verified at plan time from
`ab-aozora-pipeline`/`ab-aozora-syntax` sources; if recognition is absent,
left-ruby emission becomes a named C3 sub-task with its own reference
counts (from the construct-gap backlog), not a silent omission. If the
construct is genuinely unrepresented in the CST, `direction` emission is
descoped by an explicit spec amendment, never quietly.

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
   - ruby.direction additions (field addition on ruby nodes only).
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

- Mapping document `data/aat-to-parser-ir-mapping-v1.json` rotates:
  `mapping_id` `https://w3id.org/abc/mappings/aat-v2-to-parser-ir-v1/generated-probe`,
  `mapping_version` `0.3.0`, `source_aat_version` 2. The hardcoded
  `source_aat_version == 1` preflight in
  `crates/ab-aat-to-parser-ir/src/mapping.rs` becomes version-aware
  (accepts the document's declared version against the schema file it
  pairs with; still fail-closed on mismatch). `mapping_hash` is computed,
  never hand-written; the registry rows carry the fresh hash.
- New/changed rules with divergence accounting through the existing
  recorder (no new divergence structures):
  - `jizume_block` → parser-IR paragraph `layout` `jizume width(N)`,
  - typed layout pointers replace the `x-` layout pointer rules
    (chitsuki/burasage/jisage/line-jisage tokens unchanged on the
    parser-IR side),
  - `source_note` → parser-IR `source-note` node with placement preserved;
    `placement: "unknown"` must never map to ordinary body content,
  - warning enrichment accounted in the loss taxonomy (all five categories
    LOSS/INVENTION/AMBIGUITY/UNSUPPORTED/STRUCTURAL remain present).
- Converter (`ab-aat-to-parser-ir`) dispatches on the AAT document
  `version` field: v1 path byte-stable (pinned by existing fixtures and
  the frozen comparison-lane evidence), v2 path implements the new rules.
  Parser-IR `paragraphs[]` stays converter-derived from AAT blocks
  (Paragraph Authority contract; no AAT paragraph range table).
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
2. **Registry rows** for C3 and C4 in
   `abc/data/aat-parser-ir-compatibility.edn` (`:aat_version 2`); registry
   schema (Malli) validation green; `admission-report` for the C4
   candidate tuple returns `:admitted` (empirically run, output captured
   as evidence).
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
- **ABC renderer/policy discrepancy (ABC-owned, non-blocking here)**: the
  2026-07-04 Level-3 delta handoff says plaintext appends back-placement
  source notes after the body, while policy v0.2.0 says
  `plaintext_projection: "omit"` for terminal_provenance. ab-validator's
  instruments treat source_note as excluded from body byte accounting
  either way; ABC resolves the rendering question on its side before
  making plaintext claims for source notes. Recorded here so the ceremony
  review checks it was raised with ABC.
- **`ruby.direction` recognition** may turn out absent from the lifted
  CST; handled by the named verification step in the schema section
  (explicit sub-task or explicit descope amendment, never silent).
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
