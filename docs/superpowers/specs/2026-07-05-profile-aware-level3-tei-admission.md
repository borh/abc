# Profile-Aware Level 3 TEI Admission

Status: Proposed for review
Date: 2026-07-05
Owner boundary: ABC owns TEI publication policy and parser-IR admission semantics. ab-validator owns measured source-authority, adapter, AAT-to-parser-IR, and generated-TEI comparison evidence.

## Problem

The parser-IR Level 3 schema delta has landed and ab-validator can now materialize parser-IR through ABC TEI for the TEI-EAJ workset. The old generic blocker, "parser-IR cannot represent paragraphs or final source attribution," is no longer the current state.

Current evidence:

- `docs/superpowers/reports/2026-07-04-source-authority-representability.md`
  - source-authority gate: `SOURCE_AUTHORITY_GATE_PASS`
  - works scanned: 17,894
  - unallowlisted unknown source markers: 0
- `docs/superpowers/reports/2026-07-04-tei-eaj-structural-expansion.md`
  - TEI-EAJ files: 62
  - rows with AAT evidence: 57
  - parser-IR gap rows: 0
  - source-attribution gap rows: 0
  - adapter gap rows: 32
  - evidence gap rows: 5
- `docs/superpowers/reports/2026-07-04-tei-eaj-generated-matrix-comparison.md`
  - parser-input rows attempted: 173
  - materialized through parser-IR and ABC TEI: 173
  - materialization failures: 0
  - skipped rows: 5
  - TEI-EAJ profile buckets: plain prose 81, drama 30, Level 4 enrichment 29, notes 21, front/back matter 6, verse 6
  - paragraph origin buckets: adapter over-segmented 123, adapter collapsed 18, adapter under-segmented 7, aligned 18, source-note back routing 4, page-break projection 3

The current blocker is admission semantics. The TEI-EAJ workset mixes plain prose, drama, verse, notes, front/back matter, and Level 4 enrichment. A raw TEI `<p>` count comparison is not a valid Level 3 gate across those profiles. It conflates:

- parser-IR representability,
- adapter paragraph segmentation,
- source-note back-matter routing,
- page-break projection,
- TEI-EAJ editorial structure,
- TEI-EAJ Level 4 semantic enrichment.

The next decision must define which profile surfaces are Level 3 parser compatibility requirements and which are separate policy lanes.

## Non-Goals

- Do not claim full TEI Level 4 or Level 5 semantic enrichment from Aozora Bunko markup. Named entities, roles, places, speech attribution, and other semantic annotations require editorial understanding unless the source markup explicitly encodes them.
- Do not use TEI-EAJ as source authority. TEI-EAJ is comparison evidence. Aozora source inventory is the authority for what source constructs must be representable.
- Do not choose the final Aozora parser in this spec.
- Do not make paragraph-count parity the universal gate.
- Do not require adapters to reproduce TEI-EAJ editorial segmentation for profiles whose TEI structure is not source-marked in Aozora.
- Do not add parser-IR schema features here. The schema delta is separate; this spec defines admission policy over the measured outputs.
- Do not treat ruby readings, source notes, provenance, or other metadata as plaintext output. Plaintext admission uses body base text only.

## Decision

Adopt a profile-aware Level 3 admission model.

Use `plain_prose` as the first hard Level 3 adapter-admission gate. Keep drama, verse, notes, front/back matter, and Level 4 enrichment in explicit policy lanes until each lane has source-authority-backed TEI P5 mapping rules.

This creates two separate verdicts:

1. `LEVEL3_IR_INFRASTRUCTURE_READY`
   - Parser-IR can carry paragraph/source-note structure.
   - AAT-to-parser-IR conversion and ABC TEI materialization succeed for the measured candidates.
   - This is already supported by the current 173-row generated-TEI matrix.

2. `LEVEL3_PLAIN_PROSE_ADMITTED`
   - An adapter or parser input satisfies the plain-prose Level 3 gate over its declared evidence scope.
   - This is not currently proven for the broad adapters because most plain-prose paragraph mismatches originate before parser-IR, at the adapter/AAT boundary.

Do not collapse those verdicts. Parser-IR infrastructure readiness is a platform capability. Adapter admission is a measured claim about a concrete producer/version/corpus scope.

## Profile Classification

Every TEI-EAJ comparison row used for admission must have one primary profile.

Profiles:

- `plain_prose`: prose body where paragraph/source-note/text policy is the relevant Level 3 surface.
- `drama`: rows containing drama structure such as `sp`, `speaker`, or `stage`.
- `verse`: rows containing verse structure such as `lg` or `l`.
- `notes`: rows dominated by note-like or apparatus-like structures.
- `front_back_matter`: rows where front/back divisions are the primary structural issue.
- `lv4_enrichment`: rows where the mismatch is primarily semantic/editorial enrichment such as `persName`, `placeName`, `roleName`, `said`, or equivalent.

The profile classifier may use TEI-EAJ tags for comparison classification, but admission requirements must be stated in source/IR terms. A TEI-EAJ tag alone does not prove the Aozora source carries that semantic fact.

## Plain Prose Gate

The first hard gate is `plain_prose`.

A parser input passes `LEVEL3_PLAIN_PROSE_ADMITTED` for a declared evidence scope only when every in-scope plain-prose row satisfies all of the following:

1. Source authority has passed for the corpus snapshot.
   - Current baseline: `SOURCE_AUTHORITY_GATE_PASS`, 17,894 works, 0 unallowlisted unknown source markers.

2. Parser-IR representation is present.
   - Parser-IR validates against the current schema hash.
   - `paragraphs[]` is populated for body paragraph material.
   - Body paragraph rows have monotonic, non-overlapping `node_range` values.
   - Final/source attribution, when detected, is represented as `source-note`, not ordinary body text.

3. Materialization succeeds.
   - AAT-to-parser-IR conversion succeeds.
   - ABC TEI materialization succeeds.
   - The row has no parser-IR gap and no materialization failure.

4. Paragraph origin is acceptable.
   - `aligned` passes.
   - `source_note_back_routing` passes when the only delta is moving a source note out of body text according to `source-note.placement = "back"`.
   - `page_break_projection` is not a body paragraph failure; it is tracked in its own page-break policy bucket.
   - `adapter_over_segmented`, `adapter_under_segmented`, and `adapter_collapsed` fail adapter admission for that adapter/parser input.

5. Text policy is declared and satisfied.
   - `base_equal` passes.
   - `ruby_expanded_equal` is diagnostic only and does not pass plaintext admission. Plaintext must not include ruby readings.
   - `ruby_expanded_parenless_equal` is diagnostic only and does not pass plaintext admission. Plaintext must not include ruby readings or comparison-only parenthesis normalization.
   - `base_drop_parentheticals_equal` may pass only when the dropped parenthetical is represented as source-note/source-attribution metadata and is excluded from plaintext by that typed policy. It must not become a blanket deletion rule.
   - `generated_contains_tei_eaj`, `tei_eaj_contains_generated`, and `different` fail until a narrower source-text policy or adapter bug classification explains them.

The gate must report failures by owner:

- `adapter`: paragraph segmentation or text content already differs in AAT/source-derived evidence.
- `parser_ir`: parser-IR lost structure that AAT provided.
- `abc_renderer`: parser-IR was correct but ABC TEI materialization changed the structure incorrectly.
- `policy`: the row needs a profile/text policy decision before it can be judged.
- `evidence`: the row has no source/AAT evidence or no durable work identity.

The headline plain-prose verdict must not hide independent blocker classes. When more than one owner blocks admission, the summary must also emit `blocking_owners` and either use a composite verdict or otherwise make every blocking owner visible in the first screen of the report.

## Separate Policy Lanes

### Drama

Drama is not judged by body paragraph parity. TEI P5 drama structures such as `sp`, `speaker`, and `stage` can be represented when the source markup or parser can identify them, but Aozora plain text alone does not reliably encode speaker/stage semantics.

Drama lane decisions:

- Source-marked drama constructs become parser/IR requirements when the source-authority matrix identifies them.
- Unmarked speaker/stage enrichment remains editorial or Level 4+ work.
- Until a drama policy exists, drama rows cannot block plain-prose Level 3 admission.

### Verse

Verse is not judged by prose paragraph parity. TEI P5 line structures such as `lg` and `l` should be emitted when Aozora source markup or parser evidence identifies verse/line structure.

Verse lane decisions:

- Explicit line break and indentation/source layout markers remain representable via existing source-authority rows.
- Inferring poetic line groups from plain text is not a parser compatibility requirement unless ABC promotes it as an editorial policy.

### Notes

Notes require a note policy, not paragraph parity.

Notes lane decisions:

- Source-marked notes should map to TEI note-like constructs according to the syntax row's TEI projection.
- Unmarked TEI-EAJ apparatus/editorial notes are comparison evidence, not automatic parser requirements.
- `source-note` remains separate from `editor-note` because it changes body base-text placement.

### Front And Back Matter

Front/back matter requires a placement policy.

Front/back lane decisions:

- Source-note placement values `front`, `body`, `back`, and `unknown` must drive TEI placement.
- Bibliographic/provenance metadata outside the body remains out-of-body unless source inventory marks it as body material.
- ABC owns which source metadata becomes TEI header, front matter, back matter, or excluded provenance.

### Level 4 Enrichment

Level 4 enrichment is not a parser compatibility gate unless source markup explicitly encodes the fact.

Examples:

- `persName`
- `placeName`
- `roleName`
- `said`
- inferred speaker identity
- inferred place or person references

These are valuable TEI-EAJ comparison signals but require semantic/editorial understanding. They should not block Level 2/3 parser-IR admission.

## Evidence Gaps

The five no-work-ID TEI-EAJ rows remain evidence gaps, not parser-IR failures:

- `data/draft/tei_lib_lv2/01.xml`
- `data/draft/tei_lib_lv2/02.xml`
- `data/draft/tei_lib_lv2/yosano_genji_kiritsubo_ids.xml`
- `data/etc/Curriculum vitae of Wakugawa Pēchin, Jitchaku Village.xml`
- `data/etc/校異源氏物語_header更新版.xml`

ABC should either classify those rows as out-of-scope for Aozora parser compatibility or export durable source-material mappings. ab-validator should not fabricate identity for them.

## Report Contract

The next implementation should add a machine-readable admission summary alongside the existing generated-TEI matrix report.

Required top-level fields:

- `schema_version`
- `source_authority_gate`
- `parser_ir_infrastructure_verdict`
- `plain_prose_admission`
- `profile_lanes`
- `evidence_gaps`
- `mapping`
- `inputs`

Required `plain_prose_admission` fields:

- `scope`
- `rows_total`
- `rows_passed`
- `rows_failed`
- `verdict`
- `blocking_owners`
- `failures_by_owner`
- `failures_by_adapter`
- `text_policy_buckets`
- `paragraph_origin_buckets`

Required `mapping` fields:

- `mapping_id`
- `mapping_version`
- `mapping_hash`
- `mapping_schema_hash`
- `target_parser_ir_schema_id`
- `target_parser_ir_schema_hash`
- `generated_mapping_rules`

Plaintext policy fields:

- `plaintext_surface = "body_base_text"`
- `ruby_expanded_surfaces = "diagnostic_only"`
- `metadata_policy = "exclude_typed_metadata_from_plaintext"`

Recommended verdict values:

- `LEVEL3_PLAIN_PROSE_ADMITTED`
- `LEVEL3_PLAIN_PROSE_BLOCKED_ADAPTER_FIDELITY`
- `LEVEL3_PLAIN_PROSE_BLOCKED_TEXT_POLICY`
- `LEVEL3_PLAIN_PROSE_BLOCKED_EVIDENCE`
- `LEVEL3_PLAIN_PROSE_BLOCKED_ADAPTER_FIDELITY_AND_TEXT_POLICY`
- `LEVEL3_PLAIN_PROSE_BLOCKED_ADAPTER_FIDELITY_AND_TEXT_POLICY_AND_EVIDENCE`
- `LEVEL3_PLAIN_PROSE_NOT_EVALUATED`

Required lane verdicts:

- `LANE_POLICY_REQUIRED`
- `LANE_OUT_OF_SCOPE_FOR_LEVEL3`
- `LANE_READY_FOR_GATE_IMPLEMENTATION`
- `LANE_BLOCKED_EVIDENCE`

## Implementation Consequences

The next implementation plan should:

1. Update stale post-conversion and gap-analysis reports so source-authority is no longer listed as failing.
2. Add a profile-aware admission classifier over `docs/superpowers/reports/2026-07-04-tei-eaj-generated-matrix-comparison.summary.json`.
3. Emit a new admission JSON and Markdown report.
4. Add a smoke fixture proving:
   - plain prose aligned rows can pass,
   - adapter over/under/collapsed rows fail adapter admission,
   - drama/verse/Level 4 rows go to policy lanes instead of failing plain prose,
   - source-note back routing is not counted as a body paragraph failure,
   - unresolved evidence gaps stay separate.
5. Keep ABC registry/manifest hardening separate. Admission reports can cite compatibility evidence, but ABC owns registry acceptance.

## Self-Review

- No placeholder fields remain.
- The spec does not claim full Level 3 admission today; it separates parser-IR infrastructure readiness from adapter admission.
- The spec uses current source-authority evidence: `SOURCE_AUTHORITY_GATE_PASS`.
- The spec does not require semantic TEI Level 4 enrichment from Aozora markup.
- The spec does not treat TEI-EAJ paragraph count parity as a universal gate.
- The first hard gate is intentionally narrow: `plain_prose`.
