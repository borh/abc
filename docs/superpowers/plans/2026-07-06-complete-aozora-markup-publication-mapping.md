# Complete Aozora Markup Publication Mapping Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement implementation slices derived from this plan. This document is the reviewable overarching plan; each implementation slice should get its own focused task plan before code changes.

**Goal:** Map all observed Aozora Bunko markup through parser-IR into TEI P5 where faithful, or into an ABC custom schema/sidecar/diagnostic contract where TEI is not exact.

**Architecture:** ab-validator remains the measurement and admission-gate repo. ABC remains the owner of TEI profile, renderer behavior, custom preservation schema, and design-bundle validation. The goal is reached by keeping source authority, parser evidence, IR coverage, TEI/profile policy, custom preservation, and plaintext policy synchronized as one publication contract.

**Tech Stack:** Rust crates in `crates/`, shell smoke tests in `tests/`, Python report builders under `reports/`, JSON schemas under `data/`, ABC schema snapshots under `data/abc-schemas/`, and ABC-owned Clojure/Nix validation in the sibling `../abc` repo.

**Genre:** This is an overarching roadmap/meta-plan. It is intentionally not a checkbox implementation plan; each workstream that changes code should get a focused sibling implementation plan using the repo's `### Task N` checkbox convention.

## Global Constraints

- TEI-EAJ is calibration evidence, not source authority.
- The success definition is Aozora markup publication accounting, not TEI Level 2/3 parity.
- Plaintext is visible body text only: no ruby readings, source apparatus, source notes, layout metadata, custom records, warnings, or provenance.
- All five parser lanes must stay visible in evidence when available: `aozora2html`, `aozora-epub3`, `aozora-rs`, `aozora2`, and `aozora`.
- Parser outputs are supporting evidence; source inventory remains authoritative for observed Aozora markup.
- Custom preservation is not a dumping ground; every custom fact must have schema, hash, owner, source pointer or reason for absence, and validation.
- Legacy source-authority counters remain compatibility aliases until ABC confirms migration to `source_region_coverage`.
- Generated corpus artifacts stay out of `data/`; committed reports are summaries and contracts.

---

## Current State

The current measured reports already satisfy the ab-validator-side coverage gate:

- `docs/superpowers/reports/2026-07-06-ir-publication-coverage.summary.json`
  - `verdict == "IR_PUBLICATION_COVERAGE_COMPLETE"`
  - `parser_evidence_coverage.verdict == "FIVE_PARSER_EVIDENCE_COMPLETE"`
  - required parsers: `aozora2html`, `aozora-epub3`, `aozora-rs`, `aozora2`, `aozora`
  - `closure_gaps.classified_but_not_admitted.count == 0`
  - `closure_gaps.true_unsupported_gaps.count == 0`
- `docs/superpowers/reports/2026-07-04-source-authority-representability.summary.json`
  - `schema_version == "aozora-source-region-coverage-v1"`
  - `gate_status == "SOURCE_AUTHORITY_GATE_PASS"`
  - `works_scanned == 17894`
  - `unallowlisted_unknown_markers_total == 0`
  - `source_region_coverage.unsupported_body_markup_occurrences == 0`
  - `source_region_coverage.unknown_region_occurrences == 0`
  - `source_region_coverage.unknown_unreviewed_occurrences == 0`

That is a measured admission state, not the end of the project. The remaining work is durability: ABC must consume the source-region contract, cross-artifact validation must prove the outputs agree, and adapter/parser evidence must stay current as inputs change.

## Workstream 1: ABC Source-Region Integration

**Purpose:** Make ABC consume source-region evidence as a first-class publication input.

**Inputs:**

- `docs/handoffs/source-region-coverage-abc-integration.md`
- `docs/superpowers/specs/2026-07-06-aozora-source-region-and-apparatus-contract.md`
- `docs/superpowers/reports/2026-07-04-source-authority-representability.summary.json`

The canonical vocabulary for source regions and apparatus kinds is the source
region contract spec. The handoff and this roadmap are snapshots for ABC
coordination and must be reconciled with that spec if the vocabulary changes.

**Required ABC outcomes:**

- Accept `aozora-source-region-coverage-v1`.
- Require `SOURCE_AUTHORITY_GATE_PASS` for full publication admission.
- Require zero unsupported body markup, unknown region rows, and unknown unreviewed rows.
- Admit disposition policy for:
  - `notation_legend`
  - `notation_placeholder`
  - `body_end_boundary`
  - `terminal_provenance`
  - `colophon_metadata`
  - `malformed_source`
- Do not infer terminal-provenance or colophon prevalence from the current
  `back_matter_occurrences: 243`; the current report's back-matter count is
  exhausted by `body_end_boundary` evidence and needs a separate split before
  prevalence claims.
- Preserve malformed-source residues as diagnostics, not unsupported syntax.
- Keep plaintext body-only.

**ab-validator follow-up after ABC lands:**

- Sync changed ABC schemas/profile snapshots into `data/abc-schemas/`.
- Regenerate the source-authority and IR publication coverage reports.
- Update the report gate so source-apparatus dispositions are confirmed by ABC evidence rather than only pending policy.

**Review gate:** ABC sends commit hash, schema/profile hashes, fixture paths, and validation output. ab-validator reports still show `IR_PUBLICATION_COVERAGE_COMPLETE` after syncing those artifacts.

## Workstream 2: Cross-Artifact Publication Bundle Validation

**Purpose:** Prove parser-IR, TEI, custom preservation, source-region evidence, and plaintext are mutually consistent.

**Validation target:**

- Parser-IR validates against the pinned schema hash.
- TEI validates against ABC's TEI profile and Schematron.
- Preservation sidecar validates against ABC's custom schema.
- Every sidecar record with a TEI projection resolves to a TEI pointer.
- Every TEI `abc:*` projection resolves to a sidecar record.
- Every sidecar/source-region record resolves to either a source pointer, source-inventory row, or an explicit reason why no source pointer exists.
- Plaintext contains body-visible text only.

**Implementation shape:**

- Prefer ABC design-bundle validation as the owner of cross-file validation.
- Keep ab-validator smoke tests focused on measuring and checking the generated reports.
- Add ab-validator checks only for facts ab-validator owns: source-region counts, parser evidence completeness, mapping/coverage report shape, and synced schema hashes.

**Review gate:** A fixture bundle with source apparatus, body markup, TEI projection, sidecar records, and plaintext passes ABC validation and is cited by path/hash in ab-validator's report.

## Workstream 3: Parser Evidence And Adapter Fidelity

**Purpose:** Keep the five-parser evidence matrix useful without confusing parser consensus with source authority.

**Current parser evidence lanes:**

- `aozora2html`
- `aozora-epub3`
- `aozora-rs`
- `aozora2`
- `aozora`

**Priority order:**

1. Keep full-corpus AAT and AAT-to-parser-IR audits reproducible for all five lanes.
2. Keep performance numbers as comparison evidence, with DNF as useful signal rather than a reason to wait indefinitely.
3. Investigate adapter-specific distortions only when they affect publication accounting:
   - paragraph segmentation
   - source-note and back-matter routing
   - quote/caption association
   - layout markers such as tcy, yokogumi, keigakomi, and jisage
   - raw-source recovery and gaiji resolution
4. Do not require parsers to produce TEI-EAJ editorial enrichment such as people, places, speakers, roles, or speech acts.

**Review gate:** Missing parser evidence is explicit in the report. Parser failures are separated from IR/schema/policy gaps. Source-authority coverage does not depend on parser consensus.

## Workstream 4: TEI P5 And ABC Extension Policy

**Purpose:** Keep TEI as the primary publication surface without forcing non-TEI facts into misleading TEI elements.

**TEI-first facts:**

- text
- ruby base/reading
- gaiji where TEI `g`/`charDecl` is faithful
- page and line breaks
- paragraphs
- headings
- images and captions
- quotes where source evidence supports the target

**TEI policy projection facts:**

- source-note placement
- paragraph layout and indentation
- emphasis and style rendition
- quote/caption policy
- source apparatus routed to TEI header/front/back when ABC admits that route

**TEI plus ABC extension or sidecar facts:**

- accent source code
- warigaki split-line identity
- tcy/yokogumi/keigakomi source identity
- gaiji resolution status when TEI visible text is insufficient
- source pointers and source-inventory row IDs

**Custom-sidecar facts:**

- mapping identity and hashes
- parser/adapter identity
- divergence records
- warning/error diagnostics
- span coordinates when TEI does not preserve exact coordinate semantics
- producer metrics and provenance

These buckets are planning vocabulary. The report gate must still fold them
back into the canonical classes from the publication contract:
`tei_exact`, `tei_policy_projection`, `tei_plus_abc_extension`,
`custom_sidecar`, `plaintext_only`, or `unsupported_gap`.

**Review gate:** The coverage report has no `classified_but_not_admitted` rows and no true unsupported gaps after ABC profile/custom-contract hashes are synced.

## Workstream 5: Source Inventory And Specification Drift

**Purpose:** Prevent new source syntax or parser updates from silently escaping coverage.

**Maintenance triggers:**

- Aozora corpus update.
- Source scanner change.
- New parser adapter.
- AAT schema or parser-IR schema change.
- ABC TEI profile or custom preservation schema change.
- New external notation/spec source, including `P4suta/aozora-notation-spec`.

**Required action on each trigger:**

1. Regenerate source-authority report.
2. Regenerate parser evidence reports for affected adapters.
3. Regenerate IR publication coverage report.
4. Compare source-inventory rows against admitted TEI/custom classes.
5. Fail the gate on any unreviewed source marker, unknown region, unsupported body markup, or unmapped publication family.

**Review gate:** Updated reports include the new input identity and either preserve `IR_PUBLICATION_COVERAGE_COMPLETE` or identify a named blocker with owner and prevalence.

## Workstream 6: Goal Definition And Non-Goals

**Goal statement to keep using:**

Complete Aozora Bunko markup publication mapping: every observed source markup and parser-IR fact is represented by TEI P5, TEI plus ABC extension, custom sidecar, plaintext body text, diagnostic preservation, or an explicit unsupported blocker.

**Do not use as the goal:**

- "TEI Level 2/3 parity"
- "TEI-EAJ exact match"
- "parser consensus"
- "all TEI P5 semantic enrichment"

**Explicit non-goals:**

- Inferring people, places, speakers, roles, or speech acts from plain text.
- Putting ruby readings or metadata into plaintext.
- Treating front/back/source apparatus as malformed source.
- Treating malformed source residue as unsupported Aozora markup.

## Immediate Next Tasks

1. Send `docs/handoffs/source-region-coverage-abc-integration.md` to the ABC implementor.
2. Wait for ABC to decide and implement source-apparatus dispositions.
3. Sync ABC schema/profile/hash evidence back into ab-validator.
4. Regenerate:
   - `docs/superpowers/reports/2026-07-04-source-authority-representability.summary.json`
   - `docs/superpowers/reports/2026-07-04-source-authority-representability.md`
   - `docs/superpowers/reports/2026-07-06-ir-publication-coverage.summary.json`
   - `docs/superpowers/reports/2026-07-06-ir-publication-coverage.md`
5. Add or tighten cross-artifact checks only after ABC exposes the validation surface.

## Verification Commands

Run these in ab-validator after each contract-affecting change:

```bash
bash tests/source-representability-gate-smoke.sh
bash tests/source-inventory-smoke.sh
jq -e '.gate_status == "SOURCE_AUTHORITY_GATE_PASS"' docs/superpowers/reports/2026-07-04-source-authority-representability.summary.json
jq -e '.source_region_coverage.unsupported_body_markup_occurrences == 0 and .source_region_coverage.unknown_region_occurrences == 0 and .source_region_coverage.unknown_unreviewed_occurrences == 0' docs/superpowers/reports/2026-07-04-source-authority-representability.summary.json
jq -e '.verdict == "IR_PUBLICATION_COVERAGE_COMPLETE"' docs/superpowers/reports/2026-07-06-ir-publication-coverage.summary.json
git diff --check
```

Run this when Nix checks are in scope:

```bash
system="$(nix eval --impure --raw --expr builtins.currentSystem)"
nix build ".#checks.${system}.source-representability-gate" --print-build-logs
```

## Self-Review

- Scope: this plan covers the overall publication goal and deliberately defers per-slice implementation details to focused task plans.
- Source authority: Aozora source inventory remains the authority for source constructs.
- TEI-EAJ: comparison evidence only, not the goal.
- Plaintext: remains metadata-free visible body text.
- Parser evidence: all five active lanes are named.
- Cross-repo boundary: ABC owns disposition, renderer, profile, custom schema, and bundle validation; ab-validator owns measurement and admission reports.
- Placeholder scan: no unresolved placeholder markers.
