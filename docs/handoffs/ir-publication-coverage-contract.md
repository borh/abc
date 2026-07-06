# Aozora Markup To TEI/Custom Publication Coverage Handoff

ab-validator now treats TEI-EAJ as calibration evidence. The active goal is
complete TEI mapping from Aozora Bunko markup through parser-IR, using TEI P5
where faithful and ABC custom/schema preservation where TEI is not exact.
TEI-EAJ Level 2/3 labels are comparison vocabulary, not the success definition.

Current state:

- The IR publication coverage report is complete for the current measured
  scope.
- Source authority passes over 17,894 works with 0 unallowlisted unknown source
  markers and 0 unsupported source-feature occurrences.
- The source-authority report now keeps legacy counters as compatibility aliases
  and adds `source_region_coverage`:
  `source_apparatus_occurrences: 13920`,
  `front_matter_occurrences: 14627`,
  `back_matter_occurrences: 243`, and
  `malformed_source_occurrences: 16`.
- Five parser lanes are present in the generated matrix:
  `aozora2html`, `aozora-epub3`, `aozora-rs`, `aozora2`, and `aozora`.
- ABC owns the custom preservation contract and TEI profile evidence now synced
  into ab-validator.
- Plaintext remains metadata-free visible text.

ab-validator report:

- `docs/superpowers/reports/2026-07-06-ir-publication-coverage.md`
- `docs/superpowers/reports/2026-07-06-ir-publication-coverage.summary.json`

Current report verdict:

- `IR_PUBLICATION_COVERAGE_COMPLETE`
- `parser_evidence_coverage.verdict == FIVE_PARSER_EVIDENCE_COMPLETE`
- `custom_contract.verdict == CUSTOM_CONTRACT_CONFIRMED_BY_ABC_INTEGRATION`
- `tei_profile_contract.verdict == TEI_PROFILE_CONTRACT_CONFIRMED_BY_ABC_INTEGRATION`
- `closure_gaps.classified_but_not_admitted.count == 0`
- `closure_gaps.true_unsupported_gaps.count == 0`

Five parser inputs are required evidence:

- `aozora2html`
- `aozora-epub3`
- `aozora-rs`
- `aozora2`
- `aozora`

Remaining work is no longer "make TEI-EAJ Level 2/3 pass." It is:

1. Keep the source-authority scanner and source-inventory matrix current as new
   Aozora marker families or parser adapters land.
2. Coordinate ABC consumption of `source_region_coverage`, keeping legacy
   counter aliases during the downstream migration window.
3. Ensure every Aozora markup and source-apparatus family has a TEI P5,
   TEI-plus-ABC-extension, custom-sidecar, diagnostic, or explicit unsupported
   classification.
4. Improve adapter fidelity where comparison evidence shows lost or distorted
   source structure, especially paragraph/text segmentation, without treating
   TEI-EAJ editorial enrichment as parser-required markup.
5. Keep ABC profile/schema hashes, preservation records, and ab-validator
   coverage reports synchronized whenever the publication contract changes.

Design reference:

- `docs/superpowers/specs/2026-07-06-aozora-source-region-and-apparatus-contract.md`
