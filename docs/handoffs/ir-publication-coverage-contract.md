# Aozora Markup To TEI/Custom Publication Coverage Handoff

ab-validator now treats TEI-EAJ as calibration evidence. The active goal is
complete TEI mapping from Aozora Bunko markup through parser-IR, using TEI P5
where faithful and ABC custom/schema preservation where TEI is not exact.
TEI-EAJ Level 2/3 labels are comparison vocabulary, not the success definition.

Current state:

- The IR publication coverage report is complete for the current measured
  scope.
- Source authority passes over 17,894 works with 0 unallowlisted unknown source
  markers.
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
2. Ensure every Aozora markup family has a TEI P5, TEI-plus-ABC-extension,
   custom-sidecar, or explicit unsupported classification.
3. Improve adapter fidelity where comparison evidence shows lost or distorted
   source structure, especially paragraph/text segmentation, without treating
   TEI-EAJ editorial enrichment as parser-required markup.
4. Keep ABC profile/schema hashes, preservation records, and ab-validator
   coverage reports synchronized whenever the publication contract changes.
