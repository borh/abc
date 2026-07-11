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
  `back_matter_occurrences: 90274`,
  `body_end_boundary_occurrences: 243`,
  `terminal_provenance_occurrences: 609`,
  `colophon_metadata_occurrences: 89416`,
  `letter_address_origin_occurrences: 6`, and
  `malformed_source_occurrences: 16`.
- Five parser lanes are present in the generated matrix:
  `aozora2html`, `aozora-epub3`, `aozora-rs`, `aozora2`, and `ab-aozora`.
  `ab-aozora` is the designated publication lane (activated 2026-07-12); the
  legacy `aozora` lane's evidence is archived at
  `reports/aat-fidelity/run-sets/2026-07-12-aozora-legacy-archive.json`
  (frozen dump `aozora-full-repin-1a4f864`), retirement pending Task 21.
- ABC owns the custom preservation contract and TEI profile evidence now synced
  into ab-validator.
- ab-validator now records full-matrix publication bundle validation evidence
  that joins parser-IR, TEI XML, plaintext, preservation sidecar,
  source-region evidence, TEI manifest, and plaintext manifest. The older
  single ABC v0 fixture remains diagnostic because it preserves a raw gaiji
  marker in plaintext while the current body-only projection uses the gaiji
  fallback string.
- Plaintext remains metadata-free visible text.

ab-validator report:

- `docs/superpowers/reports/2026-07-06-ir-publication-coverage.md`
- `docs/superpowers/reports/2026-07-06-ir-publication-coverage.summary.json`
- `docs/superpowers/reports/2026-07-06-publication-bundle-full-matrix-validation.summary.json`
- `docs/superpowers/reports/2026-07-06-publication-bundle-validation.md`
- `docs/superpowers/reports/2026-07-06-publication-bundle-validation.summary.json`

Current report verdict:

- `IR_PUBLICATION_COVERAGE_COMPLETE`
- `parser_evidence_coverage.verdict == FIVE_PARSER_EVIDENCE_COMPLETE`
- `source_region_contract.verdict == SOURCE_REGION_CONTRACT_CONFIRMED_BY_ABC_INTEGRATION`
- `custom_contract.verdict == CUSTOM_CONTRACT_CONFIRMED_BY_ABC_INTEGRATION`
- `tei_profile_contract.verdict == TEI_PROFILE_CONTRACT_CONFIRMED_BY_ABC_INTEGRATION`
- `publication_bundle_contract.verdict == PUBLICATION_BUNDLE_CONTRACT_CONFIRMED_BY_ABC_VALIDATION`
- `closure_gaps.classified_but_not_admitted.count == 0`
- `closure_gaps.true_unsupported_gaps.count == 0`

Five parser inputs are required evidence:

- `aozora2html`
- `aozora-epub3`
- `aozora-rs`
- `aozora2`
- `ab-aozora` (publication lane; legacy `aozora` evidence archived at
  `reports/aat-fidelity/run-sets/2026-07-12-aozora-legacy-archive.json`,
  frozen dump `aozora-full-repin-1a4f864`, retirement pending Task 21)

Remaining work is no longer "make TEI-EAJ Level 2/3 pass." It is:

1. Keep the source-authority scanner and source-inventory matrix current as new
   Aozora marker families or parser adapters land.
2. Keep ABC source-region contract snapshots synchronized:
   `source-region-coverage.schema.json`,
   `source-region-publication-policy-v0.json`, and
   `manifest.schema.json` with the `source-region-coverage` sidecar role.
3. Ensure every Aozora markup and source-apparatus family has a TEI P5,
   TEI-plus-ABC-extension, custom-sidecar, diagnostic, or explicit unsupported
   classification.
4. Improve adapter fidelity where comparison evidence shows lost or distorted
   source structure, especially paragraph/text segmentation, without treating
   TEI-EAJ editorial enrichment as parser-required markup.
5. Keep ABC profile/schema hashes, preservation records, and ab-validator
   coverage reports synchronized whenever the publication contract changes.

Bundle validation has been run over the representative materialized workset and
the full 285-row five-parser TEI-EAJ matrix bundle. That full-matrix evidence is
the current admission input. The single ABC v0 fixture remains useful as a
diagnostic for fixture drift, but it is not the current passing gate evidence.

Design reference:

- `docs/superpowers/specs/2026-07-06-aozora-source-region-and-apparatus-contract.md`
- `docs/handoffs/source-region-coverage-abc-integration.md`
