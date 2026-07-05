# IR Publication Coverage Contract Handoff

ab-validator now treats TEI-EAJ as calibration evidence and full publication
coverage as the real gate.

Required ABC-side decision:

- Define the custom preservation contract for Parser-IR facts that TEI P5 does
  not carry exactly.
- Decide whether v1 is JSON sidecar, TEI namespace extension, or both.
- Give the contract a stable schema id, schema version, and manifest linkage.
- Keep plaintext metadata-free.

ab-validator report:

- `docs/superpowers/reports/2026-07-06-ir-publication-coverage.md`
- `docs/superpowers/reports/2026-07-06-ir-publication-coverage.summary.json`

The report's complete verdict is intentionally blocked until:

- unsupported gaps are empty, and
- ABC ships an owned integration that confirms the custom publication contract;
  a readable candidate path in ab-validator is recorded as evidence only and
  does not unblock admission.

Five parser inputs are required evidence:

- `aozora2html`
- `aozora-epub3`
- `aozora-rs`
- `aozora2`
- `aozora`
