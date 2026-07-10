# ADR 0007: External Parser Validation Boundary

Status: Accepted
Date: 2026-04-26
Accepted: 2026-07-03
Supersedes: none
Source: `docs/high-level-architecture-note.md` v0.5, `docs/adr/0002-parser-evaluation.md`, and `../ab-validator`

## Implementation Status

Accepted after the ABC-side boundary fixture became part of the design-bundle
gate. `nix run .#validate-design-bundle` validates
`examples/ab-validator-output/` without requiring `../ab-validator`: JSON
Schema covers the imported parser IR, diagnostics, run summary, manifest
inputs, and comparison report, while `validate-ab-validator-output!` checks the
producer-declared parser-IR and diagnostic schema hashes plus run-summary and
comparison-report structure.

## Context

Parser comparison and parser-candidate execution are being developed in the
neighboring `../ab-validator` repository. ABC should not duplicate that harness.
ABC's responsibility is to define stable artifact contracts, manifests,
publication formats, and reproducible pipeline boundaries.

The two projects need a narrow handoff: `ab-validator` can compare candidate
parsers and emit files, while ABC can validate and materialize those files into
content-addressed artifacts.

## Decision

ABC treats `../ab-validator` as an external producer. ABC does not call
`ab-validator` during v0 design-bundle validation and does not depend on its
internal crate/module layout.

The stable handoff is a file bundle containing:

- parser IR JSON conforming to `schemas/parser-ir.schema.json`,
- warning/error JSON Lines where each line conforms to
  `schemas/diagnostic.schema.json`,
- run summary JSON Lines with `run-start`, `work-result`, and `run-complete`
  events conforming to `schemas/run-summary.schema.json`,
- optional comparison report JSON conforming to
  `schemas/comparison-report.schema.json`,
- manifest input hashes conforming to `schemas/manifest-inputs.schema.json`
  needed to construct ABC artifact manifests.

`ab-validator` may use any internal parser representation. Only the exported
bundle is part of the ABC boundary.

## Directory Convention

ABC keeps an imported fixture at `examples/ab-validator-output/`. The fixture is
not generated during validation; it is a small contract example checked into ABC
so changes to ABC schemas reveal boundary drift early.

Future generated imports should use the same shape:

```text
<bundle>/
├── README.md
├── manifest-inputs.json
├── parser-ir.json
├── warnings.jsonl
├── run-summary.jsonl
└── comparison-report.json
```

## Responsibilities

`ab-validator` owns:

- parser candidate execution,
- parser comparison metrics,
- corpus feature indexing,
- parser performance measurements,
- candidate-specific adapter code.

ABC owns:

- parser IR schema,
- manifest schema,
- content-addressed artifact identity,
- TEI/RDF publication artifacts,
- Nix/materialization policy,
- validation that imported outputs match ABC contracts.

## Acceptance Criteria

- `nix run .#validate-design-bundle` validates the imported
  `examples/ab-validator-output/` fixture, covered by
  `test/abc/tools/validate_design_bundle_test.clj`.
- ABC validation does not require `../ab-validator` to exist.
- Warning JSON Lines are checked against `schemas/diagnostic.schema.json`.
- Run summary JSON Lines are at least structurally checked for one start event,
  one complete event, and work-result entries.
- Parser candidate execution remains outside this repository.

## Rollback

If `ab-validator` becomes the canonical parser implementation rather than only
an external evaluator, promote that relationship through a new ADR. The boundary
should still remain file-based unless there is a measured reason to link the
projects at runtime.
