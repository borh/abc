# ADR 0012: TEI ODD, Relax NG, and Schematron Validation

Status: Draft
Date: 2026-04-28
Supersedes: TEI stub language in ADR 0006

## Context

ADR 0006 made TEI validation part of `validate-design-bundle`, but the current
harness still validates sample TEI against upstream `tei_all.rng`. The
architecture note already identifies `schemas/tei-profile.odd` as the next TEI
contract. That contract is incomplete if it only generates a structural Relax
NG schema.

The TEI Guidelines describe ODD as the source for generated documentation and
schema languages, including Relax NG and ISO Schematron. They also support
additional constraints through `constraintSpec`, with `constraintDecl` carrying
Schematron query-binding declarations.

## Decision

ABC TEI validation has two layers:

1. Structural validation:
   - Generated Relax NG from `schemas/tei-profile.odd`
   - Jing remains the compatibility baseline

2. Business-rule validation:
   - ISO Schematron constraints embedded in the ODD
   - Generated Schematron validation is a required v0 TEI gate
   - Schematron failures produce validation-result artifacts and failure manifests

The ABC TEI profile is not complete until the ODD produces:

1. a project-specific Relax NG schema;
2. a project-specific Schematron schema or embedded Schematron rules;
3. validation-result artifacts for both layers.

`tei_profile_hash` identifies the canonical ODD/profile contract. Generated RNG
and Schematron files are derived validation artifacts. The generator version and
stylesheet/toolchain hash are recorded in validation run metadata, not silently
folded into the profile identity unless a later ADR explicitly promotes them.

## Consequences

- `schemas/tei-profile.odd` becomes the canonical TEI contract, not a
  placeholder.
- `schemas/tei-profile.rng` and `schemas/tei-profile.sch` are reproducible
  derived artifacts or verified checked-in equivalents.
- A TEI artifact is valid only when XML is well-formed, Relax NG validation
  passes, Schematron validation passes or produces only allowed warnings, and
  the validation result is materialized and referenced by the artifact manifest.
- At least one fixture must pass Relax NG but fail Schematron, proving that the
  Schematron layer is doing work that Relax NG cannot.
- Failed TEI validation writes a failure manifest rather than only printing a
  console error.

## Initial Rule Set

The first Schematron rules are deliberately narrow:

| Rule ID | Severity | Purpose |
| --- | --- | --- |
| `abc-tei-header-title` | error | Require a main title in `teiHeader` |
| `abc-tei-header-source-work-id` | error | Preserve the Aozora work ID or source identifier |
| `abc-ruby-complete` | error | Require ruby base and reading components |
| `abc-gaiji-reference` | error | Prevent silent character loss |
| `abc-figure-accessibility` | warning initially | Preserve image/caption semantics |
| `abc-source-span-reference` | error | Keep IR to TEI traceability testable |
| `abc-transcription-vs-annotation` | warning initially | Keep enrichment out of the transcription layer unless declared |

## Acceptance Criteria

- `validate-design-bundle` verifies ODD-derived Relax NG and Schematron
  artifacts.
- Valid TEI fixture passes both layers.
- Invalid title, gaiji, and ruby fixtures fail with expected rule IDs.
- Warning fixtures materialize reports with expected rule IDs.
- Artifact manifests reference validation-result sidecars.

## References

- TEI documentation elements: https://tei-c.org/release/doc/tei-p5-doc/en/html/TD.html
- TEI `constraintDecl`: https://tei-c.org/release/doc/tei-p5-doc/en/html/ref-constraintDecl.html
- TEI `constraintSpec`: https://tei-c.org/release/doc/tei-p5-doc/en/html/ref-constraintSpec.html
- TEI `constraintSpec` examples: https://tei-c.org/release/doc/tei-p5-doc/en/html/examples-constraintSpec.html
