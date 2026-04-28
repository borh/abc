# TEI Validation v0

Status: Draft
Date: 2026-04-28

ABC TEI validation has two required layers:

1. Structural validation with Relax NG generated from `schemas/tei-profile.odd`.
   Jing remains the compatibility baseline.
2. Business-rule validation with ISO Schematron constraints embedded in the
   same ODD and materialized as `schemas/tei-profile.sch`.

`tei_profile_hash` identifies the canonical ODD/profile contract. Generated RNG
and Schematron files are derived validation artifacts. Generator versions and
stylesheet/toolchain hashes are recorded in validation run metadata unless a
later ADR promotes them into profile identity.

## Rule Inventory

| Rule ID | Constraint | Severity | Why it matters |
| --- | --- | ---: | --- |
| `abc-tei-header-title` | Every generated TEI file must have a main title in `teiHeader` | error | Prevents metadata join failures from becoming valid-looking TEI |
| `abc-tei-header-source-work-id` | TEI header must preserve the Aozora work ID or source identifier | error | Keeps TEI traceable back to source metadata |
| `abc-ruby-complete` | Ruby structures must contain base and reading components | error | Prevents malformed ruby rendering |
| `abc-gaiji-reference` | A gaiji representation must retain original marker and either a resolved replacement or unresolved status | error | Prevents silent character loss |
| `abc-figure-accessibility` | Figures/images should have `figDesc`, `head`, or caption mapping where available | warning initially | Preserves image/caption semantics |
| `abc-source-span-reference` | Elements claiming source spans must point to valid span identifiers | error | Keeps IR to TEI traceability testable |
| `abc-transcription-vs-annotation` | Linguistic enrichment must not be mixed into the transcription layer unless explicitly declared | warning initially | Keeps TEI clean for publication |

## Acceptance Criteria

A TEI artifact is valid only when:

1. XML is well-formed.
2. Relax NG validation passes.
3. Schematron validation passes or produces only allowed warnings.
4. The validation result is materialized and referenced by the artifact
   manifest.
5. A failing TEI example produces a failure manifest, not just a console error.

At least one invalid fixture must pass Relax NG but fail Schematron. This proves
that the business-rule layer catches constraints the structural schema cannot.

## Fixture Plan

| Fixture | Expected result |
| --- | --- |
| `fixtures/tei/valid/rashomon-minimal.xml` | Passes Relax NG and Schematron |
| `fixtures/tei/invalid/missing-title.xml` | Fails `abc-tei-header-title` |
| `fixtures/tei/invalid/gaiji-missing-ref.xml` | Fails `abc-gaiji-reference` |
| `fixtures/tei/invalid/ruby-missing-reading.xml` | Fails `abc-ruby-complete` |
| `fixtures/tei/warnings/figure-missing-desc.xml` | Reports `abc-figure-accessibility` |

## Validation Result Artifacts

TEI validation-result JSON records both validation layers, rule IDs, severities,
toolchain metadata, and whether warnings were policy-allowed. The artifact
manifest references this sidecar with role `validation-result`.
