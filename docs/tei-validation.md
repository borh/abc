# TEI Validation v0

Status: Active
Date: 2026-04-28
Updated: 2026-04-29 (rule batch 2)

ABC TEI validation has two required layers:

1. Structural validation with Relax NG generated from `schemas/tei-profile.odd`.
   Jing remains the compatibility baseline.
2. Business-rule validation with ISO Schematron constraints embedded in the
   same ODD and materialized as `schemas/tei-profile.sch`.

`tei_profile_hash` identifies the canonical ODD/profile contract. Generated RNG
and Schematron files are derived validation artifacts. Generator versions and
stylesheet/toolchain hashes are recorded in validation run metadata unless a
later ADR promotes them into profile identity.

## TEI-EAJ Level Vocabulary

ABC uses TEI-EAJ materials in two different roles:

- `https://github.com/TEI-EAJ/jp_guidelines/wiki` is the authoritative
  Japanese TEI markup style reference. Markup-style decisions should cite the
  relevant guideline page when possible.
- `TEI-EAJ/aozora_tei` is the pinned comparison corpus. ABC uses its Level 2-5
  vocabulary when planning TEI depth. The pinned comparison source is recorded
  in `docs/handoffs/tei-eaj-aozora-comparison.md`.
- `docs/handoffs/tei-eaj-aozora-all-work-comparison-report.md` enumerates all
  pinned TEI-EAJ XML files and compares every file with a discovered ABC TEI
  counterpart. `docs/handoffs/tei-eaj-aozora-melos-comparison-report.md`
  remains the focused Melos view.

| Level | Planning meaning for ABC |
| --- | --- |
| Level 2 | Source-preserving Aozora transcription with core source markup retained. |
| Level 3 | Basic structural units are parser-backed values. For ABC this includes paragraph boundaries and source-note/source-attribution blocks. |
| Level 4 | Curated semantic enrichment such as people, places, roles, references, and speech attribution. |
| Level 5 | Scholarly edition or specialized corpus markup, outside generated publication TEI unless separately declared. |

The current parser-IR-derived TEI is not yet a Level 3 claim for prose works
like `走れメロス`: body text and ruby are preserved, but paragraph structure
and the final source attribution note must be preserved by parser-IR before the
TEI renderer can publish them without guessing.

## Rule Inventory

| Rule ID | Constraint | Severity | Why it matters |
| --- | --- | ---: | --- |
| `abc-tei-header-title` | Every generated TEI file must have a main title in `teiHeader` | error | Prevents metadata join failures from becoming valid-looking TEI |
| `abc-tei-header-source-work-id` | TEI header must preserve the Aozora work ID or source identifier | error | Keeps TEI traceable back to source metadata |
| `abc-header-language-declared` | `teiHeader//profileDesc/langUsage/language[@ident]` must be present and non-empty | error | Lets downstream tooling route on document language |
| `abc-ruby-complete` | Ruby structures must contain base and reading components | error | Prevents malformed ruby rendering |
| `abc-ruby-base-non-empty` | `tei:ruby/tei:rb` must have non-empty content | error | Stops `<rb></rb><rt>x</rt>` from passing the structural ruby check |
| `abc-ruby-reading-non-empty` | `tei:ruby/tei:rt` must have non-empty content | error | Stops `<rb>猫</rb><rt></rt>` from passing the structural ruby check |
| `abc-gaiji-reference` | A gaiji representation must retain original marker and either a resolved replacement or unresolved status | error | Prevents silent character loss |
| `abc-gaiji-chardecl-resolution` | Gaiji `tei:g[starts-with(@ref,'#')]` must point to a `tei:charDecl/tei:char` declaration in this document | error | Closes the dangling-gaiji-ref loophole left open by `abc-gaiji-reference`'s presence-only check |
| `abc-char-resolution-form` | Each `tei:charDecl/tei:char` must declare at least one of `mapping`/`unicodeProp`/`localProp`/`desc` | error | Closes the empty-`<char xml:id="x"/>` loophole |
| `abc-figure-accessibility` | Figures/images should have `figDesc`, `head`, or caption mapping where available | warning initially | Preserves image/caption semantics |
| `abc-source-span-reference` | Elements claiming source spans must point to valid span identifiers | error | Keeps IR to TEI traceability testable |
| `abc-source-span-target-exists` | Each fragment id in `@source` must resolve to an `@xml:id` in this document | error | Closes the dangling-source-fragment loophole left open by the `#`-prefix-only check |
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
| `fixtures/tei/valid/source-span-local-ref.xml` | Passes Relax NG and Schematron |
| `fixtures/tei/valid/transcription-enrichment-declared.xml` | Passes Relax NG and Schematron |
| `fixtures/tei/invalid/missing-title.xml` | Fails `abc-tei-header-title` |
| `fixtures/tei/invalid/missing-source-work-id.xml` | Fails `abc-tei-header-source-work-id` |
| `fixtures/tei/invalid/header-no-language.xml` | Fails `abc-header-language-declared` |
| `fixtures/tei/invalid/ruby-missing-reading.xml` | Fails `abc-ruby-complete` |
| `fixtures/tei/invalid/ruby-empty-base.xml` | Fails `abc-ruby-base-non-empty` |
| `fixtures/tei/invalid/ruby-empty-reading.xml` | Fails `abc-ruby-reading-non-empty` |
| `fixtures/tei/invalid/gaiji-missing-ref.xml` | Fails `abc-gaiji-reference` |
| `fixtures/tei/invalid/gaiji-dangling-ref.xml` | Fails `abc-gaiji-chardecl-resolution` |
| `fixtures/tei/invalid/char-empty-decl.xml` | Fails `abc-char-resolution-form` |
| `fixtures/tei/invalid/source-span-external-ref.xml` | Fails `abc-source-span-reference` and `abc-source-span-target-exists` |
| `fixtures/tei/invalid/source-span-dangling-ref.xml` | Fails `abc-source-span-target-exists` |
| `fixtures/tei/warnings/figure-missing-desc.xml` | Reports `abc-figure-accessibility` |
| `fixtures/tei/warnings/transcription-enrichment-undeclared.xml` | Reports `abc-transcription-vs-annotation` |

## Generated Artifacts and Drift Gate

`schemas/tei-profile.odd` is the canonical contract. `schemas/tei-profile.rng`
and `schemas/tei-profile.sch` are reproducibly generated from the ODD by the
Nix derivation `tei-profile-artifacts` (TEI Stylesheets v7.60.0 + p5subset
4.11.0 + Saxon-HE 12.9). Build-artifact canonicalization strips generation
timestamps, rewrites the thirteen ABC `constraintSpec` pattern IDs back to their
declared idents (the `abc-[a-z0-9-]+` regex requires lowercase + digits +
hyphens — mixed-case idents fall through and get dropped by the
inherited-pattern filter), and drops inherited TEI built-in patterns the v0
ABC Schematron evaluator does not implement. To regenerate after an ODD edit:

```
nix run .#regenerate-tei-profile
```

`nix flake check` runs `tei-profile-drift`, which fails the build if the
committed artifacts diverge from what the ODD currently produces. See ADR 0012
for the toolchain pin and canonicalization rationale.

## Validation Result Artifacts

TEI validation-result JSON records both validation layers, rule IDs, severities,
toolchain metadata, and whether warnings were policy-allowed. The artifact
manifest references this sidecar with role `validation-result`.

The parser-IR publication materializer generates TEI from parser-IR body nodes
and validates it through the same project Relax NG and Schematron gates.
