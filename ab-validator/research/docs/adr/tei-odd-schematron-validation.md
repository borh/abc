# TEI ODD, Relax NG, and Schematron Validation

## Implementation Status

As of 2026-07-09, `schemas/tei-profile.odd` is the live TEI policy source and
its `schemas/tei-profile.rng` and `schemas/tei-profile.sch` derived contracts
are enforced by `abc.tools.tei` and `abc.tools.schematron`. Coverage lives in
`test/abc/tools/tei_test.clj` and `test/abc/tools/schematron_test.clj`.

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

## Toolchain (pinned 2026-04-29)

- TEI Stylesheets v7.60.0
  (https://github.com/TEIC/Stylesheets/releases/tag/v7.60.0)
- p5subset.xml from TEI P5 4.11.0 vault
  (https://www.tei-c.org/Vault/P5/4.11.0/xml/tei/odd/p5subset.xml)
- Saxon-HE 12.9 (`pkgs.saxon-he`)

The Nix derivation `tei-profile-artifacts` (defined inline in `flake.nix`)
runs the chain `odd2odd.xsl → odd2relax.xsl` and `odd2odd.xsl →
extract-isosch.xsl`, then applies a narrow build-artifact canonicalization
step before emitting `tei-profile.rng` / `tei-profile.sch`:

1. Strip the non-deterministic generation timestamps that
   `odd2relax.xsl` and `extract-isosch.xsl` embed.
2. Rewrite ABC pattern IDs from
   `schematron-constraint-<ident>-<seq>` (the form
   `extract-isosch.xsl` mints) back to the bare `<ident>` declared by
   the ODD's `constraintSpec/@ident`. Inherited TEI built-in pattern
   IDs are left untouched.
3. Drop inherited TEI built-in patterns. The ODD's ABC
   `constraintSpec` rules are the committed contract surface; inherited
   TEI diagnostics are not part of the v0 ABC profile policy. The
   ph-schematron XSLT path is the runtime validator and can execute the
   inherited patterns, but the stricter ph-schematron pure model is useful as
   a schema-shape diagnostic and rejects inherited constructs such as
   `<sch:let>` and `role="nonfatal"`. The committed artifact therefore keeps
   that compatibility boundary explicit and executable in tests.

This canonicalization is purely artifact-shape: the ODD remains the
single source of truth, and a future ADR can swap the toolchain or
loosen the canonicalization without changing the ODD.

## Drift Gate

`nix flake check` builds `checks.<system>.tei-profile-drift`, which
regenerates `tei-profile.rng` and `tei-profile.sch` through the same
derivation, applies the same canonicalization, and `diff -u`s against
the committed files. Build fails on any drift. To intentionally update
the artifacts after editing `tei-profile.odd`, run
`nix run .#regenerate-tei-profile`; that copies the regenerated files
into `schemas/`, after which the drift gate is satisfied again.

## References

- TEI documentation elements: https://tei-c.org/release/doc/tei-p5-doc/en/html/TD.html
- TEI `constraintDecl`: https://tei-c.org/release/doc/tei-p5-doc/en/html/ref-constraintDecl.html
- TEI `constraintSpec`: https://tei-c.org/release/doc/tei-p5-doc/en/html/ref-constraintSpec.html
- TEI `constraintSpec` examples: https://tei-c.org/release/doc/tei-p5-doc/en/html/examples-constraintSpec.html
- TEI Stylesheets release: https://github.com/TEIC/Stylesheets/releases/tag/v7.60.0
- TEI P5 4.11.0 release: https://www.tei-c.org/release/doc/tei-p5-doc/en/html/index.html
