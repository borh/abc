# RDF Validation Options v0

Status: Draft
Date: 2026-04-28

SHACL remains the v0 RDF validation language because the manifest publication
view needs constraint validation with understandable reports. The v0 gate is
`schemas/manifest.shacl.ttl` over the derived RDF/PROV-O graph.

ShEx is recorded as an alternative for compact schema-like graph descriptions,
recursive shape patterns, or external consumers that prefer ShEx. No ShEx
implementation is required for v0.

`rudof` should be evaluated later if ABC wants a Rust-native tool that can work
across SHACL, ShEx, DCTAP, and related validation/modeling formats.

## Revisit Triggers

- Recursive graph constraints become painful in SHACL.
- External partners request ShEx.
- `rudof` materially simplifies validation tooling.
- Linked Art/IIIF publication views need additional schema-like validation.

## Watch Items

- Stable SHACL remains the baseline.
- SHACL 1.2 features are watch items until they leave the working-draft track
  and provide concrete value for ABC validation reports.

## References

- W3C SHACL Recommendation: https://www.w3.org/TR/shacl/
- W3C SHACL 1.2 Core: https://www.w3.org/TR/shacl12-core/
- Shape Expressions Language 2.1: https://shex.io/shex-semantics/
- rudof: https://rudof-project.github.io/rudof/
