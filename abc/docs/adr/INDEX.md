# Decision Records Index

GENERATED from `decisions.edn` — do not edit. Regenerate: `clojure -M:abc/adr-governance --write-index`.

## All records

| Record | Title | Status | Date | Topics | Derived links |
| --- | --- | --- | --- | --- | --- |
| [abc-tools-runtime](abc-tools-runtime.md) | ABC Tools Runtime | accepted | 2026-04-26 | runtime, tooling | depended on by [generated-fixture-policy](generated-fixture-policy.md) |
| [adr-governance-validation](adr-governance-validation.md) | Uniform ADR Governance Validation | accepted | 2026-07-10 | governance | amended by [subtractive-evidence-simplification](subtractive-evidence-simplification.md) [scope: typed evidence registry]; depended on by [data-driven-decision-records](data-driven-decision-records.md); depended on by [subtractive-evidence-simplification](subtractive-evidence-simplification.md); depended on by [typed-evidence-and-lifecycle-closure](typed-evidence-and-lifecycle-closure.md); superseded by [data-driven-decision-records](data-driven-decision-records.md) [scope: ADR Markdown header and claim grammar] |
| [analysis-artifact-identity](analysis-artifact-identity.md) | Analysis Artifact Identity | proposed | 2026-07-07 | analysis, identity | depended on by [analysis-packs-and-tokenizer-profiles](analysis-packs-and-tokenizer-profiles.md); depended on by [ruby-annotation-view](ruby-annotation-view.md) |
| [analysis-packs-and-tokenizer-profiles](analysis-packs-and-tokenizer-profiles.md) | Analysis Packs and Tokenizer Profiles | proposed | 2026-07-07 | analysis, tokenizer | depended on by [ruby-annotation-view](ruby-annotation-view.md) |
| [aozora-parser-selection](aozora-parser-selection.md) | Aozora Parser Selection and Fork Base | accepted | 2026-07-10 | parser | amended by [custom-parser-ownership-and-neutral-comparison](custom-parser-ownership-and-neutral-comparison.md); amended by [parser-fork-hard-detach](parser-fork-hard-detach.md); depended on by [custom-parser-ownership-and-neutral-comparison](custom-parser-ownership-and-neutral-comparison.md); depended on by [custom-parser-release-qualification](custom-parser-release-qualification.md); depended on by [parser-fork-hard-detach](parser-fork-hard-detach.md) |
| [cultural-heritage-lod-profile](cultural-heritage-lod-profile.md) | Cultural-Heritage LOD Publication Profile | accepted | 2026-04-28 | lod, publication |  |
| [custom-parser-ownership-and-neutral-comparison](custom-parser-ownership-and-neutral-comparison.md) | Custom Parser Ownership and Neutral Comparison | accepted | 2026-07-12 | parser, governance | amended by [subtractive-evidence-simplification](subtractive-evidence-simplification.md) [scope: ownership assessment freshness]; depended on by [custom-parser-release-qualification](custom-parser-release-qualification.md) |
| [custom-parser-release-qualification](custom-parser-release-qualification.md) | Custom Parser Release Qualification | accepted | 2026-07-20 | parser, release | depended on by [parser-rq-source-region-partition](parser-rq-source-region-partition.md) |
| [data-driven-decision-records](data-driven-decision-records.md) | Data-Driven Decision Records | accepted | 2026-07-24 | governance |  |
| [diagrams-as-gated-derived-views](diagrams-as-gated-derived-views.md) | Diagrams as Gated Derived Views | accepted | 2026-07-10 | diagrams, governance | amended by [adr-governance-validation](adr-governance-validation.md) [scope: ADR header and decision-graph source validation]; amended by [data-driven-decision-records](data-driven-decision-records.md) [scope: decision-graph source of truth]; depended on by [adr-governance-validation](adr-governance-validation.md) [scope: generated decision graph contract] |
| [edtf-level1-decade-century](edtf-level1-decade-century.md) | EDTF Level 1 — Decade Markers and BCE Century Prose | accepted | 2026-04-29 | temporal |  |
| [external-parser-validation-boundary](external-parser-validation-boundary.md) | External Parser Validation Boundary | accepted | 2026-04-26 | parser, boundary | depended on by [aozora-parser-selection](aozora-parser-selection.md); depended on by [owned-aat-parser-ir-mapping](owned-aat-parser-ir-mapping.md); depended on by [parser-ir-publication-rendering](parser-ir-publication-rendering.md) |
| [generated-fixture-policy](generated-fixture-policy.md) | Generated Output Policy | accepted | 2026-04-26 | fixtures |  |
| [governed-diagnostic-expectation-and-instrument-coordinate](governed-diagnostic-expectation-and-instrument-coordinate.md) | Bind Diagnostic Expectation to the Corpus, Instrument Identity to Its Own Coordinate | accepted | 2026-07-26 | parser, release |  |
| [governed-qualification-corpus-location](governed-qualification-corpus-location.md) | Locate the Qualification Corpus by Governance, Not Site Configuration | accepted | 2026-07-26 | parser, evidence | depended on by [parser-rq-instrument-before-threshold](parser-rq-instrument-before-threshold.md) |
| [iiif-applicability](iiif-applicability.md) | IIIF Applicability for ABC v0 | accepted | 2026-04-28 | iiif, publication |  |
| [imported-output-materialization](imported-output-materialization.md) | Imported Parser Output Materialization | accepted | 2026-04-26 | materialization | amended by [manifest-identity-hardening](manifest-identity-hardening.md); depended on by [generated-fixture-policy](generated-fixture-policy.md); depended on by [owned-aat-parser-ir-mapping](owned-aat-parser-ir-mapping.md) |
| [manifest-identity](manifest-identity.md) | Manifest Identity | accepted | 2026-04-26 | identity | amended by [analysis-packs-and-tokenizer-profiles](analysis-packs-and-tokenizer-profiles.md); amended by [manifest-identity-hardening](manifest-identity-hardening.md); amended by [owned-aat-parser-ir-mapping](owned-aat-parser-ir-mapping.md); amended by [source-bundle-identity](source-bundle-identity.md) [scope: work_content_hash equality relation]; depended on by [analysis-artifact-identity](analysis-artifact-identity.md); depended on by [analysis-packs-and-tokenizer-profiles](analysis-packs-and-tokenizer-profiles.md); depended on by [generated-fixture-policy](generated-fixture-policy.md); depended on by [owned-aat-parser-ir-mapping](owned-aat-parser-ir-mapping.md); depended on by [sole-publication-release-identity](sole-publication-release-identity.md) |
| [manifest-identity-hardening](manifest-identity-hardening.md) | Manifest Identity Hardening | accepted | 2026-04-26 | identity | depended on by [analysis-artifact-identity](analysis-artifact-identity.md); depended on by [analysis-packs-and-tokenizer-profiles](analysis-packs-and-tokenizer-profiles.md); depended on by [generated-fixture-policy](generated-fixture-policy.md); depended on by [source-bundle-identity](source-bundle-identity.md) |
| [nix-materialization](nix-materialization.md) | Nix Materialization Policy | draft | 2026-04-26 | nix, materialization | depended on by [analysis-artifact-identity](analysis-artifact-identity.md); depended on by [analysis-packs-and-tokenizer-profiles](analysis-packs-and-tokenizer-profiles.md) |
| [operational-runtime](operational-runtime.md) | Operational Runtime, API, and Retention | draft | 2026-04-26 | runtime |  |
| [owned-aat-parser-ir-mapping](owned-aat-parser-ir-mapping.md) | Owned AAT to Parser-IR Mapping and Compatibility Registry | accepted | 2026-07-03 | parser, identity | depended on by [analysis-artifact-identity](analysis-artifact-identity.md); depended on by [aozora-parser-selection](aozora-parser-selection.md); depended on by [custom-parser-ownership-and-neutral-comparison](custom-parser-ownership-and-neutral-comparison.md); depended on by [custom-parser-release-qualification](custom-parser-release-qualification.md); depended on by [parser-ir-publication-rendering](parser-ir-publication-rendering.md); depended on by [release-parser-identity-approval](release-parser-identity-approval.md); depended on by [ruby-annotation-view](ruby-annotation-view.md); depended on by [source-bundle-identity](source-bundle-identity.md) |
| [package-scoped-instrument-dependency-identity](package-scoped-instrument-dependency-identity.md) | Scope Instrument Dependency Identity to the Package, Not the Workspace | accepted | 2026-07-26 | parser, release | depended on by [governed-diagnostic-expectation-and-instrument-coordinate](governed-diagnostic-expectation-and-instrument-coordinate.md); depended on by [parser-rq-classified-source-policy-binding](parser-rq-classified-source-policy-binding.md) |
| [parser-evaluation](parser-evaluation.md) | Parser Evaluation Criteria | accepted | 2026-04-26 | parser | amended by [aozora-parser-selection](aozora-parser-selection.md); amended by [custom-parser-ownership-and-neutral-comparison](custom-parser-ownership-and-neutral-comparison.md); depended on by [aozora-parser-selection](aozora-parser-selection.md); depended on by [custom-parser-ownership-and-neutral-comparison](custom-parser-ownership-and-neutral-comparison.md); depended on by [custom-parser-release-qualification](custom-parser-release-qualification.md); depended on by [parser-ir-span-and-ruby-direction](parser-ir-span-and-ruby-direction.md) [scope: source_span_coverage gate]; depended on by [parser-release-instrument-bindings](parser-release-instrument-bindings.md); depended on by [process-tree-memory-qualification](process-tree-memory-qualification.md) |
| [parser-fork-hard-detach](parser-fork-hard-detach.md) | Parser Fork Hard Detach | accepted | 2026-07-10 | parser | amended by [custom-parser-ownership-and-neutral-comparison](custom-parser-ownership-and-neutral-comparison.md); depended on by [custom-parser-ownership-and-neutral-comparison](custom-parser-ownership-and-neutral-comparison.md) |
| [parser-ir-publication-rendering](parser-ir-publication-rendering.md) | Parser-IR Publication Rendering | accepted | 2026-07-03 | parser, publication | depended on by [analysis-artifact-identity](analysis-artifact-identity.md); depended on by [analysis-packs-and-tokenizer-profiles](analysis-packs-and-tokenizer-profiles.md); depended on by [custom-parser-ownership-and-neutral-comparison](custom-parser-ownership-and-neutral-comparison.md); depended on by [ruby-annotation-view](ruby-annotation-view.md); depended on by [source-bundle-identity](source-bundle-identity.md) |
| [parser-ir-span-and-ruby-direction](parser-ir-span-and-ruby-direction.md) | Parser-IR Additive Additions — Span Coordinate Semantics and `ruby.direction` | accepted | 2026-07-02 | parser, publication | depended on by [owned-aat-parser-ir-mapping](owned-aat-parser-ir-mapping.md); depended on by [parser-ir-publication-rendering](parser-ir-publication-rendering.md); depended on by [ruby-annotation-view](ruby-annotation-view.md) |
| [parser-release-instrument-bindings](parser-release-instrument-bindings.md) | Bind Final Parser Release Instruments | accepted | 2026-07-17 | parser, release | amended by [governed-diagnostic-expectation-and-instrument-coordinate](governed-diagnostic-expectation-and-instrument-coordinate.md) [scope: diagnostic expectation authority and the instrument identity coordinate]; amended by [package-scoped-instrument-dependency-identity](package-scoped-instrument-dependency-identity.md) [scope: instrument semantic identity and its dependency boundary]; amended by [parser-rq-classified-source-policy-binding](parser-rq-classified-source-policy-binding.md) [scope: the source_recognition instrument policy closure]; depended on by [custom-parser-release-qualification](custom-parser-release-qualification.md); depended on by [parser-rq-instrument-before-threshold](parser-rq-instrument-before-threshold.md); depended on by [parser-rq-retire-node-span-coverage](parser-rq-retire-node-span-coverage.md); depended on by [portable-parser-rq-evidence-integrity](portable-parser-rq-evidence-integrity.md) |
| [parser-rq-classified-source-policy-binding](parser-rq-classified-source-policy-binding.md) | Bind the Classified-Source Policy into the Qualification Identity | proposed | 2026-07-27 | parser, release, identity |  |
| [parser-rq-instrument-before-threshold](parser-rq-instrument-before-threshold.md) | Fix the Instrument Before Setting the Residual Threshold | proposed | 2026-07-27 | parser, release | depended on by [parser-rq-source-region-partition](parser-rq-source-region-partition.md) |
| [parser-rq-retire-node-span-coverage](parser-rq-retire-node-span-coverage.md) | Retire Parser-IR Node-Span Coverage | proposed | 2026-07-27 | parser, release |  |
| [parser-rq-source-region-partition](parser-rq-source-region-partition.md) | Partition the Source into Body and Metadata Populations | proposed | 2026-07-27 | parser, release |  |
| [person-identity-drift-data-model](person-identity-drift-data-model.md) | Person Identity Drift Data Model | accepted | 2026-04-29 | person-drift |  |
| [person-identity-drift-harness](person-identity-drift-harness.md) | Person Identity Drift Harness Contract | accepted | 2026-04-29 | person-drift |  |
| [portable-parser-rq-evidence-integrity](portable-parser-rq-evidence-integrity.md) | Keep Parser-RQ Evidence Portable and Self-Contained | accepted | 2026-07-18 | parser, evidence | amended by [governed-qualification-corpus-location](governed-qualification-corpus-location.md) [scope: runtime places supplied by site configuration] |
| [predicate-rename-batch-1](predicate-rename-batch-1.md) | Predicate Rename Batch 1 — `abc:reading` and `abc:copyrightExpired` | accepted | 2026-04-29 | lod, vocabulary | amended by [rights-assessment-and-external-statements](rights-assessment-and-external-statements.md) [scope: Boolean-to-external-rights RDF mapping] |
| [process-tree-memory-qualification](process-tree-memory-qualification.md) | Qualify Process-Tree Cgroup Memory | accepted | 2026-07-20 | parser, release | depended on by [custom-parser-release-qualification](custom-parser-release-qualification.md) |
| [publication-slug-source-injectivity](publication-slug-source-injectivity.md) | Publication Slug Source Injectivity | accepted | 2026-07-25 | publication, identity |  |
| [release-parser-identity-approval](release-parser-identity-approval.md) | Release Parser Identity Approval | accepted | 2026-07-25 | parser, release, identity | depended on by [sole-publication-release-identity](sole-publication-release-identity.md) |
| [reserved](reserved.md) | Reserved / Withdrawn | withdrawn | 2026-04-29 | governance |  |
| [rights-assessment-and-external-statements](rights-assessment-and-external-statements.md) | Rights Assessment and External Statements | proposed | 2026-07-12 | rights |  |
| [ruby-annotation-view](ruby-annotation-view.md) | Ruby Annotation View for the Token and Analysis Chain | proposed | 2026-07-09 | publication, annotation |  |
| [sole-publication-release-identity](sole-publication-release-identity.md) | Sole Publication Release Identity | accepted | 2026-07-24 | publication, release, identity | amended by [publication-slug-source-injectivity](publication-slug-source-injectivity.md) [scope: work slug derivation and source-slug injectivity] |
| [source-bundle-identity](source-bundle-identity.md) | Source Bundle Identity | accepted | 2026-07-12 | identity, source-bundle | depended on by [sole-publication-release-identity](sole-publication-release-identity.md) |
| [subtractive-evidence-simplification](subtractive-evidence-simplification.md) | Subtractive Evidence Simplification | accepted | 2026-07-23 | governance, evidence | depended on by [rights-assessment-and-external-statements](rights-assessment-and-external-statements.md) |
| [supply-chain-release-security](supply-chain-release-security.md) | Supply-Chain Release Security | draft | 2026-04-26 | security, release | depended on by [analysis-packs-and-tokenizer-profiles](analysis-packs-and-tokenizer-profiles.md) |
| [tei-odd-schematron-validation](tei-odd-schematron-validation.md) | TEI ODD, Relax NG, and Schematron Validation | accepted | 2026-04-28 | tei, validation | depended on by [parser-ir-publication-rendering](parser-ir-publication-rendering.md) |
| [temporal-modeling](temporal-modeling.md) | Temporal Modeling for Bibliographic Dates | accepted | 2026-04-29 | temporal |  |
| [terminal-provenance-colophon-split](terminal-provenance-colophon-split.md) | Terminal Provenance / Colophon Split | proposed | 2026-07-13 | publication, provenance |  |
| [typed-evidence-and-lifecycle-closure](typed-evidence-and-lifecycle-closure.md) | Typed Evidence and Lifecycle Closure | superseded | 2026-07-12 | governance, evidence | superseded by [subtractive-evidence-simplification](subtractive-evidence-simplification.md) |
| [upstream-ingest-drift-awareness](upstream-ingest-drift-awareness.md) | Upstream Ingest Drift Awareness | accepted | 2026-04-30 | person-drift, ingest |  |
| [v0-design-bundle-validation](v0-design-bundle-validation.md) | v0 Design Bundle Validation CLI | accepted | 2026-04-26 | validation | amended by [abc-tools-runtime](abc-tools-runtime.md); depended on by [diagrams-as-gated-derived-views](diagrams-as-gated-derived-views.md); depended on by [owned-aat-parser-ir-mapping](owned-aat-parser-ir-mapping.md); superseded by [tei-odd-schematron-validation](tei-odd-schematron-validation.md) [scope: TEI stub language] |
| [vocabulary-review](vocabulary-review.md) | Vocabulary Review and `abc:` Namespace Consistency | accepted | 2026-04-29 | lod, vocabulary | amended by [predicate-rename-batch-1](predicate-rename-batch-1.md) |

## Topic: analysis

| Record | Title | Status | Date | Topics | Derived links |
| --- | --- | --- | --- | --- | --- |
| [analysis-artifact-identity](analysis-artifact-identity.md) | Analysis Artifact Identity | proposed | 2026-07-07 | analysis, identity | depended on by [analysis-packs-and-tokenizer-profiles](analysis-packs-and-tokenizer-profiles.md); depended on by [ruby-annotation-view](ruby-annotation-view.md) |
| [analysis-packs-and-tokenizer-profiles](analysis-packs-and-tokenizer-profiles.md) | Analysis Packs and Tokenizer Profiles | proposed | 2026-07-07 | analysis, tokenizer | depended on by [ruby-annotation-view](ruby-annotation-view.md) |

## Topic: annotation

| Record | Title | Status | Date | Topics | Derived links |
| --- | --- | --- | --- | --- | --- |
| [ruby-annotation-view](ruby-annotation-view.md) | Ruby Annotation View for the Token and Analysis Chain | proposed | 2026-07-09 | publication, annotation |  |

## Topic: boundary

| Record | Title | Status | Date | Topics | Derived links |
| --- | --- | --- | --- | --- | --- |
| [external-parser-validation-boundary](external-parser-validation-boundary.md) | External Parser Validation Boundary | accepted | 2026-04-26 | parser, boundary | depended on by [aozora-parser-selection](aozora-parser-selection.md); depended on by [owned-aat-parser-ir-mapping](owned-aat-parser-ir-mapping.md); depended on by [parser-ir-publication-rendering](parser-ir-publication-rendering.md) |

## Topic: diagrams

| Record | Title | Status | Date | Topics | Derived links |
| --- | --- | --- | --- | --- | --- |
| [diagrams-as-gated-derived-views](diagrams-as-gated-derived-views.md) | Diagrams as Gated Derived Views | accepted | 2026-07-10 | diagrams, governance | amended by [adr-governance-validation](adr-governance-validation.md) [scope: ADR header and decision-graph source validation]; amended by [data-driven-decision-records](data-driven-decision-records.md) [scope: decision-graph source of truth]; depended on by [adr-governance-validation](adr-governance-validation.md) [scope: generated decision graph contract] |

## Topic: evidence

| Record | Title | Status | Date | Topics | Derived links |
| --- | --- | --- | --- | --- | --- |
| [governed-qualification-corpus-location](governed-qualification-corpus-location.md) | Locate the Qualification Corpus by Governance, Not Site Configuration | accepted | 2026-07-26 | parser, evidence | depended on by [parser-rq-instrument-before-threshold](parser-rq-instrument-before-threshold.md) |
| [portable-parser-rq-evidence-integrity](portable-parser-rq-evidence-integrity.md) | Keep Parser-RQ Evidence Portable and Self-Contained | accepted | 2026-07-18 | parser, evidence | amended by [governed-qualification-corpus-location](governed-qualification-corpus-location.md) [scope: runtime places supplied by site configuration] |
| [subtractive-evidence-simplification](subtractive-evidence-simplification.md) | Subtractive Evidence Simplification | accepted | 2026-07-23 | governance, evidence | depended on by [rights-assessment-and-external-statements](rights-assessment-and-external-statements.md) |
| [typed-evidence-and-lifecycle-closure](typed-evidence-and-lifecycle-closure.md) | Typed Evidence and Lifecycle Closure | superseded | 2026-07-12 | governance, evidence | superseded by [subtractive-evidence-simplification](subtractive-evidence-simplification.md) |

## Topic: fixtures

| Record | Title | Status | Date | Topics | Derived links |
| --- | --- | --- | --- | --- | --- |
| [generated-fixture-policy](generated-fixture-policy.md) | Generated Output Policy | accepted | 2026-04-26 | fixtures |  |

## Topic: governance

| Record | Title | Status | Date | Topics | Derived links |
| --- | --- | --- | --- | --- | --- |
| [adr-governance-validation](adr-governance-validation.md) | Uniform ADR Governance Validation | accepted | 2026-07-10 | governance | amended by [subtractive-evidence-simplification](subtractive-evidence-simplification.md) [scope: typed evidence registry]; depended on by [data-driven-decision-records](data-driven-decision-records.md); depended on by [subtractive-evidence-simplification](subtractive-evidence-simplification.md); depended on by [typed-evidence-and-lifecycle-closure](typed-evidence-and-lifecycle-closure.md); superseded by [data-driven-decision-records](data-driven-decision-records.md) [scope: ADR Markdown header and claim grammar] |
| [custom-parser-ownership-and-neutral-comparison](custom-parser-ownership-and-neutral-comparison.md) | Custom Parser Ownership and Neutral Comparison | accepted | 2026-07-12 | parser, governance | amended by [subtractive-evidence-simplification](subtractive-evidence-simplification.md) [scope: ownership assessment freshness]; depended on by [custom-parser-release-qualification](custom-parser-release-qualification.md) |
| [data-driven-decision-records](data-driven-decision-records.md) | Data-Driven Decision Records | accepted | 2026-07-24 | governance |  |
| [diagrams-as-gated-derived-views](diagrams-as-gated-derived-views.md) | Diagrams as Gated Derived Views | accepted | 2026-07-10 | diagrams, governance | amended by [adr-governance-validation](adr-governance-validation.md) [scope: ADR header and decision-graph source validation]; amended by [data-driven-decision-records](data-driven-decision-records.md) [scope: decision-graph source of truth]; depended on by [adr-governance-validation](adr-governance-validation.md) [scope: generated decision graph contract] |
| [reserved](reserved.md) | Reserved / Withdrawn | withdrawn | 2026-04-29 | governance |  |
| [subtractive-evidence-simplification](subtractive-evidence-simplification.md) | Subtractive Evidence Simplification | accepted | 2026-07-23 | governance, evidence | depended on by [rights-assessment-and-external-statements](rights-assessment-and-external-statements.md) |
| [typed-evidence-and-lifecycle-closure](typed-evidence-and-lifecycle-closure.md) | Typed Evidence and Lifecycle Closure | superseded | 2026-07-12 | governance, evidence | superseded by [subtractive-evidence-simplification](subtractive-evidence-simplification.md) |

## Topic: identity

| Record | Title | Status | Date | Topics | Derived links |
| --- | --- | --- | --- | --- | --- |
| [analysis-artifact-identity](analysis-artifact-identity.md) | Analysis Artifact Identity | proposed | 2026-07-07 | analysis, identity | depended on by [analysis-packs-and-tokenizer-profiles](analysis-packs-and-tokenizer-profiles.md); depended on by [ruby-annotation-view](ruby-annotation-view.md) |
| [manifest-identity](manifest-identity.md) | Manifest Identity | accepted | 2026-04-26 | identity | amended by [analysis-packs-and-tokenizer-profiles](analysis-packs-and-tokenizer-profiles.md); amended by [manifest-identity-hardening](manifest-identity-hardening.md); amended by [owned-aat-parser-ir-mapping](owned-aat-parser-ir-mapping.md); amended by [source-bundle-identity](source-bundle-identity.md) [scope: work_content_hash equality relation]; depended on by [analysis-artifact-identity](analysis-artifact-identity.md); depended on by [analysis-packs-and-tokenizer-profiles](analysis-packs-and-tokenizer-profiles.md); depended on by [generated-fixture-policy](generated-fixture-policy.md); depended on by [owned-aat-parser-ir-mapping](owned-aat-parser-ir-mapping.md); depended on by [sole-publication-release-identity](sole-publication-release-identity.md) |
| [manifest-identity-hardening](manifest-identity-hardening.md) | Manifest Identity Hardening | accepted | 2026-04-26 | identity | depended on by [analysis-artifact-identity](analysis-artifact-identity.md); depended on by [analysis-packs-and-tokenizer-profiles](analysis-packs-and-tokenizer-profiles.md); depended on by [generated-fixture-policy](generated-fixture-policy.md); depended on by [source-bundle-identity](source-bundle-identity.md) |
| [owned-aat-parser-ir-mapping](owned-aat-parser-ir-mapping.md) | Owned AAT to Parser-IR Mapping and Compatibility Registry | accepted | 2026-07-03 | parser, identity | depended on by [analysis-artifact-identity](analysis-artifact-identity.md); depended on by [aozora-parser-selection](aozora-parser-selection.md); depended on by [custom-parser-ownership-and-neutral-comparison](custom-parser-ownership-and-neutral-comparison.md); depended on by [custom-parser-release-qualification](custom-parser-release-qualification.md); depended on by [parser-ir-publication-rendering](parser-ir-publication-rendering.md); depended on by [release-parser-identity-approval](release-parser-identity-approval.md); depended on by [ruby-annotation-view](ruby-annotation-view.md); depended on by [source-bundle-identity](source-bundle-identity.md) |
| [parser-rq-classified-source-policy-binding](parser-rq-classified-source-policy-binding.md) | Bind the Classified-Source Policy into the Qualification Identity | proposed | 2026-07-27 | parser, release, identity |  |
| [publication-slug-source-injectivity](publication-slug-source-injectivity.md) | Publication Slug Source Injectivity | accepted | 2026-07-25 | publication, identity |  |
| [release-parser-identity-approval](release-parser-identity-approval.md) | Release Parser Identity Approval | accepted | 2026-07-25 | parser, release, identity | depended on by [sole-publication-release-identity](sole-publication-release-identity.md) |
| [sole-publication-release-identity](sole-publication-release-identity.md) | Sole Publication Release Identity | accepted | 2026-07-24 | publication, release, identity | amended by [publication-slug-source-injectivity](publication-slug-source-injectivity.md) [scope: work slug derivation and source-slug injectivity] |
| [source-bundle-identity](source-bundle-identity.md) | Source Bundle Identity | accepted | 2026-07-12 | identity, source-bundle | depended on by [sole-publication-release-identity](sole-publication-release-identity.md) |

## Topic: iiif

| Record | Title | Status | Date | Topics | Derived links |
| --- | --- | --- | --- | --- | --- |
| [iiif-applicability](iiif-applicability.md) | IIIF Applicability for ABC v0 | accepted | 2026-04-28 | iiif, publication |  |

## Topic: ingest

| Record | Title | Status | Date | Topics | Derived links |
| --- | --- | --- | --- | --- | --- |
| [upstream-ingest-drift-awareness](upstream-ingest-drift-awareness.md) | Upstream Ingest Drift Awareness | accepted | 2026-04-30 | person-drift, ingest |  |

## Topic: lod

| Record | Title | Status | Date | Topics | Derived links |
| --- | --- | --- | --- | --- | --- |
| [cultural-heritage-lod-profile](cultural-heritage-lod-profile.md) | Cultural-Heritage LOD Publication Profile | accepted | 2026-04-28 | lod, publication |  |
| [predicate-rename-batch-1](predicate-rename-batch-1.md) | Predicate Rename Batch 1 — `abc:reading` and `abc:copyrightExpired` | accepted | 2026-04-29 | lod, vocabulary | amended by [rights-assessment-and-external-statements](rights-assessment-and-external-statements.md) [scope: Boolean-to-external-rights RDF mapping] |
| [vocabulary-review](vocabulary-review.md) | Vocabulary Review and `abc:` Namespace Consistency | accepted | 2026-04-29 | lod, vocabulary | amended by [predicate-rename-batch-1](predicate-rename-batch-1.md) |

## Topic: materialization

| Record | Title | Status | Date | Topics | Derived links |
| --- | --- | --- | --- | --- | --- |
| [imported-output-materialization](imported-output-materialization.md) | Imported Parser Output Materialization | accepted | 2026-04-26 | materialization | amended by [manifest-identity-hardening](manifest-identity-hardening.md); depended on by [generated-fixture-policy](generated-fixture-policy.md); depended on by [owned-aat-parser-ir-mapping](owned-aat-parser-ir-mapping.md) |
| [nix-materialization](nix-materialization.md) | Nix Materialization Policy | draft | 2026-04-26 | nix, materialization | depended on by [analysis-artifact-identity](analysis-artifact-identity.md); depended on by [analysis-packs-and-tokenizer-profiles](analysis-packs-and-tokenizer-profiles.md) |

## Topic: nix

| Record | Title | Status | Date | Topics | Derived links |
| --- | --- | --- | --- | --- | --- |
| [nix-materialization](nix-materialization.md) | Nix Materialization Policy | draft | 2026-04-26 | nix, materialization | depended on by [analysis-artifact-identity](analysis-artifact-identity.md); depended on by [analysis-packs-and-tokenizer-profiles](analysis-packs-and-tokenizer-profiles.md) |

## Topic: parser

| Record | Title | Status | Date | Topics | Derived links |
| --- | --- | --- | --- | --- | --- |
| [aozora-parser-selection](aozora-parser-selection.md) | Aozora Parser Selection and Fork Base | accepted | 2026-07-10 | parser | amended by [custom-parser-ownership-and-neutral-comparison](custom-parser-ownership-and-neutral-comparison.md); amended by [parser-fork-hard-detach](parser-fork-hard-detach.md); depended on by [custom-parser-ownership-and-neutral-comparison](custom-parser-ownership-and-neutral-comparison.md); depended on by [custom-parser-release-qualification](custom-parser-release-qualification.md); depended on by [parser-fork-hard-detach](parser-fork-hard-detach.md) |
| [custom-parser-ownership-and-neutral-comparison](custom-parser-ownership-and-neutral-comparison.md) | Custom Parser Ownership and Neutral Comparison | accepted | 2026-07-12 | parser, governance | amended by [subtractive-evidence-simplification](subtractive-evidence-simplification.md) [scope: ownership assessment freshness]; depended on by [custom-parser-release-qualification](custom-parser-release-qualification.md) |
| [custom-parser-release-qualification](custom-parser-release-qualification.md) | Custom Parser Release Qualification | accepted | 2026-07-20 | parser, release | depended on by [parser-rq-source-region-partition](parser-rq-source-region-partition.md) |
| [external-parser-validation-boundary](external-parser-validation-boundary.md) | External Parser Validation Boundary | accepted | 2026-04-26 | parser, boundary | depended on by [aozora-parser-selection](aozora-parser-selection.md); depended on by [owned-aat-parser-ir-mapping](owned-aat-parser-ir-mapping.md); depended on by [parser-ir-publication-rendering](parser-ir-publication-rendering.md) |
| [governed-diagnostic-expectation-and-instrument-coordinate](governed-diagnostic-expectation-and-instrument-coordinate.md) | Bind Diagnostic Expectation to the Corpus, Instrument Identity to Its Own Coordinate | accepted | 2026-07-26 | parser, release |  |
| [governed-qualification-corpus-location](governed-qualification-corpus-location.md) | Locate the Qualification Corpus by Governance, Not Site Configuration | accepted | 2026-07-26 | parser, evidence | depended on by [parser-rq-instrument-before-threshold](parser-rq-instrument-before-threshold.md) |
| [owned-aat-parser-ir-mapping](owned-aat-parser-ir-mapping.md) | Owned AAT to Parser-IR Mapping and Compatibility Registry | accepted | 2026-07-03 | parser, identity | depended on by [analysis-artifact-identity](analysis-artifact-identity.md); depended on by [aozora-parser-selection](aozora-parser-selection.md); depended on by [custom-parser-ownership-and-neutral-comparison](custom-parser-ownership-and-neutral-comparison.md); depended on by [custom-parser-release-qualification](custom-parser-release-qualification.md); depended on by [parser-ir-publication-rendering](parser-ir-publication-rendering.md); depended on by [release-parser-identity-approval](release-parser-identity-approval.md); depended on by [ruby-annotation-view](ruby-annotation-view.md); depended on by [source-bundle-identity](source-bundle-identity.md) |
| [package-scoped-instrument-dependency-identity](package-scoped-instrument-dependency-identity.md) | Scope Instrument Dependency Identity to the Package, Not the Workspace | accepted | 2026-07-26 | parser, release | depended on by [governed-diagnostic-expectation-and-instrument-coordinate](governed-diagnostic-expectation-and-instrument-coordinate.md); depended on by [parser-rq-classified-source-policy-binding](parser-rq-classified-source-policy-binding.md) |
| [parser-evaluation](parser-evaluation.md) | Parser Evaluation Criteria | accepted | 2026-04-26 | parser | amended by [aozora-parser-selection](aozora-parser-selection.md); amended by [custom-parser-ownership-and-neutral-comparison](custom-parser-ownership-and-neutral-comparison.md); depended on by [aozora-parser-selection](aozora-parser-selection.md); depended on by [custom-parser-ownership-and-neutral-comparison](custom-parser-ownership-and-neutral-comparison.md); depended on by [custom-parser-release-qualification](custom-parser-release-qualification.md); depended on by [parser-ir-span-and-ruby-direction](parser-ir-span-and-ruby-direction.md) [scope: source_span_coverage gate]; depended on by [parser-release-instrument-bindings](parser-release-instrument-bindings.md); depended on by [process-tree-memory-qualification](process-tree-memory-qualification.md) |
| [parser-fork-hard-detach](parser-fork-hard-detach.md) | Parser Fork Hard Detach | accepted | 2026-07-10 | parser | amended by [custom-parser-ownership-and-neutral-comparison](custom-parser-ownership-and-neutral-comparison.md); depended on by [custom-parser-ownership-and-neutral-comparison](custom-parser-ownership-and-neutral-comparison.md) |
| [parser-ir-publication-rendering](parser-ir-publication-rendering.md) | Parser-IR Publication Rendering | accepted | 2026-07-03 | parser, publication | depended on by [analysis-artifact-identity](analysis-artifact-identity.md); depended on by [analysis-packs-and-tokenizer-profiles](analysis-packs-and-tokenizer-profiles.md); depended on by [custom-parser-ownership-and-neutral-comparison](custom-parser-ownership-and-neutral-comparison.md); depended on by [ruby-annotation-view](ruby-annotation-view.md); depended on by [source-bundle-identity](source-bundle-identity.md) |
| [parser-ir-span-and-ruby-direction](parser-ir-span-and-ruby-direction.md) | Parser-IR Additive Additions — Span Coordinate Semantics and `ruby.direction` | accepted | 2026-07-02 | parser, publication | depended on by [owned-aat-parser-ir-mapping](owned-aat-parser-ir-mapping.md); depended on by [parser-ir-publication-rendering](parser-ir-publication-rendering.md); depended on by [ruby-annotation-view](ruby-annotation-view.md) |
| [parser-release-instrument-bindings](parser-release-instrument-bindings.md) | Bind Final Parser Release Instruments | accepted | 2026-07-17 | parser, release | amended by [governed-diagnostic-expectation-and-instrument-coordinate](governed-diagnostic-expectation-and-instrument-coordinate.md) [scope: diagnostic expectation authority and the instrument identity coordinate]; amended by [package-scoped-instrument-dependency-identity](package-scoped-instrument-dependency-identity.md) [scope: instrument semantic identity and its dependency boundary]; amended by [parser-rq-classified-source-policy-binding](parser-rq-classified-source-policy-binding.md) [scope: the source_recognition instrument policy closure]; depended on by [custom-parser-release-qualification](custom-parser-release-qualification.md); depended on by [parser-rq-instrument-before-threshold](parser-rq-instrument-before-threshold.md); depended on by [parser-rq-retire-node-span-coverage](parser-rq-retire-node-span-coverage.md); depended on by [portable-parser-rq-evidence-integrity](portable-parser-rq-evidence-integrity.md) |
| [parser-rq-classified-source-policy-binding](parser-rq-classified-source-policy-binding.md) | Bind the Classified-Source Policy into the Qualification Identity | proposed | 2026-07-27 | parser, release, identity |  |
| [parser-rq-instrument-before-threshold](parser-rq-instrument-before-threshold.md) | Fix the Instrument Before Setting the Residual Threshold | proposed | 2026-07-27 | parser, release | depended on by [parser-rq-source-region-partition](parser-rq-source-region-partition.md) |
| [parser-rq-retire-node-span-coverage](parser-rq-retire-node-span-coverage.md) | Retire Parser-IR Node-Span Coverage | proposed | 2026-07-27 | parser, release |  |
| [parser-rq-source-region-partition](parser-rq-source-region-partition.md) | Partition the Source into Body and Metadata Populations | proposed | 2026-07-27 | parser, release |  |
| [portable-parser-rq-evidence-integrity](portable-parser-rq-evidence-integrity.md) | Keep Parser-RQ Evidence Portable and Self-Contained | accepted | 2026-07-18 | parser, evidence | amended by [governed-qualification-corpus-location](governed-qualification-corpus-location.md) [scope: runtime places supplied by site configuration] |
| [process-tree-memory-qualification](process-tree-memory-qualification.md) | Qualify Process-Tree Cgroup Memory | accepted | 2026-07-20 | parser, release | depended on by [custom-parser-release-qualification](custom-parser-release-qualification.md) |
| [release-parser-identity-approval](release-parser-identity-approval.md) | Release Parser Identity Approval | accepted | 2026-07-25 | parser, release, identity | depended on by [sole-publication-release-identity](sole-publication-release-identity.md) |

## Topic: person-drift

| Record | Title | Status | Date | Topics | Derived links |
| --- | --- | --- | --- | --- | --- |
| [person-identity-drift-data-model](person-identity-drift-data-model.md) | Person Identity Drift Data Model | accepted | 2026-04-29 | person-drift |  |
| [person-identity-drift-harness](person-identity-drift-harness.md) | Person Identity Drift Harness Contract | accepted | 2026-04-29 | person-drift |  |
| [upstream-ingest-drift-awareness](upstream-ingest-drift-awareness.md) | Upstream Ingest Drift Awareness | accepted | 2026-04-30 | person-drift, ingest |  |

## Topic: provenance

| Record | Title | Status | Date | Topics | Derived links |
| --- | --- | --- | --- | --- | --- |
| [terminal-provenance-colophon-split](terminal-provenance-colophon-split.md) | Terminal Provenance / Colophon Split | proposed | 2026-07-13 | publication, provenance |  |

## Topic: publication

| Record | Title | Status | Date | Topics | Derived links |
| --- | --- | --- | --- | --- | --- |
| [cultural-heritage-lod-profile](cultural-heritage-lod-profile.md) | Cultural-Heritage LOD Publication Profile | accepted | 2026-04-28 | lod, publication |  |
| [iiif-applicability](iiif-applicability.md) | IIIF Applicability for ABC v0 | accepted | 2026-04-28 | iiif, publication |  |
| [parser-ir-publication-rendering](parser-ir-publication-rendering.md) | Parser-IR Publication Rendering | accepted | 2026-07-03 | parser, publication | depended on by [analysis-artifact-identity](analysis-artifact-identity.md); depended on by [analysis-packs-and-tokenizer-profiles](analysis-packs-and-tokenizer-profiles.md); depended on by [custom-parser-ownership-and-neutral-comparison](custom-parser-ownership-and-neutral-comparison.md); depended on by [ruby-annotation-view](ruby-annotation-view.md); depended on by [source-bundle-identity](source-bundle-identity.md) |
| [parser-ir-span-and-ruby-direction](parser-ir-span-and-ruby-direction.md) | Parser-IR Additive Additions — Span Coordinate Semantics and `ruby.direction` | accepted | 2026-07-02 | parser, publication | depended on by [owned-aat-parser-ir-mapping](owned-aat-parser-ir-mapping.md); depended on by [parser-ir-publication-rendering](parser-ir-publication-rendering.md); depended on by [ruby-annotation-view](ruby-annotation-view.md) |
| [publication-slug-source-injectivity](publication-slug-source-injectivity.md) | Publication Slug Source Injectivity | accepted | 2026-07-25 | publication, identity |  |
| [ruby-annotation-view](ruby-annotation-view.md) | Ruby Annotation View for the Token and Analysis Chain | proposed | 2026-07-09 | publication, annotation |  |
| [sole-publication-release-identity](sole-publication-release-identity.md) | Sole Publication Release Identity | accepted | 2026-07-24 | publication, release, identity | amended by [publication-slug-source-injectivity](publication-slug-source-injectivity.md) [scope: work slug derivation and source-slug injectivity] |
| [terminal-provenance-colophon-split](terminal-provenance-colophon-split.md) | Terminal Provenance / Colophon Split | proposed | 2026-07-13 | publication, provenance |  |

## Topic: release

| Record | Title | Status | Date | Topics | Derived links |
| --- | --- | --- | --- | --- | --- |
| [custom-parser-release-qualification](custom-parser-release-qualification.md) | Custom Parser Release Qualification | accepted | 2026-07-20 | parser, release | depended on by [parser-rq-source-region-partition](parser-rq-source-region-partition.md) |
| [governed-diagnostic-expectation-and-instrument-coordinate](governed-diagnostic-expectation-and-instrument-coordinate.md) | Bind Diagnostic Expectation to the Corpus, Instrument Identity to Its Own Coordinate | accepted | 2026-07-26 | parser, release |  |
| [package-scoped-instrument-dependency-identity](package-scoped-instrument-dependency-identity.md) | Scope Instrument Dependency Identity to the Package, Not the Workspace | accepted | 2026-07-26 | parser, release | depended on by [governed-diagnostic-expectation-and-instrument-coordinate](governed-diagnostic-expectation-and-instrument-coordinate.md); depended on by [parser-rq-classified-source-policy-binding](parser-rq-classified-source-policy-binding.md) |
| [parser-release-instrument-bindings](parser-release-instrument-bindings.md) | Bind Final Parser Release Instruments | accepted | 2026-07-17 | parser, release | amended by [governed-diagnostic-expectation-and-instrument-coordinate](governed-diagnostic-expectation-and-instrument-coordinate.md) [scope: diagnostic expectation authority and the instrument identity coordinate]; amended by [package-scoped-instrument-dependency-identity](package-scoped-instrument-dependency-identity.md) [scope: instrument semantic identity and its dependency boundary]; amended by [parser-rq-classified-source-policy-binding](parser-rq-classified-source-policy-binding.md) [scope: the source_recognition instrument policy closure]; depended on by [custom-parser-release-qualification](custom-parser-release-qualification.md); depended on by [parser-rq-instrument-before-threshold](parser-rq-instrument-before-threshold.md); depended on by [parser-rq-retire-node-span-coverage](parser-rq-retire-node-span-coverage.md); depended on by [portable-parser-rq-evidence-integrity](portable-parser-rq-evidence-integrity.md) |
| [parser-rq-classified-source-policy-binding](parser-rq-classified-source-policy-binding.md) | Bind the Classified-Source Policy into the Qualification Identity | proposed | 2026-07-27 | parser, release, identity |  |
| [parser-rq-instrument-before-threshold](parser-rq-instrument-before-threshold.md) | Fix the Instrument Before Setting the Residual Threshold | proposed | 2026-07-27 | parser, release | depended on by [parser-rq-source-region-partition](parser-rq-source-region-partition.md) |
| [parser-rq-retire-node-span-coverage](parser-rq-retire-node-span-coverage.md) | Retire Parser-IR Node-Span Coverage | proposed | 2026-07-27 | parser, release |  |
| [parser-rq-source-region-partition](parser-rq-source-region-partition.md) | Partition the Source into Body and Metadata Populations | proposed | 2026-07-27 | parser, release |  |
| [process-tree-memory-qualification](process-tree-memory-qualification.md) | Qualify Process-Tree Cgroup Memory | accepted | 2026-07-20 | parser, release | depended on by [custom-parser-release-qualification](custom-parser-release-qualification.md) |
| [release-parser-identity-approval](release-parser-identity-approval.md) | Release Parser Identity Approval | accepted | 2026-07-25 | parser, release, identity | depended on by [sole-publication-release-identity](sole-publication-release-identity.md) |
| [sole-publication-release-identity](sole-publication-release-identity.md) | Sole Publication Release Identity | accepted | 2026-07-24 | publication, release, identity | amended by [publication-slug-source-injectivity](publication-slug-source-injectivity.md) [scope: work slug derivation and source-slug injectivity] |
| [supply-chain-release-security](supply-chain-release-security.md) | Supply-Chain Release Security | draft | 2026-04-26 | security, release | depended on by [analysis-packs-and-tokenizer-profiles](analysis-packs-and-tokenizer-profiles.md) |

## Topic: rights

| Record | Title | Status | Date | Topics | Derived links |
| --- | --- | --- | --- | --- | --- |
| [rights-assessment-and-external-statements](rights-assessment-and-external-statements.md) | Rights Assessment and External Statements | proposed | 2026-07-12 | rights |  |

## Topic: runtime

| Record | Title | Status | Date | Topics | Derived links |
| --- | --- | --- | --- | --- | --- |
| [abc-tools-runtime](abc-tools-runtime.md) | ABC Tools Runtime | accepted | 2026-04-26 | runtime, tooling | depended on by [generated-fixture-policy](generated-fixture-policy.md) |
| [operational-runtime](operational-runtime.md) | Operational Runtime, API, and Retention | draft | 2026-04-26 | runtime |  |

## Topic: security

| Record | Title | Status | Date | Topics | Derived links |
| --- | --- | --- | --- | --- | --- |
| [supply-chain-release-security](supply-chain-release-security.md) | Supply-Chain Release Security | draft | 2026-04-26 | security, release | depended on by [analysis-packs-and-tokenizer-profiles](analysis-packs-and-tokenizer-profiles.md) |

## Topic: source-bundle

| Record | Title | Status | Date | Topics | Derived links |
| --- | --- | --- | --- | --- | --- |
| [source-bundle-identity](source-bundle-identity.md) | Source Bundle Identity | accepted | 2026-07-12 | identity, source-bundle | depended on by [sole-publication-release-identity](sole-publication-release-identity.md) |

## Topic: tei

| Record | Title | Status | Date | Topics | Derived links |
| --- | --- | --- | --- | --- | --- |
| [tei-odd-schematron-validation](tei-odd-schematron-validation.md) | TEI ODD, Relax NG, and Schematron Validation | accepted | 2026-04-28 | tei, validation | depended on by [parser-ir-publication-rendering](parser-ir-publication-rendering.md) |

## Topic: temporal

| Record | Title | Status | Date | Topics | Derived links |
| --- | --- | --- | --- | --- | --- |
| [edtf-level1-decade-century](edtf-level1-decade-century.md) | EDTF Level 1 — Decade Markers and BCE Century Prose | accepted | 2026-04-29 | temporal |  |
| [temporal-modeling](temporal-modeling.md) | Temporal Modeling for Bibliographic Dates | accepted | 2026-04-29 | temporal |  |

## Topic: tokenizer

| Record | Title | Status | Date | Topics | Derived links |
| --- | --- | --- | --- | --- | --- |
| [analysis-packs-and-tokenizer-profiles](analysis-packs-and-tokenizer-profiles.md) | Analysis Packs and Tokenizer Profiles | proposed | 2026-07-07 | analysis, tokenizer | depended on by [ruby-annotation-view](ruby-annotation-view.md) |

## Topic: tooling

| Record | Title | Status | Date | Topics | Derived links |
| --- | --- | --- | --- | --- | --- |
| [abc-tools-runtime](abc-tools-runtime.md) | ABC Tools Runtime | accepted | 2026-04-26 | runtime, tooling | depended on by [generated-fixture-policy](generated-fixture-policy.md) |

## Topic: validation

| Record | Title | Status | Date | Topics | Derived links |
| --- | --- | --- | --- | --- | --- |
| [tei-odd-schematron-validation](tei-odd-schematron-validation.md) | TEI ODD, Relax NG, and Schematron Validation | accepted | 2026-04-28 | tei, validation | depended on by [parser-ir-publication-rendering](parser-ir-publication-rendering.md) |
| [v0-design-bundle-validation](v0-design-bundle-validation.md) | v0 Design Bundle Validation CLI | accepted | 2026-04-26 | validation | amended by [abc-tools-runtime](abc-tools-runtime.md); depended on by [diagrams-as-gated-derived-views](diagrams-as-gated-derived-views.md); depended on by [owned-aat-parser-ir-mapping](owned-aat-parser-ir-mapping.md); superseded by [tei-odd-schematron-validation](tei-odd-schematron-validation.md) [scope: TEI stub language] |

## Topic: vocabulary

| Record | Title | Status | Date | Topics | Derived links |
| --- | --- | --- | --- | --- | --- |
| [predicate-rename-batch-1](predicate-rename-batch-1.md) | Predicate Rename Batch 1 — `abc:reading` and `abc:copyrightExpired` | accepted | 2026-04-29 | lod, vocabulary | amended by [rights-assessment-and-external-statements](rights-assessment-and-external-statements.md) [scope: Boolean-to-external-rights RDF mapping] |
| [vocabulary-review](vocabulary-review.md) | Vocabulary Review and `abc:` Namespace Consistency | accepted | 2026-04-29 | lod, vocabulary | amended by [predicate-rename-batch-1](predicate-rename-batch-1.md) |

## Legacy numbers

| Legacy | Record |
| --- | --- |
| 1 | [manifest-identity](manifest-identity.md) |
| 2 | [parser-evaluation](parser-evaluation.md) |
| 3 | [nix-materialization](nix-materialization.md) |
| 4 | [supply-chain-release-security](supply-chain-release-security.md) |
| 5 | [operational-runtime](operational-runtime.md) |
| 6 | [v0-design-bundle-validation](v0-design-bundle-validation.md) |
| 7 | [external-parser-validation-boundary](external-parser-validation-boundary.md) |
| 8 | [abc-tools-runtime](abc-tools-runtime.md) |
| 9 | [imported-output-materialization](imported-output-materialization.md) |
| 10 | [manifest-identity-hardening](manifest-identity-hardening.md) |
| 11 | [generated-fixture-policy](generated-fixture-policy.md) |
| 12 | [tei-odd-schematron-validation](tei-odd-schematron-validation.md) |
| 13 | [cultural-heritage-lod-profile](cultural-heritage-lod-profile.md) |
| 14 | [iiif-applicability](iiif-applicability.md) |
| 15 | [temporal-modeling](temporal-modeling.md) |
| 16 | [edtf-level1-decade-century](edtf-level1-decade-century.md) |
| 17 | [vocabulary-review](vocabulary-review.md) |
| 18 | [predicate-rename-batch-1](predicate-rename-batch-1.md) |
| 19 | [reserved](reserved.md) |
| 20 | [person-identity-drift-data-model](person-identity-drift-data-model.md) |
| 21 | [person-identity-drift-harness](person-identity-drift-harness.md) |
| 22 | [upstream-ingest-drift-awareness](upstream-ingest-drift-awareness.md) |
| 23 | [owned-aat-parser-ir-mapping](owned-aat-parser-ir-mapping.md) |
| 24 | [parser-ir-span-and-ruby-direction](parser-ir-span-and-ruby-direction.md) |
| 25 | [parser-ir-publication-rendering](parser-ir-publication-rendering.md) |
| 26 | [analysis-artifact-identity](analysis-artifact-identity.md) |
| 27 | [analysis-packs-and-tokenizer-profiles](analysis-packs-and-tokenizer-profiles.md) |
| 28 | [ruby-annotation-view](ruby-annotation-view.md) |
| 29 | [diagrams-as-gated-derived-views](diagrams-as-gated-derived-views.md) |
| 30 | [aozora-parser-selection](aozora-parser-selection.md) |
| 31 | [adr-governance-validation](adr-governance-validation.md) |
| 32 | [parser-fork-hard-detach](parser-fork-hard-detach.md) |
| 33 | [source-bundle-identity](source-bundle-identity.md) |
| 34 | [typed-evidence-and-lifecycle-closure](typed-evidence-and-lifecycle-closure.md) |
| 35 | [rights-assessment-and-external-statements](rights-assessment-and-external-statements.md) |
| 37 | [terminal-provenance-colophon-split](terminal-provenance-colophon-split.md) |
| 38 | [custom-parser-ownership-and-neutral-comparison](custom-parser-ownership-and-neutral-comparison.md) |
| 39 | [custom-parser-release-qualification](custom-parser-release-qualification.md) |
| 40 | [process-tree-memory-qualification](process-tree-memory-qualification.md) |
| 41 | [parser-release-instrument-bindings](parser-release-instrument-bindings.md) |
| 42 | [portable-parser-rq-evidence-integrity](portable-parser-rq-evidence-integrity.md) |
| 43 | [subtractive-evidence-simplification](subtractive-evidence-simplification.md) |
