# JADH WIP Presentation Design

Date: 2026-07-10
Status: Approved for slide implementation planning

## Purpose

Convert the argument of
`../archive/abc/paper/jadh-2026-updated-abstract.md` into an English,
Pandoc-flavored academic slide deck that reflects the current Soranoha system
rather than freezing the February abstract's earlier work-in-progress state.
The presentation is for digital-humanities and linguistics academics who are
fluent or native readers of Japanese.

The deck presents Soranoha as evidence-driven, versioned corpus infrastructure.
It gives equal scholarly weight to source-authority markup mapping, measured
parser comparison, and tokenizer-dependent analytical views, while using the
publication system as the connective infrastructure between them.

## Medium and Scope

- English slide text with untranslated Japanese examples.
- Standard Pandoc slide Markdown.
- YAML metadata includes `type: slides` and `aspect-ratio: 16-9`.
- Approximately 26 main slides and no more than 30 slides including appendices.
- Dedicated time for a live demonstration.
- Level-one headings start slides; level-two headings structure content within a
  slide.
- Most slides communicate one claim with no more than approximately five bullets.

## Scholarly Argument

The talk follows an evidence-driven narrative:

1. Aozora Bunko is widely reused research infrastructure, but its readable source
   is not merely plain text: it encodes a rich publication syntax.
2. Sustainable transformation requires an explicit authority model. Official
   documentation and corpus observation define the source evidence; parser output
   is supporting evidence rather than source authority.
3. That evidence makes parser behavior measurable. Conformance breadth,
   corpus-weighted coverage, and fidelity versus robustness answer different
   questions and must not be collapsed into one score.
4. AAT records descriptive parser evidence, while Parser-IR records policy-bearing
   publication decisions. Their separation makes preservation, normalization,
   ambiguity, and loss inspectable.
5. Versioned manifests connect those decisions to plural scholarly outputs,
   including TEI, visible plaintext, sidecars, and analytical data.
6. Tokenizer and dictionary selection is likewise a scholarly choice, so the
   system publishes parallel analytical views rather than one authoritative
   tokenized corpus.
7. The conclusion reports completed measured infrastructure separately from
   limitations and planned work.

The phrase **versioned, inspectable, comparable, and reproducible** closes the
talk and unifies the three evidence pillars.

## Current-System Reconciliation

The slide deck updates claims from the archived abstract using checked-in July
2026 evidence. In particular, it must:

- preserve the source-authority inventory figures of approximately 17,894 works,
  4.3 million de-duplicated markup occurrences, 50 inventory rows, and 10
  presentation families where the underlying evidence still supports them;
- retain the five measured parser lanes, while replacing a neutral comparison
  framing with the current finding that `aozora-pipeline` leads overall;
- distinguish curated conformance breadth from real-corpus frequency-weighted
  coverage;
- report the current weighted-coverage ordering and explain that ruby dominates
  the mass-weighted result;
- distinguish fidelity on completed works from robustness/completion, including
  the finding that `aozora2html` can be highly faithful yet operationally
  unsuitable because of poor robustness;
- describe AAT as the descriptive adapter waist and Parser-IR as the decisional
  publication waist;
- present current publication-bundle, TEI-admission, and transformation-record
  capabilities as measured infrastructure rather than future intent; and
- retain Levels 4 and 5 editorial enrichment and broader tokenizer suitability
  evaluation as future work.

Exact values used on slides must be traced to current checked-in reports during
implementation. Conflicting snapshots or denominators must be resolved in favor
of the latest explicitly reproducible report, and the slide source should cite
that evidence in a speaker comment.

## Slide Structure

The 26-slide main sequence is:

1. Title.
2. Aozora Bunko as shared research infrastructure.
3. The same source becomes many incompatible derived corpora.
4. Aozora source is readable—but not plain.
5. Research questions and contribution.
6. What counts as evidence?
7. Building the source-authority notation inventory.
8. Inventory results: 4.3 million occurrences, 50 rows, 10 families.
9. Rare syntax still matters.
10. Figure: Soranoha Reproducibility Architecture.
11. Why compare multiple parsers?
12. Three measurements: breadth, mass, and fidelity/robustness.
13. Conformance results—and why naive rankings mislead.
14. Corpus-weighted coverage.
15. Fidelity versus robustness.
16. Parser comparison: current conclusion and remaining trade-offs.
17. Figure: Soranoha Publication Pipeline.
18. Two representation boundaries: AAT and Parser-IR.
19. Worked mapping: 傍点 from source to publication.
20. Transformation records make normalization and loss inspectable.
21. TEI, visible plaintext, and sidecars.
22. Tokenization is not a neutral preprocessing step.
23. 求むる and 竹馬の友 across tokenizer configurations.
24. Live demo: evidence chain.
25. Current limitations and next work.
26. Conclusion.

Up to four appendix slides may contain the fuller markup inventory, parser
measurement details, demo fallback material, and references or artifact links.
Dense tables belong in the appendix unless the main argument depends on reading
the individual rows.

## Presentation Figures

The deck reserves two dedicated slides for figures being implemented from
`abc/docs/superpowers/plans/2026-07-10-academic-presentation-diagrams.md`:

1. **Soranoha Reproducibility Architecture** follows the source-evidence model.
   It explains how sources, parser evidence, contracts, profiles, recipes, and
   manifest identity bind reproducible scholarly views.
2. **Soranoha Publication Pipeline** follows the parser-comparison findings and
   precedes the AAT/Parser-IR worked example. Its stable argument is source →
   validated parser process → Parser-IR → manifest → scholarly outputs; the AAT
   path is explicitly current producer detail.

The slide Markdown references the expected generated SVG files:

- `abc/docs/figures/soranoha-reproducibility-architecture.svg`
- `abc/docs/figures/soranoha-publication-pipeline.svg`

If the SVG files are unfinished at drafting time, the Markdown retains the final
paths and marks their status in non-rendered HTML comments. It does not create
substitute figures or claim that in-progress work is complete.

## Worked Evidence and Demo

The principal worked mapping uses 傍点 because it demonstrates the complete
evidence chain:

```text
source marker
  -> descriptive AAT style identity
  -> AAT-to-IR ambiguity decision and sidecar record
  -> TEI <hi> plus recoverable source-marker specificity
```

The live demo uses a small, preselected work or excerpt containing ruby and one
or two additional constructs. It should complete quickly and visibly perform:

1. source-construct detection with authority-backed mappings;
2. comparison of two or three parser lanes;
3. inspection of AAT-to-Parser-IR transformation records;
4. materialization of TEI and visible-text views; and
5. comparison of tokenizer configurations over one selected region.

The demo slide is a short audience-facing runbook. Exact commands, expected
outputs, preflight requirements, and a failure-safe fallback sequence live in
HTML comments. Corpus-scale computation is prepared in advance; the live path
uses bounded inputs and checked-in or reproducibly generated evidence.

## Tokenizer Comparison

The tokenizer section retains the abstract's examples 求むる and 竹馬の友. No
Japanese translation is required. The slides emphasize analytical consequences:

- Sudachi split modes can preserve 求むる as one verbal unit while a UniDic-backed
  lane segments 求 | む | る and assigns different parts of speech;
- Sudachi C can treat 竹馬の友 as one named entity while other configurations
  produce three tokens; and
- dictionary and mode identities belong in the versioned artifact description.

The section must avoid implying that one tokenizer is universally correct.

## Citations and Bibliography

The slide source uses Pandoc citation syntax such as `[@aozorabunko2026]` and the
same citation keys as the JADH paper. Its YAML front matter reuses:

- `../archive/abc/paper/abstract-refs.bib`; and
- `../archive/abc/paper/digital_humanities_abstracts.csl`.

Paths are written relative to the final slide file location. Citations appear on
the slides where their claims are introduced. A final references or links slide
may complement Pandoc's generated bibliography but must not replace citations in
the argument.

## Content and Authoring Conventions

- Use `Bor Hodošček` as the author metadata.
- Use Pandoc YAML and citation syntax rather than renderer-specific extensions.
- Keep repository paths, commands, detailed provenance, and speaker cues in HTML
  comments unless the audience needs them.
- Prefer short source examples, highlighted values, and direct comparisons over
  full report tables.
- Do not translate Japanese examples for this audience.
- Use current component names consistently: AAT, Parser-IR, ABC, and
  `ab-validator`.
- Do not describe parser output as source authority.
- Do not conflate ArtifactID with the byte hash of a materialized artifact.
- Do not present in-progress parser consolidation, diagram generation, editorial
  enrichment, or tokenizer evaluation as completed.

## Validation

Before the deck is considered complete:

- all cited keys resolve against the reused BibTeX file;
- the Markdown parses through Pandoc as slides;
- image paths resolve, or in-progress image status is explicitly documented in
  HTML comments;
- every quantitative headline is checked against the latest authoritative
  checked-in report;
- the main deck remains between 20 and 30 slides;
- the demo commands are preflighted on bounded inputs; and
- the rendered outline remains coherent when the live demo is skipped.

## Non-Goals

- Reproducing the abstract paragraph by paragraph.
- Explaining every manifest coordinate, schema, adapter, or TEI profile detail.
- Presenting a complete parser benchmark paper in slide form.
- Running corpus-scale measurement live.
- Creating replacement diagrams while the dedicated presentation-figure work is
  in progress.
- Claiming that one parser or tokenizer is an unqualified scholarly authority.
