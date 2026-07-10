# JADH WIP Presentation Design

Date: 2026-07-10
Status: Revised after review; pending approval for implementation planning

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

The committed deck path is:

`docs/presentations/jadh-2026-wip-slides.md`

This root-level documentation location is intentional: the deck spans both ABC
and `ab-validator`, while the presentation-diagram design remains under `abc/`
because ABC owns that publication projection. Every resource path written into
the deck is relative to the deck file, not to the repository root or shell
working directory. Paths mentioned by this design document itself are
repository-root-relative unless a passage explicitly identifies them as paths
written into the deck.

## Medium and Scope

- English slide text with untranslated Japanese examples.
- Standard Pandoc slide Markdown.
- YAML metadata includes `type: slides` and `aspect-ratio: 16-9`.
- Exactly 26 main slides, with zero to four appendix slides and no more than 30
  slides in total.
- Dedicated time for a live demonstration.
- Level-one headings start slides; level-two headings structure content within a
  slide.
- Most slides communicate one claim with no more than approximately five bullets.

The planning budget is 25 minutes: approximately 18 minutes of prepared talk,
5 minutes of bounded live demonstration, and 2 minutes of transition/buffer.
Appendix slides are not part of the timed sequence.

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
- retain the five measured parser lanes and report leadership per measurement,
  without inventing an overall score: the `aozora-pipeline` candidate has the
  highest measured frequency-weighted coverage (0.969), passes 22/25 `must`
  conformance vectors, and has 1.000 robustness; `aozora2html` has the highest
  isolated fidelity measurement (0.974), and `aozora-rs` is the speed leader;
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

The deck may state that the checked-in comparison study recommends
`aozora-pipeline` as the base for future parser work because it is the only
candidate measured as top-tier on both fidelity and robustness while also
leading frequency-weighted coverage. It must present that as a multi-criterion
engineering recommendation, not a collapsed scholarly score, and it must not
imply that Soranoha's consolidated parser is complete.

### Parser lane crosswalk

The slides use stable audience-facing names and introduce current report names
once:

| Slide label | Current report/native label | Upstream or role |
| --- | --- | --- |
| `aozora-pipeline` | `aozora`, `ab-aozora`, or `aozora-pipeline` according to measurement path | `P4suta/aozora`; reference candidate and recommended future base, not completed Soranoha consolidation |
| `aozora2` | `aozora-core` for native parser measurements | `takahashim/aozora2` |
| `aozora-rs` | `aozora-rs-core` for native token-stream measurements | `kinoko0518/aozora-rs` |
| `aozora2html` | `aozora2html` | Ruby parser lane |
| `AozoraEpub3` | `aozora-epub3` | Java/EPUB parser lane |

Tables must not mix labels from different columns without explaining the
measurement path.

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

From the deck file, the slide Markdown references the expected generated SVG
files using these paths:

- `../../abc/docs/figures/soranoha-reproducibility-architecture.svg`
- `../../abc/docs/figures/soranoha-publication-pipeline.svg`

If the SVG files are unfinished at drafting time, the Markdown retains the final
paths and marks their status in non-rendered HTML comments. It does not create
substitute figures or claim that in-progress work is complete.

The figure-producing branch is a hard sequencing dependency for final deck
validation. Slide drafting and non-image validation may proceed before it lands,
but the deck cannot be declared complete or rendered for delivery until both SVG
paths resolve from the committed deck and the imported figures have passed their
own drift/rasterization checks.

## Worked Evidence and Demo

The principal worked mapping deliberately uses 傍点 instead of the abstract's
three-nested-ruby example because it demonstrates policy-bearing normalization
across the complete publication chain rather than only structural collapse. Its
checked-in evidence is:

- `ab-validator/docs/superpowers/reports/2026-07-08-aozora-parser-comparison-study.md`,
  section 2, worked example;
- `ab-validator/data/aat-to-parser-ir-mapping-v1.json`, rule `A-06`, where AAT
  `style` projects to Parser-IR `emphasis` under the `AMBIGUITY` taxonomy; and
- `abc/data/parser-ir-publication-policy-v0.json`, where Parser-IR `emphasis`
  becomes TEI `<hi>` and visible plaintext retains only visible text.

The ambiguity taxonomy records a sidecar by default. The slide must distinguish
this taxonomy-level preservation contract from the observed 130 `A-06` mappings;
it must not claim that every observed style instance is specifically 傍点.

The evidence chain shown is:

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

- `references/abstract-refs.bib`; and
- `references/digital_humanities_abstracts.csl`.

These files are committed vendored copies of
`../../../archive/abc/paper/abstract-refs.bib` and
`../../../archive/abc/paper/digital_humanities_abstracts.csl`, respectively,
where the provenance paths are expressed relative to the deck. The external
archive is provenance only and is not a build dependency. Implementation copies
the two files byte-for-byte into `docs/presentations/references/` and records
their source SHA-256 values in an HTML comment near the YAML metadata.

Citations appear on the slides where their claims are introduced. A final
references or links slide may complement Pandoc's generated bibliography but
must not replace citations in the argument.

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
- both final image paths resolve from the deck; a draft with unresolved images
  may be reviewed but cannot pass completion validation;
- the vendored BibTeX and CSL files match their recorded provenance SHA-256
  values and the external archive is not required for rendering;
- every quantitative headline is checked against the latest authoritative
  checked-in report;
- the main sequence contains exactly 26 slides and the complete source contains
  between 26 and 30 slides including appendices;
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
