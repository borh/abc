# JADH WIP Presentation Revision Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Revise the existing JADH Pandoc deck into a 27-slide main talk plus eight to ten appendix slides that positions TEI-EAJ as parallel work, shows reproducible real XML comparisons, and gives every displayed Soranoha datum an exact repository-backed inspection or regeneration recipe.

**Architecture:** Keep the existing vendored bibliography and evidence-disciplined parser narrative. Restructure the deck around the downstream generator's metadata title view, add one early TEI-EAJ positioning slide, return to TEI-EAJ at a two-slide real XML comparison, and move high-density examples into a large appendix. Keep the Pandoc-flavored Markdown backend-neutral.

**Tech Stack:** Pandoc Markdown with citeproc, BibTeX, CSL, pinned Nix flake inputs, TEI XML, checked-in reports and generated SVGs.

## Global Constraints

- Work in `feat/jadh-wip-presentation` at `.worktrees/jadh-wip-presentation`.
- Preserve the vendored BibTeX/CSL bytes and provenance hashes.
- Retain YAML `author: Bor Hodošček` and `date` for the website generator.
- Retain YAML author/date for the website generator; remove the duplicate manual title heading.
- Produce exactly 27 rendered main slides.
- Produce 35–37 rendered slides total, including eight to ten appendix slides.
- Budget 22 minutes prepared talk, 5 minutes bounded demo, 3 minutes buffer.
- Use one early TEI-EAJ positioning slide; place detailed TEI-EAJ discussion beside the real XML comparison.
- Frame TEI-EAJ and Soranoha as parallel, complementary programs.
- Cite TEI-EAJ scholarship with `[@teieaj2023; @okada2023]` and use labeled Markdown links for web resources.
- Preserve the corrected `aozora2html` wording: 302 ruby-heavy failures, 24.7% of ruby mass missed, and 0.983 work-count completion hiding mass concentration.
- Every displayed Soranoha number/table/XML/example has an adjacent HTML comment with `Evidence path`, `Inspect/regenerate`, `Inputs`, `Expected`, and `Class`.
- Do not use sibling-archive, `/db`, `/home/bor`, or untracked `references/` paths as active evidence/demo inputs.
- Do not substitute reduced fixtures for a claimed real XML comparison.
- Do not declare completion until the figure, XML, and demo gates all pass.

---

## Existing Completed Foundation

Do not redo these commits:

- `57c60650` — vendored citation inputs and deck foundation.
- `ab5a7b25` — source-authority opening.
- `84690447` — parser/publication evidence.
- `8dbc4dad` — textual deck.
- `7129b3bb` — corrected mass-weighted robustness evidence.

## Files

- Modify: `docs/presentations/jadh-2026-wip-slides.md`
- Preserve: `docs/presentations/references/abstract-refs.bib`
- Preserve: `docs/presentations/references/digital_humanities_abstracts.csl`
- Consume when ready: `abc/docs/figures/soranoha-reproducibility-architecture.svg`
- Consume when ready: `abc/docs/figures/soranoha-publication-pipeline.svg`
- Consume: pinned `TEI-EAJ/aozora_tei` commit `77a675fc2771936f9544505d922d4cd45075338c`

---

### Task 1: Make Title Metadata, Slide Counting, and Links Deterministic

**Files:**
- Modify: `docs/presentations/jadh-2026-wip-slides.md`

**Interfaces:**
- Consumes: current 29-heading deck.
- Produces: one metadata-driven title view and no bare visible URLs.

- [ ] **Step 1: Keep metadata and remove the duplicate manual title**

Keep this YAML:

```yaml
title: "Sustaining Aozora Bunko as Versioned Corpus Infrastructure"
subtitle: "Evidence-driven source mapping, parser comparison, and analytical views"
author: Bor Hodošček
date: July 2026
type: slides
aspect-ratio: 16-9
```

Delete the manual `# Sustaining Aozora Bunko as Versioned Corpus Infrastructure` section and its visible author line. The downstream generator owns title rendering.

- [ ] **Step 2: Replace every visible bare URL**

Use labeled links, including:

```markdown
[Soranoha repository](https://github.com/borh/soranoha)
[TEI-EAJ aozora_tei](https://github.com/TEI-EAJ/aozora_tei)
[TEI-EAJ aozora_tei wiki](https://github.com/TEI-EAJ/aozora_tei/wiki)
```

HTML comments may contain command text but should also prefer logical paths over URLs.

- [ ] **Step 3: Verify metadata and link syntax**

```bash
rg -n '^author: Bor Hodošček$|^date: ' docs/presentations/jadh-2026-wip-slides.md
! rg -n '^# Sustaining Aozora Bunko' docs/presentations/jadh-2026-wip-slides.md
! rg -n '(^|[[:space:]])https?://' docs/presentations/jadh-2026-wip-slides.md
```

Expected: YAML author/date retained; no duplicate manual title; no visible bare URL.

- [ ] **Step 4: Commit**

```bash
git add docs/presentations/jadh-2026-wip-slides.md
git commit -m "docs(presentation): normalize metadata title"
```

---

### Task 2: Reframe and Resequence TEI-EAJ as Parallel Work

**Files:**
- Modify: `docs/presentations/jadh-2026-wip-slides.md`

**Interfaces:**
- Consumes: TEI-EAJ repository/wiki, vendored citations, approved 27-slide outline.
- Produces: one early positioning slide and a contribution slide that does not imply succession.

- [ ] **Step 1: Insert the early TEI-EAJ positioning slide after “One Source, Many Derived Corpora”**

Use:

```markdown
# Parallel Work: TEI-EAJ aozora_tei

- Curated human encoding of Aozora works in TEI P5
- Encoding depth organized through Levels 2–5
- Shared workflow for headers, drafts, completed files, and enrichment
- Guidelines and visualization experiments that make TEI useful beyond XML

[Project repository](https://github.com/TEI-EAJ/aozora_tei) ·
[Project wiki](https://github.com/TEI-EAJ/aozora_tei/wiki)
[@teieaj2023; @okada2023]

<!--
Evidence path: abc/flake.nix; pinned input tei-eaj-aozora-tei.
Inspect/regenerate: nix run .#abc-tei-eaj-aozora-tei-source
Inputs: TEI-EAJ/aozora_tei@77a675fc2771936f9544505d922d4cd45075338c
Expected: store path containing README.md and data/complete/tei_lib_lv4/1567_tei.xml
Class: pinned upstream comparison source
-->
```

- [ ] **Step 2: Move research questions immediately after the positioning slide**

The contribution language is:

```markdown
**Parallel emphasis:** TEI-EAJ develops curated scholarly encodings; Soranoha measures and versions corpus-scale transformations.
```

Do not use “extends,” “replaces,” “successor,” or “supersedes.”

- [ ] **Step 3: Reconcile the pre-XML/data main sequence to 23 level-one headings**

Before the appendix marker there must be 23 `# ` headings. Tasks 3 and 4 add
two XML slides and one tokenizer-data slide, producing 26 headings plus the
generated title. Merge:

- “Why Compare Multiple Parsers?” with “Three Measurements, Three Questions.”
- “Worked Mapping: 傍点” with the transformation taxonomy.
- “What Is in the Corpus?” with “Rare Syntax Still Matters.”

Keep the corrected fidelity/robustness slide unchanged except for reproduction comments.

- [ ] **Step 4: Add evidence-contract comments to current quantitative slides**

For each displayed Soranoha datum, use this exact field vocabulary:

```text
Evidence path:
Inspect/regenerate:
Inputs:
Expected:
Class:
```

Commands are read-only and repository-root-relative. For example, the coverage slide uses:

```text
Evidence path: ab-validator/docs/superpowers/reports/2026-07-08-aozora-parser-comparison-study.md
Inspect/regenerate: sed -n '219,345p' ab-validator/docs/superpowers/reports/2026-07-08-aozora-parser-comparison-study.md
Inputs: checked-in July 2026 parser study; pinned-denominator reconciliation report
Expected: coverage 0.969/0.938/0.926/0.855/0.743; robustness interpretation over 17,886 works
Class: checked-in report
```

- [ ] **Step 5: Verify wording, citations, and count**

```bash
rg -n 'Parallel Work: TEI-EAJ|parallel emphasis|@teieaj2023|@okada2023' docs/presentations/jadh-2026-wip-slides.md
! rg -ni 'extends TEI-EAJ|replaces TEI-EAJ|successor|supersedes' docs/presentations/jadh-2026-wip-slides.md
python - <<'PY'
from pathlib import Path
p = Path("docs/presentations/jadh-2026-wip-slides.md").read_text()
main = p.split("# Appendix:", 1)[0]
assert sum(line.startswith("# ") for line in main.splitlines()) == 23
print("23 pre-XML/data main headings")
PY
```

- [ ] **Step 6: Commit**

```bash
git add docs/presentations/jadh-2026-wip-slides.md
git commit -m "docs(presentation): position TEI-EAJ as parallel work"
```

---

### Task 3: Add Draft-Gated Real TEI XML Comparison Slides

**Files:**
- Modify: `docs/presentations/jadh-2026-wip-slides.md`

**Interfaces:**
- Consumes: pinned TEI-EAJ Melos Level 4 XML and a work-matched generated Soranoha Melos publication bundle.
- Produces: two main slides with actual XML and exact provenance, or an explicitly blocked draft with no fixture substitution.

- [ ] **Step 1: Resolve and inspect the pinned TEI-EAJ file**

```bash
tei_root="$(nix run .#abc-tei-eaj-aozora-tei-source)"
test -f "$tei_root/data/complete/tei_lib_lv4/1567_tei.xml"
sha256sum "$tei_root/data/complete/tei_lib_lv4/1567_tei.xml"
rg -n '<teiHeader|<text|<body|<p|<ruby|<note|<persName|<said' "$tei_root/data/complete/tei_lib_lv4/1567_tei.xml" | head -40
```

Expected: real TEI-EAJ work 1567 at pinned commit `77a675f…`.

- [ ] **Step 2: Enforce the Soranoha counterpart gate**

The selected Soranoha bundle must contain:

```text
parser-ir.json
metadata-record.json
source.manifest.json
tei.xml
tei.manifest.json
tei-validation-result.json
```

Locate only tracked or reproducibly generated candidates:

```bash
rg --files abc | rg 'demo-melos-real/(parser-ir|metadata-record|source\.manifest|tei\.xml|tei\.manifest|tei-validation-result)'
```

Expected for readiness: all six logical artifact classes. If absent, retain `DRAFT XML GATE` comments and do not show a Soranoha fixture as a real edition.

- [ ] **Step 3: Validate a ready Soranoha counterpart**

When the six files exist, regenerate into a temporary directory using the exact source bundle command recorded with those artifacts:

```bash
nix run .#abc-materialize-publication -- \
  abc/paper/demo-melos-real/parser-ir.aozora2html.json \
  abc/paper/demo-melos-real/metadata-record.json \
  abc/paper/demo-melos-real/persons \
  /tmp/jadh-melos-publication \
  --source-manifest abc/paper/demo-melos-real/source.manifest.json \
  --generated-at 2026-07-04T00:00:00Z
cmp abc/paper/demo-melos-real/tei.xml /tmp/jadh-melos-publication/tei.xml
jq -e '.findings | length == 0' /tmp/jadh-melos-publication/tei-validation-result.json
```

These are the canonical monorepo target paths recorded by the paper demo trace.
If they have not landed as tracked files, the gate remains closed.

- [ ] **Step 4: Add the two XML slides only with honest status**

Slide 1 uses two short verbatim excerpts labeled:

```markdown
## Curated TEI-EAJ — 走れメロス, Level 4
## Generated Soranoha — 走れメロス, validated publication view
```

Slide 2 annotates paragraph boundaries, ruby, notes, headers, and enrichment. Use ordinary fenced XML code blocks. Include pinned TEI-EAJ commit, work ID `1567`, Soranoha ArtifactID/manifest identity, materialization command, and validation result in HTML comments.

If the Soranoha gate is closed, keep `DRAFT XML GATE` in the TEI-EAJ slide's
HTML comment and mention the in-progress comparison on the limitations slide.
Do not add an audience-facing readiness/apology slide. When the counterpart
lands, add the second XML slide and move one dense main slide to the appendix so
the main count remains 27.

- [ ] **Step 5: Verify XML provenance**

```bash
rg -n '77a675fc2771936f9544505d922d4cd45075338c|work ID.*1567|DRAFT XML GATE|tei-validation-result' docs/presentations/jadh-2026-wip-slides.md
! rg -n 'abc-melos-single-p\.xml.*generated Soranoha|tei-eaj-melos-split-p\.xml.*curated' docs/presentations/jadh-2026-wip-slides.md
```

Expected: provenance/gate text present; reduced fixtures are not mislabeled.

- [ ] **Step 6: Commit**

```bash
git add docs/presentations/jadh-2026-wip-slides.md
git commit -m "docs(presentation): add gated TEI XML comparison"
```

---

### Task 4: Expand Examples, Tokenizer Data, and the Eight-to-Ten-Slide Appendix

**Files:**
- Modify: `docs/presentations/jadh-2026-wip-slides.md`

**Interfaces:**
- Consumes: current source inventory, parser reports, tokenizer reports, mapping taxonomy.
- Produces: 35–37 rendered slides with richer examples and a reproduction appendix.

- [ ] **Step 1: Add one corpus-scale tokenizer slide to the main sequence**

Use checked-in values from `ab-validator/docs/superpowers/reports/2026-04-29-morph-full-corpus.md`: 17,894 inputs, no analyzer/comparison failures, 7,358,000 segmentation regions, boundary-F1 median `0.972972972972973`, minimum `0.6884927066450566`. Include the full evidence-contract comment and do not imply the median erases local differences.

- [ ] **Step 2: Build eight core appendix slides**

The core appendix headings are:

```markdown
# Appendix: Source Markup Inventory
# Appendix: More Aozora Syntax Examples
# Appendix: Parser Names and Measurement Lenses
# Appendix: Parser Coverage by Construct
# Appendix: Transformation Taxonomy
# Appendix: Additional Tokenizer Regions
# Appendix: Reproducing the Evidence
```

Do not add a manual References heading; citeproc's generated bibliography is the eighth appendix slide.

- [ ] **Step 3: Add up to two optional appendices only when they carry real evidence**

Allowed headings:

```markdown
# Appendix: Fuller TEI XML Excerpts
# Appendix: Additional Parser Divergences
```

Use them if the XML gate is ready or if the cited checked-in report provides enough examples. Total rendered slides must remain 35–37.

- [ ] **Step 4: Make the reproduction appendix operational**

Include labeled links and copyable commands:

```markdown
- [Pinned TEI-EAJ source](https://github.com/TEI-EAJ/aozora_tei):
  `nix run .#abc-tei-eaj-aozora-tei-source`
- Parser study:
  `sed -n '219,345p' ab-validator/docs/superpowers/reports/2026-07-08-aozora-parser-comparison-study.md`
- Mapping rule A-06:
  `jq '.mapping_rules[] | select(.rule_id == "A-06")' ab-validator/data/aat-to-parser-ir-mapping-v1.json`
- Publication fixture:
  `nix run .#abc-materialize-publication -- examples/v0/example-work/parser-ir.json examples/v0/example-work/metadata-record.json examples/v0/example-persons /tmp/jadh-publication --generated-at 2026-07-03T00:00:00Z`
```

Label the publication command as a reproducible fixture demonstration, not the real Melos XML comparison.

- [ ] **Step 5: Verify rendered count and evidence comments**

```bash
(cd docs/presentations && pandoc jadh-2026-wip-slides.md --from markdown --to native --citeproc -o /tmp/jadh-count.native)
test -s /tmp/jadh-count.native
python - <<'PY'
from pathlib import Path
p = Path("docs/presentations/jadh-2026-wip-slides.md").read_text()
for marker in ["Evidence path:", "Inspect/regenerate:", "Inputs:", "Expected:", "Class:"]:
    assert marker in p, marker
print("evidence comment vocabulary present")
PY
rm /tmp/jadh-count.native
```

- [ ] **Step 6: Commit**

```bash
git add docs/presentations/jadh-2026-wip-slides.md
git commit -m "docs(presentation): expand reproducible examples and appendix"
```

---

### Task 5: Close Figure, XML, Demo, Render, and Timing Gates

**Files:**
- Modify: `docs/presentations/jadh-2026-wip-slides.md` only for verified final locators or timing cuts.
- Consume: generated figures and tracked demo/XML bundle.

**Interfaces:**
- Consumes: Tasks 1–4 and external in-flight artifacts.
- Produces: delivery-ready source, or a clearly reported draft blocked at named gates.

- [ ] **Step 1: Close the figure gate**

```bash
test -f abc/docs/figures/soranoha-reproducibility-architecture.svg
test -f abc/docs/figures/soranoha-publication-pipeline.svg
rg -n 'viewBox="0 0 1920 1080"' abc/docs/figures/soranoha-*.svg
nix build .#checks.x86_64-linux.presentation-diagram-renderer --no-link
nix build .#checks.x86_64-linux.presentation-diagram-drift --no-link
```

- [ ] **Step 2: Close the XML gate**

Repeat Task 3 Steps 1–3. Expected: pinned TEI-EAJ XML, tracked/generated Soranoha counterpart, byte-reproducible materialization, zero validation findings.

- [ ] **Step 3: Close the demo gate**

```bash
rg --files abc | rg 'demo-(rashomon|melos)-real/(parser-ir|divergence|tei|plain)'
```

Every chosen demo command must use tracked/pinned input, complete in under five seconds, and have its expected output in the adjacent HTML comment. If absent, retain the narrated fallback and report that the live demo is not ready.

- [ ] **Step 4: Run citation, link, path, and claim checks**

```bash
cmp ../../../archive/abc/paper/abstract-refs.bib docs/presentations/references/abstract-refs.bib
cmp ../../../archive/abc/paper/digital_humanities_abstracts.csl docs/presentations/references/digital_humanities_abstracts.csl
! rg -n 'leads overall|best parser|authoritative parser|/db/|/home/bor/' docs/presentations/jadh-2026-wip-slides.md
! rg -n '(^|[[:space:]])https?://' docs/presentations/jadh-2026-wip-slides.md
rg -n '302 ruby-heavy|24\.7% of ruby mass|0\.983 work completion' docs/presentations/jadh-2026-wip-slides.md
```

- [ ] **Step 5: Parse with Pandoc and inspect the downstream website rendering**

```bash
(cd docs/presentations && pandoc jadh-2026-wip-slides.md --from markdown --to native --citeproc -o /tmp/jadh-final.native)
test -s /tmp/jadh-final.native
```

Render through the downstream website generator. Verify the parser coverage
table, both XML slides, tokenizer table, and appendix inventory. If content does
not fit, split the slide or move detail to the appendix.

- [ ] **Step 6: Rehearse**

```text
prepared slides <= 22:00
demo            <= 05:00
total           <= 27:00
buffer          >= 03:00
```

If over budget, mark example slides as optional or move them to the appendix. Do not remove TEI-EAJ attribution, the mass-weighted robustness explanation, or reproduction metadata.

- [ ] **Step 7: Run repository validation and commit final corrections**

```bash
just validate-migration
git diff --check
git add docs/presentations/jadh-2026-wip-slides.md
git commit -m "docs(presentation): finalize TEI-EAJ and evidence-rich deck"
```

Do not make an empty commit. If any readiness gate remains open, do not claim delivery completion.

---

## Completion Evidence

Report:

- rendered main/appendix/total slide counts;
- Pandoc version and parse command;
- downstream website-rendering inspection result;
- citation-key and vendored-file checks;
- TEI-EAJ pinned revision and selected XML path;
- Soranoha XML ArtifactID, materialization command, and validation result;
- figure drift/rasterization results;
- exact demo commands and timings;
- rehearsal duration; and
- any gate that remains open.
