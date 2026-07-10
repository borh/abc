# JADH WIP Presentation Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Produce a 26-slide English Pandoc deck, plus three appendix slides, presenting Soranoha's source-authority mapping, measured parser comparison, publication evidence chain, and tokenizer comparison to a DH/linguistics audience.

**Architecture:** The deck is a checked repository projection of the archived JADH abstract and current July 2026 reports. It vendors citation inputs, consumes two separately generated ABC SVGs, and keeps evidence provenance, demo commands, and fallback instructions in HTML comments beside the claims they support.

**Tech Stack:** Pandoc 3.7 with citeproc, Pandoc Markdown, BibTeX, CSL, SVG, repository reports and JSON evidence.

## Global Constraints

- Create `docs/presentations/jadh-2026-wip-slides.md`.
- English slide text; Japanese examples remain untranslated.
- YAML includes `type: slides`, `aspect-ratio: 16-9`, and author `Bor Hodošček`.
- Exactly 26 timed main slides and 29 total slides.
- Budget: 18 minutes prepared talk, 5 minutes bounded demo, 2 minutes buffer.
- Vendor BibTeX and CSL byte-for-byte; the sibling archive is provenance, not a build dependency.
- Distinguish source authority from parser evidence.
- Keep conformance breadth, weighted coverage, fidelity, robustness, and speed separate.
- Present `aozora-pipeline` as the measured `P4suta/aozora` candidate and recommended future base, not completed Soranoha consolidation.
- Keep commands, detailed provenance, and speaker cues in HTML comments.
- Do not declare completion until both generated SVGs resolve and pass their own checks.
- Do not commit rendered HTML/PDF, corpus dumps, or demo output.

---

## File Structure

- `docs/presentations/jadh-2026-wip-slides.md` — canonical deck and comment-only demo runbook.
- `docs/presentations/references/abstract-refs.bib` — vendored bibliography.
- `docs/presentations/references/digital_humanities_abstracts.csl` — vendored CSL.
- `abc/docs/figures/soranoha-reproducibility-architecture.svg` — slide 10 input.
- `abc/docs/figures/soranoha-publication-pipeline.svg` — slide 17 input.

## Authoritative Evidence Map

| Topic | Evidence |
| --- | --- |
| Original framing/inventory/tokenizer examples | `../archive/abc/paper/jadh-2026-updated-abstract.md` |
| Parser study and 傍点 example | `ab-validator/docs/superpowers/reports/2026-07-08-aozora-parser-comparison-study.md` |
| Current denominators | `ab-validator/docs/superpowers/reports/2026-07-09-full-nix-denominator-recompute.md` |
| Fidelity/robustness | `ab-validator/docs/superpowers/reports/2026-07-08-fidelity-robustness-split.md` |
| AAT→IR rules | `ab-validator/data/aat-to-parser-ir-mapping-v1.json` |
| Publication policy | `abc/data/parser-ir-publication-policy-v0.json` |
| Source admission | `ab-validator/docs/superpowers/reports/2026-07-04-source-authority-representability.md` |
| Publication evidence | `ab-validator/docs/superpowers/reports/2026-07-06-ir-publication-coverage.md` |
| Tokenizer/demo evidence | `../archive/abc/paper/tokenizer-comparison-view.md`, `../archive/abc/paper/demo-trace.md` |

---

### Task 1: Vendor Citation Inputs and Establish a Parseable Deck

**Files:**
- Create: `docs/presentations/references/abstract-refs.bib`
- Create: `docs/presentations/references/digital_humanities_abstracts.csl`
- Create: `docs/presentations/jadh-2026-wip-slides.md`

**Interfaces:**
- Consumes: the two corresponding files in `../archive/abc/paper/`.
- Produces: self-contained citation resources and a 26-heading deck skeleton.

- [ ] **Step 1: Verify source hashes**

```bash
sha256sum ../archive/abc/paper/abstract-refs.bib ../archive/abc/paper/digital_humanities_abstracts.csl
```

Expected hashes:

```text
ecac6b1d04171239d5cd1467a37802ff8e9fe8a5820f03ebbdb05cec196ae897
51537ae9dd3a3a77a971757942c3769c722379aa78d7f2a0bec442e57197c1c9
```

- [ ] **Step 2: Add byte-identical vendored files**

Use `apply_patch` to add the exact source bytes. Do not reorder BibTeX, normalize XML, or change line endings. Verify:

```bash
cmp ../archive/abc/paper/abstract-refs.bib docs/presentations/references/abstract-refs.bib
cmp ../archive/abc/paper/digital_humanities_abstracts.csl docs/presentations/references/digital_humanities_abstracts.csl
```

Expected: both exit 0.

- [ ] **Step 3: Create YAML and 26-heading skeleton**

Start the deck with:

```markdown
---
title: "Sustaining Aozora Bunko as Versioned Corpus Infrastructure"
subtitle: "Evidence-driven source mapping, parser comparison, and analytical views"
author: Bor Hodošček
date: July 2026
type: slides
aspect-ratio: 16-9
bibliography: references/abstract-refs.bib
csl: references/digital_humanities_abstracts.csl
link-citations: true
---

<!--
Citation provenance:
- abstract-refs.bib sha256:ecac6b1d04171239d5cd1467a37802ff8e9fe8a5820f03ebbdb05cec196ae897
- digital_humanities_abstracts.csl sha256:51537ae9dd3a3a77a971757942c3769c722379aa78d7f2a0bec442e57197c1c9
Budget: 18 min talk + 5 min demo + 2 min buffer.
-->
```

Append these headings in order:

```markdown
# Sustaining Aozora Bunko as Versioned Corpus Infrastructure
# Aozora Bunko Is Shared Research Infrastructure
# One Source, Many Derived Corpora
# Readable Does Not Mean Plain
# Research Questions
# What Counts as Evidence?
# Building the Source-Authority Inventory
# What Is in the Corpus?
# Rare Syntax Still Matters
# What Makes a Corpus Version Reproducible?
# Why Compare Multiple Parsers?
# Three Measurements, Three Questions
# Conformance Breadth
# Frequency-Weighted Corpus Coverage
# Fidelity and Robustness Are Different
# Parser Comparison: What We Can Conclude
# From Authoritative Source to Scholarly Views
# Two Representation Boundaries
# Worked Mapping: 傍点
# Transformation Records Make Decisions Inspectable
# Publication Is Plural
# Tokenization Is a Scholarly Choice
# Two Regions, Different Analyses
# Live Demo: Follow the Evidence
# Current Limits and Next Work
# Versioned, Inspectable, Comparable, Reproducible
```

- [ ] **Step 4: Verify the foundation**

```bash
test "$(rg -c '^# ' docs/presentations/jadh-2026-wip-slides.md)" -eq 26
(cd docs/presentations && pandoc jadh-2026-wip-slides.md --from markdown --to revealjs --standalone --citeproc -o /tmp/jadh-skeleton.html)
test -s /tmp/jadh-skeleton.html
rm /tmp/jadh-skeleton.html
```

Expected: 26 slides and successful Pandoc output.

- [ ] **Step 5: Commit**

```bash
git add docs/presentations
git commit -m "docs(presentation): add reproducible JADH deck foundation"
```

---

### Task 2: Write Slides 1–10 — Problem, Authority, and Inventory

**Files:**
- Modify: `docs/presentations/jadh-2026-wip-slides.md`

**Interfaces:**
- Consumes: abstract, source-authority report, citations, and final reproducibility SVG path.
- Produces: the evidence-led opening.

- [ ] **Step 1: Write slides 1–5**

Use these headings and claims:

| Slide | Required content |
| ---: | --- |
| 1 | Title, subtitle, author, “Soranoha — work in progress,” repository in comment |
| 2 | Aozora uses across DH, linguistics, NLP; infrastructure question; `[@aozorabunko2026; @iwata2025]` |
| 3 | One source fans out to reading, parsers, TEI, plaintext, tokenized data, LLM/RAG; transformation is scholarly process |
| 4 | Show `吾輩《わがはい》`, gaiji, 傍点, and indentation; readable is not plain |
| 5 | Three questions: source syntax, parser preservation/loss, downstream publication/tokenizer effects |

Keep each slide to one claim and no more than five bullets.

- [ ] **Step 2: Write slides 6–9**

Required content:

| Slide | Required content |
| ---: | --- |
| 6 | Source authority = official docs + works; supporting evidence = community specs + parser measurements + TEI-EAJ |
| 7 | Extract, reconcile, group, map, gate; cite source-authority report in comment |
| 8 | 17,894 works; 4.3M de-duplicated occurrences; 50 rows; 10 families; 3,607,926 ruby; counts overlap |
| 9 | Multicolumn 26 works, tables 7, quote blocks 6; rarity is not semantic unimportance |

- [ ] **Step 3: Write slide 10 with the final path**

```markdown
# What Makes a Corpus Version Reproducible?

![Soranoha Reproducibility Architecture](../../abc/docs/figures/soranoha-reproducibility-architecture.svg){width=100%}

<!--
FINAL GATE: generated by abc/docs/superpowers/plans/2026-07-10-academic-presentation-diagrams.md.
Focus: identity binds sources, evidence, contracts, profiles, and recipes.
ArtifactID is not a materialized-file byte hash.
-->
```

- [ ] **Step 4: Verify and commit**

```bash
rg -n 'Source authority|17,894|3,607,926|FINAL GATE' docs/presentations/jadh-2026-wip-slides.md
test "$(rg -c '^# ' docs/presentations/jadh-2026-wip-slides.md)" -eq 26
(cd docs/presentations && pandoc jadh-2026-wip-slides.md --from markdown --to native --citeproc >/tmp/jadh-opening.native)
test -s /tmp/jadh-opening.native
rm /tmp/jadh-opening.native
git add docs/presentations/jadh-2026-wip-slides.md
git commit -m "docs(presentation): add source-authority opening"
```

---

### Task 3: Write Slides 11–21 — Parser and Publication Evidence

**Files:**
- Modify: `docs/presentations/jadh-2026-wip-slides.md`

**Interfaces:**
- Consumes: parser study, denominator recompute, split report, mapping, publication policy, second SVG.
- Produces: measurement-separated conclusions and the 傍点 chain.

- [ ] **Step 1: Write slides 11–12**

Slide 11 includes the crosswalk:

| Slide label | Measurement/native label |
| --- | --- |
| `aozora-pipeline` | `aozora` / `ab-aozora` |
| `aozora2` | `aozora-core` |
| `aozora-rs` | `aozora-rs-core` when measured natively |
| `aozora2html` | unchanged |
| `AozoraEpub3` | `aozora-epub3` |

Its comment says `aozora-pipeline` is `P4suta/aozora`, not completed consolidation. Slide 12 defines breadth, weighted mass, fidelity, robustness, and speed as separate questions.

- [ ] **Step 2: Write slides 13–16**

Use these exact headline values:

- Breadth: 127 vectors, 24 families, no parser passes all 25 `must`; `aozora-pipeline` 22/25 with diagnostic-only residuals.
- Weighted coverage: `aozora-pipeline` 0.969, `aozora-rs` 0.938, `AozoraEpub3` 0.926, `aozora2` 0.855, `aozora2html` 0.743.
- Explain ruby is about 90% of measured mass.
- Robustness over pinned 17,886 works: `aozora-pipeline` and `aozora-rs` 1.000.
- Highest isolated fidelity: `aozora2html` 0.974; robustness 0.983.
- Conclusion slide names per-axis leaders and ends: “This is a multi-criterion recommendation—not one overall scholarly score.”

Comments must point to the exact report sections and warn against mixing 17,894 and 17,886 denominators.

- [ ] **Step 3: Write slide 17**

```markdown
# From Authoritative Source to Scholarly Views

![Soranoha Publication Pipeline](../../abc/docs/figures/soranoha-publication-pipeline.svg){width=100%}

<!--
FINAL GATE: generated by abc/docs/superpowers/plans/2026-07-10-academic-presentation-diagrams.md.
Stable contract: source -> validated parser process -> Parser-IR -> manifest -> scholarly outputs.
AAT is current producer detail.
-->
```

- [ ] **Step 4: Write slides 18–21**

- Slide 18: AAT is descriptive/parser-shaped/raw-preserving; Parser-IR is decisional/publication-laned/loss-accounted.
- Slide 19: table with source `青空［＃「青空」に傍点］`, AAT `style_type: boten`, rule `A-06` to `emphasis`, TEI `<hi>` plus preservation evidence.
- Slide 19 comment: A-06 records 130 style mappings; do not claim all are 傍点; `AMBIGUITY` records a sidecar by taxonomy default.
- Slide 20: define `LOSS`, `INVENTION`, `AMBIGUITY`, `UNSUPPORTED`, `STRUCTURAL`.
- Slide 21: TEI, visible plaintext, sidecars, tokenizer artifacts, manifest; cite `[@okada2023]`; do not conflate ArtifactID and content hash.

- [ ] **Step 5: Verify and commit**

```bash
rg -n '22/25|0\.969|0\.974|1\.000|multi-criterion|A-06' docs/presentations/jadh-2026-wip-slides.md
! rg -n 'leads overall|best parser|authoritative parser' docs/presentations/jadh-2026-wip-slides.md
test "$(rg -c '^# ' docs/presentations/jadh-2026-wip-slides.md)" -eq 26
(cd docs/presentations && pandoc jadh-2026-wip-slides.md --from markdown --to native --citeproc >/tmp/jadh-parser.native)
test -s /tmp/jadh-parser.native
rm /tmp/jadh-parser.native
git add docs/presentations/jadh-2026-wip-slides.md
git commit -m "docs(presentation): add parser and publication evidence"
```

---

### Task 4: Write Slides 22–29 — Tokenizers, Demo, Close, Appendices

**Files:**
- Modify: `docs/presentations/jadh-2026-wip-slides.md`

**Interfaces:**
- Consumes: tokenizer comparison view, demo trace, current limitation reports.
- Produces: complete 29-slide textual deck and bounded demo fallback.

- [ ] **Step 1: Write slides 22–23**

Slide 22 says segmentation, lemma, POS, dictionary, and literary-language support are scholarly choices; publish parallel views; cite `[@kanda2025; @worksapplications2026]`.

Slide 23 uses:

| Region | Sudachi A/B | Sudachi C | Vibrato + UniDic CWJ |
| --- | --- | --- | --- |
| 求むる | 求むる（動詞） | 求むる（動詞） | 求｜む｜る（名詞／名詞／助動詞） |
| 竹馬の友 | 竹馬｜の｜友 | 竹馬の友（固有名詞） | 竹馬｜の｜友 |

Cite `[@den2008; @daactools2026]`; comment records `sudachi-20260116` and `unidic-cwj-202512`.

- [ ] **Step 2: Write slide 24 and demo comments**

Audience-facing list: inspect source mapping; compare parsers; inspect AAT→IR; materialize TEI/plaintext; compare tokenizer region.

HTML comment requirements:

```text
DEMO BUDGET: 5:00; never run corpus-scale work live.
Preflight tracked Rashōmon and Run, Melos! artifacts with rg --files.
Never use /db, /home/bor, or an untracked references/ path.
Each live read-only jq/rg/display command must complete under five seconds.
Fallback: narrate slides 19, 20, 21, and 23; do not improvise a full run.
```

- [ ] **Step 3: Write slides 25–26**

Slide 25: 字詰め and 横組 gaps; consolidation incomplete; TEI Levels 4–5 future; genre-aware tokenizer evaluation future; remeasure changed inputs.

Slide 26: inventory, measure, record loss, publish parallel views, bind identity; close with “Corpus transformation is part of the scholarly method” and repository URL.

- [ ] **Step 4: Append slides 27–29**

- Slide 27: full 10-family source inventory table copied from abstract Table 1, with overlap caveat.
- Slide 28: definitions of breadth, mass, fidelity, robustness, speed; “No single column answers every research question.”
- Slide 29: references/artifacts with citations for Aozora, TEI-EAJ, Sudachi, UniDic/Vibrato, and repository URL.

- [ ] **Step 5: Verify citation keys, count, and path hygiene**

```bash
test "$(rg -c '^# ' docs/presentations/jadh-2026-wip-slides.md)" -eq 29
! rg -n '/db/|/home/bor/' docs/presentations/jadh-2026-wip-slides.md
python - <<'PY'
import re
from pathlib import Path
deck = Path("docs/presentations/jadh-2026-wip-slides.md").read_text()
bib = Path("docs/presentations/references/abstract-refs.bib").read_text()
used = set()
for group in re.findall(r"\[@([^]]+)\]", deck):
    used.update(part.strip().lstrip("@").split()[0] for part in group.split(";"))
available = set(re.findall(r"@[A-Za-z]+\{([^,]+),", bib))
assert not used - available, sorted(used - available)
print(f"{len(used)} citation keys resolved")
PY
```

Expected: 29 slides, no machine-local paths, all keys resolved.

- [ ] **Step 6: Commit**

```bash
git add docs/presentations/jadh-2026-wip-slides.md
git commit -m "docs(presentation): complete JADH WIP narrative"
```

---

### Task 5: Integrate Figures, Preflight Demo, and Validate Delivery

**Files:**
- Modify: `docs/presentations/jadh-2026-wip-slides.md` only for verified path/claim corrections.
- Consume: both `abc/docs/figures/soranoha-*.svg` files.

**Interfaces:**
- Consumes: completed diagram branch, textual deck, bounded tracked demo artifacts.
- Produces: renderable and rehearsed delivery source.

- [ ] **Step 1: Enforce the figure gate**

```bash
test -f abc/docs/figures/soranoha-reproducibility-architecture.svg
test -f abc/docs/figures/soranoha-publication-pipeline.svg
rg -n 'viewBox="0 0 1920 1080"' abc/docs/figures/soranoha-*.svg
```

Expected: both files and both exact view boxes. If absent, stop Task 5; the deck is a draft, not complete.

- [ ] **Step 2: Run diagram verification**

From `abc/`, run:

```bash
clojure -M:abc/presentation-diagrams --check
```

From the monorepo root, run the pinned checks named by the diagram plan:

```bash
nix build .#checks.x86_64-linux.presentation-diagram-renderer --no-link
nix build .#checks.x86_64-linux.presentation-diagram-drift --no-link
```

Expected: both builds exit 0 and neither changes the committed SVGs.

- [ ] **Step 3: Resolve and preflight bounded demo commands**

```bash
rg --files abc | rg 'demo-(rashomon|melos)-real/(parser-ir|divergence|tei|plain)'
```

Update HTML comments to actual tracked paths. Run the selected read-only `jq`, `rg`, or display commands. Each must finish in under five seconds and use no machine-local inputs.

- [ ] **Step 4: Render using only repository inputs**

```bash
(cd docs/presentations && pandoc jadh-2026-wip-slides.md --from markdown --to revealjs --standalone --citeproc -o /tmp/jadh-2026-wip-slides.html)
test -s /tmp/jadh-2026-wip-slides.html
rg -n 'Soranoha Reproducibility Architecture|Soranoha Publication Pipeline|References' /tmp/jadh-2026-wip-slides.html
rm /tmp/jadh-2026-wip-slides.html
```

Expected: exit 0, non-empty HTML, both figures, resolved references.

- [ ] **Step 5: Run final invariants**

```bash
test "$(rg -c '^# ' docs/presentations/jadh-2026-wip-slides.md)" -eq 29
cmp ../archive/abc/paper/abstract-refs.bib docs/presentations/references/abstract-refs.bib
cmp ../archive/abc/paper/digital_humanities_abstracts.csl docs/presentations/references/digital_humanities_abstracts.csl
! rg -n 'leads overall|best parser|authoritative parser|/db/|/home/bor/' docs/presentations/jadh-2026-wip-slides.md
git diff --check
```

- [ ] **Step 6: Rehearse against the fixed budget**

```text
prepared slides <= 18:00
demo            <= 05:00
total           <= 23:00
buffer          >= 02:00
```

If over budget, remove commentary or bullets. Do not add slides or move appendix content into the timed sequence.

- [ ] **Step 7: Commit only if integration required tracked corrections**

```bash
git add docs/presentations/jadh-2026-wip-slides.md
git commit -m "docs(presentation): finalize demo and figure integration"
```

Do not create an empty commit.

---

## Completion Evidence

The implementation handoff reports:

- main/appendix/total slide counts;
- Pandoc version and render command;
- citation-key resolution result and vendored hashes;
- figure drift/rasterization results;
- exact bounded demo commands and preflight status;
- timed rehearsal duration; and
- any numerical claim updated because a newer reproducible report superseded this plan.
