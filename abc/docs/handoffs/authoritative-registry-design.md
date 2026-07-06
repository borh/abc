# ABC-Owned Authoritative Aozora Marker Registry — Provisional Design

> **PROVISIONAL — Hammock-Driven Design artifact. Do not implement until the
> open questions in §7 are resolved and the registry file/drift policy is
> accepted.**
> This document frames decisions; it does not pick a final format, location,
> or generator implementation.
>
> 2026-07-04 update: the 13-page draft-registry extraction probe is recorded in
> `docs/handoffs/measurement-probes-2026-07-04.md`. It produced 614 marker
> occurrences, 355 distinct raw markers, and 262 normalized templates. The
> remaining decision is curation/governance and file/drift shape, not whether
> the manual pages can be harvested.

## 1. Problem statement

An **authoritative marker registry** for Aozora Bunko markup is a single,
machine-readable, ABC-owned catalogue of every `［＃...］` annotation marker
that ABC's parser scorecards, coverage matrix, taxonomy generator, and AAT
schema care about.

Authoritative means:
- **Derived from the primary source:** `annotation/*.html` on `aozora.gr.jp`
  (the project's own prose specification), corrected by ground-truth
  observation from the real `cards/*/txt` corpus.
- **ABC-owned:** licensed under ABC's own terms, with no transitive dependency
  on AozoraEpub3's expression or a gitignored `references/` checkout.
- **Machine-readable:** directly consumed by the taxonomy generator in
  `ab-validator`, by coverage reporting, and by AAT/parser-IR vocabulary
  decisions.

It must NOT be: a copy of `chuki_tag.txt`'s marker→HTML mappings; a feature
list inflated by AozoraEpub3's converter-specific entries; dependent on a
gitignored third-party checkout as a build input.

## 2. Source model (ranked by authority)

| Rank | Source | Role | Authority |
|---|---|---|---|
| 1 | `annotation/*.html` (aozora.gr.jp) | DOCUMENTED markers + families | Authoritative |
| 2 | real `cards/*/txt` corpus | OBSERVED tier — closes the 16% doc gap | Ground truth |
| 3 | `chuki_tag.txt` (AozoraEpub3) | advisory `aozoraepub3_handles: bool` only | None as source |

### 2.1 Can annotation/*.html be mechanically parsed? — **RESOLVED YES**

See `ab-validator/docs/handoffs/authoritative-registry-evidence.md` §1. Every
page has clean `<h1>/<h2>/<h3>` headings carrying the sub-family taxonomy;
`index.html` lists the top-level family enum; markers are mechanically
extractable. The `(marker, family, sub-family, source_page)` tuple is
trustworthy without interpretation. Only the prose `description` needs
human/LLM curation (can be left `NEEDS_REVIEW` initially).

### 2.2 detail.html

Not UTF-8 (`unknown-8bit`); defer. Per-family pages are sufficient; detail.html
is a cross-reference at most.

### 2.3 Real corpus closes the 16% gap

`status` enum: `DOCUMENTED` / `OBSERVED` / `DOCUMENTED-AND-OBSERVED` /
`DEPRECATED`. The 5 known OBSERVED constructs (Unicode-codepoint gaiji ×2,
figure-with-dimensions, kunten 返り点 (ツ)/(フ)) become first-class entries with
`source_corpus_observed: true`, `source_manual_pages: []`.

### 2.4 chuki_tag.txt demoted permanently

Read only for the `aozoraepub3_handles` advisory boolean and a post-generation
diff. Never a source for entries. License rationale in §5.

## 3. Registry shape & format

### 3.1 Entry schema

| Field | Type | Meaning |
|---|---|---|
| `marker_normalized` | string | `［＃N字下げ］` etc. (reuse existing normalization) |
| `family` | enum | from index.html: Layout/Headings/Gaiji/Kunten/Emphasis/Graphics/Other/Duplication |
| `sub_family` | string? | from page heading |
| `status` | enum | DOCUMENTED / OBSERVED / DOCUMENTED-AND-OBSERVED / DEPRECATED |
| `source_manual_pages` | string[] | `annotation/<page>.html` citations |
| `source_corpus_observed` | boolean | |
| `description` | string? | prose; `NEEDS_REVIEW` until curated |
| `aat_node_kind` | string? | `emphasis`/`gaiji`/`kunten`/`figure`... |
| `parser_ir_pointer` | string? | JSON pointer |
| `aozoraepub3_handles` | boolean? | advisory |
| `notes` | string? | |

### 3.2 Envelope

```json
{"registry_id":"https://w3id.org/abc/registries/aozora-marker-v1",
 "registry_version":"1.0.0","registry_schema_hash":"sha256:...",
 "generated_at":"...","aozora_annotation_git_ref":"...",
 "corpus_sample_ref":"...","entries":[...]}
```

### 3.3 Format options (decide post-prototype)

| Option | Pros | Cons | Falsifier |
|---|---|---|---|
| A. JSON Schema + JSON in `abc/schemas/`+`abc/data/` | matches dominant pattern; Rust-readable without Clojure | | if Rust is sole heavy consumer ✓ likely |
| B. EDN in `abc/data/` | matches `aat-parser-ir-compatibility.edn` | Rust needs EDN parser | if registry read by ab-validator without Clojure |
| C. split schema in abc / instance in ab-validator | generator+consumer collocated | ABC loses instance ownership | if registry is ABC policy artifact |

**Preliminary lean:** A (matches dominant `schemas/*.json` pattern).

## 4. Generation & drift

| Generator option | Lives in | Produces |
|---|---|---|
| G1. Rust bin (revise `generate_taxonomy`) | ab-coverage | reads sources → emits registry JSON → verifies §0 |
| G2. Clojure tool `abc.tools.aozora-marker-registry` | abc | emits registry; ab-validator only validates |

**Preliminary lean:** G1 (existing machinery, real-corpus walk already in Rust).

Drift gate: mirror `tei-profile-drift` — `checks.aozora-marker-registry-drift`
regenerates and byte-diffs. Real-corpus walk is heavy → use a
periodically-refreshed committed fixture (C2), not per-build (the OBSERVED set
is stable at ~4,000-file samples per the evidence).

## 5. License cleanliness

`chuki_tag.txt` is tabular **expression** of facts; facts aren't copyrightable,
specific mappings/ordering ARE. ABC's registry must:
- derive entries from `aozora.gr.jp`'s own spec + direct corpus observation
- NEVER transcribe rows, HTML mappings, or ordering from `chuki_tag.txt`
- MAY read it for the `aozoraepub3_handles` boolean and a diff check

Clean boundary: AozoraEpub3 stays an implementation benchmark, never a source.

## 6. Relationship to existing artifacts

| Artifact | Becomes |
|---|---|
| `PARSER_REPORT.md` §0 | a view over the registry; regenerate/verify from entries |
| `ab-coverage/src/bin/generate_taxonomy.rs` | rewritten to consume the registry; chuki_tag demoted |
| `schemas/parser-ir.schema.json` | registry's `aat_node_kind` informs v2 vocab (kunten, warigaki) |
| `data/aat-parser-ir-compatibility.edn` | parallel registry; both under `abc/data/` (or `abc/data/registries/` if 2nd lands) |

## 7. Unknowns & open questions

1. ~~Mechanical parsability~~ — **resolved YES** (evidence §1)
2. Exact location & format (cross-repo split impact on Nix drift checks) — open
3. Real-corpus refresh cadence — preliminary lean: periodic fixture, not per-build
4. OBSERVED-tier governance (one observation vs two samples) — open
5. Where to track proposed AAT v2 vocabulary (kunten/warigaki) — registry vs separate AAT-evolution doc
6. aozorabunko checkout path stability for CI — open
7. DEPRECATED policy — `henkoten.html` may signal deprecations; investigate

## 8. Prototype recommendation

The linchpin prototype (can we mechanically extract trustworthy tuples from
annotation/*.html?) is **already done** — see evidence §1. Result: YES, every
page has clean headings carrying sub-families; markers are mechanically
extractable.

**Next prototype status:** completed as a measurement probe, not as production
code. See `docs/handoffs/measurement-probes-2026-07-04.md`.

Results:

- 13 pages harvested from the live `https://www.aozora.gr.jp/annotation/`
  index.
- 614 marker occurrences.
- 355 distinct raw marker strings.
- 262 normalized marker templates.
- 231 singleton normalized templates and 31 multi-example templates.

The generate-then-curate model is mechanically maintainable. The open question
is policy: whether descriptions may remain `NEEDS_REVIEW` in an initial
registry. If descriptions must be complete before promotion, all 262 normalized
entries need description review or authoring; if they can remain deferred, the
first manual pass can focus on the 54 raw-marker heuristic bucket plus
observed-only corpus constructs.

**What would kill the approach now:** if the family/sub-family heading
structure broke for >1 page (it didn't — verified all 13), or if parametric
normalization couldn't collapse variants deterministically (it can — the
existing generator already does it).

---

*Provisional spec 2026-07-02. Built on `taxonomy-authority-correction.md`,
`aozora-manual-integration-audit.md`, `owned-mapping-design.md`, and the
existing `generate_taxonomy.rs`. Per the hammock skill: incubate before
committing to generate the registry.*
