# Comment Quality Cleanup Design

**Goal:** Remove or rewrite all transient comment references (plans, specs,
handoffs, tasks, internal issue numbers, project phases, speculative language)
from Rust and Clojure source. The only allowed external references in comments
are to code symbols, specific ADRs (e.g., ADR 0013), and a small set of
stable invariant names that happen to look like issue numbers but function as
concept identifiers (see Category D, Track 1).

> **Note on location:** This document itself lives at a transient dated-spec
> path (`docs/superpowers/specs/2026-07-13-*.md`). Once this cleanup is
> complete, extract its permanent rationale (the taxonomy, keep-rules, and
> verification recipe) into an ADR; this file then becomes self-referential
> and is deleted with the rest of `docs/superpowers/`.

## Scope

- **Rust:** `ab-validator/` source files. Exclude: `target/`, `.cargo/`,
  `third_party/`
- **Clojure:** `abc/src/` source files
- **Test files:** included — transient references in test comments are equally
  invalid
- **Not in scope:** comments referencing upstream/external specifications
  (JIS, Unicode, TEI, IIIF, RDF, SHACL, JSON-LD, aozora.gr.jp); the "Tier-A
  canary" invariant name; the 6 stable invariant names from Category D Track
  1; `U+XXXX` codepoint notation; and references to local algorithm stages
  (see Keep Rules below).

## Categories to Rewrite or Remove (Unified Taxonomy)

Each problematic reference type is assigned a single owner category and a
rewrite rule. Overlaps are resolved here — no comment should be covered by
more than one category at the ownership level. (Detection at the grep layer
may produce overlaps; where a match is flagged by multiple criteria
verification commands, the executor resolves it once per comment.)

### A. Handoff References (4 occurrences, 4 files)

`docs/handoffs/...` references.

**Rule:** If the referenced document contains rationale not captured elsewhere,
extract that rationale into an ADR (see Prerequisite below), then rewrite the
comment as a self-contained explanation referencing the ADR. If the reference
is purely historical, delete it.

### B. Task / Plan Tracking Labels (~50 occurrences, ~30 files)

"Task N", "Plan G.4", "plan amendment 2", "Plan Blocker", "Phase N rotation
B", and similar project-management labels.

**Rule:** Delete the label. These carry zero meaning to a code reader. When
the label is embedded mid-sentence, rewrite the sentence to make sense without
it (see Rewrite Rules below).

Heaviest concentration: `ab-validator/crates/ab-aozora-aat/src/lib.rs` (18
occurrences).

### C. Dated Spec / Report References (~20 occurrences, ~10 files)

`docs/superpowers/specs/YYYY-MM-DD-*` and
`docs/superpowers/reports/YYYY-MM-DD-*`.

**Rule:** Same as handoffs — extract any non-durable rationale into an ADR
first (see Prerequisite), then rewrite each comment as self-contained
technical explanation.

Heaviest concentration: `ab-aozora-aat/src/lib.rs` (7 references to
`terminal-provenance-colophon-split.md`, whose source document is already
absent from the repo).

### D. Internal Issue Tracker Numbers

191 occurrences across 42 files, spanning 26 distinct issue numbers. The
project's own issue tracker numbers appearing in comments fall into two
tracks, distinguished by whether the number names a **durable semantic
invariant** used as cross-file vocabulary, or a **feature label / one-off
reference** that serves only to say "this was done as part of issue N."

#### Track 1 — Stable invariant names (6 numbers, ~101 occurrences) — KEEP

These issue numbers name specific design concepts. They are used consistently
across multiple files as shared vocabulary, exactly like "Tier-A canary" (the
no-bare-`［＃` invariant). The number itself is the unique, greppable
identifier. These are **not transient** and are **not removed**.

| #NNN | Occurrences | Concept |
|------|-------------|---------|
| #228 | 16 | Double-render invariant — styled text must never appear twice in output |
| #435 | 21 | Gothic typeface (ゴシック体) vs 太字 — first-class distinct constructs |
| #333 | 21 | Non-adjacent interior referent resolution for forward directives |
| #78  | 15 | Compound structural markers (字下げ+字組み, 改行, etc.) |
| #384 | 15 | Ruby-base forward emphasis — render-only decoration on a ruby base |
| #331 | 13 | Dotted-letter composition (ドット付き) — combining dot on Latin letters |

A glossary at `docs/glossary.md` maps each #NNN to its concept and a
one-paragraph definition. This is a living reference document, not an ADR (it
may be updated as concepts evolve). The policy decision to keep these and
exclude others is recorded in an ADR (see Prerequisite, Step 0a).

#### Track 2 — Feature labels and one-off references (~90 occurrences, 20 numbers) — REWRITE

Issue numbers that name a feature or workstream (#237 = the incremental
engine, #202 = the minimal-diff splice), or appear as isolated references
without cross-file vocabulary function.

**Rule:** Full rewrite — do NOT mechanically strip `(#NNN)`. The issue tag is
often the primary rationale. Replace each comment with a self-contained
sentence describing what the code does or why it is structured as it is.

| #NNN | Occurrences | Primary files |
|------|-------------|---------------|
| #237 | 20 | `incremental.rs` (11), `output.rs` (2), `ast/mod.rs`, `intern.rs`, `document.rs`, `span.rs`, `diagnostic.rs`, `lib.rs`, `corpus_incremental_merge.rs` |
| #202 | 12 | `splice.rs` (2), `incremental.rs` (3), `source.rs` (3), `pipeline.rs`, `lib.rs`, `corpus_splice_tiling.rs`, `splice_api.rs` |
| #420 | 11 | `directive.rs` (10), `html.rs` |
| #122 | 9 | `gaiji.rs` (7), `build.rs` (2) |
| #415 | 9 | `directive.rs` (9) |
| #89  | 5 | `build.rs` (3), `suijun.rs`, `lib.rs` |
| #326 | 5 | `gaiji.rs` (4), `build.rs` |
| #181 | 4 | `gaiji.rs` (4) |
| #180 | 3 | `pipeline.rs` (2), `format.rs` |
| #84  | 2 | `directive.rs`, `forward.rs` |
| #284 | 2 | `html.rs` (2) |
| #376 | 1 | `forward.rs` |
| #385 | 1 | `html.rs` |
| #321 | 1 | `html.rs` |
| #249 | 1 | `forward.rs` |
| #189 | 1 | `gaiji.rs` |
| #115 | 1 | `forward.rs` |
| #114 | 1 | `slugs.rs` |
| #90  | 1 | `offset.rs` |

### E. "Issue 2" Spec Artifact (6 occurrences, 6 files)

`Issue 2` is a planning/spec artifact, not a tracker issue.

**Rule:** Delete. Rewrite surrounding sentence if the reference is woven in.

**Files:** `abc/src/abc/tools/analysis_identity.clj`,
`ab-validator/crates/ab-ortho-detect/src/policy.rs`,
`ab-validator/crates/ab-ortho-detect/src/historical.rs`,
`ab-validator/crates/ab-morph-analyzers/tests/ortho_reproducibility_golden.rs`,
`ab-validator/crates/ab-morph-run/src/orthographic_select.rs`,
`ab-validator/crates/ab-morph-run/src/lib.rs`

### F. Spec Decision References (2 occurrences, 2 files)

`spec Decision #10`.

**Rule:** Delete the tag. Preserve any adjacent technical content.

### G. TODOs (3 occurrences, 3 files)

**Rule:** Convert each TODO to a factual limitation statement. Do NOT gate on
implementing the feature. A TODO is a comment-quality defect regardless of
whether the missing feature is tracked elsewhere.

- `abc/src/abc/tools/materialize_import.clj:14` →
  `;; Reads entire file into memory; large run summaries may OOM.`
- `ab-validator/crates/ab-morph-run/src/pipeline.rs:1917` →
  `// --ortho-detect is not yet threaded through the parallel/warehouse/selected paths.`
- `ab-validator/crates/ab-morph-analyzers/src/ortho_compat.rs:10` →
  `// tokenize() panics on invalid input; a future signature may return Result.`

### H. Speculative / Forward-Looking Language (~5 occurrences, ~4 files)

"ready for re-evaluation", "revisit has a baseline to beat", "eventually
export", "a later task", "leaves room for splitting later".

**Rule:** Rewrite as factual statements about current behavior or delete.

### I. UNSTABLE Markers (5 occurrences, 2 files)

`//! **UNSTABLE — not subject to semver until v0.5.0.**` in
`ab-aozora-facade/src/lib.rs` (lines 116, 151) and `.../incremental.rs`
(lines 511, 910, 1438). These sit on `///` doc comments on public items
visible in generated rustdoc; they are the items' only stability signal.

**Rule:** Remove the per-item markers and add a single `# Stability` section
to `incremental.rs`'s module-level doc. This preserves the signal for all
public items once, without the repetitive #237-tainted wording:

```rust
//! # Stability
//!
//! The incremental re-parse API is pre-1.0 and not subject to semver
//! until the crate reaches v0.5.0.
```

The two markers in `lib.rs` (lines 116, 151) are removed without
replacement — they are already covered by the crate's overall pre-1.0
posture stated in its top-level docs.

### J. Transient Tier References (~15 occurrences, ~10 files)

"Tier H", "Tier 1/2" (when tied to #237), "pre-Tier-2".

**Rule:** Delete where tied to issue numbers or project-phase naming. Keep
"Tier-A canary" — it is a named invariant (no bare `［＃` in output) used
consistently across the codebase.

### K. Transient Phase References (~20 occurrences, ~12 files)

Transient project-management phases: "Phase-3 Lane B", "Phase-C", "Phase-F",
"Phase 1.2", "Phase 1.3", "Phase 2.5", "Phase-0 responsibility".

**Rule:** Delete the phase label. If the comment's technical content is woven
around it, rewrite the sentence.

**Keep:** References to the local algorithm's own stages. Specifically the
"Phase 0" through "Phase 5" markers in `ab-aat-to-parser-ir/src/sentences.rs`
(lines 273–474, 509) that describe the sentence-splitting pipeline's internal
passes. The keep rule is semantic, not by line number: any comment of the form
`// --- Phase N: <description of this function's processing stage> ---` is a
local algorithm reference and stays.

## Rewrite Rules for Woven References

When the transient reference is not a detachable suffix but is woven into the
sentence, apply these patterns:

| Pattern | Example | Rewrite |
|---------|---------|----------|
| `for #NNN` (issue as purpose) | `//! engine for #237` | `//! Owned-AST incremental re-parse engine.` |
| `(#NNN Tier N)` (mid-sentence) | `/// The #237 Tier 2 representation` | `/// The owned-AST incremental representation` |
| `(Task N): description` | `/// (Task 14): does X` | `/// Does X.` |
| `see doc/path` (dated) | `//! see docs/handoffs/foo.md` | Inline the relevant rationale or point to ADR. |

Principle: the rewritten comment must be a valid English sentence that makes
sense standing alone, with no dangling prepositions, orphaned clauses, or
unresolved references.

## Prerequisites (ordered — must complete before code edits)

### Step 0a — Policy ADR: comment reference hygiene

Write `abc/docs/adr/0036-comment-reference-hygiene.md` recording the policy
decision: the codebase does not reference its own tracker from source
comments; recurring numbers that name durable semantic invariants used as
cross-file vocabulary are kept as stable identifiers and documented in
`docs/glossary.md`. The ADR states the criterion (durable invariant vs feature
label), lists the 6 kept numbers with one-line descriptions, and delegates
full definitions to the glossary.

### Step 0b — Write the glossary

Write `docs/glossary.md` mapping each of the 6 kept invariant names to a
one-paragraph definition. This is a living reference document, not an ADR.

### Step 0c — Extract rationale from referenced docs into ADRs

1. Inventory every external document referenced by code comments
   (`docs/handoffs/*.md`, `docs/superpowers/specs/*.md`,
   `docs/superpowers/reports/*.md`)
2. For each document still on disk, read it and decide: (a) core design
   rationale → extract into a new or existing ADR in `abc/docs/adr/`; (b)
   transient notes → discard
3. For documents already absent (e.g., `terminal-provenance-colophon-split.md`),
   reconstruct the rationale from the code comments that reference it and
   capture in an ADR
4. Commit the new/extracted ADRs

Only after Steps 0a–0c are complete do code comment edits begin.

## Sequencing (File-Oriented Batching)

Organize by file, not by category, to minimize churn and reviewer load. Each
file is touched exactly once.

### Batch 1 — Heaviest files (one file each, due to density)

| File | Categories | ~Occurrences |
|------|-----------|-------------|
| `ab-validator/crates/ab-aozora-aat/src/lib.rs` | B, C, K | ~25 |
| `ab-validator/crates/ab-aozora-facade/src/incremental.rs` | D, I, J | ~15 |
| `ab-validator/crates/ab-aozora-render/src/spelling/html.rs` | D | 14 |
| `ab-validator/crates/ab-aozora-pipeline/src/lexer/classify/forward.rs` | D | 10 |
| `ab-validator/crates/ab-aozora-pipeline/src/lexer/classify/directive.rs` | D | 6 |

### Batch 2 — Medium-density files

| File | Categories | ~Occurrences |
|------|-----------|-------------|
| `ab-validator/crates/ab-aozora-pipeline/src/lexer/sanitize.rs` | B, K | ~10 |
| `ab-validator/crates/ab-aozora-encoding/src/gaiji.rs` | D | 8 |
| `ab-validator/crates/ab-aozora-pipeline/src/pipeline.rs` | D | 8 |
| `ab-validator/crates/ab-aozora-facade/src/splice.rs` | D | 6 |
| `ab-validator/crates/ab-aozora-encoding/build.rs` | D | 5 |
| `ab-validator/crates/ab-morph-run/src/hydrate/mod.rs` | B | ~4 |

Note: The following files in Batch 2's original list contain only Track 1
(#NNN invariant) references and require no edits for Category D:
`syntax/src/format.rs` (#228, #333, #435), `classify/mod.rs` (#333), 
`render_node.rs` (#228, #331, #384), `syntax/src/lint.rs` (#435),
`spelling/source.rs` (#78, #202 — where #202 is Track 2 rewrites needed),
`syntax/src/lint.rs` (#435). The remaining Track 2 refs in these files
(#202 in source.rs) are covered in Batch 3.

### Batch 3 — Remaining files grouped by directory

| Directory / File | Categories | ~Files |
|------------------|-----------|--------|
| `ab-validator/crates/ab-aozora-render/` (remaining D Track 2) | D | 3 |
| `ab-validator/crates/ab-aozora-syntax/` (remaining D Track 2) | D | 4 |
| `ab-validator/crates/ab-aozora-spec/` (remaining D Track 2) | D | 3 |
| `ab-validator/crates/ab-aozora-pipeline/src/lexer/` (remaining D, J, K) | D, J, K | 4 |
| `ab-validator/crates/ab-aozora-facade/` (remaining D, I) | D, I | 4 |
| `ab-validator/crates/ab-aozora-encoding/` (remaining D) | D | 2 |
| `ab-validator/crates/ab-aozora-pipeline/tests/` | B, J, K | 5 |
| `ab-validator/crates/ab-morph-run/src/` | B, D, G, H | 8 |
| `ab-validator/crates/ab-ortho-detect*/` | E, F, H | 5 |
| `ab-validator/crates/ab-morph-analyzers/` | B, G, K | 3 |
| `ab-validator/crates/ab-coverage/` | B | 2 |
| `ab-validator/crates/ab-aozora-corpus/` | J | 1 |
| `ab-validator/crates/ab-aozora-proptest/` | J | 1 |
| `ab-validator/crates/ab-aat-to-parser-ir/` | A, H | 2 |
| `ab-validator/crates/ab-warehouse/` | C | 1 |
| `ab-validator/crates/ab-check/` | A | 1 |
| `ab-validator/crates/ab-aozora/` | C | 1 |
| `ab-validator/adapters/aozora-epub3/` | A | 1 |
| `abc/src/abc/tools/facts.clj` | B | 1 |
| `abc/src/abc/tools/aozora_ingest.clj` | B | 1 |
| `abc/src/abc/tools/analysis_identity.clj` | E | 1 |
| `abc/src/abc/tools/materialize_import.clj` | G | 1 |
| `abc/src/abc/annotation/schema.clj` | H | 1 |
| `abc/src/abc/tools/diagram/` | J | 3 |

## What NOT to Touch

- ADR references (e.g., "ADR 0013") — these are the only allowed external
  references
- References to upstream/external specs: JIS X 0208/0213, Unicode, TEI, IIIF,
  RDF, SHACL, JSON-LD, aozora.gr.jp, aozorabunko
- "Tier-A canary" — named invariant
- The 6 stable invariant names from Category D Track 1: #78, #228, #331,
  #333, #384, #435 — these are concept identifiers, not transient issue refs
- `U+XXXX` codepoint notation — not a transient marker
- Comments that reference specific code symbols for documentation purposes
- Local algorithm-stage markers: `// --- Phase N: <description> ---` patterns
  that describe the current function's internal processing stages
- `third_party/` directory
- `.cargo/` directory (gitignored; contains 792 TODOs and ~30 UNSTABLE
  matches that are external tooling/dependency output)

## Verification Recipe

Run these from the monorepo root. All must exit with zero matches (exit code
1 from ripgrep means "no matches" — that is a pass).

### Quick smoke test (all criteria in one pass)

```bash
just validate-migration
nix build ./abc#checks.x86_64-linux.clj-kondo
nix build ./ab-validator#checks.x86_64-linux.cargo-check
nix build ./ab-validator#checks.x86_64-linux.cargo-clippy
nix build ./ab-validator#checks.x86_64-linux.cargo-fmt
```

### Criterion-specific grep commands

```bash
# C1: zero docs/handoffs/ references
rg -n --type-add 'src:*.{rs,clj,cljc,cljs}' -t src \
  'docs/handoffs/' ab-validator/ abc/src/

# C2: zero Task/Plan tracking labels
rg -n --type-add 'src:*.{rs,clj,cljc,cljs}' -t src \
  -i '\b(task\s*\d+|plan\s+[a-g]\.?\d|plan\s+amendment|plan\s+blocker)\b' \
  ab-validator/ abc/src/

# C3: zero dated spec/report references
rg -n --type-add 'src:*.{rs,clj,cljc,cljs}' -t src \
  'docs/superpowers/(specs|reports)/' ab-validator/ abc/src/

# C4: zero TRANSIENT internal issue tracker numbers
# Uses --pcre2 for negative lookbehind. Excludes: #[attr], #![macro],
# doc # Headings, U+XXXX, &NNN; HTML entities.
# Excludes the 6 stable invariant names (#78, #228, #331, #333, #384, #435).
# These are listed explicitly — no regex trick, just grep -v.
rg -nP --type-add 'src:*.{rs,clj,cljc,cljs}' -t src \
  '(?<![A-Za-z0-9_!\[+&])#\d{2,4}\b' ab-validator/ abc/src/ \
  | grep -vE '#(78|228|331|333|384|435)\b'

# C5: zero "Issue 2" or "Spec Decision #N"
rg -n --type-add 'src:*.{rs,clj,cljc,cljs}' -t src \
  -i '\b(issue\s+2|spec\s+decision\s*#?\d+)\b' \
  ab-validator/ abc/src/

# C6: zero TODOs (after conversion to limitation statements)
rg -n --type-add 'src:*.{rs,clj,cljc,cljs}' -t src \
  '\bTODO\b' ab-validator/ abc/src/

# C7: zero speculative/forward-looking language
rg -n --type-add 'src:*.{rs,clj,cljc,cljs}' -t src \
  -i '\b(revisit|re-evaluat|eventually export|a later task|someday)\b' \
  ab-validator/ abc/src/

# C8: zero UNSTABLE markers (the semver-gating phrase specifically)
rg -n --type-add 'src:*.{rs,clj,cljc,cljs}' -t src \
  'UNSTABLE.*(semver|v0\.\d|not subject to)' ab-validator/ abc/src/

# C9: zero transient Tier refs (keep "Tier-A canary")
# Matches Tier-H, Tier 1, Tier 2, pre-Tier-2, etc.
rg -n --type-add 'src:*.{rs,clj,cljc,cljs}' -t src \
  '\bTier[\s-][B-H1-9]\b' ab-validator/ abc/src/

# C10: zero transient Phase refs
# Matches project-management Phase labels (Phase-C, Phase-3 Lane B, Phase 1.2,
# post-Phase-F, pre-Phase, Phase-0 responsibility) while excluding algorithm-
# stage markers of the form "// --- Phase N: description ---" and path-
# excluding sentences.rs (where all Phase refs are algorithm-stage markers).
rg -n --type-add 'src:*.{rs,clj,cljc,cljs}' -t src \
  -i '\b(phase[- ][a-f]|phase[- ]lane|phase\s+\d\.\d|post-phase|pre-phase)\b' \
  ab-validator/ abc/src/ \
  | grep -v 'sentences\.rs'
# C10-keep: verify algorithm-stage markers are still present
rg -n --type-add 'src:*.{rs,clj,cljc,cljs}' -t src \
  '// --- Phase [0-9]:' ab-validator/crates/ab-aat-to-parser-ir/src/sentences.rs
# Expected: matches (Phase 0 through Phase 5 + Phase-2 on line 509)

# C11: the 6 stable invariant names are still present (NOT deleted)
# Each of these should return matches — their presence is the pass condition.
for n in 78 228 331 333 384 435; do
  count=$(rg -cP --type-add 'src:*.{rs,clj,cljc,cljs}' -t src \
    "(?<![A-Za-z0-9_!\[+&])#${n}\b" ab-validator/ abc/src/ \
    | cut -d: -f2 | paste -sd+ | bc)
  echo "#${n}: ${count:-0} occurrences"
done
```

### Verification notes

- C4 requires `--pcre2` (`-P`) for the negative lookbehind. The rest use
  ripgrep's default regex engine.
- C4 filters out the 6 stable invariant names via `grep -v` on the #NNN
  values. These are listed explicitly — not excluded by regex — so the command
  is auditable.
- C10 excludes `sentences.rs` (all Phase refs there are algorithm-stage
  markers) and uses `phase[- ][a-f]` to match transient lettered phases
  (Phase-C, Phase-F) without matching numbered algorithm phases (Phase 0–5).
- C10-keep is a presence check for algorithm-stage markers.
- C11 is a presence check for the 6 kept invariant numbers.
- Exit code 1 from ripgrep means "no matches found" (pass for C1–C10). Exit
  code 0 means matches were found (fail for C1–C10, pass for C11).
- The `.cargo/` directory is excluded from the search path and from the
  `--type-add` glob. It is gitignored and contains third-party dependency
  files.
- Use `awk` instead of `bc` for summing occurrence counts: `rg -c ... \
  | awk -F: '{s+=$2} END{print s}'` (bc may not be present).

## Success Criteria

After all batches are complete:

1. Criteria C1–C10 grep commands exit with exit code 1 (zero matches)
2. Criterion C11 shows the 6 stable invariant names present at expected counts
3. `just validate-migration` passes
4. Per-language checks from AGENTS.md pass:
   - `nix build ./abc#checks.x86_64-linux.clj-kondo`
   - `nix build ./ab-validator#checks.x86_64-linux.cargo-check`
   - `nix build ./ab-validator#checks.x86_64-linux.cargo-clippy`
   - `nix build ./ab-validator#checks.x86_64-linux.cargo-fmt`
5. All existing ADR references remain intact (verified by diffing ADR grep
   output before and after)
6. All upstream-spec references remain intact
7. Policy ADR 0036 and `docs/glossary.md` are committed

## External Docs Cleanup (Batch 4)

After all code changes pass verification, delete the now-unreferenced
directories:
- `abc/docs/handoffs/`
- `docs/superpowers/specs/` (keeping only this design doc until the ADR
  extraction from the Note on Location is done)
- `docs/superpowers/reports/`
