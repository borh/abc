# Comment Quality Cleanup Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Remove ~190 transient comment references (handoffs, task/phase/plan labels, dated spec refs, internal issue numbers, TODOs, speculative language, UNSTABLE markers, transient Tier/Phase refs) across 50+ Rust and Clojure source files, keeping 6 stable invariant #NNNs and all ADR/upstream-spec references.

**Architecture:** Three phases: (0) prerequisites — policy ADR, glossary, rationale extraction; (1–3) file-oriented code edits in three batches ordered by density; (4) delete now-unreferenced `docs/handoffs/`, `docs/superpowers/specs/`, `docs/superpowers/reports/` directories. Each task edits one file or a small directory group and verifies with grep + Nix checks.

**Tech Stack:** ripgrep with PCRE2, `nix build`, `just validate-migration`, Rust `cargo check/clippy/fmt`, Clojure `clj-kondo`.

## Global Constraints

- Each task ends with verification and a commit
- Verification uses the grep commands from the spec's Verification Recipe
- `just validate-migration` must pass after every task
- Per-language checks from AGENTS.md must pass before commit
- Do NOT touch `third_party/`, `.cargo/`, ADR references, upstream-spec references, Tier-A canary, the 6 stable invariant #NNNs (#78, #228, #331, #333, #384, #435), `U+XXXX` notation, or algorithm-stage Phase markers in `sentences.rs`
- Rewrite Rule: every rewritten comment must be a valid English sentence standing alone — no bare parentheticals, no dangling clauses
- Edit safety: apply string-anchored edits first; for pure-line-number deletions, work bottom-to-top within each file so line numbers don't drift
- Where a task says "find with grep", the grep result is authoritative — line-number tables are hints that may have drifted, but the old-text strings in the tables were verified against the current tree and are reliable

---

### Task 0a: Write policy ADR 0036 — comment reference hygiene

**Files:**
- Create: `abc/docs/adr/0036-comment-reference-hygiene.md`

- [ ] **Step 1: Write the ADR**

```markdown
# ADR 0036: Comment Reference Hygiene

Status: Accepted
Date: 2026-07-13

## Context

Source comments contain references to the project's own issue tracker,
handoff documents, dated specs, task labels, and planning artifacts.
These are transient: they lose meaning once the task is closed or the
document moves.

## Decision

1. The codebase does not reference its own issue tracker from source
   comments. Feature labels and one-off issue references are rewritten
   as self-contained prose.
2. Issue numbers that name **durable semantic invariants** used as
   cross-file vocabulary are retained as stable identifiers. These
   function identically to named invariants like "Tier-A canary." Six
   such numbers are recognized: #78, #228, #331, #333, #384, #435.
   Their definitions live in `docs/glossary.md`, a living reference
   document.
3. Project-management labels (Task N, Plan G.4, plan amendment, Phase
   labels, Plan Blocker) are deleted. They carry zero meaning to a
   code reader.
4. Dated spec/report references and handoff references are rewritten as
   self-contained prose; durable rationale is extracted into ADRs
   before the reference is removed.
5. TODOs are converted to factual limitation statements. Per-item
   UNSTABLE markers are consolidated into a single crate- or
   module-level `# Stability` section.
6. References to upstream/external specifications (JIS, Unicode, TEI,
   IIIF, RDF, SHACL, JSON-LD) and to specific ADRs remain permitted.

## Consequences

- The 6 stable invariant names remain greppable cross-file vocabulary
  with a single glossary mapping them to definitions.
- ~190 transient references are removed, reducing comment debt.
- `docs/handoffs/`, `docs/superpowers/specs/`, and
  `docs/superpowers/reports/` are deleted after all code references
  are cleaned up; durable rationale is extracted into ADRs first.
```

- [ ] **Step 2: Commit**

```bash
git add abc/docs/adr/0036-comment-reference-hygiene.md
git commit -m "adr: comment reference hygiene policy (ADR 0036)"
```

---

### Task 0b: Write invariant glossary

**Files:**
- Create: `docs/glossary.md`

- [ ] **Step 1: Write the glossary**

```markdown
# Invariant Glossary

Stable concept identifiers used as cross-file vocabulary. Each entry
records the invariant name, the issue number that named it, and a
one-paragraph definition.

## #228 — double-render invariant

Styled text must never be rendered twice. When a forward directive
(emphasis, bouten, etc.) references a text run, the styled copy is the
sole rendered occurrence; the unstyled literal must be reclaimed so no
duplicate appears in output.

## #435 — gothic/太字 distinction

ゴシック体 (gothic typeface) is a first-class construct distinct from
太字 (bold). The parser keeps them separate; non-canonical corpus
variants (ゴチック, etc.) decline to `Directive{Unknown}` with a lint
suggesting the canonical form.

## #333 — non-adjacent referent resolution

A forward directive (emphasis, bouten, dotted-letter) may target a
referent that is not the immediately preceding plain-text run. The
classifier resolves the interior target position and splices the styled
decoration at it.

## #78 — compound structural markers

Structural markers (改行, 改段, 改ページ) and compound indent/line-layout
directives (ここから{N}字下げ + 字組み, etc.) form a closed set of
self-contained structural leaves recognized by the lexer.

## #384 — ruby-base emphasis

A forward directive whose target resolves to a ruby base text. Since
the ruby base is owned by the ruby node, the emphasis cannot be pulled
into a text-only forward leaf; instead a render-only `base_emphasis`
flag is set on the ruby node.

## #331 — dotted-letter composition

ドット付き directives compose a combining dot onto an addressed Latin
letter (e.g., `mは上ドット付き` → ṁ). This is a separate facility from
accent decomposition and uses a selector grammar over the reclaimed run.
```

- [ ] **Step 2: Commit**

```bash
git add docs/glossary.md
git commit -m "docs: invariant glossary for stable concept identifiers"
```

---

### Task 0c: Extract rationale from referenced docs into ADRs

**Files:**
- Read: `abc/docs/handoffs/*.md`, `docs/superpowers/specs/*.md`, `docs/superpowers/reports/*.md`
- Create: new ADRs in `abc/docs/adr/` as needed

**Interfaces:**
- Produces: ADRs capturing durable rationale before source docs are deleted

- [ ] **Step 1: Inventory referenced documents**

```bash
# Find every external doc path referenced in source comments
rg -n --type-add 'src:*.{rs,clj,cljc,cljs}' -t src \
  'docs/(handoffs|superpowers)/' ab-validator/ abc/src/ \
  | sed 's/.*\(docs\/[^ )]*\).*/\1/' | sort -u
```

Expected: list of referenced doc paths (e.g., `docs/handoffs/adding-aozora2-aozoraepub3-parser-support.md`, `docs/handoffs/crates-optimization-audit.md`, `docs/superpowers/specs/2026-07-10-consolidated-parser-phase2-absorption-design.md`, `docs/superpowers/reports/2026-07-12-terminal-provenance-colophon-split.md`, etc.)

- [ ] **Step 2: For each document still on disk, extract durable rationale**

For each document found in Step 1, read it. If it contains design rationale worth preserving, extract it into a new or existing ADR in `abc/docs/adr/`. If it's purely transient notes (handoff coordination, task breakdown), discard it.

- [ ] **Step 3: Reconstruct rationale for absent documents**

The document `docs/superpowers/reports/2026-07-12-terminal-provenance-colophon-split.md` is referenced 7 times in `ab-validator/crates/ab-aozora-aat/src/lib.rs` but is already absent from the repo. Read the code comments around each reference to reconstruct the rationale: the terminal-provenance/colophon split separates end-of-work metadata from body text in the sanitize stage, and the `sanitized_tail` field carries this terminal text with its byte offset for span rebasing through `SpanContext`. Capture this in an ADR.

Also extract the permanent rationale from this cleanup's design doc
(`docs/superpowers/specs/2026-07-13-comment-quality-cleanup-design.md`)
into an ADR: the comment taxonomy, keep-rules, invariant-name policy, and
verification recipe. This satisfies the Note on Location.

- [ ] **Step 4: Commit**

```bash
git add abc/docs/adr/
git commit -m "adr: extract durable rationale from transient docs before cleanup"
```

---

### Task 1: Rewrite `ab-validator/crates/ab-aozora-aat/`

**Files:**
- Modify: `ab-validator/crates/ab-aozora-aat/src/lib.rs`
- Modify: `ab-validator/crates/ab-aozora-aat/tests/goldens.rs`

**Categories:** B (Task/Plan labels), C (dated spec/report refs), K (transient Phase refs)
~27 occurrences across both files.

- [ ] **Step 1: Rewrite Task/Plan labels (Category B)**

All of these are mechanical deletions — the label adds nothing. Where the label is embedded mid-sentence, rewrite the sentence.

| Line | Old | New |
|------|-----|-----|
| 31 | `(Task 8's delta-audit ruby class` | `(delta-audit ruby class` |
| 59 | `Phase 4 (Task 14):` | (delete — merge with preceding sentence) |
| 166 | `NOTE (Task 14 divergence from the Python reference,` | `NOTE (divergence from the Python reference,` |
| 308 | `(Task 14 added` | `(added` |
| 1433 | `NOTE (plan amendment 2, Task 9 delta-gate block): the bare-toggle` | `NOTE: the bare-toggle` |
| 1480 | `(plan amendment 2): recurse` | `recurse` |
| 1952 | `(Task 8: ...)` | delete the parenthetical |
| 2326 | `Task 4's fix-wave concern` | `fix-wave concern` |
| 2541 | `Was the C4 identity test (Task 14); C5 (Task 8) bumps` | `Bumps` |
| 2553 | `// --- Task 14: classify_tail (transcribed from` | `// --- classify_tail (transcribed from` |
| 2696 | `// --- Task 14: source_note emission` | `// --- source_note emission` |
| 2820 | `(plan amendment, main` | `(main` |
| 2912 | `Since plan amendment 2 the adapter` | `The adapter` |
| 3203 | `(Task 9 delta-gate BLOCK → plan` | `(delta-gate BLOCK →` |
| 3268 | `// --- Phase 5 Task 5: property-test target` | `// --- property-test target` |
| 511 | `Phase 3 design spec` | `design` |
| 1124 | `its Phase 3 semantics` | `its semantics` |

- [ ] **Step 2: Rewrite dated spec/report references (Category C)**

The terminal-provenance-colophon-split.md rationale was extracted in Task 0c. Replace each reference with a brief inline note.

| Line | Old | New |
|------|-----|-----|
| 63 | `tail entirely — see docs/superpowers/reports/2026-07-12-terminal-provenance-colophon-split.md.` | `tail entirely.` |
| 167 | ```/// `docs/superpowers/reports/2026-07-12-terminal-provenance-colophon-split.md`):``` | `):` |
| 172 | `(`...split.md`)` | (delete parenthetical) |
| 614 | `/// `...split.md`, ` | (delete line) |
| 649 | `/// `...split.md`` | (delete line) |
| 738 | `/// `...split.md`), ` | `),` |
| 2555 | `// docs/superpowers/reports/2026-07-12-terminal-provenance-colophon-split.md)` | (delete line) |

- [ ] **Step 3: Rewrite handoff reference (Category A)**

| Line | Old | New |
|------|-----|-----|
| 1942 | ```/// `docs/handoffs/2026-07-10-parser-fork-provenance.md`'s feature-unification hazard section``` | ```/// The `serde_json/preserve_order` feature, if leaked into this crate's feature graph, would switch `Value`'s map from `BTreeMap` to `IndexMap`, changing serialization order.``` |

- [ ] **Step 4: Rewrite Phase labels in goldens.rs**

```bash
rg -n -i '\bphase\b' ab-validator/crates/ab-aozora-aat/tests/goldens.rs
```

| Line | Old | New |
|------|-----|-----|
| 1 | `(Phase 3 rotation B).` | (delete parenthetical) |
| 10 | `(Task 15; updated Task 14` | (delete — already covered by Task label removal above) |
| 28 | `Task 14 retains it` | `retains it` |

Also check `bare_toggle_model.rs`:

```bash
rg -n -i '\b(task|phase|plan)\b' ab-validator/crates/ab-aozora-aat/tests/bare_toggle_model.rs
```
| Line | Old | New |
|------|-----|-----|
| 4 | `plan amendment 2; Phase 5 Task 4).` | (delete parenthetical) |
| 17 | `(Task 2)` | (delete tag) |
| 204 | `(Phase 5 Task 5 spec).` | (delete parenthetical) |
| 427 | `per the task spec` | `per the spec` |

- [ ] **Step 5: Verify**

```bash
# C2: zero Task/Plan labels in this file
rg -n -i '\b(task\s*\d+|plan\s+[a-g]\.?\d|plan\s+amendment|plan\s+blocker)\b' \
  ab-validator/crates/ab-aozora-aat/src/lib.rs
# Expected: no matches

# C3: zero dated spec/report refs in this file
rg -n 'docs/superpowers/(specs|reports)/' \
  ab-validator/crates/ab-aozora-aat/src/lib.rs
# Expected: no matches

# C1: zero handoff refs in this file
rg -n 'docs/handoffs/' ab-validator/crates/ab-aozora-aat/src/lib.rs
# Expected: no matches

rg -n -i '\b(task\s*\d+|plan\s+[a-g]\.?\d|plan\s+amendment|plan\s+blocker)\b' \
  ab-validator/crates/ab-aozora-aat/
# Expected: no matches

nix build ./ab-validator#checks.x86_64-linux.cargo-check
nix build ./ab-validator#checks.x86_64-linux.cargo-clippy
```

- [ ] **Step 6: Commit**

```bash
git add ab-validator/crates/ab-aozora-aat/src/lib.rs
git commit -m "chore(ab-aozora-aat): remove transient task/plan/spec/handoff references from comments"
```

---

### Task 2: Rewrite `ab-validator/crates/ab-aozora-facade/src/incremental.rs`

**Files:**
- Modify: `ab-validator/crates/ab-aozora-facade/src/incremental.rs`

**Categories:** D (Track 2: #237, #202, #249, #284), I (UNSTABLE), J (transient Tier)
~15 occurrences.

- [ ] **Step 1: Rewrite #237 references (#237 = incremental re-parse feature)**

The #237 references name the incremental re-parse feature. Replace with descriptive prose.

| Line | Old | New |
|------|-----|-----|
| 1 | `//! Owned-AST incremental re-parse engine for #237 — the sole incremental path.` | `//! Owned-AST incremental re-parse engine — the sole incremental path.` |
| 16 | `debounced diagnostics (#237 Stage B'3).` | `debounced diagnostics.` |
| 166 | `parse's ... (#237 Tier 2).` | `parse's ... (incremental path).` |
| 191 | `region-find prologue (#237 Tier 2)` | `region-find prologue` |
| 326 | `// #237 Tier 2 (PR-1) — structure-sharing piece-sequence.` | `// Structure-sharing piece-sequence for incremental region-find.` |
| 344 | `specialisation the #237 Tier 2 design calls` | `specialisation the incremental design calls` |
| 508 | `/// The #237 Tier 2 unified, truly incremental representation of a parse's three` | `/// The unified, truly incremental representation of a parse's three` |
| 2136 | `documents (#237` | `documents (` |

**Note:** Lines 511 and 910 are handled by Step 4 (UNSTABLE) — do NOT edit them in this step.

- [ ] **Step 2: Rewrite #202 references (#202 = minimal-diff splice)**

| Line | Old | New |
|------|-----|-----|
| 1174 | `declined for safety by the #202 splice model).` | `declined for safety by the minimal-diff splice model).` |
| 1211 | `through [`classify_node_ref`] (the #202` | `through [`classify_node_ref`] (the` |
| 1212 | `splice authority) so this region-reuse guard and the #202 splice cannot` | `splice authority) so this guard and the minimal-diff splice cannot` |

- [ ] **Step 3: Rewrite one-off references (#249, #284)**

| Line | Old | New |
|------|-----|-----|
| 814 | `LSP cache's periodic forced full re-parse (#249): the incremental base` | `LSP cache's periodic forced full re-parse: the incremental base` |
| 1871 | `corpus divergence #284 surfaced:` | `corpus divergence surfaced:` |
| 1937 | `Regression for the verify finding on #284.` | `Regression for the verify finding.` |

- [ ] **Step 4: Replace UNSTABLE markers with a Stability section**

Lines 511 and 910 are on public `///` items visible in generated rustdoc — they are NOT doc-hidden. Remove both markers and add a single `# Stability` section to the module-level doc (after the existing module doc header block, around line 19):

```rust
//! # Stability
//!
//! The incremental re-parse API is pre-1.0 and not subject to semver
//! until the crate reaches v0.5.0.
```

Then rewrite the now-bare-parenthetical fragments that the marker removal left:

| Line | Old | New |
|------|-----|-----|
| 511 | `/// **UNSTABLE — not subject to semver until v0.5.0** (the #237 incremental` | `/// The incremental` |
| 910 | `/// **UNSTABLE — not subject to semver until v0.5.0** (the #237 incremental API).` | `/// The incremental API (see module-level Stability section).` |

- [ ] **Step 5: Verify**

```bash
# C4: zero transient #NNN (excluding the 6 kept)
rg -nP '(?<![A-Za-z0-9_!\[+&])#\d{2,4}\b' \
  ab-validator/crates/ab-aozora-facade/src/incremental.rs \
  | grep -vE '#(78|228|331|333|384|435)\b'
# Expected: no matches

# C8: zero UNSTABLE markers
rg -n 'UNSTABLE.*(semver|v0\.\d|not subject to)' \
  ab-validator/crates/ab-aozora-facade/src/incremental.rs
# Expected: no matches

nix build ./ab-validator#checks.x86_64-linux.cargo-check
nix build ./ab-validator#checks.x86_64-linux.cargo-clippy
```

- [ ] **Step 6: Commit**

```bash
git add ab-validator/crates/ab-aozora-facade/src/incremental.rs
git commit -m "chore(ab-aozora-facade): remove transient issue refs and UNSTABLE markers from incremental.rs"
```

---

### Task 3: Rewrite `ab-validator/crates/ab-aozora-render/src/spelling/html.rs`

**Files:**
- Modify: `ab-validator/crates/ab-aozora-render/src/spelling/html.rs`

**Categories:** D (Track 2: #415, #420, #284, #385)
~14 occurrences.

- [ ] **Step 1: Rewrite #415/#420 references (warichu/paragraph-straddle forms)**

#415 and #420 name specific warichu and paragraph-straddle behaviors. Rewrite with descriptive prose referencing the concept.

| Line | Old | New |
|------|-----|-----|
| 42 | `paragraph to EOF (#420).` | `paragraph to EOF.` |
| 49 | `block- and inline-warichu forms (9 corpus works, #415) rely on this to` | `block- and inline-warichu forms rely on this to` |
| 72 | `paragraph boundary (#420).` | `paragraph boundary.` |
| 87 | `enclosing paragraph (#415, Case 2):` | `enclosing paragraph (inline-warichu case):` |
| 94 | `straddle \`</p>\` either (#420).` | `straddle \`</p>\` either.` |
| 158 | `reopen instead of no-oping (#420).` | `reopen instead of no-oping.` |
| 213 | `forms (#415, Case 1) — is absorbed` | `forms (block-warichu case) — is absorbed` |
| 224 | `close (#415, Case 2).` | `close (inline-warichu case).` |
| 668 | `(#415) does not perturb` | `does not perturb` |
| 682 | `/// #415 Case 1: a block-form warichu open` | `/// Block-form warichu: a` |
| 701 | `/// #415 Case 2: an inline-form warichu open` | `/// Inline-form warichu: an` |
| 773 | `Byte-identity regression guard for #420:` | `Byte-identity regression guard:` |
| 785 | `straddles a paragraph close (#420).` | `straddles a paragraph close.` |
| 800–877 | (4 references to #420) | Delete the `(#420)` tag from each |

- [ ] **Step 2: Rewrite one-off references (#284, #385)**

| Line | Old | New |
|------|-----|-----|
| (find with grep) | `#284` | Delete tag, keep prose |
| (find with grep) | `#385` | Delete tag, keep prose |

- [ ] **Step 3: Verify and commit**

```bash
rg -nP '(?<![A-Za-z0-9_!\[+&])#\d{2,4}\b' \
  ab-validator/crates/ab-aozora-render/src/spelling/html.rs \
  | grep -vE '#(78|228|331|333|384|435)\b'
# Expected: no matches

nix build ./ab-validator#checks.x86_64-linux.cargo-check
nix build ./ab-validator#checks.x86_64-linux.cargo-clippy

git add ab-validator/crates/ab-aozora-render/src/spelling/html.rs
git commit -m "chore(ab-aozora-render): remove transient issue refs from html.rs"
```

---

### Task 4: Rewrite `ab-validator/crates/ab-aozora-pipeline/src/lexer/classify/forward.rs`

**Files:**
- Modify: `ab-validator/crates/ab-aozora-pipeline/src/lexer/classify/forward.rs`

**Categories:** D (Track 2: #122, #321, #115, #249, #84)
~10 occurrences.

- [ ] **Step 1: Rewrite each #NNN reference inline**

| Line | Old | New |
|------|-----|-----|
| 501 | `Standalone external-character (#122): a no-\`※\`` | `Standalone external-character (no-\`※\`): a` |
| 525 | `Standalone (no-\`※\`) external-character recogniser (#122). Reuses` | `Standalone (no-\`※\`) external-character recogniser. Reuses` |
| 1879 | `grammar owns it (#321 out-of-scope).` | `grammar owns it.` |

For remaining references (#115, #249, #84, #376), find each with:
```bash
rg -nP '(?<![A-Za-z0-9_!\[+&])#(115|249|84|376)\b' \
  ab-validator/crates/ab-aozora-pipeline/src/lexer/classify/forward.rs
```
Delete each `(#NNN)` tag, keeping adjacent prose.

- [ ] **Step 2: Verify and commit**

```bash
rg -nP '(?<![A-Za-z0-9_!\[+&])#\d{2,4}\b' \
  ab-validator/crates/ab-aozora-pipeline/src/lexer/classify/forward.rs \
  | grep -vE '#(78|228|331|333|384|435)\b'
# Expected: no matches

nix build ./ab-validator#checks.x86_64-linux.cargo-check
nix build ./ab-validator#checks.x86_64-linux.cargo-clippy

git add ab-validator/crates/ab-aozora-pipeline/src/lexer/classify/forward.rs
git commit -m "chore(ab-aozora-pipeline): remove transient issue refs from forward.rs"
```

---

### Task 5: Rewrite `ab-validator/crates/ab-aozora-pipeline/src/lexer/classify/directive.rs`

**Files:**
- Modify: `ab-validator/crates/ab-aozora-pipeline/src/lexer/classify/directive.rs`

**Categories:** D (Track 2: #415, #420, #84)
~6 occurrences of Track 2 refs. Note: #78 and #435 references in this file are Track 1 (kept).

- [ ] **Step 1: Rewrite #415 and #420 references**

| Line | Old | New |
|------|-----|-----|
| (find with grep) | `#415` in non-#78 context | Delete tag, keep prose |
| (find with grep) | `#420` in non-#78 context | Delete tag, keep prose |
| (find with grep) | `#84` | Delete tag, keep prose |

- [ ] **Step 2: Verify (C4 excluding kept numbers) and commit**

```bash
rg -nP '(?<![A-Za-z0-9_!\[+&])#\d{2,4}\b' \
  ab-validator/crates/ab-aozora-pipeline/src/lexer/classify/directive.rs \
  | grep -vE '#(78|228|331|333|384|435)\b'
# Expected: no matches

nix build ./ab-validator#checks.x86_64-linux.cargo-check && \
nix build ./ab-validator#checks.x86_64-linux.cargo-clippy && \
git add ab-validator/crates/ab-aozora-pipeline/src/lexer/classify/directive.rs && \
git commit -m "chore(ab-aozora-pipeline): remove transient issue refs from directive.rs"
```

---

### Task 6: Rewrite `ab-validator/crates/ab-aozora-pipeline/src/lexer/sanitize.rs`

**Files:**
- Modify: `ab-validator/crates/ab-aozora-pipeline/src/lexer/sanitize.rs`

**Categories:** B (Task labels), K (transient Phase refs)
~10 occurrences of "Task 13" and "Phase 3 span semantics" references.

- [ ] **Step 1: Rewrite Task and Phase labels**

All "Task 13 offset-map groundwork" references (lines 89, 183, 254, 440, 584, 679, 1251). These are woven into doc comments — the phrase "Task 13" adds nothing. Delete "Task 13" from each:

| Pattern | Rewrite |
|---------|---------|
| `(Task 13 offset-map groundwork for Phase 3 span semantics;` | `(offset-map groundwork for span semantics;` |
| `(Task 13 groundwork for Phase 3 span` | `(groundwork for span` |
| `(Task 13 offset-map groundwork).` | `(offset-map groundwork).` |
| `(Task 13 offset-map` | `(offset-map` |

Also fix "Phase 3 span semantics" → "span semantics" where it's a project-phase label (not an algorithm description).

- [ ] **Step 2: Verify**

```bash
rg -n -i '\b(task\s*\d+|plan\s+[a-g]\.?\d|plan\s+amendment)\b' \
  ab-validator/crates/ab-aozora-pipeline/src/lexer/sanitize.rs
# Expected: no matches

nix build ./ab-validator#checks.x86_64-linux.cargo-check
nix build ./ab-validator#checks.x86_64-linux.cargo-clippy
```

- [ ] **Step 3: Commit**

```bash
git add ab-validator/crates/ab-aozora-pipeline/src/lexer/sanitize.rs
git commit -m "chore(ab-aozora-pipeline): remove Task 13 and Phase 3 labels from sanitize.rs"
```

---

### Task 7: Rewrite remaining Batch 2 files

**Files:**
- Modify: `ab-validator/crates/ab-aozora-encoding/src/gaiji.rs` (D: #122, #181, #189, #326)
- Modify: `ab-validator/crates/ab-aozora-pipeline/src/pipeline.rs` (D: #180, #202)
- Modify: `ab-validator/crates/ab-aozora-facade/src/splice.rs` (D: #202)
- Modify: `ab-validator/crates/ab-aozora-encoding/build.rs` (D: #89, #122, #326)
- Modify: `ab-validator/crates/ab-aozora-render/src/spelling/source.rs` (D: #202)
- Modify: `ab-validator/crates/ab-morph-run/src/hydrate/mod.rs` (B: Task labels)

- [ ] **Step 1: gaiji.rs — rewrite #122, #181, #189, #326**

```bash
# Find all transient #NNN in gaiji.rs
rg -nP '(?<![A-Za-z0-9_!\[+&])#(122|181|189|326)\b' \
  ab-validator/crates/ab-aozora-encoding/src/gaiji.rs
```

Delete each `(#NNN)` tag. Keep adjacent prose. Where `#122` means "standalone external-character form (no `※`)", the prose already says that — just delete the tag.

- [ ] **Step 2: pipeline.rs — rewrite #180, #202**

Lines 267, 336, 339, 363, 377, 383, 398. Check with grep:

```bash
rg -nP '(?<![A-Za-z0-9_!\[+&])#(180|202)\b' \
  ab-validator/crates/ab-aozora-pipeline/src/pipeline.rs
```

Line 363: `// (issue #180, unbounded growth).` → `// (unbounded growth).`
Line 336: `// provenance (#202).` (check context) → `// provenance.`

- [ ] **Step 3: splice.rs — rewrite #202 references (lines 3, 326)**

Line 3: `//! The **minimal-diff edit splice** (issue #202) — the last pillar` → `//! The **minimal-diff edit splice** — the last pillar`

- [ ] **Step 4: build.rs — rewrite #89, #122, #326**

```bash
rg -nP '(?<![A-Za-z0-9_!\[+&])#(89|122|326)\b' \
  ab-validator/crates/ab-aozora-encoding/build.rs
```

Delete `(#NNN)` tags from each line. Keep `#89`'s descriptive text ("the reverse char → 水準 classifier").

- [ ] **Step 5: source.rs — rewrite #202**

Line 4, 25, 45: delete `(#202)` tags.

- [ ] **Step 6: hydrate/mod.rs — rewrite Task labels**

```bash
rg -n -i '\btask\s*\d+\b' ab-validator/crates/ab-morph-run/src/hydrate/mod.rs
```

Lines 415, 548–553: delete `Task 5's`, `Task 3's`, `Task 7's`, `Task 8's` prefixes.

- [ ] **Step 7: Verify all files and commit**

```bash
# Verify each file individually with C4 (excluding kept numbers)
for f in \
  ab-validator/crates/ab-aozora-encoding/src/gaiji.rs \
  ab-validator/crates/ab-aozora-pipeline/src/pipeline.rs \
  ab-validator/crates/ab-aozora-facade/src/splice.rs \
  ab-validator/crates/ab-aozora-encoding/build.rs \
  ab-validator/crates/ab-aozora-render/src/spelling/source.rs \
  ab-validator/crates/ab-morph-run/src/hydrate/mod.rs; do
  echo "=== $f ==="
  rg -nP '(?<![A-Za-z0-9_!\[+&])#\d{2,4}\b' "$f" | grep -vE '#(78|228|331|333|384|435)\b' || true
done
# Expected: no output for each file

nix build ./ab-validator#checks.x86_64-linux.cargo-check
nix build ./ab-validator#checks.x86_64-linux.cargo-clippy

git add \
  ab-validator/crates/ab-aozora-encoding/src/gaiji.rs \
  ab-validator/crates/ab-aozora-pipeline/src/pipeline.rs \
  ab-validator/crates/ab-aozora-facade/src/splice.rs \
  ab-validator/crates/ab-aozora-encoding/build.rs \
  ab-validator/crates/ab-aozora-render/src/spelling/source.rs \
  ab-validator/crates/ab-morph-run/src/hydrate/mod.rs
git commit -m "chore: remove transient issue refs and task labels from Batch 2 files"
```

---

### Task 8: Rewrite remaining Batch 3 — Rust render/syntax/spec crates

**Files:**
- `ab-validator/crates/ab-aozora-render/src/classes.rs` (D: #202)
- `ab-validator/crates/ab-aozora-render/src/serialize.rs` (D: #202)
- `ab-validator/crates/ab-aozora-render/src/html.rs` (D: #202)
- `ab-validator/crates/ab-aozora-render/tests/snapshot_html_golden.rs` (D: #202)
- `ab-validator/crates/ab-aozora-syntax/src/accent.rs` (D: none — Track 1 only)
- `ab-validator/crates/ab-aozora-syntax/src/degraded.rs` (D: none — Track 1 only)
- `ab-validator/crates/ab-aozora-syntax/src/ast/output.rs` (D: #237)
- `ab-validator/crates/ab-aozora-syntax/src/ast/mod.rs` (D: #237)
- `ab-validator/crates/ab-aozora-syntax/src/ast/intern.rs` (D: #237)
- `ab-validator/crates/ab-aozora-syntax/src/alloc.rs` (D: none — Track 1 only)
- `ab-validator/crates/ab-aozora-spec/src/slugs.rs` (D: #114)
- `ab-validator/crates/ab-aozora-spec/src/span.rs` (D: #237)
- `ab-validator/crates/ab-aozora-spec/src/diagnostic.rs` (D: #237)
- `ab-validator/crates/ab-aozora-pipeline/src/lexer/classify/gaiji.rs` (D: none — Track 2 refs already done)
- `ab-validator/crates/ab-aozora-pipeline/src/lexer/offset.rs` (D: #90)
- `ab-validator/crates/ab-aozora-pipeline/src/lexer/mod.rs` (K: transient Phase)
- `ab-validator/crates/ab-aozora-pipeline/src/pipeline.rs` (J: transient Tier, K: Phase)

- [ ] **Step 1: Rewrite #237 references in syntax/spec crates**

```bash
# Find all #237 refs in these files
rg -nP '(?<![A-Za-z0-9_!\[+&])#237\b' \
  ab-validator/crates/ab-aozora-syntax/src/ast/ \
  ab-validator/crates/ab-aozora-spec/src/span.rs \
  ab-validator/crates/ab-aozora-spec/src/diagnostic.rs
```

Delete `#237` from each. Where it says "the #237 incremental cache / LSP consumer", rewrite to "the incremental cache / LSP consumer". Where it says "the #237 LSP consumer", rewrite to "the LSP consumer".

- [ ] **Step 2: Rewrite #202 references in render crate**

```bash
rg -nP '(?<![A-Za-z0-9_!\[+&])#202\b' \
  ab-validator/crates/ab-aozora-render/src/ \
  ab-validator/crates/ab-aozora-render/tests/
```

Delete `(#202)` tags. Where it says "the #202 splice model", rewrite to "the minimal-diff splice model".

- [ ] **Step 3: Rewrite one-off refs (#114, #90)**

| File | Line | Old | New |
|------|------|-----|-----|
| `slugs.rs` | 767 | `(#114)` | Delete tag |
| `offset.rs` | 1 | `— issue #90.` | `— byte-offset mapping.` |

- [ ] **Step 4: Rewrite transient Phase refs in lexer/mod.rs, pipeline.rs**

```bash
rg -n -i '\b(post-phase|phase[- ][a-f])\b' \
  ab-validator/crates/ab-aozora-pipeline/src/lexer/mod.rs \
  ab-validator/crates/ab-aozora-pipeline/src/pipeline.rs
```

Line 59 in mod.rs: `post-Phase-F` → delete prefix.
Line 346 in pipeline.rs: `Phase 0: resolve forward heading hints` — this is a local algorithm stage, KEEP.

- [ ] **Step 5: Verify and commit**

```bash
rg -nP '(?<![A-Za-z0-9_!\[+&])#\d{2,4}\b' \
  ab-validator/crates/ab-aozora-render/src/ \
  ab-validator/crates/ab-aozora-render/tests/ \
  ab-validator/crates/ab-aozora-syntax/src/ \
  ab-validator/crates/ab-aozora-spec/src/ \
  ab-validator/crates/ab-aozora-pipeline/src/lexer/ \
  | grep -vE '#(78|228|331|333|384|435)\b'
# Expected: no matches

nix build ./ab-validator#checks.x86_64-linux.cargo-check
nix build ./ab-validator#checks.x86_64-linux.cargo-clippy

git add ab-validator/crates/ab-aozora-render/ ab-validator/crates/ab-aozora-syntax/ \
  ab-validator/crates/ab-aozora-spec/ ab-validator/crates/ab-aozora-pipeline/src/lexer/
git commit -m "chore: remove transient issue refs and phase labels from render/syntax/spec crates"
```

---

### Task 9: Rewrite remaining Batch 3 — Rust facade/encoding/pipeline/tests

**Files:**
- `ab-validator/crates/ab-aozora-facade/src/lib.rs` (D: #202, #237, I: UNSTABLE)
- `ab-validator/crates/ab-aozora-facade/src/json.rs` (D: #435 Track 1 — KEEP)
- `ab-validator/crates/ab-aozora-facade/src/document.rs` (D: #237)
- `ab-validator/crates/ab-aozora-facade/tests/corpus_incremental_merge.rs` (D: #237, J: Tier)
- `ab-validator/crates/ab-aozora-facade/tests/splice_api.rs` (D: #202)
- `ab-validator/crates/ab-aozora-facade/tests/corpus_splice_tiling.rs` (D: #202)
- `ab-validator/crates/ab-aozora-facade/tests/lint_catalogue.rs` (D: none — Track 1 only)
- `ab-validator/crates/ab-aozora-encoding/src/suijun.rs` (D: #89)
- `ab-validator/crates/ab-aozora-encoding/src/lib.rs` (D: #89)
- `ab-validator/crates/ab-aozora-pipeline/tests/` (B, J, K — 5 files)

- [ ] **Step 1: lib.rs — rewrite #202, #237, UNSTABLE**

Line 111: `Source-region ownership and minimal-diff source splicing (#202).` → `Source-region ownership and minimal-diff source splicing.`

Lines 116 and 151: remove the `UNSTABLE` markers. The crate-level docs already state the pre-1.0 posture. Rewrite so the doc comments are valid sentences:

| Line | Old | New |
|------|-----|-----|
| 116 | `/// **UNSTABLE — not subject to semver until v0.5.0.**` | (delete line) |
| 119 | `(#237 Tier 1/2). Splices` → `Splices` |
| 151 | `/// **UNSTABLE — not subject to semver until v0.5.0.**` | (delete line) |

- [ ] **Step 2: json.rs — verify #435 is Track 1 (keep)**

```bash
rg -nP '(?<![A-Za-z0-9_!\[+&])#435\b' \
  ab-validator/crates/ab-aozora-facade/src/json.rs
```
Line 45: `Schema 2 (#435):` — #435 is a Track 1 invariant, KEEP. No edit needed.

- [ ] **Step 3: document.rs — rewrite #237**

Line 228: `the #237 incremental-reparse LSP consumer` → `the incremental-reparse LSP consumer`

- [ ] **Step 4: Test files — rewrite #237, #202, Tier**

corpus_incremental_merge.rs line 1: `(#237)` → delete tag
corpus_incremental_merge.rs line 22: `#237 Tier 1:` → delete prefix
splice_api.rs line 1: `(#202),` → delete tag
corpus_splice_tiling.rs line 2: `(#202)` → delete tag

- [ ] **Step 5: encoding — rewrite #89**

suijun.rs line 1: `— issue #89.` → delete suffix
lib.rs line 161: `(issue #89),` → delete parenthetical

- [ ] **Step 6: Pipeline tests — rewrite task/phase/tier labels**

```bash
rg -n -i '\b(task\s*\d+|plan\s+[a-g]\.?\d|plan\s+amendment)\b' \
  ab-validator/crates/ab-aozora-pipeline/tests/
```

Files: streaming_semantics.rs (line 5: "Plan G.4 deliverable"), diagnostic_ordering.rs (line 3: "Plan G.4 deliverable"), deep_nesting.rs (line 3: "Plan G.4 deliverable"), slug_canonical_round_trip.rs (line 1: "Phase 1.3"), authoring_diagnostics.rs, property_annotation_unknown.rs.

Delete all project-phase and plan labels. Keep "Tier-A canary" references.

- [ ] **Step 7: Verify and commit**

```bash
rg -nP '(?<![A-Za-z0-9_!\[+&])#\d{2,4}\b' \
  ab-validator/crates/ab-aozora-facade/src/ \
  ab-validator/crates/ab-aozora-facade/tests/ \
  ab-validator/crates/ab-aozora-encoding/src/ \
  | grep -vE '#(78|228|331|333|384|435)\b'
# Expected: no matches

rg -n -i '\b(task\s*\d+|plan\s+[a-g]\.?\d|plan\s+amendment)\b' \
  ab-validator/crates/ab-aozora-pipeline/tests/
# Expected: no matches

nix build ./ab-validator#checks.x86_64-linux.cargo-check
nix build ./ab-validator#checks.x86_64-linux.cargo-clippy

git add ab-validator/crates/ab-aozora-facade/ ab-validator/crates/ab-aozora-encoding/ \
  ab-validator/crates/ab-aozora-pipeline/tests/
git commit -m "chore: remove transient refs from facade/encoding crates and pipeline tests"
```

---

### Task 10: Rewrite remaining Batch 3 — Rust morph/ortho/coverage/corpus/proptest/aat crates

**Files:**
- `ab-validator/crates/ab-morph-run/src/` (B: Task labels, D: #89/#326/#181 Track 2, G: TODO, H: speculative)
- `ab-validator/crates/ab-ortho-detect/` (E: Issue 2, F: Spec Decision #10, H: speculative)
- `ab-validator/crates/ab-ortho-detect-ml/` (H: speculative)
- `ab-validator/crates/ab-morph-analyzers/` (B: Task, G: TODO, K: Phase)
- `ab-validator/crates/ab-coverage/` (B: Task)
- `ab-validator/crates/ab-aozora-corpus/` (J: Tier)
- `ab-validator/crates/ab-aozora-proptest/` (J: Tier)
- `ab-validator/crates/ab-aat-to-parser-ir/` (A: handoff, H: speculative)
- `ab-validator/crates/ab-ir/` (K: transient Phase — `src/lib.rs` line 384 `# Phase 2 status`)
- `ab-validator/crates/ab-warehouse/` (C: dated report)
- `ab-validator/crates/ab-check/` (A: handoff)
- `ab-validator/crates/ab-aozora/` (C: dated spec)
- `ab-validator/adapters/aozora-epub3/` (A: handoff)

- [ ] **Step 1: morph-run — Task labels, issue refs, TODO, speculative language**

```bash
# Find all transient refs
rg -n -i '\b(task\s*\d+|plan\s+[a-g]\.?\d)\b' \
  ab-validator/crates/ab-morph-run/src/
```

Delete all Task/Plan labels from comments. Key files: `lib.rs` (lines 569, 2884, 3246), `main.rs` (lines 295, 296, 369, 392, 394), `options.rs` (line 19), `calibration/mod.rs` (line 1, 2, 24), `calibration/label_score.rs` (line 1, 2, 58), `calibration/label_export.rs` (line 1), `auto_jobs.rs` (line 22), `import_aozora.rs` (line 3), `summary/interesting.rs` (line 8), `oracle/reading_norm.rs` (line 320), `oracle/ruby.rs`, `hydrate/metadata.rs`, `hydrate/tables.rs`, `hydrate/source_context.rs`, `hydrate/render.rs`.

TODO in pipeline.rs:1917: change to `// --ortho-detect is not yet threaded through the parallel/warehouse/selected paths.`

- [ ] **Step 2: ortho-detect — Issue 2, Spec Decision #10, Phase labels**

Files: `policy.rs`, `historical.rs`, `types.rs`, `features.rs`, `ml.rs`

policy.rs line 1: `(Issue 2, F3 / P1)` → delete parenthetical
historical.rs line 1: `(Issue 2, Phase-3 Lane B / M2)` → `Historical-kana → modern-kana surface modernizer.` (delete parenthetical and transient Phase label)
historical.rs line 221: `The Phase-3 Lane B` → `The` (delete Phase label)
types.rs line 54: `The Phase-3 Lane B` → `The` (delete Phase label)
features.rs line 181: `(spec Decision #10 partitions` → `(partitions`
ml.rs line 23: `Per spec Decision #10.` → delete sentence

- [ ] **Step 3: ortho-detect-ml — speculative language**

main.rs line 29: `ready for re-evaluation.` → delete sentence
main.rs line 118: `revisit has a baseline to beat.` → delete sentence

- [ ] **Step 4: morph-analyzers — TODO, Phase, Task**

ortho_compat.rs line 10: `/// # Failure mode (TODO: make \`OrthoTokenizer::tokenize\` return \`Result\`)` → `/// # Failure mode: \`tokenize()\` panics on invalid input; a future signature may return \`Result\`.`
span_builder.rs line 142: `Phase 2 spec invariant #2` → `spec invariant #2`
historical_oracle.rs line 1: `Phase-3 Lane B (M2)` → delete prefix
remap_vu.rs line 1: `Task 7.5` → delete
ortho_reproducibility_golden.rs line 1: `Issue 2 P5` → delete

- [ ] **Step 5: coverage — Task labels**

schema.rs line 9: `(used during Task 3 ramp-up` → delete parenthetical
merge.rs line 5: `Used by Task 3.` → delete sentence
merge.rs line 9: `Task 4 Step 5.` → delete sentence

- [ ] **Step 6: aozora-corpus — Tier**

vendored.rs line 9: `Tier A fixtures` → rewrite (this is a local concept, not tied to #237)
vendored.rs line 29: `Tier-A-candidate` → keep (this is the invariant form)

- [ ] **Step 7: aozora-proptest — Tier H**

generators.rs lines 17, 96, 120: `Tier H` → `setext-vs-decorative-rule` or just delete the label

- [ ] **Step 8: aat-to-parser-ir — handoff, speculative**

canonical_json.rs line 19: handoff reference → rewrite as inline rationale (already in the prose)
structural_probe.rs line 733: `eventually export durable source aliases` → `// Source aliases are not yet exported; relying on this temporary mapping.`

- [ ] **Step 8a: ab-ir — transient Phase reference**

`ab-validator/crates/ab-ir/src/lib.rs` line 384: `/// # Phase 2 status — RESERVED, no producer yet` → `/// # Status — RESERVED, no producer yet`

- [ ] **Step 9: warehouse, check, aozora, epub3 — handoffs and dated refs**

warehouse/src/writer.rs line 32: dated report → delete reference
check/src/encoding.rs line 5: handoff → rewrite as inline
aozora/src/main.rs line 2: dated spec → delete reference
aozora-epub3/src/source_derived.rs line 11: handoff → rewrite as inline

- [ ] **Step 10: Verify all files and commit**

```bash
# Full C4 across all ab-validator
rg -nP --type-add 'src:*.rs' -t src \
  '(?<![A-Za-z0-9_!\[+&])#\d{2,4}\b' ab-validator/ \
  | grep -v third_party/ | grep -vE '#(78|228|331|333|384|435)\b'
# Expected: no matches

# C2: zero Task/Plan labels
rg -n --type-add 'src:*.rs' -t src \
  -i '\b(task\s*\d+|plan\s+[a-g]\.?\d|plan\s+amendment|plan\s+blocker)\b' ab-validator/
# Expected: no matches

# C5: zero Issue 2 / Spec Decision
rg -n --type-add 'src:*.rs' -t src \
  -i '\b(issue\s+2|spec\s+decision\s*#?\d+)\b' ab-validator/
# Expected: no matches

nix build ./ab-validator#checks.x86_64-linux.cargo-check
nix build ./ab-validator#checks.x86_64-linux.cargo-clippy
nix build ./ab-validator#checks.x86_64-linux.cargo-fmt

git add ab-validator/
git commit -m "chore: remove transient refs from remaining Rust crates (morph/ortho/coverage/corpus/proptest/aat/warehouse/check/adapters)"
```

---

### Task 11: Rewrite Clojure source files

**Files:**
- `abc/src/abc/tools/facts.clj` (B: Task 7)
- `abc/src/abc/tools/aozora_ingest.clj` (B: Task/Plan)
- `abc/src/abc/tools/analysis_identity.clj` (E: Issue 2)
- `abc/src/abc/tools/materialize_import.clj` (G: TODO)
- `abc/src/abc/annotation/schema.clj` (H: speculative)
- `abc/src/abc/tools/diagram/` (J: Tier — 3 files)

- [ ] **Step 1: facts.clj — remove Task 7 labels (lines 27, 50, 86)**

| Line | Old | New |
|------|-----|-----|
| 27 | `;; gate (Task 7) honest:` | `;; gate honest:` |
| 50 | `;; person_record/1 and drift_successor/2 (Task 7 queries both).` | `;; person_record/1 and drift_successor/2.` |
| 86 | `;; the referential-integrity query (Task 7) resolves against.` | `;; the referential-integrity query resolves against.` |

Also line 42: `(plan Blocker remediation).` → `(deterministic ordering).`

- [ ] **Step 2: aozora_ingest.clj — remove plan label**

Line 306: `;; corpus-level shared-person reconciliation: plans arrive sorted by` → `;; corpus-level shared-person reconciliation: records arrive sorted by`

- [ ] **Step 3: analysis_identity.clj — remove Issue 2**

Line 14: `;; (ab_ortho_detect::NormalizationPolicy::identity, spec Issue 2 P1) and used` → `;; (ab_ortho_detect::NormalizationPolicy::identity) and used`

- [ ] **Step 4: materialize_import.clj — convert TODO to limitation**

Line 14: `;; v0 simplification: reads entire file. TODO: stream to handle` → `;; Reads entire file into memory; large run summaries may OOM.`

- [ ] **Step 5: annotation/schema.clj — rewrite speculative language**

Line 5: `;; Thoughts: msd for all morphosyntactic information, lemma for lemma, pos for pos. join perhaps for non-independent morphemes.` → `;; Morphosyntactic fields: msd, lemma, pos, join (for non-independent morphemes).`

- [ ] **Step 6: diagram/ — rewrite transient Tier references**

adr_graph.clj line 2: `"Tier 1 pure builder:` → `"Pure builder:`
architecture_graph.clj line 2: `"Tier 2 pure builder:` → `"Pure builder:`
workflow_graph.clj line 2: `"Tier 3: render` → `"Render`

These "Tier N" labels are layering tiers for the diagram module, not project-phase naming. They are unnecessary labels — the docstring already describes what each does.

- [ ] **Step 7: Verify and commit**

```bash
# C2: zero Task/Plan in Clojure
rg -n --type-add 'src:*.{clj,cljc,cljs}' -t src \
  -i '\b(task\s*\d+|plan\s+[a-g]\.?\d|plan\s+amendment|plan\s+blocker)\b' abc/src/
# Expected: no matches

# C5: zero Issue 2
rg -n --type-add 'src:*.{clj,cljc,cljs}' -t src \
  -i '\bissue\s+2\b' abc/src/
# Expected: no matches

# C6: zero TODOs
rg -n --type-add 'src:*.{clj,cljc,cljs}' -t src \
  '\bTODO\b' abc/src/
# Expected: no matches

nix build ./abc#checks.x86_64-linux.clj-kondo

git add abc/src/
git commit -m "chore(abc): remove transient refs from Clojure source (Task 7, Issue 2, TODO, speculative, Tier labels)"
```

---

### Task 12: Full verification and final cleanup

- [ ] **Step 1: Run the complete verification recipe**

```bash
# C1: zero docs/handoffs/
rg -n --type-add 'src:*.{rs,clj,cljc,cljs}' -t src \
  'docs/handoffs/' ab-validator/ abc/src/
# Expected: exit 1 (no matches)

# C2: zero Task/Plan tracking labels
rg -n --type-add 'src:*.{rs,clj,cljc,cljs}' -t src \
  -i '\b(task\s*\d+|plan\s+[a-g]\.?\d|plan\s+amendment|plan\s+blocker)\b' \
  ab-validator/ abc/src/
# Expected: exit 1

# C3: zero dated spec/report references
rg -n --type-add 'src:*.{rs,clj,cljc,cljs}' -t src \
  'docs/superpowers/(specs|reports)/' ab-validator/ abc/src/
# Expected: exit 1

# C4: zero transient internal issue tracker numbers
rg -nP --type-add 'src:*.{rs,clj,cljc,cljs}' -t src \
  '(?<![A-Za-z0-9_!\[+&])#\d{2,4}\b' ab-validator/ abc/src/ \
  | grep -v third_party/ | grep -vE '#(78|228|331|333|384|435)\b'
# Expected: no output

# C5: zero "Issue 2" or "Spec Decision #N"
rg -n --type-add 'src:*.{rs,clj,cljc,cljs}' -t src \
  -i '\b(issue\s+2|spec\s+decision\s*#?\d+)\b' \
  ab-validator/ abc/src/
# Expected: exit 1

# C6: zero TODOs
rg -n --type-add 'src:*.{rs,clj,cljc,cljs}' -t src \
  '\bTODO\b' ab-validator/ abc/src/
# Expected: exit 1

# C7: zero speculative/forward-looking language
rg -n --type-add 'src:*.{rs,clj,cljc,cljs}' -t src \
  -i '\b(revisit|re-evaluat|eventually export|a later task|someday)\b' \
  ab-validator/ abc/src/
# Expected: exit 1

# C8: zero UNSTABLE markers
rg -n --type-add 'src:*.{rs,clj,cljc,cljs}' -t src \
  'UNSTABLE.*(semver|v0\.\d|not subject to)' ab-validator/ abc/src/
# Expected: exit 1

# C9: zero transient Tier refs
rg -n --type-add 'src:*.{rs,clj,cljc,cljs}' -t src \
  '\bTier[\s-][B-H1-9]\b' ab-validator/ abc/src/
# Expected: exit 1

# C10: zero transient Phase refs
# Matches project-management Phase labels (Phase-C, Phase-3 Lane B, Phase 1.2,
# post-Phase-F, pre-Phase) while excluding algorithm-stage markers of the form
# "// --- Phase N: description ---", path-excluding sentences.rs (all Phase
# refs are algorithm-stage), and path-excluding ortho-detect-ml/src/main.rs
# (string-literal markdown report output).
rg -n --type-add 'src:*.{rs,clj,cljc,cljs}' -t src \
  -i '\b(phase[- ][a-f]|phase[- ]lane|phase\s+\d\.\d|post-phase|pre-phase)\b' \
  ab-validator/ abc/src/ \
  | grep -v 'sentences\.rs' \
  | grep -v 'ortho-detect-ml/src/main\.rs'
# Expected: no matches

# C10-keep: verify algorithm-stage markers are still present
rg -n --type-add 'src:*.{rs,clj,cljc,cljs}' -t src \
  '// --- Phase [0-9]:' ab-validator/crates/ab-aat-to-parser-ir/src/sentences.rs
# Expected: matches (Phase 0 through Phase 5 section markers).

# C11: 6 stable invariant names still present
for n in 78 228 331 333 384 435; do
  count=$(rg -cP --type-add 'src:*.{rs,clj,cljc,cljs}' -t src \
    "(?<![A-Za-z0-9_!\[+&])#${n}\b" ab-validator/ abc/src/ \
    | awk -F: '{s+=$2} END{print s}')
  echo "#${n}: ${count:-0} occurrences"
done
# Expected: each shows >0 occurrences
```

- [ ] **Step 2: Run full Nix checks**

```bash
just validate-migration
nix build ./abc#checks.x86_64-linux.clj-kondo
nix build ./ab-validator#checks.x86_64-linux.cargo-check
nix build ./ab-validator#checks.x86_64-linux.cargo-clippy
nix build ./ab-validator#checks.x86_64-linux.cargo-fmt
# All expected: success
```

- [ ] **Step 3: Verify ADR references intact**

```bash
# Count ADR references before (from git) and after
git stash
before=$(rg -c --type-add 'src:*.{rs,clj,cljc,cljs}' -t src 'ADR\s*\d{4}' ab-validator/ abc/src/ | awk -F: '{s+=$2} END{print s}')
git stash pop
after=$(rg -c --type-add 'src:*.{rs,clj,cljc,cljs}' -t src 'ADR\s*\d{4}' ab-validator/ abc/src/ | awk -F: '{s+=$2} END{print s}')
echo "ADR refs before: $before, after: $after"
# Expected: after >= before
```

- [ ] **Step 4: Commit verification baseline**

```bash
git commit --allow-empty -m "chore: verification — all cleanup criteria pass"
```

---

### Task 13: Delete external docs directories (Batch 4)

**Files:**
- Delete: `abc/docs/handoffs/`
- Delete: `docs/superpowers/specs/` (except this design doc — keep until ADR extraction from Note on Location)
- Delete: `docs/superpowers/reports/`

- [ ] **Step 1: Delete handoffs directory**

```bash
git rm -r abc/docs/handoffs/
```

- [ ] **Step 2: Delete superpowers specs and reports**

```bash
# Delete all specs and reports; the design doc and plan are transient and
# are deleted here with the rest.
git rm -r docs/superpowers/specs/ 2>/dev/null || true
git rm -r docs/superpowers/plans/ 2>/dev/null || true
git rm -r docs/superpowers/reports/ 2>/dev/null || true
```

- [ ] **Step 3: Verify no code still references deleted docs**

```bash
# C1: zero docs/handoffs/
rg -n --type-add 'src:*.{rs,clj,cljc,cljs}' -t src \
  'docs/handoffs/' ab-validator/ abc/src/
# Expected: exit 1

# C3: zero dated spec/report references
rg -n --type-add 'src:*.{rs,clj,cljc,cljs}' -t src \
  'docs/superpowers/(specs|reports)/' ab-validator/ abc/src/
# Expected: exit 1
```

- [ ] **Step 4: Verify full checks still pass**

```bash
just validate-migration
nix build ./abc#checks.x86_64-linux.clj-kondo
nix build ./ab-validator#checks.x86_64-linux.cargo-check
```

- [ ] **Step 5: Commit**

```bash
git commit -m "chore: delete now-unreferenced transient doc directories"
```

---

### Self-Review Checklist

After completing all tasks, verify:

1. **Spec coverage:** Each category A–K has at least one task that implements it
2. **Placeholder scan:** No TBD, TODO, or incomplete steps remain
3. **Type consistency:** File paths match between tasks and the spec's batch tables
4. **C11 passes:** The 6 stable invariant names are still present at expected counts
5. **Validations pass:** `just validate-migration` and all per-language checks succeed
