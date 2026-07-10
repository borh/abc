# Presentation Validation Review Fixes Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Close the final deterministic-rendering and canonical-backing validation gaps in the academic presentation diagrams.

**Architecture:** Keep validation at the existing source boundaries: locale-independent number serialization in `presentation-svg`, relation-shape validation in `presentation-figures`, and live ADR membership in `presentation-model`. Preserve the public graph shapes and generated-artifact workflow.

**Tech Stack:** Clojure 1.12, `clojure.test`/Kaocha, `clojure.data.xml`, Nix, Graphviz.

## Global Constraints

- Use strict RED/GREEN TDD for every behavior change.
- Accept only canonical backing relation keys already used by the two real figures: `:coordinates`, `:stages`, `:path`, and path-only `:reachable-targets`; optional `:adrs` must be live and canonical.
- Do not hand-edit generated DOT or SVG artifacts; regenerate only through the presentation generator and only retain changed bytes.
- Do not merge or push.

---

### Task 1: Locale-independent SVG numbers

**Files:**
- Modify: `abc/test/abc/tools/diagram/presentation_svg_test.clj`
- Modify: `abc/src/abc/tools/diagram/presentation_svg.clj`

**Interfaces:**
- Consumes: `normalize-svg [graph raw-svg woff2-bytes]`.
- Produces: identical valid SVG bytes under German and root JVM format locales.

- [x] Add a test that temporarily sets `Locale/GERMANY`, compares normalized bytes with `Locale/ROOT`, and parses/validates the German result.
- [x] Run the focused test and verify RED from decimal commas in numeric SVG attributes.
- [x] Route every formatted SVG numeric attribute through root-locale formatting.
- [x] Re-run the focused test and verify GREEN.

### Task 2: Canonical non-empty item backing

**Files:**
- Modify: `abc/test/abc/tools/diagram/presentation_figures_test.clj`
- Modify: `abc/src/abc/tools/diagram/presentation_figures.clj`

**Interfaces:**
- Consumes: graph items whose canonical relation is one of non-empty `:coordinates`, `:stages`, or `:path`; `:reachable-targets` is valid only with `:path`; `:adrs` is optional evidence metadata.
- Produces: `validate-graph! [context graph] -> graph`, throwing actionable `ExceptionInfo` for empty, unknown, malformed, or edge-inappropriate backing.

- [x] Add table-driven failing tests for `{}`, unknown keys, empty recognized relations, malformed values, edge `:stages` backing, node `:path` backing, and malformed path target metadata; assert both canonical graphs still pass.
- [x] Run focused tests and verify each case is RED for the intended missing validation.
- [x] Add relation-key, value-shape, and node/edge-specific validation while retaining existing referential, reachability, and citation checks.
- [x] Re-run focused tests and verify GREEN.

### Task 3: Live ADR citation membership

**Files:**
- Modify: `abc/test/abc/tools/diagram/presentation_model_test.clj`
- Modify: `abc/src/abc/tools/diagram/presentation_model.clj`

**Interfaces:**
- Consumes: canonical context `:adr-nums` from parsed live ADR files.
- Produces: `backing-problems` and `problems` reject an otherwise stage-owned citation when it is absent from `:adr-nums`.

- [x] Add a failing test with a staged ADR `9999` that is stage-owned but absent from the context live set.
- [x] Run the focused test and verify RED.
- [x] Add live-set membership to citation validation without coupling to architecture-validator message formatting.
- [x] Re-run the focused test and verify GREEN.

### Task 4: Exact DOT terminator

**Files:**
- Modify: `abc/test/abc/tools/diagram/graphviz_test.clj`

**Interfaces:**
- Consumes: `dot [graph] -> string`.
- Produces: regression proof that DOT ends with exactly one LF.

- [x] Replace the permissive trailing-newline assertion with an exact one-LF assertion.
- [x] Run the focused graphviz test and verify GREEN because production already satisfies the stronger contract.

### Task 5: Verification, artifacts, and commit

**Files:**
- Modify only if generator bytes differ: `abc/docs/figures/*.{dot,svg}`

**Interfaces:**
- Consumes: all preceding changes.
- Produces: one verified commit `fix(diagram): close presentation validation gaps`.

- [x] Format changed Clojure files and run focused diagram tests.
- [x] Run full Kaocha, presentation generate/check, ADR and Mermaid checks, relevant Nix drift/clj-kondo checks, and inspect the diff.
- [x] Compare generated artifact hashes before/after; retain regeneration only when bytes changed.
- [x] Commit all scoped files once, then report RED/GREEN evidence, commit hash, clean status, and concerns.

### Task 6: Path membership and node-role backing review

**Files:**
- Modify: `abc/test/abc/tools/diagram/presentation_figures_test.clj`
- Modify: `abc/src/abc/tools/diagram/presentation_figures.clj`

**Interfaces:**
- Consumes: path backing over canonical stages and node backing keyed by the displayed node role.
- Produces: rejection of unknown or consecutive-identical path stages before reachability and an explicit node-role relation policy matching the canonical figures.

- [x] Add failing structured-problem tests for unknown path stages, zero-hop path segments, output nodes with coordinate backing, and unknown node roles with stage backing.
- [x] Run the focused namespace and verify RED from the intended missing validation.
- [x] Add path membership/zero-hop gates and an explicit `node-relations` table derived from canonical figure roles.
- [x] Re-run the focused namespace and verify canonical paths/graphs and all regressions are GREEN.
- [x] Run full verification, confirm generated hashes remain unchanged, and commit `fix(diagram): validate presentation backing roles` without merging or pushing.
