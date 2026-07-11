# Annotation Join Overlap Lookup Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Replace annotation join's exhaustive token scan with a validated binary-search lookup while preserving every valid-input result.

**Architecture:** `join` materializes and validates the token vector once, then each non-degenerate annotation uses a lower-bound search on token end followed by a forward overlap scan. Tests retain an independent exhaustive oracle and use `test.check` to prove equivalence over generated valid spans; an operator benchmark uses real retained run artifacts.

**Tech Stack:** Clojure, `clojure.test`, `clojure.test.check`, Kaocha.

## Global Constraints

- Modify only ABC source, tests, benchmark support, and resulting documentation.
- Preserve the exact join output shape, classification strings, token ordering, and degenerate-span behavior.
- `join` must throw `ExceptionInfo` before producing results for malformed, descending, overlapping, or degenerate token spans.
- Do not modify Phase 3 parser files.

---

### Task 1: Characterize validation and optimized lookup

**Files:**
- Modify: `abc/test/abc/tools/annotation_join_test.clj`
- Modify: `abc/src/abc/tools/annotation_join.clj`

**Interfaces:**
- Consumes: tokens shaped as `{"input_span" {"start" integer "end" integer}}` and annotations shaped as `{"span" {"start" integer "end" integer}}`.
- Produces: unchanged public `join [tokens annotations]`; invalid tokens throw `ExceptionInfo` with `:token-index`, `:start`, `:end`, and `:prev-end`.

- [ ] **Step 1: Add failing invalid-token tests.** Add cases for a list input that succeeds after vector coercion and malformed, zero-width, overlapping, and descending token spans that throw `ExceptionInfo`.
- [ ] **Step 2: Run the focused tests and verify RED.** Run `cd abc && bin/kaocha --focus abc.tools.annotation-join-test`; expect invalid token cases not to throw.
- [ ] **Step 3: Implement vector coercion and one-pass validation.** Add a private `validated-token-vector` and call it once at the start of `join`; update the public docstring with the exception contract.
- [ ] **Step 4: Run the focused tests and verify GREEN.** Run the same Kaocha command; expect all examples to pass.
- [ ] **Step 5: Add a failing lookup boundary test.** Exercise empty tokens, annotations before/after tokens, touching boundaries, token gaps, and an annotation spanning all tokens through a test-visible lookup or full `join` results.
- [ ] **Step 6: Replace exhaustive `overlapping` with lower-bound plus forward scan.** Binary-search the first token end greater than `s`, then accumulate tokens while start is less than `e`; return a vector in source order.
- [ ] **Step 7: Run the focused tests and verify GREEN.** Run the focused Kaocha command.

### Task 2: Property equivalence against the exhaustive oracle

**Files:**
- Modify: `abc/test/abc/tools/annotation_join_test.clj`

**Interfaces:**
- Consumes: generated sorted, non-overlapping positive-width token spans and arbitrary annotation spans.
- Produces: a 300-case `test.check` property asserting complete `join` equality against a test-only exhaustive reference implementation.

- [ ] **Step 1: Add the independent exhaustive oracle and generators.** Build tokens from generated nonnegative gaps and positive widths; generate annotation start/end independently so inverted and zero-width cases occur.
- [ ] **Step 2: Add the equality property.** Compare the full vector of join rows, not only classifications or token indexes, and print the shrunk result on failure.
- [ ] **Step 3: Run the property test.** Run `cd abc && bin/kaocha --focus abc.tools.annotation-join-test`; expect 300 cases to pass.
- [ ] **Step 4: Run Clojure static checks.** Run `nix build .#checks.x86_64-linux.abc-clj-kondo` from the repository root; expect success.

### Task 3: Real-work benchmark and evidence

**Files:**
- Create: `abc/test/abc/tools/annotation_join_benchmark.clj`
- Create: `abc/docs/handoffs/2026-07-11-annotation-join-overlap-benchmark.md`

**Interfaces:**
- Consumes: pairs of `<parser-ir.json> <tokens.jsonl>` paths, rendering annotations with `abc.tools.parser-ir-plaintext` and validating tokens with `abc.tools.annotation-join-stats/token-spans`.
- Produces: a CLI that warms both lookup implementations, asserts identical covers, reports counts and repeated timings, and a checked-in handoff containing commands, identities, medians, and ratios without machine-local paths in active configuration.

- [ ] **Step 1: Implement the benchmark harness.** Keep the exhaustive implementation local to the benchmark; accept two or more path pairs, use the equality-checking exhaustive pass as the single baseline, and print five optimized measurements after warm-up.
- [ ] **Step 2: Select real works.** From the retained stride-44 run, select the largest token file and independently select the highest rendered annotation-to-token ratio among works with at least 10,000 tokens and 1,000 annotations; record the selection command.
- [ ] **Step 3: Run the benchmark.** Run with `clojure -M:test -m abc.tools.annotation-join-benchmark ...`; require cover equality and at least 10× median isolated-lookup speedup for each work.
- [ ] **Step 4: Record evidence.** Write the handoff with work stems, token/annotation counts, timing medians, ratios, and the retained run label; omit absolute `/db` paths from executable configuration.
- [ ] **Step 5: Run focused tests once more.** Run the annotation join tests and ensure they remain green.

### Task 4: Final verification

**Files:** none

- [ ] **Step 1: Run `nix build .#checks.x86_64-linux.abc-clj-kondo`.** Expect success.
- [ ] **Step 2: Run `just validate-migration`.** Expect success, or report any unrelated pre-existing failure with evidence.
- [ ] **Step 3: Inspect `git diff --check` and the final diff.** Confirm no Phase 3 parser file or unrelated user change is included.
