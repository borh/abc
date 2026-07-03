# AAT Compatibility Registry Hardening Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Record the owned AAT to parser-IR mapping decision and make ABC's compatibility registry self-validating before it is used as acceptance evidence.

**Architecture:** ABC owns the boundary contract: schemas, manifest identity dimensions, and adapter-scoped compatibility acceptance. ab-validator owns measured mapping documents and adapter-specific evidence. The registry must fail closed when its own structure drifts, and documentation must not turn measured aozora-rs evidence into adapter-neutral support.

**Tech Stack:** Clojure, EDN registry data, JSON Schema, ADR markdown, `clojure -M:test`, `nix run .#validate-design-bundle`, `nix flake check --print-build-logs`.

## Global Constraints

- Do not add hand-authored mapping rules for aozora2html.
- Do not add an aozora2html compatibility registry entry until ab-validator produces a producer-owned mapping document hash and policy for warigaki/kunten.
- Keep the existing aozora-rs registry entry adapter-scoped.
- Validate with a focused Clojure test run, design-bundle validation, and full Nix flake check before committing.

---

### Task 1: Add Registry Self-Validation

**Files:**
- Modify: `src/abc/tools/aat_parser_ir_compat.clj`
- Modify: `src/abc/tools/validate_design_bundle.clj`
- Modify: `test/abc/tools/validate_design_bundle_test.clj`

**Interfaces:**
- Consumes: `data/aat-parser-ir-compatibility.edn`
- Produces: `abc.tools.aat-parser-ir-compat/registry-errors` and `validate-registry!`

- [x] **Step 1: Write failing tests**

Add tests that assert registry validation accepts the current shape, rejects missing evidence, rejects wildcard adapter claims, rejects duplicate match keys, rejects malformed hashes, and verifies that `validate-json-schemas!` invokes registry validation.

- [x] **Step 2: Run focused tests to verify failure**

Run:

```bash
ABC_TEI_SCHEMA_SKIP=1 clojure -M:test -e "(require 'abc.tools.validate-design-bundle-test 'clojure.test) (clojure.test/run-tests 'abc.tools.validate-design-bundle-test)"
```

Expected: fails because registry validation functions are not implemented yet.

- [x] **Step 3: Implement minimal validation**

Implement EDN registry validation in `abc.tools.aat-parser-ir-compat` with required fields, hash format checks, non-wildcard adapter checks, evidence-scope checks, and duplicate detection over the compatibility match key.

- [x] **Step 4: Re-run focused tests**

Run the same focused test command. Expected: all tests in `abc.tools.validate-design-bundle-test` pass.

### Task 2: Record the Mapping Boundary ADR

**Files:**
- Create: `docs/adr/0023-owned-aat-parser-ir-mapping.md`
- Modify: `docs/handoffs/owned-mapping-design.md`

**Interfaces:**
- Consumes: the implemented ABC/ab-validator ownership split and current measured evidence.
- Produces: accepted ADR text that binds the current behavior and deferred vocabulary decisions.

- [x] **Step 1: Write ADR 0023**

Record Option C: ABC owns mapping schemas, manifest identity, and adapter-scoped registry acceptance; ab-validator owns mapping documents and adapter measurement. Include mapping document hash as identity-bearing, mapping schema hash as provenance-only, and release strictness as gate-bound rather than a CLI toggle.

- [x] **Step 2: Update the handoff**

Mark Task 9 as complete and add the sibling aozora2html measurement counts as decision inputs only: warigaki 243 works / 4,050 nodes and kunten 472 works / 22,504 observations.

### Task 3: Verify and Commit

**Files:**
- Validate all files touched by Tasks 1 and 2.

**Interfaces:**
- Consumes: Tasks 1 and 2.
- Produces: one coherent commit.

- [x] **Step 1: Run focused tests**

```bash
ABC_TEI_SCHEMA_SKIP=1 clojure -M:test -e "(require 'abc.tools.validate-design-bundle-test 'clojure.test) (clojure.test/run-tests 'abc.tools.validate-design-bundle-test)"
```

- [x] **Step 2: Run design-bundle gate**

```bash
nix run .#validate-design-bundle
```

- [x] **Step 3: Run full flake check**

```bash
nix flake check --print-build-logs
```

- [ ] **Step 4: Commit**

```bash
git add docs/adr/0023-owned-aat-parser-ir-mapping.md docs/handoffs/owned-mapping-design.md docs/superpowers/plans/2026-07-03-aat-compatibility-registry-hardening.md src/abc/tools/aat_parser_ir_compat.clj src/abc/tools/validate_design_bundle.clj test/abc/tools/validate_design_bundle_test.clj
git commit -m "feat(parser-ir): validate AAT compatibility registry"
```
