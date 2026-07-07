# Soranoha Snapshot Publication Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Implement the first snapshot-publication slice: ADR 0026 status update, snapshot index identity/schema/fixture validation, request-set fixtures, and a `soranoha` flake app wrapper.

**Architecture:** Keep ADR 0026 as the analysis/request-set authority and add a thin snapshot layer above it. Snapshot identity is computed from a canonical `snapshot_index_identity_object`, while request-set fixtures reuse `abc.tools.analysis-identity` helpers. The `soranoha` app is a dispatcher over existing ABC tools, not a service runtime.

**Tech Stack:** Clojure 1.12, JSON Schema draft 2020-12, RFC 8785 JCS helpers, Nix flakes, Kaocha.

## Global Constraints

- Snapshot labels use `soranoha-snapshot-YYYY-MM-DD-NN`.
- Snapshot labels are locators, not identity.
- Snapshot index identity is `sha256(RFC8785-JCS(snapshot_index_identity_object))`.
- Empty arrays, not null, represent no applicable array values in snapshot identity.
- Request-set identity uses ADR 0026 and `abc.tools.analysis-identity`.
- Tokenized artifacts remain out of the first implementation slice.
- Flake outputs expose tools and fixtures, not the full corpus artifact matrix.

---

### Task 1: ADR 0026 Status Update

**Files:**
- Modify: `abc/docs/adr/0026-analysis-artifact-identity.md`

**Interfaces:**
- Consumes: implemented analysis files and tests.
- Produces: accurate ADR status for the snapshot plan.

- [ ] Update the Implementation Status section to say the token-independent slice is partially implemented.
- [ ] Preserve deferred tokenizer and pack decisions.

### Task 2: Snapshot Index Schema and Fixture

**Files:**
- Create: `abc/schemas/snapshot-index.schema.json`
- Create: `abc/examples/v0/snapshot/snapshot-index.json`
- Modify: `abc/schemas/schema-contracts.json`
- Modify: `abc/test/abc/tools/schema_test.clj`

**Interfaces:**
- Produces: strict schema and fixture consumed by validation and CLI code.

- [ ] Add a schema fixture test expecting schema version `0.1.0`.
- [ ] Add schema and fixture.
- [ ] Run the focused schema test.

### Task 3: Snapshot Identity Helpers

**Files:**
- Create: `abc/src/abc/tools/snapshot_index.clj`
- Create: `abc/test/abc/tools/snapshot_index_test.clj`

**Interfaces:**
- Produces:
  - `snapshot-label?`
  - `artifact-set-hash`
  - `snapshot-identity-hash`
  - `validate-snapshot-index!`

- [ ] Write failing tests for label validation, artifact ordering, empty-array semantics, and identity mismatch rejection.
- [ ] Implement the helpers.
- [ ] Run focused snapshot tests.

### Task 4: Request-Set Fixtures

**Files:**
- Create: `abc/data/request-sets/smoke-basic-ja.json`
- Create: `abc/data/request-sets/demo-basic-ja.json`
- Create: `abc/data/request-sets/full-corpus-publication-basic-ja.json`
- Create: `abc/data/request-sets/full-corpus-analysis-basic-ja.json`
- Create: `abc/data/request-sets/full-corpus-basic-ja.json`
- Create: `abc/test/abc/tools/snapshot_request_set_test.clj`

**Interfaces:**
- Consumes: `abc.tools.analysis-identity/request-set-id`.
- Produces: named request-set fixtures that validate their embedded identity.

- [ ] Add failing fixture tests.
- [ ] Add fixtures.
- [ ] Run focused request-set fixture tests.

### Task 5: Design-Bundle and CLI Wiring

**Files:**
- Modify: `abc/src/abc/tools/validate_design_bundle.clj`
- Create: `abc/src/abc/tools/soranoha.clj`
- Create: `abc/test/abc/tools/soranoha_test.clj`
- Modify: `abc/deps.edn`
- Modify: `abc/flake.nix`
- Modify: `flake.nix`

**Interfaces:**
- Consumes: snapshot helpers and fixtures.
- Produces:
  - `clojure -M:abc/soranoha list-request-sets`
  - `clojure -M:abc/soranoha explain-request-set <label>`
  - `clojure -M:abc/soranoha snapshot-index`
  - `nix run .#abc-soranoha`
  - `nix run .#soranoha`

- [ ] Add failing CLI tests for list/explain/snapshot-index output.
- [ ] Wire validation into `validate-design-bundle`.
- [ ] Add CLI namespace and aliases.
- [ ] Expose flake apps.
- [ ] Run focused CLI and validation tests.

### Task 6: Final Verification

**Files:**
- All touched files.

**Interfaces:**
- Consumes: all previous tasks.
- Produces: verified implementation state.

- [ ] Run focused Clojure tests for snapshot and analysis.
- [ ] Run `git diff --check`.
- [ ] Run a flake app metadata check or `nix flake show` if affordable.
