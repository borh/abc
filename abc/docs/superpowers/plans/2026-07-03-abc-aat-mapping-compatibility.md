# ABC AAT Mapping Compatibility Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Wire ABC's imported ab-validator parser-IR boundary to carry the producer-owned AAT mapping document hash and validate AAT-derived parser-IR against an adapter/evidence-scoped compatibility registry.

**Architecture:** ABC owns the acceptance contract: manifest identity, schema validation, and compatibility gating. ab-validator owns the mapping document and adapter measurement evidence. The compatibility registry must bind mapping metadata to adapter evidence scope so a zero-UNSUPPORTED aozora-rs generated mapping is not silently treated as adapter-neutral after aozora2html measured real warigaki and kunten.

**Tech Stack:** Clojure tools in `src/abc/tools`, JSON Schema fixtures under `schemas/` and `examples/`, EDN registry under `data/`, tests in `test/abc/tools`, Nix gate via `nix run .#validate-design-bundle`.

**Execution Status:** Implemented 2026-07-03. Focused tests, `nix run .#validate-design-bundle`, and `nix flake check --print-build-logs` passed on `x86_64-linux`; `aarch64-linux` was omitted as incompatible by flake check.

## Global Constraints

- Do not hand-transcribe new mapping rules into ABC.
- Use `aat_parser_ir_mapping_hash` as the identity-bearing field; do not use `aat_parser_ir_mapping_schema_hash` as identity.
- Keep `mapping_schema_hash` as provenance/contract metadata only.
- Treat `ruby.direction` as a direct parser-IR projection after ADR 0024.
- Do not claim warigaki/kunten support from the aozora-rs generated mapping; registry entries must name their adapter/evidence scope.
- Preserve existing uncommitted handoff doc edits; do not revert unrelated user or prior-agent changes.
- Use tests first for behavior changes.

---

### Task 1: Manifest Identity Carries Mapping Document Hash

**Files:**
- Modify: `schemas/manifest-inputs.schema.json`
- Modify: `schemas/manifest.schema.json`
- Modify: `src/abc/tools/malli.clj`
- Modify: `src/abc/tools/manifest.clj`
- Modify: `src/abc/tools/materialize_import.clj`
- Modify: `test/abc/tools/materialize_import_test.clj`
- Modify: `test/abc/tools/validate_design_bundle_test.clj`
- Modify: `examples/ab-validator-output/manifest-inputs.json`

**Interfaces:**
- Consumes: manifest input key `"mapping_hash"` with value `sha256:af2aac0855b0ab42111b7a05aae7a6c337963446a7bc620d2c11790e524fbb03`.
- Produces: manifest identity key `"aat_parser_ir_mapping_hash"` copied from manifest inputs.

- [x] **Step 1: Write failing tests**

Add assertions that `::am/manifest-inputs` requires `mapping_hash`, materialized parser and warnings manifests include `"aat_parser_ir_mapping_hash"` in `manifest_identity_object`, and parser manifest provenance `used` includes the mapping hash.

- [x] **Step 2: Run focused tests to verify failure**

Run:

```bash
clojure -M:test -e "(require 'abc.tools.materialize-import-test 'abc.tools.validate-design-bundle-test 'clojure.test) (clojure.test/run-tests 'abc.tools.materialize-import-test 'abc.tools.validate-design-bundle-test)"
```

Expected: failures naming missing `mapping_hash` or missing `aat_parser_ir_mapping_hash`.

- [x] **Step 3: Implement minimal schema/code changes**

Add `"mapping_hash"` to manifest inputs JSON Schema and Malli required keys. Add `"aat_parser_ir_mapping_hash"` to manifest identity required/properties and `manifest/identity-keys`. Copy `(get manifest-inputs "mapping_hash")` into the identity object. Add the mapping hash to parser-IR manifest `used`; warnings may carry the identity dimension but does not need to list the mapping in `used`.

- [x] **Step 4: Run focused tests to verify pass**

Run the same focused test command. Expected: zero failures.

---

### Task 2: Adapter-Scoped Compatibility Registry

**Files:**
- Create: `data/aat-parser-ir-compatibility.edn`
- Create: `src/abc/tools/aat_parser_ir_compat.clj`
- Modify: `src/abc/tools/validate_design_bundle.clj`
- Modify: `test/abc/tools/validate_design_bundle_test.clj`
- Modify: `examples/ab-validator-output/parser-ir.json`

**Interfaces:**
- Consumes: parser-IR `derived_from` with `aat_version`, `aat_adapter`, `aat_adapter_version`, `mapping_id`, `mapping_version`, and `mapping_schema_hash`.
- Consumes: manifest inputs `mapping_hash`.
- Produces: validation errors when no registry entry matches all of adapter, AAT version, mapping id/version/hash, mapping schema hash, and parser-IR schema hash.

- [x] **Step 1: Write failing tests**

Add tests for `compat/compatible?`:

```clojure
(is (true? (compat/compatible? registry valid-query)))
(is (false? (compat/compatible? registry (assoc valid-query :aat_adapter "aozora2html"))))
(is (false? (compat/compatible? registry (assoc valid-query :mapping_hash (files/example-hash "99")))))
```

Add `validate/compatibility-errors` tests for missing `derived_from` on AAT-derived imports and for adapter mismatch.

- [x] **Step 2: Run focused tests to verify failure**

Run:

```bash
clojure -M:test -e "(require 'abc.tools.validate-design-bundle-test 'clojure.test) (clojure.test/run-tests 'abc.tools.validate-design-bundle-test)"
```

Expected: namespace/function missing or compatibility assertion failures.

- [x] **Step 3: Implement registry and validation**

Create EDN registry with one measured aozora-rs entry:

```clojure
{:entries
 [{:aat_version 1
   :aat_adapter "aozora-rs-adapter"
   :aat_adapter_version nil
   :mapping_id "https://w3id.org/abc/mappings/aat-v1-to-parser-ir-v1/generated-probe"
   :mapping_version "0.1.0"
   :mapping_hash "sha256:af2aac0855b0ab42111b7a05aae7a6c337963446a7bc620d2c11790e524fbb03"
   :mapping_schema_hash "sha256:38ec7f0e5affb10329b550a091cd3a6fb5a25e26fd469dfe9f8249970cf9adb4"
   :parser_ir_schema_hash "sha256:41c43f0c88a66c31ae4fbf9b9eeb04de92756082acaaaa1c2e21f1a5bf74a396"
   :evidence_scope {:adapter "aozora-rs-adapter"
                    :corpus "aozora-rs full corpus"
                    :files_scanned 17894
                    :files_with_unsupported 0
                    :generated_rules 25}
   :compatibility "lossy"}]}
```

Validation should only run compatibility checks when `parser-ir["derived_from"]` is present or `manifest-inputs["mapping_hash"]` is present. When present, require both sides and report concrete missing/mismatch errors.

- [x] **Step 4: Run focused tests to verify pass**

Run the same focused test command. Expected: zero failures.

---

### Task 3: Fixture Refresh and Gates

**Files:**
- Modify: materialized files under `examples/ab-validator-output/materialized/` if the repo has them.
- Modify: checked-in canonical fixture hash in `test/abc/tools/materialize_import_test.clj` if required by schema hash change.
- Modify: handoff docs only to reference the new ABC registry gate, not to restate mapping rows.

**Interfaces:**
- Consumes: Task 1 and Task 2 outputs.
- Produces: repository gates passing with the new mapping identity and compatibility validation.

- [x] **Step 1: Materialize imported fixture**

Run:

```bash
tmpdir=$(mktemp -d)
clojure -M:abc/materialize-import examples/ab-validator-output "$tmpdir" 2026-04-26T00:00:00Z
find "$tmpdir" -type f -maxdepth 1 -print
```

If the repo has checked-in materialized output, copy the deterministic output through the repo's existing fixture update path.

- [x] **Step 2: Run focused tests**

Run:

```bash
clojure -M:test -e "(require 'abc.tools.materialize-import-test 'abc.tools.validate-design-bundle-test 'clojure.test) (clojure.test/run-tests 'abc.tools.materialize-import-test 'abc.tools.validate-design-bundle-test)"
```

Expected: zero failures.

- [x] **Step 3: Run design-bundle gate**

Run:

```bash
nix run .#validate-design-bundle
```

Expected: `design bundle validation ok`.

- [x] **Step 4: Run full flake gate if focused gates pass**

Run:

```bash
nix flake check --print-build-logs
```

Expected: all compatible checks pass.
