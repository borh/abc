# Annotation Input-View Widening (ADR 0028 D6 Follow-up) Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make `parser-ir-body-annotations-v1` a resolvable request-set input view — widening the resolver allow-list and the request-set schema — so recipes can consume the annotation view, replacing the first slice's demonstration-only fixture.

**Architecture:** Widen `analysis-identity/allowed-input-view-kinds` (identity layer, no resolver code change beyond the set), turn the request-set schema's `inputView` `$def` into a `oneOf` (plaintext variant keeps its three required keys; annotation variant is exactly the D6 shape `{input_view_kind, policy_hash}`), add a machine-check test that the schema's kinds and the allow-list agree (the Task-7 forward-pointer from the first slice), rotate the five committed request-set fixtures whose identity objects embed the schema hash, and land a resolved `demo-annotation-ja` fixture. The rotation also registers `annotation-output.schema.json` in the cross-repo schema contracts (the deferred final-review follow-up — same mechanics, same commit).

**Tech Stack:** Clojure (abc tools), JSON Schema, `tools/schema_contracts.py`, Nix design-bundle validation.

## Global Constraints

- All work in a git worktree on a feature branch under `.worktrees/`; merge to main when done (many concurrent sessions).
- Test runner: `bin/kaocha` from `abc/` (NOT `clojure -M:test`, which opens a REPL). Focused runs: `bin/kaocha --focus <ns>`.
- Design bundle gate: `clojure -M:abc/validate-design-bundle` from `abc/` (the `nix run .#validate-design-bundle` wrapper has a pre-existing git-cliff environment failure unrelated to this work).
- The full suite is currently 100% green (455 tests, 0 failures); it must be green at every task's commit.
- The annotation input-view shape is exactly D6's: `{"input_view_kind": "parser-ir-body-annotations-v1", "policy_hash": <annotation-policy-hash>}` — no `input_normalization_policy_hash` key (the annotation view anchors to the plaintext view directly; normalization interplay is deferred D7).
- Schema files carry a `"version"` field tracked in `test/abc/tools/schema_test.clj` `cross-project-schema-versions`.
- `schemas/request-set.schema.json`'s hash is embedded in every committed `data/request-sets/*.json` identity object AND in `schemas/schema-contracts.json` + `../ab-validator/data/abc-schemas/schema-contracts.json` — any schema edit requires regenerating all fixtures and rotating both contracts files.
- ADR 0028's Acceptance Criteria heading must stay exactly `## Acceptance Criteria` (the acceptance lint's section extraction is case-sensitive).
- ruby-gaiji-v1 annotation policy hash (verified on current main): `sha256:6b2b9f29b742d434a2a114ace68549c3ad2611454785cb7a6924f5eaf95babde`.
- Working dir for all paths below: `abc/` unless prefixed `ab-validator/`.

---

### Task 1: Widen `allowed-input-view-kinds` and prove identity-object behavior

**Files:**
- Modify: `src/abc/tools/analysis_identity.clj:9-10`
- Test: `test/abc/tools/analysis_identity_test.clj` (extend)

**Interfaces:**
- Produces: `analysis-identity/allowed-input-view-kinds` = `#{"parser-ir-plaintext-body-v1" "parser-ir-body-annotations-v1"}`; `request-set-identity-object` accepts annotation input views (Task 3's resolver flow relies on this — the resolver calls it via `canonical-input-views` with no other gating).

- [ ] **Step 1: Write the failing test** (append to `test/abc/tools/analysis_identity_test.clj`; reuse the file's existing `subject-a` def and `files/example-hash` helper — the same ones `request-set-identity-rejects-unsupported-input-view-kind-test` at line 77 uses)

```clojure
(deftest request-set-identity-accepts-annotation-input-view-test
  (let [annotation-view {"input_view_kind" "parser-ir-body-annotations-v1"
                         "policy_hash" (files/example-hash "31")}
        plaintext-view {"input_view_kind" "parser-ir-plaintext-body-v1"
                        "policy_hash" (files/example-hash "32")
                        "input_normalization_policy_hash" (files/example-hash "33")}
        identity-object (analysis-identity/request-set-identity-object
                         {:schema-hash (files/example-hash "01")
                          :corpus-snapshot-hash (files/example-hash "02")
                          :subjects [subject-a]
                          :input-views [plaintext-view annotation-view]
                          :tokenizer-profile-hashes []
                          :analysis-recipe-hashes []
                          :missing-policy "build-missing-only"
                          :pack-policy-hash (files/example-hash "05")})]
    (testing "annotation views participate and sort by kind (b… before p…)"
      (is (= ["parser-ir-body-annotations-v1" "parser-ir-plaintext-body-v1"]
             (mapv #(get % "input_view_kind")
                   (get identity-object "input_views")))))
    (testing "the D6 two-key shape passes through untouched"
      (is (= annotation-view (first (get identity-object "input_views")))))))
```

- [ ] **Step 2: Run test to verify it fails**

Run: `bin/kaocha --focus abc.tools.analysis-identity-test`
Expected: FAIL — `Invalid input view kind` ex-info from `canonical-input-views` (the new test); all pre-existing tests still pass.

- [ ] **Step 3: Widen the allow-list**

In `src/abc/tools/analysis_identity.clj`, replace lines 9-10:

```clojure
(def allowed-input-view-kinds
  #{"parser-ir-plaintext-body-v1" "parser-ir-body-annotations-v1"})
```

- [ ] **Step 4: Run tests to verify they pass**

Run: `bin/kaocha --focus abc.tools.analysis-identity-test`
Expected: PASS, including the pre-existing `request-set-identity-rejects-unsupported-input-view-kind-test` (its `"tei-body-text-v1"` probe is still outside the set).

- [ ] **Step 5: Run the full suite, then commit**

Run: `bin/kaocha`
Expected: 456 tests, 0 failures. (The request-set schema is untouched in this task, so no fixture churn.)

```bash
git add src/abc/tools/analysis_identity.clj test/abc/tools/analysis_identity_test.clj
git commit -m "feat(abc): allow parser-ir-body-annotations-v1 in request-set input views (ADR 0028 D6 follow-up)"
```

---

### Task 2: Request-set schema v0.1.4 `oneOf` + machine-check + fixture/contract rotation

**Files:**
- Modify: `schemas/request-set.schema.json` (`$defs.inputView` at lines 90-99; top-level `"version"` at line 5: `"0.1.3"` → `"0.1.4"`)
- Modify: `test/abc/tools/schema_test.clj:19` (`"schemas/request-set.schema.json" "0.1.4"`)
- Modify: `tools/schema_contracts.py` (`SCHEMA_FILES` tuple: add `"annotation-output.schema.json"` — the deferred final-review registration)
- Regenerate: all five `data/request-sets/*.json` fixtures (their identity objects embed the schema hash)
- Regenerate: `schemas/schema-contracts.json`; sync to `ab-validator/data/abc-schemas/`
- Test: `test/abc/tools/request_set_fixture_test.clj` (add the machine-check deftest)

**Interfaces:**
- Consumes: Task 1's widened `allowed-input-view-kinds`.
- Produces: schema accepting both input-view variants; a standing machine-check test binding the schema's kinds to the allow-list (catches any future one-sided widening); rotated fixtures and cross-repo schema contracts.

- [ ] **Step 1: Write the failing machine-check test** (append to `test/abc/tools/request_set_fixture_test.clj`; the ns already requires `analysis-identity` and `files`)

```clojure
(deftest input-view-kinds-schema-and-allow-list-agree-test
  (let [schema (files/read-json "schemas/request-set.schema.json")
        variants (get-in schema ["$defs" "inputView" "oneOf"])
        schema-kinds (into #{}
                           (mapcat #(get-in % ["properties" "input_view_kind" "enum"]))
                           variants)]
    (testing "inputView is a oneOf over per-kind variants"
      (is (seq variants)))
    (testing "every kind the schema admits is exactly the resolver allow-list"
      (is (= analysis-identity/allowed-input-view-kinds schema-kinds)))))
```

- [ ] **Step 2: Run test to verify it fails**

Run: `bin/kaocha --focus abc.tools.request-set-fixture-test`
Expected: FAIL — `(seq variants)` is nil (current schema has no `oneOf`).

- [ ] **Step 3: Edit the schema**

In `schemas/request-set.schema.json`: bump line 5 to `"version": "0.1.4"`, and replace the whole `$defs.inputView` value (lines 90-99) with:

```json
"inputView": {
  "oneOf": [
    {
      "type": "object",
      "additionalProperties": false,
      "required": ["input_view_kind", "policy_hash", "input_normalization_policy_hash"],
      "properties": {
        "input_view_kind": { "enum": ["parser-ir-plaintext-body-v1"] },
        "policy_hash": { "$ref": "#/$defs/hash" },
        "input_normalization_policy_hash": { "$ref": "#/$defs/hash" }
      }
    },
    {
      "type": "object",
      "additionalProperties": false,
      "required": ["input_view_kind", "policy_hash"],
      "properties": {
        "input_view_kind": { "enum": ["parser-ir-body-annotations-v1"] },
        "policy_hash": { "$ref": "#/$defs/hash" }
      }
    }
  ]
}
```

(References to `#/$defs/inputView` elsewhere in the schema are unchanged.) Update `test/abc/tools/schema_test.clj:19` to `"schemas/request-set.schema.json" "0.1.4"`.

- [ ] **Step 4: Regenerate the committed request-set fixtures**

The schema hash is part of each fixture's `request_set_identity_object`, so every fixture must be re-resolved:

```bash
clojure -M -e '(require (quote [abc.tools.request-set-resolver :as r])) (doseq [l (r/request-set-labels)] (println (str (r/write-resolved-request-set! l "data/request-sets"))))'
git diff --stat data/request-sets/
```

Expected: all five fixtures rewritten; the diff touches only `schema_hash`, `request_set_id`, and `request_set_identity_object.schema_hash` lines (resolution timestamps are the fixed `default-resolved-at`, so nothing else churns). If anything else changes, stop and investigate before committing.

- [ ] **Step 5: Register annotation-output in the schema contracts and rotate both copies**

In `tools/schema_contracts.py`, add `"annotation-output.schema.json"` to `SCHEMA_FILES` (keep the tuple's existing grouping; alphabetical near the top). Then:

```bash
python tools/schema_contracts.py --write
python tools/schema_contracts.py            # check mode: must exit 0
cp schemas/request-set.schema.json ../ab-validator/data/abc-schemas/nix-schemas/
cp schemas/annotation-output.schema.json ../ab-validator/data/abc-schemas/nix-schemas/
cp schemas/schema-contracts.json ../ab-validator/data/abc-schemas/schema-contracts.json
ls ../ab-validator/data/abc-schemas/schemas/ | grep -E "request-set|annotation" || true
```

If that last `ls` shows the `schemas/` (non-nix) directory also carries copies of the touched schema files, sync those too — mirror whatever the manifest-schema rotation did in precedent commit `1c6267cb` (Task 2 of the first slice; `git show 1c6267cb --stat` lists every ab-validator path it synced).

- [ ] **Step 6: Run the gates**

Run: `bin/kaocha --focus abc.tools.request-set-fixture-test` then `bin/kaocha` then `clojure -M:abc/validate-design-bundle`
Expected: focused PASS (machine-check + all five fixtures round-trip against the new schema hash); full suite 457 tests, 0 failures; design bundle PASS. Any failure names a stale fixture or missed contracts copy — fix and re-run.

- [ ] **Step 7: Commit**

```bash
git add -A
git commit -m "feat(abc): request-set schema v0.1.4 inputView oneOf; rotate fixtures and schema contracts; register annotation-output schema"
```

---

### Task 3: `demo-annotation-ja` — a resolved request set carrying the annotation view

**Files:**
- Create: `data/request-set-definitions/demo-annotation-ja.json`
- Create (generated): `data/request-sets/demo-annotation-ja.json`
- Modify: `test/abc/tools/request_set_fixture_test.clj` (replace the first-slice demonstration deftest and its stale comment)

**Interfaces:**
- Consumes: Tasks 1-2 (resolvable annotation input views); `resolver/write-resolved-request-set!`.
- Produces: a committed, fully resolved request-set fixture whose `input_views` carries the annotation view — automatically covered by the existing `request-set-fixtures-carry-computed-request-set-ids-test` round-trip (it iterates every file in `data/request-sets/`), and schema-validated by the design bundle.

- [ ] **Step 1: Replace the demonstration test with a resolved-fixture test** (in `test/abc/tools/request_set_fixture_test.clj`, delete `annotation-input-view-fixture-entry-matches-design-d6-test` AND its preceding `;; D6 …` comment block at lines 39-54, replacing both with)

```clojure
;; D6 (2026-07-09 design, widened 2026-07-10): annotation views participate in
;; request-set input_views as {"input_view_kind" "parser-ir-body-annotations-v1",
;; "policy_hash" <annotation-policy-hash>}. demo-annotation-ja is the resolved
;; fixture; the round-trip deftest above covers its request_set_id and resolver
;; equality like every other label, and input-view-kinds-schema-and-allow-list-
;; agree-test pins the schema/allow-list agreement.
(deftest annotation-input-view-resolves-in-demo-annotation-ja-test
  (let [request-set (files/read-json "data/request-sets/demo-annotation-ja.json")
        policy (files/read-json "data/annotation-policies/ruby-gaiji-v1.json")
        views (get-in request-set ["request_set_identity_object" "input_views"])
        ann-views (filterv #(= "parser-ir-body-annotations-v1"
                               (get % "input_view_kind"))
                           views)]
    (testing "exactly one annotation input view, carrying the ruby-gaiji-v1 hash"
      (is (= 1 (count ann-views)))
      (is (= (analysis-identity/annotation-policy-hash policy)
             (get (first ann-views) "policy_hash"))))
    (testing "the D6 two-key shape, no normalization coordinate (deferred D7)"
      (is (= #{"input_view_kind" "policy_hash"}
             (set (keys (first ann-views))))))
    (testing "the plaintext view rides alongside for the token join"
      (is (some #(= "parser-ir-plaintext-body-v1" (get % "input_view_kind"))
                views)))))
```

- [ ] **Step 2: Run test to verify it fails**

Run: `bin/kaocha --focus abc.tools.request-set-fixture-test`
Expected: FAIL — `data/request-sets/demo-annotation-ja.json` does not exist yet.

- [ ] **Step 3: Add the definition and resolve it**

`data/request-set-definitions/demo-annotation-ja.json` (subjects and plaintext view copied from `demo-basic-ja.json`; the annotation `policy_hash` is the ruby-gaiji-v1 hash from Global Constraints):

```json
{
  "request_set_definition_version": "request-set-definition-v1",
  "label": "demo-annotation-ja",
  "corpus_snapshot_hash": "sha256:1111111111111111111111111111111111111111111111111111111111111111",
  "subjects": [
    {
      "source_id": "aozora:demo-fixture-a",
      "work_id": "aozora:demo-fixture-a",
      "work_content_hash": "sha256:2323232323232323232323232323232323232323232323232323232323232323",
      "metadata_record_hash": null
    },
    {
      "source_id": "aozora:demo-fixture-b",
      "work_id": "aozora:demo-fixture-b",
      "work_content_hash": "sha256:2424242424242424242424242424242424242424242424242424242424242424",
      "metadata_record_hash": null
    }
  ],
  "input_views": [
    {
      "input_view_kind": "parser-ir-plaintext-body-v1",
      "policy_hash": "sha256:df21c590fd8d5b934fd426e632a3d8a09c2bd3fa299ca6b4c8fe4c797e1d1391",
      "input_normalization_policy_hash": "sha256:530c59689dd909c171790036cddc7916f8685897b6342bfa794d4611816d3813"
    },
    {
      "input_view_kind": "parser-ir-body-annotations-v1",
      "policy_hash": "sha256:6b2b9f29b742d434a2a114ace68549c3ad2611454785cb7a6924f5eaf95babde"
    }
  ],
  "analysis_recipe_ids": ["literary-basic-ja-v1"],
  "tokenizer_profile_ids": [],
  "missing_policy": "build-missing-only",
  "pack_policy_id": "no-pack-v1"
}
```

Resolve it into the committed fixtures directory:

```bash
clojure -M -e '(require (quote [abc.tools.request-set-resolver :as r])) (println (str (r/write-resolved-request-set! "demo-annotation-ja" "data/request-sets")))'
```

- [ ] **Step 4: Run the focused suite, full suite, and design bundle**

Run: `bin/kaocha --focus abc.tools.request-set-fixture-test` then `bin/kaocha` then `clojure -M:abc/validate-design-bundle`
Expected: focused PASS (new fixture asserted directly AND swept into the round-trip test — note that test asserts `tokenizer_profile_hashes` is `[]` for every label, which holds here); full suite 457 tests (demonstration deftest replaced 1-for-1), 0 failures; design bundle PASS (new fixture schema-validates against v0.1.4).

- [ ] **Step 5: Commit**

```bash
git add data/request-set-definitions/demo-annotation-ja.json data/request-sets/demo-annotation-ja.json test/abc/tools/request_set_fixture_test.clj
git commit -m "feat(abc): demo-annotation-ja request set resolves the annotation input view"
```

---

### Task 4: ADR 0028 + design-spec touch-up, final gates

**Files:**
- Modify: `docs/adr/0028-ruby-annotation-view.md`
- Modify: `docs/superpowers/specs/2026-07-09-ruby-annotation-view-design.md` (D6 section, one clarifying line)

- [ ] **Step 1: Update the ADR** — precise directives (read the current text first; keep the heading exactly `## Acceptance Criteria`):
  1. In Implementation Status and in the Settled-D6 bullet, replace every statement that the resolver allow-list / `request-set.schema.json` `inputView` enum are NOT widened with the new fact: widened 2026-07-10 — `allowed-input-view-kinds` now includes `parser-ir-body-annotations-v1`, request-set schema v0.1.4 accepts the D6 two-key shape via `oneOf`, and `data/request-sets/demo-annotation-ja.json` is a resolved fixture (cite `test/abc/tools/request_set_fixture_test.clj` for the machine-check and resolved-fixture tests).
  2. In the Acceptance Criteria section, upgrade the D6/request-set bullet from demonstration-only status to done, keeping at least one `test/`- or `data/`-path reference in the section (the lint's executable-path requirement).
  3. Add `schemas/schema-contracts.json` registration of `annotation-output.schema.json` to the Implementation Status (it was the deferred registration; now landed).
- [ ] **Step 2: Update the design spec's D6 section** — after the existing "**Decision.** … No resolver code change; add a fixture." paragraph, add one line: "*(First slice shipped the demonstration fixture only; the widening landed 2026-07-10: schema v0.1.4 `oneOf`, `allowed-input-view-kinds`, and the resolved `demo-annotation-ja` fixture.)*"

- [ ] **Step 3: Run the full suite and design bundle one last time**

Run: `bin/kaocha && clojure -M:abc/validate-design-bundle`
Expected: 457 tests, 0 failures (acceptance lint stays green); design bundle PASS.

- [ ] **Step 4: Commit**

```bash
git add docs/adr/0028-ruby-annotation-view.md docs/superpowers/specs/2026-07-09-ruby-annotation-view-design.md
git commit -m "docs(adr/spec): ADR 0028 D6 widening landed — annotation input views resolvable"
# merge the worktree branch to main once green (repo convention)
```
