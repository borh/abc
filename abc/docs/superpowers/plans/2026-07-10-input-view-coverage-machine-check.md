# Recipe/Request-Set Input-View Coverage Machine-Check Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Machine-check the agreement between analysis-recipe `supported_input_view_kinds` and request-set `input_views` — every referenced recipe must be able to consume at least one view the request set provides, and every declared view must have a consumer — enforced at resolve time and pinned by a fixture-wide test over all committed request-set definitions.

**Architecture:** A pure assertion function in `abc.tools.analysis-identity` (which already owns the input-view-kind vocabulary) implements a two-direction consumer model; `abc.tools.request-set-resolver/resolve-request-set` calls it with the raw recipe registry values it already reads (a one-line addition to `resolve-semantic-registry-value`'s return map). A fixture-wide test binds all committed definitions + the recipe registry to the check so registry drift is caught even without resolving.

**Tech Stack:** Clojure (abc/), kaocha (`bin/kaocha` from `abc/` — plain `clojure -M:test` opens a REPL, never use it), design-bundle gate `clojure -M:abc/validate-design-bundle` from `abc/` (do NOT use `nix run .#validate-design-bundle` — pre-existing git-cliff env failure).

## Global Constraints

- All commands run from `abc/` inside this worktree unless stated otherwise.
- Test runner is exactly `bin/kaocha` (focused: `bin/kaocha --focus <test-ns>`).
- Do not modify any JSON schema file — this slice deliberately avoids schema-hash rotation. `schemas/analysis-recipe.schema.json`'s enum still admits only `parser-ir-plaintext-body-v1` and `token-stream-v1` for `supported_input_view_kinds`; widening it for annotation-consuming recipes is explicitly future work.
- Do not modify any file under `data/` — no fixture rotation; the resolved request-set golden files must remain byte-identical (the new `:value` key in the resolver's internal registry-resolution map is never serialized).
- The consumer model (fixed by this plan, do not redesign):
  - **Provided kinds** = the definition's `input_views` kinds ∪ `{"token-stream-v1"}` iff `tokenizer_profile_ids` is non-empty (the token stream is a derived view produced by tokenizer profiles).
  - **Direction A (recipe runnability):** every referenced recipe's `supported_input_view_kinds` must intersect the provided kinds.
- Error message (exact, tests match on it): `"Analysis recipe cannot consume any provided input view"`.

## Revision (2026-07-10, during Task 2)

The plan originally also specified a **Direction B (no dead views)**: every
declared view kind needs a consumer among {referenced recipes, tokenizer,
annotation materializer}. Task 2's fixture-wide machine-check disproved that
model: `full-corpus-publication-basic-ja` legitimately declares a plaintext
input view with zero recipes and zero tokenizer profiles — its consumer is
the *publication* materialization flow, which has no signal in the
definition or the recipe registry. Dead-view detection is therefore not
decidable from the recipe registry, and Direction B is dropped: the check is
Direction A only. Task 1's code blocks below are superseded where they
mention Direction B / the `"Request-set input view has no consumer"` error;
the committed code (post-fix) is authoritative. Do not "fix" fixtures under
`data/` to satisfy a check — a check that rejects a legitimate committed
fixture is evidence against the check's model.
- ADR heading must remain exactly `## Acceptance Criteria` (the lint's section extraction is case-sensitive... was fixed, but keep exact case anyway).

---

### Task 1: `assert-input-view-coverage!` in analysis-identity

**Files:**
- Modify: `abc/src/abc/tools/analysis_identity.clj` (add `clojure.set` require; add fn after `canonical-input-views`, around line 81)
- Test: `abc/test/abc/tools/analysis_identity_test.clj` (append)

**Interfaces:**
- Consumes: nothing new.
- Produces: `abc.tools.analysis-identity/assert-input-view-coverage!` — takes `{:input-views [<{"input_view_kind" ...}>], :recipes [<raw recipe JSON values>], :tokenizer-profile-ids [<string>]}`, returns `nil` on success, throws `clojure.lang.ExceptionInfo` on violation. Task 2 calls exactly this.

- [ ] **Step 1: Write the failing tests** (append to `abc/test/abc/tools/analysis_identity_test.clj`; the file already requires `abc.tools.files :as files` and `clojure.test`):

```clojure
;; --- assert-input-view-coverage! (recipe supported_input_view_kinds ↔
;; request-set input_views agreement; ADR 0028 D6 follow-through) ---

(def coverage-plaintext-view
  {"input_view_kind" "parser-ir-plaintext-body-v1"
   "policy_hash" (files/example-hash "41")
   "input_normalization_policy_hash" (files/example-hash "42")})

(def coverage-annotation-view
  {"input_view_kind" "parser-ir-body-annotations-v1"
   "policy_hash" (files/example-hash "43")})

(def coverage-plaintext-recipe
  {"recipe_id" "plaintext-recipe-v1"
   "supported_input_view_kinds" ["parser-ir-plaintext-body-v1"]})

(def coverage-token-recipe
  {"recipe_id" "token-recipe-v1"
   "supported_input_view_kinds" ["token-stream-v1"]})

(def coverage-annotation-recipe
  {"recipe_id" "annotation-recipe-v1"
   "supported_input_view_kinds" ["parser-ir-body-annotations-v1"]})

(deftest input-view-coverage-passes-for-plaintext-recipe-test
  (is (nil? (analysis-identity/assert-input-view-coverage!
             {:input-views [coverage-plaintext-view]
              :recipes [coverage-plaintext-recipe]
              :tokenizer-profile-ids []}))))

(deftest input-view-coverage-rejects-token-recipe-without-tokenizer-test
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #"Analysis recipe cannot consume any provided input view"
       (analysis-identity/assert-input-view-coverage!
        {:input-views [coverage-plaintext-view]
         :recipes [coverage-plaintext-recipe coverage-token-recipe]
         :tokenizer-profile-ids []}))))

(deftest input-view-coverage-accepts-token-recipe-with-tokenizer-test
  ;; tokenizer profiles provide the derived token-stream-v1 view AND consume
  ;; the plaintext view, so both directions pass.
  (is (nil? (analysis-identity/assert-input-view-coverage!
             {:input-views [coverage-plaintext-view]
              :recipes [coverage-token-recipe]
              :tokenizer-profile-ids ["fixture-tokenizer-ja-v1"]}))))

(deftest input-view-coverage-rejects-unconsumed-plaintext-view-test
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #"Request-set input view has no consumer"
       (analysis-identity/assert-input-view-coverage!
        {:input-views [coverage-plaintext-view]
         :recipes []
         :tokenizer-profile-ids []}))))

(deftest input-view-coverage-accepts-tokenizer-only-request-set-test
  ;; mirrors the committed tokenizer-profile resolver test: no recipes, one
  ;; profile — the tokenizer is the plaintext view's consumer.
  (is (nil? (analysis-identity/assert-input-view-coverage!
             {:input-views [coverage-plaintext-view]
              :recipes []
              :tokenizer-profile-ids ["fixture-tokenizer-ja-v1"]}))))

(deftest input-view-coverage-accepts-unconsumed-annotation-view-test
  ;; demo-annotation-ja shape: the annotation view is consumed by the
  ;; annotation materializer (ADR 0028), never dead even with no
  ;; annotation-consuming recipe.
  (is (nil? (analysis-identity/assert-input-view-coverage!
             {:input-views [coverage-plaintext-view coverage-annotation-view]
              :recipes [coverage-plaintext-recipe]
              :tokenizer-profile-ids []}))))

(deftest input-view-coverage-accepts-annotation-consuming-recipe-test
  ;; forward case: once annotation-consuming recipes exist, an
  ;; annotation-only request set is runnable.
  (is (nil? (analysis-identity/assert-input-view-coverage!
             {:input-views [coverage-annotation-view]
              :recipes [coverage-annotation-recipe]
              :tokenizer-profile-ids []}))))

(deftest input-view-coverage-empty-request-set-passes-test
  (is (nil? (analysis-identity/assert-input-view-coverage!
             {:input-views []
              :recipes []
              :tokenizer-profile-ids []}))))
```

- [ ] **Step 2: Run tests to verify they fail**

Run (from `abc/`): `bin/kaocha --focus abc.tools.analysis-identity-test`
Expected: FAIL — `assert-input-view-coverage!` unresolved / does not exist.

- [ ] **Step 3: Implement**

In `abc/src/abc/tools/analysis_identity.clj`, change the ns form to add `clojure.set`:

```clojure
(ns abc.tools.analysis-identity
  (:require [abc.tools.hash :as hash]
            [abc.tools.manifest :as manifest]
            [clojure.set :as set]
            [clojure.string :as string]))
```

Then add after `canonical-input-views` (before `canonical-hashes`):

```clojure
(defn assert-input-view-coverage!
  "Machine-checks the agreement between analysis-recipe
  `supported_input_view_kinds` and a request-set definition's `input_views`
  (ADR 0028 D6 follow-through).

  Consumer model:
  - Provided kinds are the declared input-view kinds, plus the derived
    \"token-stream-v1\" view when tokenizer profiles are present.
  - Direction A (recipe runnability): every referenced recipe must support at
    least one provided kind — otherwise the recipe could never run.
  - Direction B (no dead views): every declared view kind must have a
    consumer — a recipe that supports it, the tokenizer (consumes
    \"parser-ir-plaintext-body-v1\" when profiles are present), or the
    annotation materializer (\"parser-ir-body-annotations-v1\" is materialized
    directly from parser IR per ADR 0028, not recipe-mediated) — otherwise
    the view changes the request_set_id without affecting any output.

  Takes {:input-views ..., :recipes <raw recipe registry values>,
  :tokenizer-profile-ids ...}; returns nil, throws ex-info on violation."
  [{:keys [input-views recipes tokenizer-profile-ids]}]
  (let [view-kinds (into #{} (map #(get % "input_view_kind")) input-views)
        tokenized? (boolean (seq tokenizer-profile-ids))
        provided (cond-> view-kinds
                   tokenized? (conj "token-stream-v1"))]
    (doseq [recipe recipes]
      (let [supported (set (get recipe "supported_input_view_kinds"))]
        (when (empty? (set/intersection supported provided))
          (throw (ex-info "Analysis recipe cannot consume any provided input view"
                          {:recipe_id (get recipe "recipe_id")
                           :supported_input_view_kinds (vec (sort supported))
                           :provided_input_view_kinds (vec (sort provided))})))))
    (let [consumed (cond-> (into #{"parser-ir-body-annotations-v1"}
                                 (mapcat #(get % "supported_input_view_kinds"))
                                 recipes)
                     tokenized? (conj "parser-ir-plaintext-body-v1"))]
      (doseq [kind (sort view-kinds)]
        (when-not (contains? consumed kind)
          (throw (ex-info "Request-set input view has no consumer"
                          {:input_view_kind kind
                           :consumed_input_view_kinds (vec (sort consumed))})))))
    nil))
```

- [ ] **Step 4: Run tests to verify they pass**

Run: `bin/kaocha --focus abc.tools.analysis-identity-test`
Expected: PASS (all, including the pre-existing tests in the ns).

- [ ] **Step 5: Commit**

```bash
git add src/abc/tools/analysis_identity.clj test/abc/tools/analysis_identity_test.clj
git commit -m "feat(abc): input-view coverage assertion for recipe/request-set agreement"
```

---

### Task 2: Resolver enforcement + fixture-wide machine-check

**Files:**
- Modify: `abc/src/abc/tools/request_set_resolver.clj` (two edits: `resolve-semantic-registry-value` ~line 84-95; `resolve-request-set` ~line 256-266)
- Test: `abc/test/abc/tools/request_set_resolver_test.clj` (append)
- Test: `abc/test/abc/tools/request_set_fixture_test.clj` (append)

**Interfaces:**
- Consumes: `abc.tools.analysis-identity/assert-input-view-coverage!` from Task 1 — signature `{:input-views ..., :recipes ..., :tokenizer-profile-ids ...}` → nil or throws.
- Produces: `resolve-request-set` now throws `ExceptionInfo` with the Task 1 messages on coverage violations; `resolve-semantic-registry-value`'s return map gains a `:value` key (raw registry JSON value) — internal only, never serialized.

- [ ] **Step 1: Write the failing tests**

Append to `abc/test/abc/tools/request_set_resolver_test.clj` (the file already has `source-snapshot-definition` and the `with-redefs` pattern; `files/example-hash` is required):

```clojure
(defn- token-recipe-definition [tokenizer-profile-ids]
  (assoc (source-snapshot-definition "unused-source-snapshot.json")
         "label" "token-recipe-coverage-basic-ja"
         "corpus_snapshot_hash" (files/example-hash "aa")
         "subjects" [{"source_id" "aozora:000001"
                      "work_id" "aozora:000001"
                      "work_content_hash" (files/example-hash "bb")
                      "metadata_record_hash" nil}]
         "subject_source" nil
         "analysis_recipe_ids" ["token-basic-ja-v1"]
         "tokenizer_profile_ids" tokenizer-profile-ids))

(deftest resolve-request-set-rejects-unconsumable-recipe-test
  ;; token-basic-ja-v1 supports only token-stream-v1; with no tokenizer
  ;; profile the request set provides nothing it can consume.
  (with-redefs [resolver/read-request-set-definition
                (fn [_] (token-recipe-definition []))
                resolver/request-set-definition-path
                (fn [_] "unit/token-recipe-coverage-basic-ja.json")]
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"Analysis recipe cannot consume any provided input view"
         (resolver/resolve-request-set "token-recipe-coverage-basic-ja")))))

(deftest resolve-request-set-accepts-token-recipe-with-tokenizer-profile-test
  (with-redefs [resolver/read-request-set-definition
                (fn [_] (token-recipe-definition ["fixture-tokenizer-ja-v1"]))
                resolver/request-set-definition-path
                (fn [_] "unit/token-recipe-coverage-basic-ja.json")]
    (let [resolved (resolver/resolve-request-set "token-recipe-coverage-basic-ja")]
      (is (= (analysis-identity/request-set-id resolved)
             (get resolved "request_set_id"))))))
```

Append to `abc/test/abc/tools/request_set_fixture_test.clj` (the ns already requires `analysis-identity`, `files`, and `resolver`):

```clojure
;; Machine-check (ADR 0028 D6 follow-through): every committed request-set
;; definition's input_views agree with its referenced recipes'
;; supported_input_view_kinds — checked against the raw registry values, so
;; registry drift bites here even before anything resolves.
(deftest definitions-and-recipes-input-view-coverage-machine-check-test
  (let [labels (resolver/request-set-labels)]
    (is (seq labels))
    (doseq [label labels]
      (testing label
        (let [definition (resolver/read-request-set-definition label)
              recipes (mapv #(files/read-json
                              (str "data/analysis-recipes/" % ".json"))
                            (get definition "analysis_recipe_ids" []))]
          (is (nil? (analysis-identity/assert-input-view-coverage!
                     {:input-views (get definition "input_views")
                      :recipes recipes
                      :tokenizer-profile-ids (get definition
                                                  "tokenizer_profile_ids"
                                                  [])}))))))))
```

- [ ] **Step 2: Run tests to verify the resolver ones fail**

Run: `bin/kaocha --focus abc.tools.request-set-resolver-test --focus abc.tools.request-set-fixture-test`
Expected: `resolve-request-set-rejects-unconsumable-recipe-test` FAILS (no exception thrown — resolver doesn't check yet). The fixture-test machine-check PASSES already (it calls the Task 1 fn directly) — that is expected; it exists to pin future registry/definition edits.

- [ ] **Step 3: Wire the resolver**

In `abc/src/abc/tools/request_set_resolver.clj`, edit `resolve-semantic-registry-value` to carry the raw value through:

```clojure
(defn- resolve-semantic-registry-value
  [{:keys [semantic-id resolved-at hash-fn label-fn] :as opts}]
  (let [value (read-registry-value opts)
        value-hash (hash-fn value)
        registry-entry-hash (analysis-identity/hash-json-value
                             (semantic-registry-entry semantic-id
                                                      value-hash
                                                      resolved-at))]
    {:value value
     :hash value-hash
     :label (label-fn (assoc opts
                             :content-hash value-hash
                             :registry-entry-hash registry-entry-hash))}))
```

In `resolve-request-set`, insert the coverage check into the `let` bindings immediately after `resolved-tokenizer-profiles` is bound (before `resolved-pack-policy`), as a `_coverage` binding so it runs before the identity object is assembled:

```clojure
         _coverage (analysis-identity/assert-input-view-coverage!
                    {:input-views (get definition "input_views")
                     :recipes (mapv :value resolved-recipes)
                     :tokenizer-profile-ids (get definition
                                                 "tokenizer_profile_ids"
                                                 [])})
```

- [ ] **Step 4: Run the focused suites, then the full suite**

Run: `bin/kaocha --focus abc.tools.request-set-resolver-test --focus abc.tools.request-set-fixture-test`
Expected: PASS — including the pre-existing golden-file test (`resolve-request-set-golden-files-match-resolver-output-test`), which proves the `:value` addition changed no serialized output.

Run: `bin/kaocha`
Expected: PASS, 0 failures.

- [ ] **Step 5: Commit**

```bash
git add src/abc/tools/request_set_resolver.clj test/abc/tools/request_set_resolver_test.clj test/abc/tools/request_set_fixture_test.clj
git commit -m "feat(abc): enforce input-view coverage at request-set resolve time"
```

---

### Task 3: ADR 0028 status update

**Files:**
- Modify: `abc/docs/adr/0028-ruby-annotation-view.md` (the D6 bullet in Implementation Status, ~line 55-68; the Acceptance Criteria section, line 275+)

**Interfaces:**
- Consumes: the test/fn names landed in Tasks 1-2 (exact paths below).
- Produces: documentation only.

- [ ] **Step 1: Update the D6 Implementation Status bullet**

In the bullet beginning `- **D6 widened (2026-07-10)**:`, replace the final sentence:

```
A request set can now actually resolve an annotation
  input view; materializing annotation artifacts *per request set* remains
  future work (only the identity/schema layer accepts the view).
```

with:

```
A request set can now actually resolve an annotation
  input view, and `abc.tools.analysis-identity/assert-input-view-coverage!`
  (enforced by the resolver, pinned fixture-wide by
  `definitions-and-recipes-input-view-coverage-machine-check-test` in
  `test/abc/tools/request_set_fixture_test.clj`) machine-checks the
  recipe-`supported_input_view_kinds` ↔ request-set-views agreement:
  every referenced recipe must be able to consume at least one view the
  request set provides (tokenizer profiles supply the derived
  `token-stream-v1` view). The converse — flagging a declared view no
  recipe consumes — is deliberately not checked: views are consumed
  outside the recipe system too (the publication flow consumes the
  plaintext view, as in `full-corpus-publication-basic-ja`; the annotation
  materializer consumes annotation views directly), so dead-view detection
  is not decidable from the recipe registry. Materializing annotation
  artifacts *per request set* remains future work, as does widening
  `schemas/analysis-recipe.schema.json`'s `supported_input_view_kinds` enum
  so a recipe can declare annotation-view consumption — that widening
  belongs with the first annotation-consuming recipe, because the schema's
  `tokenizer_required` conditionals (plaintext-policy/newline coupling)
  need a design decision for annotation-only recipes.
```

- [ ] **Step 2: Update the Acceptance Criteria "Not yet built" bullet**

Replace:

```
- **Not yet built.** A tokenizer-backed analysis recipe fixture that states a
  supplantation policy and consumes the join. The join primitive exists
  (above); no recipe schema or fixture consumes it yet. Deferred to a later
  task.
```

with:

```
- **Not yet built.** A tokenizer-backed analysis recipe fixture that states a
  supplantation policy and consumes the join. The join primitive exists
  (above), and the recipe/request-set input-view coverage machine-check is
  in place (`abc.tools.analysis-identity/assert-input-view-coverage!`,
  `test/abc/tools/analysis_identity_test.clj`,
  `test/abc/tools/request_set_fixture_test.clj`) so such a recipe cannot be
  referenced by a request set that fails to feed it; no recipe schema or
  fixture consumes the join yet. Deferred to a later task.
```

- [ ] **Step 3: Run the gates**

Run (from `abc/`): `bin/kaocha --focus abc.tools.acceptance-criteria-lint-test`
Expected: PASS (heading case untouched; the new text adds `test/` executable paths).

Run (from `abc/`): `clojure -M:abc/validate-design-bundle`
Expected: `design bundle validation ok` (exit 0).

- [ ] **Step 4: Commit**

```bash
git add docs/adr/0028-ruby-annotation-view.md
git commit -m "docs(adr): record input-view coverage machine-check in ADR 0028 status"
```
