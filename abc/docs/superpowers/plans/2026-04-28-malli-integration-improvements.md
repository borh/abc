# Malli Integration Improvements Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make malli the single coherent in-process validation layer for the Clojure side of ABC: instrumented in CI, default-registered, used to humanize JSON-Schema errors readably, used to express the cross-event invariants that JSON Schema can't, and used as the decoder for ad-hoc CSV-cell parsers.

**Architecture:** A single foundation namespace `abc.tools.malli` owns one explicit `install!` function. `install!` requires the registry-owning namespaces (`abc.annotation.schema`, `abc.aozora`, `abc.tei`) in declared order, composes their `registry` values into one composite, publishes that composite as malli's default registry, then calls `mi/instrument!` exactly once — *after* every `m/=>` declaration is loaded. No registration-on-require, no instrumentation-on-load. The focused-test alias preamble calls `install!` once before tests run; full-suite kaocha calls it from a `:once` fixture in the foundation test namespace and in any test namespace that exercises the default registry. JSON Schema, SHACL, and Schematron remain the on-disk wire-format truth; malli sits behind them, not in front.

**Tech Stack:** Clojure 1.12.4, metosin/malli 0.20.1 (already pinned in `deps.edn` and `nix/clj-nix-deps.edn`), `m3.json-schema` (already in tree), kaocha (full suite), `clojure -M:abc/focused-test` (Nix-sandboxed CI gate).

---

## File Structure

**New files:**
- `src/abc/tools/malli.clj` — foundation. Owns `install!` (require + compose + set-default-registry! + instrument!), `cached-schema` (delay-backed JSON-Schema content cache, identity-stable), `cached-schema-hash` (delay-backed schema-bytes hash that delegates to `manifest/schema-hash` to preserve the on-disk contract), `humanize-validation-errors` (formats m3 error vectors into readable strings), `explain-or-throw!` (single malli-side validation gate that throws ex-info with humanized errors embedded in the message), and the design-bundle `:fn` schemas used by Task 6.
- `test/abc/tools/malli_test.clj` — exercises `install!` (idempotence, default-registry resolution, instrumentation activation), the schema cache, and humanization. Calls `(am/install!)` from a `:once` fixture.

**Modified source:**
- `deps.edn` — add `-Dmalli.registry/type=custom` to `:jvm-opts`.
- `nix/clj-nix-deps.edn` — same JVM opt; add `abc.tools.malli-test` to the focused-test namespace list; **prepend** `(require 'abc.tools.malli) ((requiring-resolve 'abc.tools.malli/install!))` to the `-e` form so installation runs after every `require` and before `test/run-tests`.
- `src/abc/tools/schema.clj` — add `validation-errors-humanized` that wraps `validation-errors` and `am/humanize-validation-errors`.
- `src/abc/tools/metadata_record.clj`, `src/abc/tools/person_record.clj` — call `am/cached-schema` directly; on failure throw ex-info with both `:errors` and `:errors-humanized`.
- `src/abc/tools/aozora_ingest.clj` — call `am/cached-schema-hash` (one cache, not three per-namespace delays).
- `src/abc/tools/validate_design_bundle.clj` — replace `manifest-input-errors`, `run-summary-errors`, `comparison-report-errors` with `am/explain-or-throw!` calls against the schemas registered in `abc.tools.malli`. `schema-hash-errors` and `parser-ir-schema-hash-errors` stay as plain Clojure (cross-file equality, not value shape).
- `src/abc/aozora.clj` — keep `(def registry …)` as a pure value (no side effects); attach `:decode/csv` properties on leaf schemas; introduce `csv-cell-transformer` (`mt/transformer {:name :csv}`); route `record-to-entities` leaf calls through `m/decode`. Fix the schema bug at line 462 where `merge-entities` declares `::db-entry` input but receives a sequence.
- `src/abc/annotation/schema.clj` — keep registry as a pure value (no side effects).
- `src/abc/annotation.clj` — keep existing `m/=>`.
- `src/abc/tei.clj` — keep `tei-quotation`'s `mx/defn`; either uncomment the `m/=>` block for `header`/`body`/`doc` only after auditing them against the current code, or delete the commented blocks.
- `test/abc/test_utils.cljc` — drop `registry` argument from `schema-valid` macro and `schema-validate` fn.
- Test files at every call site that passed an explicit registry: `test/abc/annotation_test.clj`, `test/abc/tei_test.clj`, `test/abc/aozora_test.clj`, `test/abc/annotation_schema_test.clj`, `test/abc/load_test.clj` (line 25).
- `test/abc/annotation_test.clj` — move `(dev/start! …)` from line 10 (top-level) into the existing `:once` fixture body; namespace becomes side-effect-free at load time.
- `test/abc/tools/validate_design_bundle_test.clj` — **rewrite** (not augment) `validate-run-summary-test`, `manifest-input-errors-test`, `comparison-report-errors-test` to drive `am/explain-or-throw!` and assert against `:errors-humanized` in `ex-data` and the embedded message.

**Out of scope (explicitly):**
- Replacing JSON Schema, SHACL, or Schematron files — they remain the on-disk wire-format truth.
- Rewriting `record-to-entities`'s projection logic from CSV column names to Clojure keys — only leaf-cell decoders move.
- Moving `abc.aozora`, `abc.tei`, `abc.annotation` into the focused-test sandbox (next-steps item).
- Replacing `manifest/schema-hash` — `am/cached-schema-hash` delegates to it.

---

## Conventions used in this plan

- **Test framework:** `clojure.test` + kaocha. Run a single var with `clojure -M:test:kaocha --focus 'abc.tools.malli-test/install!-is-idempotent'`. Run focused contract sandbox with `clojure -M:abc/focused-test`. Run full suite with `bin/kaocha`.
- **TDD rhythm:** every step pair is "write test → run and fail → implement → run and pass → commit". Steps annotate the exact command and expected output.
- **Commits:** one per task or per logical sub-step. Conventional-commit style: `feat:`, `refactor:`, `test:`, `chore:`. Author/email come from git config (do not override).
- **Time discipline:** there is exactly one moment when the project registry is composed and instrumentation runs — `abc.tools.malli/install!`. No registry-publishing on namespace require, no instrumentation-on-load. Tests that need the default registry call `install!` from a `:once` fixture; the focused-test alias calls it once at the top of its `-e` form.

---

## Task 1: Foundation — `abc.tools.malli/install!`

**Files:**
- Create: `src/abc/tools/malli.clj`
- Create: `test/abc/tools/malli_test.clj`
- Modify: `deps.edn`, `nix/clj-nix-deps.edn`

- [ ] **Step 1.1: Add the JVM opt that enables a custom default registry**

`malli.registry/set-default-registry!` is a no-op unless `-Dmalli.registry/type=custom` is set at JVM startup.

Edit `deps.edn` line 2:

```clojure
 :jvm-opts ["--add-opens" "java.base/java.util.concurrent=ALL-UNNAMED"
            "-Dmalli.registry/type=custom"]
```

Add the same opt to the `:abc/focused-test` alias map in `nix/clj-nix-deps.edn`:

```clojure
 :aliases
 {:abc/focused-test
  {:jvm-opts ["-Dmalli.registry/type=custom"]
   :main-opts [...existing...]}}
```

- [ ] **Step 1.2: Verify the JVM opt is picked up**

```bash
clojure -M:test -e "(println (System/getProperty \"malli.registry/type\"))"
```
Expected stdout: `custom`.

- [ ] **Step 1.3: Write the failing foundation tests**

Create `test/abc/tools/malli_test.clj`:

```clojure
(ns abc.tools.malli-test
  (:require [abc.tools.malli :as am]
            [abc.tools.schema :as schema]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [malli.core :as m]))

(use-fixtures :once (fn [f] (am/install!) (f)))

(deftest install!-is-idempotent
  (testing "calling install! twice produces an equal composite registry"
    (let [first-call (am/install!)
          second-call (am/install!)]
      (is (= first-call second-call)))))

(deftest install!-merges-project-registries
  (testing "annotation schema reachable through default registry"
    (is (m/validate :document/paragraphs
                    [{:paragraph/sentences
                      [{:sentence/annotated-text "x"
                        :sentence/text "x"}]}])))
  (testing "aozora schema reachable through default registry"
    (is (m/validate :abc.aozora.ndc/category "日本文学"))))

(deftest install!-instruments-m=>-contracts
  (require 'abc.aozora)
  ;; Re-install after requiring abc.aozora so the m/=> declarations on
  ;; that namespace are wrapped (instrument! only wraps contracts that
  ;; exist at the time of the call).
  (am/install!)
  (is (thrown-with-msg? Exception #":malli\.core/(invalid-input|invalid-output)"
        ((resolve 'abc.aozora/to-ndc) 42))))

(deftest cached-schema-returns-identical-value
  (let [first-read (am/cached-schema "schemas/manifest.schema.json")
        second-read (am/cached-schema "schemas/manifest.schema.json")]
    (is (identical? first-read second-read))))

(deftest humanize-validation-errors-returns-readable-strings
  (let [schema (am/cached-schema "schemas/person-record.schema.json")
        errs (schema/validation-errors schema {"person_id" "not-six-digits"})
        humanized (am/humanize-validation-errors errs)]
    (is (sequential? humanized))
    (is (every? string? humanized))
    (is (some #(re-find #"person_id" %) humanized))))
```

Run:
```bash
clojure -M:test:kaocha --focus 'abc.tools.malli-test'
```
Expected: FAIL — `Could not locate abc/tools/malli`.

- [ ] **Step 1.4: Implement the foundation namespace**

Create `src/abc/tools/malli.clj`:

```clojure
(ns abc.tools.malli
  "Single-place validation foundation. `install!` composes registries
  from the project's registry-owning namespaces, publishes the result
  as malli's default registry, and instruments every `m/=>` and
  `mx/defn` contract.

  Tests and the focused-test alias call `install!` exactly once, *after*
  every namespace that declares schemas has been loaded. Namespaces
  themselves stay side-effect-free at load time."
  (:require [abc.tools.json :as abc-json]
            [clojure.string :as string]
            [malli.core :as m]
            [malli.error :as me]
            [malli.instrument :as mi]
            [malli.registry :as mr]))

(def ^:private project-namespaces
  '[abc.annotation.schema abc.aozora abc.tei])

(defn- compose-project-registry []
  (reduce
   (fn [acc ns-sym]
     (require ns-sym)
     (let [v (some-> (resolve (symbol (name ns-sym) "registry")) deref)]
       (cond-> acc (map? v) (merge v))))
   {}
   project-namespaces))

;; Placeholder; Task 6 replaces this `def` with the design-bundle :fn
;; schemas. Defined here so `install!` is loadable on its own.
(def design-bundle-schemas {})

(defn install!
  "Idempotent. Requires the project's registry-owning namespaces in
  declared order, composes their `registry` values plus the
  design-bundle :fn schemas, publishes the composite as malli's default
  registry, then instruments every registered function schema. Returns
  the composite map."
  []
  (let [composite (merge (compose-project-registry) design-bundle-schemas)]
    (mr/set-default-registry!
     (mr/composite-registry (m/default-schemas) composite))
    (mi/instrument!)
    composite))

(let [cache (atom {})]
  (defn cached-schema
    "Read and parse the JSON Schema at `path` exactly once per JVM.
    Identity-stable: callers can compare with `identical?`."
    [path]
    (or (get @cache path)
        (let [v (abc-json/read-json-file path)]
          (swap! cache assoc path v)
          v)))

  (defn cached-schema-hash
    "Compute and cache the schema-bytes hash for `path`. Delegates to
    `abc.tools.manifest/schema-hash` so the on-disk hash contract is
    preserved exactly."
    [path]
    (or (get @cache [::hash path])
        (let [schema-hash-fn (requiring-resolve 'abc.tools.manifest/schema-hash)
              v (schema-hash-fn path)]
          (swap! cache assoc [::hash path] v)
          v))))

(defn- m3-leaf-errors
  "Walk an m3 error tree. m3 nests errors via `:errors`; leaves carry
  `:document-path`, `:schema-path`, and `:message`. Yields a flat seq
  of leaf maps (descending into `:errors` when present, ignoring
  intermediate composite-schema messages)."
  [node]
  (cond
    (sequential? node) (mapcat m3-leaf-errors node)
    (and (map? node) (seq (:errors node))) (mapcat m3-leaf-errors (:errors node))
    (map? node) [(select-keys node [:document-path :schema-path :message])]
    :else nil))

(defn humanize-validation-errors
  "Format an m3 error vector into a flat sequence of readable strings.
  Returns an empty vector when `errors` is nil or empty."
  [errors]
  (->> (m3-leaf-errors errors)
       (mapv (fn [{:keys [document-path message]}]
               (let [path (when (seq document-path)
                            (string/join "/" (map str document-path)))]
                 (cond
                   (and path message) (str path ": " message)
                   message message
                   path path
                   :else (pr-str document-path)))))))

(defn explain-or-throw!
  "Validate `value` against `schema-key` using malli's default registry.
  On failure, throws ex-info whose **message embeds the humanized
  errors** (so `thrown-with-msg?` regexes match) and whose ex-data
  carries `:errors-humanized` (vector of strings), `:label`, and the
  raw `:explanation`. Returns `:ok` on success."
  [schema-key value label]
  (if-let [explanation (m/explain schema-key value)]
    (let [humanized (->> (me/humanize explanation)
                         (tree-seq coll? seq)
                         (filter string?)
                         vec)]
      (throw (ex-info (str label " failed malli validation: "
                           (string/join "; " humanized))
                      {:label label
                       :errors-humanized humanized
                       :explanation explanation})))
    :ok))
```

Notes:
- m3's documented error shape (per `m3.json-schema` docstring) is `{:schema-path … :document-path … :message … :document … :schema … :errors […]}`. `m3-leaf-errors` walks the `:errors` tree and ignores composite-level messages, returning only leaves with `:document-path` + `:message`.
- `cached-schema-hash` deliberately delegates to `abc.tools.manifest/schema-hash` (the existing on-disk contract). Do not re-implement JCS canonicalization here.
- `design-bundle-schemas` is `{}` until Task 6 replaces the `def` body. `install!` works either way.

- [ ] **Step 1.5: Run the foundation tests**

```bash
clojure -M:test:kaocha --focus 'abc.tools.malli-test'
```

Expected: every test PASSES.
- `cached-schema-returns-identical-value` — PASS
- `install!-is-idempotent` — PASS
- `install!-merges-project-registries` — PASS (each `registry` var is required and merged)
- `humanize-validation-errors-returns-readable-strings` — PASS
- `install!-instruments-m=>-contracts` — PASS (the `m/=>` on `to-ndc` is wrapped after the second `install!` call)

If `humanize-…` fails, m3's actual error keys differ from the docstring. Inspect a sample at the REPL: `(m3.json-schema/validate (am/cached-schema "schemas/person-record.schema.json") {"person_id" "x"})` — adjust `m3-leaf-errors` to match.

- [ ] **Step 1.6: Commit**

```bash
git add src/abc/tools/malli.clj test/abc/tools/malli_test.clj deps.edn nix/clj-nix-deps.edn
git commit -m "$(cat <<'EOF'
feat: introduce abc.tools.malli/install! foundation

One explicit installer requires the project's registry-owning namespaces,
composes their registries, publishes the composite as malli's default,
and instruments every m/=> contract. No registration-on-require; no
instrumentation-on-load. Sets -Dmalli.registry/type=custom so
set-default-registry! is permitted.

🤖 Generated with [Claude Code](https://claude.com/claude-code)
EOF
)"
```

---

## Task 2: Wire `install!` into the focused-test sandbox

**Files:**
- Modify: `nix/clj-nix-deps.edn`

- [ ] **Step 2.1: Prepend `install!` to the focused-test `-e` form**

Edit `nix/clj-nix-deps.edn`. The current `-e` form starts with `(require '[clojure.test :as test] 'abc.annotation-schema-test ...)` and ends with `(let [{:keys [fail error]} (test/run-tests …)] (when … (System/exit 1)))`.

Replace with:

```clojure
"(require '[clojure.test :as test]
          'abc.tools.malli
          'abc.tools.malli-test
          'abc.annotation-schema-test
          'abc.text-test
          'abc.ndc-test
          'abc.tools.hash-test
          'abc.tools.jcs-test
          'abc.tools.schema-test
          'abc.tools.manifest-index-test
          'abc.tools.manifest-to-rdf-test
          'abc.tools.validate-design-bundle-test
          'abc.tools.materialize-import-test
          'abc.tools.shacl-test
          'abc.tools.tei-test
          'abc.tools.schematron-test
          'abc.tools.aozora-csv-test
          'abc.tools.metadata-record-test
          'abc.tools.person-record-test
          'abc.tools.aozora-ingest-test
          'abc.tools.tei-header-test)
 ((requiring-resolve 'abc.tools.malli/install!))
 (let [{:keys [fail error]} (test/run-tests
                             'abc.tools.malli-test
                             'abc.annotation-schema-test
                             'abc.text-test
                             'abc.ndc-test
                             'abc.tools.hash-test
                             'abc.tools.jcs-test
                             'abc.tools.schema-test
                             'abc.tools.manifest-index-test
                             'abc.tools.manifest-to-rdf-test
                             'abc.tools.validate-design-bundle-test
                             'abc.tools.materialize-import-test
                             'abc.tools.shacl-test
                             'abc.tools.tei-test
                             'abc.tools.schematron-test
                             'abc.tools.aozora-csv-test
                             'abc.tools.metadata-record-test
                             'abc.tools.person-record-test
                             'abc.tools.aozora-ingest-test
                             'abc.tools.tei-header-test)]
   (when (pos? (+ fail error)) (System/exit 1)))"
```

Three structural changes:
1. `'abc.tools.malli` is required (so `install!` is resolvable).
2. `'abc.tools.malli-test` joins the require list and the `run-tests` list.
3. `((requiring-resolve 'abc.tools.malli/install!))` runs *after* every `require` and *before* `test/run-tests`. This is the one moment when project registries are composed and `m/=>` contracts are wrapped.

- [ ] **Step 2.2: Run the focused-test suite**

```bash
clojure -M:abc/focused-test
```

Expected: every previously-passing test still passes; `abc.tools.malli-test` joins the green count.

- [ ] **Step 2.3: Commit**

```bash
git add nix/clj-nix-deps.edn
git commit -m "feat: install malli registry + instrumentation in focused-test sandbox"
```

---

## Task 3: Drop the `registry` argument from test call sites

**Files:**
- Modify: `test/abc/test_utils.cljc`
- Modify: `test/abc/annotation_test.clj`
- Modify: `test/abc/tei_test.clj`
- Modify: `test/abc/aozora_test.clj`
- Modify: `test/abc/annotation_schema_test.clj`
- Modify: `test/abc/load_test.clj`

`abc.annotation.schema`, `abc.aozora`, and `abc.tei` source files are NOT modified — they remain side-effect-free at load time. The default registry is composed by `install!`, which any test fixture that needs it calls.

- [ ] **Step 3.1: Add the install! fixture to test files that exercise the default registry**

For `test/abc/tei_test.clj`, `test/abc/aozora_test.clj`, `test/abc/annotation_schema_test.clj`:

Add to `:require`:
```clojure
[abc.tools.malli :as am]
```

Add:
```clojure
(use-fixtures :once (fn [f] (am/install!) (f)))
```

For `test/abc/load_test.clj` (already has `(use-fixtures :once db-fixture)`): compose so `install!` runs first.

```clojure
(use-fixtures :once
  (fn [f] (am/install!) (f))
  db-fixture)
```

For `test/abc/annotation_test.clj` (has an existing `fixture` fn that Task 4 also modifies): edit the `fixture` function body to call `(am/install!)` at the top:

```clojure
(defn fixture [f]
  (am/install!)
  ;; existing body unchanged at this step
  ;; (Task 4 adds dev/start! / dev/stop! around (f))
  (f))
```

Add `[abc.tools.malli :as am]` to the namespace's `:require`.

- [ ] **Step 3.2: Drop the registry argument from `schema-valid` and `schema-validate`**

Edit `test/abc/test_utils.cljc`:

```clojure
(ns abc.test-utils
  (:require [clojure.pprint :as pprint]
            [clojure.test :as t]
            [malli.core :as m]
            [malli.error :as me]))

(defmethod t/assert-expr 'schema-valid
  [msg [_ schema data]]
  `(let [is-valid?# (m/validate ~schema ~data)]
     (t/do-report {:actual   ~data
                   :expected (-> ~schema (m/explain ~data) (me/humanize))
                   :message  ~msg
                   :type     (if is-valid?# :pass :fail)})))

(defn schema-validate
  [s v]
  (let [r (me/humanize (m/explain s v))]
    (if (empty? r)
      true
      (do (pprint/pprint {:error r}) false))))
```

- [ ] **Step 3.3: Update every call site to drop the registry argument**

Files and exact lines:
- `test/abc/annotation_test.clj:43` — drop the `registry` arg
- `test/abc/annotation_test.clj:44` — drop `registry`
- `test/abc/annotation_test.clj:48` — drop `registry`
- `test/abc/annotation_test.clj:60` — drop `registry`
- `test/abc/annotation_test.clj:64` — drop `registry`
- `test/abc/annotation_schema_test.clj:23` — drop `annotation-schema/registry`
- `test/abc/aozora_test.clj:10-11` — collapse `[:schema {:registry registry} :abc.aozora/entity-map]` to `:abc.aozora/entity-map`
- `test/abc/tei_test.clj:30-35`, `:39-42` — drop the `(merge registry annotation-schema/registry)` arg
- `test/abc/load_test.clj:25` — drop `abc.aozora/registry`; the line becomes `(is (schema-valid :abc.aozora/db-entries *db*))`

- [ ] **Step 3.4: Run every affected test namespace**

```bash
clojure -M:test:kaocha --focus 'abc.annotation-test'
clojure -M:test:kaocha --focus 'abc.aozora-test'
clojure -M:test:kaocha --focus 'abc.tei-test'
clojure -M:test:kaocha --focus 'abc.annotation-schema-test'
clojure -M:test:kaocha --focus 'abc.load-test'
```

Expected: every previously-passing assertion still passes.

- [ ] **Step 3.5: Commit**

```bash
git add test/abc/test_utils.cljc test/abc/annotation_test.clj test/abc/aozora_test.clj test/abc/tei_test.clj test/abc/annotation_schema_test.clj test/abc/load_test.clj
git commit -m "refactor: route project schemas through malli default registry"
```

---

## Task 4: Move `(dev/start!)` out of namespace top-level

**Files:**
- Modify: `test/abc/annotation_test.clj`

`abc.annotation-test` currently runs `(dev/start! {:report (pretty/reporter)})` at namespace load (line 10). That's a side effect on namespace require, which is wrong: requiring the test ns from the REPL leaks watcher state, and the existing fixture pattern is the right place for it.

- [ ] **Step 4.1: Move `dev/start!` into the once-fixture**

Edit `test/abc/annotation_test.clj`. Remove the top-level `(dev/start! …)` (line 10). Update the `fixture` function — preserving the `am/install!` call added in Task 3 — to also wrap `dev/start!`/`dev/stop!`:

```clojure
(defn fixture [f]
  (am/install!)
  (dev/start! {:report (pretty/reporter)})
  (try (f)
       (finally (dev/stop!))))

(use-fixtures :once fixture)
```

- [ ] **Step 4.2: Run abc.annotation-test**

```bash
clojure -M:test:kaocha --focus 'abc.annotation-test'
```

Expected: every previously-passing assertion still passes.

- [ ] **Step 4.3: Verify no top-level `dev/start!` remains**

```bash
grep -nE '^\(dev/start' test/abc/annotation_test.clj
```

Expected: no output.

- [ ] **Step 4.4: Commit**

```bash
git add test/abc/annotation_test.clj
git commit -m "test: move malli.dev/start into once-fixture"
```

---

## Task 5: Cache schema reads and humanize JSON-Schema errors

**Files:**
- Modify: `src/abc/tools/schema.clj`
- Modify: `src/abc/tools/metadata_record.clj`
- Modify: `src/abc/tools/person_record.clj`
- Modify: `src/abc/tools/aozora_ingest.clj`
- Modify: `test/abc/tools/metadata_record_test.clj`
- Modify: `test/abc/tools/person_record_test.clj`

There is one schema cache: `am/cached-schema` (content) and `am/cached-schema-hash` (hash). No per-namespace `delay`s.

- [ ] **Step 5.1: Add `validation-errors-humanized` in `abc.tools.schema`**

Edit `src/abc/tools/schema.clj`. Add `[abc.tools.malli :as am]` to `:require`. Append:

```clojure
(defn validation-errors-humanized
  "Return [errors humanized-strings] for `value` against `schema`.
  Both nil/empty when `value` validates."
  [schema value]
  (let [errors (validation-errors schema value)]
    (if (seq errors)
      [errors (am/humanize-validation-errors errors)]
      [nil nil])))
```

- [ ] **Step 5.2: Write failing tests**

Add to `test/abc/tools/metadata_record_test.clj`:

```clojure
(ns ...
  (:require ...
            [abc.tools.malli :as am]))

(use-fixtures :once (fn [f] (am/install!) (f)))

(deftest validate!-humanizes-errors
  (try
    (metadata-record/validate! {"work" {} "contributors" []})
    (is false "validate! should have thrown")
    (catch clojure.lang.ExceptionInfo e
      (let [d (ex-data e)]
        (is (contains? d :errors))
        (is (contains? d :errors-humanized))
        (is (every? string? (:errors-humanized d)))))))

(deftest schema-read-is-cached
  (let [counter (atom 0)
        original abc.tools.json/read-json-file]
    (am/cached-schema "schemas/metadata-record.schema.json") ; prime
    (with-redefs [abc.tools.json/read-json-file
                  (fn [p]
                    (when (clojure.string/ends-with? (str p)
                                                     "metadata-record.schema.json")
                      (swap! counter inc))
                    (original p))]
      (metadata-record/validate!
       (abc.tools.files/read-json
        "examples/v0/example-work/metadata-record.json"))
      (metadata-record/validate!
       (abc.tools.files/read-json
        "examples/v0/example-work/metadata-record.json"))
      (is (zero? @counter)
          "no disk read after the cache is primed"))))
```

Add the analogous tests in `test/abc/tools/person_record_test.clj` (also add the `:once` install! fixture and `[abc.tools.malli :as am]` if not present).

Run:
```bash
clojure -M:test:kaocha --focus 'abc.tools.metadata-record-test'
clojure -M:test:kaocha --focus 'abc.tools.person-record-test'
```
Expected: FAIL — current `validate!` re-reads on every call and ex-data has only `:errors`.

- [ ] **Step 5.3: Rewrite `metadata-record/validate!`**

Edit `src/abc/tools/metadata_record.clj`. Add `[abc.tools.malli :as am]` to `:require`. Replace `validate!`:

```clojure
(defn validate!
  "Validate `record` against schemas/metadata-record.schema.json.
  Returns :ok on success; throws ex-info on failure with both :errors
  (raw m3 vector) and :errors-humanized (readable strings)."
  [record]
  (let [[errors humanized] (schema/validation-errors-humanized
                            (am/cached-schema schema-path) record)]
    (if (seq errors)
      (throw (ex-info "metadata-record validation failed"
                      {:errors errors
                       :errors-humanized humanized}))
      :ok)))
```

- [ ] **Step 5.4: Same change in `person_record.clj`**

Mirror in `src/abc/tools/person_record.clj`.

- [ ] **Step 5.5: Replace `(manifest/schema-hash …)` with `(am/cached-schema-hash …)` in `aozora_ingest.clj`**

Edit `src/abc/tools/aozora_ingest.clj`. Add `[abc.tools.malli :as am]` to `:require`. For each call site:
- `(manifest/schema-hash schema-path)` → `(am/cached-schema-hash schema-path)`
- `(manifest/schema-hash person-schema-path)` → `(am/cached-schema-hash person-schema-path)`

`am/cached-schema-hash` delegates to `manifest/schema-hash` internally, so output values are identical. Tests that compare hashes byte-for-byte (including the manifest-byte-identity tests in `aozora-ingest-test`) keep passing.

- [ ] **Step 5.6: Run all affected tests**

```bash
clojure -M:test:kaocha --focus 'abc.tools.metadata-record-test'
clojure -M:test:kaocha --focus 'abc.tools.person-record-test'
clojure -M:test:kaocha --focus 'abc.tools.aozora-ingest-test'
clojure -M:abc/focused-test
```
Expected: PASS.

- [ ] **Step 5.7: Commit**

```bash
git add src/abc/tools/schema.clj src/abc/tools/metadata_record.clj src/abc/tools/person_record.clj src/abc/tools/aozora_ingest.clj test/abc/tools/metadata_record_test.clj test/abc/tools/person_record_test.clj
git commit -m "refactor: cache schema reads and humanize JSON-Schema errors"
```

---

## Task 6: Replace ad-hoc `*-errors` validators with malli `:fn` schemas

**Files:**
- Modify: `src/abc/tools/malli.clj`
- Modify: `src/abc/tools/validate_design_bundle.clj`
- Modify: `test/abc/tools/validate_design_bundle_test.clj`

This task swaps three existing `defn`s — `manifest-input-errors`, `run-summary-errors`, `comparison-report-errors` — for malli `:fn` schemas plus `am/explain-or-throw!`. The existing tests that reference those `defn`s are **rewritten** (not augmented) to drive the malli pipeline. `schema-hash-errors` and `parser-ir-schema-hash-errors` stay as plain Clojure: they encode cross-file equality, not value shape.

- [ ] **Step 6.1: Replace the placeholder `design-bundle-schemas` in `abc.tools.malli`**

Edit `src/abc/tools/malli.clj`. The placeholder `(def design-bundle-schemas {})` from Task 1 is replaced in place:

```clojure
(def design-bundle-schemas
  {::sha256-hash
   [:re #"^sha256:[0-9a-f]{64}$"]

   ::manifest-inputs
   [:and
    [:map-of :string :any]
    [:fn {:error/message "manifest inputs missing required keys"}
     (fn [m]
       (every? #(contains? m %)
               ["producer" "producer_version" "work_id"
                "corpus_snapshot_hash" "work_content_hash"
                "parser_build_hash" "parser_config_hash"
                "parser_ir_schema_hash" "diagnostic_schema_hash"
                "warning_sidecar_hash" "run_summary_hash"
                "comparison_report_hash"]))]
    [:fn {:error/message "every *_hash key must be a sha256: hash"}
     (fn [m]
       (every? (fn [[k v]]
                 (or (not (string/ends-with? (str k) "_hash"))
                     (and (string? v)
                          (re-matches #"^sha256:[0-9a-f]{64}$" v))))
               m))]]

   ::run-summary-event
   [:map-of :string :any]

   ::run-summary-events
   [:and
    [:vector ::run-summary-event]
    [:fn {:error/message "run summary must contain exactly one run-start event"}
     (fn [es] (= 1 (count (filter #(= "run-start" (get % "event")) es))))]
    [:fn {:error/message "run summary must contain exactly one run-complete event"}
     (fn [es] (= 1 (count (filter #(= "run-complete" (get % "event")) es))))]
    [:fn {:error/message "run summary must start with run-start"}
     (fn [es] (= "run-start" (get (first es) "event")))]
    [:fn {:error/message "run summary must end with run-complete"}
     (fn [es] (= "run-complete" (get (last es) "event")))]
    [:fn {:error/message "every run summary event must include run_id"}
     (fn [es] (every? #(contains? % "run_id") es))]
    [:fn {:error/message "all run summary events must share one run_id"}
     (fn [es] (<= (count (set (keep #(get % "run_id") es))) 1))]]

   ::comparison-report
   [:and
    [:map-of :string :any]
    [:fn {:error/message "comparison report has unexpected report_schema"}
     (fn [r] (= "abc.ab-validator-comparison.v0" (get r "report_schema")))]
    [:fn {:error/message "comparison report must list parser_candidates"}
     (fn [r] (seq (get r "parser_candidates")))]]})
```

`install!` already merges `design-bundle-schemas` (it was a `{}` placeholder until now). After this edit, `(am/install!)` from any test fixture composes both project registries and design-bundle schemas.

- [ ] **Step 6.2: Rewrite the existing tests in `validate_design_bundle_test.clj`**

Edit `test/abc/tools/validate_design_bundle_test.clj`. Add `[abc.tools.malli :as am]` to `:require`. Add `(use-fixtures :once (fn [f] (am/install!) (f)))`.

**Replace** `validate-run-summary-test` (current lines 26–57) with:

```clojure
(deftest run-summary-malli-schema-test
  (testing "accepts start, work result, complete"
    (is (= :ok
           (am/explain-or-throw!
            ::am/run-summary-events
            [{"event" "run-start" "run_id" "r1"}
             {"event" "work-result" "run_id" "r1"}
             {"event" "run-complete" "run_id" "r1"}]
            "test"))))
  (testing "accepts start and complete without work results"
    (is (= :ok
           (am/explain-or-throw!
            ::am/run-summary-events
            [{"event" "run-start" "run_id" "r1"}
             {"event" "run-complete" "run_id" "r1"}]
            "test"))))
  (testing "rejects mismatched run ids"
    (let [thrown (try (am/explain-or-throw!
                       ::am/run-summary-events
                       [{"event" "run-start" "run_id" "r1"}
                        {"event" "work-result" "run_id" "r2"}
                        {"event" "run-complete" "run_id" "r1"}]
                       "test")
                      (catch clojure.lang.ExceptionInfo e e))]
      (is (some? thrown))
      (is (some #(re-find #"share one run_id" %)
                (:errors-humanized (ex-data thrown))))))
  (testing "rejects extra lifecycle events"
    (let [thrown (try (am/explain-or-throw!
                       ::am/run-summary-events
                       [{"event" "run-start" "run_id" "r1"}
                        {"event" "run-start" "run_id" "r1"}
                        {"event" "run-complete" "run_id" "r1"}
                        {"event" "run-complete" "run_id" "r1"}]
                       "test")
                      (catch clojure.lang.ExceptionInfo e e))
          humanized (:errors-humanized (ex-data thrown))]
      (is (some #(re-find #"exactly one run-start" %) humanized))
      (is (some #(re-find #"exactly one run-complete" %) humanized))))
  (testing "rejects missing run id"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"include run_id"
          (am/explain-or-throw!
           ::am/run-summary-events
           [{"event" "run-start" "run_id" "r1"}
            {"event" "work-result"}
            {"event" "run-complete" "run_id" "r1"}]
           "test")))))
```

**Replace** `manifest-input-errors-test` (current lines 59–93) with:

```clojure
(deftest manifest-inputs-malli-schema-test
  (testing "accepts complete manifest inputs"
    (is (= :ok
           (am/explain-or-throw!
            ::am/manifest-inputs
            {"producer" "ab-validator"
             "producer_version" "0.0.0"
             "work_id" "fixture"
             "corpus_snapshot_hash" (files/example-hash "00")
             "work_content_hash" (files/example-hash "01")
             "parser_build_hash" (files/example-hash "02")
             "parser_config_hash" (files/example-hash "03")
             "parser_ir_schema_hash" (files/example-hash "04")
             "diagnostic_schema_hash" (files/example-hash "08")
             "warning_sidecar_hash" (files/example-hash "05")
             "run_summary_hash" (files/example-hash "06")
             "comparison_report_hash" (files/example-hash "07")}
            "test"))))
  (testing "reports missing keys"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"missing required keys"
          (am/explain-or-throw! ::am/manifest-inputs
                                {"producer" "ab-validator"} "test"))))
  (testing "reports invalid hash values"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"sha256: hash"
          (am/explain-or-throw!
           ::am/manifest-inputs
           {"producer" "ab-validator"
            "producer_version" "0.0.0"
            "work_id" "fixture"
            "corpus_snapshot_hash" (files/example-hash "00")
            "work_content_hash" "nope"
            "parser_build_hash" (files/example-hash "02")
            "parser_config_hash" (files/example-hash "03")
            "parser_ir_schema_hash" (files/example-hash "04")
            "diagnostic_schema_hash" (files/example-hash "08")
            "warning_sidecar_hash" (files/example-hash "05")
            "run_summary_hash" (files/example-hash "06")
            "comparison_report_hash" (files/example-hash "07")}
           "test")))))
```

**Replace** `comparison-report-errors-test` (current lines 95–104) with:

```clojure
(deftest comparison-report-malli-schema-test
  (testing "accepts a well-formed report"
    (is (= :ok
           (am/explain-or-throw!
            ::am/comparison-report
            {"report_schema" "abc.ab-validator-comparison.v0"
             "parser_candidates" [{"parser_id" "fixture"}]}
            "test"))))
  (testing "rejects wrong report_schema and missing parser_candidates"
    (let [thrown (try (am/explain-or-throw!
                       ::am/comparison-report
                       {"report_schema" "wrong" "parser_candidates" []}
                       "test")
                      (catch clojure.lang.ExceptionInfo e e))
          humanized (:errors-humanized (ex-data thrown))]
      (is (some #(re-find #"unexpected report_schema" %) humanized))
      (is (some #(re-find #"parser_candidates" %) humanized)))))
```

`schema-hash-errors-test` (lines 106–115) and `parser-ir-schema-hash-errors-test` (lines 117–123) stay unchanged — those defns are not being replaced.

Run:
```bash
clojure -M:test:kaocha --focus 'abc.tools.validate-design-bundle-test'
```
Expected: the three rewritten tests PASS (they call `am/explain-or-throw!` against the schemas registered by `(am/install!)` in the `:once` fixture; nothing here depends on the deleted defns). The other tests in this namespace stay green. The legacy defns are deleted in 6.3.

- [ ] **Step 6.3: Replace the validators in `validate_design_bundle.clj`**

Read the current call site first:
```bash
grep -nE '(manifest-input-errors|run-summary-errors|comparison-report-errors)' src/abc/tools/validate_design_bundle.clj
```

Edit `src/abc/tools/validate_design_bundle.clj`. Add `[abc.tools.malli :as am]` to `:require`. Delete the three `defn` bodies. Update the `validate-ab-validator-output!` (or whichever function calls them — the `grep` above reveals the exact call sites) to use `am/explain-or-throw!`:

```clojure
(defn validate-ab-validator-output! []
  (let [manifest-inputs (files/read-json (files/path "examples" "ab-validator-output" "manifest-inputs.json"))]
    (am/explain-or-throw! ::am/manifest-inputs manifest-inputs
                          "ab-validator manifest inputs")
    (check-errors! (schema-hash-errors manifest-inputs)))
  (check-errors!
   (parser-ir-schema-hash-errors
    (files/read-json (files/path "examples" "ab-validator-output" "parser-ir.json"))))
  (am/explain-or-throw! ::am/run-summary-events
                        (files/read-json-lines (files/path "examples" "ab-validator-output" "run-summary.jsonl"))
                        "ab-validator run summary")
  (am/explain-or-throw! ::am/comparison-report
                        (files/read-json (files/path "examples" "ab-validator-output" "comparison-report.json"))
                        "ab-validator comparison report"))
```

Adjust against the actual structure revealed by `grep`. Wherever a `check-errors!` wraps one of the deleted defns, replace the wrap with the equivalent `am/explain-or-throw!` call.

- [ ] **Step 6.4: Run validate-design-bundle suite + CLI**

```bash
clojure -M:test:kaocha --focus 'abc.tools.validate-design-bundle-test'
clojure -M:abc/focused-test
clojure -M:abc/validate-design-bundle
```
Expected: PASS. The CLI exits 0 and logs `ok` for each step.

- [ ] **Step 6.5: Commit**

```bash
git add src/abc/tools/malli.clj src/abc/tools/validate_design_bundle.clj test/abc/tools/validate_design_bundle_test.clj
git commit -m "refactor: replace ad-hoc validators with malli :fn schemas"
```

---

## Task 7: Activate `m/=>` annotations under instrumentation

**Files:**
- Modify: `src/abc/aozora.clj`
- Modify: `src/abc/annotation.clj`
- Modify: `src/abc/tei.clj`
- Modify: `test/abc/tools/malli_test.clj`

`abc.aozora`, `abc.annotation`, and `abc.tei` are NOT in the focused-test namespace list today. The `install!-instruments-m=>-contracts` test in Task 1 already requires `abc.aozora` explicitly. This task audits the existing `m/=>` declarations against the current code and fixes anything that doesn't match.

- [ ] **Step 7.1: Confirm `instrument!-fires-m=>-contracts` passes**

```bash
clojure -M:test:kaocha --focus 'abc.tools.malli-test/install!-instruments-m=>-contracts'
```

If FAIL because `abc.aozora/to-ndc` accepts `42` without throwing, the schema doesn't match. Read the schema annotation:

```bash
grep -nA 3 '(m/=> to-ndc' src/abc/aozora.clj
```

If the input schema is `:any` instead of `:string`, tighten it.

- [ ] **Step 7.2: Audit each `m/=>` annotation against the body**

For each annotation, confirm signature + registry key still match the schema. Known suspects:

- `src/abc/aozora.clj:260` — `to-ndc`: schema `[:cat :string]` → `[:maybe ::NDC]`. Body returns `(set …)` or `nil`. Confirm.
- `src/abc/aozora.clj:416` — `record-to-entities`: schema `[:map-of :string :string]` → `::db-entry`. Body returns `{::work … ::person …}` then `(remove-nils …)`. Schema is sound.
- `src/abc/aozora.clj:462` — `merge-entities`: **schema is wrong.** Input declared `::db-entry`, but the body argument is a sequence of entries. Fix:

```clojure
(m/=> merge-entities
      [:=>
       [:cat [:sequential ::db-entry]]
       ::db-entries])
```

(With the default registry now active, the inner `[:schema {:registry registry} …]` wrappers can be dropped.)

- `src/abc/annotation.clj:499` — `parse-text`: schema input `:string`, output `:document`. Confirm `:document` exists in the annotation registry (it does, see `annotation/schema.clj` line 108).
- `src/abc/tei.clj:178` — `tei-tags`: schema `[:alt …]`. Body takes any tag and returns `{:tags tags}`. Loose; leave.

For each commented-out `#_(m/=> …)` in `tei.clj` (`header`, `tei-quotation`, `body`, `doc`):
- `tei-quotation`'s `mx/defn` is the source of truth — already active. Leave.
- `header`, `body`, `doc` — **delete** the commented blocks. They reference fields the body never sets (e.g., `:tei/channel-description` referenced at line 125 has no schema definition). Documentation-as-contract that doesn't compile is worse than no contract.

- [ ] **Step 7.3: Run tests with instrumentation active**

```bash
clojure -M:abc/focused-test
clojure -M:test:kaocha --focus 'abc.aozora-test'
clojure -M:test:kaocha --focus 'abc.annotation-test'
clojure -M:test:kaocha --focus 'abc.tei-test'
```

Expected: PASS. If a real bug surfaces under instrumentation (the schema didn't match the body), fix the body or the schema in the same task; do not silence the contract.

- [ ] **Step 7.4: Commit**

```bash
git add src/abc/aozora.clj src/abc/annotation.clj src/abc/tei.clj test/abc/tools/malli_test.clj
git commit -m "feat: activate m/=> contracts under instrumentation"
```

---

## Task 8: Express CSV-cell decoders as a malli transformer

**Files:**
- Modify: `src/abc/aozora.clj`
- Modify: `test/abc/aozora_test.clj`

Malli transformers dispatch decoders by **schema property** keyed `:decode/<transformer-name>`, NOT by registry keyword. The transformer carries only the name; each schema definition holds its own decoder as a property. This is the malli idiom and is what `mt/transformer` actually walks.

The schema describes the **decoded** value's runtime type, not the input string. So:
- A field that ends up as `LocalDate` (registry alias `::date`) gets `[:schema {:decode/csv to-date} ::date]` — properties on a wrapper that references the existing simple-schema.
- A `:boolean` field gets `[:boolean {:decode/csv flag-to-boolean}]` (inline property; `:boolean` already accepts the post-decode boolean value).
- A `:int` field gets `[:int {:min 0 :decode/csv to-integer}]`.
- A `[:set …]` field gets `[:set {:decode/csv to-ndc} …]` — inline on the existing inline `:set` schema.
- A `[:enum …]` field gets `[:enum {:decode/csv to-encoding} …]` — inline on the existing enum.

The current `(def registry …)` is at line 16, **before** `to-date` (185), `aozora-to-date` (213), `flag-to-boolean` (243), `to-ndc` (248), `to-encoding` (265), `to-integer` (153). Attaching `{:decode/csv to-date}` inside the current registry won't compile because those vars are unresolved. Step 8.0 moves the registry below the decoders.

Keep the scope narrow: only leaf decoders move; `record-to-entities`'s projection layout stays. If the migration grows beyond ~200 lines diff, stop and split.

- [ ] **Step 8.0: Move `(def registry …)` below the decoder fns**

Cut the entire `(def registry { …40 lines… })` form (currently lines 16–140). Paste it immediately *after* `to-encoding` (currently line 265–269), so all of `to-integer`, `to-uri`, `to-date`, `aozora-to-date`, `flag-to-boolean`, `to-ndc`, `to-encoding` are defined first. Don't change any contents yet.

The `(m/=> to-ndc …)` annotation at line 260 references `registry`. After the move, that annotation must come *after* the registry definition (so `registry` is resolved). Move the `(m/=> to-ndc …)` form too — past the registry — or rewrite the annotation to drop the explicit `[:schema {:registry registry} …]` wrapping (the default registry composed by `am/install!` resolves `::NDC` without it). Same for the `(m/=> record-to-entities …)` (line 416) and `(m/=> merge-entities …)` (line 462) annotations.

After the move, run:
```bash
clojure -M:test:kaocha --focus 'abc.aozora-test'
```
Expected: every previously-passing assertion still passes. The reorder is a pure code motion.

Commit this move on its own:
```bash
git add src/abc/aozora.clj
git commit -m "refactor: move abc.aozora registry below decoder fns"
```

- [ ] **Step 8.1: Write the failing transformer test**

Add to `test/abc/aozora_test.clj`:

```clojure
(deftest csv-cell-transformer-decodes-leaf-types
  (testing "date string → LocalDate via simple-date regex"
    (is (instance? java.time.LocalDate
                   (m/decode :abc.aozora/last-modified-date "2024-12-01"
                             aozora/csv-cell-transformer))))
  (testing "wareki → LocalDate"
    (is (instance? java.time.LocalDate
                   (m/decode :abc.aozora/first-published "1922（大正11）年7月"
                             aozora/csv-cell-transformer))))
  (testing "あり/なし → boolean"
    (is (true? (m/decode :abc.aozora/copyright-expired "あり"
                         aozora/csv-cell-transformer)))
    (is (false? (m/decode :abc.aozora/copyright-expired "なし"
                          aozora/csv-cell-transformer))))
  (testing "encoding string → canonical token"
    (is (= "SJIS" (m/decode :abc.aozora/encoding "ShiftJIS"
                            aozora/csv-cell-transformer))))
  (testing "NDC string → set"
    (let [out (m/decode :abc.aozora/NDC "NDC 913"
                        aozora/csv-cell-transformer)]
      (is (set? out))
      (is (= "日本文学" (-> out first :abc.aozora.ndc/category))))))
```

Run:
```bash
clojure -M:test:kaocha --focus 'abc.aozora-test/csv-cell-transformer-decodes-leaf-types'
```
Expected: FAIL — `csv-cell-transformer` doesn't exist.

- [ ] **Step 8.2: Attach `:decode/csv` properties to the leaf schemas**

Edit `src/abc/aozora.clj`. Add `[malli.transform :as mt]` to `:require`.

The schema describes the **decoded** runtime value. Apply these exact rewrites to the registry entries (and only these — leave the rest unchanged):

```clojure
;; Date-typed leaves: the registry has these as ::date references. Wrap
;; with [:schema {:decode/csv …} ::date] so the wrapper carries the
;; property while the validation type stays `LocalDate`.
::last-modified-date        [:schema {:decode/csv to-date} ::date]
::first-published           [:schema {:decode/csv aozora-to-date} ::date]
::aozora-publishing-date    [:schema {:decode/csv to-date} ::date]
::aozora-last-modified-date [:schema {:decode/csv to-date} ::date]
::date-of-birth             [:schema {:decode/csv aozora-to-date} ::date]
::date-of-death             [:schema {:decode/csv aozora-to-date} ::date]

;; Booleans: inline property; :boolean already accepts the decoded value.
::copyright-expired        [:boolean {:decode/csv flag-to-boolean}]
::person-copyright-expired [:boolean {:decode/csv flag-to-boolean}]

;; Encoding: existing schema is [:enum "SJIS" "EUC" "UTF-8"]; attach inline.
::encoding                 [:enum {:decode/csv to-encoding} "SJIS" "EUC" "UTF-8"]

;; NDC: existing schema is [:set [:map :abc.aozora.ndc/category …]].
;; Attach inline; the inline :map child stays the same.
::NDC                      [:set {:decode/csv to-ndc}
                            [:map :abc.aozora.ndc/category
                             [:abc.aozora.ndc/children {:optional true}]]]

;; Revision count: [:int {:min 0}] → add :decode/csv to the same props.
::revision-count           [:int {:min 0 :decode/csv to-integer}]
```

Append after the registry:

```clojure
(def csv-cell-transformer
  "Decoders fire when malli walks the schema with name :csv. Decoder
  functions live as :decode/csv properties on each leaf schema in the
  project registry."
  (mt/transformer {:name :csv}))
```

- [ ] **Step 8.3: Run the transformer test**

```bash
clojure -M:test:kaocha --focus 'abc.aozora-test/csv-cell-transformer-decodes-leaf-types'
```
Expected: PASS.

- [ ] **Step 8.4: Route `record-to-entities` leaf calls through `m/decode`**

Edit `src/abc/aozora.clj` `record-to-entities`. Replace each direct decoder call:

```clojure
::last-modified-date (to-date (g "テキストファイル最終更新日"))
```

With:

```clojure
::last-modified-date (m/decode :abc.aozora/last-modified-date
                               (g "テキストファイル最終更新日")
                               csv-cell-transformer)
```

Apply to: `::last-modified-date` (×2), `::first-published`, `::aozora-publishing-date`, `::aozora-last-modified-date`, `::date-of-birth`, `::date-of-death`, `::copyright-expired`, `::person-copyright-expired`, `::encoding` (×2), `::NDC`, `::revision-count` (×2).

- [ ] **Step 8.5: Run aozora-test**

```bash
clojure -M:test:kaocha --focus 'abc.aozora-test'
```
Expected: PASS. The `mg/generate` fixture still produces a valid entity map; the transformer changes don't affect generation.

- [ ] **Step 8.6: Verify byte-identity through end-to-end ingest**

```bash
clojure -M:abc/aozora-ingest -- \
  --zip references/aozorabunko/index_pages/list_person_all_extended_utf8.zip \
  --work-id 000127 \
  --output /tmp/abc-roundtrip-metadata-record.json
diff -u examples/v0/example-work/metadata-record.json /tmp/abc-roundtrip-metadata-record.json
```
Expected: empty diff.

- [ ] **Step 8.7: Run the full focused-test suite**

```bash
clojure -M:abc/focused-test
```
Expected: PASS.

- [ ] **Step 8.8: Commit**

```bash
git add src/abc/aozora.clj test/abc/aozora_test.clj
git commit -m "refactor: route CSV-cell decoders through malli :decode/csv transformer"
```

---

## Final verification

- [ ] **Step F.1: Run the complete focused-test suite**

```bash
clojure -M:abc/focused-test
```
Expected: PASS.

- [ ] **Step F.2: Run the full kaocha suite**

```bash
bin/kaocha
```
Expected: PASS (or no new failures relative to `main`).

- [ ] **Step F.3: Run the design-bundle CLI**

```bash
clojure -M:abc/validate-design-bundle
```
Expected: every step logs `ok`; exit 0.

- [ ] **Step F.4: Run `nix flake check`**

```bash
nix flake check
```
Expected: every check passes. Pre-existing sandbox limitations (e.g., `org.clojars.jules_gosnell/m3` availability) should fail the same way they did on `main` — no new failure modes introduced.

- [ ] **Step F.5: Push the branch and open a PR**

```bash
git push -u origin <branch>
gh pr create --title "Malli integration improvements" --body "$(cat <<'EOF'
## Summary
- Promotes malli to the in-process validation layer for the Clojure side of ABC via a single `abc.tools.malli/install!` entry point — no registration-on-require, no instrumentation-on-load.
- Default-registers the project schemas so call sites stop threading registries.
- Activates `m/=>` contracts under `malli.instrument` after every namespace is loaded.
- Replaces the ad-hoc `*-errors` family in `validate_design_bundle.clj` with malli `:fn` schemas (cross-event invariants are now expressible).
- Caches schema reads behind `am/cached-schema` and schema hashes behind `am/cached-schema-hash` (delegates to `manifest/schema-hash`).
- Humanizes JSON-Schema errors by formatting the m3 error vector directly.
- Routes Aozora CSV-cell decoders through a `malli.transform/transformer` keyed by `:decode/csv` schema properties.

## Test plan
- [ ] `clojure -M:abc/focused-test` passes
- [ ] `bin/kaocha` passes
- [ ] `clojure -M:abc/validate-design-bundle` passes
- [ ] `nix flake check` passes
- [ ] `clojure -M:abc/aozora-ingest -- --work-id 000127 …` produces a byte-identical metadata-record fixture

🤖 Generated with [Claude Code](https://claude.com/claude-code)
EOF
)"
```
