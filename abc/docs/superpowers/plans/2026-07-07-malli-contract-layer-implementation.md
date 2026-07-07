# Malli Contract Layer Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Move the two ABC-owned EDN registry validators onto shared Malli schema data while preserving their public APIs and entry-indexed error strings.

**Architecture:** Extend `abc.tools.malli` with shared scalar schemas, registry-entry schemas, and a tiny explanation helper. Then migrate `abc.tools.parser-evidence` and `abc.tools.aat-parser-ir-compat` one at a time so `index-errors`, `registry-errors`, `validate-index!`, and `validate-registry!` keep their existing observable behavior.

**Tech Stack:** Clojure 1.12.4, Malli 0.20.1, `clojure.test`/Kaocha, clj-kondo, cljfmt, Nix flake checks.

## Global Constraints

- JSON Schema remains the cross-language wire contract for Rust, Python, and ABC JSON artifacts.
- TEI ODD/RelaxNG/Schematron remain the TEI publication contract.
- SHACL remains the RDF graph contract.
- Rust serde/domain types remain the Rust in-process contract.
- Do not make Python or Rust invoke Clojure/Malli at runtime.
- Do not rotate `parser-ir.schema.json`, `manifest.schema.json`, or other v0 wire schemas.
- Keep duplicate-key checks as explicit collection-level functions.
- Keep `registry-errors` and `index-errors` as public APIs returning vectors of human-readable strings.
- Preserve entry-indexed error strings such as `entry 0 is missing :evidence_scope`.

---

## File Structure

**Modified source:**

- `abc/src/abc/tools/malli.clj` - owns shared scalar schemas, registry-entry schemas, example generator values, and `explanation-messages`.
- `abc/src/abc/tools/parser_evidence.clj` - uses Malli for parser evidence entry-local validation and keeps duplicate checks explicit.
- `abc/src/abc/tools/aat_parser_ir_compat.clj` - uses Malli for compatibility registry entry-local validation and keeps duplicate checks explicit.

**Modified tests:**

- `abc/test/abc/tools/malli_test.clj` - covers shared scalar schemas and generator viability.
- `abc/test/abc/tools/parser_evidence_test.clj` - proves parser evidence errors remain entry-indexed and adds generator-backed valid/invalid tests.
- `abc/test/abc/tools/validate_design_bundle_test.clj` - keeps AAT compatibility regex assertions and adds generator-backed cross-field rejection.

**No new generated artifacts:**

- This plan does not export JSON Schema.
- This plan does not modify files under `abc/schemas/`.

---

### Task 1: Shared Malli Contract Schemas

**Files:**
- Modify: `abc/src/abc/tools/malli.clj`
- Modify: `abc/test/abc/tools/malli_test.clj`

**Interfaces:**
- Produces schemas consumed by Tasks 2 and 3:
  - `::am/nonblank-string`
  - `::am/nullable-nonblank-string`
  - `::am/sha256-hash`
  - `::am/semver`
  - `::am/positive-int`
  - `::am/nonnegative-int`
  - `::am/workspace-logical-path`
  - `::am/concrete-adapter`
  - `::am/parser-evidence-entry`
  - `::am/aat-parser-ir-compat-entry`
  - `am/explanation-messages`
- Does not change current public behavior of `am/install!`.

- [ ] **Step 1: Write failing tests for shared schemas and explanation messages**

Add these tests to `abc/test/abc/tools/malli_test.clj`:

```clojure
(deftest contract-scalar-schemas-test
  (am/install!)
  (testing "shared scalar schemas accept valid values"
    (is (m/validate ::am/nonblank-string "abc"))
    (is (m/validate ::am/nullable-nonblank-string nil))
    (is (m/validate ::am/nullable-nonblank-string "abc"))
    (is (m/validate ::am/sha256-hash
                    "sha256:bf0910f5316efc2cd528f504c0cbd16816ca993e7ccb2b99122aab8a71359527"))
    (is (m/validate ::am/semver "0.2.3"))
    (is (m/validate ::am/positive-int 1))
    (is (m/validate ::am/nonnegative-int 0))
    (is (m/validate ::am/workspace-logical-path "abc/docs/report.md"))
    (is (m/validate ::am/concrete-adapter "aozora2html")))
  (testing "shared scalar schemas reject invalid values"
    (is (not (m/validate ::am/nonblank-string "")))
    (is (not (m/validate ::am/nullable-nonblank-string "")))
    (is (not (m/validate ::am/sha256-hash "sha256:not-real")))
    (is (not (m/validate ::am/semver "0.2")))
    (is (not (m/validate ::am/positive-int 0)))
    (is (not (m/validate ::am/nonnegative-int -1)))
    (is (not (m/validate ::am/workspace-logical-path "../abc/docs/report.md")))
    (is (not (m/validate ::am/workspace-logical-path "/abc/docs/report.md")))
    (is (not (m/validate ::am/concrete-adapter "*")))))

(deftest registry-entry-schemas-generate-valid-examples-test
  (am/install!)
  (testing "parser evidence schema has a valid generated example"
    (let [entry (mg/generate ::am/parser-evidence-entry)]
      (is (m/validate ::am/parser-evidence-entry entry))
      (is (string? (:evidence_id entry)))))
  (testing "compatibility schema has a valid generated example"
    (let [entry (mg/generate ::am/aat-parser-ir-compat-entry)]
      (is (m/validate ::am/aat-parser-ir-compat-entry entry))
      (is (= (get entry :aat_adapter)
             (get-in entry [:evidence_scope :adapter]))))))

(deftest explanation-messages-return-schema-messages-test
  (am/install!)
  (let [explanation (m/explain ::am/parser-evidence-entry
                               {:evidence_id ""
                                :evidence_class :unknown
                                :producer_component "ab-validator"
                                :logical_path "../bad.md"
                                :sha256 "sha256:not-real"
                                :status :citable
                                :summary "x"})
        messages (am/explanation-messages explanation)]
    (is (some #(re-find #"must be a non-empty string" %) messages))
    (is (some #(re-find #"must be workspace-relative" %) messages))
    (is (some #(re-find #"must be a sha256 hash" %) messages))))
```

Also add the required namespace import to the test `:require` form:

```clojure
[malli.generator :as mg]
```

- [ ] **Step 2: Run the Malli tests and confirm they fail**

Run:

```bash
cd abc
nix develop .#default --command bash -lc "./bin/kaocha --focus abc.tools.malli-test"
```

Expected: failure because the new schema keys and `explanation-messages` do not exist.

- [ ] **Step 3: Implement shared schemas in `abc.tools.malli`**

In `abc/src/abc/tools/malli.clj`, add these definitions above `design-bundle-schemas`:

```clojure
(def ^:private example-hash
  "sha256:bf0910f5316efc2cd528f504c0cbd16816ca993e7ccb2b99122aab8a71359527")

(def ^:private concrete-adapter-placeholders
  #{"*" "all" "any" "<any>" "adapter-neutral"})

(defn- nonblank-string? [value]
  (and (string? value)
       (not (string/blank? value))))

(defn- concrete-adapter? [value]
  (and (nonblank-string? value)
       (not (contains? concrete-adapter-placeholders
                       (string/lower-case value)))))

(defn- workspace-logical-path? [value]
  (and (nonblank-string? value)
       (not (string/starts-with? value "../"))
       (not (string/starts-with? value "/"))))

(def ^:private parser-evidence-example
  {:evidence_id "ab-validator/example"
   :evidence_class :conversion-compatibility
   :producer_component "ab-validator"
   :logical_path "ab-validator/docs/example.md"
   :current_external_path "../ab-validator/docs/example.md"
   :sha256 example-hash
   :status :citable
   :summary "Example evidence."})

(def ^:private compat-entry-example
  {:aat_version 1
   :aat_adapter "aozora2html"
   :aat_adapter_version "aozora2html-adapter 0.1.0 gem-3.0.1"
   :mapping_id "https://w3id.org/abc/mappings/aat-to-parser-ir/v1"
   :mapping_version "0.2.3"
   :mapping_hash example-hash
   :mapping_schema_hash example-hash
   :parser_ir_schema_id "https://w3id.org/abc/schemas/parser-ir.schema.json"
   :parser_ir_schema_hash example-hash
   :compatibility "lossy"
   :evidence_scope {:adapter "aozora2html"
                    :adapter_version "aozora2html-adapter 0.1.0 gem-3.0.1"
                    :corpus "fixture"
                    :evidence_type :conversion-audit
                    :files_scanned 2
                    :files_succeeded 1
                    :files_failed 1
                    :parser_ir_nodes 10
                    :divergence_records 3
                    :divergence_occurrences 4
                    :rules_total 5
                    :rules_emitted 2
                    :rules_missing 3
                    :unsupported_occurrences 0}})
```

Then replace the first `::sha256-hash` entry in `design-bundle-schemas` with a merge of a new `contract-schemas` map:

```clojure
(def contract-schemas
  {::nonblank-string
   [:fn {:error/message "must be a non-empty string"
         :gen/elements ["abc"]}
    nonblank-string?]

   ::nullable-nonblank-string
   [:maybe ::nonblank-string]

   ::sha256-hash
   [:re {:error/message "must be a sha256 hash"
         :gen/elements [example-hash]}
    #"^sha256:[0-9a-f]{64}$"]

   ::semver
   [:re {:error/message "must be semver"
         :gen/elements ["0.2.3"]}
    #"^[0-9]+\.[0-9]+\.[0-9]+$"]

   ::positive-int
   [:int {:error/message "must be a positive integer"
          :min 1
          :gen/elements [1]}]

   ::nonnegative-int
   [:int {:error/message "must be a non-negative integer"
          :min 0
          :gen/elements [0 1]}]

   ::workspace-logical-path
   [:fn {:error/message "must be workspace-relative and must not start with ../ or /"
         :gen/elements ["ab-validator/docs/example.md"]}
    workspace-logical-path?]

   ::concrete-adapter
   [:fn {:error/message "must name a concrete adapter"
         :gen/elements ["aozora2html"]}
    concrete-adapter?]

   ::parser-evidence-entry
   [:map {:gen/elements [parser-evidence-example]}
    [:evidence_id ::nonblank-string]
    [:evidence_class [:enum :conversion-compatibility :parser-selection :comparator-oracle]]
    [:producer_component ::nonblank-string]
    [:logical_path ::workspace-logical-path]
    [:current_external_path {:optional true} ::nullable-nonblank-string]
    [:sha256 ::sha256-hash]
    [:status [:enum :citable :provisional :superseded]]
    [:summary ::nonblank-string]]

   ::aat-parser-ir-evidence-scope
   [:multi {:dispatch :evidence_type}
    [:mapping-generation
     [:map
      [:adapter ::concrete-adapter]
      [:adapter_version {:optional true} ::nullable-nonblank-string]
      [:corpus ::nonblank-string]
      [:evidence_type [:= :mapping-generation]]
      [:files_scanned ::positive-int]
      [:files_with_unsupported ::nonnegative-int]
      [:generated_rules ::positive-int]]]
    [:conversion-audit
     [:map
      [:adapter ::concrete-adapter]
      [:adapter_version {:optional true} ::nullable-nonblank-string]
      [:corpus ::nonblank-string]
      [:evidence_type [:= :conversion-audit]]
      [:files_scanned ::positive-int]
      [:files_succeeded ::nonnegative-int]
      [:files_failed ::nonnegative-int]
      [:parser_ir_nodes ::nonnegative-int]
      [:divergence_records ::nonnegative-int]
      [:divergence_occurrences ::nonnegative-int]
      [:rules_total ::nonnegative-int]
      [:rules_emitted ::nonnegative-int]
      [:rules_missing ::nonnegative-int]
      [:unsupported_occurrences ::nonnegative-int]]]]

   ::aat-parser-ir-compat-entry
   [:and {:gen/elements [compat-entry-example]}
    [:map
     [:aat_version ::positive-int]
     [:aat_adapter ::concrete-adapter]
     [:aat_adapter_version {:optional true} ::nullable-nonblank-string]
     [:mapping_id ::nonblank-string]
     [:mapping_version ::semver]
     [:mapping_hash ::sha256-hash]
     [:mapping_schema_hash ::sha256-hash]
     [:parser_ir_schema_id ::nonblank-string]
     [:parser_ir_schema_hash ::sha256-hash]
     [:compatibility [:enum "lossy" "lossless"]]
     [:evidence_scope ::aat-parser-ir-evidence-scope]]
    [:fn {:error/message ":evidence_scope :adapter must equal :aat_adapter"}
     (fn [entry] (= (:aat_adapter entry)
                   (get-in entry [:evidence_scope :adapter])))]
    [:fn {:error/message ":evidence_scope :adapter_version must equal :aat_adapter_version"}
     (fn [entry] (= (:aat_adapter_version entry)
                   (get-in entry [:evidence_scope :adapter_version])))]
    [:fn {:error/message "files_scanned must equal files_succeeded plus files_failed"}
     (fn [entry]
       (let [scope (:evidence_scope entry)]
         (or (not= :conversion-audit (:evidence_type scope))
             (= (:files_scanned scope)
                (+ (:files_succeeded scope) (:files_failed scope))))))]
    [:fn {:error/message "rules_total must equal rules_emitted plus rules_missing"}
     (fn [entry]
       (let [scope (:evidence_scope entry)]
         (or (not= :conversion-audit (:evidence_type scope))
             (= (:rules_total scope)
                (+ (:rules_emitted scope) (:rules_missing scope))))))]]})
```

Remove the current `::sha256-hash` entry from `design-bundle-schemas`, because
`contract-schemas` now owns that key. Leave the existing
`::manifest-inputs`, `::run-summary-event`, `::run-summary-events`, and
`::comparison-report` entries unchanged.

Then update `install!` so the composed registry includes both maps:

```clojure
(defn install!
  "Idempotent. Requires the project's registry-owning namespaces in
  declared order, composes their `registry` values plus contract
  schemas and design-bundle :fn schemas, publishes the composite as
  malli's default registry, then instruments every registered function
  schema. Returns the composite map."
  []
  (let [composite (merge (compose-project-registry)
                         contract-schemas
                         design-bundle-schemas)]
    (mr/set-default-registry!
     (mr/composite-registry (m/default-schemas) composite))
    (mi/instrument!)
    composite))
```

Finally add this helper below `humanize-validation-errors`:

```clojure
(defn explanation-messages
  "Return stable message strings from a Malli explanation. Prefer explicit
  :error/message values from schemas; fall back to the path when a Malli
  primitive emits no custom message."
  [explanation]
  (->> (:errors explanation)
       (mapv (fn [{:keys [message path in]}]
               (cond
                 message message
                 (seq in) (str (string/join " " (map str in)) " is invalid")
                 (seq path) (str (string/join " " (map str path)) " is invalid")
                 :else "value is invalid")))))
```

- [ ] **Step 4: Run focused Malli tests**

Run:

```bash
cd abc
nix develop .#default --command bash -lc "./bin/kaocha --focus abc.tools.malli-test"
```

Expected: pass.

- [ ] **Step 5: Format and commit**

Run:

```bash
cd abc
nix shell nixpkgs#cljfmt -c cljfmt fix src/abc/tools/malli.clj test/abc/tools/malli_test.clj
nix develop .#default --command bash -lc "./bin/kaocha --focus abc.tools.malli-test"
git add src/abc/tools/malli.clj test/abc/tools/malli_test.clj
git commit -m "feat(abc): add shared malli contract schemas"
```

Expected: tests pass and a commit is created.

---

### Task 2: Parser Evidence Validator Migration

**Files:**
- Modify: `abc/src/abc/tools/parser_evidence.clj`
- Modify: `abc/test/abc/tools/parser_evidence_test.clj`

**Interfaces:**
- Consumes from Task 1:
  - `::am/parser-evidence-entry`
  - `am/explanation-messages`
- Preserves:
  - `index-errors [index] -> vector<string>`
  - `validate-index! [index] -> :ok or throws ex-info with `:errors`
  - `load-index [] -> valid index map`

- [ ] **Step 1: Add failing tests for nullable path and generator-backed validation**

In `abc/test/abc/tools/parser_evidence_test.clj`, extend the `:require` form:

```clojure
[abc.tools.malli :as am]
[malli.core :as m]
[malli.generator :as mg]
```

Add these tests after `parser-evidence-index-validation-test`:

```clojure
(deftest parser-evidence-nullable-external-path-test
  (testing "current_external_path may be absent or nil but not blank"
    (is (empty? (parser-evidence/index-errors
                 {:entries [(dissoc valid-entry :current_external_path)]})))
    (is (empty? (parser-evidence/index-errors
                 {:entries [(assoc valid-entry :current_external_path nil)]})))
    (is (has-error? #":current_external_path must be null or a non-empty string"
                    (parser-evidence/index-errors
                     {:entries [(assoc valid-entry :current_external_path "")]})))))

(deftest parser-evidence-generator-backed-validation-test
  (am/install!)
  (testing "generated parser evidence entries pass the public validator"
    (let [entry (mg/generate ::am/parser-evidence-entry)]
      (is (m/validate ::am/parser-evidence-entry entry))
      (is (empty? (parser-evidence/index-errors {:entries [entry]})))))
  (testing "mutating a generated entry to violate a scalar contract is rejected"
    (let [entry (mg/generate ::am/parser-evidence-entry)
          invalid-entry (assoc entry :sha256 "sha256:not-real")]
      (is (has-error? #":sha256 must be a sha256 hash"
                      (parser-evidence/index-errors
                       {:entries [invalid-entry]}))))))
```

- [ ] **Step 2: Run parser evidence tests and confirm the generator test fails**

Run:

```bash
cd abc
nix develop .#default --command bash -lc "./bin/kaocha --focus abc.tools.parser-evidence-test"
```

Expected: failure until `parser_evidence.clj` uses the new Malli schema and the test imports are available.

- [ ] **Step 3: Replace entry-local predicate validation with Malli**

In `abc/src/abc/tools/parser_evidence.clj`, update the namespace requires:

```clojure
(ns abc.tools.parser-evidence
  (:require [abc.tools.files :as files]
            [abc.tools.malli :as am]
            [clojure.edn :as edn]
            [clojure.string :as string]
            [malli.core :as m]))
```

Remove these private predicate definitions from `parser_evidence.clj`:

- `sha256-hash?`
- `nonblank-string?`
- `logical-path?`

Keep `required-entry-keys` and `missing-key-errors` so the existing missing-key strings stay exactly entry-indexed.

Replace `entry-errors` with:

```clojure
(defn- parser-evidence-malli-errors
  [idx entry]
  (if-let [explanation (m/explain ::am/parser-evidence-entry entry)]
    (mapv #(str "Parser evidence index entry " idx " " %)
          (am/explanation-messages explanation))
    []))

(defn- entry-errors
  [idx entry]
  (if-not (map? entry)
    [(str "Parser evidence index entry " idx " must be a map")]
    (vec
     (concat
      (missing-key-errors idx entry)
      (parser-evidence-malli-errors idx entry)))))
```

The `missing-key-errors` call remains before Malli errors so tests that assert `entry 0 is missing :sha256` continue to pass.

- [ ] **Step 4: Run parser evidence tests**

Run:

```bash
cd abc
nix develop .#default --command bash -lc "./bin/kaocha --focus abc.tools.parser-evidence-test"
```

Expected: pass.

- [ ] **Step 5: Run focused validation bundle regex coverage**

Run:

```bash
cd abc
nix develop .#default --command bash -lc "./bin/kaocha --focus abc.tools.validate-design-bundle-test/aat-parser-ir-registry-validation-test"
```

Expected: pass; this confirms Task 2 did not disturb the compatibility registry tests.

- [ ] **Step 6: Format and commit**

Run:

```bash
cd abc
nix shell nixpkgs#cljfmt -c cljfmt fix src/abc/tools/parser_evidence.clj test/abc/tools/parser_evidence_test.clj
nix develop .#default --command bash -lc "./bin/kaocha --focus abc.tools.parser-evidence-test"
git add src/abc/tools/parser_evidence.clj test/abc/tools/parser_evidence_test.clj
git commit -m "refactor(abc): validate parser evidence with malli"
```

Expected: tests pass and a commit is created.

---

### Task 3: AAT Parser-IR Compatibility Validator Migration

**Files:**
- Modify: `abc/src/abc/tools/aat_parser_ir_compat.clj`
- Modify: `abc/test/abc/tools/validate_design_bundle_test.clj`

**Interfaces:**
- Consumes from Task 1:
  - `::am/aat-parser-ir-compat-entry`
  - `am/explanation-messages`
- Preserves:
  - `registry-errors [registry] -> vector<string>`
  - `validate-registry! [registry] -> :ok or throws ex-info with `:errors`
  - `load-registry [] -> valid registry map`
  - `admission-report [registry candidates] -> existing report map shape`

- [ ] **Step 1: Add generator-backed compatibility tests**

In `abc/test/abc/tools/validate_design_bundle_test.clj`, extend the `:require` form with:

```clojure
[abc.tools.malli :as am]
[malli.core :as m]
[malli.generator :as mg]
```

Add this test after `aat-parser-ir-registry-validation-test`:

```clojure
(deftest aat-parser-ir-registry-generator-backed-validation-test
  (am/install!)
  (testing "generated compatibility entries pass the public validator"
    (let [entry (mg/generate ::am/aat-parser-ir-compat-entry)]
      (is (m/validate ::am/aat-parser-ir-compat-entry entry))
      (is (empty? (compat/registry-errors {:entries [entry]})))))
  (testing "mutating a generated entry to violate conversion file counts is rejected"
    (let [entry (mg/generate ::am/aat-parser-ir-compat-entry)
          invalid-entry (assoc-in entry [:evidence_scope :files_succeeded] 99)]
      (is (has-error? #"files_scanned must equal files_succeeded plus files_failed"
                      (compat/registry-errors
                       {:entries [invalid-entry]}))))))
```

- [ ] **Step 2: Run the compatibility registry tests and confirm failure**

Run:

```bash
cd abc
nix develop .#default --command bash -lc "./bin/kaocha --focus abc.tools.validate-design-bundle-test/aat-parser-ir-registry-generator-backed-validation-test"
```

Expected: failure until `aat_parser_ir_compat.clj` delegates entry-local validation to Malli.

- [ ] **Step 3: Replace compatibility entry-local predicate validation with Malli**

In `abc/src/abc/tools/aat_parser_ir_compat.clj`, update the namespace requires:

```clojure
(ns abc.tools.aat-parser-ir-compat
  (:require [abc.tools.files :as files]
            [abc.tools.malli :as am]
            [clojure.edn :as edn]
            [clojure.string :as string]
            [clojure.tools.cli :as cli]
            [malli.core :as m]))
```

Remove these private predicate and constant definitions once they have no
callers:

- `evidence-types`
- `hash-keys`
- `concrete-adapter-placeholders`
- `sha256-hash?`
- `nonblank-string?`
- `concrete-adapter?`
- `nonnegative-int?`
- `positive-int?`

Keep these vectors and constants:

- `registry-path`
- `match-keys`
- `required-entry-keys`
- `required-evidence-scope-common-keys`
- `required-mapping-generation-evidence-keys`
- `required-conversion-audit-evidence-keys`

Those vectors continue to drive entry-indexed missing-key errors. Keeping them preserves message text such as `:evidence_scope is missing :evidence_type`.

Replace `evidence-scope-errors` and `entry-errors` with:

```clojure
(defn- missing-evidence-scope-key-errors
  [idx entry]
  (let [scope (:evidence_scope entry)]
    (if-not (map? scope)
      [(str "AAT parser-IR compatibility registry entry " idx
            " :evidence_scope must be a map")]
      (let [evidence-type (:evidence_type scope)
            mode-required-keys (case evidence-type
                                 :mapping-generation required-mapping-generation-evidence-keys
                                 :conversion-audit required-conversion-audit-evidence-keys
                                 [])]
        (->> (concat required-evidence-scope-common-keys mode-required-keys)
             (remove #(contains? scope %))
             (mapv #(str "AAT parser-IR compatibility registry entry " idx
                         " :evidence_scope is missing " %)))))))

(defn- compatibility-malli-errors
  [idx entry]
  (if-let [explanation (m/explain ::am/aat-parser-ir-compat-entry entry)]
    (mapv #(str "AAT parser-IR compatibility registry entry " idx " " %)
          (am/explanation-messages explanation))
    []))

(defn- entry-errors
  [idx entry]
  (if-not (map? entry)
    [(str "AAT parser-IR compatibility registry entry " idx
          " must be a map")]
    (vec
     (concat
      (missing-key-errors idx entry)
      (when (contains? entry :evidence_scope)
        (missing-evidence-scope-key-errors idx entry))
      (compatibility-malli-errors idx entry)))))
```

The order is intentional:

1. top-level missing-key strings,
2. nested evidence-scope missing-key strings,
3. Malli scalar/enum/cross-field strings,
4. duplicate-key strings from `duplicate-key-errors`.

- [ ] **Step 4: Run compatibility tests**

Run:

```bash
cd abc
nix develop .#default --command bash -lc "./bin/kaocha --focus abc.tools.validate-design-bundle-test/aat-parser-ir-registry-validation-test --focus abc.tools.validate-design-bundle-test/aat-parser-ir-registry-generator-backed-validation-test"
```

Expected: pass. The existing `has-error?` regexes remain covered.

- [ ] **Step 5: Run parser evidence tests again**

Run:

```bash
cd abc
nix develop .#default --command bash -lc "./bin/kaocha --focus abc.tools.parser-evidence-test"
```

Expected: pass.

- [ ] **Step 6: Format and commit**

Run:

```bash
cd abc
nix shell nixpkgs#cljfmt -c cljfmt fix src/abc/tools/aat_parser_ir_compat.clj test/abc/tools/validate_design_bundle_test.clj
nix develop .#default --command bash -lc "./bin/kaocha --focus abc.tools.validate-design-bundle-test/aat-parser-ir-registry-validation-test --focus abc.tools.validate-design-bundle-test/aat-parser-ir-registry-generator-backed-validation-test"
git add src/abc/tools/aat_parser_ir_compat.clj test/abc/tools/validate_design_bundle_test.clj
git commit -m "refactor(abc): validate compatibility registry with malli"
```

Expected: tests pass and a commit is created.

---

### Task 4: Full Verification and Spec Status

**Files:**
- Modify: `abc/docs/superpowers/specs/2026-07-07-malli-contract-layer-design.md`

**Interfaces:**
- Consumes completed Tasks 1-3.
- Produces an accepted spec status and verified green gates.

- [ ] **Step 1: Mark the design accepted**

In `abc/docs/superpowers/specs/2026-07-07-malli-contract-layer-design.md`, change:

```markdown
Status: Draft
```

to:

```markdown
Status: Accepted
```

- [ ] **Step 2: Run focused Clojure tests**

Run:

```bash
cd abc
nix develop .#default --command bash -lc "./bin/kaocha --focus abc.tools.malli-test --focus abc.tools.parser-evidence-test --focus abc.tools.validate-design-bundle-test/aat-parser-ir-registry-validation-test --focus abc.tools.validate-design-bundle-test/aat-parser-ir-registry-generator-backed-validation-test"
```

Expected: pass.

- [ ] **Step 3: Run ABC quality gates**

Run:

```bash
cd abc
./bin/lint-active
nix run .#validate-design-bundle
nix flake check --print-build-logs
```

Expected: all pass.

- [ ] **Step 4: Run monorepo quality gates**

Run:

```bash
just python-quality
just nix-format-check
just validate-migration
git diff --check
```

Expected: all pass.

- [ ] **Step 5: Confirm no schema hashes rotated**

Run:

```bash
git diff --name-only HEAD~3..HEAD -- abc/schemas abc/schemas/schema-contracts.json
```

Expected: no output.

If this command prints a schema path, stop and inspect before committing. This implementation slice must not modify `abc/schemas/`.

- [ ] **Step 6: Commit final status update**

Run:

```bash
git add abc/docs/superpowers/specs/2026-07-07-malli-contract-layer-design.md
git commit -m "docs(abc): accept malli contract layer"
```

Expected: a commit is created.

---

## Final Verification

After all tasks are complete, run:

```bash
git status --short
git log --oneline -5
```

Expected:

- working tree is clean
- recent commits include:
  - `feat(abc): add shared malli contract schemas`
  - `refactor(abc): validate parser evidence with malli`
  - `refactor(abc): validate compatibility registry with malli`
  - `docs(abc): accept malli contract layer`

## Self-Review

- **Spec coverage:** Tasks 1-3 implement the approved first migration slice: shared Malli schemas plus the two EDN validators. Task 4 verifies the non-goals by checking no schema artifacts changed.
- **Placeholder scan:** No unresolved placeholders.
- **Type consistency:** The plan uses `::am/parser-evidence-entry`, `::am/aat-parser-ir-compat-entry`, and `am/explanation-messages` consistently across source and tests.
- **Behavior preservation:** Existing public validator functions and regex-based error assertions remain in place; Malli is introduced behind the existing public APIs.
