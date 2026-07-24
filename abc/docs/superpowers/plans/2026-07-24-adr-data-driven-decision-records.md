# Data-Driven Decision Records Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Replace the ADR Markdown grammar with a single authoritative `docs/adr/decisions.edn`, retire the old parser/grammar/relations-sidecar entirely, and re-point every consumer (governance CLI, decision graph, architecture metadata, docs) at the data.

**Architecture:** A new namespace `abc.tools.decisions` owns a strict EDN loading boundary, a Malli shape schema (per the Change E convention: namespace-owned schemas composed with `abc.tools.malli` scalars), and corpus-level semantic checks as plain queries. A branch-only extractor uses the *old* parser to emit `decisions.edn`; a branch-only equivalence test proves complete-fact-map fidelity; both are deleted in the atomic cutover task that also strips/renames the 42 narrative files and deletes `abc.tools.adr`.

**Tech Stack:** Clojure (deps.edn), Malli, babashka.fs, kaocha (`clojure -M:test:kaocha -m kaocha.runner`, or `bin/kaocha` if present — verify at Task 1), Nix flake checks.

**Spec:** `abc/docs/superpowers/specs/2026-07-24-adr-data-driven-decision-records-design.md` — read it before starting. All work happens inside `abc/` (paths below are relative to `abc/` unless prefixed `../`).

## Global Constraints

- Total retirement: at merge, none of these exist: `src/abc/tools/adr.clj`, `test/abc/tools/adr_test.clj`, `docs/adr/adr-relations.edn`, `fixtures/adr-governance-invalid/`, number-prefixed `docs/adr/NNNN-*.md` files, the extractor script, the equivalence test, `nix/adr-family-clean.jq` (already dead — no consumer), `data/evidence-higher-order-calls/adr-validate-repository-star.edn` (documents a deleted higher-order call).
- The governance CLI contract is preserved: alias `:abc/adr-governance`, `--repo-root` option, exit 0/1/2, problem maps with `:kind`/`:file`/`:message`, `ADR-LINT` stderr lines, `"ADR governance valid"` on success.
- Statuses: `:draft :proposed :accepted :superseded :withdrawn`. Validation scopes: `:structural :fixture :smoke-corpus :full-corpus :operational`. Release authorities: `:none :development :publication`.
- Lifecycle relation types (closed): `:supersedes :amends :depends-on`. Annotation types (open keywords): currently `:restates-hard-rule :schema-hash-cascade :harness-for :extends`.
- `:accepted`/`:validation-scope`/`:release-authority`: required iff `:status :accepted`; also permitted when `:superseded`; forbidden otherwise.
- Claim `:kind` and non-empty `:evidence`: required for `:accepted` and `:superseded` records; optional for `:draft`/`:proposed`/`:withdrawn`.
- Frozen legacy-number set (literal in schema): `#{1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 37 38 39 40 41 42 43}` (36 was never used).
- Evidence roots: `test/` `fixtures/` `nix/` `docs/evidence/external/`; reuse `abc.tools.path-containment` unchanged.
- Slug grammar: `^[a-z0-9]+(?:-[a-z0-9]+)*$`.
- Frozen prose (ADR narrative bodies, historical reports/specs) keeps its `ADR NNNN` mentions; only *active* docs and machine consumers are re-pointed.
- Commit after every green step; conventional-commit style matching repo history (`feat(adr):`, `refactor(adr):`, `docs(adr):`, `test(adr):`).

---

### Task 1: Branch-only extractor and committed `decisions.edn`

**Files:**
- Create: `dev/migrate_decisions.clj` (branch-only; deleted in Task 6)
- Create: `docs/adr/decisions.edn`
- Create: `test/abc/tools/decisions_migration_test.clj` (branch-only; deleted in Task 6)

**Interfaces:**
- Consumes: `abc.tools.adr/parse-all` (old parser), `docs/adr/adr-relations.edn`.
- Produces: `docs/adr/decisions.edn` with shape `{:decisions [record …]}` where record =
  `{:slug string, :legacy-number int, :title string, :status keyword, :date "YYYY-MM-DD", :accepted "YYYY-MM-DD"?, :validation-scope keyword?, :release-authority keyword?, :source string, :topics [keyword …], :relations [rel …], :claims [claim …]}`;
  rel = `{:class :lifecycle|:annotation, :type keyword, :to slug, :scope string?, :note string?}`;
  claim = `{:id :cN, :kind keyword?, :statement string, :evidence [string …]?}`.
  Later tasks rely on these exact keys.

- [ ] **Step 1: Verify the test runner command**

Run: `ls bin/ && cat tests.edn | head -20`
Use whatever kaocha entry the repo uses (expect `clojure -M:test:kaocha -m kaocha.runner` to work; if `bin/kaocha` exists, use it). Referred to below as `KAOCHA`.

- [ ] **Step 2: Write the failing equivalence test**

`test/abc/tools/decisions_migration_test.clj` — complete-fact-map equivalence. The only normalizations: number→slug, inverse-edge removal, `:cN` assignment for headerless criteria, claim-header-prefix stripping, `:topics` ignored (new, additive).

```clojure
(ns abc.tools.decisions-migration-test
  "BRANCH-ONLY: proves decisions.edn ≡ the legacy Markdown corpus.
   Deleted together with abc.tools.adr in the cutover task."
  (:require [abc.tools.adr :as adr]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]))

(def ^:private adr-dir "docs/adr")

(defn- slug-of [filename] (subs filename 5 (- (count filename) 3)))

(def ^:private claim-prefix
  #"^\*\*ADR-[0-9]{4}-C[1-9][0-9]* — [a-z]+(?:-[a-z]+)*:\*\*\s*")

(defn- legacy-claims [{:keys [criteria]}]
  (vec
   (map-indexed
    (fn [i {:keys [body claim-id claim-kind]}]
      (let [tokens (->> (re-seq #"`([^`]+)`" body)
                        (map second)
                        (filterv (fn [t]
                                   (some #(str/starts-with? t %)
                                         ["test/" "fixtures/" "nix/"
                                          "docs/evidence/external/"]))))]
        ;; "ADR-0043-C1" -> :c1 (digits start after "ADR-NNNN-C", index 10)
        (cond-> {:id (if claim-id
                       (keyword (str "c" (subs claim-id 10)))
                       (keyword (str "c" (inc i))))
                 :statement (str/replace body claim-prefix "")}
          claim-kind (assoc :kind claim-kind)
          (seq tokens) (assoc :evidence tokens))))
    criteria)))

(defn- legacy-relations [num->slug {:keys [relations]}]
  (set
   (concat
    (for [[type key] [[:supersedes :supersedes]
                      [:amends :amends]
                      [:depends-on :depends-on]]
          {:keys [target scope]} (get relations key)]
      (cond-> {:class :lifecycle :type type :to (num->slug target)}
        scope (assoc :scope scope))))))

(defn- legacy-fact-map [num->slug a]
  {:slug (slug-of (:file a))
   :legacy-number (:num a)
   :title (:title a)
   :status (keyword (str/lower-case (:status a)))
   :date (:date a)
   :accepted (:accepted a)
   :validation-scope (some-> (:validation-scope a) keyword)
   :release-authority (some-> (:release-authority a) keyword)
   :source (get (:fields a) "Source")
   :claims (legacy-claims a)})

(deftest decisions-edn-is-fact-equivalent-to-legacy-corpus
  (let [adrs (adr/parse-all adr-dir)
        num->slug (into {} (map (juxt :num #(slug-of (:file %))) adrs))
        sidecar (:relations (edn/read-string (slurp "docs/adr/adr-relations.edn")))
        corpus (edn/read-string (slurp "docs/adr/decisions.edn"))
        by-slug (into {} (map (juxt :slug identity) (:decisions corpus)))]
    (is (= (count adrs) (count (:decisions corpus))))
    (doseq [a adrs
            :let [slug (slug-of (:file a))
                  rec (get by-slug slug)]]
      (is (some? rec) slug)
      ;; scalar facts + claims, field by field; nil legacy value means
      ;; the key must be absent from the record
      (doseq [[k expected] (legacy-fact-map num->slug a)]
        (if (nil? expected)
          (is (not (contains? rec k)) (str slug " must omit " k))
          (is (= expected (get rec k)) (str slug " " k))))
      ;; lifecycle edges: acting side only
      (is (= (legacy-relations num->slug a)
             (set (filter #(= :lifecycle (:class %)) (:relations rec))))
          (str slug " lifecycle relations")))
    ;; annotation edges: all sidecar rows, notes verbatim
    (let [expected (set (for [r sidecar]
                          (cond-> {:class :annotation :type (:type r)
                                   :from (num->slug (:from r))
                                   :to (num->slug (:to r))}
                            (:note r) (assoc :note (:note r)))))
          actual (set (for [rec (:decisions corpus)
                            rel (:relations rec)
                            :when (= :annotation (:class rel))]
                        (assoc rel :from (:slug rec))))]
      (is (= expected actual) "annotation relations with notes"))))
```

- [ ] **Step 3: Run it to make sure it fails**

Run: `KAOCHA --focus abc.tools.decisions-migration-test`
Expected: FAIL (`docs/adr/decisions.edn` does not exist).

- [ ] **Step 4: Write the extractor**

`dev/migrate_decisions.clj` (add `"dev"` to `:test` alias `:extra-paths` temporarily, or run via `clojure -M -e` load-file; keep it out of `src/`):

```clojure
(ns migrate-decisions
  "BRANCH-ONLY extractor: legacy Markdown corpus + adr-relations.edn
   -> docs/adr/decisions.edn. Deleted in the cutover task."
  (:require [abc.tools.adr :as adr]
            [clojure.edn :as edn]
            [clojure.pprint :as pp]
            [clojure.string :as str]))

(def adr-dir "docs/adr")

(def topics
  ;; Initial descriptive topics per legacy number; reviewed by hand.
  {1 [:identity] 2 [:parser] 3 [:nix :materialization]
   4 [:security :release] 5 [:runtime] 6 [:validation]
   7 [:parser :boundary] 8 [:runtime :tooling] 9 [:materialization]
   10 [:identity] 11 [:fixtures] 12 [:tei :validation]
   13 [:lod :publication] 14 [:iiif :publication] 15 [:temporal]
   16 [:temporal] 17 [:lod :vocabulary] 18 [:lod :vocabulary]
   19 [:governance] 20 [:person-drift] 21 [:person-drift]
   22 [:person-drift :ingest] 23 [:parser :identity]
   24 [:parser :publication] 25 [:parser :publication]
   26 [:analysis :identity] 27 [:analysis :tokenizer]
   28 [:publication :annotation] 29 [:diagrams :governance]
   30 [:parser] 31 [:governance] 32 [:parser]
   33 [:identity :source-bundle] 34 [:governance :evidence]
   35 [:rights] 37 [:publication :provenance] 38 [:parser :governance]
   39 [:parser :release] 40 [:parser :release] 41 [:parser :release]
   42 [:parser :evidence] 43 [:governance :evidence]})

(defn- slug-of [filename] (subs filename 5 (- (count filename) 3)))

(def claim-prefix
  #"^\*\*ADR-[0-9]{4}-C[1-9][0-9]* — [a-z]+(?:-[a-z]+)*:\*\*\s*")

(defn- claims [a]
  (vec
   (map-indexed
    (fn [i {:keys [body claim-id claim-kind]}]
      (let [tokens (->> (re-seq #"`([^`]+)`" body) (map second)
                        (filterv (fn [t]
                                   (some #(str/starts-with? t %)
                                         ["test/" "fixtures/" "nix/"
                                          "docs/evidence/external/"]))))]
        ;; "ADR-0043-C1" -> :c1 (digits start after "ADR-NNNN-C", index 10)
        (cond-> {:id (if claim-id
                       (keyword (str "c" (subs claim-id 10)))
                       (keyword (str "c" (inc i))))
                 :statement (str/replace body claim-prefix "")}
          claim-kind (assoc :kind claim-kind)
          (seq tokens) (assoc :evidence tokens))))
    (:criteria a))))

(defn- record [num->slug sidecar-by-from a]
  (let [n (:num a)]
    (cond->
     {:slug (num->slug n)
      :legacy-number n
      :title (:title a)
      :status (keyword (str/lower-case (:status a)))
      :date (:date a)
      :topics (get topics n [])
      :source (get (:fields a) "Source")
      :relations
      (vec
       (concat
        (for [[type key] [[:supersedes :supersedes]
                          [:amends :amends]
                          [:depends-on :depends-on]]
              {:keys [target scope]} (get-in a [:relations key])]
          (cond-> {:class :lifecycle :type type :to (num->slug target)}
            scope (assoc :scope scope)))
        (for [r (get sidecar-by-from n)]
          (cond-> {:class :annotation :type (:type r)
                   :to (num->slug (:to r))}
            (:note r) (assoc :note (:note r))))))
      :claims (claims a)}
      (:accepted a) (assoc :accepted (:accepted a))
      (:validation-scope a) (assoc :validation-scope
                                   (keyword (:validation-scope a)))
      (:release-authority a) (assoc :release-authority
                                    (keyword (:release-authority a))))))

(defn -main [& _]
  (let [adrs (adr/parse-all adr-dir)
        num->slug (into {} (map (juxt :num #(slug-of (:file %))) adrs))
        sidecar (group-by :from
                          (:relations (edn/read-string
                                       (slurp "docs/adr/adr-relations.edn"))))]
    (spit "docs/adr/decisions.edn"
          (with-out-str
            (pp/pprint {:decisions
                        (mapv #(record num->slug sidecar %) adrs)})))
    (println "wrote" (count adrs) "records")))
```

- [ ] **Step 5: Run the extractor, eyeball the output, run the equivalence test**

Run: `clojure -M:test -e "(load-file \"dev/migrate_decisions.clj\") (migrate-decisions/-main)"`
Then read `docs/adr/decisions.edn` end to end (42 records; check 0034 keeps `:validation-scope :full-corpus` with `:status :superseded` and no `:accepted`; 0019 is `:withdrawn`; 0027 claims have no `:kind`).
Run: `KAOCHA --focus abc.tools.decisions-migration-test`
Expected: PASS.

- [ ] **Step 6: Commit**

```bash
git add docs/adr/decisions.edn dev/migrate_decisions.clj test/abc/tools/decisions_migration_test.clj
git commit -m "feat(adr): extract decisions.edn corpus from legacy ADR grammar"
```

---

### Task 2: `abc.tools.decisions` — strict loading boundary

**Files:**
- Create: `src/abc/tools/decisions.clj`
- Create: `test/abc/tools/decisions_test.clj`

**Interfaces:**
- Produces: `(decisions/load-corpus path)` → `{:corpus {:decisions […]}}` or `{:problems [{:kind :invalid-edn :file <path> :message string}]}`. `(decisions/problem kind file message & kvs)` mirrors the old helper. Later tasks call `load-corpus` and `validate-corpus`.

- [ ] **Step 1: Write the failing tests**

```clojure
(ns abc.tools.decisions-test
  (:require [abc.tools.decisions :as d]
            [babashka.fs :as fs]
            [clojure.test :refer [deftest is]]))

(defn- load-str [s]
  (let [f (str (fs/create-temp-file {:suffix ".edn"}))]
    (spit f s)
    (d/load-corpus f)))

(deftest missing-file-is-a-problem-map
  (let [{:keys [problems]} (d/load-corpus "no/such/decisions.edn")]
    (is (= [:invalid-edn] (map :kind problems)))))

(deftest directory-path-is-a-problem-map
  (let [{:keys [problems]} (d/load-corpus "docs")]
    (is (= [:invalid-edn] (map :kind problems)))))

(deftest malformed-edn-is-a-problem-map
  (doseq [s ["{:decisions [" "{:decisions ]}" "#=(boom)"]]
    (let [{:keys [problems]} (load-str s)]
      (is (= [:invalid-edn] (map :kind problems)) s))))

(deftest trailing-second-form-is-a-problem-map
  (let [{:keys [problems]} (load-str "{:decisions []} {:junk true}")]
    (is (= [:invalid-edn] (map :kind problems)))))

(deftest single-form-corpus-loads
  (let [{:keys [corpus problems]} (load-str "{:decisions []}")]
    (is (nil? problems))
    (is (= {:decisions []} corpus))))
```

- [ ] **Step 2: Run to verify failure**

Run: `KAOCHA --focus abc.tools.decisions-test`
Expected: FAIL — namespace `abc.tools.decisions` not found.

- [ ] **Step 3: Implement the loader**

```clojure
(ns abc.tools.decisions
  "Authoritative decision-records corpus: strict loader, Malli shape
   schema, and corpus semantic checks over docs/adr/decisions.edn.
   Replaces the retired abc.tools.adr Markdown grammar."
  (:require [babashka.fs :as fs]
            [clojure.edn :as edn])
  (:import [java.io PushbackReader StringReader]))

(defn problem [kind file message & {:as data}]
  (merge {:kind kind :file file :message message} data))

(def ^:private eof ::eof)

(defn load-corpus
  "Read exactly one EDN form (followed by EOF) from path.
   Returns {:corpus value} or {:problems [{:kind :invalid-edn …}]}."
  [path]
  (let [fail (fn [msg] {:problems [(problem :invalid-edn (str path) msg)]})]
    (cond
      (not (fs/exists? path)) (fail "decisions file does not exist")
      (fs/directory? path) (fail "decisions path is not a file")
      :else
      (try
        (with-open [r (PushbackReader. (StringReader. (slurp (fs/file path))))]
          (let [form (edn/read {:eof eof} r)
                extra (edn/read {:eof eof} r)]
            (cond
              (= eof form) (fail "decisions file contains no EDN form")
              (not= eof extra) (fail "decisions file must contain exactly one EDN form")
              :else {:corpus form})))
        (catch Exception e
          (fail (str "decisions file is not readable EDN: "
                     (.getMessage e))))))))
```

- [ ] **Step 4: Run tests to verify pass, commit**

Run: `KAOCHA --focus abc.tools.decisions-test` — expected PASS.

```bash
git add src/abc/tools/decisions.clj test/abc/tools/decisions_test.clj
git commit -m "feat(adr): strict EDN loading boundary for decisions corpus"
```

---

### Task 3: Malli shape schema

**Files:**
- Modify: `src/abc/tools/decisions.clj`
- Modify: `test/abc/tools/decisions_test.clj`

**Interfaces:**
- Produces: `(decisions/shape-problems corpus file)` → vector of problem maps (`:kind :invalid-shape`, with malli error message text). `decisions/registry` composed from `abc.tools.malli/scalar-schemas` per the Change E convention.

- [ ] **Step 1: Write failing tests** (append to `decisions_test.clj`)

```clojure
(def valid-record
  {:slug "example-decision" :legacy-number 1
   :title "Example" :status :accepted
   :date "2026-07-24" :accepted "2026-07-24"
   :validation-scope :structural :release-authority :none
   :source "docs/superpowers/specs/example.md"
   :topics [:governance]
   :relations []
   :claims [{:id :c1 :kind :structural-invariant
             :statement "Something holds."
             :evidence ["test/abc/tools/decisions_test.clj"]}]})

(defn- shape-of [record]
  (d/shape-problems {:decisions [record]} "decisions.edn"))

(deftest valid-record-has-no-shape-problems
  (is (empty? (shape-of valid-record))))

(deftest shape-rejections
  (doseq [[label bad] {"bad status" (assoc valid-record :status :acceptedd)
                       "bad date" (assoc valid-record :date "2026-7-24")
                       "bad slug" (assoc valid-record :slug "Bad_Slug")
                       "legacy number outside frozen set"
                       (assoc valid-record :legacy-number 44)
                       "misspelled lifecycle type"
                       (assoc valid-record :relations
                              [{:class :lifecycle :type :depend-on :to "x"}])
                       "misspelled supersedes"
                       (assoc valid-record :relations
                              [{:class :lifecycle :type :supercedes :to "x"}])
                       "unknown relation class"
                       (assoc valid-record :relations
                              [{:class :informative :type :extends :to "x"}])
                       "claim id not cN"
                       (assoc-in valid-record [:claims 0 :id] :one)
                       "duplicate claim ids"
                       (assoc valid-record :claims
                              [{:id :c1 :kind :k :statement "a"
                                :evidence ["test/x"]}
                               {:id :c1 :kind :k :statement "b"
                                :evidence ["test/x"]}])}]
    (is (seq (shape-of bad)) label)
    (is (every? #(= :invalid-shape (:kind %)) (shape-of bad)) label)))

(deftest corpus-level-shape-rejections
  (let [two (fn [f] {:decisions [valid-record (f valid-record)]})]
    (is (seq (d/shape-problems (two identity) "decisions.edn"))
        "duplicate slug")
    (is (seq (d/shape-problems
              (two #(assoc % :slug "other-decision")) "decisions.edn"))
        "duplicate legacy number")))

(deftest lifecycle-conditional-shape
  (doseq [[label bad]
          {"draft with accepted date"
           (-> valid-record (assoc :status :draft) (dissoc :validation-scope
                                                           :release-authority))
           "accepted without validation scope"
           (dissoc valid-record :validation-scope)
           "accepted claim without evidence"
           (update-in valid-record [:claims 0] dissoc :evidence)
           "accepted claim without kind"
           (update-in valid-record [:claims 0] dissoc :kind)}]
    (is (seq (shape-of bad)) label))
  (let [superseded (-> valid-record
                       (assoc :status :superseded)
                       (dissoc :accepted))]
    (is (empty? (shape-of superseded))
        "superseded keeps scope/authority, accepted date optional"))
  (let [draft (-> valid-record
                  (assoc :status :draft)
                  (dissoc :accepted :validation-scope :release-authority)
                  (assoc :claims [{:id :c1 :statement "promotion condition"}]))]
    (is (empty? (shape-of draft))
        "draft claims may omit kind and evidence")))
```

- [ ] **Step 2: Run to verify failure**

Run: `KAOCHA --focus abc.tools.decisions-test` — expected FAIL (`shape-problems` undefined).

- [ ] **Step 3: Implement**

Append to `src/abc/tools/decisions.clj` (requires `[abc.tools.malli :as am]`, `[malli.core :as m]`, `[malli.error :as me]`, `[malli.registry :as mr]`):

```clojure
(def statuses #{:draft :proposed :accepted :superseded :withdrawn})
(def validation-scopes #{:structural :fixture :smoke-corpus :full-corpus :operational})
(def release-authorities #{:none :development :publication})
(def lifecycle-types #{:supersedes :amends :depends-on})
(def frozen-legacy-numbers
  #{1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25
    26 27 28 29 30 31 32 33 34 35 37 38 39 40 41 42 43})
(def evidence-prefixes ["test/" "fixtures/" "nix/" "docs/evidence/external/"])

(def ^:private slug-pattern #"^[a-z0-9]+(?:-[a-z0-9]+)*$")
(def ^:private date-pattern #"^\d{4}-\d{2}-\d{2}$")
(def ^:private claim-id-pattern #"^c[1-9][0-9]*$")

(def schemas
  {::slug [:re {:error/message "must be a kebab-case slug"} slug-pattern]
   ::date [:and [:re {:error/message "must be YYYY-MM-DD"} date-pattern]
           [:fn {:error/message "must be a calendar-valid date"}
            (fn [s] (try (java.time.LocalDate/parse s) true
                         (catch Exception _ false)))]]
   ::relation
   [:multi {:dispatch :class
            :error/message "relation :class must be :lifecycle or :annotation"}
    [:lifecycle
     [:map {:closed true}
      [:class [:= :lifecycle]]
      [:type (into [:enum {:error/message
                           "lifecycle type must be :supersedes, :amends, or :depends-on"}]
                   lifecycle-types)]
      [:to ::slug]
      [:scope {:optional true} ::am/nonblank-string]]]
    [:annotation
     [:map {:closed true}
      [:class [:= :annotation]]
      [:type [:and :keyword
              [:fn {:error/message "annotation type must not shadow a lifecycle type"}
               (fn [t] (not (contains? lifecycle-types t)))]]]
      [:to ::slug]
      [:note {:optional true} ::am/nonblank-string]]]]
   ::claim
   [:map {:closed true}
    [:id [:and :keyword
          [:fn {:error/message "claim id must be :cN"}
           (fn [k] (boolean (re-matches claim-id-pattern (name k))))]]]
    [:kind {:optional true} :keyword]
    [:statement ::am/nonblank-string]
    [:evidence {:optional true} [:vector {:min 1} ::am/nonblank-string]]]
   ::record
   [:and
    [:map {:closed true}
     [:slug ::slug]
     [:legacy-number {:optional true}
      (into [:enum {:error/message "legacy number outside the frozen set"}]
            (sort frozen-legacy-numbers))]
     [:title ::am/nonblank-string]
     [:status (into [:enum] (sort statuses))]
     [:date ::date]
     [:accepted {:optional true} ::date]
     [:validation-scope {:optional true} (into [:enum] (sort validation-scopes))]
     [:release-authority {:optional true} (into [:enum] (sort release-authorities))]
     [:source ::am/nonblank-string]
     [:topics [:vector :keyword]]
     [:relations [:vector ::relation]]
     [:claims [:vector ::claim]]]
    [:fn {:error/message "claim ids must be unique within the record"}
     (fn [{:keys [claims]}] (or (empty? claims) (apply distinct? (map :id claims))))]
    [:fn {:error/message
          "accepted/validation-scope/release-authority are required iff :accepted and permitted on :superseded"}
     (fn [{:keys [status accepted validation-scope release-authority]}]
       (case status
         :accepted (boolean (and accepted validation-scope release-authority))
         :superseded true
         (not (or accepted validation-scope release-authority))))]
    [:fn {:error/message
          "accepted and superseded claims require :kind and :evidence"}
     (fn [{:keys [status claims]}]
       (or (not (contains? #{:accepted :superseded} status))
           (every? #(and (:kind %) (seq (:evidence %))) claims)))]]
   ::corpus
   [:and
    [:map {:closed true} [:decisions [:vector ::record]]]
    [:fn {:error/message "slugs must be unique"}
     (fn [{:keys [decisions]}]
       (or (empty? decisions) (apply distinct? (map :slug decisions))))]
    [:fn {:error/message "legacy numbers must be unique"}
     (fn [{:keys [decisions]}]
       (let [ns' (keep :legacy-number decisions)]
         (or (empty? ns') (apply distinct? ns'))))]]})

(def registry
  (mr/composite-registry (m/default-schemas) am/scalar-schemas schemas))

(defn shape-problems [corpus file]
  (if-let [explanation (m/explain ::corpus corpus {:registry registry})]
    (vec (for [msg (distinct (me/humanize explanation {:wrap :message}))
               :let [text (pr-str msg)]]
           (problem :invalid-shape file text)))
    []))
```

Note on `shape-problems`: humanized malli output is nested; flattening it into one problem map per distinct message is sufficient — the CLI contract needs `:kind`/`:file`/`:message`, not per-path granularity. If `me/humanize` shapes prove awkward, emit a single `:invalid-shape` problem whose `:message` is `(pr-str (me/humanize explanation))`; the tests above only assert kind and non-emptiness.

- [ ] **Step 4: Run tests, commit**

Run: `KAOCHA --focus abc.tools.decisions-test` — expected PASS.

```bash
git add src/abc/tools/decisions.clj test/abc/tools/decisions_test.clj
git commit -m "feat(adr): malli shape schema for decisions corpus"
```

---

### Task 4: Corpus semantic checks

**Files:**
- Modify: `src/abc/tools/decisions.clj`
- Modify: `test/abc/tools/decisions_test.clj`

**Interfaces:**
- Produces: `(decisions/semantic-problems corpus repo-root file)` and the top-level `(decisions/validate-repository repo-root)` / `(decisions/validate-repository repo-root adr-dir)` returning a problems vector — same call shape the governance CLI uses today against `abc.tools.adr`.

- [ ] **Step 1: Write failing tests** (append; build tiny corpora inline)

```clojure
(defn- rec [slug & {:as kvs}]
  (merge {:slug slug :title slug :status :draft :date "2026-07-24"
          :source "docs/superpowers/specs/example.md"
          :topics [] :relations [] :claims []}
         kvs))

(defn- accepted [slug & {:as kvs}]
  (merge (rec slug
              :status :accepted :accepted "2026-07-24"
              :validation-scope :structural :release-authority :none
              :claims [{:id :c1 :kind :structural-invariant
                        :statement "s"
                        :evidence ["test/abc/tools/decisions_test.clj"]}])
         kvs))

(defn- lifecycle [type to & [scope]]
  (cond-> {:class :lifecycle :type type :to to}
    scope (assoc :scope scope)))

(defn- sem [decisions]
  (d/semantic-problems {:decisions decisions} "." "decisions.edn"))

(deftest dangling-and-duplicate-relations
  (is (= [:missing-relation-target]
         (map :kind (sem [(rec "a" :relations [(lifecycle :depends-on "ghost")])]))))
  (is (= [:duplicate-relation]
         (map :kind (sem [(rec "a" :relations [(lifecycle :depends-on "b")
                                               (lifecycle :depends-on "b")])
                          (rec "b")])))))

(deftest lifecycle-self-edges-and-cycles-are-rejected
  (is (some #(= :self-relation (:kind %))
            (sem [(rec "a" :relations [(lifecycle :depends-on "a")])])))
  (is (some #(= :relation-cycle (:kind %))
            (sem [(rec "a" :relations [(lifecycle :depends-on "b")])
                  (rec "b" :relations [(lifecycle :depends-on "a")])])))
  (is (some #(= :relation-cycle (:kind %))
            (sem [(rec "a" :status :superseded
                       :relations [(lifecycle :supersedes "b")])
                  (rec "b" :status :superseded
                       :relations [(lifecycle :supersedes "a")])])))
  (is (some #(= :relation-cycle (:kind %))
            (sem [(rec "a" :relations [(lifecycle :amends "b" "x")])
                  (rec "b" :relations [(lifecycle :amends "a" "y")])]))))

(deftest supersession-status-rules
  (is (some #(= :unscoped-supersession-target-not-superseded (:kind %))
            (sem [(rec "a" :relations [(lifecycle :supersedes "b")])
                  (rec "b")]))
      "unscoped supersession of a non-superseded record")
  (is (some #(= :superseded-without-successor (:kind %))
            (sem [(rec "b" :status :superseded
                       :validation-scope :structural)]))
      "superseded record with no incoming unscoped supersession")
  (is (empty? (sem [(rec "a" :relations [(lifecycle :supersedes "b" "one scope")])
                    (accepted "b")]))
      "scoped supersession leaves the target's Accepted status alone"))

(deftest accepted-lifecycle-rules
  (is (some #(= :accepted-before-date (:kind %))
            (sem [(accepted "a" :date "2026-07-25" :accepted "2026-07-24")])))
  (is (some #(= :noncanonical-dependency-path (:kind %))
            (sem [(accepted "a" :relations [(lifecycle :depends-on "b")])
                  (rec "b")]))
      "accepted record depending on a draft")
  (is (some #(= :missing-claims (:kind %))
            (sem [(accepted "a" :claims [])]))
      "accepted records need at least one claim"))

(deftest evidence-path-rules
  (is (some #(= :missing-evidence-path (:kind %))
            (sem [(accepted "a" :claims [{:id :c1 :kind :k :statement "s"
                                          :evidence ["test/no/such/file.clj"]}])])))
  (is (some #(= :evidence-path-traversal (:kind %))
            (sem [(accepted "a" :claims [{:id :c1 :kind :k :statement "s"
                                          :evidence ["test/../deps.edn"]}])])))
  (is (some #(= :unverified-evidence-directory (:kind %))
            (sem [(accepted "a" :claims [{:id :c1 :kind :k :statement "s"
                                          :evidence ["fixtures"]}])]))
      "directory evidence requires a test/ or nix/ file in the same claim"))

(deftest narrative-file-rules
  ;; run against a temp repo-root with docs/adr/<slug>.md present/absent
  (let [root (str (fs/create-temp-dir))]
    (fs/create-dirs (fs/path root "docs/adr"))
    (spit (str (fs/path root "docs/adr/a.md")) "# A\n")
    (spit (str (fs/path root "docs/adr/orphan.md")) "# Orphan\n")
    (let [problems (d/narrative-problems
                    {:decisions [(rec "a") (rec "b")]} root "docs/adr")]
      (is (= #{:missing-narrative :orphan-narrative}
             (set (map :kind problems)))))))
```

(`semantic-problems` covers everything except narrative pairing, which needs a
repo root; `narrative-problems` is separate so both are testable. Evidence
tests run with repo-root `"."` — the real repo — so real paths like
`test/abc/tools/decisions_test.clj` exist.)

- [ ] **Step 2: Run to verify failure**

Run: `KAOCHA --focus abc.tools.decisions-test` — expected FAIL.

- [ ] **Step 3: Implement** (append to `decisions.clj`; requires `[abc.tools.path-containment :as containment]`, `[abc.tools.files :as files]`, `[clojure.string :as str]`)

```clojure
(defn- by-slug [corpus]
  (into {} (map (juxt :slug identity)) (:decisions corpus)))

(defn- lifecycle-edges [record type & {:keys [unscoped-only]}]
  (for [r (:relations record)
        :when (and (= :lifecycle (:class r)) (= type (:type r))
                   (or (not unscoped-only) (nil? (:scope r))))]
    r))

(defn- relation-problems [corpus file]
  (let [known (set (map :slug (:decisions corpus)))]
    (vec
     (concat
      (for [rec (:decisions corpus)
            [edge freq] (frequencies (map #(dissoc % :note) (:relations rec)))
            :when (< 1 freq)]
        (problem :duplicate-relation file
                 "relation must not appear more than once"
                 :slug (:slug rec) :value edge))
      (for [rec (:decisions corpus)
            r (:relations rec)
            :when (not (contains? known (:to r)))]
        (problem :missing-relation-target file
                 "relation target does not exist"
                 :slug (:slug rec) :value r))
      (for [rec (:decisions corpus)
            r (:relations rec)
            :when (and (= :lifecycle (:class r)) (= (:slug rec) (:to r)))]
        (problem :self-relation file
                 "lifecycle relation must not point at its own record"
                 :slug (:slug rec) :value r))))))

(defn- cycle-problems [corpus file]
  ;; DFS three-color cycle detection, one pass per lifecycle graph.
  (let [records (:decisions corpus)
        graphs {:depends-on
                (fn [r] (map :to (lifecycle-edges r :depends-on)))
                :supersedes
                (fn [r] (map :to (lifecycle-edges r :supersedes
                                                  :unscoped-only true)))
                :amends
                (fn [r] (map :to (lifecycle-edges r :amends)))}
        index (by-slug corpus)]
    (vec
     (for [[type neighbors] graphs
           :let [cyclic?
                 (fn cyclic? [slug state]
                   (case (get @state slug)
                     :done false
                     :active true
                     (do (swap! state assoc slug :active)
                         (let [hit (some #(cyclic? % state)
                                         (when-let [r (get index slug)]
                                           (neighbors r)))]
                           (swap! state assoc slug :done)
                           (boolean hit)))))
                 hits (filterv #(cyclic? (:slug %) (atom {})) records)]
           rec hits]
       (problem :relation-cycle file
                (str (name type) " relations must be acyclic")
                :slug (:slug rec) :relation type)))))

(defn- supersession-problems [corpus file]
  (let [index (by-slug corpus)
        unscoped-targets (set (for [rec (:decisions corpus)
                                    r (lifecycle-edges rec :supersedes
                                                       :unscoped-only true)]
                                (:to r)))]
    (vec
     (concat
      (for [rec (:decisions corpus)
            r (lifecycle-edges rec :supersedes :unscoped-only true)
            :let [target (get index (:to r))]
            :when (and target (not= :superseded (:status target)))]
        (problem :unscoped-supersession-target-not-superseded file
                 "an unscoped supersession target must be :superseded"
                 :slug (:slug rec) :value r))
      (for [{:keys [slug status]} (:decisions corpus)
            :when (and (= :superseded status)
                       (not (contains? unscoped-targets slug)))]
        (problem :superseded-without-successor file
                 "a :superseded record requires an incoming unscoped supersession"
                 :slug slug))))))

(defn- lifecycle-date-problems [corpus file]
  (for [{:keys [slug status date accepted]} (:decisions corpus)
        :when (and (= :accepted status) date accepted
                   (.isBefore (java.time.LocalDate/parse accepted)
                              (java.time.LocalDate/parse date)))]
    (problem :accepted-before-date file
             "accepted date must not be before :date" :slug slug)))

(defn- dependency-problems [corpus file]
  (let [index (by-slug corpus)
        closure (fn [slug]
                  (loop [queue (vec (map :to (lifecycle-edges
                                              (get index slug) :depends-on)))
                         seen #{}]
                    (if-let [s (first queue)]
                      (if (contains? seen s)
                        (recur (subvec queue 1) seen)
                        (recur (into (subvec queue 1)
                                     (map :to (lifecycle-edges
                                               (get index s) :depends-on)))
                               (conj seen s)))
                      seen)))]
    (for [{:keys [slug status]} (:decisions corpus)
          :when (= :accepted status)
          dep (sort (closure slug))
          :let [target (get index dep)]
          :when (and target (not= :accepted (:status target)))]
      (problem :noncanonical-dependency-path file
               "Accepted dependency closure contains a non-Accepted record"
               :slug slug :target dep :target-status (:status target)))))

(defn- evidence-problems [corpus repo-root file]
  (vec
   (concat
    (for [{:keys [slug status claims]} (:decisions corpus)
          :when (and (= :accepted status) (empty? claims))]
      (problem :missing-claims file
               "Accepted records require at least one claim" :slug slug))
    (for [{:keys [slug claims]} (:decisions corpus)
          {:keys [id evidence]} claims
          path evidence
          :let [{:keys [state] :as contained}
                (containment/path-state repo-root path)
                normalized (some-> (:relative contained)
                                   (str/replace "\\" "/"))]
          :let [kind (case state
                       :ok (when-not (some #(str/starts-with? normalized %)
                                           evidence-prefixes)
                             :evidence-path-traversal)
                       :missing :missing-evidence-path
                       :real-path-escape :evidence-real-path-escape
                       :malformed-path :malformed-evidence-path
                       :evidence-path-traversal)]
          :when kind]
      (problem kind file "evidence path is invalid"
               :slug slug :claim id :value path))
    (for [{:keys [slug claims]} (:decisions corpus)
          {:keys [id evidence]} claims
          path evidence
          :let [contained (containment/path-state repo-root path)]
          :when (and (= :ok (:state contained))
                     (files/directory? (:path contained)))
          :when (not-any?
                 (fn [companion]
                   (and (or (str/starts-with? companion "test/")
                            (str/starts-with? companion "nix/"))
                        (let [c (containment/path-state repo-root companion)]
                          (and (= :ok (:state c)) (files/file? (:path c))))))
                 evidence)]
      (problem :unverified-evidence-directory file
               "a directory evidence path requires an existing test/ or nix/ file in the same claim"
               :slug slug :claim id :value path)))))

(defn semantic-problems [corpus repo-root file]
  (vec (concat (relation-problems corpus file)
               (cycle-problems corpus file)
               (supersession-problems corpus file)
               (lifecycle-date-problems corpus file)
               (dependency-problems corpus file)
               (evidence-problems corpus repo-root file))))

(defn narrative-problems [corpus repo-root adr-dir]
  (let [dir (fs/path repo-root adr-dir)
        expected (set (map #(str (:slug %) ".md") (:decisions corpus)))
        generated #{"README.md" "INDEX.md"}
        actual (set (for [f (fs/list-dir dir)
                          :let [n (fs/file-name f)]
                          :when (and (fs/regular-file? f)
                                     (str/ends-with? n ".md"))]
                      n))]
    (vec
     (concat
      (for [n (sort expected) :when (not (contains? actual n))]
        (problem :missing-narrative (str adr-dir "/" n)
                 "decision record has no narrative file"))
      (for [n (sort actual)
            :when (and (not (contains? expected n))
                       (not (contains? generated n)))]
        (problem :orphan-narrative (str adr-dir "/" n)
                 "narrative file has no decision record"))))))

(def corpus-file "docs/adr/decisions.edn")

(defn validate-repository
  ([repo-root] (validate-repository repo-root "docs/adr"))
  ([repo-root adr-dir]
   (let [path (str (fs/path repo-root adr-dir "decisions.edn"))
         {:keys [corpus problems]} (load-corpus path)]
     (if problems
       problems
       (let [shape (shape-problems corpus corpus-file)]
         (if (seq shape)
           shape
           (vec (concat (semantic-problems corpus repo-root corpus-file)
                        (narrative-problems corpus repo-root adr-dir)))))))))
```

Semantic checks run only on a shape-valid corpus (shape problems short-circuit), so they can assume well-formed records.

- [ ] **Step 4: Run tests, commit**

Run: `KAOCHA --focus abc.tools.decisions-test` — expected PASS.

```bash
git add src/abc/tools/decisions.clj test/abc/tools/decisions_test.clj
git commit -m "feat(adr): corpus semantic checks for decisions records"
```

---

### Task 5: INDEX.md generator and byte-currency check

**Files:**
- Create: `src/abc/tools/decisions_index.clj`
- Create: `test/abc/tools/decisions_index_test.clj`

**Interfaces:**
- Produces: `(decisions-index/render corpus)` → Markdown string; `(decisions-index/currency-problems corpus repo-root)` → problems (`:stale-index`) when `docs/adr/INDEX.md` bytes ≠ render output; `(decisions-index/write! repo-root)`. Governance calls `currency-problems`; the cutover task wires `write!` into the governance CLI behind `--write-index`.

- [ ] **Step 1: Write failing tests**

```clojure
(ns abc.tools.decisions-index-test
  (:require [abc.tools.decisions-index :as index]
            [babashka.fs :as fs]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]))

(def corpus
  {:decisions
   [{:slug "b-decision" :legacy-number 2 :title "B" :status :accepted
     :date "2026-07-01" :accepted "2026-07-02"
     :validation-scope :structural :release-authority :none
     :source "s" :topics [:parser]
     :relations [{:class :lifecycle :type :supersedes :to "a-decision"}]
     :claims [{:id :c1 :kind :k :statement "s" :evidence ["test/x"]}]}
    {:slug "a-decision" :legacy-number 1 :title "A" :status :superseded
     :date "2026-06-01" :source "s" :topics [:parser :identity]
     :relations [] :claims []}]})

(deftest render-contains-topic-status-and-derived-inverse-sections
  (let [out (index/render corpus)]
    (is (str/includes? out "| [a-decision](a-decision.md) |"))
    (is (str/includes? out "superseded by [b-decision](b-decision.md)"))
    (is (str/includes? out "## Topic: parser"))
    (is (str/includes? out "| 2 | [b-decision](b-decision.md) |")
        "legacy-number table")))

(deftest currency-detects-staleness
  (let [root (str (fs/create-temp-dir))]
    (fs/create-dirs (fs/path root "docs/adr"))
    (is (= [:stale-index]
           (map :kind (index/currency-problems corpus root)))
        "missing INDEX.md is stale")
    (spit (str (fs/path root "docs/adr/INDEX.md")) (index/render corpus))
    (is (empty? (index/currency-problems corpus root)))
    (spit (str (fs/path root "docs/adr/INDEX.md")) "stale")
    (is (= [:stale-index]
           (map :kind (index/currency-problems corpus root))))))
```

- [ ] **Step 2: Run to verify failure**

Run: `KAOCHA --focus abc.tools.decisions-index-test` — expected FAIL.

- [ ] **Step 3: Implement**

```clojure
(ns abc.tools.decisions-index
  "Generated, byte-current derived view of decisions.edn (ADR 0029
   discipline: derived views are gated, never identity)."
  (:require [abc.tools.decisions :as decisions]
            [babashka.fs :as fs]
            [clojure.string :as str]))

(def out-path "docs/adr/INDEX.md")

(defn- inverse-note [corpus slug]
  (let [incoming (for [rec (:decisions corpus)
                       r (:relations rec)
                       :when (and (= :lifecycle (:class r)) (= slug (:to r)))]
                   [(:type r) (:slug rec) (:scope r)])]
    (str/join "; "
              (for [[type from scope] (sort incoming)]
                (str (case type
                       :supersedes "superseded by"
                       :amends "amended by"
                       :depends-on "depended on by")
                     " [" from "](" from ".md)"
                     (when scope (str " [scope: " scope "]")))))))

(defn- record-row [corpus {:keys [slug title status date topics]}]
  (str "| [" slug "](" slug ".md) | " title " | " (name status) " | " date
       " | " (str/join ", " (map name topics))
       " | " (inverse-note corpus slug) " |\n"))

(defn render [corpus]
  (let [decisions (sort-by :slug (:decisions corpus))
        header (str "| Record | Title | Status | Date | Topics | Derived links |\n"
                    "| --- | --- | --- | --- | --- | --- |\n")]
    (str "# Decision Records Index\n\n"
         "GENERATED from `decisions.edn` — do not edit. Regenerate: "
         "`clojure -M:abc/adr-governance --write-index`.\n\n"
         "## All records\n\n" header
         (apply str (map #(record-row corpus %) decisions))
         (apply str
                (for [topic (->> decisions (mapcat :topics) distinct sort)]
                  (str "\n## Topic: " (name topic) "\n\n" header
                       (apply str (for [d decisions
                                        :when (some #{topic} (:topics d))]
                                    (record-row corpus d))))))
         "\n## Legacy numbers\n\n| Legacy | Record |\n| --- | --- |\n"
         (apply str (for [d (sort-by :legacy-number
                                     (filter :legacy-number decisions))]
                      (str "| " (:legacy-number d) " | [" (:slug d) "]("
                           (:slug d) ".md) |\n"))))))

(defn currency-problems [corpus repo-root]
  (let [path (fs/path repo-root out-path)
        expected (render corpus)]
    (if (and (fs/exists? path) (= expected (slurp (fs/file path))))
      []
      [(decisions/problem :stale-index out-path
                          "INDEX.md is not byte-current with decisions.edn; regenerate with --write-index")])))

(defn write! [repo-root]
  (let [{:keys [corpus problems]} (decisions/load-corpus
                                   (fs/path repo-root decisions/corpus-file))]
    (when problems
      (throw (ex-info "cannot index an unreadable corpus" {:problems problems})))
    (spit (str (fs/path repo-root out-path)) (render corpus))))
```

- [ ] **Step 4: Run tests, commit**

Run: `KAOCHA --focus abc.tools.decisions-index-test` — expected PASS.

```bash
git add src/abc/tools/decisions_index.clj test/abc/tools/decisions_index_test.clj
git commit -m "feat(adr): generated INDEX.md derived view with byte-currency gate"
```

---

### Task 6: Atomic cutover

This task flips the repository from the old representation to the new one and deletes the old machinery. It is one revertable unit; expect the suite to be red mid-task and green at the end.

**Files:**
- Modify: `src/abc/tools/adr_governance.clj` (re-point to decisions, add `--write-index`)
- Modify: `src/abc/tools/decisions.clj` (wire `decisions-index/currency-problems` into `validate-repository`)
- Modify: `src/abc/tools/diagram/adr_graph.clj` (consume `decisions.edn`)
- Modify: `src/abc/tools/diagram/architecture_graph.clj` (slug owners)
- Modify: `src/abc/tools/diagram/presentation_figures.clj` (slug `:adrs`)
- Modify: `docs/architecture-stages.edn`, `docs/architecture-presentation.edn` (integer ADR refs → slugs)
- Modify: `test/abc/tools/adr_governance_test.clj`, `test/abc/tools/diagram/adr_graph_test.clj`, `test/abc/tools/diagram/architecture_graph_test.clj`, `test/abc/tools/diagram/workflow_graph_test.clj` (only if it touches adr), `test/abc/sim/ingest_sim_test.clj` (check its `docs/adr` reference)
- Delete: `src/abc/tools/adr.clj`, `test/abc/tools/adr_test.clj`, `docs/adr/adr-relations.edn`, `fixtures/adr-governance-invalid/`, `dev/migrate_decisions.clj`, `test/abc/tools/decisions_migration_test.clj`, `nix/adr-family-clean.jq`, `data/evidence-higher-order-calls/adr-validate-repository-star.edn`
- Rename: all 42 `docs/adr/NNNN-*.md` → `docs/adr/<slug>.md` (git mv), stripped of header block and Acceptance Criteria section
- Regenerate: `docs/adr/adr-graph.mmd`, create `docs/adr/INDEX.md`

**Interfaces:**
- Consumes: `decisions/load-corpus`, `decisions/validate-repository`, `decisions-index/write!`, `decisions-index/currency-problems`.
- Produces: governance CLI on the new corpus; `adr-graph/build` returning the same graph-map shape (`{:direction :nodes :edges :class-defs}`) with slug node ids.

- [ ] **Step 1: Strip and rename narrative files (scripted, reviewed)**

Write a one-shot bash+awk (or babashka) pass; for each `docs/adr/NNNN-<slug>.md`:
- delete from the `Status:` line through the blank line ending the header block (the title line stays);
- retitle `# ADR NNNN: Title` → `# Title`;
- delete the `## Acceptance Criteria` section (heading through the line before the next `##` heading);
- `git mv docs/adr/NNNN-<slug>.md docs/adr/<slug>.md`.

The 0019 tombstone keeps its narrative (reason for withdrawal) under the new name. Verify with `git diff --stat` that only header/criteria lines were removed, then spot-read three files (0001, 0027, 0043) fully.

- [ ] **Step 2: Re-point the governance CLI**

`adr_governance.clj` — requires become `[abc.tools.decisions :as decisions]` and `[abc.tools.decisions-index :as index]`:

```clojure
(def cli-options
  [[nil "--repo-root PATH" "ABC artifact and ADR root."]
   [nil "--write-index" "Regenerate docs/adr/INDEX.md from decisions.edn."]])

(defn run!
  "Strictly validate the decisions corpus, including INDEX byte currency.
   INDEX currency lives here (not in decisions/validate-repository) so that
   decisions-index may depend on decisions without a require cycle."
  [repo-root]
  (let [core (decisions/validate-repository repo-root)
        currency (if (seq core)
                   []   ; unreadable/invalid corpus already reported
                   (let [{:keys [corpus]}
                         (decisions/load-corpus
                          (babashka.fs/path repo-root decisions/corpus-file))]
                     (index/currency-problems corpus repo-root)))
        problems (vec (concat core currency))
        ok? (empty? problems)]
    {:ok? ok? :exit-code (if ok? 0 1) :problems problems}))
```

`-main`'s `:run` gains: when `(:write-index options)` call `(index/write! root)` before validating. `emit-result!` is unchanged (problem maps still carry `:kind`/`:file`/`:message`).

- [ ] **Step 3: Rewrite the decision graph builder**

`adr_graph.clj` becomes a consumer of the corpus value:

```clojure
(ns abc.tools.diagram.adr-graph
  "Pure builder: decisions.edn -> decision-map graph value. See ADR 0029
   and the data-driven-decision-records record."
  (:require [abc.tools.decisions :as decisions]
            [clojure.string :as str]))

(def out-path "docs/adr/adr-graph.mmd")

(def status-class
  {:accepted "accepted" :proposed "proposed" :draft "draft"
   :superseded "superseded" :withdrawn "withdrawn"})

(def class-defs
  {:accepted   "fill:#1b5e20,stroke:#a5d6a7,color:#fff"
   :proposed   "fill:#e65100,stroke:#ffcc80,color:#fff"
   :draft      "fill:#37474f,stroke:#b0bec5,color:#fff"
   :superseded "fill:#4a148c,stroke:#ce93d8,color:#fff"
   :withdrawn  "fill:#b71c1c,stroke:#ef9a9a,color:#fff"})

(def edge-style
  {:amends [:solid "amends"]
   :supersedes [:thick "supersedes"]
   :depends-on [:dashed "depends on"]})

(defn- scoped-label [label scope]
  (if scope (str label " — " scope) label))

(defn- edge-label [{:keys [type scope]}]
  (if-let [[style label] (get edge-style type)]
    [style (scoped-label label scope)]
    [:dashed (str/replace (name type) "-" " ")]))

(defn graph-from [corpus]
  {:direction "LR"
   :nodes (for [{:keys [slug title status]} (:decisions corpus)]
            {:id slug :label title
             :class (get status-class status "draft")})
   :edges (for [{:keys [slug relations]} (:decisions corpus)
                rel relations
                :let [[style label] (edge-label rel)]]
            {:from slug :to (:to rel) :style style :label label})
   :class-defs class-defs})

(defn lint* []
  (decisions/validate-repository "."))

(defn build []
  (let [{:keys [corpus problems]} (decisions/load-corpus decisions/corpus-file)]
    (when problems (throw (ex-info "unreadable decisions corpus" {:problems problems})))
    (graph-from corpus)))
```

(Mermaid node ids: slugs are already `[a-z0-9-]`, valid Mermaid identifiers. Check `abc.tools.diagram.mermaid` for id quoting; if dashes need quoting, keep ids as slugs and let the renderer quote labels as it already does for the old `ADR0043` ids.)

Update `adr_graph_test.clj` to feed synthetic corpus values into `graph-from` and assert node ids/labels/classes and scoped edge labels — mirror the old test's assertions with slug ids.

- [ ] **Step 4: Architecture metadata to slugs**

- `docs/architecture-stages.edn`: every integer ADR reference becomes the slug string, e.g. `"manifest_schema_hash" [1 10]` → `"manifest_schema_hash" ["manifest-identity" "manifest-identity-hardening"]`, stage `:adr` vectors likewise. Build the number→slug mapping from `decisions.edn` `:legacy-number`s (a 10-line babashka one-liner or by hand with the INDEX.md legacy table).
- `docs/architecture-presentation.edn`: same conversion for `:adrs` backing.
- `architecture_graph.clj`: `adr-nums` becomes

```clojure
(defn adr-slugs []
  (let [{:keys [corpus problems]} (decisions/load-corpus decisions/corpus-file)]
    (when problems (throw (ex-info "unreadable decisions corpus" {:problems problems})))
    (set (map :slug (:decisions corpus)))))
```

  and every `contains? adr-nums owner` comparison keys on slug strings.
- `presentation_figures.clj:111-113`: the backing `:adrs` check becomes `(every? string? adrs)` with the error message "backing ADRs must be a sequential collection of decision slugs".
- Update `architecture_graph_test.clj` expectations to slugs.

- [ ] **Step 5: Governance test rewrite**

`adr_governance_test.clj`: keep the structure; the redefs now target `decisions/validate-repository`; extend `retired-typed-evidence-apparatus-is-absent-test` (rename to `retired-apparatus-is-absent-test`) with:

```clojure
"src/abc/tools/adr.clj"
"test/abc/tools/adr_test.clj"
"docs/adr/adr-relations.edn"
"fixtures/adr-governance-invalid"
"dev/migrate_decisions.clj"
"test/abc/tools/decisions_migration_test.clj"
"nix/adr-family-clean.jq"
"data/evidence-higher-order-calls/adr-validate-repository-star.edn"
```

plus a check that no `docs/adr/*.md` filename starts with four digits. `repository-corpus-is-strictly-valid-test` stays: `(governance/run! ".")` must be `:ok?` over the real migrated corpus.

- [ ] **Step 6: Delete the old machinery**

```bash
git rm src/abc/tools/adr.clj test/abc/tools/adr_test.clj \
       docs/adr/adr-relations.edn dev/migrate_decisions.clj \
       test/abc/tools/decisions_migration_test.clj \
       nix/adr-family-clean.jq \
       data/evidence-higher-order-calls/adr-validate-repository-star.edn
git rm -r fixtures/adr-governance-invalid
```

Then `grep -rn "abc.tools.adr\b\|tools/adr\b\|parse-all" src/ test/ nix/ flake.nix` — every remaining reference must be `abc.tools.adr-governance` (the CLI ns keeps its name) or `abc.tools.decisions*`.

- [ ] **Step 7: Regenerate derived views, run everything**

```bash
clojure -M:abc/adr-governance --write-index
clojure -M:abc/diagrams
KAOCHA
clojure -M:abc/adr-governance
```

Expected: full suite PASS; governance prints `ADR governance valid`, exit 0. If the Nix gate is cheap, also `nix build .#checks.<system>.adr-governance` (see `flake.nix:817`); otherwise defer to Task 8's final gate.

- [ ] **Step 8: Commit**

```bash
git add -A
git commit -m "refactor(adr)!: cut over to decisions.edn; retire Markdown ADR grammar

Slug-named narrative files; governance, decision graph, and
architecture metadata consume decisions.edn; old parser, relations
sidecar, claim grammar, and their fixtures deleted."
```

---

### Task 7: README rewrite and active-doc sweep

**Files:**
- Modify: `docs/adr/README.md`
- Modify: `docs/v0-design-bundle/README.md` (numbered `docs/adr/NNNN-*.md` links → slug paths)
- Modify: any other active doc with `docs/adr/NNNN` links: `grep -rn "docs/adr/00" docs/ --include="*.md" | grep -v superpowers | grep -v archive` — fix hits in `architecture.md`, `high-level-architecture-note.md`, etc.; leave `docs/superpowers/` (historical specs/plans/reports) and `docs/archive/` untouched.

**Interfaces:** none (prose only).

- [ ] **Step 1: Rewrite `docs/adr/README.md`**

Keep: purpose paragraph, status-vocabulary table (reworded for keywords), Draft-vs-Proposed guidance, edit policy (the split table from the spec §2), manifest-identity invariants section, known open questions. Replace the "File naming", "Header fields", "Typed Acceptance Criterion headers", and "Acceptance Criteria gate" sections with:

```markdown
## Representation

Machine-facing decision facts live in `decisions.edn` — one record per
decision: identity (`:slug`), lifecycle (`:status`, `:date`, `:accepted`,
`:validation-scope`, `:release-authority`), `:topics`, one-directional
`:relations` (lifecycle edges `:supersedes`/`:amends`/`:depends-on` with
optional `[scope]`; open annotation edges), and `:claims` (acceptance
criteria with `:statement`, `:kind`, `:evidence` paths). The Malli schema in
`src/abc/tools/decisions.clj` is the authoritative shape; `clojure
-M:abc/adr-governance` validates shape, graph semantics, evidence paths,
narrative pairing, and INDEX currency. `<slug>.md` files are pure narrative —
no tool parses them. `INDEX.md` and `adr-graph.mmd` are generated views
(`--write-index`, `clojure -M:abc/diagrams`).

Claims are cited as `slug#c1`. Historical `ADR NNNN` / `ADR-NNNN-CN`
references resolve through `:legacy-number` (see the INDEX legacy table);
the frozen number set is closed — new records are slug-only.
```

- [ ] **Step 2: Sweep active docs**

Run the grep above; update each active-doc link from `docs/adr/NNNN-slug.md` to `docs/adr/slug.md` (text mentions of "ADR NNNN" in active docs may stay where they read as history; links must not 404).

- [ ] **Step 3: Verify and commit**

Run: `clojure -M:abc/adr-governance` (still valid) and `grep -c "ADR-....-C\|NNNN" docs/adr/README.md` (expect 0 grammar remnants).

```bash
git add docs/
git commit -m "docs(adr): README describes decisions.edn; re-point active doc links"
```

---

### Task 8: The decision record for this refactor + final gates

**Files:**
- Modify: `docs/adr/decisions.edn` (append record)
- Create: `docs/adr/data-driven-decision-records.md`

**Interfaces:** none new.

- [ ] **Step 1: Append the record**

```clojure
{:slug "data-driven-decision-records"
 :title "Data-Driven Decision Records"
 :status :accepted
 :date "2026-07-24"
 :accepted "2026-07-24"
 :validation-scope :structural
 :release-authority :none
 :source "docs/superpowers/specs/2026-07-24-adr-data-driven-decision-records-design.md"
 :topics [:governance]
 :relations [{:class :lifecycle :type :supersedes
              :to "adr-governance-validation"
              :scope "ADR Markdown header and claim grammar"}
             {:class :lifecycle :type :amends
              :to "diagrams-as-gated-derived-views"
              :scope "decision-graph source of truth"}]
 :claims
 [{:id :c1 :kind :structural-invariant
   :statement "decisions.edn is loaded through a strict boundary: missing, malformed, or multi-form input yields :invalid-edn problem maps and nonzero governance exit. Evidence boundary: `test/abc/tools/decisions_test.clj`."
   :evidence ["test/abc/tools/decisions_test.clj"]}
  {:id :c2 :kind :structural-invariant
   :statement "The relation model fails closed: misspelled lifecycle types and unknown relation classes are schema errors, and lifecycle graphs are irreflexive and acyclic. Evidence boundary: `test/abc/tools/decisions_test.clj`."
   :evidence ["test/abc/tools/decisions_test.clj"]}
  {:id :c3 :kind :structural-invariant
   :statement "Repository governance validates the decisions corpus strictly — shape, graph semantics, evidence paths, narrative pairing, INDEX currency — with nonzero exit on any problem. Evidence boundary: `test/abc/tools/adr_governance_test.clj`."
   :evidence ["test/abc/tools/adr_governance_test.clj"]}
  {:id :c4 :kind :structural-invariant
   :statement "No Markdown ADR grammar machinery remains: the legacy parser, relations sidecar, claim-header grammar, invalid-Markdown fixtures, and migration scaffolding are absent from the working tree. Evidence boundary: `test/abc/tools/adr_governance_test.clj`."
   :evidence ["test/abc/tools/adr_governance_test.clj"]}]}
```

Note: because this record uses a scoped supersession, ADR 0031's record keeps `:status :accepted` (scoped supersession does not flip status — matches the rule).

- [ ] **Step 2: Write the narrative** `docs/adr/data-driven-decision-records.md`

Sections: Context (prose grammar recovered by a 600-line parser; adr-relations.edn split; 0043's subtractive lesson), Decision (decisions.edn authoritative; slugs primary; prose narrative-only; derived views generated; old machinery deleted without replacement), Consequences (single representation; typo-fail-closed relations; INDEX/graph derived; legacy numbers frozen), Rollback (revert the atomic cutover change via a new record).

- [ ] **Step 3: Regenerate views, full gates**

```bash
clojure -M:abc/adr-governance --write-index
clojure -M:abc/diagrams
KAOCHA
clojure -M:abc/adr-governance
nix build .#checks.$(nix eval --raw --impure --expr builtins.currentSystem).adr-governance 2>/dev/null || nix flake check
```

Expected: all PASS; governance valid exit 0.

- [ ] **Step 4: Commit**

```bash
git add docs/adr/
git commit -m "feat(adr): record the data-driven decision-records decision"
```

---

## Self-Review Checklist (run after Task 8)

1. Retirement checklist from the spec: `ls docs/adr/` shows only `<slug>.md`, `README.md`, `INDEX.md`, `decisions.edn`, `adr-graph.mmd`, the two `.pl` files (unrelated Prolog checks, kept); no `NNNN-*` names anywhere; `git grep -l "abc.tools.adr "` returns nothing.
2. Spec §3 checks all implemented: loader boundary, shape schema (frozen set, discriminated relations), semantics (self-edge, three acyclicity checks, supersession, dates, closure, evidence, narrative pairing), INDEX currency.
3. Governance CLI contract intact: `clojure -M:abc/adr-governance --repo-root .` exits 0 with `ADR governance valid`; a scratch corpus with junk appended exits 1 with an `ADR-LINT … invalid-edn:` line.
4. Equivalence test passed before deletion (check git log shows Task 1 commit before the cutover commit).
