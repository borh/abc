(ns abc.tools.adr-claim-migration-test
  (:require [abc.tools.adr :as adr]
            [abc.tools.adr-claim-migration :as migration]
            [abc.tools.cli :as cli]
            [abc.tools.files :as files]
            [abc.tools.schema :as schema]
            [babashka.fs :as fs]
            [clojure.test :refer [deftest is]]))

(def revision "0123456789012345678901234567890123456789")

(defn- kinds [problems]
  (set (map :kind problems)))

(defn- checked-in-baseline []
  (files/read-json "docs/adr/adr-claim-migration-baseline.json"))

(defn- dispatch [args]
  (cli/dispatch! (cli/parse args migration/cli-config)
                 migration/cli-config))

(deftest cli-preserves-usage-success-and-refusal-exit-codes-test
  (let [dir (fs/create-temp-dir {:prefix "adr-claim-migration-cli-"})
        output (fs/file dir "baseline.json")]
    (is (= 2 (dispatch [])))
    (is (= 0 (dispatch ["--write-baseline" (str output)
                        "--revision" revision])))
    (is (fs/regular-file? output))
    (is (= 1 (dispatch ["--write-baseline" (str output)
                        "--revision" revision])))))

(deftest criterion-hash-is-exact-and-baseline-is-deterministic-test
  (is (not= (migration/criterion-text-hash "First.")
            (migration/criterion-text-hash "First.\n")))
  (let [adrs (adr/parse-all "docs/adr")
        baseline (migration/baseline-value revision adrs)]
    (is (= revision (get baseline "baseline_revision")))
    (is (= 32 (get baseline "accepted_adr_count")))
    (is (= 156 (get baseline "criterion_count"))
        "the post-migration accepted-criterion baseline remains exact")
    (is (= 36 (get baseline "normative_section_count")))
    (is (= baseline (migration/baseline-value revision (reverse adrs))))
    (is (= (sort-by (juxt #(get % "adr") #(get % "original_criterion_index"))
                    (get baseline "criteria"))
           (get baseline "criteria")))
    (is (= (sort-by (juxt #(get % "adr") #(get % "section"))
                    (get baseline "normative_sections"))
           (get baseline "normative_sections")))
    (is (= #{"adr" "file" "original_criterion_index" "original_text"
             "original_text_hash"}
           (set (keys (first (get baseline "criteria"))))))))

(deftest inline-fixture-uses-exact-heading-coordinates-test
  (let [adrs [{:num 1 :file "0001-one.md" :status "Accepted"
               :criteria [{:criterion-index 0 :body "First."}]
               :section-bodies {"Decision" "D1"
                                "Decision Matrix" "not coordinated"
                                "Hard Rule" "not coordinated"}}
              {:num 2 :file "0002-two.md" :status "Accepted"
               :criteria [{:criterion-index 0 :body "Second."}]
               :section-bodies {"Decision" "D2"
                                "Hard Rule (carried forward from ADR 0015)"
                                "not coordinated"}}]
        baseline (migration/baseline-value revision adrs)]
    (is (= {"adr" 1 "file" "0001-one.md" "original_criterion_index" 0
            "original_text" "First."
            "original_text_hash" (migration/criterion-text-hash "First.")}
           (first (get baseline "criteria"))))
    (is (= [[1 "Decision"] [2 "Decision"]]
           (mapv (juxt #(get % "adr") #(get % "section"))
                 (get baseline "normative_sections"))))))

(deftest baseline-generation-rejects-a-missing-explicit-coordinate-test
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo #"normative coordinate"
       (migration/baseline-value
        revision
        [{:num 1 :file "0001-one.md" :status "Accepted"
          :criteria [] :section-bodies {}}]))))

(deftest baseline-generation-rejects-a-duplicate-source-coordinate-test
  (let [inventory-var #'abc.tools.adr-claim-migration/normative-coordinate-inventory
        coordinates (inventory-var)]
    (with-redefs-fn {inventory-var
                     (fn [] (conj coordinates (first coordinates)))}
      #(is (thrown-with-msg?
            clojure.lang.ExceptionInfo #"duplicate normative coordinate"
            (migration/baseline-value revision (adr/parse-all "docs/adr")))))))

(deftest baseline-validation-test
  (let [baseline (checked-in-baseline)
        row (first (get baseline "criteria"))]
    (is (empty? (migration/validate-baseline baseline)))
    (is (contains? (kinds (migration/validate-baseline (assoc baseline "extra" true)))
                   :invalid-migration-baseline))
    (is (contains? (kinds (migration/validate-baseline
                           (assoc baseline "criteria" [row (assoc row "original_criterion_index" 1)]
                                  "criterion_count" 2)))
                   :invalid-migration-baseline))
    (is (contains? (kinds (migration/validate-baseline
                           (assoc-in baseline ["criteria" 0 "original_text"] "changed")))
                   :invalid-migration-baseline))
    (let [schema-value (files/read-json "schemas/adr-claim-migration-baseline.schema.json")]
      (is (seq (schema/validation-errors schema-value (assoc baseline "extra" true))))
      (is (seq (schema/validation-errors schema-value
                                         (assoc baseline "schema_version" "wrong")))))))

(deftest all-normative-coordinates-are-byte-guarded-test
  (let [adrs (adr/parse-all "docs/adr")
        baseline (migration/baseline-value revision adrs)]
    (doseq [{:strs [adr section]} (get baseline "normative_sections")]
      (let [changed (mapv #(if (= adr (:num %))
                             (update-in % [:section-bodies section] str "\nchanged")
                             %)
                          adrs)]
        (is (contains? (kinds (migration/normative-section-problems baseline changed))
                       :accepted-normative-section-drift)
            (str adr " " section))))
    (doseq [change [(fn [x] (assoc-in x [:section-bodies "Implementation Status"] "changed"))
                    (fn [x] (assoc-in x [:section-bodies "Acceptance Criteria"] "changed"))
                    (fn [x] (assoc-in x [:section-bodies "Historical Evidence"] "changed"))
                    (fn [x] (assoc-in x [:section-bodies "Future Verification"] "changed"))
                    (fn [x] (assoc-in x [:fields "Supersedes"] "ADR 9999"))]]
      (is (empty? (migration/normative-section-problems baseline
                                                        (update adrs 0 change)))))))

(defn- ledger-fixture [baseline]
  (let [row (first (get baseline "criteria"))
        key [(get row "adr") (get row "original_text_hash")]]
    {:key key
     :ledger {:schema-version "abc-adr-claim-migration-v1"
              :baseline-revision (get baseline "baseline_revision")
              :baseline-manifest-hash (migration/baseline-hash baseline)
              :entries {key {:disposition :correct
                             :rationale "Narrow to the implemented artifact-ID oracle."
                             :planned-evidence-boundaries [:manifest-index-oracle]
                             :resulting-claim-ids ["ADR-0001-C4"]}}}}))

(deftest ledger-validation-contract-test
  (let [baseline (checked-in-baseline)
        {:keys [key ledger]} (ledger-fixture baseline)
        claims {"ADR-0001-C4" {}}
        validate #(migration/validate-ledger baseline %1 %2 {:require-complete? false})]
    (is (empty? (validate ledger claims)))
    (is (contains? (kinds (migration/validate-ledger baseline ledger claims
                                                     {:require-complete? true}))
                   :unresolved-baseline-key))
    (is (contains? (kinds (validate (update ledger :entries assoc [999 "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"]
                                            (get-in ledger [:entries key])) claims))
                   :unknown-baseline-key))
    (is (contains? (kinds (validate (assoc-in ledger [:entries key :disposition] :unknown) claims))
                   :invalid-disposition))
    (is (contains? (kinds (validate (update-in ledger [:entries key] dissoc :rationale) claims))
                   :missing-disposition-rationale))
    (is (contains? (kinds (validate (assoc-in ledger [:entries key :planned-evidence-boundaries] []) claims))
                   :missing-evidence-boundary))
    (is (contains? (kinds (validate (assoc-in ledger [:entries key :resulting-claim-ids]
                                              ["ADR-0001-C4" "ADR-0001-C4"]) claims))
                   :duplicate-resulting-claim-id))
    (is (contains? (kinds (validate ledger {})) :missing-resulting-claim-id))
    (is (contains? (kinds (validate (assoc ledger :baseline-revision (apply str (repeat 40 "f"))) claims))
                   :baseline-revision-mismatch))
    (is (contains? (kinds (validate (assoc ledger :baseline-manifest-hash
                                           "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa") claims))
                   :baseline-hash-mismatch))
    (is (contains? (kinds (validate (-> ledger
                                        (assoc-in [:entries key :disposition] :move-out-of-acceptance)
                                        (assoc-in [:entries key :planned-evidence-boundaries] [])
                                        (assoc-in [:entries key :resulting-claim-ids] ["ADR-0001-C4"])) claims))
                   :invalid-disposition))))

(deftest foundation-corrected-ledger-provenance-test
  (let [entries (:entries (files/read-edn "docs/adr/adr-claim-migration.edn"))
        expected
        {[1 "sha256:6d495f4c83d65f6dc7a1c4cebf20b528d6fe9befad0c262b5f407eed5bf63d35"]
         {:disposition :correct
          :rationale "Correct example-only wording to the universal schema rejection of nested artifact_id."
          :planned-evidence-boundaries [:nested-artifact-id-schema-rejection]
          :resulting-claim-ids ["ADR-0001-C3"]}
         [9 "sha256:32934fe5b8af881021dfd1872b6582c8631d3e7f8f58c27c7e4a4a1c64c367d9"]
         {:disposition :correct
          :rationale "Correct the broad schema-compatibility statement to pinned AAT mapping plus adapter registry agreement and diagnostic exact-current equality."
          :planned-evidence-boundaries [:aat-parser-ir-adapter-registry-agreement
                                        :diagnostic-schema-exact-current]
          :resulting-claim-ids ["ADR-0009-C5"]}
         [10 "sha256:f2fd52b030dc92074964127d3bd207aa2fd18426b8e2945aa6c41d71e99b2801"]
         {:disposition :correct
          :rationale "Split parser-IR schema mismatch rejection from diagnostic exact-current mismatch rejection."
          :planned-evidence-boundaries [:parser-ir-schema-mismatch-rejection
                                        :diagnostic-schema-exact-current-mismatch-rejection]
          :resulting-claim-ids ["ADR-0010-C4"]}
         [33 "sha256:5f0ee5797b8e45e6292ed4015d791002b96ec3d18e980f699de73ce3203bf2d0"]
         {:disposition :correct
          :rationale "Narrow this row to the canonical all-member schema fixture; the separate known-answer baseline row solely owns ADR-0033-C2."
          :planned-evidence-boundaries [:canonical-all-member-schema-fixture]
          :resulting-claim-ids ["ADR-0033-C1"]}}]
    (doseq [[key value] expected]
      (is (= value (get entries key)) (pr-str key)))
    (is (= 1 (count (filter #(some #{"ADR-0033-C2"} (:resulting-claim-ids %))
                            (vals entries)))))))
