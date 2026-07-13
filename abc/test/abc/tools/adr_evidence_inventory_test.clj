(ns abc.tools.adr-evidence-inventory-test
  (:require [abc.tools.adr :as adr]
            [abc.tools.adr-claim-migration :as migration]
            [abc.tools.adr-evidence-inventory :as inventory]
            [abc.tools.files :as files]
            [abc.tools.json :as json]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]))

(def retained-text "Criterion text.")
(def corrected-text "Correct this.")
(def moved-text "Move this.")

(def sample-adrs
  [{:num 1 :file "0001-one.md" :status "Accepted"
    :criteria [{:criterion-index 7 :body retained-text
                :claim-id "ADR-0001-C1" :claim-kind :fixture-behavior}
               {:criterion-index 8 :body "Narrow claim one."
                :claim-id "ADR-0001-C2" :claim-kind :structural-invariant}
               {:criterion-index 9 :body "Narrow claim two."
                :claim-id "ADR-0001-C3" :claim-kind :fixture-behavior}]
    :evidence [{:criterion-index 7 :path "test/example.clj"}]}
   {:num 2 :file "0002-two.md" :status "Accepted"
    :criteria [{:criterion-index 0 :body "Second." :claim-id nil :claim-kind nil}]
    :evidence []}
   {:num 99 :file "0099-proposed.md" :status "Proposed"
    :criteria [{:criterion-index 0 :body "Not inventoried."}]
    :evidence []}])

(def sample-baseline
  {"baseline_revision" "0123456789012345678901234567890123456789"
   "criteria"
   [{"adr" 1 "file" "0001-one.md" "original_criterion_index" 0
     "original_text" retained-text
     "original_text_hash" (migration/criterion-text-hash retained-text)}
    {"adr" 1 "file" "0001-one.md" "original_criterion_index" 1
     "original_text" corrected-text
     "original_text_hash" (migration/criterion-text-hash corrected-text)}
    {"adr" 1 "file" "0001-one.md" "original_criterion_index" 2
     "original_text" moved-text
     "original_text_hash" (migration/criterion-text-hash moved-text)}]})

(def sample-ledger
  {:entries
   {[1 (migration/criterion-text-hash retained-text)]
    {:disposition :retain
     :planned-evidence-boundaries [:retained-boundary]
     :resulting-claim-ids ["ADR-0001-C1"]}
    [1 (migration/criterion-text-hash corrected-text)]
    {:disposition :correct
     :rationale "Split the broad statement into two observable claims."
     :planned-evidence-boundaries [:first-boundary :second-boundary]
     :resulting-claim-ids ["ADR-0001-C2" "ADR-0001-C3"]}
    [1 (migration/criterion-text-hash moved-text)]
    {:disposition :move-out-of-acceptance
     :rationale "This belongs under future verification."
     :planned-evidence-boundaries [:future-verification]
     :resulting-claim-ids []}}})

(def sample-state
  {:baseline sample-baseline :ledger sample-ledger :by-key (:entries sample-ledger)
   :problems []})

(deftest inventory-joins-reviewed-baseline-to-live-criteria-test
  (let [value (inventory/inventory-value sample-adrs sample-state)
        baseline-rows (get value "baseline_criteria")
        live-rows (get value "criteria")]
    (is (= 3 (get value "baseline_criterion_count")))
    (is (= 2 (get value "accepted_adr_count")))
    (is (= 4 (get value "accepted_criterion_count")))
    (is (= 7 (get (first live-rows) "criterion_index"))
        "retained rows join by exact text hash, not descriptive index")
    (is (= ["ADR-0001-C2" "ADR-0001-C3"]
           (get (second baseline-rows) "resulting_claim_ids")))
    (is (= 2 (count (filter #(= [1 (migration/criterion-text-hash corrected-text)]
                                (get % "baseline_key"))
                            live-rows)))
        "a corrected baseline row maps to both resulting live claims")
    (is (= "move-out-of-acceptance" (get (nth baseline-rows 2) "disposition")))
    (is (not-any? #(= moved-text (get % "body")) live-rows))
    (is (every? some? (map #(get % "disposition") baseline-rows)))
    (is (= [1 (migration/criterion-text-hash retained-text)]
           (get (first baseline-rows) "baseline_key")))
    (is (= "Split the broad statement into two observable claims."
           (get (second baseline-rows) "disposition_rationale")))
    (is (= ["first-boundary" "second-boundary"]
           (get (second baseline-rows) "planned_evidence_boundaries")))))

(deftest inventory-json-is-byte-identical
  (let [dir (.toFile (java.nio.file.Files/createTempDirectory
                      "abc-inventory-test" (make-array java.nio.file.attribute.FileAttribute 0)))
        first-file (io/file dir "first.json")
        second-file (io/file dir "second.json")]
    (inventory/write-inventory! first-file sample-adrs sample-state)
    (inventory/write-inventory! second-file sample-adrs sample-state)
    (is (= (slurp first-file) (slurp second-file)))
    (is (= (inventory/inventory-value sample-adrs sample-state)
           (json/read-json-file first-file)))))

(deftest foundation-stage-a-claim-and-lifecycle-contract-test
  (let [expected-ranges {1 5, 8 4, 9 7, 10 5, 11 3, 33 11}
        expected-ids (set (mapcat (fn [[adr count]]
                                    (map #(format "ADR-%04d-C%d" adr %)
                                         (range 1 (inc count))))
                                  expected-ranges))
        expected-lifecycle {1 ["fixture" "publication"]
                            8 ["operational" "development"]
                            9 ["fixture" "development"]
                            10 ["fixture" "publication"]
                            11 ["fixture" "development"]
                            33 ["full-corpus" "publication"]}
        adrs (into {} (map (juxt :num identity) (adr/parse-all "docs/adr")))
        ledger (files/read-edn "docs/adr/adr-claim-migration.edn")
        foundation-rows (filter (fn [[[adr _] _]] (contains? expected-ranges adr))
                                (:entries ledger))
        live-ids (set (mapcat (comp :resulting-claim-ids val) foundation-rows))]
    (is (= 35 (count foundation-rows)))
    (is (= expected-ids live-ids))
    (doseq [[adr [scope authority]] expected-lifecycle]
      (is (= scope (get-in adrs [adr :fields "Validation scope"])))
      (is (= authority (get-in adrs [adr :fields "Release authority"]))))
    (is (= expected-ids
           (set (keep :claim-id
                      (mapcat (comp :criteria adrs) (keys expected-ranges))))))))
