(ns abc.tools.adr-evidence-inventory-test
  (:require [abc.tools.adr :as adr]
            [abc.tools.adr-claim-migration :as migration]
            [abc.tools.adr-evidence-inventory :as inventory]
            [abc.tools.cli :as cli]
            [abc.tools.files :as files]
            [abc.tools.json :as json]
            [babashka.fs :as fs]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]))

(def retained-text "Criterion text.")
(def corrected-text "Correct this.")
(def moved-text "Move this.")

(defn- dispatch [args]
  (cli/dispatch! (cli/parse args inventory/cli-config)
                 inventory/cli-config))

(deftest cli-preserves-usage-success-and-debt-exit-codes-test
  (let [dir (fs/create-temp-dir {:prefix "adr-evidence-inventory-cli-"})
        output (fs/file dir "inventory.json")
        state {:problems []}]
    (is (= 2 (dispatch [])))
    (with-redefs [migration/load-migration-state (fn [& _] state)
                  adr/parse-all (constantly [])
                  inventory/inventory-value
                  (fn [& _] {"families" {"unclassified" 0}})]
      (is (= 0 (dispatch ["--output" (str output)])))
      (is (fs/regular-file? output)))
    (with-redefs [migration/load-migration-state
                  (fn [& _] {:problems [{:kind :migration-debt}]})
                  adr/parse-all (constantly [])
                  inventory/inventory-value
                  (fn [& _] {"families" {"unclassified" 0}})]
      (is (= 1 (dispatch ["--output" (str output)]))))))

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

(deftest schema-rdf-tei-stage-a-claim-and-lifecycle-contract-test
  (let [expected-ranges {6 9, 12 6, 13 3, 14 4, 17 5, 18 3}
        expected-ids (set (mapcat (fn [[adr count]]
                                    (map #(format "ADR-%04d-C%d" adr %)
                                         (range 1 (inc count))))
                                  expected-ranges))
        expected-lifecycle {6 ["fixture" "development"]
                            12 ["fixture" "publication"]
                            13 ["fixture" "publication"]
                            14 ["fixture" "publication"]
                            17 ["fixture" "publication"]
                            18 ["fixture" "none"]}
        adrs (into {} (map (juxt :num identity) (adr/parse-all "docs/adr")))
        ledger (files/read-edn "docs/adr/adr-claim-migration.edn")
        family-rows (filter (fn [[[adr _] _]] (contains? expected-ranges adr))
                            (:entries ledger))
        live-ids (set (mapcat (comp :resulting-claim-ids val) family-rows))]
    (is (= 31 (count family-rows)))
    (is (= expected-ids live-ids))
    (doseq [[adr [scope authority]] expected-lifecycle]
      (is (= scope (get-in adrs [adr :fields "Validation scope"])))
      (is (= authority (get-in adrs [adr :fields "Release authority"]))))
    (is (= expected-ids
           (set (keep :claim-id
                      (mapcat (comp :criteria adrs) (keys expected-ranges))))))))

(deftest temporal-person-ingest-stage-a-claim-ledger-and-inventory-contract-test
  (let [expected-ranges {15 6, 16 4, 20 4, 21 12, 22 6}
        expected-ids (set (mapcat (fn [[adr count]]
                                    (map #(format "ADR-%04d-C%d" adr %)
                                         (range 1 (inc count))))
                                  expected-ranges))
        expected-lifecycle {15 ["fixture" "publication"]
                            16 ["fixture" "publication"]
                            20 ["fixture" "publication"]
                            21 ["fixture" "publication"]
                            22 ["operational" "development"]}
        adrs (into {} (map (juxt :num identity) (adr/parse-all "docs/adr")))
        state (migration/load-migration-state "." {:require-complete? false})
        value (inventory/inventory-value (vals adrs) state)
        family-rows (filter (fn [[[adr _] _]] (contains? expected-ranges adr))
                            (get-in state [:ledger :entries]))
        baseline-rows (filter #(contains? expected-ranges (get % "adr"))
                              (get value "baseline_criteria"))]
    (is (not-any? #(and (= :duplicate-resulting-claim-id (:kind %))
                        (contains? expected-ids (:claim-id %)))
                  (:problems state)))
    (is (= 36 (count family-rows)))
    (is (= 36 (count baseline-rows)))
    (is (every? some? (map #(get % "disposition") baseline-rows)))
    (is (= expected-ids
           (set (mapcat (comp :resulting-claim-ids val) family-rows))))
    (is (= 32 (get-in value ["families" "temporal-person-ingest"])))
    (is (= expected-ids
           (->> (get value "criteria")
                (filter #(= "temporal-person-ingest" (get % "family")))
                (map #(get % "claim_id"))
                set)))
    (doseq [[adr [scope authority]] expected-lifecycle]
      (is (= scope (get-in adrs [adr :fields "Validation scope"])))
      (is (= authority (get-in adrs [adr :fields "Release authority"]))))))

(deftest parser-ir-publication-live-adr-classification-test
  (let [adrs (adr/parse-all "docs/adr")
        accepted (filter #(= "Accepted" (:status %)) adrs)
        state (migration/load-migration-state "." {:require-complete? false})
        value (inventory/inventory-value adrs state)]
    (is (= 26 (get value "baseline_adr_count"))
        "the immutable baseline contains 26 ADRs")
    (is (= 29 (count accepted))
        "ADRs 0034, 0038, and 0041 are the additional live Accepted ADRs")
    (is (zero? (get-in value ["families" "unclassified"])))
    (is (= "parser-ir-publication"
           (get (first (filter #(= 38 (get % "adr"))
                               (get value "criteria")))
                "family")))))

(deftest diagrams-governance-stage-a-claim-ledger-and-lifecycle-contract-test
  (let [expected-ranges {29 5, 31 4, 34 3}
        baseline-ranges {29 5, 31 4}
        expected-ids (set (mapcat (fn [[adr count]]
                                    (map #(format "ADR-%04d-C%d" adr %)
                                         (range 1 (inc count))))
                                  expected-ranges))
        baseline-ids (set (mapcat (fn [[adr count]]
                                    (map #(format "ADR-%04d-C%d" adr %)
                                         (range 1 (inc count))))
                                  baseline-ranges))
        expected-lifecycle {29 ["fixture" "none"]
                            31 ["structural" "none"]
                            34 ["full-corpus" "none"]}
        adrs (into {} (map (juxt :num identity) (adr/parse-all "docs/adr")))
        state (migration/load-migration-state "." {:require-complete? false})
        value (inventory/inventory-value (vals adrs) state)
        family-rows (filter (fn [[[adr _] _]] (contains? expected-ranges adr))
                            (get-in state [:ledger :entries]))
        baseline-rows (filter #(contains? expected-ranges (get % "adr"))
                              (get value "baseline_criteria"))]
    (is (= 8 (count family-rows)))
    (is (= 8 (count baseline-rows)))
    (is (every? some? (map #(get % "disposition") baseline-rows)))
    (is (= baseline-ids
           (set (mapcat (comp :resulting-claim-ids val) family-rows))))
    (is (= expected-ids
           (->> (get value "criteria")
                (filter #(= "diagrams-governance" (get % "family")))
                (map #(get % "claim_id"))
                set)))
    (doseq [[adr [scope authority]] expected-lifecycle]
      (is (= scope (get-in adrs [adr :fields "Validation scope"])))
      (is (= authority (get-in adrs [adr :fields "Release authority"]))))))

(deftest adr-0034-family-rows-appear-only-after-acceptance-test
  (let [criteria (mapv (fn [n]
                         {:criterion-index (dec n)
                          :body (str "Claim " n ".")
                          :claim-id (str "ADR-0034-C" n)
                          :claim-kind (if (= n 3)
                                        :corpus-behavior
                                        :structural-invariant)})
                       [1 2 3])
        proposed {:num 34 :file "0034-closure.md" :status "Proposed"
                  :criteria criteria :evidence []}
        state {:baseline {"baseline_revision" (apply str (repeat 40 "0"))
                          "criteria" []}
               :ledger {:entries {}} :by-key {} :problems []}
        proposed-value (inventory/inventory-value [proposed] state)
        accepted-value (inventory/inventory-value [(assoc proposed :status "Accepted")]
                                                  state)
        rows (get accepted-value "criteria")]
    (is (empty? (get proposed-value "criteria")))
    (is (= 3 (count rows)))
    (is (every? #(= "diagrams-governance" (get % "family")) rows))
    (is (zero? (get-in accepted-value ["families" "unclassified"])))))
