(ns ab-research.parser-rq-resource-test
  (:require [ab-research.files :as files]
            [ab-research.hash :as hash]
            [ab-research.parser-rq-resource :as resource]
            [ab-research.parser-release-qualification :as qualification]
            [clojure.test :refer [deftest is]]))

(def hash-a (str "sha256:" (apply str (repeat 64 "a"))))
(def policy {:policy_hash hash-a
             :work_ids ["a" "b"]
             :threshold_bytes 2147483648})
(deftest aggregate-is-the-exact-maximum
  (let [records [{:work_id "a" :policy_hash hash-a :status "measured"
                  :peak_cgroup_memory_bytes 10}
                 {:work_id "b" :policy_hash hash-a :status "measured"
                  :peak_cgroup_memory_bytes 20}]
        result (resource/analyze policy hash-a {:work_ids ["a" "b"]} records)]
    (is (= :measured (:status result)))
    (is (= 20 (:value result)))
    (is (= :pass (:verdict result)))))

(deftest unavailable-work-poisons-the-aggregate
  (let [records [{:work_id "a" :policy_hash hash-a :status "measured"
                  :peak_cgroup_memory_bytes 10}
                 {:work_id "b" :policy_hash hash-a :status "unavailable"
                  :reason "counter_unavailable"}]
        result (resource/analyze policy hash-a {:work_ids ["a" "b"]} records)]
    (is (= :unavailable (:status result)))
    (is (nil? (:value result)))))

(deftest ceiling-clipped-work-is-an-available-failure
  (let [records [{:work_id "a" :policy_hash hash-a :status "measured"
                  :peak_cgroup_memory_bytes 10}
                 {:work_id "b" :policy_hash hash-a :status "ceiling_clipped"
                  :peak_cgroup_memory_bytes 3221225472}]
        result (resource/analyze policy hash-a {:work_ids ["a" "b"]} records)]
    (is (= :measured (:status result)))
    (is (= :fail (:verdict result)))))

(deftest resource-observation-installs-an-envelope
  (let [envelope {:value 10 :identity_ref hash-a}]
    (is (= envelope
           (:peak_cgroup_memory_bytes
            (qualification/install-resource-observation {} envelope))))))

(def ^:private campaign-capture-root
  "The capture the committed report was projected from.

  This tracks the live projection, not a frozen run: the assertions below tie
  `docs/reports/parser-release-qualification-report.json` to the capture it
  came from, so it must move whenever a requalification replaces that report.
  The frozen historical run is pinned separately by the
  `parser-rq-p5-promotion-audit` flake check, which is where a fixed audit
  target belongs."
  (str "docs/reports/parser-rq/runs/"
       "24d61fc75b0dd2405a423ebc0a06bd3002e37b7d8b9ed4a76b2c21d57eb2b3ec"
       "/captures/"
       "e27fa29f2ed85eb68e17586b8617f24287cee9adfbb096553735027c5c1841cd"))

(deftest hinoki-resource-witness-binds-the-campaign-capture
  (let [smoke (files/read-json
               "docs/superpowers/reports/2026-07-17-parser-rq-resource-hinoki-smoke.json")
        identity (files/read-json
                  (str campaign-capture-root "/qualification-identity.json"))
        measurements (files/read-json
                      (str campaign-capture-root "/measurements.json"))
        resource (files/read-json (str campaign-capture-root "/resource.json"))
        report (files/read-json "docs/reports/parser-release-qualification-report.json")
        memory-verdict (first (filter #(= "memory" (get % "predicate_id"))
                                      (get report "predicate_verdicts")))]
    (is (= "parser-rq-resource-live-smoke-v1" (get smoke "schema_version")))
    (is (= "parser-rq-resource-v1" (get smoke "instrument_id")))
    (is (= "yes" (get-in smoke ["systemd_properties" "MemoryAccounting"])))
    (is (= 0 (get-in smoke ["systemd_properties" "MemorySwapMax"])))
    (is (= #{["measured" false] ["ceiling_clipped" true]}
           (set (map (juxt #(get % "status") #(get % "right_censored"))
                     (get smoke "cases")))))
    (is (every? #(zero? (get % "peak_swap_bytes")) (get smoke "cases")))
    (is (= 9 (count measurements)))
    (is (= #{(get-in report ["coherence" "identity_ref"])}
           (set (map #(get % "identity_ref") (vals measurements)))))
    (is (= "ok" (get-in report ["coherence" "status"])))
    (is (= [] (get-in report ["coherence" "errors"])))
    (is (= (get-in report ["identity" "predicate_set_hash"])
           (get identity "predicate_set_hash")))
    (is (= "parser-rq-resource-v1"
           (get-in identity ["instrument_versions" "peak_cgroup_memory_bytes"])))
    (is (= (get resource "peak_cgroup_memory_bytes")
           (get measurements "peak_cgroup_memory_bytes")))
    (is (= "parser-rq-resource-v1" (get memory-verdict "instrument")))
    (is (= {"comparator" "<=" "value" 2147483648} (get memory-verdict "expected")))
    (is (= (get-in resource ["peak_cgroup_memory_bytes" "value"])
           (get memory-verdict "observed")))
    (is (= {"pass" 9} (get report "verdict_tally")))))

(deftest process-tree-memory-policy-contract
  (let [predicates (files/read-edn "data/parser-release-qualification-predicates.edn")
        policy (files/read-json "data/parser-rq-resource-policy-v1.json")
        memory (first (filter #(= :memory (:predicate_id %))
                              (:predicates predicates)))]
    (is (= :peak_cgroup_memory_bytes (:observed_key memory)))
    (is (= 2147483648 (get-in memory [:expected :value])))
    (is (= 0 (get-in policy ["systemd_properties" "MemorySwapMax"])))
    (is (= (:predicate_set_hash predicates)
           (get policy "predicate_set_hash")))
    (is (= (get policy "policy_hash")
           (hash/format-sha256
            (hash/sha256-json-jcs (dissoc policy "policy_hash")))))
    (is (re-matches #"sha256:[0-9a-f]{64}"
                    (get policy "wrapper_identity_hash")))))
