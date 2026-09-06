(ns ab-research.parser-rq-core-attempt-test
  (:require [ab-research.files :as files]
            [ab-research.hash :as hash]
            [charred.api :as charred]
            [soranoha.core.json :as record-json]
            [ab-research.parser-rq-core-attempt :as core]
            [ab-research.parser-release-qualification :as qualification]
            [clojure.test :refer [deftest is testing]]))

(def hash-a (str "sha256:" (apply str (repeat 64 "a"))))
(def hash-b (str "sha256:" (apply str (repeat 64 "b"))))
(def hash-c (str "sha256:" (apply str (repeat 64 "c"))))

(def candidate {:candidate_ref hash-a :qualification_identity_ref hash-b})
(def policy-base {:qualification_identity_ref hash-b
                  :expected_work_ids ["a" "b" "c"]
                  :expected_sources [{:work_id "a" :source_sha256 hash-a}
                                     {:work_id "b" :source_sha256 hash-b}
                                     {:work_id "c" :source_sha256 hash-c}]
                  :repetitions 3
                  :reduction "maximum"})
(def policy (assoc policy-base :policy_hash (core/policy-hash policy-base)))

(defn- json-bytes [value]
  (.getBytes (str (record-json/write-deterministic-json-str value) "\n") "UTF-8"))

(defn- add-blob [store locator value]
  (let [bytes (if (bytes? value) value (json-bytes value))
        ref {:sha256 (hash/format-sha256 (hash/sha256-bytes bytes))
             :bytes (alength bytes)
             :media_type (if (bytes? value) "text/plain" "application/json")
             :locator locator}]
    [(assoc store locator bytes) ref]))

(defn- fixture
  ([] (fixture [["fatal_error" "fatal_error" "parsed"]
                ["parsed" "parsed" "parsed"]
                ["adapter_timeout" "fatal_error" "parsed"]]
               ["10.0\n" "12.5\n" "11.0\n"]))
  ([dispositions elapsed-records]
   (loop [repetition 1
          store {}
          attempts []
          records []]
     (if (> repetition 3)
       {:store store
        :index {:qualification_identity_ref hash-b
                :candidate_ref hash-a
                :policy_hash (:policy_hash policy)
                :expected_work_ids ["a" "b" "c"]
                :repetitions 3
                :reduction "maximum"
                :attempts attempts
                :records records}}
       (let [[store elapsed-ref]
             (add-blob store
                       (str "timing/" repetition ".txt")
                       (.getBytes (nth elapsed-records (dec repetition)) "UTF-8"))
             attempt {:repetition repetition
                      :elapsed_record elapsed-ref
                      :argv ["ab-check" "--batch"]
                      :exit_status 0
                      :lock_retained true}
             [store records]
             (reduce
              (fn [[store records] [work-id source-sha disposition]]
                (let [[store report-ref]
                      (add-blob store
                                (str "reports/" repetition "/" work-id ".json")
                                {:work_id work-id :results {:fixture {:pass true}}})
                      record {:work_id work-id
                              :source_sha256 source-sha
                              :qualification_identity_ref hash-b
                              :candidate_ref hash-a
                              :policy_hash (:policy_hash policy)
                              :repetition repetition
                              :status "measured"
                              :disposition disposition
                              :report report-ref}
                      [store record-ref]
                      (add-blob store
                                (str "records/" repetition "/" work-id ".json")
                                record)]
                  [store (conj records {:work_id work-id
                                        :repetition repetition
                                        :record record-ref})]))
              [store records]
              (map vector ["a" "b" "c"] [hash-a hash-b hash-c]
                   (nth dispositions (dec repetition))))]
         (recur (inc repetition) store (conj attempts attempt) records))))))

(defn- blob-reader [store]
  (fn [{:keys [sha256 bytes locator]}]
    (if-let [raw (get store locator)]
      (if (and (= bytes (alength raw))
               (= sha256 (hash/format-sha256 (hash/sha256-bytes raw))))
        {:status :ok :bytes raw}
        {:status :unavailable :reason "blob logical identity mismatch"})
      {:status :unavailable :reason "blob absent"})))

(deftest authenticates-and-reduces-three-closed-repetitions
  (let [{:keys [store index]} (fixture)
        authenticated (core/authenticate-index policy candidate index
                                               (blob-reader store))
        aggregate (core/derive-aggregate authenticated)
        envelopes (core/observation-envelopes candidate aggregate)]
    (is (= :ok (:status authenticated)))
    (is (= {:fatal_failures 2.0
            :wall_time_seconds 12.5
            :timeouts 1.0}
           (select-keys aggregate
                        [:fatal_failures :wall_time_seconds :timeouts])))
    (is (= #{:fatal_failures :wall_time_seconds :timeouts}
           (set (keys envelopes))))
    (is (every? double? (map :value (vals envelopes))))))

(deftest authentication-and-reduction-ignore-record-order
  (let [{:keys [store index]} (fixture)
        analyze #(-> (core/authenticate-index policy candidate % (blob-reader store))
                     core/derive-aggregate
                     (select-keys [:fatal_failures :wall_time_seconds :timeouts]))]
    (is (= (analyze index) (analyze (update index :records reverse))))))

(deftest core-observations-use-the-live-comparator-types
  (let [{:keys [store index]}
        (fixture (repeat 3 ["parsed" "parsed" "parsed"])
                 ["10.0\n" "12.5\n" "11.0\n"])
        envelopes (->> (core/authenticate-index policy candidate index
                                                (blob-reader store))
                       core/derive-aggregate
                       (core/observation-envelopes candidate))
        predicates (->> (qualification/load-predicates)
                        :predicates
                        (filter #(contains? (set (keys envelopes))
                                            (:observed_key %))))]
    (is (= #{:pass}
           (set (map #(-> (qualification/evaluate-predicate % envelopes)
                          :verdict)
                     predicates))))
    (is (every? double? (map :value (vals envelopes))))))

(deftest authentication-fails-closed-on-index-and-blob-drift
  (let [{:keys [store index]} (fixture)
        authenticate #(core/authenticate-index policy candidate %
                                               (blob-reader store))
        first-record (get-in index [:records 0])]
    (doseq [[label invalid]
            [["missing" (update index :records #(remove (fn [row] (= row first-record)) %))]
             ["extra" (update index :records conj
                              (assoc first-record :work_id "extra"))]
             ["duplicate" (update index :records conj first-record)]
             ["cross-candidate" (assoc index :candidate_ref hash-b)]
             ["incomplete repetition" (update index :attempts pop)]
             ["command failure" (assoc-in index [:attempts 0 :exit_status] 1)]]]
      (testing label
        (is (= :unavailable (:status (authenticate invalid))))))
    (testing "bad logical hash"
      (is (= :unavailable
             (:status
              (authenticate (assoc-in index [:records 0 :record :sha256]
                                      hash-a))))))))

(deftest authentication-fails-closed-on-record-and-timing-protocol-errors
  (let [{:keys [store index]} (fixture)
        record-locator (get-in index [:records 0 :record :locator])
        timing-locator (get-in index [:attempts 0 :elapsed_record :locator])
        authenticate (fn [store]
                       (core/authenticate-index policy candidate index
                                                (blob-reader store)))]
    (testing "unknown disposition"
      (let [record (-> (get store record-locator)
                       (String. "UTF-8")
                       charred/read-json
                       (assoc "disposition" "unknown"))]
        (is (= :unavailable
               (:status (authenticate (assoc store record-locator
                                             (json-bytes record))))))))
    (testing "malformed elapsed value"
      (is (= :unavailable
             (:status (authenticate (assoc store timing-locator
                                           (.getBytes "1e3\n" "UTF-8")))))))))

(deftest committed-policy-is-generated-from-closed-reviewed-values
  (let [committed (files/read-json "data/parser-rq-core-attempt-policy-v1.json")]
    (is (= 3 (get committed "repetitions")))
    (is (= "maximum" (get committed "reduction")))
    (is (= (get committed "policy_hash")
           (core/policy-hash (dissoc committed "policy_hash"))))
    (is (= (set (get committed "expected_work_ids"))
           (set (map #(get % "work_id") (get committed "expected_sources")))))))
