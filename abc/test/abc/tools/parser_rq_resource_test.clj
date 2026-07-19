(ns abc.tools.parser-rq-resource-test
  (:require [abc.tools.files :as files]
            [abc.tools.hash :as hash]
            [abc.tools.parser-rq-resource :as resource]
            [abc.tools.parser-release-qualification :as qualification]
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
