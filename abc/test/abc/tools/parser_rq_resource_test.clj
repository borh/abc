(ns abc.tools.parser-rq-resource-test
  (:require [abc.tools.parser-rq-resource :as resource]
            [abc.tools.parser-release-qualification :as qualification]
            [clojure.test :refer [deftest is]]))

(def hash-a (str "sha256:" (apply str (repeat 64 "a"))))
(def policy {:policy_hash hash-a
             :work_ids ["a" "b"]
             :threshold_bytes 2147483648})
(def qualification-identity {:identity_ref hash-a})

(deftest aggregate-is-the-exact-maximum
  (let [records [{:work_id "a" :policy_hash hash-a :status "measured"
                  :peak_cgroup_memory_bytes 10}
                 {:work_id "b" :policy_hash hash-a :status "measured"
                  :peak_cgroup_memory_bytes 20}]
        result (resource/analyze policy qualification-identity {:work_ids ["a" "b"]} records)]
    (is (= :measured (:status result)))
    (is (= 20 (:value result)))
    (is (= :pass (:verdict result)))))

(deftest unavailable-work-poisons-the-aggregate
  (let [records [{:work_id "a" :policy_hash hash-a :status "measured"
                  :peak_cgroup_memory_bytes 10}
                 {:work_id "b" :policy_hash hash-a :status "unavailable"
                  :reason "counter_unavailable"}]
        result (resource/analyze policy qualification-identity {:work_ids ["a" "b"]} records)]
    (is (= :unavailable (:status result)))
    (is (nil? (:value result)))))

(deftest ceiling-clipped-work-is-an-available-failure
  (let [records [{:work_id "a" :policy_hash hash-a :status "measured"
                  :peak_cgroup_memory_bytes 10}
                 {:work_id "b" :policy_hash hash-a :status "ceiling_clipped"
                  :peak_cgroup_memory_bytes 3221225472}]
        result (resource/analyze policy qualification-identity {:work_ids ["a" "b"]} records)]
    (is (= :measured (:status result)))
    (is (= :fail (:verdict result)))))

(deftest resource-observation-installs-an-envelope
  (let [envelope {:value 10 :identity_ref hash-a}]
    (is (= envelope
           (:peak_cgroup_memory_bytes
            (qualification/install-resource-observation {} envelope))))))
