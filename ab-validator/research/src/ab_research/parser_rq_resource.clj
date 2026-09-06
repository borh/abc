(ns ab-research.parser-rq-resource
  "Pure authentication and folding for process-tree memory observations."
  (:require [clojure.walk :as walk]))

(defn- keywordize [value]
  (walk/keywordize-keys value))

(defn- unavailable [reason]
  {:status :unavailable :value nil :verdict :unavailable :reason reason})

(defn analyze
  "Authenticate a closed record set and derive the exact maximum in bytes."
  [policy qualification-identity-ref index records]
  (let [policy (keywordize policy)
        index (keywordize index)
        records (mapv keywordize records)
        expected (:work_ids policy)
        actual (mapv :work_id records)
        acceptable? #(contains? #{"measured" "ceiling_clipped"} (:status %))]
    (cond
      (not= expected (:work_ids index)) (unavailable "index membership differs from policy")
      (not= expected actual) (unavailable "record membership differs from policy")
      (not= (count actual) (count (set actual))) (unavailable "duplicate work record")
      (some #(not= (:policy_hash policy) (:policy_hash %)) records)
      (unavailable "work policy identity mismatch")
      (some (complement acceptable?) records)
      (unavailable "one or more resource works are unavailable")
      (not (every? #(integer? (:peak_cgroup_memory_bytes %)) records))
      (unavailable "one or more resource counters are invalid")
      :else
      (let [peak (apply max (map :peak_cgroup_memory_bytes records))
            threshold (:threshold_bytes policy)]
        {:status :measured
         :value peak
         :identity_ref qualification-identity-ref
         :verdict (if (<= peak threshold) :pass :fail)
         :counts {:expected (count expected)
                  :measured (count records)
                  :ceiling_clipped (count (filter #(= "ceiling_clipped" (:status %)) records))}
         :witnesses (mapv #(select-keys % [:work_id :status :peak_cgroup_memory_bytes]) records)}))))

(defn observation-envelope [analysis]
  (if (= :unavailable (:status analysis))
    (select-keys analysis [:status :reason])
    (select-keys analysis [:value :identity_ref])))
