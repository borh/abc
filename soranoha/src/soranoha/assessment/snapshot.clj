(ns soranoha.assessment.snapshot
  "Project currently applicable assessments to the frozen publication snapshot."
  (:require [clojure.string :as str]
            [soranoha.assessment.records :as records]
            [soranoha.snh.decode :as decode]
            [soranoha.za.scaffold :as scaffold]))

(defn- fact [result]
  (if (= "available" (:state result))
    {"status" (:value result) "jurisdiction" "jp"
     "effective_date" (:effective-date result)
     "basis" (str/join "\n" (distinct (map #(get % "text") (:basis result))))}
    scaffold/not-evaluated))

(defn encode [{:keys [facts candidates reliances]}]
  (decode/encode
   "assessment-snapshot"
   {"schema" "snh-assessment-snapshot/2"
    "candidates"
    (mapv (fn [[slug provisional]]
            (if-let [reliance (get reliances slug)]
              {"slug" slug "reliance" reliance}
              (let [complete (get facts (records/fact-key slug "contribution-set"))
                    established? (= "available" (:state complete))
                    ids (if established? (:value complete) provisional)]
                {"slug" slug
                 "work_assessment" (if established?
                                     (fact (get facts (records/fact-key slug "work-status")))
                                     scaffold/not-evaluated)
                 "contributions"
                 (mapv (fn [id]
                         (assoc (if established?
                                  (fact (get facts (records/fact-key
                                                    (records/contribution-subject slug id)
                                                    "contribution-status")))
                                  scaffold/not-evaluated)
                                "contribution_id" id))
                       (sort ids))})))
          (sort-by key candidates))}))
