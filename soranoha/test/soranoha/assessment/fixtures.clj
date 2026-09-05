(ns soranoha.assessment.fixtures
  (:require [clojure.string :as str]
            [soranoha.assessment.records :as records]))

(defn finding [id subject predicate value premises]
  {"id" id "fact" (records/fact-key subject predicate) "value" value
   "effective_date" "2020-01-01" "reviewed_at" "2020-01-01"
   "assessor" "Synthetic assessor" "method" "synthetic-reviewed-evidence"
   "basis" (str "Synthetic evidence for " id "; not a production assessment.")
   "premises" premises})

(defn observation-premise [id value]
  {"kind" "observation" "ref" id "fingerprint" (records/fingerprint value)})

(defn assessed-source
  "Synthetic established premises for a derived public-domain assessment.
  Captures are values under observation ids bundle and catalog."
  [slug ids captures]
  (let [coordinates (sort (distinct (map #(second (str/split % #":" 2)) ids)))
        work-findings [(finding "work-type" slug "work-type" "non-film-non-photo" [])
                       (finding "publication" slug "publication-timing" "lifetime" [])
                       (finding "chain" slug "derivative-chain" [] [])]
        people (map #(finding (str "death-" %) (str "person:" %) "death-year" 1900 []) coordinates)
        contributions (mapcat
                       (fn [id]
                         (let [subject (records/contribution-subject slug id)]
                           [(finding (str "attribution-" id) subject "attribution-form" "real-name" [])
                            (finding (str "wartime-" id) subject "no-wartime-addition" true [])]))
                       ids)]
    (assoc records/empty-source
           "observations" [{"id" "bundle" "selector" "canonical-source-bundle" "slug" slug}
                           {"id" "catalog" "selector" "catalog-contributors" "slug" slug}]
           "findings"
           (into [(finding "complete" slug "contribution-set" (vec (sort ids))
                           [(observation-premise "bundle" (get captures "bundle"))
                            (observation-premise "catalog" (get captures "catalog"))])]
                 (concat work-findings people contributions)))))
