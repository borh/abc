(ns soranoha.assessment.fixtures
  (:require [soranoha.assessment.records :as records]))

(defn finding [id subject predicate value premises]
  {"id" id "fact" (records/fact-key subject predicate) "value" value
   "effective_date" "2020-01-01" "reviewed_at" "2020-01-01"
   "assessor" "Synthetic assessor" "method" "synthetic-reviewed-evidence"
   "basis" (str "Synthetic evidence for " id "; not a production assessment.")
   "premises" premises})

(defn observation-premise [id value]
  {"kind" "observation" "ref" id "fingerprint" (records/fingerprint value)})

(defn assessed-source
  "Synthetic completed findings for one work and its established set.
  Captures are values under observation ids bundle and catalog."
  [slug ids captures]
  (assoc records/empty-source
         "observations" [{"id" "bundle" "selector" "canonical-source-bundle" "slug" slug}
                         {"id" "catalog" "selector" "catalog-contributors" "slug" slug}]
         "findings"
         (into [(finding "complete" slug "contribution-set" (vec (sort ids))
                         [(observation-premise "bundle" (get captures "bundle"))
                          (observation-premise "catalog" (get captures "catalog"))])]
               (map (fn [id]
                      (finding (str "status-" id)
                               (records/contribution-subject slug id)
                               "contribution-status" "public-domain" []))) ids)))
