(ns abc.tools.adr-evidence-inventory
  (:require [abc.tools.adr :as adr]
            [abc.tools.json :as json]
            [clojure.tools.cli :as cli]))

(def ^:private families
  ["foundation-runtime-identity" "schema-rdf-tei" "temporal-person-ingest"
   "parser-ir-publication" "diagrams-governance" "unclassified"])

(def ^:private adr-families
  {1 "foundation-runtime-identity" 2 "parser-ir-publication"
   6 "schema-rdf-tei" 7 "parser-ir-publication"
   8 "foundation-runtime-identity" 9 "foundation-runtime-identity"
   10 "foundation-runtime-identity" 11 "foundation-runtime-identity"
   12 "schema-rdf-tei" 13 "schema-rdf-tei" 14 "schema-rdf-tei"
   15 "temporal-person-ingest" 16 "temporal-person-ingest"
   17 "schema-rdf-tei" 18 "schema-rdf-tei"
   20 "temporal-person-ingest" 21 "temporal-person-ingest"
   22 "temporal-person-ingest" 23 "parser-ir-publication"
   24 "parser-ir-publication" 25 "parser-ir-publication"
   29 "diagrams-governance" 30 "parser-ir-publication"
   31 "diagrams-governance" 32 "parser-ir-publication"
   33 "foundation-runtime-identity"})

(defn- criterion-row [{:keys [num file evidence]} criterion]
  (let [index (:criterion-index criterion)]
    {"adr" num
     "file" file
     "criterion_index" index
     "body" (:body criterion)
     "claim_id" (:claim-id criterion)
     "claim_kind" (some-> (:claim-kind criterion) name)
     "evidence_paths" (->> evidence
                           (filter #(= index (:criterion-index %)))
                           (map :path) distinct sort vec)
     "family" (get adr-families num "unclassified")
     "disposition" nil}))

(defn inventory-value [adrs]
  (let [accepted (filter #(= "Accepted" (:status %)) adrs)
        rows (->> accepted
                  (mapcat (fn [item]
                            (map #(criterion-row item %) (:criteria item))))
                  (sort-by (juxt #(get % "adr") #(get % "criterion_index")))
                  vec)
        counts (frequencies (map #(get % "family") rows))]
    {"schema_version" "abc-adr-claim-migration-inventory-v1"
     "accepted_adr_count" (count accepted)
     "accepted_criterion_count" (count rows)
     "families" (into (sorted-map)
                      (map (fn [family] [family (get counts family 0)]))
                      families)
     "criteria" rows}))

(defn write-inventory! [output adrs]
  (json/write-deterministic-json-file! output (inventory-value adrs)))

(def cli-options [[nil "--output PATH"]])

(defn -main [& args]
  (let [args (if (= "--" (first args)) (rest args) args)
        {:keys [options errors]} (cli/parse-opts args cli-options)]
    (if (or (seq errors) (nil? (:output options)))
      (System/exit 2)
      (let [value (inventory-value (adr/parse-all "docs/adr"))]
        (json/write-deterministic-json-file! (:output options) value)
        (System/exit (if (pos? (get-in value ["families" "unclassified"])) 1 0))))))
