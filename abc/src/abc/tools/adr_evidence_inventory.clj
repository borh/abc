(ns abc.tools.adr-evidence-inventory
  (:require [abc.tools.adr :as adr]
            [abc.tools.adr-claim-migration :as migration]
            [abc.tools.cli :as abc-cli]
            [abc.tools.json :as json]))

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

(defn- keyword-name [value]
  (some-> value name))

(defn- ledger-fields [entry]
  {"disposition" (keyword-name (:disposition entry))
   "disposition_rationale" (:rationale entry)
   "planned_evidence_boundaries" (mapv keyword-name (:planned-evidence-boundaries entry []))
   "resulting_claim_ids" (vec (:resulting-claim-ids entry []))})

(defn- criterion-row [{:keys [num file evidence]} criterion baseline-key entry]
  (let [index (:criterion-index criterion)]
    (merge
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
      "baseline_key" baseline-key}
     (ledger-fields entry))))

(defn- baseline-row [row entry]
  (let [key [(get row "adr") (get row "original_text_hash")]]
    (merge row
           {"family" (get adr-families (get row "adr") "unclassified")
            "baseline_key" key}
           (ledger-fields entry))))

(defn inventory-value [adrs migration-state]
  (let [accepted (filter #(= "Accepted" (:status %)) adrs)
        by-key (:by-key migration-state)
        baseline-rows (mapv (fn [row]
                              (let [key [(get row "adr") (get row "original_text_hash")]]
                                (baseline-row row (get by-key key))))
                            (get-in migration-state [:baseline "criteria"] []))
        claim-to-key (into {}
                           (mapcat (fn [[key entry]]
                                     (map #(vector % key) (:resulting-claim-ids entry))))
                           by-key)
        rows (->> accepted
                  (mapcat (fn [item]
                            (keep (fn [criterion]
                                    (let [exact-key [(:num item)
                                                     (migration/criterion-text-hash (:body criterion))]
                                          key (or (when (contains? by-key exact-key) exact-key)
                                                  (get claim-to-key (:claim-id criterion)))
                                          entry (get by-key key)]
                                      (when-not (= :move-out-of-acceptance (:disposition entry))
                                        (criterion-row item criterion key entry))))
                                  (:criteria item))))
                  (sort-by (juxt #(get % "adr") #(get % "criterion_index")))
                  vec)
        counts (frequencies (map #(get % "family") rows))]
    {"schema_version" "abc-adr-claim-migration-inventory-v1"
     "baseline_revision" (get-in migration-state [:baseline "baseline_revision"])
     "baseline_criterion_count" (count baseline-rows)
     "baseline_criteria" baseline-rows
     "accepted_adr_count" (count accepted)
     "accepted_criterion_count" (count rows)
     "families" (into (sorted-map)
                      (map (fn [family] [family (get counts family 0)]))
                      families)
     "criteria" rows}))

(defn write-inventory! [output adrs migration-state]
  (json/write-deterministic-json-file! output (inventory-value adrs migration-state)))

(def cli-options [[nil "--output PATH"]])

(defn usage [_]
  "Usage: clojure -M:abc/adr-evidence-inventory --output PATH")

(def cli-config
  {:cli-options cli-options
   :required [:output]
   :max-args 0
   :usage-fn usage
   :run (fn [{:keys [options]}]
          (let [migration-state (migration/load-migration-state "." {:require-complete? false})
                value (inventory-value (adr/parse-all "docs/adr") migration-state)]
            (json/write-deterministic-json-file! (:output options) value)
            {:ok? (and (empty? (:problems migration-state))
                       (zero? (get-in value ["families" "unclassified"])))}))
   :fail? (complement :ok?)})

(defn -main [& args]
  (abc-cli/run-cli! args cli-config))
