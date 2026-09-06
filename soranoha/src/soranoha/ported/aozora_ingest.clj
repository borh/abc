(ns soranoha.ported.aozora-ingest
  "Build validated metadata and person records from catalog rows."
  (:require [soranoha.ported.assets :as assets]
            [soranoha.ported.aozora-csv :as ac]
            [soranoha.ported.person-record :as person-record]
            [soranoha.ported.schema :as schema]))

(defn- schema-path []
  (assets/resolve-path "schemas/metadata-record.schema.json"))
(defn- person-schema-path []
  (assets/resolve-path "schemas/person-record.schema.json"))

(def ^:private person-body-keys
  ["person_id"
   "family_name"
   "given_name"
   "family_name_reading"
   "given_name_reading"
   "family_name_sort"
   "given_name_sort"
   "family_name_romaji"
   "given_name_romaji"
   "date_of_birth"
   "date_of_death"
   "person_copyright_expired"
   "external_links"])

(defn- build-person-record [body]
  (merge (zipmap person-body-keys (map #(get body %) person-body-keys))
         {"person_record_schema_id" "https://w3id.org/abc/schemas/person-record.schema.json"
          "person_record_schema_hash" (schema/cached-schema-hash (person-schema-path))
          "external_links" (or (get body "external_links") [])}))

(defn build-records
  "Build and validate a work metadata record and its contributor person records
  from catalog rows. Returns {:metadata-rec record :person-records {id record}}."
  [{:keys [rows work-id]}]
  (let [matching (filter #(= work-id (get % "作品ID")) rows)]
    (when-not (seq matching)
      (throw (ex-info (str "no rows for work_id " work-id " in supplied rows")
                      {:work-id work-id})))
    (let [{:keys [work persons-by-id contributors]}
          (ac/build-record-fragment-from-rows matching)
          person-records
          (into (sorted-map)
                (map (fn [[pid body]]
                       (let [record (build-person-record body)]
                         (person-record/validate! record)
                         [pid record]))
                     persons-by-id))
          contributor-entries
          (vec (for [c contributors
                     :let [pid (get c "person_id")]]
                 {"person_id" pid
                  "person_record_hash" (person-record/record-hash
                                        (get person-records pid))
                  "relation_to_work" (get c "relation_to_work")}))
          metadata-rec {"metadata_record_schema_id" "https://w3id.org/abc/schemas/metadata-record.schema.json"
                        "metadata_record_schema_hash" (schema/cached-schema-hash (schema-path))
                        "work" work
                        "contributors" (vec (sort-by #(get % "person_id")
                                                     contributor-entries))}]
      (let [errors (schema/validation-errors (schema/cached-schema (schema-path)) metadata-rec)]
        (when (seq errors)
          (throw (ex-info "generated metadata-record fails schema validation" {:errors errors}))))
      {:person-records person-records
       :metadata-rec metadata-rec})))
