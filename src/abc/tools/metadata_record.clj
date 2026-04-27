(ns abc.tools.metadata-record
  "Identity, validation, and (in Task 5) RDF mapping for ABC metadata
  records. record-hash hashes the canonical-identity form: schema_id
  and schema_hash included; source_csv_provenance excluded; persons[]
  sorted by person_id; nullable fields explicit as JSON null."
  (:require [abc.tools.files :as files]
            [abc.tools.hash :as hash]
            [abc.tools.manifest :as manifest]
            [abc.tools.schema :as schema]))

(def schema-path "schemas/metadata-record.schema.json")
(def schema-id "https://w3id.org/abc/schemas/metadata-record.schema.json")

(defn validate!
  "Validate `record` against schemas/metadata-record.schema.json.
  Returns :ok on success; throws ex-info with :errors on failure."
  [record]
  (let [s (files/read-json schema-path)
        errors (schema/validation-errors s record)]
    (if (seq errors)
      (throw (ex-info "metadata-record validation failed"
                      {:errors errors}))
      :ok)))

(defn- canonical-identity-form
  "Returns the JSON value used as input to record-hash. Drops
  source_csv_provenance and sorts persons[] by person_id."
  [record]
  (-> record
      (dissoc "source_csv_provenance")
      (update "persons" (fn [persons]
                          (vec (sort-by #(get % "person_id") persons))))))

(defn record-hash
  "Compute sha256:<hex> over the canonical-identity form of `record`
  using RFC 8785 JCS via abc.tools.hash/sha256-json-jcs."
  [record]
  (hash/format-sha256
   (hash/sha256-json-jcs (canonical-identity-form record))))

(defn build-metadata-record
  "Construct an immutable metadata record from {:work, :persons}.
  Populates the self-describing schema fields. Persons are sorted by
  person_id. Caller is expected to validate! the result before
  hashing or shipping."
  [{:keys [work persons]}]
  {"metadata_record_schema_id" schema-id
   "metadata_record_schema_hash" (manifest/schema-hash schema-path)
   "work" work
   "persons" (vec (sort-by #(get % "person_id") persons))})
