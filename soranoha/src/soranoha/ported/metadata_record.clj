(ns soranoha.ported.metadata-record
  "Identity and validation for metadata records. record-hash hashes the
  canonical-identity form: schema_id and schema_hash included;
  source_csv_provenance excluded; persons[] sorted by person_id; nullable
  fields explicit as JSON null. Copied from abc.tools.metadata-record with
  the RDF/Turtle mapping stripped: the kernel never renders RDF, and the
  stripped functions were the only consumers of the Jena stack."
  (:require [soranoha.ported.assets :as assets]
            [soranoha.ported.hash :as hash]
            [soranoha.ported.manifest :as manifest]
            [soranoha.ported.schema :as schema]))

(defn- schema-path []
  (assets/resolve-path "schemas/metadata-record.schema.json"))
(def schema-id "https://w3id.org/abc/schemas/metadata-record.schema.json")

(defn validate!
  "Validate `record` against schemas/metadata-record.schema.json.
  Returns :ok on success; throws ex-info on failure with both :errors
  (raw m3 vector) and :errors-humanized (readable strings)."
  [record]
  (let [[errors humanized] (schema/validation-errors-humanized
                            (schema/cached-schema (schema-path)) record)]
    (if (seq errors)
      (throw (ex-info "metadata-record validation failed"
                      {:errors errors
                       :errors-humanized humanized}))
      :ok)))

(defn- canonical-identity-form
  "Returns the JSON value used as input to record-hash. Drops
  source_csv_provenance and sorts contributors[] by person_id."
  [record]
  (-> record
      (dissoc "source_csv_provenance")
      (update "contributors"
              (fn [cs] (vec (sort-by #(get % "person_id") cs))))))

(defn record-hash
  "Compute sha256:<hex> over the canonical-identity form of `record`
  using RFC 8785 JCS via soranoha.ported.hash/sha256-json-jcs."
  [record]
  (hash/format-sha256
   (hash/sha256-json-jcs (canonical-identity-form record))))

(defn build-metadata-record
  "Construct an immutable metadata record from {:work, :contributors}.
  Populates the self-describing schema fields. Contributors are sorted
  by person_id. Caller is expected to validate! the result before
  hashing or shipping."
  [{:keys [work contributors]}]
  {"metadata_record_schema_id" schema-id
   "metadata_record_schema_hash" (manifest/schema-hash (schema-path))
   "work" work
   "contributors" (vec (sort-by #(get % "person_id") contributors))})
