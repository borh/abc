(ns abc.tools.person-record
  "Identity, validation, and RDF mapping for ABC person records.
  record-hash hashes the canonical-identity form: schema_id and
  schema_hash included; source_csv_provenance excluded; nullable
  fields explicit as JSON null. record->graph renders a single
  Person as FOAF + RDA Group 2 triples."
  (:require [abc.tools.files :as files]
            [abc.tools.hash :as hash]
            [abc.tools.schema :as schema]))

(def schema-path "schemas/person-record.schema.json")
(def schema-id "https://w3id.org/abc/schemas/person-record.schema.json")

(defn validate!
  "Validate `record` against schemas/person-record.schema.json.
  Returns :ok on success; throws ex-info with :errors on failure."
  [record]
  (let [s (files/read-json schema-path)
        errors (schema/validation-errors s record)]
    (if (seq errors)
      (throw (ex-info "person-record validation failed"
                      {:errors errors}))
      :ok)))

(defn- canonical-identity-form
  "Returns the JSON value used as input to record-hash. Drops
  source_csv_provenance."
  [record]
  (dissoc record "source_csv_provenance"))

(defn record-hash
  "Compute sha256:<hex> over the canonical-identity form of `record`
  using RFC 8785 JCS via abc.tools.hash/sha256-json-jcs."
  [record]
  (hash/format-sha256
   (hash/sha256-json-jcs (canonical-identity-form record))))
