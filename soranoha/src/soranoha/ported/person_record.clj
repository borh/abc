(ns soranoha.ported.person-record
  "Identity and validation for person records. record-hash hashes the
  canonical-identity form: schema_id and schema_hash included;
  source_csv_provenance excluded; nullable fields explicit as JSON null.
  Copied from abc.tools.person-record with the RDF/Turtle mapping stripped
  (the kernel never renders RDF; the stripped functions were the only
  consumers of the Jena stack)."
  (:require [soranoha.ported.jcs :as jcs]
            [soranoha.core.hash :as hash]
            [soranoha.ported.schema :as schema])
  (:import [java.time DateTimeException LocalDate]))

(def ^:private full-date-shape-pattern #"^-?\d{4}-\d{2}-\d{2}$")

(defn- assert-calendar-valid!
  "Reject records whose YYYY-MM-DD date fields are regex-shaped but
  calendar-impossible (e.g. 2020-02-31, -0426-02-31). The JSON Schema
  regex only bounds month 01-12 and day 01-31; full calendar validity
  needs a date library. java.time accepts signed negative years, so
  this gate covers BCE full dates too. Partial dates (YYYY, YYYY-MM)
  skip this check — see ADR 0015."
  [record]
  (doseq [field ["date_of_birth" "date_of_death"]
          :let [v (get record field)]
          :when (and v (re-matches full-date-shape-pattern v))]
    (try (LocalDate/parse ^String v)
         (catch DateTimeException _
           (throw (ex-info (str "person-record validation failed: " field
                                " is not a valid calendar date: " v)
                           {:field field :value v}))))))

(defn validate!
  "Validate `record` against the supplied person schema plus a
  calendar-validity check for full-date shapes. Returns :ok on success;
  throws ex-info on failure with both :errors (structured error maps) and
  :errors-humanized (readable strings) for schema failures, or with
  :field/:value for calendar-validity failures."
  [person-schema record]
  (let [[errors humanized] (schema/validation-errors-humanized
                            person-schema record)]
    (when (seq errors)
      (throw (ex-info "person-record validation failed"
                      {:errors errors
                       :errors-humanized humanized}))))
  (assert-calendar-valid! record)
  :ok)

(defn- canonical-identity-form
  "Returns the JSON value used as input to record-hash. Drops
  source_csv_provenance."
  [record]
  (dissoc record "source_csv_provenance"))

(defn record-hash
  "Compute sha256:<hex> over the canonical-identity form of `record`
  using the record JSON canonicalization."
  [record]
  (hash/format-sha256
   (hash/sha256-bytes (jcs/canonical-json-bytes (canonical-identity-form record)))))
