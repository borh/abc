(ns soranoha.aozora.person-record
  "Identity and validation for person records. record-hash hashes the
  canonical-identity form: schema_id and schema_hash included;
  source_csv_provenance excluded; nullable fields explicit as JSON null."
  (:require [soranoha.aozora.csv :as csv]
            [soranoha.core.jcs :as jcs]
            [soranoha.core.hash :as hash]
            [soranoha.core.schema :as schema]))

(defn- assert-calendar-valid!
  "Reject a record whose date field passes the schema regex but names a
  day that does not exist. The schema bounds month and day separately;
  the calendar check needs a date library."
  [record]
  (doseq [field ["date_of_birth" "date_of_death"]
          :let [v (get record field)]
          :when (not (csv/calendar-valid? v))]
    (throw (ex-info (str "person-record validation failed: " field
                         " is not a valid calendar date: " v)
                    {:field field :value v}))))

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
