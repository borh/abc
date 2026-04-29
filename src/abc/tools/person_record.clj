(ns abc.tools.person-record
  "Identity, validation, and RDF mapping for ABC person records.
  record-hash hashes the canonical-identity form: schema_id and
  schema_hash included; source_csv_provenance excluded; nullable
  fields explicit as JSON null. record->graph renders a single
  Person as FOAF + RDA Group 2 triples plus an EDTF-typed echo of
  the canonical date string per ADR 0015."
  (:require [abc.tools.files :as files]
            [abc.tools.hash :as hash]
            [abc.tools.malli :as am]
            [abc.tools.manifest-to-rdf :as manifest-to-rdf]
            [abc.tools.rdf-prefixes :as rdf-prefixes]
            [abc.tools.schema :as schema]
            [arachne.aristotle :as aa])
  (:import [java.time DateTimeException LocalDate]
           [org.apache.jena.datatypes BaseDatatype]
           [org.apache.jena.datatypes.xsd XSDDatatype]
           [org.apache.jena.graph NodeFactory]))

(def schema-path "schemas/person-record.schema.json")
(def schema-id "https://w3id.org/abc/schemas/person-record.schema.json")

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
  "Validate `record` against schemas/person-record.schema.json plus a
  calendar-validity check for full-date shapes. Returns :ok on success;
  throws ex-info on failure with both :errors (raw m3 vector) and
  :errors-humanized (readable strings) for schema failures, or with
  :field/:value for calendar-validity failures."
  [record]
  (let [[errors humanized] (schema/validation-errors-humanized
                            (am/cached-schema schema-path) record)]
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
  using RFC 8785 JCS via abc.tools.hash/sha256-json-jcs."
  [record]
  (hash/format-sha256
   (hash/sha256-json-jcs (canonical-identity-form record))))

;; ---------------------------------------------------------------------------
;; RDF mapping
;; ---------------------------------------------------------------------------

(defn person-iri
  "Existing Aozora LOD uses http://www.aozora.gr.jp/index_pages/personNNNNNN.html
  as the person IRI."
  [person-id]
  (str "http://www.aozora.gr.jp/index_pages/person" person-id ".html"))

(def ^:private full-date-pattern  #"-?\d{4}-\d{2}-\d{2}")
(def ^:private year-month-pattern #"-?\d{4}-\d{2}")
(def ^:private year-pattern       #"-?\d{4}")

(def ^:private edtf-datatype
  (BaseDatatype. "https://w3id.org/abc/EDTF"))

(defn- date-literal-for
  "Pick the most-precise XSD literal for a v0 EDTF lexical value
  (`YYYY[-MM[-DD]]`, optional leading `-`). Returns nil for nil.
  Throws on shape mismatch — schema validation should have caught it."
  [s]
  (when s
    (cond
      (re-matches full-date-pattern s)
      (NodeFactory/createLiteral ^String s XSDDatatype/XSDdate)
      (re-matches year-month-pattern s)
      (NodeFactory/createLiteral ^String s XSDDatatype/XSDgYearMonth)
      (re-matches year-pattern s)
      (NodeFactory/createLiteral ^String s XSDDatatype/XSDgYear)
      :else
      (throw (ex-info (str "unrecognized date shape: " s) {:value s})))))

(defn- edtf-literal [s]
  (when s
    (NodeFactory/createLiteral ^String s ^BaseDatatype edtf-datatype)))

(defn- ->int-literal [s]
  (NodeFactory/createLiteral ^String s XSDDatatype/XSDint))

(defn- person-data
  "Aristotle map for one Person. ADR 0015: each non-null date is
  emitted twice — as a precision-typed XSD literal under the RDA
  Group 2 predicate, and as the canonical EDTF lexical string under
  a parallel abc:edtf* predicate."
  [person]
  (let [iri (str "<" (person-iri (get person "person_id")) ">")
        given (get person "given_name")
        family (get person "family_name")
        dob (get person "date_of_birth")
        dod (get person "date_of_death")]
    (cond-> {:rdf/about iri
             :rdf/type [:foaf/Person]
             :dcterms/identifier (->int-literal (get person "person_id"))
             :foaf/familyName family
             :foaf/name (if given (str family " " given) family)}
      given
      (assoc :foaf/givenName given)
      (get person "family_name_reading")
      (assoc :abc/familyNameReading (get person "family_name_reading"))
      (get person "given_name_reading")
      (assoc :abc/givenNameReading (get person "given_name_reading"))
      (get person "family_name_sort")
      (assoc :abc/familyNameForSort (get person "family_name_sort"))
      (get person "given_name_sort")
      (assoc :abc/givenNameForSort (get person "given_name_sort"))
      (get person "family_name_romaji")
      (assoc :abc/familyNameRomaji (get person "family_name_romaji"))
      (get person "given_name_romaji")
      (assoc :abc/givenNameRomaji (get person "given_name_romaji"))
      dob (assoc :rdag2/dateOfBirth (date-literal-for dob))
      dob (assoc :abc/edtfDateOfBirth (edtf-literal dob))
      dod (assoc :rdag2/dateOfDeath (date-literal-for dod))
      dod (assoc :abc/edtfDateOfDeath (edtf-literal dod))
      (seq (get person "external_links"))
      (assoc :rdfs/seeAlso (mapv #(str "<" % ">") (get person "external_links"))))))

(defn record->graph
  "Build a Jena graph from a single Person record."
  [record]
  (rdf-prefixes/ensure!)
  (let [graph (aa/graph :simple)]
    (aa/add graph (person-data record))
    graph))

(defn record->ttl
  "Convenience: (comp manifest-to-rdf/graph->ttl record->graph)."
  [record]
  (manifest-to-rdf/graph->ttl (record->graph record)))
