(ns abc.tools.person-record
  "Identity, validation, and RDF mapping for ABC person records.
  record-hash hashes the canonical-identity form: schema_id and
  schema_hash included; source_csv_provenance excluded; nullable
  fields explicit as JSON null. record->graph renders a single
  Person as FOAF + RDA Group 2 triples plus an EDTF-typed echo of
  the canonical date string per ADR 0015."
  (:require [abc.tools.hash :as hash]
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

(defn numeric-person-id? [person-id]
  (boolean (re-matches #"^[0-9]{6}$" person-id)))

(defn abc-local-person-id? [person-id]
  (boolean (re-matches #"^abc-[0-9a-f]{12}$" person-id)))

(defn person-iri
  "Aozora numeric IDs keep their Aozora page IRI. ABC-local IDs use the
  ADR 0020 local person namespace."
  [person-id]
  (cond
    (numeric-person-id? person-id)
    (str "http://www.aozora.gr.jp/index_pages/person" person-id ".html")

    (abc-local-person-id? person-id)
    (str "https://w3id.org/abc/persons/" person-id)

    :else
    (throw (ex-info (str "unsupported person_id shape: " person-id)
                    {:person_id person-id}))))

(def ^:private full-date-pattern  #"-?\d{4}-\d{2}-\d{2}")
(def ^:private year-month-pattern #"-?\d{4}-\d{2}")
(def ^:private year-pattern       #"-?\d{4}")
(def ^:private decade-pattern     #"-?\d{3}X")
(def ^:private century-pattern    #"-?\d{2}XX")

(def ^:private edtf-datatype
  (BaseDatatype. "https://w3id.org/abc/EDTF"))

(defn- date-literal-for
  "Pick the most-precise XSD literal for an EDTF lexical value.
  Returns nil for nil, and also nil for EDTF Level 1 decade and
  century shapes (`192X`, `-06XX`) — XSD's coarsest temporal type is
  `xsd:gYear`, so there is no precision-honest XSD literal at decade
  or century granularity. The EDTF echo via `edtf-literal` is the
  carrier for those values; `person-data` omits the RDA Group 2
  predicate when this returns nil. Throws on unrecognized shape —
  schema validation should have caught it. ADR 0015 / ADR 0016."
  [s]
  (when s
    (cond
      (re-matches full-date-pattern s)
      (NodeFactory/createLiteral ^String s XSDDatatype/XSDdate)
      (re-matches year-month-pattern s)
      (NodeFactory/createLiteral ^String s XSDDatatype/XSDgYearMonth)
      (re-matches year-pattern s)
      (NodeFactory/createLiteral ^String s XSDDatatype/XSDgYear)
      (re-matches decade-pattern s) nil
      (re-matches century-pattern s) nil
      :else
      (throw (ex-info (str "unrecognized date shape: " s) {:value s})))))

(defn- edtf-literal [s]
  (when s
    (NodeFactory/createLiteral ^String s ^BaseDatatype edtf-datatype)))

(defn- ->int-literal [s]
  (NodeFactory/createLiteral ^String s XSDDatatype/XSDint))

(defn- ->identifier-literal [person-id]
  (if (numeric-person-id? person-id)
    (->int-literal person-id)
    (NodeFactory/createLiteral ^String person-id XSDDatatype/XSDstring)))

(defn- person-data
  "Aristotle map for one Person. ADR 0015: each non-null date is
  emitted as a canonical EDTF lexical string under the abc:edtf*
  predicate, and additionally as a precision-typed XSD literal under
  the RDA Group 2 predicate when XSD has a precision-honest type
  (Level 0 only). For EDTF Level 1 decade / century shapes the RDA
  Group 2 predicate is omitted — the EDTF echo is the carrier. ADR
  0016."
  [person]
  (let [iri (str "<" (person-iri (get person "person_id")) ">")
        given (get person "given_name")
        family (get person "family_name")
        dob (get person "date_of_birth")
        dod (get person "date_of_death")
        dob-xsd (date-literal-for dob)
        dod-xsd (date-literal-for dod)]
    (cond-> {:rdf/about iri
             :rdf/type [:foaf/Person]
             :dcterms/identifier (->identifier-literal (get person "person_id"))
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
      dob-xsd (assoc :rdag2/dateOfBirth dob-xsd)
      dob (assoc :abc/edtfDateOfBirth (edtf-literal dob))
      dod-xsd (assoc :rdag2/dateOfDeath dod-xsd)
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
