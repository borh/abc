(ns abc.tools.person-record
  "Identity, validation, and RDF mapping for ABC person records.
  record-hash hashes the canonical-identity form: schema_id and
  schema_hash included; source_csv_provenance excluded; nullable
  fields explicit as JSON null. record->graph renders a single
  Person as FOAF + RDA Group 2 triples."
  (:require [abc.tools.files :as files]
            [abc.tools.hash :as hash]
            [abc.tools.manifest-to-rdf :as manifest-to-rdf]
            [abc.tools.rdf-prefixes :as rdf-prefixes]
            [abc.tools.schema :as schema]
            [arachne.aristotle :as aa])
  (:import [org.apache.jena.datatypes.xsd XSDDatatype]
           [org.apache.jena.graph NodeFactory]))

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

;; ---------------------------------------------------------------------------
;; RDF mapping
;; ---------------------------------------------------------------------------

(defn person-iri
  "Existing Aozora LOD uses http://www.aozora.gr.jp/index_pages/personNNNNNN.html
  as the person IRI."
  [person-id]
  (str "http://www.aozora.gr.jp/index_pages/person" person-id ".html"))

(defn- ->date-literal [iso-date]
  (NodeFactory/createLiteral ^String iso-date XSDDatatype/XSDdate))

(defn- ->int-literal [s]
  (NodeFactory/createLiteral ^String s XSDDatatype/XSDint))

(defn- person-data
  "Aristotle map for one Person. Lifted from
  abc.tools.metadata-record/person-data; the IRI/predicate set is
  unchanged so RDF/Turtle output remains byte-identical when the
  metadata-record namespace delegates here."
  [person]
  (let [iri (str "<" (person-iri (get person "person_id")) ">")]
    (cond-> {:rdf/about iri
             :rdf/type [:foaf/Person]
             :dcterms/identifier (->int-literal (get person "person_id"))
             :foaf/familyName (get person "family_name")
             :foaf/givenName (get person "given_name")
             :foaf/name (str (get person "family_name") " "
                             (get person "given_name"))}
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
      (get person "date_of_birth")
      (assoc :rdag2/dateOfBirth (->date-literal (get person "date_of_birth")))
      (get person "date_of_death")
      (assoc :rdag2/dateOfDeath (->date-literal (get person "date_of_death")))
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
