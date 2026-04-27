(ns abc.tools.metadata-record
  "Identity, validation, and RDF mapping for ABC metadata records.
  record-hash hashes the canonical-identity form: schema_id and
  schema_hash included; source_csv_provenance excluded; persons[]
  sorted by person_id; nullable fields explicit as JSON null.
  record->graph + graph->ttl produce the RDF/Turtle view; the
  vocabulary is the resolved table from the spec (BIBO + Schema.org
  for work; FOAF + RDA Group 2 for person; Dublin Core throughout;
  abc: for project-specific concepts)."
  (:require [abc.tools.files :as files]
            [abc.tools.hash :as hash]
            [abc.tools.manifest :as manifest]
            [abc.tools.manifest-to-rdf :as manifest-to-rdf]
            [abc.tools.rdf-prefixes :as rdf-prefixes]
            [abc.tools.schema :as schema]
            [arachne.aristotle :as aa]
            [clojure.string :as string])
  (:import [org.apache.jena.datatypes.xsd XSDDatatype]
           [org.apache.jena.datatypes BaseDatatype]
           [org.apache.jena.graph NodeFactory]))

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

;; ---------------------------------------------------------------------------
;; RDF mapping
;; ---------------------------------------------------------------------------

(defn- card-iri
  "Existing Aozora LOD uses the http://www.aozora.gr.jp card URL as the
  work IRI. The `card_url` field carries https:; we normalize to http:
  to match the canonical Aozora LOD scheme."
  [card-url]
  (str "<" (string/replace card-url "https://" "http://") ">"))

(defn- person-iri [person-id]
  (str "<http://www.aozora.gr.jp/index_pages/person" person-id ".html>"))

(defn- ->date-literal [iso-date]
  (NodeFactory/createLiteral ^String iso-date XSDDatatype/XSDdate))

(defn- ->int-literal [s]
  (NodeFactory/createLiteral ^String s XSDDatatype/XSDint))

(defn- ->bool-literal [b]
  (NodeFactory/createLiteral ^String (str b) XSDDatatype/XSDboolean))

(def ^:private dcndl-ndc-datatype
  (BaseDatatype. "http://ndl.go.jp/dcndl/terms/NDC"))

(defn- ndc-literal [ndc-string]
  (NodeFactory/createLiteral ^String ndc-string ^BaseDatatype dcndl-ndc-datatype))

(defn- person-data [person]
  (let [iri (person-iri (get person "person_id"))]
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

(defn- title-blank-node [work]
  (let [reading (get work "title_reading")]
    (cond-> {:rdf/value (get work "title")}
      reading (assoc :abc/reading reading))))

(defn- contributor-blank [person]
  {:rdf/about (person-iri (get person "person_id"))
   :dcterms/role (get person "relation_to_work")})

(defn- work-data [work persons]
  (let [iri (card-iri (get work "card_url"))
        authors (filter #(= "著者" (get % "relation_to_work")) persons)
        contributors (remove #(= "著者" (get % "relation_to_work")) persons)]
    (cond-> {:rdf/about iri
             :rdf/type [:bibo/Document :schema/CreativeWork]
             :dcterms/identifier (->int-literal (get work "work_id"))
             :dcterms/title (title-blank-node work)
             :dc/subject (ndc-literal (get work "ndc"))
             :abc/orthographicStyle (get work "orthographic_style")
             :abc/copyrightExpired (->bool-literal (get work "copyright_expired"))
             :dcterms/available (->date-literal (get work "aozora_available"))
             :dcterms/modified (->date-literal (get work "aozora_modified"))}
      (seq authors)
      (assoc :dcterms/creator (mapv #(person-iri (get % "person_id")) authors))
      (seq contributors)
      (assoc :dcterms/contributor (mapv contributor-blank contributors)))))

(defn record->graph
  "Build a Jena graph from a metadata record using the resolved
  vocabulary mapping (see spec §RDF vocabulary alignment)."
  [record]
  (rdf-prefixes/ensure!)
  (let [work (get record "work")
        persons (get record "persons")
        graph (aa/graph :simple)]
    (aa/add graph (work-data work persons))
    (doseq [p persons]
      (aa/add graph (person-data p)))
    graph))

(defn record->ttl
  "Convenience composition: (comp manifest-to-rdf/graph->ttl record->graph)."
  [record]
  (manifest-to-rdf/graph->ttl (record->graph record)))
