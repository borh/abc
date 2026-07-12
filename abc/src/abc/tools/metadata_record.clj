(ns abc.tools.metadata-record
  "Identity, validation, and RDF mapping for ABC metadata records.
  record-hash hashes the canonical-identity form: schema_id and
  schema_hash included; source_csv_provenance excluded; persons[]
  sorted by person_id; nullable fields explicit as JSON null.
  record->graph + graph->ttl produce the RDF/Turtle view; the
  vocabulary is the resolved table from the spec (BIBO + Schema.org
  for work; FOAF + RDA Group 2 for person; Dublin Core throughout;
  abc: for project-specific concepts)."
  (:require [abc.tools.hash :as hash]
            [abc.tools.malli :as am]
            [abc.tools.manifest :as manifest]
            [abc.tools.manifest-to-rdf :as manifest-to-rdf]
            [abc.tools.person-record :as person-record]
            [abc.tools.rdf-prefixes :as rdf-prefixes]
            [abc.tools.schema :as schema]
            [arachne.aristotle :as aa]
            [clojure.string :as string])
  (:import [org.apache.jena.datatypes.xsd XSDDatatype]
           [org.apache.jena.datatypes BaseDatatype]
           [org.apache.jena.graph GraphUtil NodeFactory]))

(def schema-path "schemas/metadata-record.schema.json")
(def schema-id "https://w3id.org/abc/schemas/metadata-record.schema.json")

(defn validate!
  "Validate `record` against schemas/metadata-record.schema.json.
  Returns :ok on success; throws ex-info on failure with both :errors
  (raw m3 vector) and :errors-humanized (readable strings)."
  [record]
  (let [[errors humanized] (schema/validation-errors-humanized
                            (am/cached-schema schema-path) record)]
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
  using RFC 8785 JCS via abc.tools.hash/sha256-json-jcs."
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
   "metadata_record_schema_hash" (manifest/schema-hash schema-path)
   "work" work
   "contributors" (vec (sort-by #(get % "person_id") contributors))})

;; ---------------------------------------------------------------------------
;; RDF mapping
;; ---------------------------------------------------------------------------

(defn- card-iri
  "Existing Aozora LOD uses the http://www.aozora.gr.jp card URL as the
  work IRI. The `card_url` field carries https:; we normalize to http:
  to match the canonical Aozora LOD scheme."
  [card-url]
  (str "<" (string/replace card-url "https://" "http://") ">"))

(defn- person-iri-bracketed [person-id]
  (str "<" (person-record/person-iri person-id) ">"))

(defn- ->date-literal [iso-date]
  (NodeFactory/createLiteral ^String iso-date XSDDatatype/XSDdate))

(defn- ->int-literal [s]
  (NodeFactory/createLiteral ^String s XSDDatatype/XSDint))

(def ^:private dcndl-ndc-datatype
  (BaseDatatype. "http://ndl.go.jp/dcndl/terms/NDC"))

(defn- ndc-literal [ndc-string]
  (NodeFactory/createLiteral ^String ndc-string ^BaseDatatype dcndl-ndc-datatype))

(defn- title-blank-node [work]
  (let [reading (get work "title_reading")]
    (cond-> {:rdf/value (get work "title")}
      reading (assoc :dcndl/titleTranscription reading))))

(defn- contributor-blank [contributor]
  {:rdf/about (person-iri-bracketed (get contributor "person_id"))
   :dcterms/role (get contributor "relation_to_work")})

(defn- work-data [work contributors]
  (let [iri (card-iri (get work "card_url"))
        authors (filter #(= "著者" (get % "relation_to_work")) contributors)
        others (remove #(= "著者" (get % "relation_to_work")) contributors)]
    (cond-> {:rdf/about iri
             :rdf/type [:bibo/Document :schema/CreativeWork]
             :dcterms/identifier (->int-literal (get work "work_id"))
             :dcterms/title (title-blank-node work)
             :abc/orthographicStyle (get work "orthographic_style")
             :dcterms/available (->date-literal (get work "aozora_available"))
             :dcterms/modified (->date-literal (get work "aozora_modified"))}
      (get work "ndc")
      (assoc :dc/subject (ndc-literal (get work "ndc")))
      (seq authors)
      (assoc :dcterms/creator (mapv #(person-iri-bracketed (get % "person_id")) authors))
      (seq others)
      (assoc :dcterms/contributor (mapv contributor-blank others)))))

(defn record->graph
  "Build a Jena graph from a metadata record. Emits only work-side
  triples plus contributor edges; person bodies come from
  abc.tools.person-record/record->graph and are composed by
  record+persons->graph."
  [record]
  (rdf-prefixes/ensure!)
  (let [work (get record "work")
        contributors (get record "contributors")
        graph (aa/graph :simple)]
    (aa/add graph (work-data work contributors))
    graph))

(defn record+persons->graph
  "Compose a metadata record with its resolved person bodies into a
  single graph: work + contributor edges + each person's full body
  triples. `persons-by-id` maps person_id → person-record map."
  [record persons-by-id]
  (let [graph (record->graph record)]
    (doseq [contributor (get record "contributors")
            :let [pid (get contributor "person_id")
                  body (get persons-by-id pid)]]
      (when-not body
        (throw (ex-info (str "no resolved person body for person_id " pid)
                        {:person-id pid
                         :persons-by-id (vec (keys persons-by-id))})))
      (GraphUtil/addInto graph (person-record/record->graph body)))
    graph))

(defn record+persons->ttl
  "Convenience: compose work and persons, render Turtle."
  [record persons-by-id]
  (manifest-to-rdf/graph->ttl (record+persons->graph record persons-by-id)))
