(ns abc.tools.person-drift
  (:require [abc.tools.files :as files]
            [abc.tools.hash :as hash]
            [abc.tools.manifest :as manifest]
            [abc.tools.person-record :as person-record]
            [abc.tools.rdf-prefixes :as rdf-prefixes]
            [abc.tools.schema :as schema]
            [abc.tools.shacl :as shacl]
            [arachne.aristotle :as aa]
            [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as string])
  (:import [org.apache.jena.datatypes.xsd XSDDatatype]
           [org.apache.jena.graph NodeFactory Triple]))

(def event-schema-path "schemas/person-drift-event.schema.json")
(def event-schema-id "https://w3id.org/abc/schemas/person-drift-event.schema.json")

(def index-schema-path "schemas/person-drift-index.schema.json")
(def index-schema-id "https://w3id.org/abc/schemas/person-drift-index.schema.json")

(def person-id-pattern #"^([0-9]{6}|abc-[0-9a-f]{12})$")
(def hash-pattern #"^sha256:[0-9a-f]{64}$")

(def rdf-type-uri "http://www.w3.org/1999/02/22-rdf-syntax-ns#type")
(def abc-base "https://w3id.org/abc/")
(def prov-base "http://www.w3.org/ns/prov#")
(def dcterms-base "http://purl.org/dc/terms/")

(defn drift-event-id [event]
  (hash/format-sha256
   (hash/sha256-json-jcs (dissoc event "drift_event_id"))))

(defn materialize-event-id [event]
  (assoc event "drift_event_id" (drift-event-id event)))

(def drift-editor-curie "abc:DriftEditor")
(def allowed-role-curies #{drift-editor-curie})

(def failure-codes
  "Authoritative set of person-drift validation failure codes emitted by this namespace."
  #{:duplicate-snapshot-id
    :event-missing-from-participant-index
    :exception
    :generated-not-sorted
    :index-target-missing
    :invalid-agent-iri
    :invalid-had-role
    :missing-rdf-type
    :orphan-event-file
    :participant-in-both-used-and-generated
    :participant-not-covered
    :participants-not-sorted
    :rdf-participant-prov-mismatch
    :schema-hash-mismatch
    :shacl-violation
    :snapshot-prefix-usage-mismatch
    :unexpected-rdf-type
    :unknown-snapshot-reference
    :unresolved-curie-prefix
    :used-not-sorted})

(defn resolve-role-curie [role]
  (rdf-prefixes/resolve-curie role))

(defn- resolved-role-uri! [event]
  (let [role (get-in event ["prov" "qualified_association" "had_role"])]
    (or (resolve-role-curie role)
        (throw (ex-info "unresolvable drift event role CURIE"
                        {:had_role role})))))

(defn- sorted-lex? [xs]
  (= (vec xs) (vec (sort xs))))

(defn- duplicate-values [xs]
  (->> xs
       frequencies
       (filter (fn [[_ n]] (> n 1)))
       (mapv first)))

(defn- participant-ids [event]
  (mapv #(get % "snapshot_id") (get event "participants")))

(defn- used-ids [event]
  (get-in event ["prov" "used"]))

(defn- generated-ids [event]
  (get-in event ["prov" "was_generated_by"]))

(defn- valid-iri? [s]
  (try
    (let [uri (java.net.URI. s)]
      (and (.isAbsolute uri) (some? (.getScheme uri))))
    (catch Exception _ false)))

(defn event-json-coherence-failures [event]
  (let [snapshot-ids (participant-ids event)
        snapshot-set (set snapshot-ids)
        used (vec (used-ids event))
        generated (vec (generated-ids event))
        used-set (set used)
        generated-set (set generated)
        referenced (set (concat used generated))
        participant-set (set snapshot-ids)
        role (get-in event ["prov" "qualified_association" "had_role"])
        resolved-role (resolve-role-curie role)
        agent (get-in event ["prov" "qualified_association" "agent"])]
    (vec
     (concat
      (when-not (sorted-lex? snapshot-ids)
        [{:code :participants-not-sorted
          :snapshot_ids snapshot-ids}])
      (when-not (sorted-lex? used)
        [{:code :used-not-sorted
          :snapshot_ids used}])
      (when-not (sorted-lex? generated)
        [{:code :generated-not-sorted
          :snapshot_ids generated}])
      (map (fn [id] {:code :duplicate-snapshot-id :snapshot_id id})
           (duplicate-values snapshot-ids))
      (map (fn [id] {:code :unknown-snapshot-reference :snapshot_id id})
           (sort (remove snapshot-set referenced)))
      (map (fn [id] {:code :participant-not-covered :snapshot_id id})
           (sort (set/difference participant-set referenced)))
      (map (fn [id] {:code :participant-in-both-used-and-generated :snapshot_id id})
           (sort (set/intersection used-set generated-set)))
      (map (fn [id] {:code :snapshot-prefix-usage-mismatch
                     :snapshot_id id
                     :usage "used"})
           (remove #(string/starts-with? % "pre-") used))
      (map (fn [id] {:code :snapshot-prefix-usage-mismatch
                     :snapshot_id id
                     :usage "was_generated_by"})
           (remove #(string/starts-with? % "post-") generated))
      (when (nil? resolved-role)
        [{:code :unresolved-curie-prefix :had_role role}])
      (when (and resolved-role (not (contains? allowed-role-curies role)))
        [{:code :invalid-had-role :had_role role}])
      (when-not (valid-iri? agent)
        [{:code :invalid-agent-iri :agent agent}])))))

(defn assert-event-json-coherent! [event]
  (let [failures (event-json-coherence-failures event)]
    (when (seq failures)
      (throw (ex-info "person drift event JSON graph-coherence failed"
                      {:failures failures}))))
  :ok)

(defn- uri [s]
  (NodeFactory/createURI s))

(defn- literal [s]
  (NodeFactory/createLiteral ^String s))

(defn- date-literal [s]
  (NodeFactory/createLiteral ^String s XSDDatatype/XSDdate))

(defn- add-triple! [graph s p o]
  (.add graph (Triple/create s p o)))

(defn event-iri [drift-event-id]
  (let [hex (subs drift-event-id (count "sha256:"))]
    (str abc-base "person-drift-events/" hex)))

(defn- hash-short [person-record-hash]
  (subs person-record-hash (count "sha256:") (+ (count "sha256:") 12)))

(defn- person-iri [person-id]
  (if (re-matches #"^[0-9]{6}$" person-id)
    (person-record/person-iri person-id)
    (str abc-base "persons/" person-id)))

(defn snapshot-iri [participant]
  (str abc-base "persons/" (get participant "person_id")
       "#snapshot-"
       (hash-short (get participant "person_record_hash"))))

(defn- participant-by-id [event]
  (into {} (map (juxt #(get % "snapshot_id") identity)
                (get event "participants"))))

(defn- subclass-type-uri [event-type]
  (case event-type
    "split" (str abc-base "DriftSplitEvent")
    "merge" (str abc-base "DriftMergeEvent")))

(defn event->graph [event]
  (rdf-prefixes/ensure!)
  (assert-event-json-coherent! event)
  (let [graph (aa/graph :simple)
        by-id (participant-by-id event)
        event-node (uri (event-iri (get event "drift_event_id")))
        used (get-in event ["prov" "used"])
        generated (get-in event ["prov" "was_generated_by"])
        agent (uri (get-in event ["prov" "qualified_association" "agent"]))
        role (uri (resolved-role-uri! event))
        assoc-node (NodeFactory/createBlankNode)]
    (doseq [type-uri [(str abc-base "DriftEvent")
                      (str prov-base "Activity")
                      (subclass-type-uri (get event "drift_event_type"))]]
      (add-triple! graph event-node (uri rdf-type-uri) (uri type-uri)))
    (add-triple! graph event-node (uri (str abc-base "driftEventType"))
                 (literal (get event "drift_event_type")))
    (add-triple! graph event-node (uri (str dcterms-base "date"))
                 (date-literal (get event "date")))
    (doseq [evidence (get event "evidence")]
      (add-triple! graph event-node (uri (str abc-base "driftEvidence")) (uri evidence)))
    (doseq [participant (get event "participants")]
      (let [snapshot-node (uri (snapshot-iri participant))
            person-node (uri (person-iri (get participant "person_id")))]
        (add-triple! graph snapshot-node (uri rdf-type-uri) (uri (str prov-base "Entity")))
        (add-triple! graph snapshot-node (uri (str prov-base "specializationOf")) person-node)))
    (doseq [snapshot-id used]
      (let [snapshot-node (uri (snapshot-iri (get by-id snapshot-id)))]
        (add-triple! graph event-node (uri (str prov-base "used")) snapshot-node)
        (add-triple! graph snapshot-node (uri (str prov-base "wasInvalidatedBy")) event-node)))
    (doseq [snapshot-id generated]
      (let [snapshot-node (uri (snapshot-iri (get by-id snapshot-id)))]
        (add-triple! graph snapshot-node (uri (str prov-base "wasGeneratedBy")) event-node)
        (doseq [used-id used]
          (add-triple! graph snapshot-node
                       (uri (str prov-base "wasDerivedFrom"))
                       (uri (snapshot-iri (get by-id used-id)))))))
    (add-triple! graph event-node (uri (str prov-base "wasAssociatedWith")) agent)
    (add-triple! graph event-node (uri (str prov-base "qualifiedAssociation")) assoc-node)
    (add-triple! graph assoc-node (uri rdf-type-uri) (uri (str prov-base "Association")))
    (add-triple! graph assoc-node (uri (str prov-base "agent")) agent)
    (add-triple! graph assoc-node (uri (str prov-base "hadRole")) role)
    graph))

(defn- type-uris [graph event-node-uri]
  (let [node (uri event-node-uri)]
    (->> (iterator-seq (.find graph node (uri rdf-type-uri) nil))
         (map #(.getURI (.getObject %)))
         set)))

(defn expected-type-uris [event]
  #{(str abc-base "DriftEvent")
    (str prov-base "Activity")
    (subclass-type-uri (get event "drift_event_type"))})

(defn typing-coherence-failures [event graph]
  (let [actual (type-uris graph (event-iri (get event "drift_event_id")))
        expected (expected-type-uris event)]
    (vec
     (concat
      (map (fn [type-uri] {:code :missing-rdf-type :type type-uri})
           (sort (set/difference expected actual)))
      (map (fn [type-uri] {:code :unexpected-rdf-type :type type-uri})
           (sort (set/difference actual expected)))))))

(defn assert-typing-coherent! [event graph]
  (let [failures (typing-coherence-failures event graph)]
    (when (seq failures)
      (throw (ex-info "person drift event RDF typing coherence failed"
                      {:failures failures}))))
  :ok)

(defn validate-event-shacl! [graph label]
  (shacl/validate! {:shapes-graph (shacl/load-shapes-graph)
                    :data-graph graph
                    :label label}))

(defn- uri-objects [graph subject-uri predicate-uri]
  (let [subject (uri subject-uri)
        predicate (uri predicate-uri)]
    (->> (iterator-seq (.find graph subject predicate nil))
         (keep (fn [triple]
                 (let [object (.getObject triple)]
                   (when (.isURI object)
                     (.getURI object)))))
         set)))

(defn- uri-subjects [graph predicate-uri object-uri]
  (let [predicate (uri predicate-uri)
        object (uri object-uri)]
    (->> (iterator-seq (.find graph nil predicate object))
         (keep (fn [triple]
                 (let [subject (.getSubject triple)]
                   (when (.isURI subject)
                     (.getURI subject)))))
         set)))

(defn- expected-snapshot-iris [event snapshot-ids]
  (let [by-id (participant-by-id event)]
    (set (map #(snapshot-iri (get by-id %)) snapshot-ids))))

(defn graph-participant-prov-failures [event graph]
  (let [event-node (event-iri (get event "drift_event_id"))
        expected-used (expected-snapshot-iris event (used-ids event))
        actual-used (uri-objects graph event-node (str prov-base "used"))
        expected-generated (expected-snapshot-iris event (generated-ids event))
        actual-generated (uri-subjects graph (str prov-base "wasGeneratedBy") event-node)
        actual-invalidated (uri-subjects graph (str prov-base "wasInvalidatedBy") event-node)
        mismatch (fn [field expected actual]
                   (when (not= expected actual)
                     {:code :rdf-participant-prov-mismatch
                      :field field
                      :expected (sort expected)
                      :actual (sort actual)}))]
    (vec
     (keep identity
           [(mismatch "prov:used" expected-used actual-used)
            (mismatch "prov:wasGeneratedBy" expected-generated actual-generated)
            (mismatch "prov:wasInvalidatedBy" expected-used actual-invalidated)]))))

(defn- shacl-failures [graph label]
  (try
    (validate-event-shacl! graph label)
    []
    (catch clojure.lang.ExceptionInfo e
      (mapv #(assoc % :code :shacl-violation :path label)
            (:errors (ex-data e))))))

(defn validate-drift-graph-failures
  "Validate an externally supplied drift RDF graph against an independently
  supplied event JSON contract. The live pipeline derives its graph from JSON
  with event->graph, so participant/PROV mismatch checks are primarily for TTL
  negative fixtures and other hand-authored graph validation."
  [event graph label]
  (vec (concat
        (mapv #(assoc % :path label)
              (typing-coherence-failures event graph))
        (mapv #(assoc % :path label)
              (graph-participant-prov-failures event graph))
        (shacl-failures graph label))))

(defn- drift-dir [persons-dir dirname]
  (java.io.File. (io/file persons-dir) dirname))

(defn- json-files [dir]
  (if (.isDirectory dir)
    (->> (.listFiles dir)
         (filter #(and (.isFile %) (string/ends-with? (.getName %) ".json")))
         sort
         vec)
    []))

(defn- expected-schema-hash [schema-path]
  (manifest/schema-hash schema-path))

(defn- schema-hash-failure [artifact-kind path expected actual]
  {:code :schema-hash-mismatch
   :artifact_kind artifact-kind
   :path (str path)
   :expected expected
   :actual actual})

(defn- validate-json-value! [schema-value value path]
  (when-let [errors (schema/validation-errors schema-value value)]
    (throw (ex-info (str "JSON Schema validation failed: " path)
                    {:path (str path)
                     :errors errors}))))

(defn- load-event-file [file]
  (let [event (files/read-json (str file))
        event-schema (files/read-json event-schema-path)
        live-hash (expected-schema-hash event-schema-path)]
    (validate-json-value! event-schema event file)
    {:path (str file)
     :value event
     :schema-failures (when-not (= live-hash (get event "schema_hash"))
                        [(schema-hash-failure :event file live-hash
                                              (get event "schema_hash"))])}))

(defn- load-index-file [file]
  (let [index (files/read-json (str file))
        index-schema (files/read-json index-schema-path)
        live-hash (expected-schema-hash index-schema-path)]
    (validate-json-value! index-schema index file)
    {:path (str file)
     :value index
     :schema-failures (when-not (= live-hash (get index "schema_hash"))
                        [(schema-hash-failure :index file live-hash
                                              (get index "schema_hash"))])}))

(defn- event-failures [{:keys [path value schema-failures]}]
  (let [json-failures (mapv #(assoc % :path path)
                            (event-json-coherence-failures value))
        graph (when (empty? json-failures) (event->graph value))
        typing-failures (if graph
                          (mapv #(assoc % :path path)
                                (typing-coherence-failures value graph))
                          [])
        shacl-violations (if (and graph (empty? typing-failures))
                           (shacl-failures graph path)
                           [])]
    (vec (concat schema-failures json-failures typing-failures shacl-violations))))

(defn- participants-by-person-id [event]
  (->> (get event "participants")
       (map (fn [participant] [(get participant "person_id")
                               (get event "drift_event_id")]))
       (group-by first)
       (map (fn [[person-id pairs]]
              [person-id (set (map second pairs))]))
       (into {})))

(defn- expected-index-map [events]
  (apply merge-with set/union (map participants-by-person-id events)))

(defn- actual-index-map [indexes]
  (into {}
        (map (fn [index]
               [(get index "person_id") (set (get index "drift_event_ids"))]))
        indexes))

(defn- referential-integrity-failures [events indexes]
  (let [events-by-id (into {} (map (juxt #(get % "drift_event_id") identity) events))
        event-ids (set (keys events-by-id))
        expected (expected-index-map events)
        actual (actual-index-map indexes)
        indexed-event-ids (apply set/union #{} (vals actual))]
    (vec
     (concat
      (for [[person-id ids] actual
            event-id (sort ids)
            :when (not (contains? event-ids event-id))]
        {:code :index-target-missing
         :person_id person-id
         :drift_event_id event-id})
      (for [[person-id expected-ids] expected
            :let [actual-ids (get actual person-id #{})]
            event-id (sort (set/difference expected-ids actual-ids))]
        {:code :event-missing-from-participant-index
         :person_id person-id
         :drift_event_id event-id})
      (for [event-id (sort (set/difference event-ids indexed-event-ids))]
        {:code :orphan-event-file
         :drift_event_id event-id})))))

(defn validate-drift-events! [{:keys [persons-dir]}]
  (let [events-dir (drift-dir persons-dir "_events")
        indexes-dir (drift-dir persons-dir "_indexes")
        event-files (json-files events-dir)
        index-files (json-files indexes-dir)]
    (if (and (empty? event-files) (empty? index-files))
      {:status :not-present}
      (try
        (let [loaded-events (mapv load-event-file event-files)
              loaded-indexes (mapv load-index-file index-files)
              events (mapv :value loaded-events)
              indexes (mapv :value loaded-indexes)
              failures (vec (concat
                             (mapcat event-failures loaded-events)
                             (mapcat :schema-failures loaded-indexes)
                             (referential-integrity-failures events indexes)))]
          (if (seq failures)
            {:status :error :failures failures}
            {:status :ok :events (count events) :indexes (count indexes)}))
        (catch clojure.lang.ExceptionInfo e
          {:status :error
           :failures [{:code :exception
                       :message (ex-message e)
                       :data (ex-data e)}]})))))
