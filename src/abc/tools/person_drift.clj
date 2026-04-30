(ns abc.tools.person-drift
  (:require [abc.tools.hash :as hash]
            [abc.tools.person-record :as person-record]
            [abc.tools.rdf-prefixes :as rdf-prefixes]
            [abc.tools.shacl :as shacl]
            [arachne.aristotle :as aa]
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
      (when-not (= drift-editor-curie role)
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
    (add-triple! graph assoc-node (uri (str prov-base "hadRole")) (uri (str abc-base "DriftEditor")))
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
