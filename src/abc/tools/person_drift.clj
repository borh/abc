(ns abc.tools.person-drift
  (:require [abc.tools.hash :as hash]
            [clojure.set :as set]
            [clojure.string :as string]))

(def event-schema-path "schemas/person-drift-event.schema.json")
(def event-schema-id "https://w3id.org/abc/schemas/person-drift-event.schema.json")

(def index-schema-path "schemas/person-drift-index.schema.json")
(def index-schema-id "https://w3id.org/abc/schemas/person-drift-index.schema.json")

(def person-id-pattern #"^([0-9]{6}|abc-[0-9a-f]{12})$")
(def hash-pattern #"^sha256:[0-9a-f]{64}$")

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
