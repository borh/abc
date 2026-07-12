(ns abc.sim.sidecar
  "Observer-layer drift-sidecar authoring for the simulation harness:
  valid _events/ + _indexes/ sidecars built from applied forced drift
  intents, plus sampled fault injectors for the invalid-sidecar
  properties. Editorial artifacts stay OUT of abc.sim.model.
  Spec: docs/superpowers/specs/2026-07-12-drift-sidecar-interplay-design.md"
  (:require [abc.tools.hash :as hash]
            [abc.tools.json :as json]
            [abc.tools.manifest :as manifest]
            [abc.tools.person-drift :as drift]
            [clojure.java.io :as io]))

(defn placeholder-hash
  "Well-formed, deterministic, corpus-independent person_record_hash.
  validate-drift-events! pattern-checks participant hashes but never
  compares them against a corpus, so any stable sha256:<hex> is valid."
  [person-id]
  (hash/format-sha256 (hash/sha256-json-jcs person-id)))

(defn- participant [prefix person-id]
  {"person_id" person-id
   "person_record_hash" (placeholder-hash person-id)
   "snapshot_id" (str prefix person-id)})

(defn event-for-intent
  "Author a valid drift event from an applied forced intent
  ({:intent :clean-split|:clean-merge :event e}). Participants sorted by
  snapshot_id (post- sorts before pre-); used/was_generated_by sorted;
  live schema id/hash; drift_event_id materialized."
  [{:keys [intent event]}]
  (let [[pre-pids post-pids etype]
        (case intent
          :clean-split [[(:pid event)] (vec (:targets event)) "split"]
          :clean-merge [(vec (:pids event)) [(:target event)] "merge"])
        pres (mapv #(participant "pre-" %) pre-pids)
        posts (mapv #(participant "post-" %) post-pids)]
    (drift/materialize-event-id
     {"schema_id" drift/event-schema-id
      "schema_hash" (manifest/schema-hash drift/event-schema-path)
      "drift_event_type" etype
      "date" "2026-07-12"
      "evidence" [(str "https://example.org/abc/drift-evidence/sim-" etype)]
      "participants" (vec (sort-by #(get % "snapshot_id") (into pres posts)))
      "prov" {"used" (vec (sort (map #(get % "snapshot_id") pres)))
              "was_generated_by" (vec (sort (map #(get % "snapshot_id") posts)))
              "qualified_association"
              {"agent" "https://w3id.org/abc/agents/editorial-board"
               "had_role" "abc:DriftEditor"}}})))

(defn indexes-for-event [event]
  (vec (for [pid (distinct (map #(get % "person_id")
                                (get event "participants")))]
         {"schema_id" drift/index-schema-id
          "schema_hash" (manifest/schema-hash drift/index-schema-path)
          "person_id" pid
          "drift_event_ids" [(get event "drift_event_id")]})))

(defn write-sidecars!
  "Write _events/<drift_event_id>.json and one _indexes/<pid>.json per
  participant under dir. Returns event."
  [dir event]
  (let [events-dir (io/file dir "_events")
        indexes-dir (io/file dir "_indexes")]
    (.mkdirs events-dir)
    (.mkdirs indexes-dir)
    (json/write-deterministic-json-file!
     (io/file events-dir (str (get event "drift_event_id") ".json")) event)
    (doseq [index (indexes-for-event event)]
      (json/write-deterministic-json-file!
       (io/file indexes-dir (str (get index "person_id") ".json")) index))
    event))

(defn corrupt!
  "Rewrite an already-written sidecar dir so validate-drift-events!
  reports `fault` (a documented abc.tools.person-drift failure code).
  No content-hash-vs-id check exists, so rewriting the event body without
  recomputing drift_event_id is safe for :participants-not-sorted."
  [dir event fault]
  (let [indexes-dir (io/file dir "_indexes")
        event-file (io/file dir "_events"
                            (str (get event "drift_event_id") ".json"))]
    (case fault
      :schema-hash-mismatch
      (json/write-deterministic-json-file!
       event-file
       (assoc event "schema_hash" (str "sha256:" (apply str (repeat 64 "0")))))

      :orphan-event-file
      (doseq [f (.listFiles indexes-dir)] (io/delete-file f))

      :index-target-missing
      (json/write-deterministic-json-file!
       (io/file indexes-dir "999999.json")
       {"schema_id" drift/index-schema-id
        "schema_hash" (manifest/schema-hash drift/index-schema-path)
        "person_id" "999999"
        "drift_event_ids" [(str "sha256:" (apply str (repeat 64 "f")))]})

      :participants-not-sorted
      (json/write-deterministic-json-file!
       event-file (update event "participants" (comp vec reverse))))))
