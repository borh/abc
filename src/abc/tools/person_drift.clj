(ns abc.tools.person-drift
  (:require [abc.tools.hash :as hash]))

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
