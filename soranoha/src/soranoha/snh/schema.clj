(ns soranoha.snh.schema
  "The four FROZEN protocol JSON Schemas (spec section 11, F80), loaded from
  resources and keyed by artifact type. Authority split (F88): these schemas
  govern STRUCTURE; the protocol spec governs semantic and state invariants;
  the conformance vectors demonstrate both."
  (:require [charred.api :as json]
            [clojure.java.io :as io]
            [soranoha.core.hash :as hash]
            [soranoha.ported.schema :as ported-schema]))

(def schema-resources
  "Artifact type -> classpath resource of its frozen JSON Schema. This map is
  the executable form of the spec section 2 registry's release-level protocol
  JSON objects — the ONLY types boundary decode applies to (F142)."
  {"release-manifest"    "snh/schemas/snh-manifest-1.schema.json"
   "assessment-snapshot" "snh/schemas/snh-assessment-snapshot-1.schema.json"
   "admission-report"    "snh/schemas/snh-admission-report-1.schema.json"
   "governance-event"    "snh/schemas/snh-governance-event-1.schema.json"})

(def ^:private loaded
  (delay
    (into {}
          (map (fn [[type resource]]
                 (let [url (or (io/resource resource)
                               (throw (ex-info "Frozen schema resource missing"
                                               {:type type :resource resource})))
                       content (slurp url)]
                   [type {:schema (json/read-json content)
                          :hash (hash/sha256-string content)}])))
          schema-resources)))

(defn schema-for
  "Parsed frozen schema for a protocol artifact type; throws on any other type."
  [type]
  (or (some-> (get @loaded type) :schema)
      (throw (ex-info "No frozen schema for artifact type"
                      {:type type :known (keys schema-resources)}))))

(defn schema-file-hash
  "sha256 hex over the schema file's exact bytes (freeze-review identity)."
  [type]
  (or (some-> (get @loaded type) :hash)
      (throw (ex-info "No frozen schema for artifact type" {:type type}))))

(defn validation-errors
  "Validation errors for `value` against the frozen schema of `type`;
  nil when valid."
  [type value]
  (ported-schema/validation-errors (schema-for type) value))
