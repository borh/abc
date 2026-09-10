(ns soranoha.snh.schema
  "Protocol JSON Schemas, loaded from resources and keyed by artifact type."
  (:require [charred.api :as json]
            [clojure.java.io :as io]
            [soranoha.core.schema :as schema-validator]))

(def schema-resources
  "Artifact type -> classpath resource of its JSON Schema."
  {"release-manifest"    "snh/schemas/snh-manifest-3.schema.json"
   "assessment-snapshot" "snh/schemas/snh-assessment-snapshot-2.schema.json"
   "admission-report"    "snh/schemas/snh-admission-report-1.schema.json"
   "governance-event"    "snh/schemas/snh-governance-event-1.schema.json"
   "catalog"             "snh/schemas/snh-catalog-1.schema.json"})

(def ^:private loaded
  (delay
    (into {}
          (map (fn [[type resource]]
                 (let [url (or (io/resource resource)
                               (throw (ex-info "Protocol schema resource missing"
                                               {:type type :resource resource})))]
                   [type (json/read-json (slurp url))])))
          schema-resources)))

(defn schema-for
  "Parsed protocol schema for an artifact type; throws on any other type."
  [type]
  (or (get @loaded type)
      (throw (ex-info "No protocol schema for artifact type"
                      {:type type :known (keys schema-resources)}))))

(defn validation-errors
  "Validation errors for `value` against the schema of `type`; nil when valid."
  [type value]
  (schema-validator/validation-errors (schema-for type) value))
