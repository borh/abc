(ns abc.tools.source-assertion
  "Validation boundary for source assertions shared by rights and temporal
  records. This namespace preserves provenance only; domain interpretation
  belongs to the consuming assessment module."
  (:require [abc.tools.files :as files]
            [abc.tools.schema :as schema]))

(def schema-path "schemas/source-assertion.schema.json")
(def schema-id "https://w3id.org/abc/schemas/source-assertion.schema.json")

(def ^:private contract
  (delay (files/read-json schema-path)))

(defn validation-errors
  "Return deterministic JSON Schema problem maps for `value`, or nil when the
  source-assertion envelope is valid."
  [value]
  (schema/validation-errors @contract value))
