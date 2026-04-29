(ns abc.tools.iiif
  "IIIF applicability gate (ADR 0014).

  ABC v0 does not require IIIF for text-only artifacts. Each work
  records a per-work decision in a small JSON document whose contract
  lives in `schemas/iiif-applicability.schema.json`. This namespace is
  the schema-driven gate that `validate-design-bundle` calls. The
  policy itself (when each status is appropriate) lives in ADR 0014;
  the schema here only enforces the structural shape of a single
  decision record."
  (:require [abc.tools.files :as files]
            [abc.tools.schema :as schema]))

(def ^:const schema-path
  "schemas/iiif-applicability.schema.json")

(defn validate-applicability!
  "Validate the IIIF applicability decision at `path` against the JSON
  schema. Throws ex-info with `:errors` populated by m3 on failure."
  [path]
  (let [s (files/read-json schema-path)]
    (schema/validate-json! s path)))
