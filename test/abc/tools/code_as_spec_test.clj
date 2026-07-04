(ns abc.tools.code-as-spec-test
  "Code-as-spec gates (design: docs/superpowers/specs/2026-07-04-code-as-spec-formal-models-design.md).
  Layer A: condemned fixtures assert the REAL validator rejects invariant
  violations. Reads schemas/manifest.schema.json — no hand-transcribed model.

  Layer D: test.check property test bound to the REAL
  manifest-index/reproducibility-conflicts oracle (replaces R1)."
  (:require [abc.tools.files :as files]
            [abc.tools.schema :as schema]
            [abc.tools.manifest :as manifest]
            [abc.tools.manifest-index :as manifest-index]
            [clojure.test :refer [deftest is testing]]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]))

(def nested-id-fixture-path "fixtures/v0/invalid/manifest-nested-artifact-id/manifest.json")

(deftest r2-manifest-schema-rejects-nested-artifact-id-test
  "ADR 0001 R2: artifact_id must not be nested inside manifest_identity_object.
  The real carrier is the JSON Schema's $defs/identityObject, which declares
  additionalProperties:false and a fixed 13-key property set that does NOT
  include artifact_id. This fixture violates that; the real validator must
  reject it. (Replaces vacuous r2-non-circularity.smt2 — see
  docs/handoffs/formal-verification-assessment-critique.md §2.)"
  (let [schema (files/read-json "schemas/manifest.schema.json")
        fixture (files/read-json nested-id-fixture-path)
        errors (schema/validation-errors schema fixture)]
    (is (seq errors) "schema must reject a manifest whose identity object nests artifact_id")
    ;; The error must be specifically about the nested artifact_id (via the
    ;; additionalProperties:false on $defs/identityObject), not an unrelated
    ;; field — otherwise the fixture passes for the wrong reason. m3 errors are
    ;; maps; str them and assert the path mentions the offending property.
    (is (some #(re-find #"(?i)manifest_identity_object|artifact_id|additionalProperties"
                       (str %))
              (map str errors))
        (str "error path must target manifest_identity_object/artifact_id; got: "
             (pr-str errors)))))
