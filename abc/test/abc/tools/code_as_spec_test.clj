(ns abc.tools.code-as-spec-test
  "Code-as-spec gates (design: docs/superpowers/specs/2026-07-04-code-as-spec-formal-models-design.md).
  Layer A: condemned fixtures assert the REAL validator rejects invariant
  violations. Reads schemas/manifest.schema.json — no hand-transcribed model.

  Layer D: test.check property test bound to the REAL
  manifest-index/reproducibility-conflicts oracle (replaces R1)."
  (:require [abc.tools.adr-evidence-runtime-inputs :as runtime]
            [abc.tools.evidence-test-support :as evidence-support]
            [abc.tools.files :as files]
            [abc.tools.schema :as schema]
            [abc.tools.manifest-index :as manifest-index]
            [clojure.test :refer [deftest is testing]]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]))

(def nested-id-fixture-path "fixtures/v0/invalid/manifest-nested-artifact-id/manifest.json")

(defn- r2-manifest-schema-rejects-nested-artifact-id-assertions []
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
    ;; field — otherwise the fixture passes for the wrong reason. m3 errors
    ;; are maps; str them and assert the path mentions the offending property.
    (is (some #(re-find #"(?i)manifest_identity_object|artifact_id|additionalProperties"
                        (str %))
              (map str errors))
        (str "error path must target manifest_identity_object/artifact_id; got: "
             (pr-str errors)))))

(deftest r2-manifest-schema-rejects-nested-artifact-id-test
  (runtime/with-validated-read-trace!
    (evidence-support/focused-trace-options "adr-0001-c3-nested-artifact-id-rejection")
    (fn [] (r2-manifest-schema-rejects-nested-artifact-id-assertions))))

;; Layer D — R1 reproducibility conflict, bound to the REAL oracle
;; (manifest-index/reproducibility-conflicts). Replaces vacuous
;; r1-reproducibility-conflict.smt2 — see
;; docs/handoffs/formal-verification-assessment-critique.md §2.

;; Note: success-statuses and the schema status enum are referenced from the
;; REAL sources (manifest-index/successful-statuses def + the schema file),
;; NOT re-transcribed. Re-transcribing would re-introduce the mode-B sin this
;; test exists to catch: a regression widening the real predicate (e.g.
;; accepting "retrying") would silently diverge between generator and oracle.
;; Design spec §7.3: no hand-translated restatement of an oracle's contract.

(def ^:private schema-status-enum
  ;; The REAL manifest schema enum, loaded once at ns-load. Drawing from this
  ;; (not a hand-transcribed vector) binds the generator to the real schema
  ;; contract — a schema regression (adding a status) reaches the generator.
  (-> (files/read-json "schemas/manifest.schema.json")
      (get-in ["properties" "validation_status" "enum"])))

(def ^:private successful-statuses-vec
  ;; vec of the REAL successful-statuses set (manifest-index/successful-statuses,
  ;; src/abc/tools/manifest_index.clj:4). vec because gen/elements takes a seq.
  (vec manifest-index/successful-statuses))

(defn- successful? [entry]
  ;; Delegate to the REAL predicate (manifest-index/successful-entry?). Do NOT
  ;; re-transcribe its logic — design §7.3 forbids hand-translated restatements.
  (manifest-index/successful-entry? entry))

(def gen-conflict-tuple
  "Generate a [m1 m2 expected-conflict?] tuple. m1, m2 are manifest-index
   entries (validation_status/content_hash/artifact_id/manifest_path). A
   conflict exists iff both are successful, share artifact_id, and differ on
   content_hash. expected? is computed from the ACTUAL generated hash equality
   (not a separate same-hash? flag) so the generator and the oracle see the
   same inputs. Statuses are drawn from the REAL schema enum
   schemas/manifest.schema.json (not-run/passed/warning/failed) and success is
   decided by the REAL predicate (#{passed warning})."
  (gen/bind
   (gen/tuple gen/string-alphanumeric                  ; artifact_id
              gen/string-alphanumeric                  ; content_hash_1
              gen/string-alphanumeric                  ; content_hash_2 (independent)
              (gen/elements successful-statuses-vec)    ; m1 status (always successful)
              (gen/elements schema-status-enum))        ; m2 status (any real enum value)
   (fn [[id h1 h2 m1-status m2-status]]
     (let [m1 {"validation_status" m1-status
               "content_hash" h1
               "artifact_id" id
               "manifest_path" "/m1"}
           m2 {"validation_status" m2-status
               "content_hash" h2
               "artifact_id" id
               "manifest_path" "/m2"}
            ;; expected? derived from the actual generated values, using the
            ;; REAL success predicate and REAL string equality on hashes.
           expected? (and (successful? m1) (successful? m2)
                          (not= h1 h2))]
       (gen/return [m1 m2 expected?])))))

(def reproducibility-property
  (prop/for-all [[m1 m2 expected?] gen-conflict-tuple]
                (let [entries [m1 m2]
                      conflicts (manifest-index/reproducibility-conflicts entries)
          ;; boolean() — (seq conflicts) is a seq or nil, not a boolean, so
          ;; (= expected? (seq conflicts)) would compare true to a seq and be
          ;; wrong on the satisfying case. Coerce to boolean.
                      detected? (boolean (seq conflicts))]
                  (= expected? detected?))))

(deftest r1-reproducibility-conflict-property-test
  (testing "real oracle detects conflict iff both success, same id, different hash"
    (let [result (tc/quick-check 200 reproducibility-property)]
      (is (:pass? result)
          (str "property failed:\n" (pr-str result))))))
