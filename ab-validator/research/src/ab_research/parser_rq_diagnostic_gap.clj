(ns ab-research.parser-rq-diagnostic-gap
  "Closed validators for diagnostic-gap evidence artifacts and the
  single authority for the ABC diagnostic vocabulary. Inputs are
  string-keyed JSON values."
  (:require [ab-research.files :as files]
            [ab-research.hash :as hash]
            [ab-research.parser-rq-decoded-utf8 :as decoded-utf8]
            [ab-research.schema :as schema]))

(def vocabulary
  "The closed ABC diagnostic vocabulary: [code kind severity source
  disposition] per rule, in policy order."
  [["source-contains-pua" "source_contains_pua" "warning" "source" "authorize_exact_span"]
   ["unclosed-bracket" "unclosed_bracket" "error" "source" "observe_only"]
   ["unmatched-close" "unmatched_close" "error" "source" "observe_only"]
   ["accent-decomposition-applied" "accent_decomposition_applied" "note" "source" "observe_only"]
   ["unresolved-gaiji" "unresolved_gaiji" "warning" "source" "observe_only"]
   ["mismatched-container-close" "mismatched_container_close" "error" "source" "observe_only"]
   ["empty-ruby-reading" "empty_ruby_reading" "error" "source" "observe_only"]
   ["nested-ruby" "nested_ruby" "error" "source" "observe_only"]
   ["unrecognised-container-directive" "unrecognised_container_directive" "warning" "source" "observe_only"]
   ["tcy-target-not-found" "tcy_target_not_found" "warning" "source" "observe_only"]
   ["bouten-target-ambiguous" "bouten_target_ambiguous" "warning" "source" "observe_only"]
   ["forward-referent-not-stylable" "forward_referent_not_stylable" "warning" "source" "observe_only"]
   ["break-in-single-line-container" "break_in_single_line_container" "warning" "source" "observe_only"]
   ["bracketed-kaeriten-no-pair" "bracketed_kaeriten_no_pair" "error" "source" "observe_only"]
   ["kaeriten-outside-kanbun" "kaeriten_outside_kanbun" "warning" "source" "observe_only"]
   ["mismatched-bouten-container" "mismatched_bouten_container" "error" "source" "observe_only"]
   ["non-canonical-directive" "non_canonical_directive" "warning" "source" "observe_only"]
   ["residual-annotation-marker" "residual_annotation_marker" "error" "internal" "reject_internal"]
   ["unregistered-sentinel" "unregistered_sentinel" "error" "internal" "reject_internal"]
   ["registry-out-of-order" "registry_out_of_order" "error" "internal" "reject_internal"]
   ["registry-position-mismatch" "registry_position_mismatch" "error" "internal" "reject_internal"]])

(def live-codes
  "Vocabulary codes in policy order."
  (mapv first vocabulary))

(defn policy-errors [policy]
  (let [rules (get policy "rules" [])
        tuples (mapv (fn [rule]
                       (mapv #(get rule %)
                             ["code" "kind" "severity" "source" "disposition"]))
                     rules)
        raw-schema (files/read-json
                    "schemas/parser-rq-ab-aozora-diagnostics-v3.schema.json")
        expected-schema-hash (hash/format-sha256
                              (hash/sha256-json-jcs raw-schema))
        expected-policy-hash (hash/format-sha256
                              (hash/sha256-json-jcs
                               (dissoc policy "policy_hash")))]
    (vec
     (concat
      (when-not (= (count tuples) (count (distinct tuples)))
        ["diagnostic-gap policy contains duplicate or ambiguous selectors"])
      (when-not (= (count rules) (count (distinct (map #(get % "code") rules))))
        ["diagnostic-gap policy contains duplicate selector codes"])
      (when-not (= vocabulary tuples)
        ["diagnostic-gap policy does not equal the closed ABC vocabulary"])
      (when-not (= expected-schema-hash
                   (get policy "raw_diagnostic_schema_hash"))
        ["diagnostic-gap policy raw diagnostic schema hash mismatch"])
      (when-not (= expected-policy-hash (get policy "policy_hash"))
        ["diagnostic-gap policy identity hash mismatch"])))))

(defn raw-diagnostics-errors [policy capture decoded]
  (let [raw-schema (files/read-json
                    "schemas/parser-rq-ab-aozora-diagnostics-v3.schema.json")
        source-bytes (when (string? decoded)
                       (.getBytes ^String decoded "UTF-8"))
        policy-by-code (into {}
                             (map (fn [rule] [(get rule "code") rule])
                                  (get policy "rules" [])))]
    (vec
     (concat
      (when-let [errors (schema/validation-errors raw-schema capture)]
        (map #(str "raw diagnostics schema: " %) errors))
      (policy-errors policy)
      (when-not (= (count (get capture "data" []))
                   (count (distinct (get capture "data" []))))
        ["raw diagnostics contain duplicate complete diagnostic identities"])
      (mapcat
       (fn [diagnostic]
         (let [code (get diagnostic "code")
               rule (get policy-by-code code)
               span (get diagnostic "span")
               source-slice (decoded-utf8/decoded-slice source-bytes
                                                        (get span "start")
                                                        (get span "end"))
               selector (select-keys diagnostic
                                     ["code" "kind" "severity" "source"])
               expected (select-keys rule
                                     ["code" "kind" "severity" "source"])]
           (concat
            (when-not (= selector expected)
              [(str "raw diagnostic selector does not match policy for " code)])
            (when (= "internal" (get diagnostic "source"))
              [(str "raw diagnostic uses rejected internal source for " code)])
            (when-not source-slice
              [(str "raw diagnostic span is not a nonempty decoded UTF-8 interval for " code)])
            (when (= code "source-contains-pua")
              (let [codepoint (get diagnostic "codepoint")]
                (concat
                 (when-not (decoded-utf8/unicode-private-use? codepoint)
                   ["source-contains-pua codepoint is not one Unicode private-use scalar"])
                 (when-not (= source-slice codepoint)
                   ["source-contains-pua span does not equal its codepoint"])))))))
       (get capture "data" []))))))

(defn result-coherence-errors
  [result source-recognition-work source-recognition-artifact-ref]
  (if (not= "ok" (get result "status"))
    []
    (let [evidence (get result "source_recognition_evidence")
          expected-value-hash
          (hash/format-sha256 (hash/sha256-json-jcs source-recognition-work))]
      (vec
       (concat
        (when-not (= source-recognition-artifact-ref
                     (get evidence "artifact_ref"))
          ["diagnostic-gap result source-recognition artifact reference mismatch"])
        (when-not (= expected-value-hash (get evidence "value_hash"))
          ["diagnostic-gap result source-recognition value hash mismatch"])
        (for [[field outer-value]
              [["work_id" (get result "work_id")]
               ["capture_generation_ref" (get result "capture_generation_ref")]
               ["qualification_identity_ref"
                (get source-recognition-work "qualification_identity_ref")]]
              :when (or (not= outer-value (get evidence field))
                        (not= outer-value (get source-recognition-work field)))]
          (str "diagnostic-gap result source-recognition " field
               " is not coherent")))))))
