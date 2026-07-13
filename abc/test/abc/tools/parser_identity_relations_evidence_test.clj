(ns abc.tools.parser-identity-relations-evidence-test
  (:require [abc.tools.files :as files]
            [abc.tools.manifest :as manifest]
            [clojure.test :refer [deftest is]]))

(deftest parser-tuple-change-preserves-source-role-relations-test
  (let [source (files/read-json "fixtures/source-bundle/abc-source-bundle-v1-known-answer.json")
        bundle-hash (get source "bundle_hash")
        primary-text-hash (get source "primary_text_hash")
        base-inputs (-> (files/read-json "examples/ab-validator-output/manifest-inputs.json")
                        (assoc "work_content_hash" bundle-hash))
        base-ir (-> (files/read-json "examples/ab-validator-output/parser-ir.json")
                    (assoc-in ["source" "work_content_hash"] bundle-hash)
                    (assoc-in ["source" "primary_text_hash"] primary-text-hash))
        changed-inputs (assoc base-inputs
                              "parser_build_hash" (files/example-hash "91")
                              "mapping_hash" (files/example-hash "92"))
        changed-ir (-> base-ir
                       (assoc-in ["derived_from" "aat_adapter"] "changed-parser-adapter")
                       (assoc-in ["derived_from" "aat_adapter_version"] "changed-parser-adapter 2.0")
                       (assoc-in ["derived_from" "mapping_id"] "https://w3id.org/abc/mappings/changed")
                       (assoc-in ["derived_from" "mapping_version"] "2.0"))
        manifest-schema-hash (manifest/schema-hash "schemas/manifest.schema.json")
        output-format-hash (get base-inputs "parser_ir_schema_hash")
        identity-for #(manifest/identity-object
                       %
                       {:manifest-schema-hash manifest-schema-hash
                        :output-format-spec-hash output-format-hash})
        base-identity (identity-for base-inputs)
        changed-identity (identity-for changed-inputs)
        base-tuple [(get base-inputs "parser_build_hash")
                    (get-in base-ir ["derived_from" "aat_adapter"])
                    (get base-inputs "mapping_hash")]
        changed-tuple [(get changed-inputs "parser_build_hash")
                       (get-in changed-ir ["derived_from" "aat_adapter"])
                       (get changed-inputs "mapping_hash")]]
    (is (not= base-tuple changed-tuple))
    (is (not= base-identity changed-identity))
    (is (not= (manifest/artifact-id base-identity)
              (manifest/artifact-id changed-identity)))
    (doseq [parser-ir [base-ir changed-ir]]
      (is (= bundle-hash (get-in parser-ir ["source" "work_content_hash"])))
      (is (= primary-text-hash (get-in parser-ir ["source" "primary_text_hash"])))
      (is (not= bundle-hash primary-text-hash)))))
