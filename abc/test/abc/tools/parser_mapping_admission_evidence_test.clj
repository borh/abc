(ns abc.tools.parser-mapping-admission-evidence-test
  (:require [abc.tools.adr-evidence-runtime-inputs :as runtime-inputs]
            [abc.tools.aat-parser-ir-compat :as compat]
            [abc.tools.files :as files]
            [abc.tools.parser-evidence :as parser-evidence]
            [abc.tools.schema :as schema]
            [abc.tools.validate-design-bundle :as validate]
            [clojure.test :refer [deftest is]]))

(deftest parser-mapping-admission-contract
  (runtime-inputs/with-validated-read-trace!
    {:identity-root ".." :cwd-root "." :repo-root "." :workspace-root ".."
     :descriptor {:path "abc/docs/evidence/adr-capture/parser-mapping-admission.edn"
                  :value (files/read-edn "docs/evidence/adr-capture/parser-mapping-admission.edn")}}
    (fn []
      (doseq [path ["data/aat-parser-ir-compatibility.edn" "data/parser-evidence-citations.edn"
                    "examples/ab-validator-output/manifest-inputs.json" "examples/ab-validator-output/parser-ir.json"
                    "schemas/aat-parser-ir-divergence-bundle.schema.json" "schemas/aat-parser-ir-divergence.schema.json"
                    "schemas/aat-parser-ir-mapping.schema.json" "schemas/parser-ir.schema.json"]]
        (files/read-text path))
      (is true))))

(deftest mapping-contract-schemas-meta-validate-test
  (doseq [path ["schemas/aat-parser-ir-mapping.schema.json"
                "schemas/aat-parser-ir-divergence.schema.json"
                "schemas/aat-parser-ir-divergence-bundle.schema.json"]]
    (is (nil? (schema/schema-valid! (files/read-json path) path)))))

(deftest parser-citations-cannot-bypass-exact-admission-test
  (let [registry (compat/load-registry)
        parser-ir (files/read-json "examples/ab-validator-output/parser-ir.json")
        manifest-inputs (files/read-json "examples/ab-validator-output/manifest-inputs.json")
        citations (:entries (parser-evidence/load-index))
        historical-selection (some #(when (and (= :parser-selection (:evidence_class %))
                                               (= :citable (:status %)))
                                      %)
                                   citations)
        exact-candidate (some #(when (= (select-keys % compat/match-keys)
                                        {:aat_version (get-in parser-ir ["derived_from" "aat_version"])
                                         :aat_adapter (get-in parser-ir ["derived_from" "aat_adapter"])
                                         :aat_adapter_version (get-in parser-ir ["derived_from" "aat_adapter_version"])
                                         :mapping_id (get-in parser-ir ["derived_from" "mapping_id"])
                                         :mapping_version (get-in parser-ir ["derived_from" "mapping_version"])
                                         :mapping_hash (get manifest-inputs "mapping_hash")
                                         :mapping_schema_hash (get-in parser-ir ["derived_from" "mapping_schema_hash"])
                                         :parser_ir_schema_id (get parser-ir "schema_id")
                                         :parser_ir_schema_hash (get parser-ir "schema_hash")})
                                 %)
                              (:entries registry))
        missing-inputs (assoc manifest-inputs "mapping_hash" (files/example-hash "99"))
        conflicting-entry (assoc exact-candidate :compatibility "lossless")
        conflicting-ir (assoc-in parser-ir ["derived_from" "mapping_version"] "conflicting")]
    (is (some? historical-selection))
    (is (= :citable (:status historical-selection)))
    (with-redefs [parser-evidence/load-index
                  (fn [] (throw (ex-info "citation index crossed admission boundary" {})))
                  parser-evidence/validate-index!
                  (fn [_] (throw (ex-info "citation validation crossed admission boundary" {})))]
      (is (seq (validate/compatibility-errors registry parser-ir missing-inputs)))
      (is (seq (validate/compatibility-errors registry conflicting-ir manifest-inputs))))
    (is (= :conflict
           (:status (compat/admission-report registry {:entries [conflicting-entry]}))))
    (is (= :admitted
           (:status (compat/admission-report registry {:entries [exact-candidate]}))))))
