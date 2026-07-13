(ns abc.tools.parser-mapping-admission-evidence-test
  (:require [abc.tools.aat-parser-ir-compat :as compat]
            [abc.tools.files :as files]
            [abc.tools.parser-evidence :as parser-evidence]
            [abc.tools.schema :as schema]
            [abc.tools.validate-design-bundle :as validate]
            [clojure.test :refer [deftest is]]))

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
        conflicting-ir (assoc-in parser-ir ["derived_from" "mapping_version"] "conflicting")
        release-report (fn [citation candidate-ir candidate-inputs]
                         (parser-evidence/validate-index! {:entries [citation]})
                         {:citation-status (:status citation)
                          :compatibility-errors
                          (validate/compatibility-errors registry candidate-ir candidate-inputs)})
        missing-release (release-report historical-selection parser-ir missing-inputs)
        conflicting-release (release-report historical-selection conflicting-ir manifest-inputs)]
    (is (some? historical-selection))
    (is (= :citable (:citation-status missing-release)))
    (is (seq (:compatibility-errors missing-release)))
    (is (= :citable (:citation-status conflicting-release)))
    (is (seq (:compatibility-errors conflicting-release)))
    (is (= :conflict
           (:status (compat/admission-report registry {:entries [conflicting-entry]}))))
    (is (= :admitted
           (:status (compat/admission-report registry {:entries [exact-candidate]}))))))
