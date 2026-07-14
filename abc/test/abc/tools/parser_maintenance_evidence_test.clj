(ns abc.tools.parser-maintenance-evidence-test
  (:require [abc.tools.files :as files]
            [abc.tools.parser-maintenance-evidence :as maintenance]
            [abc.tools.schema :as schema]
            [abc.tools.validate-design-bundle :as validate]
            [clojure.test :refer [deftest is testing]]))

(defn- validator-var [symbol]
  (try
    (requiring-resolve symbol)
    (catch Exception _ nil)))

(deftest maintenance-evidence-protocol-exists-test
  (testing "the maintenance protocol has a callable semantic validator"
    (is (some? (validator-var 'abc.tools.parser-maintenance-evidence/problems)))))

(deftest committed-maintenance-record-is-valid-test
  (let [problems (validator-var 'abc.tools.parser-maintenance-evidence/problems)]
    (is (some? problems))
    (when problems
      (is (empty? (problems
                   (files/read-json
                    "docs/evidence/external/custom-parser-maintenance-2026-q3.json")))))))

(deftest maintenance-schema-and-template-exist-test
  (let [schema-path "schemas/custom-parser-maintenance-evidence.schema.json"
        schema-value (try (files/read-json schema-path) (catch Exception _ nil))
        template (try
                   (files/read-text
                    "docs/evidence/templates/custom-parser-maintenance-quarterly.md")
                   (catch Exception _ nil))]
    (is (some? schema-value))
    (is (some? template))
    (when schema-value
      (is (nil? (schema/schema-valid! schema-value schema-path)))
      (is (nil? (schema/validation-errors
                 schema-value
                 (files/read-json
                  "docs/evidence/external/custom-parser-maintenance-2026-q3.json")))))
    (when template
      (doseq [heading ["Upstream review" "Selective ports" "Security review"
                       "Missed fixes" "Fork-only defects" "Planned divergence"
                       "Remeasurement" "Maintainer availability"
                       "Disposable trial merge" "Decision-revisit predicates"]]
        (is (clojure.string/includes? template heading) heading)))))

(deftest maintenance-contract-is-part-of-design-validation-inputs-test
  (let [inputs (set (validate/evidence-input-paths))]
    (is (contains? inputs
                   "schemas/custom-parser-maintenance-evidence.schema.json"))
    (is (contains? inputs
                   "docs/evidence/external/custom-parser-maintenance-2026-q3.json"))))

(deftest maintenance-validator-rejects-ambiguous-evidence-and-dates-test
  (let [record (files/read-json
                "docs/evidence/external/custom-parser-maintenance-2026-q3.json")]
    (is (seq (maintenance/problems
              (assoc-in record ["trial_merge" "estimate" "evidence_kind"]
                        "benchmark"))))
    (is (seq (maintenance/problems
              (assoc-in record ["trial_merge" "benchmarks"]
                        [{"evidence_kind" "expert-assessment"}]))))
    (is (seq (maintenance/problems (assoc record "expires_on" "2026-09-01"))))))

(deftest estimate-is-rederived-from-components-test
  (let [record (files/read-json
                "docs/evidence/external/custom-parser-maintenance-2026-q3.json")]
    (is (empty? (maintenance/problems record)))
    (is (seq (maintenance/problems
              (assoc-in record ["trial_merge" "estimate" "estimated_minutes"]
                        359))))))

(deftest revisit-predicates-are-executable-test
  (let [record (files/read-json
                "docs/evidence/external/custom-parser-maintenance-2026-q3.json")
        triggered-reviews
        (validator-var 'abc.tools.parser-maintenance-evidence/triggered-reviews)]
    (is (some? triggered-reviews))
    (when triggered-reviews
      (is (= [] (triggered-reviews
                 record
                 {"rolling_quarter_maintenance_minutes" 1200
                  "selective_port_p90_minutes" 200
                  "maximum_missed_fix_severity" 1
                  "maintainer_available" true})))
      (is (= ["immediate ownership review"]
             (triggered-reviews
              record
              {"rolling_quarter_maintenance_minutes" 1200
               "selective_port_p90_minutes" 200
               "maximum_missed_fix_severity" 1
               "maintainer_available" false}))))))
