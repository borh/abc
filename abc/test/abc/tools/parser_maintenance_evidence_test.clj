(ns abc.tools.parser-maintenance-evidence-test
  (:require [abc.tools.files :as files]
            [abc.tools.hash :as hash]
            [abc.tools.parser-maintenance-evidence :as maintenance]
            [abc.tools.schema :as schema]
            [abc.tools.validate-design-bundle :as validate]
            [abc.test-fs :refer [with-temp-dir]]
            [babashka.fs :as fs]
            [clojure.test :refer [deftest is testing]]))

(defn- validator-var [symbol]
  (try
    (requiring-resolve symbol)
    (catch Exception _ nil)))

(defn- semantic-problems [record as-of]
  (try
    (maintenance/problems record as-of)
    (catch clojure.lang.ArityException _ [:missing-as-of-validation])))

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
                   "docs/evidence/external/custom-parser-maintenance-2026-q3.json"))
    (is (contains? inputs
                   "docs/evidence/external/custom-parser-maintenance-as-of.edn"))))

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

(deftest maintenance-expiry-is-deterministic-from-explicit-as-of-test
  (let [record (files/read-json
                "docs/evidence/external/custom-parser-maintenance-2026-q3.json")]
    (is (empty? (semantic-problems record "2026-10-01")))
    (is (some #(clojure.string/includes? (str %) "review is due")
              (semantic-problems record "2026-10-02")))
    (is (some #(clojure.string/includes? (str %) "expired")
              (semantic-problems record "2026-10-16")))
    (is (seq (semantic-problems record "not-a-date")))))

(deftest maintenance-record-cannot-postdate-governance-test
  (let [record (files/read-json
                "docs/evidence/external/custom-parser-maintenance-2026-q3.json")]
    (is (some #(clojure.string/includes? (str %) "recorded_at")
              (semantic-problems record "2026-07-12")))
    (is (empty? (semantic-problems record "2026-07-14")))))

(deftest record-status-and-self-contained-observations-are-required-test
  (let [record (files/read-json
                "docs/evidence/external/custom-parser-maintenance-2026-q3.json")]
    (is (= "initial" (get record "status")))
    (is (= #{"rolling_quarter_maintenance_minutes"
             "selective_port_samples_minutes"
             "selective_port_p90_minutes"
             "maximum_missed_fix_severity"
             "maintainer_available"}
           (set (keys (get record "observations")))))
    (is (seq (semantic-problems (dissoc record "observations") "2026-07-14")))
    (is (seq (semantic-problems (assoc record "status" "unknown")
                                "2026-07-14")))))

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
      (let [safe-result (try (triggered-reviews record)
                             (catch clojure.lang.ArityException _
                               :missing-self-contained-evaluation))
            unavailable (assoc-in record ["observations" "maintainer_available"]
                                  false)
            unavailable-result (try (triggered-reviews unavailable)
                                    (catch clojure.lang.ArityException _
                                      :missing-self-contained-evaluation))]
        (is (= [] safe-result))
        (is (= ["immediate ownership review"] unavailable-result))))))

(deftest predicate-types-and-benchmark-artifacts-are-closed-by-schema-test
  (let [record (files/read-json
                "docs/evidence/external/custom-parser-maintenance-2026-q3.json")
        schema-value (files/read-json
                      "schemas/custom-parser-maintenance-evidence.schema.json")
        unsafe-predicate (assoc-in record ["revisit_predicates" 0 "threshold"]
                                   false)
        incomplete-benchmark
        (assoc-in record ["trial_merge" "benchmarks"]
                  [{"evidence_kind" "benchmark"
                    "metric" "conflicted_paths"
                    "scaled_integer" 0
                    "unit" "count"
                    "command" "git diff --name-only --diff-filter=U"
                    "environment" "disposable worktree"
                    "result_hash" (files/example-hash "a1")}])]
    (is (seq (schema/validation-errors schema-value unsafe-predicate)))
    (is (seq (schema/validation-errors schema-value incomplete-benchmark)))
    (is (= []
           (try
             (maintenance/triggered-reviews unsafe-predicate)
             (catch ClassCastException _ [:unsafe-operator-dispatch]))))))

(deftest design-validation-runs-maintenance-semantics-test
  (let [record (files/read-json
                "docs/evidence/external/custom-parser-maintenance-2026-q3.json")
        validate-maintenance
        (validator-var 'abc.tools.validate-design-bundle/validate-maintenance-evidence!)]
    (is (some? validate-maintenance))
    (when validate-maintenance
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"expired"
           (validate-maintenance record "2026-10-16"))))))

(deftest benchmark-raw-artifacts-are-contained-and-content-addressed-test
  (with-temp-dir [root]
    (let [allowed (fs/path root "docs/evidence/external/maintenance-benchmarks")
          artifact (fs/path allowed "trial-merge.txt")
          _ (fs/create-dirs allowed)
          _ (spit (fs/file artifact) "raw trial merge output\n")
          benchmark {"evidence_kind" "benchmark"
                     "metric" "conflicted_paths"
                     "scaled_integer" 0
                     "unit" "count"
                     "command" "git diff --name-only --diff-filter=U"
                     "environment" "disposable worktree"
                     "result_hash" (files/example-hash "a1")
                     "raw_artifact_path"
                     "docs/evidence/external/maintenance-benchmarks/trial-merge.txt"
                     "raw_artifact_hash"
                     (str "sha256:" (hash/sha256-file (fs/file artifact)))}
          record (assoc-in
                  (files/read-json
                   "docs/evidence/external/custom-parser-maintenance-2026-q3.json")
                  ["trial_merge" "benchmarks"] [benchmark])
          artifact-problems
          (validator-var
           'abc.tools.parser-maintenance-evidence/benchmark-artifact-problems)]
      (is (some? artifact-problems))
      (when artifact-problems
        (is (empty? (artifact-problems root record)))
        (is (seq (artifact-problems
                  root
                  (assoc-in record
                            ["trial_merge" "benchmarks" 0 "raw_artifact_path"]
                            "docs/evidence/external/maintenance-benchmarks/missing.txt"))))
        (is (seq (artifact-problems
                  root
                  (assoc-in record
                            ["trial_merge" "benchmarks" 0 "raw_artifact_hash"]
                            (files/example-hash "ff")))))
        (is (seq (artifact-problems
                  root
                  (assoc-in record
                            ["trial_merge" "benchmarks" 0 "raw_artifact_path"]
                            "../trial-merge.txt"))))))))
