(ns abc.tools.workflow-test
  (:require [abc.tools.files :as files]
            [abc.tools.schema :as schema]
            [abc.tools.workflow :as workflow]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing use-fixtures]])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(def workflow-run-schema-path
  "schemas/workflow-run.schema.json")

;; Track temp dirs and delete them in a :each fixture so a failing test
;; cannot orphan them (these tests previously created temp dirs and never
;; cleaned up).
(def ^:private created-temp-dirs (atom []))

(defn- temp-dir [prefix]
  (let [d (.toFile (Files/createTempDirectory prefix (make-array FileAttribute 0)))]
    (swap! created-temp-dirs conj d)
    d))

(use-fixtures :each
  (fn [f]
    (try
      (f)
      (finally
        (run! files/delete-tree! @created-temp-dirs)
        (reset! created-temp-dirs [])))))

(deftest workflow-run-schema-fixtures-test
  (let [schema-value (files/read-json workflow-run-schema-path)]
    (testing "valid fixture"
      (is (nil? (schema/validation-errors
                 schema-value
                 (files/read-json "examples/workflow/passed.workflow-run.json")))))
    (testing "invalid status fixture"
      (is (seq (schema/validation-errors
                schema-value
                (files/read-json
                 "fixtures/v0/invalid/workflow-run/invalid-status.workflow-run.json")))))))

(deftest run-workflow-executes-topologically-and-writes-reports-test
  (let [out (temp-dir "abc-workflow")
        calls (atom [])
        clock-values (atom ["2026-07-08T00:00:00Z"
                            "2026-07-08T00:00:01Z"
                            "2026-07-08T00:00:02Z"
                            "2026-07-08T00:00:03Z"
                            "2026-07-08T00:00:04Z"
                            "2026-07-08T00:00:05Z"])
        clock (fn []
                (let [value (first @clock-values)]
                  (swap! clock-values rest)
                  value))
        result (workflow/run-workflow!
                {:workflow-id "fixture.workflow.v1"
                 :run-id "fixture-run"
                 :output-root out
                 :clock clock
                 :initial-state {:a 1}
                 :steps [{:id :first
                          :requires [:a]
                          :produces [:b]
                          :run (fn [state]
                                 (swap! calls conj :first)
                                 {:state-updates {:b (inc (:a state))}
                                  :outputs [{:role "b"
                                             :path "b.json"
                                             :content_hash
                                             "sha256:1111111111111111111111111111111111111111111111111111111111111111"
                                             :request_set_id
                                             "sha256:2222222222222222222222222222222222222222222222222222222222222222"}]})}
                         {:id :second
                          :requires [:b]
                          :produces [:c]
                          :run (fn [state]
                                 (swap! calls conj :second)
                                 {:state-updates {:c (* 2 (:b state))}
                                  :messages [{:level "info"
                                              :message "computed c"}]})}]})]
    (is (= [:first :second] @calls))
    (is (= {:a 1 :b 2 :c 4} (:state result)))
    (is (.isFile (io/file out "workflow-plan.json")))
    (is (.isFile (io/file out "workflow-run.json")))
    (let [run (files/read-json (io/file out "workflow-run.json"))
          schema-value (files/read-json workflow-run-schema-path)]
      (is (nil? (schema/validation-errors schema-value run)))
      (is (= "passed" (get run "status")))
      (is (= 2 (get run "step_count")))
      (is (= 2 (get run "steps_passed")))
      (is (= 0 (get run "steps_failed")))
      (is (= "sha256:1111111111111111111111111111111111111111111111111111111111111111"
             (get-in run ["steps" 0 "outputs" 0 "content_hash"])))
      (is (= "sha256:2222222222222222222222222222222222222222222222222222222222222222"
             (get-in run ["steps" 0 "outputs" 0 "request_set_id"]))))))

(deftest validate-run-reports-semantic-invariants-test
  (let [valid-run (files/read-json "examples/workflow/passed.workflow-run.json")]
    (is (= [] (workflow/validate-run valid-run)))
    (let [future-producing-step {"id" "future"
                                 "status" "passed"
                                 "started_at" "2026-07-08T00:00:01Z"
                                 "ended_at" "2026-07-08T00:00:02Z"
                                 "duration_ms" 1000
                                 "requires" ["not-yet-produced"]
                                 "produces" ["not-yet-produced"]
                                 "inputs" []
                                 "outputs" []
                                 "messages" []}
          invalid-run (-> valid-run
                          (update "steps" conj future-producing-step)
                          (assoc "step_count" 99
                                 "steps_passed" 0
                                 "duration_ms" 0)
                          (assoc-in ["steps" 0 "duration_ms"] 999))
          errors (workflow/validate-run invalid-run)
          messages (set (map :message errors))]
      (is (contains? messages "step_count must equal number of steps"))
      (is (contains? messages "steps_passed must equal passed step count"))
      (is (contains? messages "duration_ms must match started_at and ended_at"))
      (is (contains? messages "step duration_ms must match started_at and ended_at"))
      (is (contains? messages "step requires a value before it is produced")))))

(deftest validate-plan-rejects-missing-dependency-before-running-test
  (let [calls (atom [])]
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"missing workflow dependencies"
         (workflow/run-workflow!
          {:workflow-id "bad.workflow.v1"
           :run-id "bad-run"
           :output-root (temp-dir "abc-workflow-bad")
           :initial-state {}
           :steps [{:id :bad
                    :requires [:missing]
                    :produces [:out]
                    :run (fn [_]
                           (swap! calls conj :bad)
                           {:state-updates {:out true}})}]})))
    (is (= [] @calls))))

(deftest node-summary-sidecar-protocol-is-retired-test
  (testing "the target-graph engine and workflow-nodes apparatus stay deleted"
    (doseq [path ["src/abc/tools/workflow/target.clj"
                  "src/abc/tools/workflow/report.clj"
                  "src/abc/tools/workflow/nix_bridge.clj"
                  "src/abc/tools/workflow/cache.clj"
                  "src/abc/tools/workflow/query.clj"
                  "schemas/workflow-nodes.schema.json"
                  "examples/workflow/admission.workflow-nodes.jsonl"
                  "fixtures/v0/invalid/workflow-nodes"]]
      (is (not (files/exists? path)) path)))
  (testing "workflow-run 0.3.0 rejects the retired node_summary_ref member"
    (let [run-schema (files/read-json workflow-run-schema-path)
          base-run (files/read-json "examples/workflow/passed.workflow-run.json")
          run-with-ref (assoc-in base-run ["steps" 0 "node_summary_ref"]
                                 {"schema_version" "soranoha-workflow-nodes-v1"
                                  "path" "admission.workflow-nodes.jsonl"
                                  "node_count" 2
                                  "realized_count" 1
                                  "skipped_count" 1})]
      (is (= "0.3.0" (get run-schema "version")))
      (is (nil? (schema/validation-errors run-schema base-run)))
      (is (seq (schema/validation-errors run-schema run-with-ref))
          "the contract surface narrowed; node_summary_ref is no longer accepted")))
  (testing "the contract manifest carries no workflow-nodes row"
    (let [rows (get (files/read-json "schemas/schema-contracts.json") "schemas")]
      (is (not-any? #(re-find #"workflow-nodes" (get % "path" "")) rows)))))
