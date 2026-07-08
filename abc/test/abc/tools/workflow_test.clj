(ns abc.tools.workflow-test
  (:require [abc.tools.files :as files]
            [abc.tools.schema :as schema]
            [abc.tools.workflow :as workflow]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(def workflow-run-schema-path
  "schemas/workflow-run.schema.json")

(defn- temp-dir [prefix]
  (.toFile (Files/createTempDirectory
            prefix
            (make-array FileAttribute 0))))

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
