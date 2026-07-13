(ns abc.tools.parser-import-boundary-evidence-test
  (:require [abc.tools.files :as files]
            [abc.tools.malli :as am]
            [abc.tools.schema :as schema]
            [abc.tools.validate-design-bundle :as validate]
            [babashka.fs :as fs]
            [clojure.test :refer [deftest is use-fixtures]]
            [malli.core :as m]
            [malli.registry :as mr]))

(use-fixtures :once (fn [f] (am/install!) (f)))

(def ^:private fixture-root "examples/ab-validator-output")

(defn- fixture-path [name]
  (files/path fixture-root name))

(defn- run-summary-event-set-errors [events]
  (m/explain ::am/run-summary-events
             events
             {:registry (mr/composite-registry (m/default-schemas)
                                               (am/install!))}))

(deftest imported-boundary-validates-parser-ir-diagnostics-and-run-summary-test
  (let [parser-ir-schema (files/read-json "schemas/parser-ir.schema.json")
        diagnostic-schema (files/read-json "schemas/diagnostic.schema.json")
        run-summary-schema (files/read-json "schemas/run-summary.schema.json")
        warnings (files/read-json-lines (fixture-path "warnings.jsonl"))
        events (files/read-json-lines (fixture-path "run-summary.jsonl"))]
    (is (nil? (schema/validation-errors
               parser-ir-schema
               (files/read-json (fixture-path "parser-ir.json")))))
    (is (every? #(nil? (schema/validation-errors diagnostic-schema %)) warnings))
    (is (every? #(nil? (schema/validation-errors run-summary-schema %)) events))
    (is (nil? (run-summary-event-set-errors events)))))

(deftest imported-boundary-rejects-malformed-diagnostic-and-event-set-test
  (let [diagnostic-schema (files/read-json "schemas/diagnostic.schema.json")
        warning (first (files/read-json-lines (fixture-path "warnings.jsonl")))
        malformed-warning (assoc warning "severity" "warn" "code" "lowercase")
        events (files/read-json-lines (fixture-path "run-summary.jsonl"))
        incomplete-events (filterv #(not= "run-complete" (get % "event")) events)]
    (is (= [["code"] ["severity"]]
           (->> (schema/validation-errors diagnostic-schema malformed-warning)
                (map :document-path)
                sort
                vec)))
    (is (some? (run-summary-event-set-errors incomplete-events)))))

(deftest abc-boundary-does-not-execute-parser-candidates-test
  (fs/with-temp-dir [root {}]
    (let [input (fs/file root "imported")
          sentinel (fs/file root "parser-candidate-sentinel")
          invoked? (atom false)
          original-path files/path]
      (fs/copy-tree fixture-root input)
      (spit sentinel "#!/bin/sh\nexit 97\n")
      (fs/set-posix-file-permissions sentinel "rwx------")
      (with-redefs [files/path (fn [& parts]
                                (if (= ["examples" "ab-validator-output"]
                                       (vec (take 2 parts)))
                                  (apply fs/file input (drop 2 parts))
                                  (apply original-path parts)))
                    validate/run-command! (fn [& command]
                                            (when (some #{(str sentinel)} command)
                                              (reset! invoked? true))
                                            (throw (ex-info "ABC crossed parser execution boundary"
                                                            {:command command})))]
        (is (= :ok (validate/validate-ab-validator-output!))))
      (is (false? @invoked?)))))
