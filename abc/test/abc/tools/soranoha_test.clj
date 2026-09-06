(ns abc.tools.soranoha-test
  (:require [abc.test-fs :refer [with-temp-dir]]
            [abc.tools.files :as files]
            [abc.tools.request-set-resolver :as resolver]
            [abc.tools.source-snapshot-fixture :as fixture]
            [abc.tools.soranoha :as soranoha]
            [abc.tools.snapshot-index-test :as six]
            [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.test :refer [deftest is testing]]))

(deftest list-request-sets-prints-checked-in-labels-test
  (let [out (with-out-str
              (is (zero? (soranoha/run! ["list-request-sets"]))))]
    (is (string/includes? out "smoke-basic-ja"))
    (is (string/includes? out "full-corpus-basic-ja"))))

(deftest explain-request-set-prints-request-set-id-test
  (let [resolved (resolver/resolve-request-set "smoke-basic-ja")
        out (with-out-str
              (is (zero? (soranoha/run! ["explain-request-set"
                                         "smoke-basic-ja"]))))]
    (is (string/includes? out "smoke-basic-ja"))
    (is (string/includes? out (get resolved "request_set_id")))
    (is (not (string/includes? out "fixture_role")))))

;; The producer commands (snapshot-index/reproduce/materialize-snapshot-root!)
;; were retired with the competing publication composition. The read-only
;; explain/validate projections stay and are characterized here against the
;; checked-in 0.2.0 example index (a read-only value, not a materialized root).
(def ^:private example-snapshot-index-path
  "examples/v0/snapshot/snapshot-index.json")

(deftest explain-snapshot-command-explains-checked-in-example-index-test
  (let [snapshot (files/read-json example-snapshot-index-path)
        out (with-out-str
              (is (zero? (soranoha/run! ["explain-snapshot"
                                         example-snapshot-index-path]))))]
    (is (string/includes? out (get snapshot "snapshot_date")))
    (is (string/includes? out (get snapshot "snapshot_identity_hash")))
    (is (string/includes?
         out (get-in snapshot ["snapshot_index_identity_object"
                               "source_selection_hash"])))
    (is (string/includes? out "failure_rate:"))))

(deftest validate-command-validates-checked-in-example-index-test
  (let [snapshot (files/read-json example-snapshot-index-path)
        out (with-out-str
              (is (zero? (soranoha/run! ["validate"
                                         example-snapshot-index-path]))))]
    (is (string/includes? out "snapshot_valid: true"))
    (is (string/includes? out (get snapshot "snapshot_date")))
    (is (string/includes? out (get snapshot "snapshot_identity_hash")))))

(deftest validate-on-a-root-recomputes-admissibility-and-ignores-report-test
  ;; The retained validate/explain-snapshot projections recompute current
  ;; admissibility over the installed root; they never trust or rewrite the
  ;; build-time publications-report.json.
  (with-temp-dir [dir]
    (let [{:keys [root]} (six/build-completed-root! (io/file dir "root"))]
      (files/write-text! (io/file root "publications" "publications-report.json")
                         "{\"admissible?\":true,\"problems\":[]}")
      (let [out (with-out-str
                  (is (zero? (soranoha/run! ["validate" (str root)]))))]
        (is (string/includes? out "snapshot_valid: true"))
        (is (string/includes? out "release_admissible: false")
            "the fixture root is recomputed as inadmissible despite the asserted report")
        (is (string/includes? out "rights_policy_file_hash:")))
      (let [out (with-out-str
                  (is (zero? (soranoha/run! ["explain-snapshot" (str root)]))))]
        (is (string/includes? out "release_admissible: false"))
        (is (string/includes? out "decisions_file_hash:"))))))

(deftest source-snapshot-command-generates-workset-and-snapshot-test
  (let [root (fixture/temp-dir "abc-soranoha-source-snapshot")
        input-root (io/file root "materialized")
        output-root (io/file root "source-snapshot")]
    (try
      (fixture/materialized-work! input-root
                                  {:slug "alpha"
                                   :title "一"
                                   :work-id "000001"
                                   :person-id "000101"
                                   :work-hash (fixture/example-hash "a1")})
      (let [out (with-out-str
                  (is (zero? (soranoha/run!
                              ["source-snapshot"
                               (str input-root)
                               (str output-root)
                               "unit-test-source-snapshot"
                               "2026-07-07"]))))
            workset-file (io/file output-root "source-snapshot.workset.edn")
            snapshot-file (io/file output-root "source-snapshot.json")
            source-manifest (io/file input-root
                                     "works"
                                     "alpha"
                                     "source.manifest.json")]
        (is (.exists workset-file))
        (is (.exists snapshot-file))
        (is (.exists source-manifest))
        (let [snapshot (files/read-json snapshot-file)]
          (is (= "unit-test-source-snapshot"
                 (get-in snapshot ["snapshot_identity_object"
                                   "snapshot_scope"])))
          (is (= "source"
                 (get (files/read-json source-manifest)
                      "artifact_kind")))
          (is (string/includes? out (str snapshot-file)))
          (is (string/includes? out (get snapshot "snapshot_hash")))))
      (finally
        (fixture/delete-tree! root)))))

(deftest resolve-request-set-command-uses-generated-source-snapshot-test
  (let [root (fixture/temp-dir "abc-soranoha-resolve-source-snapshot")
        input-root (io/file root "materialized")
        source-snapshot-root (io/file root "source-snapshot")
        request-set-file (io/file root "full-corpus-basic-ja.json")]
    (try
      (fixture/materialized-work! input-root
                                  {:slug "alpha"
                                   :title "一"
                                   :work-id "000001"
                                   :person-id "000101"
                                   :work-hash (fixture/example-hash "a1")})
      (with-out-str
        (is (zero? (soranoha/run!
                    ["source-snapshot"
                     (str input-root)
                     (str source-snapshot-root)
                     "unit-test-source-snapshot"
                     "2026-07-07"]))))
      (let [snapshot-file (io/file source-snapshot-root "source-snapshot.json")
            out (with-out-str
                  (is (zero? (soranoha/run!
                              ["resolve-request-set"
                               "full-corpus-basic-ja"
                               (str request-set-file)
                               (str snapshot-file)]))))
            resolved (files/read-json request-set-file)
            snapshot (files/read-json snapshot-file)]
        (is (.exists request-set-file))
        (is (= (get snapshot "snapshot_hash")
               (get-in resolved ["request_set_identity_object"
                                 "corpus_snapshot_hash"])))
        (is (= 1
               (count (get-in resolved ["request_set_identity_object"
                                        "subjects"]))))
        (is (string/includes? out (str request-set-file)))
        (is (string/includes? out (get resolved "request_set_id")))
        (is (string/includes? out "subjects_count: 1")))
      (finally
        (fixture/delete-tree! root)))))

(deftest cli-status-and-stream-contract-test
  (testing "global help is successful stdout"
    (doseq [args [[] ["help"] ["--help"]]]
      (let [out (java.io.StringWriter.)
            err (java.io.StringWriter.)]
        (binding [*out* out *err* err]
          (is (= 0 (soranoha/run! args))))
        (is (string/includes? (str out) "Usage: soranoha"))
        (is (string/blank? (str err))))))
  (testing "unknown command is status 2 on stderr"
    (let [out (java.io.StringWriter.)
          err (java.io.StringWriter.)]
      (binding [*out* out *err* err]
        (is (= 2 (soranoha/run! ["nope"]))))
      (is (string/blank? (str out)))
      (is (string/includes? (str err) "Unknown command: nope"))))
  (testing "fixed positional arity is status 2 on stderr"
    (let [err (java.io.StringWriter.)]
      (binding [*err* err]
        (is (= 2 (soranoha/run! ["explain-request-set"]))))
      (is (string/includes? (str err) "Required option")))))

(deftest generated-command-help-test
  (doseq [args [["help" "publication-report"]
                ["publication-report" "--help"]
                ["publication-report" "-h"]]]
    (let [out (java.io.StringWriter.)
          err (java.io.StringWriter.)]
      (binding [*out* out *err* err]
        (is (= 0 (soranoha/run! args))))
      (is (string/includes? (str out) "Usage: soranoha publication-report"))
      (is (string/includes? (str out) "<snapshot-root>"))
      (is (string/includes? (str out) "<output-path>"))
      (is (string/includes? (str out) "--help"))
      (is (string/blank? (str err))))))

(deftest generated-global-help-lists-commands-test
  (let [out (with-out-str
              (is (= 0 (soranoha/run! ["--help"]))))]
    (is (string/includes? out "Usage: soranoha"))
    (is (string/includes? out "Commands:"))
    (doseq [command ["explain-request-set"
                     "annotation-join-stats-run"]]
      (is (string/includes? out command)))))

(deftest command-help-does-not-run-command-test
  (let [ran? (atom false)]
    (with-redefs [soranoha/publication-report! (fn [& _] (reset! ran? true))]
      (is (= 0 (soranoha/run! ["publication-report" "--help"]))))
    (is (false? @ran?))))

(deftest malformed-help-requests-are-usage-errors-test
  (doseq [args [["help" "nope"] ["help" "explain-request-set" "extra"]]]
    (let [err (java.io.StringWriter.)]
      (binding [*err* err]
        (is (= 2 (soranoha/run! args))))
      (is (not (string/blank? (str err)))))))

(deftest unknown-command-returns-nonzero-test
  (let [err (java.io.StringWriter.)]
    (binding [*err* err]
      (is (= 2 (soranoha/run! ["nope"]))))
    (is (string/includes? (str err) "Unknown command"))))

;; Executable proof that the competing publication composition is gone: the
;; retired dispatcher commands no longer resolve, and the five retired producer
;; vars are absent from the namespace.
(deftest retired-publication-producer-surface-is-absent-test
  (testing "retired dispatcher commands return the unknown-command exit"
    (doseq [command ["build-publication" "snapshot-index" "reproduce"
                     "publication-rehearsal" "validate-workflow"]]
      (let [err (java.io.StringWriter.)]
        (binding [*err* err]
          (is (= 2 (soranoha/run! [command])) command))
        (is (string/includes? (str err) "Unknown command") command))))
  (testing "retired producer vars no longer resolve"
    (doseq [var-name ['build-snapshot-index 'snapshot-index!
                      'materialize-snapshot-root! 'reproduce!
                      'publication-rehearsal!]]
      (is (nil? (ns-resolve 'abc.tools.soranoha var-name))
          (str var-name)))))
