(ns abc.tools.soranoha-test
  (:require [abc.tools.files :as files]
            [abc.tools.soranoha :as soranoha]
            [clojure.string :as string]
            [clojure.test :refer [deftest is]]))

(deftest list-request-sets-prints-checked-in-labels-test
  (let [out (with-out-str
              (is (zero? (soranoha/run! ["list-request-sets"]))))]
    (is (string/includes? out "smoke-basic-ja"))
    (is (string/includes? out "full-corpus-basic-ja"))))

(deftest explain-request-set-prints-request-set-id-test
  (let [fixture (files/read-json "data/request-sets/smoke-basic-ja.json")
        out (with-out-str
              (is (zero? (soranoha/run! ["explain-request-set"
                                         "smoke-basic-ja"]))))]
    (is (string/includes? out "smoke-basic-ja"))
    (is (string/includes? out (get fixture "request_set_id")))))

(deftest snapshot-index-command-prints-fixture-identity-test
  (let [fixture (files/read-json "examples/v0/snapshot/snapshot-index.json")
        out (with-out-str
              (is (zero? (soranoha/run! ["snapshot-index"]))))]
    (is (string/includes? out (get fixture "snapshot_label")))
    (is (string/includes? out (get fixture "snapshot_identity_hash")))))

(deftest unknown-command-returns-nonzero-test
  (let [err (java.io.StringWriter.)]
    (binding [*err* err]
      (is (= 2 (soranoha/run! ["nope"]))))
    (is (string/includes? (str err) "unknown command"))))
