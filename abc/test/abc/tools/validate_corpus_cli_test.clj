(ns abc.tools.validate-corpus-cli-test
  "Pilot CLI-contract test for the abc.tools.cli migration: exercises
  validate-corpus's real cli-options + usage wiring through the shared
  dispatch! and pins the exit-code + stderr contract. Full subprocess
  smoke (`clojure -M:abc/validate-corpus`) was verified manually during
  the pilot; this keeps the contract green without a per-test JVM spawn."
  (:require [abc.tools.cli :as cli]
            [abc.tools.validate-corpus :as vc]
            [clojure.string :as string]
            [clojure.test :refer [deftest is testing]]))

(defn- run-dispatch [args]
  (let [err (java.io.StringWriter.)
        code (binding [*err* err]
               (cli/dispatch!
                (cli/parse args {:cli-options vc/cli-options :required [:input-dir]})
                {:usage-fn vc/usage
                 :run (fn [_] (throw (ex-info "run should not be reached" {})))}))]
    {:code code :err (str err)}))

(deftest missing-required-input-dir-exits-2-with-usage-on-stderr-test
  (let [{:keys [code err]} (run-dispatch [])]
    (is (= 2 code))
    (is (string/includes? err "Missing required option(s): input-dir"))
    (is (string/includes? err "Usage: clojure -M:abc/validate-corpus"))))

(deftest unknown-flag-exits-2-with-error-on-stderr-test
  (let [{:keys [code err]} (run-dispatch ["--bogus"])]
    (is (= 2 code))
    (is (string/includes? err "Unknown option"))
    (is (string/includes? err "Usage:"))))

(deftest strips-leading-double-dash-before-parsing-test
  (testing "clojure -M:tool -- --bogus still parses --bogus (not the separator)"
    (let [{:keys [code err]} (run-dispatch ["--" "--bogus"])]
      (is (= 2 code))
      (is (string/includes? err "Unknown option")))))
