(ns ab-research.aat-parser-ir-compat-test
  "Characterization tests written before migrating -main onto ab-research.cli.
  Pins admission-report's status logic and the CLI exit-code contract."
  (:require [ab-research.aat-parser-ir-compat :as compat]
            [ab-research.cli :as cli]
            [ab-research.files :as files]
            [clojure.string :as string]
            [clojure.test :refer [deftest is testing]]))

(def registry (files/read-edn (str compat/registry-path)))

(deftest self-admission-is-admitted-test
  (testing "the registry compared against itself admits every entry"
    (let [report (compat/admission-report registry registry)]
      (is (= :admitted (:status report)))
      (is (= (count (:entries registry)) (count (:admitted report))))
      (is (empty? (:missing report)))
      (is (empty? (:conflicts report))))))

(deftest novel-match-key-is-missing-test
  (testing "a candidate whose match keys are absent from the registry is :missing"
    (let [novel (assoc (first (:entries registry)) :aat_version 99999)
          report (compat/admission-report registry {:entries [novel]})]
      (is (= :missing (:status report)))
      (is (= 1 (count (:missing report)))))))

(deftest same-match-key-changed-evidence-is-conflict-test
  (testing "a candidate with existing match keys but changed non-match field is :conflict"
    (let [entry (first (:entries registry))
          ;; change a non-match-key numeric evidence field: still schema-valid,
          ;; but differs from the registry entry -> conflict, not admitted.
          changed (update entry :compatibility
                          {"lossy" "lossless" "lossless" "lossy"})
          report (compat/admission-report registry {:entries [changed]})]
      (is (= :conflict (:status report)))
      (is (= 1 (count (:conflicts report)))))))

(deftest append-missing-is-exact-and-append-only-test
  (let [entry (first (:entries registry))
        novel (assoc entry :aat_version 99999)
        appended (compat/append-missing registry {:entries [novel]})]
    (is (= (:entries registry)
           (subvec (:entries appended) 0 (count (:entries registry)))))
    (is (= novel (last (:entries appended))))
    (is (= (inc (count (:entries registry)))
           (count (:entries appended))))))

(deftest append-missing-refuses-non-missing-input-test
  (let [entry (first (:entries registry))
        conflict (update entry :compatibility
                         {"lossy" "lossless" "lossless" "lossy"})]
    (is (thrown? clojure.lang.ExceptionInfo
                 (compat/append-missing registry {:entries [entry]})))
    (is (thrown? clojure.lang.ExceptionInfo
                 (compat/append-missing registry {:entries [conflict]})))
    (is (thrown? clojure.lang.ExceptionInfo
                 (compat/append-missing registry {:entries [{}]})))))

(defn- run-dispatch [args]
  (let [err (java.io.StringWriter.)
        code (binding [*err* err]
               (cli/dispatch!
                (cli/parse args {:cli-options compat/cli-options :required [:candidates]})
                {:usage-fn compat/usage
                 :run (fn [_] (throw (ex-info "run should not be reached" {})))}))]
    {:code code :err (str err)}))

(deftest missing-candidates-exits-2-test
  (let [{:keys [code err]} (run-dispatch [])]
    (is (= 2 code))
    (is (string/includes? err "Usage: clojure -M:abc/aat-compat-admission"))))
