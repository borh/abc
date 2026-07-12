(ns abc.tools.adr-governance-test
  (:require [abc.tools.adr :as adr]
            [abc.tools.adr-governance :as governance]
            [abc.tools.json :as json]
            [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.test :refer [deftest is testing]]))

(def ^:private sample-problem
  {:file "docs/adr/0031-adr-governance-validation.md"
   :kind :noncanonical-dependency-path
   :message "Accepted ADR reaches Proposed ADR"
   :path [31 29]})

(defn- run-mode [mode]
  (try
    (governance/run! "." {:mode mode})
    (catch clojure.lang.ArityException _ nil)))

(deftest audit-and-enforce-mode-test
  (with-redefs [adr/validate-repository (fn [_repo-root] [sample-problem])
                adr/validate-repository-legacy (fn [_repo-root] [])]
    (testing "audit reports problems without making the migration gate red"
      (is (= {:ok? false
              :exit-code 0
              :mode :audit
              :problems [sample-problem]}
             (run-mode :audit))))
    (testing "enforcement fails while legacy remains pre-migration compatible"
      (is (= 1 (:exit-code (run-mode :enforce))))
      (is (= 0 (:exit-code (run-mode :legacy)))))))

(deftest audit-report-is-deterministic-json-test
  (let [dir (.toFile (java.nio.file.Files/createTempDirectory
                      "adr-governance-test"
                      (make-array java.nio.file.attribute.FileAttribute 0)))
        report (io/file dir "report.json")]
    (try
      (with-redefs [adr/validate-repository (fn [_repo-root] [sample-problem])
                    adr/validate-repository-legacy (fn [_repo-root] [])]
        (let [run-cli! (ns-resolve 'abc.tools.adr-governance 'run-cli!)
              result (when run-cli!
                       (run-cli! ["--mode" "audit"
                                  "--report" (.getPath report) "."]))
              value (when (.isFile report) (json/read-json-file report))]
          (is (some? run-cli!))
          (is (.isFile report))
          (is (= 0 (:exit-code result)))
          (is (= "audit" (get value "mode")))
          (is (false? (get value "ok")))
          (is (= [31 29] (get-in value ["problems" 0 "path"])))))
      (finally
        (doseq [file (reverse (file-seq dir))]
          (.delete file))))))

(deftest nix-governance-check-selects-audit-mode-test
  (let [flake (slurp "flake.nix")]
    (is (string/includes? flake
                          "clojure -M:abc/adr-governance --mode audit"))))
