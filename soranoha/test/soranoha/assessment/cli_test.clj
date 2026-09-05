(ns soranoha.assessment.cli-test
  (:require [babashka.fs :as fs]
            [babashka.process :as process]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [soranoha.assessment.records :as records]))

(defn- cli-result [args]
  (apply process/sh
         {:out :string :err :string}
         (str (fs/path (System/getProperty "java.home") "bin" "java"))
         "-cp" (System/getProperty "java.class.path")
         "clojure.main" "-m" "soranoha.main" args))

(deftest filesystem-input-errors-have-the-cli-error-contract
  (let [dir (fs/create-temp-dir {:prefix "assessment-cli"})
        source (str (fs/path dir "source.json"))
        missing (str (fs/path dir "missing"))
        input (assoc records/empty-source "observations"
                     [{"id" "evidence" "selector" "retained-evidence"
                       "path" "evidence.txt" "sha256" (apply str (repeat 64 "a"))}])]
    (try
      (fs/write-bytes source (:bytes (records/encode input)))
      (doseq [[label args]
              [["missing source" ["assessment-evaluate" "--assessment-source" missing
                                  "--as-of" "2026-09-05"]]
               ["missing evidence directory" ["assessment-evaluate" "--assessment-source" source
                                              "--as-of" "2026-09-05" "--evidence-root" missing]]
               ["missing snapshot" ["release" "--assessment" missing
                                    "--aozora-root" missing "--chain-clone" missing
                                    "--upstream-origin" "https://example.invalid/corpus.git"
                                    "--policy" missing "--release-pub" missing
                                    "--governance-pub" missing "--release-key" missing]]]]
        (testing label
          (let [{:keys [exit err]} (cli-result args)]
            (is (= 1 exit))
            (is (str/includes? err "error: input/output operation failed"))
            (is (str/includes? err ":reason :io-failure"))
            (is (not (str/includes? err "Execution error"))))))
      (finally (fs/delete-tree dir)))))

(deftest unavailable-subprocess-has-the-cli-error-contract
  (let [program "(require '[soranoha.main :as main] '[babashka.process :as process]) (with-redefs [main/assessment-evaluate! (fn [_] (process/sh \"git\" \"--version\"))] (main/-main \"assessment-evaluate\"))"
        {:keys [exit err]}
        (process/sh {:out :string :err :string :extra-env {"PATH" "/no-executables"}}
                    (str (fs/path (System/getProperty "java.home") "bin" "java"))
                    "-cp" (System/getProperty "java.class.path")
                    "clojure.main" "-e" program)]
    (is (= 1 exit))
    (is (str/includes? err ":reason :io-failure"))
    (is (str/includes? err "git"))
    (is (not (str/includes? err "Execution error")))))
