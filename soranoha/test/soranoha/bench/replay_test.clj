(ns soranoha.bench.replay-test
  (:require [babashka.fs :as fs]
            [clojure.test :refer [deftest is]]
            [charred.api :as json]
            [soranoha.bench.replay :as replay]
            [soranoha.kura.engine :as engine]
            [soranoha.main :as main]
            [soranoha.za.corpus :as corpus]))

(deftest replay-isolated-history-and-noop-contract
  (let [work {:work-id "000001" :person-id "000001" :card "000001"
              :book "1" :n "1" :text "one"}
        repo (corpus/init-corpus! [work])
        first-revision (main/source-provenance! repo)
        _ (corpus/write-work! repo (assoc work :text "two"))
        second-revision (corpus/commit-corpus! repo)
        parent (fs/create-temp-dir {:prefix "replay-test"})
        out (fs/path parent "run")
        opts {:repo repo :from first-revision :to second-revision :out (str out)
              :assets-root "." :clj-toolchain-id corpus/fixture-toolchain}]
    (try
      (is (= [first-revision second-revision]
             (replay/revisions repo first-revision second-revision)))
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"first-parent"
                            (replay/revisions repo second-revision first-revision)))
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"outside the source repository"
                            (replay/replay! (assoc opts :out (str (fs/path repo "run"))))))
      (with-redefs-fn {#'main/build-stages (constantly (dissoc corpus/stage-set :fidelity))}
        #(is (= 2 (:revisions (replay/replay! opts)))))
      (let [rows (mapv json/read-json (line-seq (java.io.BufferedReader.
                                                 (java.io.StringReader.
                                                  (slurp (str (fs/path out "measurements.jsonl")))))))]
        (is (= ["setup" "cold-build" "incremental"] (mapv #(get % "phase") rows)))
        (is (= 1 (get-in rows [2 "delta" "source" "changed"]))))
      (is (= second-revision (main/source-provenance! repo)))
      (is (thrown? java.nio.file.FileAlreadyExistsException (replay/replay! opts)))
      (let [run-stage! engine/run-stage!]
        (with-redefs-fn {#'main/build-stages (constantly (dissoc corpus/stage-set :fidelity))
                         #'engine/run-stage! (fn [& args]
                                               (assoc (apply run-stage! args) :cached? false))}
          #(is (thrown-with-msg? clojure.lang.ExceptionInfo #"unchanged derivations"
                                 (replay/replay! (assoc opts :out (str (fs/path parent "broken"))))))))
      (finally
        (fs/delete-tree parent)
        (fs/delete-tree repo)))))
