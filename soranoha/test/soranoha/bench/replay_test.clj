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
      (with-redefs-fn {#'main/build-stages (constantly (dissoc corpus/stage-set :accountability :coverage))}
        #(is (= 2 (:revisions (replay/replay! opts)))))
      (let [rows (mapv json/read-json (line-seq (java.io.BufferedReader.
                                                 (java.io.StringReader.
                                                  (slurp (str (fs/path out "measurements.jsonl")))))))]
        (is (= ["setup" "cold-build" "incremental"] (mapv #(get % "phase") rows)))
        (is (= 1 (get-in rows [2 "delta" "source" "changed"]))))
      (is (= second-revision (main/source-provenance! repo)))
      (is (thrown? java.nio.file.FileAlreadyExistsException (replay/replay! opts)))
      (let [run-stage! engine/run-stage!]
        (with-redefs-fn {#'main/build-stages (constantly (dissoc corpus/stage-set :accountability :coverage))
                         #'engine/run-stage! (fn [& args]
                                               (assoc (apply run-stage! args) :cached? false))}
          #(is (thrown-with-msg? clojure.lang.ExceptionInfo #"unchanged derivations"
                                 (replay/replay! (assoc opts :out (str (fs/path parent "broken"))))))))
      (finally
        (fs/delete-tree parent)
        (fs/delete-tree repo)))))

(deftest failed-work-batches-retain-all-diagnostics-and-resume-completed-work
  (let [works (mapv (fn [id text]
                      {:work-id id :person-id "000001" :card "000001"
                       :book id :n "1" :text text})
                    ["000001" "000002" "000003"] ["broken-one" "healthy" "broken-two"])
        repo (corpus/init-corpus! works)
        root (fs/create-temp-dir {:prefix "batch-failure-test"})
        out (str (fs/path root "exports"))
        opts {:root (str root) :aozora-root repo :assets-root "." :out out
              :clj-toolchain-id corpus/fixture-toolchain :concurrency 3}
        parse (get-in corpus/stage-set [:parse :f])
        failing (assoc-in corpus/stage-set [:parse :f]
                          (fn [{:keys [blob] :as context} inputs]
                            (let [text (String. ^bytes (blob (get inputs "source")) "UTF-8")]
                              (if (.startsWith text "broken")
                                (throw (ex-info "source rejected" {:source text}))
                                (parse context inputs)))))
        build #(binding [*out* (java.io.StringWriter.)] (main/build! opts))]
    (try
      (let [failure (with-redefs-fn {#'main/build-stages (constantly failing)}
                      #(try (build) nil (catch clojure.lang.ExceptionInfo e (ex-data e))))]
        (is (= :build/work-failures (:reason failure)))
        (is (= (mapv corpus/work-slug [(first works) (last works)])
               (mapv :slug (:failures failure))))
        (is (= ["broken-one" "broken-two"] (mapv #(get-in % [:data :source]) (:failures failure))))
        (is (not (fs/exists? out))))
      (let [report (with-redefs-fn {#'main/build-stages (constantly corpus/stage-set)} build)]
        (is (every? true? (vals (get-in report ["works" (corpus/work-slug (second works)) "cached"]))))
        (is (false? (get-in report ["works" (corpus/work-slug (first works)) "cached" "parse"])))
        (is (fs/exists? (fs/path out "build.json"))))
      (finally (fs/delete-tree root) (fs/delete-tree repo)))))
