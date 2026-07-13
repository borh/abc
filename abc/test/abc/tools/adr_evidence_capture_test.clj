(ns abc.tools.adr-evidence-capture-test
  (:require [abc.tools.adr-evidence-capture :as capture]
            [abc.tools.json :as json]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is]])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(defn- temp-dir [prefix]
  (.toFile (Files/createTempDirectory prefix (make-array FileAttribute 0))))

(defn- exec! [dir & argv]
  (let [process (.start (doto (ProcessBuilder. argv)
                          (.directory dir)
                          (.redirectErrorStream true)))]
    (slurp (.getInputStream process))
    (.waitFor process)))

(defn- git-repo []
  (let [repo (temp-dir "abc-evidence-capture-repo")
        source (io/file repo "src/example/core.clj")]
    (.mkdirs (.getParentFile source))
    (spit source "(ns example.core)\n")
    (exec! repo "git" "init" "-q")
    (exec! repo "git" "config" "user.email" "capture@example.invalid")
    (exec! repo "git" "config" "user.name" "Capture Test")
    (exec! repo "git" "add" ".")
    (exec! repo "git" "commit" "-q" "-m" "fixture")
    repo))

(defn- descriptor [argv]
  {:schema-version "abc-adr-evidence-capture-v1"
   :tool "sh"
   :argv argv
   :input-profile {:kind "repo-files-v1"
                   :roots []
                   :explicit ["src/example/core.clj"]}
   :observation-key "command-passed"})

(deftest run-process-captures-working-directory-output-and-status
  (let [run-process (ns-resolve 'abc.tools.adr-evidence-capture 'run-process)
        dir (temp-dir "abc-evidence-capture-process")
        result (@run-process dir
                             ["sh" "-c"
                              "printf %s \"$PWD\"; printf err >&2; exit 4"])]
    (is (= #{:exit-code :stdout :stderr} (set (keys result))))
    (is (= 4 (:exit-code result)))
    (is (= (.getCanonicalPath dir) (:stdout result)))
    (is (= "err" (:stderr result)))))

(deftest captures-byte-identical-clean-tree-bundles
  (let [repo (git-repo)
        output-root (temp-dir "abc-evidence-capture-output")
        first-output (io/file output-root "first.json")
        second-output (io/file output-root "second.json")
        first-result (capture/capture!
                      {:repo-root repo :descriptor (descriptor ["sh" "-c" "exit 0"])
                       :output first-output})
        second-result (capture/capture!
                       {:repo-root repo :descriptor (descriptor ["sh" "-c" "exit 0"])
                        :output second-output})
        value (json/read-json-file first-output)]
    (is (= 0 (:exit-code first-result)))
    (is (= (slurp first-output) (slurp second-output)))
    (is (= "abc-adr-evidence-run-v1" (get value "schema_version")))
    (is (= true (get-in value ["observations" "command-passed" "value"])))
    (is (= 0 (get-in value ["observations" "command-passed" "details" "exit_code"])))
    (is (= 40 (count (get-in value ["producer" "revision"]))))))

(deftest dirty-tree-is-rejected-before-and-after-command
  (let [repo (git-repo)
        output-root (temp-dir "abc-evidence-capture-dirty")
        output (io/file output-root "bundle.json")
        marker (io/file output-root "executed")]
    (spit (io/file repo "untracked") "dirty")
    (is (thrown? clojure.lang.ExceptionInfo
                 (capture/capture!
                  {:repo-root repo
                   :descriptor (descriptor ["sh" "-c"
                                            (str "touch " (.getAbsolutePath marker))])
                   :output output})))
    (is (not (.exists marker)))
    (.delete (io/file repo "untracked"))
    (is (thrown? clojure.lang.ExceptionInfo
                 (capture/capture!
                  {:repo-root repo
                   :descriptor (descriptor ["sh" "-c" "touch command-dirtied"])
                   :output output})))
    (is (not (.exists output)))))

(deftest failing-command-is-captured-before-cli-failure
  (let [repo (git-repo)
        output (io/file (temp-dir "abc-evidence-capture-fail") "bundle.json")
        result (capture/capture!
                {:repo-root repo :descriptor (descriptor ["sh" "-c" "exit 7"])
                 :output output})]
    (is (= 1 (:exit-code result)))
    (is (= 7 (get-in (json/read-json-file output)
                     ["observations" "command-passed" "details" "exit_code"])))))

(deftest descriptor-key-set-is-closed
  (let [repo (git-repo)
        output (io/file (temp-dir "abc-evidence-capture-invalid") "bundle.json")]
    (is (thrown? clojure.lang.ExceptionInfo
                 (capture/capture!
                  {:repo-root repo
                   :descriptor (assoc (descriptor ["true"]) :unexpected true)
                   :output output})))
    (is (not (.exists output)))))
