(ns soranoha.logging-test
  (:require [babashka.process :as process]
            [clojure.test :refer [deftest is]]))

(deftest fresh-cli-startup-keeps-results-and-diagnostics-separate
  (let [result (process/sh
                {:env (dissoc (into {} (System/getenv))
                              "JAVA_TOOL_OPTIONS" "JDK_JAVA_OPTIONS" "_JAVA_OPTIONS")}
                (str (System/getProperty "java.home") "/bin/java")
                "-cp" (System/getProperty "java.class.path")
                "clojure.main" "-e"
                (str "(require 'soranoha.main) "
                     "(println \"result-marker\") "
                     "(soranoha.main/-main \"unknown-command\")"))]
    (is (= 2 (:exit result)))
    (is (= "result-marker\n" (:out result)))
    (is (re-matches #"usage: [^\n]+\n" (:err result)))))
