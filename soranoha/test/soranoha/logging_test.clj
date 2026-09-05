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
                (str "(require 'soranoha.main 'soranoha.ported.logging) "
                     "(println \"result-marker\") "
                     "(soranoha.ported.logging/log! :error \"diagnostic-marker\")"))]
    (is (zero? (:exit result)))
    (is (= "result-marker\n" (:out result)))
    (is (= "error diagnostic-marker\n" (:err result)))))
