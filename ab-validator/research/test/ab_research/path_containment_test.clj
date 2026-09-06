(ns ab-research.path-containment-test
  (:require [ab-research.path-containment :as containment]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is]])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(defn- temp-dir []
  (.toFile (Files/createTempDirectory
            "abc-path-containment-test" (make-array FileAttribute 0))))

(defn- write-path! [root path body]
  (let [file (io/file root path)]
    (.mkdirs (.getParentFile file))
    (spit file body)
    file))

(deftest lexical-and-real-path-containment-states-are-distinct
  (let [repo (temp-dir)
        outside (temp-dir)
        inside (write-path! repo "docs/evidence/run.json" "{}")
        outside-file (write-path! outside "escaped.json" "{}")]
    (.mkdirs (io/file repo "docs/evidence"))
    (Files/createSymbolicLink (.toPath (io/file repo "docs/evidence/escape.json"))
                              (.toPath outside-file)
                              (make-array FileAttribute 0))
    (is (= {:state :ok
            :path inside
            :relative "docs/evidence/run.json"}
           (containment/path-state repo "docs/evidence/run.json")))
    (is (= :path-traversal
           (:state (containment/path-state repo "docs/../outside.json"))))
    (is (= :path-traversal
           (:state (containment/path-state repo (.getAbsolutePath outside-file)))))
    (is (= :malformed-path
           (:state (containment/path-state repo "docs/\u0000bad.json"))))
    (is (= :missing
           (:state (containment/path-state repo "docs/evidence/missing.json"))))
    (is (= :real-path-escape
           (:state (containment/path-state repo "docs/evidence/escape.json"))))))
