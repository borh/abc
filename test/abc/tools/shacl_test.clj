(ns abc.tools.shacl-test
  (:require [abc.tools.shacl :as shacl]
            [clojure.test :refer [deftest is testing]])
  (:import [org.apache.jena.graph Graph]))

(deftest load-shapes-graph-test
  (testing "loads the manifest SHACL shapes file as a Jena graph"
    (let [g (shacl/load-shapes-graph)]
      (is (instance? Graph g))
      (is (pos? (count (iterator-seq (.find g))))
          "shapes graph must contain triples"))))
