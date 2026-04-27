(ns abc.tools.shacl-test
  (:require [abc.tools.files :as files]
            [abc.tools.manifest-to-rdf :as manifest-to-rdf]
            [abc.tools.shacl :as shacl]
            [clojure.test :refer [deftest is testing]])
  (:import [org.apache.jena.graph Graph]))

(deftest load-shapes-graph-test
  (testing "loads the manifest SHACL shapes file as a Jena graph"
    (let [g (shacl/load-shapes-graph)]
      (is (instance? Graph g))
      (is (pos? (count (iterator-seq (.find g))))
          "shapes graph must contain triples"))))

(deftest validate-success-manifest-conforms-test
  (testing "example success manifest RDF conforms to the SHACL shapes"
    (let [shapes (shacl/load-shapes-graph)
          manifest (files/read-json "examples/v0/example-work/manifest.json")
          data (manifest-to-rdf/manifest->graph manifest)]
      (is (= :ok (shacl/validate! {:shapes-graph shapes
                                   :data-graph data
                                   :label "examples/v0/example-work/manifest.json"}))))))

(deftest validate-failure-manifest-conforms-test
  (testing "example failure manifest RDF conforms to the SHACL shapes"
    (let [shapes (shacl/load-shapes-graph)
          manifest (files/read-json "examples/v0/example-work/failure-manifest.example.json")
          data (manifest-to-rdf/manifest->graph manifest)]
      (is (= :ok (shacl/validate! {:shapes-graph shapes
                                   :data-graph data
                                   :label "examples/v0/example-work/failure-manifest.example.json"}))))))
