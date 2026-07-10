(ns abc.tools.diagram.architecture-graph-test
  (:require [clojure.test :refer [deftest is]]
            [abc.tools.diagram.architecture-graph :as arch]
            [abc.tools.json :as json]))

(deftest schemas-and-adrs-and-inputs-resolve
  (is (= [] (arch/lint*))))

(deftest build-is-deterministic-flowchart-input
  (let [g (arch/build)]
    (is (= "TD" (:direction g)))
    (is (= (arch/build) g))
    (is (seq (:nodes g)))))

(deftest manifest-identity-coordinate-ownership-is-total
  (let [doc (arch/load-stages)
        required (arch/manifest-identity-required
                  (json/read-json-file "schemas/manifest.schema.json"))]
    (is (= required
           (set (keys (get-in doc [:manifest-identity-contract :coordinates])))))
    (is (= #{1 10 23 27 28}
           (set (mapcat val
                        (get-in doc [:manifest-identity-contract :coordinates])))))))

(deftest architecture-prose-lists-the-live-identity-contract
  (is (= (arch/manifest-identity-required
          (json/read-json-file "schemas/manifest.schema.json"))
         (arch/documented-identity-coordinates
          (slurp "docs/architecture.md")))))
