(ns abc.tools.diagram.registry-test
  (:require [clojure.test :refer [deftest is]]
            [abc.tools.diagram.core :as core]
            [abc.tools.diagram.registry :as registry]))

(deftest registry-ids-are-the-closed-committed-set
  ;; Guard the drift gate against a vacuous pass: dropping a registry entry
  ;; would silently remove its drift check while every other test stays green.
  (is (= #{:adr-decision-map :architecture}
         (set (map :id registry/committed-diagrams)))))

(deftest committed-diagrams-are-current
  (doseq [d registry/committed-diagrams]
    (is (= (slurp (:out-path d)) (core/render d))
        (str (:id d) " committed file is stale vs core/render"))))
