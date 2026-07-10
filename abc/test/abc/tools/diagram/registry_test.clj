(ns abc.tools.diagram.registry-test
  (:require [clojure.test :refer [deftest is]]
            [abc.tools.diagram.core :as core]
            [abc.tools.diagram.registry :as registry]))

(deftest registry-is-not-empty
  ;; Guard the drift gate against a vacuous pass: an emptied registry would make
  ;; committed-diagrams-are-current iterate zero times and go green checking nothing.
  (is (seq registry/committed-diagrams)))

(deftest committed-diagrams-are-current
  (doseq [d registry/committed-diagrams]
    (is (= (slurp (:out-path d)) (core/render d))
        (str (:id d) " committed file is stale vs core/render"))))
