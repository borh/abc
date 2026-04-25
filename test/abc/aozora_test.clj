(ns abc.aozora-test
  (:require [abc.aozora :as aozora :refer :all]
            [clojure.test :as t :refer [deftest is use-fixtures]]
            [malli.core :as m]
            [malli.generator :as mg]))

(def ^:dynamic ^:private *example-entity* nil)

(defn fixture [f]
  (binding [*example-entity* (mg/generate [:schema {:registry registry}
                                           :abc.aozora/entity-map])]
    (f)))

(use-fixtures :once fixture)

(deftest entity-test
  (is *example-entity*))
