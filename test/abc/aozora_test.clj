(ns abc.aozora-test
  (:require [abc.aozora :as aozora :refer :all]
            [abc.tools.malli :as am]
            [clojure.test :as t :refer [deftest is use-fixtures]]
            [malli.core :as m]
            [malli.generator :as mg]))

(def ^:dynamic ^:private *example-entity* nil)

(defn fixture [f]
  (am/install!)
  (binding [*example-entity* (mg/generate :abc.aozora/entity-map)]
    (f)))

(use-fixtures :once fixture)

(deftest entity-test
  (is *example-entity*))
