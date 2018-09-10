(ns abc.aozora-test
  (:require [abc.aozora :as aozora :refer :all]
            [clojure.test :as t :refer [deftest is use-fixtures]]
            [clojure.spec.test.alpha :as stest]
            [clojure.spec.alpha :as s]
            [expound.alpha :as expound]
            [orchestra.spec.test :as st]))

(alter-var-root #'s/*explain-out* (constantly expound/printer))

(st/instrument)

(stest/check (stest/enumerate-namespace 'abc.aozora))

(def ^:dynamic ^:private *entities* nil)

(defn fixture [f]
  (binding [*entities* (ffirst (s/exercise :abc.aozora/entity-coll))]
    (f)))

(use-fixtures :once fixture)

(deftest entity-test
  (is *entities*))
