(ns abc.load-test
  (:require [abc.load :as load :refer :all]
            [abc.config :as config]
            [abc.aozora :as aozora]
            [clojure.test :as t :refer [deftest is use-fixtures]]
            [clojure.spec.test.alpha :as stest]
            [clojure.spec.alpha :as s]
            [expound.alpha :as expound]
            [orchestra.spec.test :as st]))

(st/instrument)

(stest/check (stest/enumerate-namespace 'abc.load))

(alter-var-root #'s/*explain-out* (constantly expound/printer))

(def ^:dynamic ^:private *db* nil)

(defn db-fixture [f]
  (let [db (take 5 (aozora-bunko-db-coll (aozora-bunko-db config/aozora-bunko-path)))]
    (binding [*db* db]
      (f))))

(use-fixtures :once db-fixture)

(deftest load-test
  (is *db*))

(deftest extract-texts-test
  (is (nil? (dorun (extract-texts *db*)))))
