(ns abc.tei-test
  (:require [abc.tei :refer :all]
            [clojure.test :as t :refer [deftest is use-fixtures]]
            [clojure.spec.test.alpha :as stest]
            [clojure.spec.alpha :as s]
            [orchestra.spec.test :as st]
            [expound.alpha :as expound]
            [clojure.data.xml :as xml]))

(alter-var-root #'s/*explain-out* (constantly expound/printer))

(st/instrument)

(stest/check (stest/enumerate-namespace 'abc.tei))

(def ^:dynamic ^:private *metadata* nil)
(def ^:dynamic ^:private *text* nil)

(defn tei-fixture [f]
  (binding [*metadata* (ffirst (s/exercise :tei/header 1))
            *text* (ffirst (s/exercise :document/text 1))]
    (f)))

(use-fixtures :once tei-fixture)

(deftest header-test
  (is *metadata*)
  (is (s/exercise :tei/header 10))
  (is (header *metadata*)))

(deftest body-test
  (is *text*)
  (is (body *text*)))

(deftest document-test
  (is (doc *metadata* *text*))
  (is (string? (xml/emit-str (doc *metadata* *text*)))))

#_(deftest serialization-test
    (let [d (doc *metadata* *text*)
          _ (save! "tmp.xml" d)]
      (is (= d (load-tei "tmp.xml")))))
