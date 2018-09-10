(ns abc.git-test
  (:require [abc.git :refer :all]
            [clojure.test :as t :refer [deftest is use-fixtures]]
            [clojure.spec.test.alpha :as stest]
            [clojure.spec.alpha :as s]
            [orchestra.spec.test :as st]
            [expound.alpha :as expound]
            [me.raynes.fs :as fs]))

(alter-var-root #'s/*explain-out* (constantly expound/printer))

(st/instrument)

(stest/check (stest/enumerate-namespace 'abc.git))

(def ^:dynamic ^:private *ab-repo* nil)
(def ^:dynamic ^:private *repo* nil)

(defn fixture [f]
  (binding [*ab-repo* (load-aozora-bunko-git)
            *repo* (fs/temp-dir "aozora-bunko-tei")]
    (f)
    (fs/delete-dir *repo*)))

(use-fixtures :once fixture)

(deftest git-log
  (is (> (count (get-file-log *ab-repo* "index.html"))
         1)
      (= 167 (count (file-time-span *ab-repo* "index.html" #inst "2012" #inst "2016")))))
