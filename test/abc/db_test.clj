(ns abc.db-test
  (:require [abc.db :as db :refer :all]
            [abc.aozora :as aozora]
            [abc.load :as load]
            [abc.config :as config]
            [arachne.aristotle.validation :as validation]
            [arachne.aristotle.query :as q]
            [clojure.test :as t :refer [deftest is use-fixtures]]
            [clojure.spec.test.alpha :as stest]
            [clojure.spec.alpha :as s]
            [orchestra.spec.test :as st]
            [expound.alpha :as expound])
  (:import [org.apache.jena.riot RDFFormat]))

(alter-var-root #'s/*explain-out* (constantly expound/printer))

(st/instrument)

(stest/check (stest/enumerate-namespace 'abc.db))

(def ^:dynamic ^:private *generated-graph* nil)
(def ^:dynamic ^:private *graph* nil)
(def ^:dynamic ^:private *entities* nil)

(defn graph-fixture [f]
  (let [data (take 10 (load/aozora-bunko-db-coll (load/aozora-bunko-db config/aozora-bunko-path)))
        triples (to-triples data)
        graph (to-graph triples)]
    (binding [*graph* graph
              *entities* (ffirst (s/exercise :abc.aozora/entity-coll))]
      (f))))

(use-fixtures :once graph-fixture)

(deftest rdf-datatypes-test
  (let [g (to-graph (to-triples *entities*))]
    (is (validation/validate g))))

(deftest validation-test
  (is (validation/validate *graph*)))

(deftest query-test
  (is (= 105 (count (q/run *graph* '[?work]
                      '[:distinct [:bgp
                                   [?work :abc.aozora/author ?author]
                                   [?author :abc.aozora/family-name ?family-name]]]
                      {'?family-name "夏目"})))))

(comment
  (deftest serialization-test
    (let [test-filename "tmp.ttl" #_"tmp.rt"]
      (save-graph! test-filename *graph*
                   ;; :format RDFFormat/RDF_THRIFT #_RDFFormat/TURTLE_PRETTY
                   )
      (let [graph' (load-graph test-filename)]
        (is (= (.size graph') (.size *graph*)))
        (is (.isIsomorphicWith graph' *graph*))))))
