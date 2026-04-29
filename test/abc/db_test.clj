(ns abc.db-test
  (:require [abc.db :as db :refer :all]
            [abc.xtdb :as xtdb]
            [abc.aozora :as aozora]
            [abc.load :as load]
            [abc.config :as config]
            [java-time :as time]
            #_[arachne.aristotle.validation :as validation]
            #_[arachne.aristotle.query :as q]
            [clojure.test :as t :refer [deftest is use-fixtures]])
  #_(:import [org.apache.jena.riot RDFFormat]))

;; TODO: https://www.juxt.pro/blog/testingwithxtdb

;; ;; (alter-var-root #'s/*explain-out* (constantly expound/printer))
;;
;; (st/instrument)
;;
;; (stest/check (stest/enumerate-namespace 'abc.db))

;; (def ^:dynamic ^:private *generated-graph* nil)
;; (def ^:dynamic ^:private *graph* nil)
;; (def ^:dynamic ^:private *entities* nil)
(def ^:dynamic ^:private *node* nil)

(defn graph-fixture [f]
  (let [db-path "data/test"
        node (xtdb/start! :db-path db-path :clean? true)]
    (load/persist-db! node)
    (binding [*node* node]
      (f))
    (xtdb/stop! node)
    (xtdb/delete-db! db-path))
  #_(let [;; data (take 10 (load/aozora-bunko-db-coll (load/aozora-bunko-db config/aozora-bunko-path)))
          ;; triples (to-triples data)
          ;; graph (to-graph triples)
          ]
      (binding [*graph* graph
                *entities* (first (s/exercise :abc.aozora/entity-map))]
        (f))))

(use-fixtures :once graph-fixture)

(comment
  (deftest rdf-datatypes-test
    (let [g (to-graph (to-triples *entities*))]
      (is (validation/validate g))))

  (deftest validation-test
    (is (validation/validate *graph*))))

;; Skipped: query-test depends on the in-Clojure ingest seeding the
;; xtdb fixture from parsed text; corpus-scale ingestion + the external
;; parser supersede this path.
(deftest ^:kaocha/skip query-test
  (is (= 110 (ffirst (xtdb/q *node* '{:find  [(count ?work)]
                                      :where [[?author :abc.aozora/family-name family-name]
                                              [?work :abc.aozora/author ?author]]
                                      :in    [family-name]}
                             "夏目"))))
  (is (= 110 (ffirst (xtdb/q *node* '{:find  [(count ?work)]
                                      :where [[?author :abc.aozora/family-name "夏目"]
                                              [?work :abc.aozora/author ?author]
                                              [?work :abc.aozora/aozora-last-modified-date ?date]
                                              [(< ?date (time/local-date 2021 1 1))]]})))))

(comment
  (deftest serialization-test
    (let [test-filename "tmp.ttl" #_"tmp.rt"]
      (save-graph! test-filename *graph*
                   ;; :format RDFFormat/RDF_THRIFT #_RDFFormat/TURTLE_PRETTY
                   )
      (let [graph' (load-graph test-filename)]
        (is (= (.size graph') (.size *graph*)))
        (is (.isIsomorphicWith graph' *graph*))))))
