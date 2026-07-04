(ns abc.load-test
  (:require [clojure.test :as t :refer [deftest testing is use-fixtures]]
            [abc.test-utils :refer :all]
            [abc.tools.malli :as am]
            [abc.load :as load :refer :all]
            [abc.config :as config]
            [abc.xtdb :as xtdb]))

(def ^:dynamic ^:private *db* nil)

(defn db-fixture [f]
  (let [db (aozora-bunko-db config/aozora-bunko-path)]
    (binding [*db* db]
      (f))))

(use-fixtures :once
  (fn [f] (am/install!) (f))
  db-fixture)

(deftest load-test
  (testing "Loading AB database fixture"
    (is *db*)
    (is (= (set (keys *db*)) #{:works :persons})))
  (testing "Database schema"
    ;; A full database validation is slow
    (is (schema-valid :abc.aozora/db-entries *db*))))

(deftest persist-texts-uses-supplied-node-for-reads-test
  (testing "persist-texts! keeps read and write paths on the explicit node"
    (let [submitted (atom nil)]
      (reset! load/!times [])
      (with-redefs [xtdb/node (fn []
                                (throw (ex-info "default XTDB node should not be used" {})))
                    xtdb/all-works (fn [& {:keys [node]}]
                                     (is (= ::explicit-node node))
                                     #{:abc.aozora/w043661})
                    load/work-id->document (fn [node id]
                                             (is (= ::explicit-node node))
                                             (is (= :abc.aozora/w043661 id))
                                             {:document/metadata {:abc.aozora/work-id id}})
                    xtdb/submit-tx (fn [node tx]
                                     (reset! submitted {:node node :tx (doall tx)}))]
        (load/persist-texts! ::explicit-node)
        (is (= ::explicit-node (:node @submitted)))
        (is (= [[:xtdb.api/put {:document/metadata {:abc.aozora/work-id :abc.aozora/w043661}
                                 :xt/id :abc.aozora/d043661}]]
               (:tx @submitted)))))))

;; Skipped: Aozora text parsing moves out of Clojure; consumed as JSON AST
;; from an external parser per the parser-IR contract.
(deftest ^:kaocha/skip extract-texts-test
  (testing "Extracting texts from database fixture"
    ;; Good testcase:
    ;; :abc.aozora/w043688
    #_(is (schema-validate :document/body :TODO))
    #_(doseq [doc (take 10 (extract-texts *db*))]
        (is (schema-valid :document/body doc)
            #_(nil? (dorun (extract-texts *db*)))))))
