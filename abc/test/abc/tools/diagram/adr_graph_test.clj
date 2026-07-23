(ns abc.tools.diagram.adr-graph-test
  (:require [abc.tools.diagram.adr-graph :as adr]
            [abc.test-fs :refer [with-temp-dir]]
            [babashka.fs :as fs]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]))

(deftest missing-relations-file-is-empty-test
  (with-temp-dir [dir]
    (with-redefs [adr/relations-path (str (fs/file dir "missing.edn"))]
      (is (= [] (adr/load-relations))))))

(deftest header-lint-clean-on-current-set
  (is (= [] (adr/lint*))))

(deftest graph-from-builds-header-edges
  (let [adrs [{:num 1 :title "A" :status "Accepted" :relations {}}
              {:num 2 :title "B" :status "Accepted"
               :relations {:amends [{:target 1 :scope "governance source"}]}}]
        g (adr/graph-from adrs [])]
    (is (= "LR" (:direction g)))
    (is (some #(and (= "ADR0002" (:from %))
                    (= "ADR0001" (:to %))
                    (= "amends — governance source" (:label %)))
              (:edges g)))))

(deftest build-is-deterministic-flowchart-input
  (let [g (adr/build)]
    (is (= "LR" (:direction g)))
    (is (= (adr/build) g))
    (is (seq (:nodes g)))))

(deftest sidecar-rejects-header-owned-type
  (let [adrs [{:num 1 :title "A" :status "Accepted" :relations {}}
              {:num 2 :title "B" :status "Accepted" :relations {}}]
        rels [{:from 2 :to 1 :type :amends}]]
    (is (some #(str/includes? % "header-owned") (adr/lint-adrs adrs rels)))))

(deftest sidecar-rejects-dangling-and-unknown
  (let [adrs [{:num 1 :title "A" :status "Accepted" :relations {}}]]
    (is (some #(str/includes? % "0099") (adr/lint-adrs adrs [{:from 1 :to 99 :type :extends}])))
    (is (some #(str/includes? % "unknown relation") (adr/lint-adrs adrs [{:from 1 :to 1 :type :bogus}])))))

(deftest sidecar-rejects-malformed-endpoints
  (let [adrs [{:num 1 :title "A" :status "Accepted" :relations {}}
              {:num 2 :title "B" :status "Accepted" :relations {}}]]
    (is (some #(str/includes? % "malformed")
              (adr/lint-adrs adrs [{:from "bad" :to 2 :type :extends}])))))

(deftest sidecar-note-must-be-a-string
  (let [adrs [{:num 1 :title "A" :status "Accepted" :relations {}}]
        relations [{:from 1 :to 1 :type :extends :note 42}]]
    (is (some #(str/includes? % ":note must be a string")
              (adr/lint-adrs adrs relations)))))

(deftest sidecar-clean-and-committed-file-valid
  (is (= [] (adr/lint*)))                          ;; real edn passes all rules
  (let [g (adr/build)]                             ;; semantic edge present
    (is (some #(= "restates hard rule" (:label %)) (:edges g)))
    (is (some #(= "depends on — source_span_coverage gate" (:label %))
              (:edges g)))))
