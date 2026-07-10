(ns abc.tools.diagram.adr-graph-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [abc.tools.diagram.adr-graph :as adr]))

(deftest parses-header-fields
  (let [by-num (into {} (map (juxt :num identity)) (adr/parse-all "docs/adr"))]
    (is (= "Accepted" (:status (by-num 1))))
    (is (= [10 23 27] (sort (:amended-by (by-num 1)))))
    (is (= [7 12 23 24] (sort (:depends-on (by-num 25)))))
    (is (= [] (:supersedes (by-num 1))))))

(deftest header-lint-clean-on-current-set
  (is (= [] (adr/lint*))))

(deftest malformed-reference-width-is-linted-even-when-the-adr-exists
  (let [dir (.toFile (java.nio.file.Files/createTempDirectory
                      "abc-adr-graph-test"
                      (make-array java.nio.file.attribute.FileAttribute 0)))
        file (io/file dir "0001-test.md")]
    (try
      (spit file (str "# ADR 0001: Test\n\n"
                      "Status: Accepted\n"
                      "Depends on: ADR 6, ADR 60, ADR 0006, ADR 12345678901234567890\n"))
      (let [parsed (adr/parse-adr (.getPath dir) (.getName file))
            existing {:num 6 :title "Existing" :status "Accepted"
                      :supersedes [] :amends [] :amended-by [] :depends-on []}
            problems (adr/lint-adrs [parsed existing] [])]
        ;; Only the syntactically valid token becomes a graph reference.
        (is (= [6] (:depends-on parsed)))
        (is (= #{{:field "Depends on" :token "6"}
                 {:field "Depends on" :token "60"}
                 {:field "Depends on" :token "12345678901234567890"}}
               (set (:malformed-refs parsed))))
        ;; ADR 6 is malformed even though ADR 0006 can exist in a real set.
        (is (some #(str/includes? % "ADR 6 must use exactly four digits") problems))
        (is (some #(str/includes? % "ADR 60 must use exactly four digits") problems))
        (is (some #(str/includes? % "12345678901234567890") problems)))
      (finally
        (java.nio.file.Files/deleteIfExists (.toPath file))
        (java.nio.file.Files/deleteIfExists (.toPath dir))))))

(deftest lint-catches-missing-reciprocal
  (let [adrs [{:num 1 :title "A" :status "Accepted" :supersedes [] :amends [] :amended-by [] :depends-on []}
              {:num 2 :title "B" :status "Accepted" :supersedes [] :amends [1] :amended-by [] :depends-on []}]]
    (is (some #(str/includes? % "0001") (adr/lint-adrs adrs [])))))

(deftest lint-catches-dangling-reference
  (let [adrs [{:num 1 :title "A" :status "Accepted" :supersedes [] :amends [] :amended-by [] :depends-on [99]}]]
    (is (some #(str/includes? % "0099") (adr/lint-adrs adrs [])))))

(deftest graph-from-builds-header-edges
  (let [adrs [{:num 1 :title "A" :status "Accepted" :supersedes [] :amends [] :amended-by [2] :depends-on []}
              {:num 2 :title "B" :status "Accepted" :supersedes [] :amends [1] :amended-by [] :depends-on []}]
        g (adr/graph-from adrs [])]
    (is (= "LR" (:direction g)))
    (is (some #(and (= "ADR0002" (:from %)) (= "ADR0001" (:to %)) (= "amends" (:label %)))
              (:edges g)))))

(deftest build-is-deterministic-flowchart-input
  (let [g (adr/build)]
    (is (= "LR" (:direction g)))
    (is (= (adr/build) g))
    (is (seq (:nodes g)))))

(deftest sidecar-rejects-header-owned-type
  (let [adrs [{:num 1 :title "A" :status "Accepted" :supersedes [] :amends [] :amended-by [] :depends-on []}
              {:num 2 :title "B" :status "Accepted" :supersedes [] :amends [] :amended-by [] :depends-on []}]
        rels [{:from 2 :to 1 :type :amends}]]
    (is (some #(str/includes? % "header-owned") (adr/lint-adrs adrs rels)))))

(deftest sidecar-rejects-dangling-and-unknown
  (let [adrs [{:num 1 :title "A" :status "Accepted" :supersedes [] :amends [] :amended-by [] :depends-on []}]]
    (is (some #(str/includes? % "0099") (adr/lint-adrs adrs [{:from 1 :to 99 :type :extends}])))
    (is (some #(str/includes? % "unknown relation") (adr/lint-adrs adrs [{:from 1 :to 1 :type :bogus}])))))

(deftest sidecar-note-must-be-a-string
  (let [adrs [{:num 1 :title "A" :status "Accepted" :supersedes []
               :amends [] :amended-by [] :depends-on []}]
        relations [{:from 1 :to 1 :type :extends :note 42}]]
    (is (some #(str/includes? % ":note must be a string")
              (adr/lint-adrs adrs relations)))))

(deftest sidecar-clean-and-committed-file-valid
  (is (= [] (adr/lint*)))                          ;; real edn passes all rules
  (let [g (adr/build)]                             ;; semantic edge present
    (is (some #(= "restates hard rule" (:label %)) (:edges g)))))
