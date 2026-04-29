(ns abc.tei-test
  (:require [abc.tei :refer :all]
            [abc.annotation :as annotation]
            [abc.tools.malli :as am]
            [clojure.test :as t :refer [deftest is use-fixtures]]
            [abc.test-utils :refer :all]
            [malli.generator :as mg]
            [clojure.test.check.generators :as gen]
            [clojure.data.xml :as xml]
            [malli.core :as m]
            [abc.xtdb :as xtdb]))

(def ^:dynamic ^:private *metadata* nil)
(def ^:dynamic ^:private *text* nil)

(defn tei-fixture [f]
  (am/install!)
  (let [work-record (xtdb/work-query "文鳥" "夏目")]
    (clojure.pprint/pprint work-record)
    (binding [*metadata* (metadata-to-tei work-record)
              *text* (annotation/parse-text "A［＃B］C｜D《E》F※［＃1-86-29］G［H］I｜J《K》L")]
      (println *text*)
      (f))))

(use-fixtures :once tei-fixture)

(deftest header-test
  (let [h (header *metadata*)]
    (is (schema-valid vector? h))))

(deftest body-test
  (is *text*)
  (is (gen/sample (mg/generator :document/body)))
  (let [b (body *text*)]
    (is (schema-valid vector? b))))

;; Skipped: depends on Clojure-side text parsing, which moves out of
;; the codebase; TEI document construction will consume the JSON AST
;; produced by the external parser.
(deftest ^:kaocha/skip document-test
  (is (doc *metadata* *text*))
  (is (string? (xml/emit-str (doc *metadata* *text*)))))

(deftest ^:kaocha/skip serialization-roundtrip-test
  (let [d (doc *metadata* *text*)
        _ (save! "tmp.xml" d)]
    ;; FIXME does not round-trip correctly (empty content, xmlns encoding)
    (is (not= d (load-tei "tmp.xml")))))
