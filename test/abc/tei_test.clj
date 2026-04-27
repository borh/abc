(ns abc.tei-test
  (:require [abc.tei :refer :all]
            [abc.annotation :as annotation]
            [abc.annotation.schema :as annotation-schema]
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
  (let [work-record (xtdb/work-query "文鳥" "夏目")]
    (clojure.pprint/pprint work-record)
    (binding [*metadata* (metadata-to-tei work-record) #_(mg/generate [:schema {:registry registry} :tei/header])
              *text* (annotation/parse-text "A［＃B］C｜D《E》F※［＃1-86-29］G［H］I｜J《K》L") #_(mg/generate [:schema {:registry annotation-schema/registry} :document/body])]
      (println *text*)
      (f))))

(use-fixtures :once tei-fixture)

(deftest header-test
  #_(is *metadata*)
  #_(is (gen/sample (mg/generator [:schema {:registry registry} :tei/header])))
  (let [h (header *metadata*)]
    (is (schema-valid
          vector?
          #_[:vector [:teiHeader :vector] [:profileDesc :vector] [:revisionDesc :vector] [:classDecl :vector]]
          h
          (merge registry
                 annotation-schema/registry)))))

(deftest body-test
  (is *text*)
  (is (gen/sample (mg/generator [:schema {:registry annotation-schema/registry} :document/body])))
  (let [b (body *text*)]
    (is (schema-valid vector? b (merge registry
                                       annotation-schema/registry)))))

(deftest document-test
  (is (doc *metadata* *text*))
  (is (string? (xml/emit-str (doc *metadata* *text*)))))

(deftest serialization-roundtrip-test
  (let [d (doc *metadata* *text*)
        _ (save! "tmp.xml" d)]
    ;; FIXME does not round-trip correctly (empty content, xmlns encoding)
    (is (not= d (load-tei "tmp.xml")))))
