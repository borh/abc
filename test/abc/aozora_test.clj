(ns abc.aozora-test
  (:require [abc.aozora :as aozora :refer :all]
            [abc.tools.malli :as am]
            [clojure.test :as t :refer [deftest is use-fixtures]]
            [malli.core :as m]
            [malli.generator :as mg]))

(def ^:dynamic ^:private *example-entity* nil)

(defn fixture [f]
  (am/install!)
  (binding [*example-entity* (mg/generate :abc.aozora/entity-map)]
    (f)))

(use-fixtures :once fixture)

(deftest entity-test
  (is *example-entity*))

(deftest csv-cell-transformer-decodes-leaf-types
  (t/testing "date string → LocalDate via simple-date regex"
    (is (instance? java.time.LocalDate
                   (m/decode :abc.aozora/last-modified-date "2024-12-01"
                             aozora/csv-cell-transformer))))
  (t/testing "wareki → LocalDate"
    (is (instance? java.time.LocalDate
                   (m/decode :abc.aozora/first-published "1922（大正11）年7月"
                             aozora/csv-cell-transformer))))
  (t/testing "あり/なし → boolean"
    (is (true? (m/decode :abc.aozora/copyright-expired "あり"
                         aozora/csv-cell-transformer)))
    (is (false? (m/decode :abc.aozora/copyright-expired "なし"
                          aozora/csv-cell-transformer))))
  (t/testing "encoding string → canonical token"
    (is (= "SJIS" (m/decode :abc.aozora/encoding "ShiftJIS"
                            aozora/csv-cell-transformer))))
  (t/testing "NDC string → set of category maps"
    (let [out (m/decode :abc.aozora/NDC "NDC 913"
                        aozora/csv-cell-transformer)]
      (is (set? out))
      (is (= 1 (count out)))
      (is (contains? (first out) :abc.aozora.ndc/category)))))
