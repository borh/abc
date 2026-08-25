(ns soranoha.snh.semantic-test
  "The semantic boundary rules: real-calendar effective dates and
  absolute upstream origins — enforced in code, never via JSON Schema
  `format`. The valid fixture vectors must pass both checks."
  (:require [charred.api :as json]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [soranoha.snh.decode :as decode]
            [soranoha.snh.semantic :as semantic]))

(defn- vector-value [file type]
  (with-open [in (io/input-stream (io/resource (str "snh/vectors/" file)))]
    (:value (decode/decode type (.readAllBytes in)))))

(deftest real-calendar-date-rule
  (doseq [[date expect] [["2026-08-01" true]
                         ["2028-02-29" true]   ;; leap day, leap year
                         ["2026-99-99" false]  ;; schema pattern admits this
                         ["2027-02-29" false]  ;; non-leap year
                         ["2026-00-10" false]
                         ["2026-13-01" false]
                         ["2026-04-31" false]
                         ["2026-8-1" false]    ;; not zero-padded
                         [nil false]]]
    (testing (str date)
      (is (= expect (semantic/real-calendar-date? date))))))

(deftest absolute-origin-rule
  (doseq [[origin expect] [["https://github.com/aozorabunko/aozorabunko.git" true]
                           ["ssh://git@forgejo.example/soranoha/pub.git" true]
                           ["aozorabunko" false]           ;; not absolute
                           ["file:///srv/aozorabunko" false] ;; no host
                           ["https://" false]
                           ["git@github.com:aozorabunko/aozorabunko.git" false] ;; scp form is not a URI
                           ["" false]
                           [nil false]]]
    (testing (str origin)
      (is (= expect (semantic/absolute-origin? origin))))))

(deftest valid-fixture-vectors-satisfy-the-semantic-rules
  ;; decode already applies these on every accept vector; this pins the
  ;; helpers directly. Rejection coverage lives in the full-boundary reject
  ;; vectors (relative origin, impossible date) exercised by the conformance
  ;; suite.
  (let [manifest (vector-value "release-manifest-valid.json" "release-manifest")
        snapshot (vector-value "assessment-snapshot-valid.json" "assessment-snapshot")]
    (is (= manifest (semantic/check-manifest-origin! manifest)))
    (is (= snapshot (semantic/check-snapshot-dates! snapshot)))))
