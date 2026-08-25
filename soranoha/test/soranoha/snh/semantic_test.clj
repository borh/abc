(ns soranoha.snh.semantic-test
  "The semantic boundary rules: real-calendar effective dates and
  absolute upstream origins — enforced in code, never via JSON Schema
  `format`. The valid fixture vectors must pass both checks."
  (:require [charred.api :as json]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [soranoha.core.canonical :as canonical]
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
  ;; per-type checks directly. Rejection coverage lives in the full-boundary
  ;; reject vectors and the verifier mutation table.
  (let [manifest (vector-value "release-manifest-valid.json" "release-manifest")
        snapshot (vector-value "assessment-snapshot-valid.json" "assessment-snapshot")]
    (is (= manifest (semantic/check-manifest! manifest)))
    (is (= snapshot (semantic/check-snapshot! snapshot)))))

(deftest single-object-ordering-rules-reject-through-decode
  (let [reason-of (fn [type value]
                    (try (decode/decode-string
                          type
                          (String. (canonical/rfc8785-safe-integer-json-bytes-v1 value)
                                   "UTF-8"))
                         :accepted
                         (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))
        manifest (vector-value "release-manifest-valid.json" "release-manifest")
        snapshot (vector-value "assessment-snapshot-valid.json" "assessment-snapshot")
        report (vector-value "admission-report-valid.json" "admission-report")
        event (vector-value "governance-event-withdrawal-valid.json" "governance-event")]
    (testing "manifest works reversed"
      (is (= :works-not-sorted-unique
             (reason-of "release-manifest"
                        (update manifest "works" (comp vec reverse))))))
    (testing "manifest summary count mismatch"
      (is (= :invalid-count-mismatch
             (reason-of "release-manifest"
                        (assoc manifest "validation_summary"
                               {"invalid_count" 1 "invalid_slugs" []})))))
    (testing "manifest summary naming a non-work slug"
      (is (= :invalid-slugs-outside-works
             (reason-of "release-manifest"
                        (assoc manifest "validation_summary"
                               {"invalid_count" 1
                                "invalid_slugs" ["zzz_not_a_work_1_1"]})))))
    (testing "snapshot candidates reversed"
      (is (= :candidates-not-sorted-unique
             (reason-of "assessment-snapshot"
                        (update snapshot "candidates" (comp vec reverse))))))
    (testing "snapshot contributions reversed"
      (is (= :contributions-not-sorted-unique
             (reason-of "assessment-snapshot"
                        (update snapshot "candidates"
                                (fn [cs]
                                  (update (vec cs) 2 update "contributions"
                                          (comp vec reverse))))))))
    (testing "report admitted reversed"
      (is (= :admitted-not-sorted-unique
             (reason-of "admission-report"
                        (update report "admitted" (comp vec reverse))))))
    (testing "report partition overlap"
      ;; a full copy of admitted keeps each list sorted and unique, so only
      ;; the cross-list disjointness rule can reject it
      (is (= :partition-sets-overlap
             (reason-of "admission-report"
                        (assoc report "quarantined"
                               (mapv (fn [slug] {"slug" slug
                                                 "reason_code" "duplicated"})
                                     (get report "admitted")))))))
    (testing "event entries duplicated"
      (is (= :entries-not-sorted-unique
             (reason-of "governance-event"
                        (update event "entries" #(vec (concat % %)))))))))
