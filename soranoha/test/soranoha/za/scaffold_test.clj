(ns soranoha.za.scaffold-test
  (:require [clojure.test :refer [deftest is testing]]
            [soranoha.snh.decode :as decode]
            [soranoha.yomi.catalog :as catalog]
            [soranoha.za.assemble :as assemble]
            [soranoha.za.scaffold :as scaffold]))

(def csv-header
  "作品ID,人物ID,役割フラグ,テキストファイルURL")

(defn- rows [& lines]
  (catalog/read-rows-from-string
   (apply str csv-header "\n" (map #(str % "\n") lines))))

(defn- candidate [slug rows-for-work]
  {:slug slug :row (first rows-for-work)})

(deftest total-not-evaluated-snapshot
  (let [;; work 000100 has an author row (the joined one) plus a
        ;; translator row with no text URL — the contributor-only shape
        rs (rows "000100,000001,著者,https://example.org/cards/000001/files/100_ruby_200.zip"
                 "000100,000002,翻訳者,"
                 "000300,000003,著者,https://example.org/cards/000003/files/300_ruby_400.zip")
        enc (scaffold/snapshot
             rs
             [(candidate "000300_000003_000003_300_ruby_400" [(nth rs 2)])
              (candidate "000100_000001_000001_100_ruby_200" [(first rs)])])
        value (:value (decode/decode "assessment-snapshot" (:bytes enc)))
        candidates (get value "candidates")]
    (testing "canonical bytes boundary-decode and cover the selection sorted by slug"
      (is (= ["000100_000001_000001_100_ruby_200"
              "000300_000003_000003_300_ruby_400"]
             (mapv #(get % "slug") candidates))))
    (testing "the full catalog row set of the 作品ID becomes the contribution set"
      (is (= ["author:000001" "translator:000002"]
             (mapv #(get % "contribution_id")
                   (get (first candidates) "contributions")))))
    (testing "every fact is the explicit not-evaluated fact"
      (doseq [c candidates
              fact (cons (get c "work_assessment") (get c "contributions"))]
        (is (= "not-evaluated" (get fact "status")))
        (is (nil? (get fact "jurisdiction")))
        (is (nil? (get fact "effective_date")))
        (is (nil? (get fact "basis")))))))

(deftest duplicate-and-multi-role-contributors
  (let [rs (rows "000100,000001,著者,https://example.org/cards/000001/files/100_ruby_200.zip"
                 "000100,000001,著者,"
                 "000100,000001,校訂者,")
        enc (scaffold/snapshot rs [(candidate "s-100" [(first rs)])])
        contributions (get (first (get (:value enc) "candidates"))
                           "contributions")]
    (testing "identical (role, person) rows deduplicate; a second role is a second contribution"
      (is (= ["author:000001" "reviser:000001"]
             (mapv #(get % "contribution_id") contributions))))))

(deftest fail-closed-inputs
  (testing "an unknown 役割フラグ fails the scaffold"
    (let [rs (rows "000100,000001,監修,https://example.org/x.zip")]
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"unknown-role-flag"
                            (scaffold/snapshot rs [(candidate "s" [(first rs)])])))))
  (testing "a malformed person id fails the scaffold"
    (let [rs (rows "000100,1257,著者,https://example.org/x.zip")]
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"malformed-person-id"
                            (scaffold/snapshot rs [(candidate "s" [(first rs)])])))))
  (testing "a ragged row inside a selected work's row set fails the scaffold"
    (let [rs (catalog/read-rows-from-string
              (str csv-header "\n"
                   "000100,000001,著者,https://example.org/x.zip\n"
                   "000100,000002,著者\n"))]
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"ragged-contributor-row"
                            (scaffold/snapshot rs [(candidate "s" [(first rs)])])))))
  (testing "a ragged row for an unselected work does not block the scaffold"
    (let [rs (catalog/read-rows-from-string
              (str csv-header "\n"
                   "000100,000001,著者,https://example.org/x.zip\n"
                   "000999,000002,著者\n"))]
      (is (= 1 (count (get (:value (scaffold/snapshot
                                    rs [(candidate "s" [(first rs)])]))
                           "candidates")))))))

(deftest projection-drift-detection
  (let [checkout-rows (rows "000100,000001,著者,https://example.org/cards/000001/files/100_ruby_200.zip"
                            "000100,000002,翻訳者,"
                            "000300,000003,著者,https://example.org/cards/000003/files/300_ruby_400.zip")
        selection [(candidate "s-100" [(first checkout-rows)])
                   (candidate "s-300" [(nth checkout-rows 2)])]
        checkout (scaffold/projection checkout-rows selection)]
    (testing "a snapshot scaffolded from the same checkout shows no drift"
      (let [enc (scaffold/snapshot checkout-rows selection)]
        (is (nil? (scaffold/projection-drift
                   checkout
                   (scaffold/snapshot-projection (:value enc)))))))
    (testing "a contributor added under surviving slugs is drift"
      ;; exactly the case slug totality cannot see: same slugs, new
      ;; translator row on an existing work
      (let [stale (scaffold/snapshot-projection
                   (:value (scaffold/snapshot
                            (rows "000100,000001,著者,https://example.org/cards/000001/files/100_ruby_200.zip"
                                  "000300,000003,著者,https://example.org/cards/000003/files/300_ruby_400.zip")
                            selection)))
            drift (scaffold/projection-drift checkout stale)]
        (is (some? drift))
        (is (= 0 (:only-in-checkout-count drift)))
        (is (= 0 (:only-in-snapshot-count drift)))
        (is (= 1 (:contributions-differ-count drift)))
        (is (= ["s-100"] (:contributions-differ-sample drift)))))
    (testing "a changed role under a surviving slug is drift"
      (let [stale (scaffold/snapshot-projection
                   (:value (scaffold/snapshot
                            (rows "000100,000001,著者,https://example.org/cards/000001/files/100_ruby_200.zip"
                                  "000100,000002,編者,"
                                  "000300,000003,著者,https://example.org/cards/000003/files/300_ruby_400.zip")
                            selection)))]
        (is (= 1 (:contributions-differ-count
                  (scaffold/projection-drift checkout stale))))))
    (testing "slug-level differences are reported on their own side"
      (let [drift (scaffold/projection-drift checkout (dissoc checkout "s-300"))]
        (is (= 1 (:only-in-checkout-count drift)))
        (is (= ["s-300"] (:only-in-checkout-sample drift)))
        (is (= 0 (:contributions-differ-count drift)))))
    (testing "samples are bounded while counts stay complete"
      (let [wide (into {} (map (fn [i] [(format "w-%04d" i) ["author:000001"]]))
                       (range 50))
            drift (scaffold/projection-drift wide {} 20)]
        (is (= 50 (:only-in-checkout-count drift)))
        (is (= 20 (count (:only-in-checkout-sample drift))))))))

(deftest admission-quarantines-everything
  (let [rs (rows "000100,000001,著者,https://example.org/cards/000001/files/100_ruby_200.zip")
        enc (scaffold/snapshot rs [(candidate "s-100" [(first rs)])])
        partition (#'assemble/partition-candidates
                   assemble/inclusion-rule
                   (get (:value enc) "candidates"))]
    (testing "the inclusion rule quarantines every all-not-evaluated candidate"
      (is (empty? (:admitted partition)))
      (is (empty? (:excluded partition)))
      (is (= [{"slug" "s-100" "reason_code" "not-fully-evaluated"}]
             (:quarantined partition))))))
