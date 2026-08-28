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
