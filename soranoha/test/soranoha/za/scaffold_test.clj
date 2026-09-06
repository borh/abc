(ns soranoha.za.scaffold-test
  (:require [clojure.test :refer [deftest is testing]]
            [soranoha.aozora.csv :as csv]
            [soranoha.za.scaffold :as scaffold]))

(def csv-header
  "作品ID,人物ID,役割フラグ,テキストファイルURL")

(defn- rows [& lines]
  (csv/read-rows-from-string
   (apply str csv-header "\n" (map #(str % "\n") lines))))

(defn- candidate [slug rows-for-work]
  {:slug slug :row (first rows-for-work)})

(deftest catalog-contribution-projection
  (let [rs (rows "000100,000001,著者,https://example.org/x.zip"
                 "000100,000002,翻訳者,"
                 "000100,000001,著者,"
                 "000100,000001,校訂者,"
                 "000300,000003,著者,https://example.org/y.zip")]
    (is (= {"s-100" ["author:000001" "reviser:000001" "translator:000002"]
            "s-300" ["author:000003"]}
           (scaffold/projection rs [(candidate "s-300" [(nth rs 4)])
                                    (candidate "s-100" [(first rs)])])))))

(deftest fail-closed-inputs
  (testing "an unknown 役割フラグ fails the projection"
    (let [rs (rows "000100,000001,監修,https://example.org/x.zip")]
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"unknown-role-flag"
                            (scaffold/projection rs [(candidate "s" [(first rs)])])))))
  (testing "a malformed person id fails the projection"
    (let [rs (rows "000100,1257,著者,https://example.org/x.zip")]
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"malformed-person-id"
                            (scaffold/projection rs [(candidate "s" [(first rs)])])))))
  (testing "a ragged row inside a selected work's row set fails the projection"
    (let [rs (csv/read-rows-from-string
              (str csv-header "\n"
                   "000100,000001,著者,https://example.org/x.zip\n"
                   "000100,000002,著者\n"))]
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"ragged-contributor-row"
                            (scaffold/projection rs [(candidate "s" [(first rs)])])))))
  (testing "a ragged row for an unselected work does not block the projection"
    (let [rs (csv/read-rows-from-string
              (str csv-header "\n"
                   "000100,000001,著者,https://example.org/x.zip\n"
                   "000999,000002,著者\n"))]
      (is (= 1 (count (scaffold/projection rs [(candidate "s" [(first rs)])])))))))

(deftest projection-drift-detection
  (let [checkout-rows (rows "000100,000001,著者,https://example.org/cards/000001/files/100_ruby_200.zip"
                            "000100,000002,翻訳者,"
                            "000300,000003,著者,https://example.org/cards/000003/files/300_ruby_400.zip")
        selection [(candidate "s-100" [(first checkout-rows)])
                   (candidate "s-300" [(nth checkout-rows 2)])]
        checkout (scaffold/projection checkout-rows selection)]
    (testing "a snapshot projection from the same checkout shows no drift"
      (is (nil? (scaffold/projection-drift
                 checkout
                 (scaffold/snapshot-projection
                  {"candidates"
                   [{"slug" "s-100" "contributions"
                     [{"contribution_id" "author:000001"}
                      {"contribution_id" "translator:000002"}]}
                    {"slug" "s-300" "contributions"
                     [{"contribution_id" "author:000003"}]}]})))))
    (testing "a contributor added under surviving slugs is drift"
      (let [stale (assoc checkout "s-100" ["author:000001"])
            drift (scaffold/projection-drift checkout stale)]
        (is (= 0 (:only-in-checkout-count drift)))
        (is (= 0 (:only-in-snapshot-count drift)))
        (is (= 1 (:contributions-differ-count drift)))
        (is (= ["s-100"] (:contributions-differ-sample drift)))))
    (testing "a changed role under a surviving slug is drift"
      (is (= 1 (:contributions-differ-count
                (scaffold/projection-drift
                 checkout (assoc checkout "s-100" ["author:000001" "editor:000002"]))))))
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
