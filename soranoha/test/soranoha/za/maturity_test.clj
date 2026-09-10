(ns soranoha.za.maturity-test
  (:require [clojure.test :refer [deftest is testing]]
            [soranoha.za.maturity :as maturity]))

(deftest label-resolves-a-known-key
  (let [early (maturity/label! "early")]
    (is (= "初期リリース" (:ja early)))
    (is (= "early release" (:en early)))
    (testing "both languages, because the site renders both and a label
              present in one only would serve a corpus whose maturity a
              Japanese or an English reader could not read"
      (is (every? (fn [[_ label]]
                    (every? #(and (string? (get label %)) (seq (get label %)))
                            [:ja :en :note-ja :note-en]))
                  maturity/labels)))))

(deftest no-label-configured-is-not-an-error
  (testing "a deployment may deliberately say nothing about maturity"
    (is (nil? (maturity/label! nil)))
    (is (nil? (maturity/label! "")))
    (is (nil? (maturity/label! "   ")))))

(deftest an-unknown-label-stops-the-export
  (let [thrown (try (maturity/label! "beta") nil
                    (catch clojure.lang.ExceptionInfo e (ex-data e)))]
    (is (= :unknown-maturity-label (:reason thrown)))
    (testing "the error names what this build can render, because the
              operator's next action is to pick one of them"
      (is (= ["early" "stable"] (:known thrown))))))

(deftest release-name-is-checked-for-shape
  (is (= "v0.1" (maturity/release-name! " v0.1 ")))
  (is (= "v1" (maturity/release-name! "v1")))
  (is (= "v1.2.3" (maturity/release-name! "v1.2.3")))
  (is (nil? (maturity/release-name! nil)))
  (is (nil? (maturity/release-name! "  ")))
  (doseq [bad ["0.1" "v0.1-rc1" "early" "v0.1 (early)"]]
    (testing (str "refused: " bad)
      (is (= :invalid-release-name
             (:reason (try (maturity/release-name! bad) nil
                           (catch clojure.lang.ExceptionInfo e (ex-data e)))))))))
