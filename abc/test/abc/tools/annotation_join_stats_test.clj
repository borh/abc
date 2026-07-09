(ns abc.tools.annotation-join-stats-test
  (:require [abc.tools.annotation-join-stats :as stats]
            [abc.tools.files :as files]
            [abc.tools.parser-ir-plaintext :as plaintext]
            [clojure.test :refer [deftest is testing]]))

(deftest reconstruct-token-spans-walks-scalars-and-skips-whitespace-test
  (let [{:keys [tokens failure]} (stats/reconstruct-token-spans
                                  "吾輩は\n猫である"
                                  ["吾輩" "は" "猫" "で" "ある"])]
    (is (nil? failure))
    (is (= [{"token_index" 0 "input_span" {"start" 0 "end" 2} "text" "吾輩"}
            {"token_index" 1 "input_span" {"start" 2 "end" 3} "text" "は"}
            {"token_index" 2 "input_span" {"start" 4 "end" 5} "text" "猫"}
            {"token_index" 3 "input_span" {"start" 5 "end" 6} "text" "で"}
            {"token_index" 4 "input_span" {"start" 6 "end" 8} "text" "ある"}]
           tokens))))

(deftest reconstruct-token-spans-counts-astral-scalars-once-test
  (let [{:keys [tokens failure]} (stats/reconstruct-token-spans
                                  "𠮟る" ["𠮟る"])]
    (is (nil? failure))
    (is (= {"start" 0 "end" 2} (get-in tokens [0 "input_span"])))))

(deftest reconstruct-token-spans-records-failure-test
  (let [{:keys [tokens failure]} (stats/reconstruct-token-spans
                                  "吾輩は猫" ["吾輩" "犬"])]
    (is (nil? tokens))
    (is (= 1 (:token-index failure)))
    (is (= 2 (:offset failure)))))

(deftest work-stats-classifies-fixture-annotations-test
  (let [parser-ir (files/read-json "examples/ab-validator-output/parser-ir.json")
        {:keys [text annotations]} (plaintext/render-with-annotations parser-ir)
        ;; one token exactly covering the ruby base = aligned-single; the
        ;; rest of the text as whatever tokens the walk yields
        ruby-span (get-in (first (filter #(= "ruby" (get % "annotation_kind"))
                                         annotations))
                          ["span"])
        result (stats/work-stats
                {:annotations annotations
                 :tokens [{"token_index" 0
                           "input_span" ruby-span
                           "text" "x"}]})]
    (is (pos? (get-in result [:annotation_counts "ruby"])))
    (is (= 1 (get-in result [:classifications "ruby" "aligned-single"])))
    (testing "every annotation is classified"
      (is (= (reduce + (vals (:annotation_counts result)))
             (reduce + (mapcat vals (vals (:classifications result)))))))))

(deftest aggregate-sums-and-rates-test
  (let [agg (stats/aggregate
             [{:work-id "a"
               :annotation_counts {"ruby" 2 "gaiji" 1}
               :classifications {"ruby" {"aligned-single" 1 "stem-prefix" 1}
                                 "gaiji" {"conflict" 1}}}
              {:work-id "b"
               :annotation_counts {"ruby" 1}
               :classifications {"ruby" {"aligned-multi" 1}}}])]
    (is (= 2 (:work_count agg)))
    (is (= {"ruby" 3 "gaiji" 1} (:annotation_counts agg)))
    (is (= 1 (get-in agg [:classifications "ruby" "aligned-single"])))
    (is (= 1 (get-in agg [:classifications "ruby" "aligned-multi"])))
    (is (= 0.5 (get-in agg [:classification_rates "ruby" "stem-prefix"])))))
