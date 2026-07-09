(ns abc.tools.annotation-join-test
  (:require [abc.tools.annotation-join :as join]
            [clojure.test :refer [deftest is testing]]))

(defn- tok [i s e t] {"token_index" i "input_span" {"start" s "end" e} "text" t})
(defn- ruby-ann [s e base reading]
  {"annotation_kind" "ruby" "span" {"start" s "end" e}
   "ruby" {"base" base "reading" reading "direction" nil}})

(deftest join-classifications-test
  (let [;; text: 吾輩が手綱で行く道
        tokens [(tok 0 0 2 "吾輩") (tok 1 2 3 "が")
                (tok 2 3 4 "手") (tok 3 4 5 "綱")
                (tok 4 5 6 "で") (tok 5 6 8 "行く") (tok 6 8 9 "道")]
        annotations [(ruby-ann 0 2 "吾輩" "わがはい")   ; = token 0 exactly
                     (ruby-ann 3 5 "手綱" "たづな")     ; = tokens 2+3
                     (ruby-ann 6 7 "行" "い")           ; stem: token 5 crosses end
                     (ruby-ann 7 9 "く道" "くみち")]    ; crosses token 5 start: conflict
        result (join/join tokens annotations)
        by-base (fn [b] (first (filter #(= b (get-in % ["annotation" "ruby" "base"])) result)))]
    (is (= "aligned-single" (get (by-base "吾輩") "classification")))
    (is (= [0] (get (by-base "吾輩") "token_indexes")))
    (is (= "aligned-multi" (get (by-base "手綱") "classification")))
    (is (= [2 3] (get (by-base "手綱") "token_indexes")))
    (is (= "stem-prefix" (get (by-base "行") "classification")))
    (is (= [5] (get (by-base "行") "token_indexes")))
    (is (= "conflict" (get (by-base "く道") "classification")))))

(deftest join-zero-width-span-test
  (testing "zero-width spans (D3 allows empty gaiji spans) cover no text:
            no spurious token_indexes from point-containment, classified
            conflict under the deliberate four-class contract"
    (let [tokens [(tok 0 0 2 "吾輩") (tok 1 2 3 "が")]
          result (join/join tokens [(ruby-ann 1 1 "" "")])]
      (is (= [] (get (first result) "token_indexes")))
      (is (= "conflict" (get (first result) "classification"))))))
