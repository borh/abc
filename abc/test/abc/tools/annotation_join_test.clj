(ns abc.tools.annotation-join-test
  (:require [abc.tools.annotation-join :as join]
            [clojure.test :refer [deftest is testing]]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]))

(defn- tok [i s e t] {"token_index" i "input_span" {"start" s "end" e} "text" t})
(defn- ruby-ann [s e base reading]
  {"annotation_kind" "ruby" "span" {"start" s "end" e}
   "ruby" {"base" base "reading" reading "direction" nil}})

(defn- exhaustive-join [tokens annotations]
  (mapv (fn [ann]
          (let [{:strs [start end]} (get ann "span")
                cover (if (>= start end)
                        []
                        (filterv (fn [token]
                                   (let [{token-start "start" token-end "end"}
                                         (get token "input_span")]
                                     (and (< token-start end)
                                          (< start token-end))))
                                 tokens))
                first-span (get (first cover) "input_span")
                last-span (get (last cover) "input_span")
                start-aligned? (= start (get first-span "start"))
                end-aligned? (= end (get last-span "end"))]
            {"annotation" ann
             "token_indexes" (mapv #(get % "token_index") cover)
             "classification"
             (cond
               (empty? cover) "conflict"
               (and start-aligned? end-aligned? (= 1 (count cover))) "aligned-single"
               (and start-aligned? end-aligned?) "aligned-multi"
               (and start-aligned? (not end-aligned?)) "stem-prefix"
               :else "conflict")}))
        annotations))

(def ^:private span-parts-gen
  (gen/vector (gen/tuple (gen/choose 0 5) (gen/choose 1 8)) 0 40))

(def ^:private annotation-bounds-gen
  (gen/vector (gen/tuple (gen/choose 0 300) (gen/choose 0 300)) 0 50))

(defn- parts->tokens [parts]
  (:tokens
   (reduce (fn [{:keys [cursor tokens]} [gap width]]
             (let [start (+ cursor gap)
                   end (+ start width)
                   index (count tokens)]
               {:cursor end
                :tokens (conj tokens (tok index start end (str "t" index)))}))
           {:cursor 0 :tokens []}
           parts)))

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
      (is (= "conflict" (get (first result) "classification")))))
  (testing "inverted spans (start > end) are degenerate too: same no-coverage,
            conflict outcome as zero-width, guarded by (>= start end)"
    (let [tokens [(tok 0 0 2 "吾輩") (tok 1 2 3 "が")]
          result (join/join tokens [(ruby-ann 2 1 "" "")])]
      (is (= [] (get (first result) "token_indexes")))
      (is (= "conflict" (get (first result) "classification"))))))

(deftest join-validates-token-precondition-test
  (testing "list input is materialized and accepted"
    (is (= [0]
           (get (first (join/join (list (tok 0 0 1 "a"))
                                  [(ruby-ann 0 1 "a" "a")]))
                "token_indexes"))))
  (testing "invalid token spans fail before joining"
    (doseq [[label tokens]
            [["missing span" [{"token_index" 0}]]
             ["zero width" [(tok 0 1 1 "")]]
             ["overlap" [(tok 0 0 2 "ab") (tok 1 1 3 "bc")]]
             ["descending" [(tok 0 2 3 "c") (tok 1 0 1 "a")]]]]
      (testing label
        (is (thrown-with-msg? clojure.lang.ExceptionInfo
                              #"Invalid token span"
                              (join/join tokens [])))))))

(deftest join-overlap-boundaries-test
  (let [tokens [(tok 0 2 4 "ab") (tok 1 6 8 "cd")]
        indexes (fn [annotations]
                  (mapv #(get % "token_indexes")
                        (join/join tokens annotations)))]
    (is (= [[] [] [] [] [0] [1] [0 1]]
           (indexes [(ruby-ann 0 2 "" "")
                     (ruby-ann 0 1 "" "")
                     (ruby-ann 4 6 "" "")
                     (ruby-ann 8 9 "" "")
                     (ruby-ann 3 4 "" "")
                     (ruby-ann 6 7 "" "")
                     (ruby-ann 0 10 "" "")]))))
  (is (= [] (get (first (join/join [] [(ruby-ann 0 1 "" "")]))
                 "token_indexes"))))

(deftest optimized-join-equals-exhaustive-property-test
  (let [result (tc/quick-check
                300
                (prop/for-all [parts span-parts-gen
                               bounds annotation-bounds-gen]
                              (let [tokens (parts->tokens parts)
                                    annotations (mapv (fn [[start end]]
                                                        (ruby-ann start end "base" "reading"))
                                                      bounds)]
                                (= (exhaustive-join tokens annotations)
                                   (join/join tokens annotations)))))]
    (is (:pass? result) (pr-str result))))
