(ns soranoha.ori.catalog-text-test
  "The reader the metadata stage hands to the catalog boundary, run against
  the parser the kernel runs. The fields are the ones the upstream catalog
  carries: seventeen occurrences of Aozora Bunko's notation in eleven works,
  one of each kind here."
  (:require [clojure.test :refer [deftest is testing]]
            [soranoha.ori.stages :as stages]))

(def ^:private read-text
  (delay (:read (stages/catalog-text-reader (stages/resolve-adapter)))))

(deftest a-field-carrying-notation-is-read-as-the-text-would-be
  (testing "a character described in words comes from Aozora Bunko's gaiji dictionary"
    (is (= "八ガ岳登山記" (@read-text "八※［＃小書き片仮名ガ］岳登山記")))
    (is (= "勇士ウヲルター（実話）" (@read-text "勇士ウ※［＃小書き片仮名ヲ］ルター（実話）"))))
  (testing "a code point the annotation names is that code point"
    (is (= "失𫝹術講義" (@read-text "失※［＃「人がしら／二／心」、U+2B779］術講義"))))
  (testing "a JIS X 0213 reference is looked up"
    (is (= "輪𢌞と轉生「日本詩人　第二卷第七號」1922（大正11）年7月号"
           (@read-text "輪※［＃「廴＋囘」、第4水準2-12-11］と轉生「日本詩人　第二卷第七號」1922（大正11）年7月号"))))
  (testing "a layout instruction leaves its text and takes nothing else with it"
    (is (= "闇×幻想13＝黎明　幻想・怪奇名作選"
           (@read-text "闇×幻想13［＃「13」は指数］＝黎明　幻想・怪奇名作選")))
    (is (= "開拓地帯（大陸開拓小説集（一））"
           (@read-text "開拓地帯（大陸開拓小説集（一）［＃「（一）」は縦中横］）"))))
  (testing "a character the parser cannot resolve is the placeholder the body text carries"
    (is (= "「第二回岡田\uFFFC滞仏油絵展目録」"
           (@read-text "「第二回岡田※［＃「穀」の「禾」に代えて「釆」］滞仏油絵展目録」")))))

(deftest a-field-without-notation-never-reaches-the-parser
  ;; The parser reads a bare 《》 as a ruby reading with no base and drops
  ;; it, so a subtitle written with those brackets is the case that shows
  ;; the field was not parsed.
  (is (= "《断片》" (@read-text "《断片》")))
  (is (= "札幌｜小樽｜函館" (@read-text "札幌｜小樽｜函館"))))

(deftest a-field-that-reads-as-nothing-is-refused
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"read as nothing"
                        (@read-text "《断片》［＃「（一）」は縦中横］"))))
