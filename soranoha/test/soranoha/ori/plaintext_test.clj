(ns soranoha.ori.plaintext-test
  (:require [clojure.test :refer [deftest is]]
            [soranoha.ported.parser-ir-plaintext :as plaintext]))

(deftest initial-heading-does-not-invent-a-blank-line-or-shift-ruby
  (let [result (plaintext/render-with-annotations
                {"nodes" [{"type" "heading" "text" "一"}
                          {"type" "ruby" "ruby" {"base" "池" "reading" "いけ"}}]})]
    (is (= "一\n池" (:text result)))
    (is (= {"start" 2 "end" 3} (get-in result [:annotations 0 "span"]))))
  (is (= "本文\n二\n"
         (:text (plaintext/render {"nodes" [{"type" "text" "text" "本文"}
                                            {"type" "heading" "text" "二"}]})))))

(deftest paragraph-boundaries-separate-layout-text-without-doubling-source-breaks
  (doseq [ending ["末行" "末行\n"]
          beginning ["次行" "\n次行"]]
    (let [result (plaintext/render-with-annotations
                  {"nodes" [{"type" "text" "text" ending}
                            {"type" "text" "text" ""}
                            {"type" "text" "text" beginning}
                            {"type" "ruby" "ruby" {"base" "月" "reading" "つき"}}]
                   "paragraphs" [{"role" "body" "layout" {"kind" "jisage" "indent" 2}
                                  "node_range" {"start" 0 "end" 1}}
                                 {"role" "body" "node_range" {"start" 1 "end" 4}}]})]
      (is (= "末行\n次行月" (:text result)))
      (is (= {"start" 5 "end" 6} (get-in result [:annotations 0 "span"]))))))
