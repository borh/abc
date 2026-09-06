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
