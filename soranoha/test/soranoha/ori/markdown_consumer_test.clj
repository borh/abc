(ns soranoha.ori.markdown-consumer-test
  (:require [babashka.process :as process]
            [clojure.string :as string]
            [clojure.test :refer [deftest is]]
            [soranoha.annotations.view :as view]
            [soranoha.ori.projection :as projection]))

(defn- reading [body]
  (view/from-tei (str "<TEI xmlns='http://www.tei-c.org/ns/1.0'><text><body>" body "</body></text></TEI>")))

(deftest commonmark-consumer-retains-source-punctuation-inside-inline-html
  (let [input (reading (str "<p><ruby><rb>*字*</rb><rt>_reading_ &lt;b&gt;</rt></ruby>"
                            "<hi rend='bold'>[bold](url)</hi><hi rend='italic'>`code`</hi>"
                            " &lt;script&gt;alert(1)&lt;/script&gt;<lb/>後</p>"))
        {:keys [exit out err]} @(process/process ["cmark" "--unsafe"]
                                                 {:in (projection/markdown input) :out :string :err :string})]
    (is (zero? exit) err)
    (is (string/includes? out "<ruby><rb>*字*</rb><rt>_reading_ &lt;b&gt;</rt></ruby>"))
    (is (string/includes? out "<strong>[bold](url)</strong><em>`code`</em>"))
    (is (string/includes? out "&lt;script&gt;alert(1)&lt;/script&gt;"))
    (is (string/includes? out "<br />\n後"))
    (is (not (string/includes? out "<script>")))))

(deftest reports-follow-the-content-each-writer-actually-emits
  (let [input (reading "<p><ruby><rb>字</rb><rt place='left'><g ref='#unknown'/></rt></ruby><hi rend='left-line'>本文</hi><choice><sic>誤</sic><corr>正</corr></choice></p>")
        markdown (projection/report :projection/markdown input)
        plain (projection/report :projection/plaintext input)
        counts (set (map #(select-keys % ["family" "disposition"]) (get markdown "counts")))]
    (is (= "limited" (get markdown "status")))
    (is (= "complete-for-profile" (get plain "status")))
    (is (contains? counts {"family" "g" "disposition" "unresolved"}))
    (is (contains? counts {"family" "ruby-placement" "disposition" "unsupported"}))
    (is (contains? counts {"family" "hi" "disposition" "unsupported"}))
    (is (contains? counts {"family" "alternative-reading" "disposition" "omitted"}))
    (is (thrown? clojure.lang.ExceptionInfo (projection/report :assessment/plaintext input)))))
