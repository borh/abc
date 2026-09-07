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

(deftest commonmark-preserves-corpus-rendition-shapes-and-source-side
  (doseq [[rend css]
          [["bouten 傍点 right" "text-emphasis-style: filled sesame; text-emphasis-position: over right"]
           ["bouten 白ゴマ傍点 right" "text-emphasis-style: open sesame; text-emphasis-position: over right"]
           ["bouten 丸傍点 right" "text-emphasis-style: filled circle; text-emphasis-position: over right"]
           ["bouten 白丸傍点 left" "text-emphasis-style: open circle; text-emphasis-position: under left"]
           ["bouten 白三角傍点 right" "text-emphasis-style: open triangle; text-emphasis-position: over right"]
           ["bouten 黒三角傍点 right" "text-emphasis-style: filled triangle; text-emphasis-position: over right"]
           ["bouten 二重丸傍点 right" "text-emphasis-style: '◎'; text-emphasis-position: over right"]
           ["bouten 蛇の目傍点 right" "text-emphasis-style: '◉'; text-emphasis-position: over right"]
           ["bouten ばつ傍点 right" "text-emphasis-style: '×'; text-emphasis-position: over right"]
           ["bosen 傍線 right" "text-decoration-line: underline; text-decoration-style: solid"]
           ["bosen 傍線 left" "text-decoration-line: overline; text-decoration-style: solid"]
           ["bosen 傍線 both" "text-decoration-line: underline overline; text-decoration-style: solid"]
           ["bosen 二重傍線 right" "text-decoration-line: underline; text-decoration-style: double"]
           ["bosen 波線 right" "text-decoration-line: underline; text-decoration-style: wavy"]
           ["text-combine-upright" "text-combine-upright: all"]
           ["yokogumi horizontal" "writing-mode: horizontal-tb"]
           ["keigakomi" "border: 1px solid"]]]
    (let [input (reading (str "<p>前<hi rend='" rend "'><ruby><rb>字*</rb><rt>じ_</rt></ruby>"
                              "<lb/>[後]</hi>。</p>"))
          {:keys [exit out err]} @(process/process ["cmark" "--unsafe"]
                                                   {:in (projection/markdown input) :out :string :err :string})]
      (is (zero? exit) err)
      (is (= (str "<p>前<span data-tei-rend=\"" rend "\" style=\"" css "\">"
                  "<ruby><rb>字*</rb><rt>じ_</rt></ruby><br>[後]</span>。</p>\n") out) rend)
      (is (= "前字*\n[後]。" (projection/plaintext input)))
      (is (= "complete-for-profile" (get (projection/report :projection/markdown input) "status"))))))
