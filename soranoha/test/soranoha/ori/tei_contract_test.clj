(ns soranoha.ori.tei-contract-test
  (:require [soranoha.ori.tei :as parser-ir-tei]
            [clojure.string :as string]
            [clojure.test :refer [deftest is testing]]))

(defn hiccup-nodes [node]
  (tree-seq vector? rest node))

(deftest empty-body-renders-explicit-gap-test
  (testing "empty parser-IR bodies render valid TEI without inventing text"
    (let [result (parser-ir-tei/render {"nodes" []
                                        "paragraphs" []})]
      (is (= [:text
              [:body
               [:p
                [:gap {:reason "missing"}]]]]
             (:body result)))
      (is (empty? (:node_counts result)))
      (is (empty? (:omitted result))))))

(deftest line-break-renders-as-tei-lb-test
  (testing "explicit source line breaks project to the TEI P5 lb milestone"
    (let [result (parser-ir-tei/render
                  {"nodes" [{"type" "text"
                             "span" {"start" 0 "end" 3}
                             "text" "前"}
                            {"type" "line-break"
                             "span" {"start" 3 "end" 8}
                             "marker" "［＃改行］"}
                            {"type" "text"
                             "span" {"start" 8 "end" 11}
                             "text" "後"}]})
          paragraph (some #(when (= :p (first %)) %) (hiccup-nodes (:body result)))]
      (is (= [:p "前" [:lb] "後"] paragraph))
      (is (= {"text" 2 "line-break" 1} (:node_counts result)))
      (is (empty? (:omitted result))))))

(deftest source-text-newline-runs-render-as-structural-lb-test
  (testing "source/control-line separator whitespace does not enter TEI as literal body text"
    (let [result (parser-ir-tei/render
                  {"nodes" [{"type" "text"
                             "span" {"start" 0 "end" 20 "coordinate_system" "parser_text_utf8"}
                             "text" "\r\n\r\n序\r\n\r\n\r\n本文\r\n\r\n"}]})
          paragraph (some #(when (= :p (first %)) %) (hiccup-nodes (:body result)))]
      (is (= [:p "序" [:lb] "本文"] paragraph))
      (is (not-any? #(and (string? %) (re-find #"\r|\n" %))
                    (hiccup-nodes (:body result)))))))

(deftest emphasis-inline-children-render-nested-tei-test
  (testing "emphasis inline_children render nested hi and ruby nodes"
    (let [result (parser-ir-tei/render
                  {"nodes" [{"type" "emphasis"
                             "span" {"start" 0 "end" 2 "coordinate_system" "parser_text_utf8"}
                             "style" "bold"
                             "text" "東京"
                             "inline_children" [{"type" "ruby"
                                                 "span" {"start" 0 "end" 2 "coordinate_system" "parser_text_utf8"}
                                                 "ruby" {"base" "東京"
                                                         "reading" "とうきょう"
                                                         "scope" "explicit"
                                                         "direction" "right"}}]}
                            {"type" "emphasis"
                             "span" {"start" 2 "end" 4 "coordinate_system" "parser_text_utf8"}
                             "style" "outer"
                             "text" "内"
                             "inline_children" [{"type" "emphasis"
                                                 "span" {"start" 2 "end" 4 "coordinate_system" "parser_text_utf8"}
                                                 "style" "inner"
                                                 "text" "内"}]}]})
          paragraph (some #(when (= :p (first %)) %) (hiccup-nodes (:body result)))]
      (is (= [:p
              [:hi {:rend "bold"}
               [:ruby {:type "furigana" :rend "right"}
                [:rb "東京"]
                [:rt "とうきょう"]]]
              [:hi {:rend "outer"}
               [:hi {:rend "inner"} "内"]]]
             paragraph))
      (is (= {"emphasis" 3 "ruby" 1} (:node_counts result)))
      (is (empty? (:omitted result))))))

(deftest nested-heading-renders-inline-and-keeps-source-order-test
  (testing "a heading inside inline content stays in the phrase that contains it"
    ;; The converter's inline child projection maps the `heading` kind and
    ;; copies the style through, so a `normal` heading inside emphasis
    ;; children is a shape the corpus can produce. Rendering it as a division
    ;; heading would close the paragraph from inside the emphasis, which put
    ;; the text before the emphasis after the heading and cost the emphasis
    ;; the run in front of it.
    (let [result (parser-ir-tei/render
                  {"nodes" [{"type" "text"
                             "span" {"start" 0 "end" 2 "coordinate_system" "parser_text_utf8"}
                             "text" "前置"}
                            {"type" "emphasis"
                             "span" {"start" 2 "end" 6 "coordinate_system" "parser_text_utf8"}
                             "style" "bold"
                             "text" "甲見出乙"
                             "inline_children" [{"type" "text"
                                                 "span" {"start" 2 "end" 3 "coordinate_system" "parser_text_utf8"}
                                                 "text" "甲"}
                                                {"type" "heading"
                                                 "span" {"start" 3 "end" 5 "coordinate_system" "parser_text_utf8"}
                                                 "text" "見出"
                                                 "level" 2
                                                 "style" "normal"}
                                                {"type" "text"
                                                 "span" {"start" 5 "end" 6 "coordinate_system" "parser_text_utf8"}
                                                 "text" "乙"}]}]})
          paragraphs (filterv #(= :p (first %)) (hiccup-nodes (:body result)))]
      (is (= [[:p "前置"
               [:hi {:rend "bold"}
                "甲"
                [:seg {:n "2" :type "heading" :rend "normal"} "見出"]
                "乙"]]]
             paragraphs)
          "one paragraph, in source order, with the emphasis spanning the heading")
      (is (empty? (filterv #(= :head (first %)) (hiccup-nodes (:body result)))))
      (is (empty? (:omitted result))))))

(deftest nested-front-source-note-stays-in-the-phrase-test
  (testing "a front-placed source note inside inline content renders where it stands"
    ;; Only hand-written or third-party parser IR reaches this: the converter
    ;; emits source notes as blocks. It used to throw, because the wrapper
    ;; recovered its fragment by index from a paragraph the note had closed.
    (let [result (parser-ir-tei/render
                  {"nodes" [{"type" "text"
                             "span" {"start" 0 "end" 2 "coordinate_system" "parser_text_utf8"}
                             "text" "前置"}
                            {"type" "emphasis"
                             "span" {"start" 2 "end" 4 "coordinate_system" "parser_text_utf8"}
                             "style" "bold"
                             "text" "甲乙"
                             "inline_children" [{"type" "text"
                                                 "span" {"start" 2 "end" 3 "coordinate_system" "parser_text_utf8"}
                                                 "text" "甲"}
                                                {"type" "source-note"
                                                 "span" {"start" 3 "end" 3 "coordinate_system" "parser_text_utf8"}
                                                 "text" "底本注"
                                                 "note_type" "source"
                                                 "placement" "front"}
                                                {"type" "text"
                                                 "span" {"start" 3 "end" 4 "coordinate_system" "parser_text_utf8"}
                                                 "text" "乙"}]}]})]
      (is (= [:text
              [:body
               [:p "前置"
                [:hi {:rend "bold"}
                 "甲"
                 [:note {:type "source"} [:seg {:type "source-line"} "底本注"]]
                 "乙"]]]]
             (:body result))
          "no front matter, and the note keeps its place in the run of text")
      (is (empty? (:omitted result))))))

(deftest heading-inline-children-render-inside-head-test
  (testing "heading inline_children render structured TEI inside head"
    (let [result (parser-ir-tei/render
                  {"nodes" [{"type" "heading"
                             "span" {"start" 0 "end" 2 "coordinate_system" "parser_text_utf8"}
                             "text" "東京"
                             "level" 2
                             "inline_children" [{"type" "ruby"
                                                 "span" {"start" 0 "end" 2 "coordinate_system" "parser_text_utf8"}
                                                 "ruby" {"base" "東京"
                                                         "reading" "とうきょう"
                                                         "scope" "explicit"
                                                         "direction" "right"}}]}]})
          head (some #(when (= :head (first %)) %) (hiccup-nodes (:body result)))]
      (is (= [:head {:n "2"}
              [:ruby {:type "furigana" :rend "right"}
               [:rb "東京"]
               [:rt "とうきょう"]]]
             head))
      (is (= {"heading" 1 "ruby" 1} (:node_counts result)))
      (is (empty? (:omitted result))))))

(deftest layout-span-renders-profile-rend-test
  (testing "layout-span projects typed layout facts to TEI rend"
    (let [result (parser-ir-tei/render
                  {"nodes" [{"type" "layout-span"
                             "span" {"start" 0 "end" 2 "coordinate_system" "parser_text_utf8"}
                             "text" "12"
                             "inline_children" [{"type" "text"
                                                 "span" {"start" 0 "end" 2 "coordinate_system" "parser_text_utf8"}
                                                 "text" "12"}]
                             "layout" {"kind" "tcy"
                                       "source" "aat-inline"
                                       "marker" "縦中横"}}
                            {"type" "layout-span"
                             "span" {"start" 2 "end" 3 "coordinate_system" "parser_text_utf8"}
                             "text" "大"
                             "layout" {"kind" "font-size"
                                       "source" "aat-inline"
                                       "size_type" "large"
                                       "level" 1}}]})
          paragraph (some #(when (= :p (first %)) %) (hiccup-nodes (:body result)))]
      (is (= [:p
              [:hi {:rend "text-combine-upright"
                    :snh/layout-kind "tcy"
                    :snh/layout-params "marker=縦中横"} "12"]
              [:hi {:rend "font-size large(1)"
                    :snh/layout-kind "font-size"
                    :snh/layout-params "size-type=large;level=1"} "大"]]
             paragraph))
      (is (= {"layout-span" 2 "text" 1} (:node_counts result)))
      (is (empty? (:omitted result))))))

(deftest paragraph-layout-renders-as-tei-rend-test
  (testing "paragraph layout metadata is projected to TEI p@rend"
    (let [parser-ir {"nodes" [{"type" "text"
                               "span" {"start" 0 "end" 6}
                               "text" "台詞"}
                              {"type" "text"
                               "span" {"start" 6 "end" 36}
                               "text" "（大正十一年十二月）"}]
                     "paragraphs" [{"id" "p000000"
                                    "span" {"start" 0 "end" 6
                                            "coordinate_system" "parser_text_utf8"}
                                    "node_range" {"start" 0 "end" 1}
                                    "role" "body"
                                    "source_pointer" "blocks[0]"
                                    "classification" "direct"
                                    "layout" {"kind" "burasage"
                                              "first_line_indent" 0
                                              "continuation_indent" 1
                                              "source" "aat-style"}}
                                   {"id" "p000001"
                                    "span" {"start" 6 "end" 36
                                            "coordinate_system" "parser_text_utf8"}
                                    "node_range" {"start" 1 "end" 2}
                                    "role" "body"
                                    "source_pointer" "blocks[1]"
                                    "classification" "direct"
                                    "layout" {"kind" "chitsuki"
                                              "align" "right"
                                              "offset_from_end" 1
                                              "source" "aat-style"}}]}
          result (parser-ir-tei/render parser-ir)]
      (is (= [:text
              [:body
               [:p {:rend "burasage first(0) rest(1)"
                    :snh/layout-kind "burasage"
                    :snh/layout-params "first-line-indent=0;continuation-indent=1"} "台詞"]
               [:p {:rend "chitsuki align(right) offset-from-end(1)"
                    :snh/layout-kind "chitsuki"
                    :snh/layout-params "align=right;offset-from-end=1"} "（大正十一年十二月）"]]]
             (:body result)))
      (is (= {"text" 2} (:node_counts result)))
      (is (empty? (:omitted result))))))

(deftest paragraph-layout-renders-every-supported-rend-token-test
  (testing "each supported paragraph layout kind has a deterministic TEI rend token"
    (let [parser-ir {"nodes" [{"type" "text" "span" {"start" 0 "end" 1} "text" "一"}
                              {"type" "text" "span" {"start" 1 "end" 2} "text" "二"}
                              {"type" "text" "span" {"start" 2 "end" 3} "text" "三"}]
                     "paragraphs" [{"id" "p000000"
                                    "span" {"start" 0 "end" 1 "coordinate_system" "parser_text_utf8"}
                                    "node_range" {"start" 0 "end" 1}
                                    "role" "body"
                                    "source_pointer" "blocks[0]"
                                    "classification" "direct"
                                    "layout" {"kind" "jisage"
                                              "indent" 2
                                              "source" "aat-block"}}
                                   {"id" "p000001"
                                    "span" {"start" 1 "end" 2 "coordinate_system" "parser_text_utf8"}
                                    "node_range" {"start" 1 "end" 2}
                                    "role" "body"
                                    "source_pointer" "blocks[1]"
                                    "classification" "direct"
                                    "layout" {"kind" "jizume"
                                              "width" 20
                                              "source" "source-derived"}}
                                   {"id" "p000002"
                                    "span" {"start" 2 "end" 3 "coordinate_system" "parser_text_utf8"}
                                    "node_range" {"start" 2 "end" 3}
                                    "role" "body"
                                    "source_pointer" "blocks[2]"
                                    "classification" "direct"
                                    "layout" {"kind" "line-jisage"
                                              "indent" 3
                                              "source" "source-derived"}}]}
          result (parser-ir-tei/render parser-ir)]
      (is (= [:text
              [:body
               [:p {:rend "jisage indent(2)"
                    :snh/layout-kind "jisage"
                    :snh/layout-params "indent=2"} "一"]
               [:p {:rend "jizume width(20)"
                    :snh/layout-kind "jizume"
                    :snh/layout-params "width=20"} "二"]
               [:p {:rend "line-jisage indent(3)"
                    :snh/layout-kind "line-jisage"
                    :snh/layout-params "indent=3"} "三"]]]
             (:body result)))
      (is (= {"text" 3} (:node_counts result)))
      (is (empty? (:omitted result))))))

(deftest ruby-direction-uses-profile-valid-rend-test
  (testing "ruby direction is preserved without TEI-invalid place attributes"
    (doseq [direction ["right" "left"]]
      (let [result (parser-ir-tei/render
                    {"nodes" [{"type" "ruby"
                               "span" {"start" 0 "end" 3}
                               "ruby" {"base" "下人"
                                       "reading" "げにん"
                                       "direction" direction}}]})
            ruby-node (some #(when (= :ruby (first %)) %)
                            (hiccup-nodes (:body result)))]
        (is (= [:ruby {:type "furigana" :rend direction} [:rb "下人"] [:rt "げにん"]]
               ruby-node))
        (is (not (contains? (second ruby-node) :place)))))))

(deftest empty-ruby-base-is-omitted-test
  (testing "ruby with no source base does not emit TEI-invalid empty rb"
    (let [result (parser-ir-tei/render
                  {"nodes" [{"type" "ruby"
                             "span" {"start" 7 "end" 7}
                             "ruby" {"base" ""
                                     "reading" "ね"
                                     "direction" "right"}}]})]
      (is (not-any? #(= :ruby (first %))
                    (hiccup-nodes (:body result))))
      (is (= [{:type "ruby" :policy "omitted"}]
             (:omitted result))))))

(deftest whitespace-only-ruby-base-is-omitted-test
  (testing "ruby with only source whitespace does not emit Schematron-invalid rb"
    (let [result (parser-ir-tei/render
                  {"nodes" [{"type" "ruby"
                             "span" {"start" 6 "end" 7}
                             "ruby" {"base" "\r"
                                     "reading" "ラルュウ"
                                     "direction" "right"}}]})]
      (is (not-any? #(= :ruby (first %))
                    (hiccup-nodes (:body result))))
      (is (= [{:type "ruby" :policy "omitted"}]
             (:omitted result))))))

(deftest ruby-base-carried-only-by-children-is-not-dropped-test
  (testing "a ruby whose base lives in its children renders that base"
    ;; The converter derives `ruby.base` as the plain visible text of the AAT
    ;; node's base content, and the kinds `raw`, `kunten`, `editorial_note`,
    ;; `layout_break` and `figure` contribute none. A base made only of those
    ;; therefore arrives as an empty string beside the `inline_children` that
    ;; hold it. Below: the direct shape, then the converter-derived one.
    (let [direct (parser-ir-tei/render
                  {"nodes" [{"type" "ruby"
                             "span" {"start" 0 "end" 1 "coordinate_system" "parser_text_utf8"}
                             "ruby" {"base" "" "reading" "かんじ" "direction" "right"}
                             "inline_children" [{"type" "text"
                                                 "span" {"start" 0 "end" 1 "coordinate_system" "parser_text_utf8"}
                                                 "text" "漢"}]}]})
          converted (parser-ir-tei/render
                     {"nodes" [{"type" "ruby"
                                "span" {"start" 0 "end" 0 "coordinate_system" "parser_text_utf8"}
                                "ruby" {"base" "" "reading" "かんじ" "direction" "right"}
                                "inline_children" [{"type" "kunten"
                                                    "span" {"start" 0 "end" 0 "coordinate_system" "parser_text_utf8"}
                                                    "kunten_kind" "return-mark"
                                                    "text" "レ"}]}]})]
      (is (= [:p [:ruby {:type "furigana" :rend "right"}
                  [:rb "漢"]
                  [:rt "かんじ"]]]
             (some #(when (= :p (first %)) %) (hiccup-nodes (:body direct)))))
      (is (empty? (:omitted direct)))
      (is (= [:p [:ruby {:type "furigana" :rend "right"}
                  [:rb [:note {:type "kunten" :subtype "return-mark" :rend "subscript"} "レ"]]
                  [:rt "かんじ"]]]
             (some #(when (= :p (first %)) %) (hiccup-nodes (:body converted)))))
      (is (empty? (:omitted converted))))))

(deftest ruby-side-that-renders-to-nothing-is-omitted-test
  (testing "children that render to nothing leave no empty rb behind"
    ;; The profile has no empty `rb` or `rt`, so presence is decided on what a
    ;; side rendered rather than on whether it had children to render.
    (let [result (parser-ir-tei/render
                  {"nodes" [{"type" "ruby"
                             "span" {"start" 0 "end" 0 "coordinate_system" "parser_text_utf8"}
                             "ruby" {"base" "" "reading" "かんじ" "direction" "right"}
                             "inline_children" [{"type" "quote"
                                                 "span" {"start" 0 "end" 0 "coordinate_system" "parser_text_utf8"}
                                                 "marker_type" "open"
                                                 "text" ""}]}]})]
      (is (not-any? #(= :ruby (first %)) (hiccup-nodes (:body result))))
      (is (= [{:type "ruby" :policy "omitted"}] (:omitted result))))))

(deftest gaiji-jis-reference-mints-xml-safe-id-test
  (testing "Aozora JIS references are preserved semantically but minted as XML-safe char ids"
    (let [parser-ir {"nodes" [{"type" "gaiji"
                               "span" {"start" 10 "end" 20}
                               "gaiji" {"raw_marker" "※［＃二の字点、1-2-22］"
                                        "reference" "1-2-22"
                                        "resolved" false}}]}
          result (parser-ir-tei/render parser-ir)]
      (is (= [{:xml-id "gaiji-1-2-22"
               :raw-marker "※［＃二の字点、1-2-22］"}]
             (:char_declarations result)))
      (is (some #(= [:g {:ref "#gaiji-1-2-22"}] %)
                (hiccup-nodes (:body result)))))))

(deftest gaiji-reference-cannot-take-a-source-span-note-id-test
  (testing "a reference shaped like a span note id still mints under the gaiji prefix"
    ;; Character declarations and source-span notes write into the one xml:id
    ;; space from two schemes that never consult each other, so `gaiji-` is
    ;; reserved for this one. Without that, a reference of `source-0-6` landed
    ;; on the note for extent 0 to 6 and g/@ref resolved to both.
    (let [result (parser-ir-tei/render
                  {"nodes" [{"type" "text"
                             "source_span" {"start" 0 "end" 6 "coordinate_system" "decoded_utf8"}
                             "text" "本文です"}
                            {"type" "gaiji"
                             "span" {"start" 6 "end" 7}
                             "source_span" {"start" 6 "end" 7 "coordinate_system" "decoded_utf8"}
                             "gaiji" {"raw_marker" "※［＃衝突］"
                                      "reference" "source-0-6"
                                      "resolved" false}}]})
          ids (concat (map :xml-id (:char_declarations result))
                      (keep #(get-in % [1 :xml/id]) (hiccup-nodes (:body result))))]
      (is (= ["gaiji-source-0-6"] (map :xml-id (:char_declarations result))))
      (is (some #(= [:g {:ref "#gaiji-source-0-6" :source "#source-6-7"}] %)
                (hiccup-nodes (:body result))))
      (is (= (count ids) (count (distinct ids)))
          "every minted xml:id in the document is its own"))))

(deftest gaiji-without-reference-declaration-contract-test
  (testing "gaiji without reference generates a span-derived id and preserves the raw marker"
    (let [parser-ir {"nodes" [{"type" "gaiji"
                               "span" {"start" 12 "end" 34}
                               "gaiji" {"raw_marker" "[?]" "resolved" false}}]}
          result (parser-ir-tei/render parser-ir)]
      (is (= [{:xml-id "gaiji-12-34" :raw-marker "[?]"}]
             (:char_declarations result)))
      (is (some #(= [:g {:ref "#gaiji-12-34"}] %)
                (hiccup-nodes (:body result)))))))

(deftest ruby-reading-gaiji-retains-declaration-and-source-identity-test
  (doseq [unicode ["ヹ" nil]]
    (let [gaiji {"type" "gaiji"
                 "source_span" {"start" 9 "end" 54 "coordinate_system" "decoded_utf8"}
                 "gaiji" {"reference" "1-7-84" "unicode" unicode
                          "raw_marker" "濁点付き片仮名ヱ" "resolved" (some? unicode)}}
          ir {"nodes" [{"type" "ruby" "span" {"start" 0 "end" 6}
                        "ruby" {"base" "淡絹" "reading" (or unicode "濁点付き片仮名ヱ") "scope" "explicit"}
                        "reading_children" [gaiji]}]}
          result (parser-ir-tei/render ir)
          rt (some #(when (= :rt (first %)) %) (hiccup-nodes (:body result)))]
      (is (= (if unicode [:rt [:g (second (second rt)) unicode]]
                 [:rt [:g (second (second rt))]]) rt))
      (is (= "#gaiji-1-7-84" (get-in rt [1 1 :ref])))
      (is (string/starts-with? (get-in rt [1 1 :source]) "#source-"))
      (is (= [{:xml-id "gaiji-1-7-84" :unicode unicode :raw-marker "濁点付き片仮名ヱ"}]
             (:char_declarations result)))
      (is (empty? (:omitted result))))))

(deftest font-size-and-script-layout-retain-distinct-source-axes
  (doseq [[layout rend params]
          [[{"kind" "font-size" "size_type" "small" "level" 2}
            "font-size small(2)" "size-type=small;level=2"]
           [{"kind" "font-size" "size_type" "absolute" "size" "small"}
            "font-size absolute(small)" "size-type=absolute;size=small"]
           [{"kind" "small-script" "position" "right"}
            "small-script right" "position=right"]
           [{"kind" "small-script" "position" "left"}
            "small-script left" "position=left"]]]
    (let [result (parser-ir-tei/render {"nodes" [{"type" "layout-span" "layout" layout "text" "字"
                                                  "inline_children" [{"type" "ruby"
                                                                      "ruby" {"base" "字" "reading" "じ"}}]}]})
          hi (first (filter #(and (vector? %) (= :hi (first %))) (hiccup-nodes (:body result))))]
      (is (= rend (get-in hi [1 :rend])))
      (is (= params (get-in hi [1 :snh/layout-params])))
      (is (= [:rb "字"] (some #(when (and (vector? %) (= :rb (first %))) %) (hiccup-nodes hi)))))))

(deftest quotation-delimiters-render-as-text-not-as-quote-elements-test
  (testing "a quote marker is one delimiter character, so it is text"
    ;; The parser IR emits one `quote` node per delimiter, carrying
    ;; `marker_type` and the single character. Rendering that as TEI `<quote>`
    ;; said the delimiter was the quoted passage and the speech beside it was
    ;; not, which inverts what the source means.
    (let [result (parser-ir-tei/render
                  {"nodes" [{"type" "quote"
                             "span" {"start" 0 "end" 3
                                     "coordinate_system" "decoded_utf8"}
                             "marker_type" "open"
                             "text" "「"}
                            {"type" "text"
                             "span" {"start" 3 "end" 12
                                     "coordinate_system" "decoded_utf8"}
                             "text" "新小説"}
                            {"type" "quote"
                             "span" {"start" 12 "end" 15
                                     "coordinate_system" "decoded_utf8"}
                             "marker_type" "close"
                             "text" "」"}]
                   "paragraphs" [{"node_range" {"start" 0 "end" 3}}]})
          tags (into #{} (comp (filter vector?) (map first))
                     (hiccup-nodes (:body result)))
          text (apply str (filter string? (hiccup-nodes (:body result))))]
      (is (not (contains? tags :quote))
          "no element may claim the delimiter is the quotation")
      (is (= "「新小説」" text)
          "every character of the source reaches the reader"))))

(deftest body-end-boundary-is-not-published-as-a-note-test
  (testing "the marker saying the body ended is not a note about the source edition"
    (let [result (parser-ir-tei/render
                  {"nodes" [{"type" "text"
                             "span" {"start" 0 "end" 3}
                             "text" "本文"}
                            {"type" "source-note"
                             "span" {"start" 3 "end" 11}
                             "text" "［＃本文終わり］\n"
                             "note_type" "body-end-boundary"
                             "placement" "back"
                             "classification" "direct"
                             "source_pointer" "blocks[1]"}
                            {"type" "source-note"
                             "span" {"start" 11 "end" 19}
                             "text" "底本：「作品集」"
                             "note_type" "source-attribution"
                             "placement" "back"
                             "classification" "direct"
                             "source_pointer" "blocks[2]"}]
                   "paragraphs" [{"role" "body" "node_range" {"start" 0 "end" 1}}
                                 {"role" "source-note" "node_range" {"start" 1 "end" 2}}
                                 {"role" "source-note" "node_range" {"start" 2 "end" 3}}]})
          xml (pr-str (:body result))]
      (is (not (string/includes? xml "本文終わり"))
          "TEI says where the body ends by where the source division starts")
      (is (string/includes? xml "底本：「作品集」")
          "the colophon itself is still published")
      (is (= [{:type "body-end-boundary" :policy "omitted"}] (:omitted result))
          "the marker is accounted for as an omission, not dropped in silence"))))
