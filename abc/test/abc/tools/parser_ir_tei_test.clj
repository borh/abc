(ns abc.tools.parser-ir-tei-test
  (:require [abc.tools.files :as files]
            [abc.tools.parser-ir-publication-policy :as policy]
            [abc.tools.parser-ir-plaintext-test :refer [all-node-parser-ir]]
            [abc.tools.parser-ir-tei :as parser-ir-tei]
            [abc.tools.parser-ir-vocabulary :as vocab]
            [clojure.test :refer [deftest is testing]]))

(def policy-path "data/parser-ir-publication-policy-v0.json")

(defn hiccup-nodes [node]
  (tree-seq vector? rest node))

(deftest coverage-test
  (testing "TEI renderer covers current parser-IR schema node vocabulary"
    (is (empty? (vocab/coverage-errors
                 (vocab/node-types (files/read-json "schemas/parser-ir.schema.json"))
                 "TEI"
                 parser-ir-tei/covered-node-types)))
    (is (= parser-ir-tei/covered-node-types
           (some-> (ns-resolve 'abc.tools.parser-ir-tei 'node-renderers)
                   deref
                   keys
                   set)))
    (is (= (policy/renderer-covered-node-types (policy/load-policy policy-path) "tei")
           parser-ir-tei/covered-node-types))))

(deftest render-body-shape-test
  (testing "TEI renderer returns explicit text/body/paragraph shape"
    (let [result (parser-ir-tei/render all-node-parser-ir)
          [text-node [body-node first-div]] (:body result)
          [div-node head-node & div-children] first-div]
      (is (= :text text-node))
      (is (= :body body-node))
      (is (= :div div-node))
      (is (= :head (first head-node)))
      (is (some #(= :p (first %)) div-children))
      (is (seq (:char_declarations result)))
      (is (= {"heading" 1 "text" 1 "ruby" 1 "gaiji" 2 "editor-note" 1
              "emphasis" 1 "indentation" 1 "page-break" 1 "line-break" 1
              "image" 1 "caption" 1 "quote" 1 "source-note" 1}
             (:node_counts result))))))

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

(deftest emphasis-inline-children-render-nested-tei-test
  (testing "emphasis inline_children render nested hi and ruby nodes"
    (let [result (parser-ir-tei/render
                  {"nodes" [{"type" "emphasis"
                             "span" {"start" 0 "end" 2 "coordinate_system" "decoded_utf8"}
                             "style" "bold"
                             "text" "東京"
                             "inline_children" [{"type" "ruby"
                                                 "span" {"start" 0 "end" 2 "coordinate_system" "decoded_utf8"}
                                                 "ruby" {"base" "東京"
                                                         "reading" "とうきょう"
                                                         "scope" "explicit"
                                                         "direction" "right"}}]}
                            {"type" "emphasis"
                             "span" {"start" 2 "end" 4 "coordinate_system" "decoded_utf8"}
                             "style" "outer"
                             "text" "内"
                             "inline_children" [{"type" "emphasis"
                                                 "span" {"start" 2 "end" 4 "coordinate_system" "decoded_utf8"}
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

(deftest heading-inline-children-render-inside-head-test
  (testing "heading inline_children render structured TEI inside head"
    (let [result (parser-ir-tei/render
                  {"nodes" [{"type" "heading"
                             "span" {"start" 0 "end" 2 "coordinate_system" "decoded_utf8"}
                             "text" "東京"
                             "level" 2
                             "inline_children" [{"type" "ruby"
                                                 "span" {"start" 0 "end" 2 "coordinate_system" "decoded_utf8"}
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
                             "span" {"start" 0 "end" 2 "coordinate_system" "decoded_utf8"}
                             "text" "12"
                             "inline_children" [{"type" "text"
                                                 "span" {"start" 0 "end" 2 "coordinate_system" "decoded_utf8"}
                                                 "text" "12"}]
                             "layout" {"kind" "tcy"
                                       "source" "aat-inline"
                                       "marker" "縦中横"}}
                            {"type" "layout-span"
                             "span" {"start" 2 "end" 3 "coordinate_system" "decoded_utf8"}
                             "text" "大"
                             "layout" {"kind" "font-size"
                                       "source" "aat-inline"
                                       "size_type" "large"
                                       "level" 1}}]})
          paragraph (some #(when (= :p (first %)) %) (hiccup-nodes (:body result)))]
      (is (= [:p
              [:hi {:rend "text-combine-upright"
                    :abc/layout-kind "tcy"
                    :abc/layout-params "marker=縦中横"} "12"]
              [:hi {:rend "font-size large(1)"
                    :abc/layout-kind "font-size"
                    :abc/layout-params "size-type=large;level=1"} "大"]]
             paragraph))
      (is (= {"layout-span" 2 "text" 1} (:node_counts result)))
      (is (empty? (:omitted result))))))

(def ^:private level3-parser-ir
  {"schema_id" "https://w3id.org/abc/schemas/parser-ir.schema.json"
   "schema_hash" "sha256:0000000000000000000000000000000000000000000000000000000000000001"
   "source" {"work_content_hash" "sha256:0000000000000000000000000000000000000000000000000000000000000002"
             "encoding" "Shift_JIS"
             "normalization" "source"}
   "nodes" [{"type" "text" "span" {"start" 0 "end" 12} "text" "第一段"}
            {"type" "text" "span" {"start" 12 "end" 24} "text" "第二段"}
            {"type" "source-note"
             "span" {"start" 24 "end" 64}
             "text" "（古伝説と、シルレルの詩から。）"
             "note_type" "source-attribution"
             "placement" "back"
             "classification" "heuristic"
             "source_pointer" "blocks[78]"}]
   "paragraphs" [{"id" "p000000"
                  "span" {"start" 0 "end" 12 "coordinate_system" "decoded_utf8"}
                  "span_source" "direct"
                  "node_range" {"start" 0 "end" 1}
                  "role" "body"
                  "source_pointer" "blocks[0]"
                  "classification" "direct"}
                 {"id" "p000001"
                  "span" {"start" 12 "end" 24 "coordinate_system" "decoded_utf8"}
                  "span_source" "direct"
                  "node_range" {"start" 1 "end" 2}
                  "role" "body"
                  "source_pointer" "blocks[1]"
                  "classification" "direct"}
                 {"id" "p000002"
                  "span" {"start" 24 "end" 64 "coordinate_system" "decoded_utf8"}
                  "span_source" "direct"
                  "node_range" {"start" 2 "end" 3}
                  "role" "source-note"
                  "source_pointer" "blocks[78]"
                  "classification" "heuristic"}]
   "warnings" []
   "errors" []})

(deftest paragraph-table-renders-body-paragraphs-and-back-source-note-test
  (testing "paragraphs[] drives Level 3 body paragraph boundaries and source-note placement"
    (let [result (parser-ir-tei/render level3-parser-ir)]
      (is (= [:text
              [:body
               [:p "第一段"]
               [:p "第二段"]]
              [:back
               [:div {:type "source"}
                [:note {:type "source-attribution"} "（古伝説と、シルレルの詩から。）"]]]]
             (:body result)))
      (is (= {"text" 2 "source-note" 1}
             (:node_counts result)))
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
                                            "coordinate_system" "decoded_utf8"}
                                    "span_source" "direct"
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
                                            "coordinate_system" "decoded_utf8"}
                                    "span_source" "direct"
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
                    :abc/layout-kind "burasage"
                    :abc/layout-params "first-line-indent=0;continuation-indent=1"} "台詞"]
               [:p {:rend "chitsuki align(right) offset-from-end(1)"
                    :abc/layout-kind "chitsuki"
                    :abc/layout-params "align=right;offset-from-end=1"} "（大正十一年十二月）"]]]
             (:body result)))
      (is (= {"text" 2} (:node_counts result)))
      (is (empty? (:omitted result))))))

(deftest sentence-rows-render-as-tei-s-test
  (testing "parser-IR sentence rows drive TEI s wrappers and orthographic type"
    (let [result (parser-ir-tei/render
                  {"nodes" [{"type" "text"
                             "span" {"start" 0 "end" 24 "coordinate_system" "decoded_utf8"}
                             "text" "吾輩ハ猫デアル。"}
                            {"type" "ruby"
                             "span" {"start" 24 "end" 30 "coordinate_system" "decoded_utf8"}
                             "ruby" {"base" "名前"
                                     "reading" "なまえ"
                                     "scope" "explicit"}}
                            {"type" "text"
                             "span" {"start" 30 "end" 45 "coordinate_system" "decoded_utf8"}
                             "text" "はまだ無い。"}]
                   "paragraphs" [{"id" "p000000"
                                  "span" {"start" 0 "end" 45 "coordinate_system" "decoded_utf8"}
                                  "span_source" "direct"
                                  "node_range" {"start" 0 "end" 3}
                                  "role" "body"
                                  "source_pointer" "blocks[0]"
                                  "classification" "direct"
                                  "layout" {"kind" "jisage"
                                            "indent" 2}}]
                   "sentence_segmentation" {"schema_version" "sentence-segmentation-v1"
                                            "splitter_id" "ab-plaintext-japanese-v1"
                                            "coordinate_system" "decoded_utf8"
                                            "coverage" "body-paragraphs"}
                   "sentences" [{"id" "s000000"
                                 "paragraph_id" "p000000"
                                 "span" {"start" 0 "end" 24 "coordinate_system" "decoded_utf8"}
                                 "node_range" {"start" 0 "end" 1}
                                 "tags" ["orthographic-katakana"]
                                 "orthographic_annotation_indices" [0]}
                                {"id" "s000001"
                                 "paragraph_id" "p000000"
                                 "span" {"start" 24 "end" 45 "coordinate_system" "decoded_utf8"}
                                 "node_range" {"start" 1 "end" 3}
                                 "tags" []
                                 "orthographic_annotation_indices" []}]})
          paragraph (some #(when (= :p (first %)) %) (hiccup-nodes (:body result)))]
      (is (= [:p {:rend "jisage indent(2)"
                  :abc/layout-kind "jisage"
                  :abc/layout-params "indent=2"}
              [:s {:xml:id "s000000" :type "orthographic-katakana"} "吾輩ハ猫デアル。"]
              [:s {:xml:id "s000001"}
               [:ruby {:type "furigana"}
                [:rb "名前"]
                [:rt "なまえ"]]
               "はまだ無い。"]]
             paragraph))
      (is (= {"text" 2 "ruby" 1} (:node_counts result)))
      (is (empty? (:omitted result))))))

(deftest sentence-wrapper-preserves-ruby-reading-test
  (testing "sentence rendering wraps ruby without using reading as sentence text"
    (let [result (parser-ir-tei/render
                  {"nodes" [{"type" "ruby"
                             "span" {"start" 0 "end" 6 "coordinate_system" "decoded_utf8"}
                             "ruby" {"base" "名前"
                                     "reading" "めいしょう"
                                     "scope" "explicit"}}
                            {"type" "text"
                             "span" {"start" 6 "end" 24 "coordinate_system" "decoded_utf8"}
                             "text" "はまだ無い。"}
                            {"type" "text"
                             "span" {"start" 24 "end" 39 "coordinate_system" "decoded_utf8"}
                             "text" "ここは次。"}]
                   "paragraphs" [{"id" "p000000"
                                  "span" {"start" 0 "end" 39 "coordinate_system" "decoded_utf8"}
                                  "span_source" "direct"
                                  "node_range" {"start" 0 "end" 3}
                                  "role" "body"
                                  "source_pointer" "blocks[0]"
                                  "classification" "direct"}]
                   "sentence_segmentation" {"schema_version" "sentence-segmentation-v1"
                                            "splitter_id" "ab-plaintext-japanese-v1"
                                            "coordinate_system" "decoded_utf8"
                                            "coverage" "body-paragraphs"}
                   "sentences" [{"id" "s000000"
                                 "paragraph_id" "p000000"
                                 "span" {"start" 0 "end" 24 "coordinate_system" "decoded_utf8"}
                                 "node_range" {"start" 0 "end" 2}
                                 "tags" []
                                 "orthographic_annotation_indices" []}
                                {"id" "s000001"
                                 "paragraph_id" "p000000"
                                 "span" {"start" 24 "end" 39 "coordinate_system" "decoded_utf8"}
                                 "node_range" {"start" 2 "end" 3}
                                 "tags" []
                                 "orthographic_annotation_indices" []}]})
          paragraph (some #(when (= :p (first %)) %) (hiccup-nodes (:body result)))]
      (is (= [:p
              [:s {:xml:id "s000000"}
               [:ruby {:type "furigana"}
                [:rb "名前"]
                [:rt "めいしょう"]]
               "はまだ無い。"]
              [:s {:xml:id "s000001"} "ここは次。"]]
             paragraph))
      (is (= {"ruby" 1 "text" 2} (:node_counts result)))
      (is (empty? (:omitted result))))))

(deftest fragment-attributes-test
  (testing "fragmented sentences render part/xml:id/next/prev; every <s> gets xml:id"
    (let [result (parser-ir-tei/render
                  {"nodes" [{"type" "text"
                             "span" {"start" 0 "end" 15 "coordinate_system" "decoded_utf8"}
                             "text" "先生は言った。"}
                            {"type" "text"
                             "span" {"start" 15 "end" 30 "coordinate_system" "decoded_utf8"}
                             "text" "「綺麗だ」"}
                            {"type" "text"
                             "span" {"start" 30 "end" 45 "coordinate_system" "decoded_utf8"}
                             "text" "といった。"}]
                   "paragraphs" [{"id" "p000000"
                                  "span" {"start" 0 "end" 45 "coordinate_system" "decoded_utf8"}
                                  "span_source" "direct"
                                  "node_range" {"start" 0 "end" 3}
                                  "role" "body"
                                  "source_pointer" "blocks[0]"
                                  "classification" "direct"}]
                   "sentence_segmentation" {"schema_version" "sentence-segmentation-v1"
                                            "splitter_id" "ab-plaintext-japanese-v2"
                                            "coordinate_system" "decoded_utf8"
                                            "coverage" "body-paragraphs"}
                   "sentences" [{"id" "s000000" "paragraph_id" "p000000"
                                 "span" {"start" 0 "end" 15 "coordinate_system" "decoded_utf8"}
                                 "node_range" {"start" 0 "end" 1}
                                 "tags" [] "orthographic_annotation_indices" []
                                 "part" "I" "fragment_group" "fg000000" "next_id" "s000002"}
                                {"id" "s000001" "paragraph_id" "p000000"
                                 "span" {"start" 15 "end" 30 "coordinate_system" "decoded_utf8"}
                                 "node_range" {"start" 1 "end" 2}
                                 "tags" [] "orthographic_annotation_indices" []}
                                {"id" "s000002" "paragraph_id" "p000000"
                                 "span" {"start" 30 "end" 45 "coordinate_system" "decoded_utf8"}
                                 "node_range" {"start" 2 "end" 3}
                                 "tags" [] "orthographic_annotation_indices" []
                                 "part" "F" "fragment_group" "fg000000" "prev_id" "s000000"}]})
          paragraph (some #(when (= :p (first %)) %) (hiccup-nodes (:body result)))
          s1 (get paragraph 1)
          s2 (get paragraph 2)
          s3 (get paragraph 3)]
      (is (= [:s {:xml:id "s000000" :part "I" :next "#s000002"} "先生は言った。"] s1))
      (is (= [:s {:xml:id "s000001"} "「綺麗だ」"] s2))
      (is (= [:s {:xml:id "s000002" :part "F" :prev "#s000000"} "といった。"] s3)))))

(deftest heading-paragraph-with-sentence-rows-renders-only-head-test
  (testing "sentence rows do not force heading-only body paragraphs into TEI p wrappers"
    (let [result (parser-ir-tei/render
                  {"nodes" [{"type" "heading"
                             "span" {"start" 0 "end" 3 "coordinate_system" "decoded_utf8"}
                             "text" "序"
                             "level" 1}]
                   "paragraphs" [{"id" "p000000"
                                  "span" {"start" 0 "end" 3 "coordinate_system" "decoded_utf8"}
                                  "span_source" "direct"
                                  "node_range" {"start" 0 "end" 1}
                                  "role" "body"
                                  "source_pointer" "blocks[0]"
                                  "classification" "direct"}]
                   "sentence_segmentation" {"schema_version" "sentence-segmentation-v1"
                                            "splitter_id" "ab-plaintext-japanese-v1"
                                            "coordinate_system" "decoded_utf8"
                                            "coverage" "body-paragraphs"}
                   "sentences" [{"id" "s000000"
                                 "paragraph_id" "p000000"
                                 "span" {"start" 0 "end" 3 "coordinate_system" "decoded_utf8"}
                                 "node_range" {"start" 0 "end" 1}
                                 "tags" []
                                 "orthographic_annotation_indices" []}]})]
      (is (= [:text
              [:body
               [:div
                [:head {:n "1"} "序"]]]]
             (:body result)))
      (is (= {"heading" 1} (:node_counts result)))
      (is (empty? (:omitted result))))))

(deftest paragraph-layout-renders-every-supported-rend-token-test
  (testing "each supported paragraph layout kind has a deterministic TEI rend token"
    (let [parser-ir {"nodes" [{"type" "text" "span" {"start" 0 "end" 1} "text" "一"}
                              {"type" "text" "span" {"start" 1 "end" 2} "text" "二"}
                              {"type" "text" "span" {"start" 2 "end" 3} "text" "三"}]
                     "paragraphs" [{"id" "p000000"
                                    "span" {"start" 0 "end" 1 "coordinate_system" "decoded_utf8"}
                                    "span_source" "direct"
                                    "node_range" {"start" 0 "end" 1}
                                    "role" "body"
                                    "source_pointer" "blocks[0]"
                                    "classification" "direct"
                                    "layout" {"kind" "jisage"
                                              "indent" 2
                                              "source" "aat-block"}}
                                   {"id" "p000001"
                                    "span" {"start" 1 "end" 2 "coordinate_system" "decoded_utf8"}
                                    "span_source" "direct"
                                    "node_range" {"start" 1 "end" 2}
                                    "role" "body"
                                    "source_pointer" "blocks[1]"
                                    "classification" "direct"
                                    "layout" {"kind" "jizume"
                                              "width" 20
                                              "source" "source-derived"}}
                                   {"id" "p000002"
                                    "span" {"start" 2 "end" 3 "coordinate_system" "decoded_utf8"}
                                    "span_source" "direct"
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
                    :abc/layout-kind "jisage"
                    :abc/layout-params "indent=2"} "一"]
               [:p {:rend "jizume width(20)"
                    :abc/layout-kind "jizume"
                    :abc/layout-params "width=20"} "二"]
               [:p {:rend "line-jisage indent(3)"
                    :abc/layout-kind "line-jisage"
                    :abc/layout-params "indent=3"} "三"]]]
             (:body result)))
      (is (= {"text" 3} (:node_counts result)))
      (is (empty? (:omitted result))))))

(deftest gaiji-reference-declaration-contract-test
  (testing "fixture gaiji.reference is preserved as ref and charDecl id"
    (let [result (parser-ir-tei/render
                  (files/read-json "examples/v0/example-work/parser-ir.json"))]
      (is (some #(= "example-gaiji" (:xml-id %))
                (:char_declarations result)))
      (is (= "※［＃例字］"
             (:raw-marker (some #(when (= "example-gaiji" (:xml-id %)) %)
                                (:char_declarations result)))))
      (is (some #(= [:g {:ref "#example-gaiji"}] %)
                (hiccup-nodes (:body result)))))))

(deftest ruby-direction-uses-profile-valid-rend-test
  (testing "ruby direction is preserved without TEI-invalid place attributes"
    (let [result (parser-ir-tei/render
                  {"nodes" [{"type" "ruby"
                             "span" {"start" 0 "end" 3}
                             "ruby" {"base" "下人"
                                     "reading" "げにん"
                                     "direction" "right"}}]})
          ruby-node (some #(when (= :ruby (first %)) %)
                          (hiccup-nodes (:body result)))]
      (is (= [:ruby {:type "furigana" :rend "right"} [:rb "下人"] [:rt "げにん"]]
             ruby-node))
      (is (not (contains? (second ruby-node) :place))))))

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

(deftest trailing-heading-starts-a-new-division-test
  (testing "headings after paragraph content keep source order by starting a later div"
    (let [result (parser-ir-tei/render
                  (files/read-json "examples/v0/example-work/parser-ir.json"))
          [_ [body-node first-child second-child third-child]] (:body result)]
      (is (= :body body-node))
      (is (= :p (first first-child)))
      (is (= :p (first second-child)))
      (is (= :div (first third-child)))
      (is (= :head (first (second third-child)))))))

(deftest char-declaration-order-test
  (testing "char declarations are first-appearance ordered and deduplicated"
    (let [parser-ir (assoc all-node-parser-ir "nodes"
                           [{"type" "gaiji" "span" {"start" 0 "end" 1}
                             "gaiji" {"raw_marker" "A" "reference" "gaiji-b" "resolved" false}}
                            {"type" "gaiji" "span" {"start" 1 "end" 2}
                             "gaiji" {"raw_marker" "B" "reference" "gaiji-a" "resolved" false}}
                            {"type" "gaiji" "span" {"start" 2 "end" 3}
                             "gaiji" {"raw_marker" "C" "reference" "gaiji-b" "resolved" false}}])
          result (parser-ir-tei/render parser-ir)]
      (is (= ["gaiji-b" "gaiji-a"]
             (mapv :xml-id (:char_declarations result)))))))

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
