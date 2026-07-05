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
               [:p {:rend "burasage first(0) rest(1)"} "台詞"]
               [:p {:rend "chitsuki align(right) offset-from-end(1)"} "（大正十一年十二月）"]]]
             (:body result)))
      (is (= {"text" 2} (:node_counts result)))
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
      (is (= [:ruby {:rend "right"} [:rb "下人"] [:rt "げにん"]]
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
