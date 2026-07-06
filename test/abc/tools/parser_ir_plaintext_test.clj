(ns abc.tools.parser-ir-plaintext-test
  (:require [abc.tools.files :as files]
            [abc.tools.parser-ir-publication-policy :as policy]
            [abc.tools.parser-ir-plaintext :as plaintext]
            [abc.tools.parser-ir-vocabulary :as vocab]
            [clojure.test :refer [deftest is testing]]))

(def policy-path "data/parser-ir-publication-policy-v0.json")

(def all-node-parser-ir
  {"schema_id" "https://w3id.org/abc/schemas/parser-ir.schema.json"
   "schema_hash" "sha256:0000000000000000000000000000000000000000000000000000000000000001"
   "source" {"work_content_hash" "sha256:0000000000000000000000000000000000000000000000000000000000000002"
             "encoding" "Shift_JIS"
             "normalization" "source"}
   "nodes" [{"type" "heading" "span" {"start" 0 "end" 1} "text" "H" "level" 1}
            {"type" "text" "span" {"start" 1 "end" 2} "text" "A"}
            {"type" "ruby" "span" {"start" 2 "end" 3} "ruby" {"base" "B" "reading" "b" "scope" "explicit"}}
            {"type" "gaiji" "span" {"start" 3 "end" 4} "gaiji" {"raw_marker" "※［＃x］" "unicode" "X" "resolved" true}}
            {"type" "gaiji" "span" {"start" 4 "end" 5} "gaiji" {"raw_marker" "※［＃y］" "resolved" false}}
            {"type" "editor-note" "span" {"start" 5 "end" 6} "note" {"raw" "［＃地付き］" "category" "indentation"}}
            {"type" "emphasis" "span" {"start" 6 "end" 7} "text" "C" "style" "boten"}
            {"type" "indentation" "span" {"start" 7 "end" 8} "depth" 2 "text" "D"}
            {"type" "page-break" "span" {"start" 8 "end" 9} "marker" "［＃改ページ］"}
            {"type" "line-break" "span" {"start" 9 "end" 10} "marker" "［＃改行］"}
            {"type" "image" "span" {"start" 10 "end" 11} "src" "fig.png" "alt" "ALT"}
            {"type" "caption" "span" {"start" 11 "end" 12} "text" "CAP"}
            {"type" "quote" "span" {"start" 12 "end" 13} "marker_type" "inline" "text" "Q"}
            {"type" "source-note" "span" {"start" 13 "end" 14}
             "text" "SRC" "note_type" "source-attribution" "placement" "back"
             "classification" "direct" "source_pointer" "blocks[3]"}]
   "warnings" []
   "errors" []})

(def omitted-metadata-parser-ir
  {"schema_id" "https://w3id.org/abc/schemas/parser-ir.schema.json"
   "schema_hash" "sha256:0000000000000000000000000000000000000000000000000000000000000001"
   "source" {"work_content_hash" "sha256:0000000000000000000000000000000000000000000000000000000000000002"
             "encoding" "Shift_JIS"
             "normalization" "source"}
   "nodes" [{"type" "indentation" "span" {"start" 0 "end" 1} "depth" 2 "text" ""}
            {"type" "image" "span" {"start" 1 "end" 2} "src" "fig.png"}
            {"type" "quote" "span" {"start" 2 "end" 3} "marker_type" "inline" "text" ""}]
   "warnings" []
   "errors" []})

(deftest render-string-test
  (testing "plaintext renders visible text policy for every current node type"
    (is (= "\nH\nABX※［＃y］CD\n\nALTCAPQ"
           (plaintext/render-string all-node-parser-ir)))))

(deftest source-note-back-matter-is-separated-test
  (testing "front/back/body source notes are metadata and do not enter plaintext"
    (is (= {:text "Body"
            :node_counts {"text" 1 "source-note" 3}
            :omitted [{:type "source-note" :policy "omitted"}
                      {:type "source-note" :policy "omitted"}
                      {:type "source-note" :policy "omitted"}]}
           (plaintext/render
            {"nodes" [{"type" "text" "span" {"start" 0 "end" 4} "text" "Body"}
                      {"type" "source-note"
                       "span" {"start" 4 "end" 20}
                       "text" "入力者注"
                       "note_type" "transcriber-note"
                       "placement" "front"
                       "classification" "direct"
                       "source_pointer" "blocks[0]"}
                      {"type" "source-note"
                       "span" {"start" 20 "end" 32}
                       "text" "本文注"
                       "note_type" "bibliographic-note"
                       "placement" "body"
                       "classification" "direct"
                       "source_pointer" "blocks[1]"}
                      {"type" "source-note"
                       "span" {"start" 32 "end" 72}
                       "text" "（古伝説と、シルレルの詩から。）"
                       "note_type" "source-attribution"
                       "placement" "back"
                       "classification" "heuristic"
                       "source_pointer" "blocks[78]"}]})))))

(deftest paragraph-layout-does-not-enter-plaintext-test
  (testing "plaintext ignores paragraph layout metadata and emits content only"
    (is (= "台詞"
           (plaintext/render-string
            {"nodes" [{"type" "text"
                       "span" {"start" 0 "end" 6}
                       "text" "台詞"}]
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
                                      "source" "aat-style"}}]})))))

(deftest emphasis-inline-children-render-visible-plaintext-test
  (testing "plaintext uses inline children but omits ruby readings and metadata"
    (is (= "東京X内"
           (plaintext/render-string
            {"nodes" [{"type" "emphasis"
                       "span" {"start" 0 "end" 2 "coordinate_system" "decoded_utf8"}
                       "style" "bold"
                       "text" "fallback"
                       "inline_children" [{"type" "ruby"
                                           "span" {"start" 0 "end" 2 "coordinate_system" "decoded_utf8"}
                                           "ruby" {"base" "東京"
                                                   "reading" "とうきょう"
                                                   "scope" "explicit"}}
                                          {"type" "gaiji"
                                           "span" {"start" 2 "end" 3 "coordinate_system" "decoded_utf8"}
                                           "gaiji" {"raw_marker" "※［＃x］"
                                                    "unicode" "X"
                                                    "resolved" true}}
                                          {"type" "editor-note"
                                           "span" {"start" 3 "end" 4 "coordinate_system" "decoded_utf8"}
                                           "note" {"raw" "［＃注］"
                                                   "category" "misc"}}
                                          {"type" "emphasis"
                                           "span" {"start" 4 "end" 5 "coordinate_system" "decoded_utf8"}
                                           "style" "inner"
                                           "text" "内"}]}]})))))

(deftest emphasis-inline-children-gaiji-falls-back-to-visible-text-test
  (testing "plaintext does not surface raw gaiji markers when emphasis carries visible text"
    (is (= "G"
           (plaintext/render-string
            {"nodes" [{"type" "emphasis"
                       "span" {"start" 0 "end" 1 "coordinate_system" "decoded_utf8"}
                       "style" "bold"
                       "text" "G"
                       "inline_children" [{"type" "gaiji"
                                           "span" {"start" 0 "end" 1 "coordinate_system" "decoded_utf8"}
                                           "gaiji" {"raw_marker" "※［＃gaiji-G］"
                                                    "unicode" nil
                                                    "resolved" true}}]}]})))))

(deftest layout-span-plaintext-renders-visible-text-only-test
  (testing "layout-span metadata does not enter plaintext"
    (is (= "12大"
           (plaintext/render-string
            {"nodes" [{"type" "layout-span"
                       "span" {"start" 0 "end" 2 "coordinate_system" "decoded_utf8"}
                       "text" "fallback"
                       "inline_children" [{"type" "ruby"
                                           "span" {"start" 0 "end" 2 "coordinate_system" "decoded_utf8"}
                                           "ruby" {"base" "12"
                                                   "reading" "じゅうに"
                                                   "scope" "explicit"}}]
                       "layout" {"kind" "tcy"
                                 "source" "aat-inline"
                                 "marker" "縦中横"}}
                      {"type" "layout-span"
                       "span" {"start" 2 "end" 3 "coordinate_system" "decoded_utf8"}
                       "text" "大"
                       "layout" {"kind" "font-size"
                                 "source" "aat-inline"
                                 "size_type" "large"
                                 "level" 1}}]})))))

(deftest heading-inline-children-do-not-change-plaintext-test
  (testing "heading plaintext uses required visible text field"
    (is (= "\n東京\n"
           (plaintext/render-string
            {"nodes" [{"type" "heading"
                       "span" {"start" 0 "end" 2 "coordinate_system" "decoded_utf8"}
                       "text" "東京"
                       "level" 2
                       "inline_children" [{"type" "ruby"
                                           "span" {"start" 0 "end" 2 "coordinate_system" "decoded_utf8"}
                                           "ruby" {"base" "東京"
                                                   "reading" "とうきょう"
                                                   "scope" "explicit"}}]}]})))))

(deftest render-omits-empty-or-missing-policy-metadata-test
  (testing "plaintext omits policy-required metadata when node payload is empty or missing"
    (is (= {:text ""
            :node_counts {"indentation" 1 "image" 1 "quote" 1}
            :omitted [{:type "indentation" :policy "omitted"}
                      {:type "image" :policy "omitted"}
                      {:type "quote" :policy "omitted"}]}
           (plaintext/render omitted-metadata-parser-ir)))))

(deftest render-metadata-test
  (testing "render returns omitted node notes"
    (let [result (plaintext/render all-node-parser-ir)]
      (is (= "editor-note" (:type (first (:omitted result)))))
      (is (some #(= {:type "source-note" :policy "omitted"} %)
                (:omitted result)))
      (is (= {"heading" 1 "text" 1 "ruby" 1 "gaiji" 2 "editor-note" 1
              "emphasis" 1 "indentation" 1 "page-break" 1 "line-break" 1
              "image" 1 "caption" 1 "quote" 1 "source-note" 1}
             (:node_counts result))))))

(deftest coverage-test
  (testing "plaintext renderer covers current parser-IR schema node vocabulary"
    (is (empty? (vocab/coverage-errors
                 (vocab/node-types (files/read-json "schemas/parser-ir.schema.json"))
                 "plaintext"
                 plaintext/covered-node-types)))
    (is (= (policy/renderer-covered-node-types (policy/load-policy policy-path) "plaintext")
           plaintext/covered-node-types))))
