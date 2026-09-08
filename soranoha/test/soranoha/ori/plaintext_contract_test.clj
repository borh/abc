(ns soranoha.ori.plaintext-contract-test
  (:require [soranoha.annotations.view :as view]
            [soranoha.ori.projection :as projection]
            [soranoha.ori.tei :as tei]
            [soranoha.ori.tei-header :as header]
            [clojure.string :as string]
            [clojure.test :refer [deftest is testing]]))

(defn render [parser-ir]
  {:text (projection/plaintext
          (view/from-tei
           (header/hiccup->pretty-xml-string [:TEI {:xmlns/snh "https://w3id.org/soranoha/ns/tei"}
                                              (:body (tei/render parser-ir))])))})

(deftest
  plaintext-is-visible-body-text-only-test
  (testing
   "ruby readings, apparatus, provenance, layout, and back matter remain outside plaintext"
    (let [parser-ir {"source" {"source_path" "cards/work.txt", "encoding" "Shift_JIS"},
                     "nodes"
                     [{"type" "ruby",
                       "span" {"start" 0, "end" 3},
                       "ruby" {"base" "猫", "reading" "ねこ", "scope" "explicit", "direction" "right"}}
                      {"type" "editor-note",
                       "span" {"start" 3, "end" 9},
                       "note" {"raw" "［＃注］", "category" "apparatus"}}
                      {"type" "layout-span",
                       "span" {"start" 9, "end" 12},
                       "text" "本文",
                       "layout" {"kind" "tcy", "marker" "縦中横"}}
                      {"type" "source-note",
                       "span" {"start" 12, "end" 20},
                       "text" "底本注",
                       "note_type" "source-attribution",
                       "placement" "back",
                       "classification" "direct",
                       "source_pointer" "blocks[9]"}]}
          text (:text (render parser-ir))]
      (is (= "猫本文" text))
      (doseq [excluded ["ねこ" "［＃注］" "cards/work.txt" "Shift_JIS" "tcy" "縦中横" "底本注" "blocks[9]"]]
        (is (not (string/includes? text excluded)) excluded)))))

(deftest
  source-note-back-matter-is-separated-test
  (testing
   "front/back/body source notes are metadata and do not enter plaintext"
    (is
     (=
      {:text "Body"}
      (render
       {"nodes"
        [{"type" "text", "span" {"start" 0, "end" 4}, "text" "Body"}
         {"type" "source-note",
          "span" {"start" 4, "end" 20},
          "text" "入力者注",
          "note_type" "transcriber-note",
          "placement" "front",
          "classification" "direct",
          "source_pointer" "blocks[0]"}
         {"type" "source-note",
          "span" {"start" 20, "end" 32},
          "text" "本文注",
          "note_type" "bibliographic-note",
          "placement" "body",
          "classification" "direct",
          "source_pointer" "blocks[1]"}
         {"type" "source-note",
          "span" {"start" 32, "end" 72},
          "text" "（古伝説と、シルレルの詩から。）",
          "note_type" "source-attribution",
          "placement" "back",
          "classification" "heuristic",
          "source_pointer" "blocks[78]"}]})))))

(deftest
  paragraph-layout-does-not-enter-plaintext-test
  (testing
   "plaintext ignores paragraph layout metadata and emits content only"
    (is
     (=
      "台詞"
      (:text
       (render
        {"nodes" [{"type" "text", "span" {"start" 0, "end" 6}, "text" "台詞"}],
         "paragraphs"
         [{"id" "p000000",
           "span" {"start" 0, "end" 6, "coordinate_system" "parser_text_utf8"},
           "node_range" {"start" 0, "end" 1},
           "role" "body",
           "source_pointer" "blocks[0]",
           "classification" "direct",
           "layout"
           {"kind" "burasage",
            "first_line_indent" 0,
            "continuation_indent" 1,
            "source" "aat-style"}}]}))))))

(deftest
  source-text-newline-runs-are-publication-normalized-test
  (testing
   "source/control-line separator whitespace does not leak as empty publication lines"
    (is
     (=
      "序\n本文"
      (:text
       (render
        {"nodes"
         [{"type" "text",
           "span" {"start" 0, "end" 20, "coordinate_system" "parser_text_utf8"},
           "text" "\r\n\r\n序\r\n\r\n\r\n本文\r\n\r\n"}]}))))))

(deftest
  emphasis-inline-children-render-visible-plaintext-test
  (testing
   "plaintext uses inline children but omits ruby readings and metadata"
    (is
     (=
      "東京X内"
      (:text
       (render
        {"nodes"
         [{"type" "emphasis",
           "span" {"start" 0, "end" 2, "coordinate_system" "parser_text_utf8"},
           "style" "bold",
           "text" "fallback",
           "inline_children"
           [{"type" "ruby",
             "span" {"start" 0, "end" 2, "coordinate_system" "parser_text_utf8"},
             "ruby" {"base" "東京", "reading" "とうきょう", "scope" "explicit"}}
            {"type" "gaiji",
             "span" {"start" 2, "end" 3, "coordinate_system" "parser_text_utf8"},
             "gaiji" {"raw_marker" "※［＃x］", "unicode" "X", "resolved" true}}
            {"type" "editor-note",
             "span" {"start" 3, "end" 4, "coordinate_system" "parser_text_utf8"},
             "note" {"raw" "［＃注］", "category" "misc"}}
            {"type" "emphasis",
             "span" {"start" 4, "end" 5, "coordinate_system" "parser_text_utf8"},
             "style" "inner",
             "text" "内"}]}]}))))))

(deftest
  unresolved-gaiji-does-not-guess-from-enclosing-text-test
  (testing
   "an unresolved TEI glyph keeps an explicit placeholder"
    (is
     (=
      "\uFFFC"
      (:text
       (render
        {"nodes"
         [{"type" "emphasis",
           "span" {"start" 0, "end" 1, "coordinate_system" "parser_text_utf8"},
           "style" "bold",
           "text" "G",
           "inline_children"
           [{"type" "gaiji",
             "span" {"start" 0, "end" 1, "coordinate_system" "parser_text_utf8"},
             "gaiji" {"raw_marker" "※［＃gaiji-G］", "unicode" nil, "resolved" true}}]}]}))))))

(deftest
  layout-span-plaintext-renders-visible-text-only-test
  (testing
   "layout-span metadata does not enter plaintext"
    (is
     (=
      "12大"
      (:text
       (render
        {"nodes"
         [{"type" "layout-span",
           "span" {"start" 0, "end" 2, "coordinate_system" "parser_text_utf8"},
           "text" "fallback",
           "inline_children"
           [{"type" "ruby",
             "span" {"start" 0, "end" 2, "coordinate_system" "parser_text_utf8"},
             "ruby" {"base" "12", "reading" "じゅうに", "scope" "explicit"}}],
           "layout" {"kind" "tcy", "source" "aat-inline", "marker" "縦中横"}}
          {"type" "layout-span",
           "span" {"start" 2, "end" 3, "coordinate_system" "parser_text_utf8"},
           "text" "大",
           "layout" {"kind" "font-size", "source" "aat-inline", "size_type" "large", "level" 1}}]}))))))

(deftest
  remaining-visible-node-payloads-and-missing-payloads
  (is
   (=
    "D\n\nCAPQ"
    (:text
     (render
      {"nodes"
       [{"type" "indentation", "depth" 2, "text" "D"}
        {"type" "page-break", "marker" "［＃改ページ］"}
        {"type" "line-break", "marker" "［＃改行］"}
        {"type" "image", "src" "fig.png", "alt" "ALT"}
        {"type" "caption", "text" "CAP"}
        {"type" "quote", "marker_type" "inline", "text" "Q"}]}))))
  (is
   (=
    ""
    (:text
     (render
      {"nodes"
       [{"type" "indentation", "depth" 2, "text" ""}
        {"type" "image", "src" "fig.png"}
        {"type" "quote", "marker_type" "inline", "text" ""}]})))))
