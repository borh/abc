(ns soranoha.za.reading-test
  "The reading view's contract is that it is a faithful projection: it must
  select the same text the published plaintext selects, show the encoding
  facts a reader would otherwise have to read TEI source to find, and keep
  the parser's own audit record out of the story.

  Every fixture here is real published TEI, rendered by the same
  `soranoha.ori.render` path that produces the artifact a manifest names,
  so a change in what is published cannot pass these tests by leaving the
  reading view untouched."
  (:require [clojure.string :as string]
            [clojure.test :refer [deftest is testing]]
            [soranoha.annotations.view :as view]
            [soranoha.ori.fixture :as fixture]
            [soranoha.ori.projection :as projection]
            [soranoha.ori.render :as render]
            [soranoha.za.html :as html]
            [soranoha.za.reading :as reading]))

(defn- tei [parser-ir]
  (:tei (render/render-work
         {:rights @fixture/grant
          :parser-ir parser-ir
          :metadata-record {"work" {"title" "試験"} "contributors" []}
          :persons-by-id {}})))

(defn- markup
  "A rendered section as one HTML string, which is what a reader receives.
  `html/render` reads a vector as an element, so a list of sibling nodes is
  handed to it as a sequence, exactly as the browse layer does."
  [nodes]
  (html/render (seq nodes)))

(defn- read-html [parser-ir]
  (markup (:body (reading/render (tei parser-ir)))))

(defn- node-text [node]
  (cond
    (string? node) node
    (vector? node) (apply str (map node-text (rest node)))
    (sequential? node) (apply str (map node-text node))
    :else ""))

(defn- text-of
  "Visible text of a list of sibling nodes: attribute maps are not text, so
  what this returns is what a reader sees rather than what the markup says."
  [nodes]
  (apply str (map node-text nodes)))

(deftest ruby-renders-above-its-base-characters-test
  (let [source (tei {"nodes" [{"type" "text" "text" "深い"}
                              {"type" "ruby"
                               "ruby" {"base" "池" "reading" "いけ" "scope" "explicit"
                                       "direction" "right"}}]})
        rendered (markup (:body (reading/render source)))]
    (testing "a furigana ruby becomes an HTML ruby, not a parenthesised gloss"
      (is (string/includes? rendered "<ruby"))
      (is (string/includes? rendered "<rt>いけ</rt>"))
      (is (not (string/includes? rendered "（いけ）"))))
    (testing "and the base character stays in the reading, ahead of its reading"
      (is (< (string/index-of rendered "池") (string/index-of rendered "いけ"))))))

(deftest gaiji-shows-its-mapped-character-and-keeps-the-source-marker-test
  (let [mapped (read-html
                {"nodes" [{"type" "gaiji"
                           "span" {"start" 0 "end" 1}
                           "gaiji" {"reference" "sample" "unicode" "騫"
                                    "raw_marker" "※［＃「馬＋寒」、第3水準1-94-32］"}}]})]
    (testing "the mapped character is what the reader reads"
      (is (string/includes? mapped "騫")))
    (testing "and the Aozora marker stays available on the same element"
      (is (string/includes? mapped "第3水準1-94-32"))))

  (testing "with no mapping there is a visible placeholder naming the marker"
    (let [unmapped (read-html
                    {"nodes" [{"type" "gaiji"
                               "span" {"start" 0 "end" 1}
                               "gaiji" {"reference" "unmapped"
                                        "raw_marker" "※［＃「熾」の火が金］"}}]})]
      (is (string/includes? unmapped "gaiji-unmapped"))
      (is (string/includes? unmapped "〔※［＃「熾」の火が金］〕")))))

(deftest a-source-reference-becomes-a-link-only-where-a-link-is-safe-test
  (testing "an external table reference names its companion file as text"
    ;; This is the target the corpus actually holds: an external table
    ;; reference carries the Aozora Bunko companion filename, which this site
    ;; does not serve, so an anchor would resolve to nothing.
    (let [rendered (read-html
                    {"nodes" [{"type" "editor-note" "note_kind" "external-table-reference"
                               "span" {"start" 0 "end" 1}
                               "source_filename" "densyanokonzatsu_table.txt"
                               "text" "表"}]})]
      (is (string/includes? rendered "densyanokonzatsu_table.txt"))
      (is (not (string/includes? rendered "<a")))))

  (testing "a target carrying an executable scheme reaches no href"
    ;; @target is transcriber-controlled source content, so the value is not
    ;; the publisher's. The chain signature and the TEI profile stand in front
    ;; of this; the allowlist is what stands behind them.
    (doseq [target ["javascript:alert(1)"
                    "JavaScript:alert(1)"
                    "data:text/html,<script>alert(1)</script>"
                    "vbscript:msgbox(1)"]]
      (let [rendered (read-html
                      {"nodes" [{"type" "editor-note" "note_kind" "external-table-reference"
                                 "span" {"start" 0 "end" 1}
                                 "source_filename" target
                                 "text" "表"}]})]
        (is (not (string/includes? rendered "href")) target))))

  (testing "an http or https target still links"
    (doseq [target ["https://example.org/table" "http://example.org/table"]]
      (let [rendered (read-html
                      {"nodes" [{"type" "editor-note" "note_kind" "external-table-reference"
                                 "span" {"start" 0 "end" 1}
                                 "source_filename" target
                                 "text" "表"}]})]
        (is (string/includes? rendered (str "href=\"" target "\"")) target)))))

(deftest an-emphasis-mark-reaches-the-reader-by-shape-or-by-name-test
  (testing "a mark this stylesheet can draw becomes a class"
    (let [rendered (read-html
                    {"nodes" [{"type" "emphasis" "text" "大事"
                               "style" "bouten"
                               "decoration" {"kind" "傍点" "position" "right"}}]})]
      (is (string/includes? rendered "rend-dots"))
      (is (string/includes? rendered "rend-bouten"))
      (is (string/includes? rendered "大事"))))

  (testing "and one it cannot is named instead of being silently dropped"
    (let [rendered (read-html
                    {"nodes" [{"type" "emphasis" "text" "珍しい"
                               "style" "bouten"
                               "decoration" {"kind" "四角傍点" "position" "right"}}]})]
      (is (string/includes? rendered "四角傍点")
          "an unknown mark is stated in the element's title")
      (is (string/includes? rendered "珍しい")))))

(deftest source-indentation-is-rendered-as-a-measured-indent-test
  (let [rendered (read-html
                  {"nodes" [{"type" "text" "text" "字下げ"}]
                   "paragraphs" [{"role" "body"
                                  "node_range" {"start" 0 "end" 1}
                                  "layout" {"kind" "jisage" "indent" 3}}]})]
    (is (string/includes? rendered "padding-inline-start:3em")
        "the source counts characters and a character is one em in either direction")
    (is (string/includes? rendered "layout-jisage"))))

(deftest the-reading-selects-the-same-text-the-plaintext-projection-selects-test
  (testing "an apparatus shows its one lemma and keeps the rejected witness"
    (let [source (tei {"nodes" [{"type" "base-text-variant"
                                 "text" "直した"
                                 "variant" {"base_text" "もとの"
                                            "base_children" [{"type" "text" "text" "もとの"}]}}]})
          {:keys [body]} (reading/render source)]
      (is (= "直した" (text-of body)))
      (is (= (projection/plaintext (view/from-tei source)) (text-of body))
          "the reading and the published plaintext cannot disagree about the text")
      (is (string/includes? (markup body) "もとの")
          "the witness the editor set aside stays available on the element")))

  (testing "a choice shows the branch the reading policy prefers"
    (let [source (tei {"nodes" [{"type" "iteration-mark"
                                 "span" {"start" 0 "end" 3 "coordinate_system" "decoded_utf8"}
                                 "annotation_span" {"start" 3 "end" 6 "coordinate_system" "decoded_utf8"}
                                 "source" "〳〵"
                                 "text" "ところ"}]})
          {:keys [body]} (reading/render source)]
      (is (= "ところ" (text-of body)))
      (is (= (projection/plaintext (view/from-tei source)) (text-of body)))
      (is (string/includes? (markup body) "〳〵")
          "the source's own realization stays available on the element"))))

(deftest the-parsers-audit-record-stays-out-of-the-story-test
  (let [source (tei {"source" {"work_content_hash" (str "sha256:" (apply str (repeat 64 "a")))}
                     "derived_from" {"parse_complete" false}
                     "nodes" [{"type" "text" "text" "本文"}]
                     "errors" [{"code" "unclosed_inline" "severity" "error"
                                "message" "閉じられていない"
                                "span" {"start" 0 "end" 2 "coordinate_system" "decoded_utf8"}}]})
        {:keys [body back]} (reading/render source)
        rendered (str (markup body) (markup back))]
    (is (string/includes? (markup body) "本文"))
    (doseq [leaked ["unclosed_inline" "閉じられていない" "parser-completion"
                    "decoded_utf8" "interpretation-problem"]]
      (is (not (string/includes? rendered leaked))
          (str leaked " belongs to the validation report, not to the reading")))))

(deftest the-colophon-is-kept-because-a-reader-cannot-reconstruct-it-test
  (let [{:keys [back body]} (reading/render
                             (tei {"nodes" [{"type" "text" "text" "本文"}
                                            {"type" "source-note"
                                             "placement" "back"
                                             "note_type" "colophon"
                                             "text" "底本：「芥川龍之介全集　第三巻」筑摩書房"}]}))]
    (is (some? back))
    (is (string/includes? (text-of back) "筑摩書房"))
    (is (not (string/includes? (text-of body) "筑摩書房"))
        "the colophon is bibliography, not the work")))

(deftest markup-in-the-source-text-reaches-the-page-as-text-test
  (let [rendered (read-html {"nodes" [{"type" "text" "text" "<script>alert(1)</script>"}]})]
    (is (string/includes? rendered "&lt;script&gt;"))
    (is (not (string/includes? rendered "<script>")))))

(deftest the-same-tei-renders-the-same-page-test
  (let [source (tei {"nodes" [{"type" "text" "text" "前"}
                              {"type" "ruby" "ruby" {"base" "池" "reading" "いけ"
                                                     "scope" "explicit"}}
                              {"type" "text" "text" "後"}]})]
    (is (= (markup (:body (reading/render source)))
           (markup (:body (reading/render source))))
        "generation must be a pure function of the release for the exporter's
         reuse check to hold over these pages")))
