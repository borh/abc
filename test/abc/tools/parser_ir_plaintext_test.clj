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
            {"type" "image" "span" {"start" 9 "end" 10} "src" "fig.png" "alt" "ALT"}
            {"type" "caption" "span" {"start" 10 "end" 11} "text" "CAP"}
            {"type" "quote" "span" {"start" 11 "end" 12} "marker_type" "inline" "text" "Q"}]
   "warnings" []
   "errors" []})

(deftest render-string-test
  (testing "plaintext renders visible text policy for every current node type"
    (is (= "\nH\nABX※［＃y］CD\nALTCAPQ"
           (plaintext/render-string all-node-parser-ir)))))

(deftest render-metadata-test
  (testing "render returns omitted node notes"
    (let [result (plaintext/render all-node-parser-ir)]
      (is (= "editor-note" (:type (first (:omitted result)))))
      (is (= {"heading" 1 "text" 1 "ruby" 1 "gaiji" 2 "editor-note" 1
              "emphasis" 1 "indentation" 1 "page-break" 1 "image" 1
              "caption" 1 "quote" 1}
             (:node_counts result))))))

(deftest coverage-test
  (testing "plaintext renderer covers current parser-IR schema node vocabulary"
    (is (empty? (vocab/coverage-errors
                 (vocab/node-types (files/read-json "schemas/parser-ir.schema.json"))
                 "plaintext"
                 plaintext/covered-node-types)))
    (is (= (policy/renderer-covered-node-types (policy/load-policy policy-path) "plaintext")
           plaintext/covered-node-types))))
