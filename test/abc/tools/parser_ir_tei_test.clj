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
          [text-node [body-node & body-children]] (:body result)]
      (is (= :text text-node))
      (is (= :body body-node))
      (is (= :head (ffirst body-children)))
      (is (some #(= :p (first %)) body-children))
      (is (seq (:char_declarations result)))
      (is (= {"heading" 1 "text" 1 "ruby" 1 "gaiji" 2 "editor-note" 1
              "emphasis" 1 "indentation" 1 "page-break" 1 "image" 1
              "caption" 1 "quote" 1}
             (:node_counts result))))))

(deftest gaiji-reference-declaration-contract-test
  (testing "fixture gaiji.reference is preserved as ref and charDecl id"
    (let [result (parser-ir-tei/render
                  (files/read-json "examples/v0/example-work/parser-ir.json"))]
      (is (some #(= "example-gaiji" (:xml-id %))
                (:char_declarations result)))
      (is (some #(= [:g {:ref "#example-gaiji"}] %)
                (hiccup-nodes (:body result)))))))

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
