(ns soranoha.tei-fidelity-test
  (:require [babashka.fs :as fs]
            [soranoha.ori.render :as render]
            [soranoha.ported.tei :as rng]
            [clojure.test :refer [deftest is]]
            [soranoha.ported.parser-ir-tei :as tei]))

(defn- elements [tree tag]
  (filter #(and (vector? %) (= tag (first %)))
          (tree-seq coll? seq tree)))

(deftest paragraph-ranges-do-not-discard-intervening-structure
  (let [rendered (tei/render
                  {"nodes" [{"type" "heading" "level" 2 "text" "一"}
                            {"type" "text" "text" "本文"}
                            {"type" "heading" "level" 2 "text" "二"}
                            {"type" "text" "text" "続き"}
                            {"type" "heading" "level" 2 "text" "三"}]
                   "paragraphs" [{"id" "p1" "role" "body" "node_range" {"start" 1 "end" 2}}
                                 {"id" "p2" "role" "body" "node_range" {"start" 3 "end" 4}}]})]
    (is (= ["一" "二" "三"] (map last (elements (:body rendered) :head))))
    (is (= 3 (get-in rendered [:node_counts "heading"])))))

(deftest layout-indentation-is-not-lexical-whitespace
  (let [rendered (tei/render
                  {"nodes" [{"type" "text" "text" "　最初。"}
                            {"type" "text" "text" "　続く　本文。"}
                            {"type" "source-note" "placement" "back"
                             "note_type" "source-attribution"
                             "text" "底本：書名　巻数\n　　　刊行日\n"}]
                   "paragraphs" [{"id" "p1" "role" "body" "node_range" {"start" 0 "end" 2}}]
                   "sentences" [{"id" "s1" "paragraph_id" "p1" "node_range" {"start" 0 "end" 1}}
                                {"id" "s2" "paragraph_id" "p1" "node_range" {"start" 1 "end" 2}}]})
        body (:body rendered)
        paragraph (first (elements body :p))
        note (first (elements body :note))]
    (is (= "text-indent: 1em" (:style (second paragraph))))
    (is (= ["最初。" "　続く　本文。"] (map last (elements paragraph :s))))
    (is (= [[:seg {:type "source-line"} "底本：書名　巻数"]
            [:seg {:type "source-line" :style "padding-inline-start: 3em"} "刊行日"]]
           (vec (elements note :seg))))
    (is (= 1 (count (elements note :lb))))))

(deftest indentation-follows-empty-text-and-apparatus
  (doseq [prefix [[{"type" "text" "text" ""}]
                  [{"type" "editor-note" "note" {"category" "correction" "raw" "注記"}}
                   {"type" "text" "text" ""}]]]
    (let [nodes (into prefix [{"type" "text" "text" "　本文　中。"}])
          body (:body (tei/render
                        {"nodes" nodes
                         "paragraphs" [{"id" "p1" "role" "body"
                                        "node_range" {"start" 0 "end" (count nodes)}}]}))
          paragraph (first (elements body :p))]
      (is (= "text-indent: 1em" (:style (second paragraph))))
      (is (= "本文　中。" (last paragraph)))
      (is (= (count (filter #(= "editor-note" (get % "type")) prefix))
             (count (elements body :note)))))))

(deftest visible-nodes-stop-paragraph-indentation-search
  (doseq [prefix [{"type" "text" "text" "前"}
                  {"type" "ruby" "ruby" {"base" "前" "reading" "まえ"}}
                  {"type" "emphasis" "style" "sesame-dot"
                   "inline_children" [{"type" "text" "text" "前"}]}]]
    (let [body (:body (tei/render
                        {"nodes" [prefix {"type" "text" "text" "　本文。"}]
                         "paragraphs" [{"id" "p1" "role" "body"
                                        "node_range" {"start" 0 "end" 2}}]}))
          paragraph (first (elements body :p))]
      (is (nil? (:style (second paragraph))))
      (is (= "　本文。" (last paragraph))))))

(deftest heading-indentation-is-preserved
  (let [body (:body (tei/render {"nodes" [{"type" "heading" "level" 2 "indent" 8 "text" "一"}]}))]
    (is (= {:n "2" :style "padding-inline-start: 8em"}
           (second (first (elements body :head)))))))

(deftest gaiji-remains-inside-ruby-base
  (let [rendered (tei/render
                  {"nodes" [{"type" "ruby"
                             "ruby" {"base" "犍陀多" "reading" "かんだた"}
                             "inline_children" [{"type" "gaiji"
                                                 "gaiji" {"reference" "1-87-71" "unicode" "犍"}}
                                                {"type" "text" "text" "陀多"}]}]})
        rb (first (elements (:body rendered) :rb))]
    (is (= [:rb [:g {:ref "#gaiji-1-87-71"} "犍"] "陀多"] rb))
    (is (= "犍" (:unicode (first (:char_declarations rendered)))))))

(deftest source-layout-validates-against-the-publication-profile
  (let [dir (fs/create-temp-dir {:prefix "tei-layout-profile"})
        xml-path (str (fs/path dir "tei.xml"))
        result (render/render-work
                {:parser-ir {"sentence_segmentation" {"coordinate_system" "parser_text_utf8"}
                             "nodes" [{"type" "heading" "level" 2 "indent" 8 "text" "一"}
                                      {"type" "text" "text" "　本文。"}
                                      {"type" "source-note" "placement" "back"
                                       "note_type" "source-attribution" "text" "底本：書名\n　　　刊行日\n"}]
                             "paragraphs" [{"id" "p1" "role" "body" "node_range" {"start" 1 "end" 2}}]}
                 :metadata-record {"work" {"title" "試験" "aozora_modified" "2026-09-06"} "contributors" []}
                 :persons-by-id {}})]
    (try
      (spit xml-path (:tei result))
      (is (empty? (:violations (rng/validate! {:schema-path "../abc/schemas/tei-profile.rng"
                                               :xml-path xml-path :label "source layout"}))))
      (finally (fs/delete-tree dir)))))
