(ns soranoha.ori.base-text-variant-test
  (:require [babashka.fs :as fs]
            [charred.api :as json]
            [clojure.test :refer [deftest is]]
            [soranoha.annotations.view :as view]
            [soranoha.core.hash :as hash]
            [soranoha.ori.projection :as projection]
            [soranoha.ori.render :as render]
            [soranoha.ori.validate :as validation])
  (:import [org.w3c.dom Document Element]))

(deftest supplied-reading-and-base-text-alternative-remain-distinct
  (let [span {"coordinate_system" "decoded_utf8" "start" 21 "end" 75}
        variant {"type" "base-text-variant" "text" "ざる"
                 "variant" {"base_text" "さる" "base_children" [{"type" "text" "text" "さる"}]}
                 "inline_children" [{"type" "text" "text" "ざる"}]
                 "source_span" span}
        ir {"nodes" [{"type" "text" "text" "私は"}
                     {"type" "ruby" "ruby" {"base" "籠" "reading" "ざる"}
                      "reading_children" [variant]}
                     {"type" "text" "text" "をさげ"}]}
        result (render/render-work {:parser-ir ir
                                    :metadata-record {"work" {"title" "試験" "work_id" "1" "aozora_modified" "2026-09-07"} "contributors" []}
                                    :persons-by-id {}})
        reading (view/from-tei (:tei result))
        ^Document doc (:view/document reading)
        ^Element app (.item (.getElementsByTagNameNS doc view/tei-namespace "app") 0)
        lemma (.item (.getElementsByTagNameNS doc view/tei-namespace "lem") 0)
        rdg (.item (.getElementsByTagNameNS doc view/tei-namespace "rdg") 0)]
    (is (= "私は籠をさげ" (:view/text reading)))
    (is (= "ざる" (.getTextContent lemma)))
    (is (= "さる" (.getTextContent rdg)))
    (is (= (str "#source-" (get span "start") "-" (get span "end")) (.getAttribute app "source")))
    (is (zero? (.getLength (.getElementsByTagNameNS doc view/tei-namespace "sic"))))
    (is (= "私は<ruby><rb>籠</rb><rt>ざる</rt></ruby>をさげ" (projection/markdown reading)))
    (is (some #(= {"family" "alternative-reading" "disposition" "omitted" "count" 1} %)
              (get (projection/report :projection/markdown reading) "counts")))
    (let [dir (fs/create-temp-dir {:prefix "base-text-variant"})
          path (str (fs/path dir "tei.xml"))]
      (try
        (spit path (:tei result))
        (let [validated (validation/tei-validation-result (validation/profile-paths ".") path)]
          (is (= "passed" (get validated "status")) (pr-str (get validated "findings"))))
        (finally (fs/delete-tree dir))))))

(deftest explicit-source-correspondence-survives-without-parser-offset-fallback
  (let [source-hash (hash/format-sha256 (hash/sha256-string "original source"))
        source-span {"coordinate_system" "decoded_utf8" "start" 3 "end" 12}
        supplied {"type" "text" "text" "本文" "source_span" source-span}
        ir {"source" {"primary_text_hash" source-hash}
            "nodes" [supplied (assoc supplied "text" "続き")
                     {"type" "text" "text" "未知" "span" {"coordinate_system" "parser_text_utf8" "start" 12 "end" 18}}]}
        render-ir #(render/render-work {:parser-ir %
                                        :metadata-record {"work" {"title" "試験" "work_id" "1"} "contributors" []}
                                        :persons-by-id {}})
        output (render-ir ir)
        ^Document doc (view/read-document (:tei output))
        segments (.getElementsByTagNameNS doc view/tei-namespace "seg")
        notes (.getElementsByTagNameNS doc view/tei-namespace "note")
        source-notes (filter (fn [^Element note] (= "source-span" (.getAttribute note "type")))
                             (map #(.item notes %) (range (.getLength notes))))
        ^Element note (first source-notes)]
    (is (= "本文続き未知" (:view/text (view/from-tei (:tei output)))))
    (is (= 2 (.getLength segments)))
    (is (= 1 (count source-notes)))
    (is (= source-span (json/read-json (.getTextContent note))))
    (is (= (str "urn:" source-hash) (.getAttribute note "corresp")))
    (is (= (str "#" (.getAttributeNS note view/xml-namespace "id"))
           (.getAttribute ^Element (.item segments 0) "source")
           (.getAttribute ^Element (.item segments 1) "source")))
    (is (thrown? clojure.lang.ExceptionInfo
                 (render-ir (assoc-in ir ["nodes" 1 "source_span" "line"] 2))))
    (is (thrown? clojure.lang.ExceptionInfo
                 (render-ir (assoc-in ir ["nodes" 0 "source_span" "coordinate_system"] "parser_text_utf8"))))))

(deftest warichu-retains-only-source-established-divisions
  (doseq [[content expected-halves] [[{"inline_children" [{"type" "text" "text" "上下"}]} 0]
                                     [{"upper_children" [{"type" "text" "text" "上"}]
                                       "lower_children" [{"type" "text" "text" "下"}]} 2]]]
    (let [result (render/render-work
                  {:parser-ir {"nodes" [(merge {"type" "warichu" "text" "上下"} content)]}
                   :metadata-record {"work" {"title" "試験" "work_id" "1"} "contributors" []}
                   :persons-by-id {}})
          reading (view/from-tei (:tei result))
          ^Document doc (:view/document reading)
          ^Element wrapper (.item (.getElementsByTagNameNS doc view/tei-namespace "seg") 0)]
      (is (= "上下" (:view/text reading)))
      (is (= "上下" (projection/markdown reading)))
      (is (= "warichu" (.getAttribute wrapper "type")))
      (is (= expected-halves (.getLength (.getElementsByTagNameNS wrapper view/tei-namespace "seg"))))
      (is (some #(= "layout" (get % "family")) (get (projection/report :projection/markdown reading) "counts"))))))

(deftest witness-ruby-is-apparatus-with-its-own-source-reference
  (let [span {"coordinate_system" "decoded_utf8" "start" 60 "end" 84}
        result (render/render-work
                {:parser-ir {"nodes" [{"type" "base-text-variant" "text" "狼狽てて"
                                       "inline_children" [{"type" "ruby" "ruby" {"base" "狼狽" "reading" "あわ"}}
                                                          {"type" "text" "text" "てて"}]
                                       "variant" {"base_text" "狼狙てて"
                                                  "base_children" [{"type" "ruby" "ruby" {"base" "狼狙" "reading" "あわ"}
                                                                    "source_span" span}
                                                                   {"type" "text" "text" "てて"}]}}]}
                 :metadata-record {"work" {"title" "試験" "work_id" "1" "aozora_modified" "2026-09-07"} "contributors" []}
                 :persons-by-id {}})
        reading (view/from-tei (:tei result))
        ^Document doc (:view/document reading)
        ^Element rdg (.item (.getElementsByTagNameNS doc view/tei-namespace "rdg") 0)
        ^Element ruby (.item (.getElementsByTagNameNS rdg view/tei-namespace "ruby") 0)]
    (is (= "狼狽てて" (:view/text reading)))
    (is (= "<ruby><rb>狼狽</rb><rt>あわ</rt></ruby>てて" (projection/markdown reading)))
    (is (= "狼狙" (.getTextContent (.item (.getElementsByTagNameNS ruby view/tei-namespace "rb") 0))))
    (is (= "あわ" (.getTextContent (.item (.getElementsByTagNameNS ruby view/tei-namespace "rt") 0))))
    (is (= "#source-60-84" (.getAttribute ruby "source")))
    (let [dir (fs/create-temp-dir {:prefix "rich-witness"})
          path (str (fs/path dir "tei.xml"))]
      (try
        (spit path (:tei result))
        (let [validated (validation/tei-validation-result (validation/profile-paths ".") path)]
          (is (= "passed" (get validated "status")) (pr-str (get validated "findings"))))
        (finally (fs/delete-tree dir))))))
