(ns soranoha.ori.illustrations-test
  (:require [babashka.fs :as fs]
            [clojure.test :refer [deftest is]]
            [soranoha.annotations.view :as view]
            [soranoha.ori.projection :as projection]
            [soranoha.ori.fixture :as fixture]
            [soranoha.ori.render :as render]
            [soranoha.ori.validate :as validation])
  (:import [org.w3c.dom Document Element]))

(deftest image-description-is-apparatus-and-separate-caption-is-principal-text
  (let [result (render/render-work
                {:rights @fixture/grant
                 :parser-ir {"nodes" [{"type" "text" "text" "前"}
                                      {"type" "image" "src" "fig1.png" "alt" "図の説明"
                                       "width" 320 "height" 322 "number" "１"
                                       "description_children" [{"type" "ruby" "ruby" {"base" "図" "reading" "ず"}}]
                                       "caption_reference_children" [{"type" "ruby" "ruby" {"base" "漢" "reading" "かん"}}]}
                                      {"type" "text" "text" "後"}
                                      {"type" "caption" "text" "漢" "inline_children" [{"type" "ruby" "ruby" {"base" "漢" "reading" "かん"}}]}]}
                 :metadata-record {"work" {"title" "試験" "work_id" "1" "aozora_modified" "2026-09-07"} "contributors" []}
                 :persons-by-id {}})
        reading (view/from-tei (:tei result))
        ^Document doc (:view/document reading)
        ^Element graphic (.item (.getElementsByTagNameNS doc view/tei-namespace "graphic") 0)]
    (is (= "前後漢" (:view/text reading) (projection/plaintext reading)))
    (is (= "前後<ruby><rb>漢</rb><rt>かん</rt></ruby>" (projection/markdown reading)))
    (is (= "320px" (.getAttribute graphic "width")))
    (is (= "322px" (.getAttribute graphic "height")))
    (is (= 0 (.getLength (.getElementsByTagNameNS doc view/tei-namespace "figDesc"))))
    (let [dir (fs/create-temp-dir {:prefix "illustration"}) path (str (fs/path dir "tei.xml"))]
      (try
        (spit path (:tei result))
        (let [validated (validation/tei-validation-result (validation/profile-paths ".") path)]
          (is (= "passed" (get validated "status")) (pr-str (get validated "findings"))))
        (finally (fs/delete-tree dir))))))

(deftest figure-metadata-does-not-change-principal-view-identity
  (let [wrap #(str "<TEI xmlns=\"http://www.tei-c.org/ns/1.0\"><text><body><p>前" % "後</p></body></text></TEI>")
        plain (view/from-tei (wrap ""))
        image (view/from-tei (wrap "<figure><graphic url=\"fig.png\"/><figDesc>説明</figDesc></figure>"))]
    (is (= (:view/id plain) (:view/id image)))
    (is (= :view/body-v1 (:view/policy image)))))

(deftest bare-image-retains-the-accessibility-warning
  (let [tei (:tei (render/render-work
                   {:rights @fixture/grant
                    :parser-ir {"nodes" [{"type" "image" "src" "fig.png"}]}
                    :metadata-record {"work" {"title" "試験" "work_id" "1" "aozora_modified" "2026-09-07"} "contributors" []}
                    :persons-by-id {}}))
        dir (fs/create-temp-dir {:prefix "bare-illustration"})
        path (str (fs/path dir "tei.xml"))]
    (try
      (spit path tei)
      (let [validated (validation/tei-validation-result (validation/profile-paths ".") path)]
        (is (= "warning" (get validated "status")))
        (is (= ["snh-figure-accessibility"] (mapv #(get % "rule_id") (get validated "findings")))))
      (finally (fs/delete-tree dir)))))

(deftest figure-edition-statement-does-not-supply-an-image-description
  (let [tei (:tei (render/render-work
                   {:rights @fixture/grant
                    :parser-ir {"nodes" [{"type" "text" "text" "前"}
                                         {"type" "image" "src" "fig.png"
                                          "annotation_children" [{"type" "editor-note" "note_kind" "base-edition" "text" "底本では異なる寸法"}]}
                                         {"type" "text" "text" "後"}]}
                    :metadata-record {"work" {"title" "試験" "work_id" "1" "aozora_modified" "2026-09-07"} "contributors" []}
                    :persons-by-id {}}))
        reading (view/from-tei tei)
        ^Document doc (:view/document reading)
        ^Element figure (.item (.getElementsByTagNameNS doc view/tei-namespace "figure") 0)
        ^Element note (.item (.getElementsByTagNameNS figure view/tei-namespace "note") 0)
        dir (fs/create-temp-dir {:prefix "figure-edition"})
        path (str (fs/path dir "tei.xml"))]
    (is (= "前後" (:view/text reading) (projection/plaintext reading) (projection/markdown reading)))
    (is (= "base-edition" (.getAttribute note "type")))
    (is (= figure (.getParentNode note)))
    (is (= 0 (.getLength (.getElementsByTagNameNS figure view/tei-namespace "figDesc"))))
    (try
      (spit path tei)
      (let [validated (validation/tei-validation-result (validation/profile-paths ".") path)]
        (is (= "warning" (get validated "status")))
        (is (= ["snh-figure-accessibility"] (mapv #(get % "rule_id") (get validated "findings")))))
      (finally (fs/delete-tree dir)))))

(deftest unresolvable-asset-name-is-a-note-rather-than-a-graphic-url
  ;; Four markers across three works write a stem with no extension separator
  ;; where the filename belongs. The parser reports the text as `src_source`
  ;; with a null `src`, and the figure must not turn that into a url: the
  ;; source named no file, and a `graphic` would claim one.
  (let [result (render/render-work
                {:rights @fixture/grant
                 :parser-ir {"nodes" [{"type" "image" "src" nil
                                       "src_source" "fig45338_01png"
                                       "alt" "ひめだるまの写真"
                                       "width" 441 "height" 233}]}
                 :metadata-record {"work" {"title" "試験" "work_id" "1" "aozora_modified" "2026-09-07"} "contributors" []}
                 :persons-by-id {}})
        reading (view/from-tei (:tei result))
        ^Document doc (:view/document reading)]
    (is (= 0 (.getLength (.getElementsByTagNameNS doc view/tei-namespace "graphic"))))
    (is (= 1 (.getLength (.getElementsByTagNameNS doc view/tei-namespace "figure"))))
    (let [notes (.getElementsByTagNameNS doc view/tei-namespace "note")
          typed (into {} (for [i (range (.getLength notes))
                               :let [^Element note (.item notes i)]]
                           [(.getAttribute note "type") (.getTextContent note)]))]
      (is (= "fig45338_01png" (get typed "uninterpreted-image-source"))))
    (let [dir (fs/create-temp-dir {:prefix "unresolvable-asset"}) path (str (fs/path dir "tei.xml"))]
      (try
        (spit path (:tei result))
        (let [validated (validation/tei-validation-result (validation/profile-paths ".") path)]
          (is (= "passed" (get validated "status")) (pr-str (get validated "findings"))))
        (finally (fs/delete-tree dir))))))

(deftest an-unresolvable-asset-name-does-not-cost-the-work-its-analysis-eligibility
  ;; The uncertainty is about the figure's own metadata, which `body-v1` omits.
  ;; Reporting it as a content aspect would empty the eligible spans for the
  ;; whole work over a fact that is not in the transcribed text at all.
  (let [render (fn [aspects]
                 (view/from-tei
                  (:tei (render/render-work
                         {:rights @fixture/grant
                          :parser-ir {"nodes" [{"type" "text" "text" "本文"}
                                               {"type" "image" "src" nil
                                                "src_source" "fig45338_01png"}]
                                      "interpretation_problems"
                                      [{"kind" "uninterpreted-notation"
                                        "code" "uninterpreted-notation"
                                        "aspects" aspects
                                        "raw" "［＃ひめだるまの写真（fig45338_01png）入る］"
                                        "influence" {"kind" "document"}
                                        "source_span" {"start" 0 "end" 1 "coordinate_system" "decoded_utf8"}}]}
                          :metadata-record {"work" {"title" "試験" "work_id" "1" "aozora_modified" "2026-09-07"} "contributors" []}
                          :persons-by-id {}}))))]
    (is (seq (:view/eligible-spans (render ["structure"]))))
    (is (empty? (:view/eligible-spans (render ["content"]))))))

(deftest a-resolvable-asset-name-still-becomes-a-graphic-url
  (let [result (render/render-work
                {:rights @fixture/grant
                 :parser-ir {"nodes" [{"type" "image" "src" "fig45338_01.png"
                                       "alt" "ひめだるまの写真"
                                       "width" 441 "height" 233}]}
                 :metadata-record {"work" {"title" "試験" "work_id" "1" "aozora_modified" "2026-09-07"} "contributors" []}
                 :persons-by-id {}})
        reading (view/from-tei (:tei result))
        ^Document doc (:view/document reading)
        ^Element graphic (.item (.getElementsByTagNameNS doc view/tei-namespace "graphic") 0)]
    (is (= "fig45338_01.png" (.getAttribute graphic "url")))
    (is (= "441px" (.getAttribute graphic "width")))))
