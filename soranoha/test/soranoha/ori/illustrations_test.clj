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
