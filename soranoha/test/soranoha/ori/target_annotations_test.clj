(ns soranoha.ori.target-annotations-test
  (:require [babashka.fs :as fs]
            [clojure.test :refer [deftest is]]
            [soranoha.annotations.view :as view]
            [soranoha.ori.projection :as projection]
            [soranoha.ori.render :as render]
            [soranoha.ori.validate :as validation])
  (:import [org.w3c.dom Document Element]))

(deftest associated-notes-preserve-rich-apparatus-and-only-supplied-placement
  (doseq [position [nil "left" "right"]]
    (let [marker {"coordinate_system" "decoded_utf8" "start" 21 "end" 99}
          node (cond-> {"type" "annotated-text" "text" "漢" "note_kind" "gloss"
                        "source_span" marker
                        "inline_children" [{"type" "ruby" "ruby" {"base" "漢" "reading" "かん"}}]
                        "annotation_children" [{"type" "ruby" "ruby" {"base" "字" "reading" "じ"}}]}
                 position (assoc "position" position))
          result (render/render-work
                  {:parser-ir {"nodes" [node {"type" "text" "text" "後"}]}
                   :metadata-record {"work" {"title" "試験" "work_id" "1" "aozora_modified" "2026-09-07"} "contributors" []}
                   :persons-by-id {}})
          reading (view/from-tei (:tei result))
          ^Document doc (:view/document reading)
          ^Element wrapper (.item (.getElementsByTagNameNS doc view/tei-namespace "seg") 0)
          ^Element note (.item (.getElementsByTagNameNS wrapper view/tei-namespace "note") 0)]
      (is (= "漢後" (:view/text reading) (projection/plaintext reading)))
      (is (= "<ruby><rb>漢</rb><rt>かん</rt></ruby>後" (projection/markdown reading)))
      (is (= "annotated-text" (.getAttribute wrapper "type")))
      (is (= "#source-21-99" (.getAttribute wrapper "source")))
      (is (= (or position "") (.getAttribute note "place")))
      (is (= "字じ" (.getTextContent note)))
      (let [dir (fs/create-temp-dir {:prefix "target-annotation"})
            path (str (fs/path dir "tei.xml"))]
        (try
          (spit path (:tei result))
          (let [validated (validation/tei-validation-result (validation/profile-paths ".") path)]
            (is (= "passed" (get validated "status")) (pr-str (get validated "findings"))))
          (finally (fs/delete-tree dir)))))))
