(ns soranoha.ori.iteration-conformance-test
  (:require [babashka.fs :as fs]
            [charred.api :as json]
            [clojure.string :as string]
            [clojure.test :refer [deftest is]]
            [soranoha.annotations.view :as view]
            [soranoha.core.hash :as hash]
            [soranoha.kura.cas :as cas]
            [soranoha.kura.engine :as engine]
            [soranoha.ori.accountability :as accountability]
            [soranoha.ori.projection :as projection]
            [soranoha.ori.render :as render]
            [soranoha.ori.stages :as stages]
            [soranoha.ori.validate :as validation])
  (:import [org.w3c.dom Document Element Node]))

(defn- read-output [store result output]
  (json/read-json (String. ^bytes (cas/get-bytes (:cas-dir store) (get-in result [:outputs output])) "UTF-8")))

(deftest supplied-iteration-marks-select-the-unicode-realization
  (let [dir (fs/create-temp-dir {:prefix "iteration-conformance"})
        store (engine/open-store! {:cas-dir (str (fs/path dir "objects"))
                                   :db-path (str (fs/path dir "trace.sqlite"))})
        adapter (stages/resolve-adapter)
        tool (accountability/resolve-tool)]
    (try
      (doseq [[body expected] [["フゴ／＼と、とき／″＼。" "フゴ〱と、とき〲。"]
                               ["｜とき／″＼《時々》" "とき〲"]
                               ["時々《とき／″＼》" "時々"]
                               ["「とき／″＼」" "「とき〲」"]
                               ["／゛＼〳〵／″" "／゛＼〳〵／″"]]]
        (let [source (str "題\n作者\n\n" body "\n\n底本：本\n")
              source-id (cas/put-bytes! (:cas-dir store) (.getBytes source "UTF-8"))
              parsed (engine/run-stage! store (stages/parse-stage adapter) {"source" source-id})
              converted (engine/run-stage! store (stages/convert-stage adapter)
                                           {"aat" (get-in parsed [:outputs "aat"])
                                            "work_content_hash" (hash/format-sha256 source-id)})
              ir (read-output store converted "parser-ir")
              oracle (engine/run-stage! store (accountability/source-stage tool) {"source" source-id})
              coverage (accountability/coverage-report (read-output store oracle "source-accountability") ir)
              tei (:tei (render/render-work {:parser-ir ir
                                             :metadata-record {"work" {"work_id" "1" "title" "題" "aozora_modified" "2026-09-07"}
                                                               "contributors" []}
                                             :persons-by-id {}}))
              reading (view/from-tei tei)
              ^Document document (:view/document reading)
              choices (.getElementsByTagNameNS document view/tei-namespace "choice")
              path (str (fs/path dir "text.tei.xml"))]
          (is (= expected (projection/plaintext reading)) body)
          (is (= expected (:view/text reading)) body)
          (is (not (string/includes? (projection/markdown reading) "／″＼")) body)
          (is (not (string/includes? (projection/markdown reading) "／＼")) body)
          (when (string/includes? body "／″＼")
            (is (string/includes? (projection/markdown reading) "〲") body))
          (is (empty? (get ir "interpretation_problems")) body)
          (is (every? #(empty? (get % "unaccounted_families")) (get coverage "occurrences")) coverage)
          (dotimes [index (.getLength choices)]
            (let [^Element choice (.item choices index)
                  ^Node original (.item (.getElementsByTagNameNS choice view/tei-namespace "orig") 0)
                  ^Node regular (.item (.getElementsByTagNameNS choice view/tei-namespace "reg") 0)]
              (is (string/starts-with? (.getAttribute choice "source") "#source-"))
              (is (#{["／＼" "〱"] ["／″＼" "〲"]} [(.getTextContent original) (.getTextContent regular)]))
              (is (every? #(not= original (.getParentNode ^Node (:view/node %))) (:view/segments reading)))))
          (spit path tei)
          (let [validation (validation/tei-validation-result (validation/profile-paths ".") path)]
            (is (= "passed" (get validation "status")) validation))))
      (finally (engine/close-store! store) (fs/delete-tree dir)))))
