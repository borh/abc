(ns soranoha.ori.supplied-diacritic-conformance-test
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
            [soranoha.ori.fixture :as fixture]
            [soranoha.ori.render :as render]
            [soranoha.ori.stages :as stages]
            [soranoha.ori.validate :as validation])
  (:import [org.w3c.dom Document Element]))

(defn- read-output [store result output]
  (json/read-json (String. ^bytes (cas/get-bytes (:cas-dir store) (get-in result [:outputs output])) "UTF-8")))

(deftest supplied-diacritics-realize-the-owned-target-once
  (let [dir (fs/create-temp-dir {:prefix "diacritic-conformance"})
        store (engine/open-store! {:cas-dir (str (fs/path dir "objects"))
                                   :db-path (str (fs/path dir "trace.sqlite"))})
        adapter (stages/resolve-adapter)]
    (try
      (doseq [[body expected expected-markdown] [["Venus［＃「e」はアクサン（´）付き］" "Vénus"]
                                                 ["〔ru_pam［＃mは上ドット付き］〕" "rūpaṁ"]
                                                 ["〔Mi_hr〕［＃hは下ドット付き］" "Mīḥr"]
                                                 ["〔samgha_disesa.v〕［＃mは上ドット付き］" "saṁghādisesa.v" "saṁghādisesa\\.v"]
                                                 ["〔Ritva_disu to dah〕［＃Rは下ドット付き。sは下ドット付き。hは下ドット付き］" "Ṛitvādiṣu to daḥ"]
                                                 ["Samgha《サングハ》［＃mは上ドット付き］" "Saṁgha" "<ruby><rb>Saṁgha</rb><rt>サングハ</rt></ruby>"]
                                                 ["〔rattha-sva_mi_〕［＃tはともに下ドット付き］" "raṭṭha-svāmī" "raṭṭha\\-svāmī"]
                                                 ["Konkana［＃前のnは上ドット付き、後のnは下ドット付き］" "Koṅkaṇa"]]]
        (let [source (str "題\n作者\n\n" body "\n\n底本：本\n")
              source-id (cas/put-bytes! (:cas-dir store) (.getBytes source "UTF-8"))
              parsed (engine/run-stage! store (stages/parse-stage adapter) {"source" source-id})
              converted (engine/run-stage! store (stages/convert-stage adapter)
                                           {"aat" (get-in parsed [:outputs "aat"])
                                            "work_content_hash" (hash/format-sha256 source-id)})
              ir (read-output store converted "parser-ir")
              oracle (engine/run-stage! store (accountability/source-stage (accountability/resolve-tool)) {"source" source-id})
              coverage (accountability/coverage-report (read-output store oracle "source-accountability") ir)
              tei (:tei (render/render-work {:rights @fixture/grant
                                             :parser-ir ir
                                             :metadata-record {"work" {"work_id" "1" "title" "題" "aozora_modified" "2026-09-07"}
                                                               "contributors" []}
                                             :persons-by-id {}}))
              reading (view/from-tei tei)
              path (str (fs/path dir "text.tei.xml"))]
          (is (= expected (projection/plaintext reading)) body)
          (is (= expected (:view/text reading)) body)
          (is (= (or expected-markdown expected) (string/trim (projection/markdown reading))) body)
          (is (empty? (get ir "interpretation_problems")) body)
          (is (every? #(not-any? #{"accent.dotted_letter" "glyph.variant_note"}
                                 (get % "unaccounted_families")) (get coverage "occurrences")) coverage)
          (let [^Document document (:view/document reading)]
            (is (= 1 (.getLength (.getElementsByTagNameNS document view/tei-namespace "orig"))))
            (is (= 1 (.getLength (.getElementsByTagNameNS document view/tei-namespace "reg"))))
            (let [^Element choice (.item (.getElementsByTagNameNS document view/tei-namespace "choice") 0)
                  nodes (get ir "nodes")
                  span (get (first (filter #(= "supplied-diacritic" (get % "type"))
                                           (concat nodes (mapcat #(get % "inline_children") nodes)))) "annotation_span")]
              (is (= (str "#source-" (get span "start") "-" (get span "end")) (.getAttribute choice "corresp")))))
          (spit path tei)
          (is (= "passed" (get (validation/tei-validation-result (validation/profile-paths ".") path) "status")))))
      (finally (engine/close-store! store) (fs/delete-tree dir)))))
