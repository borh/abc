(ns abc.sim.render
  "Projection of model states into the concrete upstream formats: CSV rows,
  quoted CSV text (charred), and post-ingest corpus directories. Dirty-data
  corruptions are applied here so the model stays well-formed."
  (:require [abc.sim.oracle :as oracle]
            [abc.tools.aozora-ingest :as ingest]
            [abc.tools.files :as files]
            [abc.tools.json :as json]
            [abc.tools.malli :as am]
            [charred.api :as charred]
            [clojure.java.io :as io]
            [clojure.string])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(def headers
  (vec (sort ["作品ID" "人物ID" "役割フラグ" "作品名" "作品名読み" "ソート用読み"
              "副題" "副題読み" "原題" "初出" "分類番号" "文字遣い種別"
              "作品著作権フラグ" "公開日" "最終更新日" "図書カードURL"
              "底本名1" "底本出版社名1" "底本名2" "底本出版社名2"
              "底本初版発行年1" "底本初版発行年2"
              "入力に使用した版1" "入力に使用した版2"
              "校正に使用した版1" "校正に使用した版2"
              "底本の親本名1" "底本の親本名2"
              "底本の親本出版社名1" "底本の親本出版社名2"
              "底本の親本初版発行年1" "底本の親本初版発行年2"
              "姓" "名" "姓読み" "名読み" "姓読みソート用" "名読みソート用"
              "姓ローマ字" "名ローマ字" "生年月日" "没年月日" "人物著作権フラグ"])))

(defn- flag [b] (if b "なし" "あり"))

(defn- person-cells [pid p]
  {"人物ID" pid
   "姓" (:family_name p) "名" (:given_name p)
   "姓読み" (:family_name_reading p) "名読み" (:given_name_reading p)
   "姓読みソート用" (:family_name_sort p) "名読みソート用" (:given_name_sort p)
   "姓ローマ字" (:family_name_romaji p) "名ローマ字" (:given_name_romaji p)
   "生年月日" (:date_of_birth p) "没年月日" (:date_of_death p)
   "人物著作権フラグ" (flag (:copyright_expired p))})

(defn- work-cells [wid w]
  {"作品ID" wid
   "作品名" (:title w) "作品名読み" (:title_reading w)
   "ソート用読み" (:sort_reading w)
   "分類番号" (:ndc w) "文字遣い種別" (:orthography w)
   "作品著作権フラグ" (flag (:copyright_expired w))
   "公開日" (:available w) "最終更新日" (:modified w)
   "図書カードURL" (str "https://www.aozora.gr.jp/cards/000001/card" wid ".html")
   "底本名1" (:edition_title w) "底本出版社名1" (:edition_publisher w)})

(defn model->rows
  "One row per work-contributor-role tuple of projection(model), all 43
  headers present (blank when inapplicable), sorted by [wid relation pid]."
  [m]
  (let [{:keys [persons works edges]} (oracle/projection m)]
    (vec (for [[[wid rel] pids] (sort edges)
               pid (sort pids)]
           (merge (zipmap headers (repeat ""))
                  (work-cells wid (get works wid))
                  (person-cells pid (get persons pid))
                  {"役割フラグ" rel})))))

(defn- csv-quote-cell [s]
  (if (or (nil? s) (= "" s))
    ""
    (let [s (str s)]
      (if (or (clojure.string/includes? s ",")
              (clojure.string/includes? s "\"")
              (clojure.string/includes? s "\n"))
        (str "\"" (clojure.string/replace s "\"" "\"\"") "\"")
        s))))

(defn- csv-row [cells]
  (clojure.string/join "," (map csv-quote-cell cells)))

(defn rows->csv
  "Quoted CSV text. Rows may be header-keyed maps or raw cell vectors
  (ragged corruptions)."
  ([rows] (rows->csv rows {}))
  ([rows {:keys [bom? header-cells] :or {bom? false}}]
   (let [hs (or header-cells headers)
         cells (map (fn [r] (if (vector? r) r (mapv #(get r % "") hs))) rows)
         csv-lines (cons (csv-row hs) (map csv-row cells))]
     (str (when bom? "﻿") (clojure.string/join "\n" csv-lines) "\n"))))

(defn- rows-for-wid [rows wid] (filter #(= wid (get % "作品ID")) rows))

(defn corrupt-rows
  "Apply render-layer corruptions. Cell corruptions rewrite the first row of
  the target work; :divergent-* corruptions require the work to have ≥ 2 rows
  and rewrite only the SECOND row's cell, creating cross-row divergence;
  :duplicate-row repeats the first row; ragged corruptions replace the first
  row with a raw cell vector (short: drops the last 3 cells; long: appends 2)."
  [rows corruptions]
  (reduce
   (fn [rows {:corrupt/keys [type] :keys [wid pid column value]}]
     (let [idxs (keep-indexed (fn [i r] (when (and (map? r) (= wid (get r "作品ID"))
                                                   (or (nil? pid) (= pid (get r "人物ID"))))
                                          i))
                              rows)
           i0 (first idxs)
           i1 (second idxs)]
       (if (nil? i0)
         rows
         (case type
           :cell (update rows i0 assoc column value)
           :divergent-person (if i1 (update rows i1 assoc column value) rows)
           :divergent-work-fields (if i1 (update rows i1 assoc column value) rows)
           :duplicate-row (conj rows (nth rows i0))
           :ragged-short (assoc rows i0 (vec (drop-last 3 (mapv #(get (nth rows i0) % "") headers))))
           :ragged-long (assoc rows i0 (conj (mapv #(get (nth rows i0) % "") headers) "x" "y"))
           rows))))
   (vec rows)
   corruptions))

(defn temp-dir [prefix]
  (.toFile (Files/createTempDirectory prefix (make-array FileAttribute 0))))

(defn delete-tree! [f] (files/delete-tree! (io/file f)))

(defn- person-record-json [pid p]
  {"person_record_schema_id" ingest/person-schema-id
   "person_record_schema_hash" (am/cached-schema-hash ingest/person-schema-path)
   "person_id" pid
   "family_name" (:family_name p) "given_name" (:given_name p)
   "family_name_reading" (:family_name_reading p)
   "given_name_reading" (:given_name_reading p)
   "family_name_sort" (:family_name_sort p) "given_name_sort" (:given_name_sort p)
   "family_name_romaji" (:family_name_romaji p)
   "given_name_romaji" (:given_name_romaji p)
   "date_of_birth" (:date_of_birth p) "date_of_death" (:date_of_death p)
   "person_copyright_expired" (:copyright_expired p)
   "external_links" []})

(defn write-corpus-dirs!
  "Render projection(model) as a post-ingest corpus root (persons/ +
  works/), the input contract of person-drift-history/report."
  [root m]
  (let [{:keys [persons works edges]} (oracle/projection m)
        persons-dir (io/file root "persons")
        works-dir (io/file root "works")]
    (.mkdirs persons-dir)
    (.mkdirs works-dir)
    (doseq [[pid p] persons]
      (json/write-deterministic-json-file!
       (io/file persons-dir (str pid ".json")) (person-record-json pid p)))
    (doseq [[wid w] works]
      (json/write-deterministic-json-file!
       (io/file works-dir (str wid ".json"))
       {"work" {"work_id" wid "title" (:title w)}
        "contributors" (vec (for [[[ewid rel] pids] (sort edges)
                                  :when (= ewid wid)
                                  pid (sort pids)]
                              {"person_id" pid "relation_to_work" rel}))}))))
