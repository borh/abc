(ns abc.sim.render
  "Projection of model states into the concrete upstream formats: CSV rows,
  quoted CSV text (charred), and post-ingest corpus directories. Dirty-data
  corruptions are applied here so the model stays well-formed."
  (:require [abc.git :as abc-git]
            [abc.sim.oracle :as oracle]
            [abc.tools.aozora-ingest :as ingest]
            [abc.tools.files :as files]
            [abc.tools.json :as json]
            [abc.tools.schema :as schema]
            [charred.api :as charred]
            [clojure.java.io :as io])
  (:import [java.io ByteArrayOutputStream]
           [java.nio.charset StandardCharsets]
           [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]
           [java.util.zip CRC32 ZipEntry ZipOutputStream]
           [org.eclipse.jgit.api Git]))

(def headers
  (vec (sort ["作品ID" "人物ID" "役割フラグ" "作品名" "作品名読み" "ソート用読み"
              "副題" "副題読み" "原題" "初出" "分類番号" "文字遣い種別"
              "作品著作権フラグ" "公開日" "最終更新日" "図書カードURL"
              "テキストファイルURL"
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

(defn content-zip-name [wid] (str wid "_t.zip"))

(defn- content-cells [m proj wid]
  (if (contains? (:contents m) wid)
    {"テキストファイルURL"
     (str "https://www.aozora.gr.jp/cards/" (oracle/card-pid proj wid)
          "/files/" (content-zip-name wid))}
    {}))

(defn model->rows
  "One row per work-contributor-role tuple of projection(model), all 44
  headers present (blank when inapplicable), sorted by [wid relation pid]."
  [m]
  (let [{:keys [persons works edges] :as proj} (oracle/projection m)]
    (vec (for [[[wid rel] pids] (sort edges)
               pid (sort pids)]
           (merge (zipmap headers (repeat ""))
                  (work-cells wid (get works wid))
                  (person-cells pid (get persons pid))
                  (content-cells m proj wid)
                  {"役割フラグ" rel})))))

(defn rows->csv
  "Quoted CSV text. Rows may be header-keyed maps or raw cell vectors
  (ragged corruptions)."
  ([rows] (rows->csv rows {}))
  ([rows {:keys [bom? header-cells] :or {bom? false}}]
   (let [hs (or header-cells headers)
         cells (cons hs (map (fn [r] (if (vector? r) r (mapv #(get r % "") hs))) rows))
         sw (java.io.StringWriter.)
         result (do (charred/write-csv sw cells :close-writer? true)
                    (str sw))]
     (str (when bom? "﻿") result))))

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
   "person_record_schema_hash" (schema/cached-schema-hash ingest/person-schema-path)
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

(def zip-path "index_pages/list_person_all_extended_utf8.zip")

(defn csv->zip-bytes
  ([csv] (csv->zip-bytes csv {}))
  ([csv {:keys [entry-name no-entry?]
         :or {entry-name "list_person_all_extended_utf8.csv"}}]
   (let [out (ByteArrayOutputStream.)
         ;; deterministic entry mtime: rendered bytes must be a pure function
         ;; of the CSV text (P11.no-diff-invisible, P14 rely on this)
         entry (fn [^String n] (doto (ZipEntry. n) (.setTime 0)))]
     (with-open [zip (ZipOutputStream. out)]
       (if no-entry?
         (do (.putNextEntry zip (entry "README.txt"))
             (.write zip (.getBytes "no csv here" StandardCharsets/UTF_8)))
         (do (.putNextEntry zip (entry entry-name))
             (.write zip (.getBytes ^String csv StandardCharsets/UTF_8))))
       (.closeEntry zip))
     (.toByteArray out))))

(defn- rendered-members
  "Render-side member projection. Kept separate from the oracle's projection
  so a mistake here can be detected by identity properties."
  [{:keys [text images]} wid]
  (into (sorted-map (str wid ".txt") (.getBytes ^String text StandardCharsets/UTF_8))
        (map (fn [[path content]]
               [path (.getBytes ^String content StandardCharsets/UTF_8)]))
        images))

(defn- ordered-members [members order]
  (case order
    :reverse (reverse members)
    :sorted members
    (throw (ex-info "unsupported simulated ZIP member order"
                    {:order order :supported [:sorted :reverse]}))))

(defn content->zip-bytes
  "ZIP bytes for a work's text and image members. Layout controls change
  packaging only: :order (:sorted/:reverse), :mtime, :comment,
  and :compression (:deflated/:stored)."
  ([content wid] (content->zip-bytes content wid {}))
  ([content wid {:keys [order mtime comment compression]
                 :or {order :sorted mtime 0 compression :deflated}}]
   (let [out (ByteArrayOutputStream.)
         members (rendered-members content wid)]
     (with-open [zip (ZipOutputStream. out)]
       (when comment (.setComment zip comment))
       (doseq [[path ^bytes member-bytes] (ordered-members members order)]
         (let [entry (doto (ZipEntry. ^String path) (.setTime (long mtime)))]
           (when (= :stored compression)
             (let [crc (doto (CRC32.) (.update member-bytes))]
               (.setMethod entry ZipEntry/STORED)
               (.setSize entry (alength member-bytes))
               (.setCompressedSize entry (alength member-bytes))
               (.setCrc entry (.getValue crc))))
           (.putNextEntry zip entry)
           (.write zip member-bytes)
           (.closeEntry zip)))
       (.finish zip))
     (.toByteArray out))))

(defn text->zip-bytes
  "Compatibility helper for a deterministic single-text-member bundle."
  [text wid]
  (content->zip-bytes {:text text :images (sorted-map)} wid))

(defn content-sources
  "Per projected content-bearing work: card dir, basename, relpath, and the
  sha256 pin of the exact ZIP bytes write-aozora-root! writes. Single source
  of truth for paths and hashes on the oracle side."
  ([m] (content-sources m {}))
  ([m zip-layouts]
   (let [proj (oracle/projection m)]
     (into (sorted-map)
           (for [[wid content] (:contents m)
                 :when (contains? (:works proj) wid)]
             (let [cp (oracle/card-pid proj wid)
                   archive-bytes (content->zip-bytes content wid
                                                     (get zip-layouts wid {}))
                   identity (oracle/expected-content-identity m wid archive-bytes)]
               [wid (merge {:card-pid cp
                            :basename (content-zip-name wid)
                            :relpath (str "cards/" cp "/files/"
                                          (content-zip-name wid))
                            :archive-bytes archive-bytes}
                           (select-keys identity
                                        [:archive-hash :bundle-hash
                                         :primary-text-hash
                                         :primary-text-member :members
                                         :identity-object]))]))))))

(defn- write-render-bytes! [dir relpath bytes]
  (let [file (io/file dir relpath)]
    (files/create-parent-dirs! file)
    (files/write-bytes! file bytes)))

(defn write-aozora-root!
  "Render a model state as a plain aozora-root: catalog ZIP, per-work
  content ZIPs (from content-sources, so paths/hashes agree with the
  oracle), two deterministic rejection decoys, and a fake .git/HEAD (git
  provenance is best-effort in the SUT)."
  ([dir m] (write-aozora-root! dir m {}))
  ([dir m {:keys [zip-layouts] :or {zip-layouts {}}}]
   (do
     (write-render-bytes! dir zip-path (csv->zip-bytes (rows->csv (model->rows m))))
     (doseq [[_wid {:keys [relpath archive-bytes]}] (content-sources m zip-layouts)]
       (write-render-bytes! dir relpath archive-bytes))
     (write-render-bytes! dir "cards/999999/files/decoy.zip" (text->zip-bytes "decoy 999999" "999999"))
     (write-render-bytes! dir "support/tools.zip" (text->zip-bytes "tools 000000" "000000"))
     (let [head (io/file dir ".git/HEAD")]
       (files/create-parent-dirs! head)
       (files/write-text! head "sim-fixture-head\n")))))

(defn init-repo! [dir]
  (-> (Git/init) (.setDirectory (io/file (str dir))) .call))

(defn- commit-at! [git message instant-str]
  (abc-git/commit-at! git message instant-str))

(defn commit-file-at! [git root relpath content message instant-str]
  (let [f (io/file root relpath)]
    (files/create-parent-dirs! f)
    (if (bytes? content)
      (files/write-bytes! f content)
      (files/write-text! f content))
    (abc-git/add-file! git relpath)
    (commit-at! git message instant-str)))

(defn commit-zip-at! [git root zip-bytes message instant-str]
  (commit-file-at! git root zip-path zip-bytes message instant-str))

(defn commit-history!
  "Commit each model state as a ZIP-changing commit at the given instants
  (same count as states). Returns the vector of RevCommits."
  [git root states instants]
  (mapv (fn [m instant i]
          (commit-zip-at!
           git root
           (csv->zip-bytes (rows->csv (model->rows m)))
           (str "state " i) instant))
        states instants (range)))

(defn monotone-instants
  "n monthly instants in 2024 (n ≤ 12)."
  [n]
  (mapv #(format "2024-%02d-01T00:00:00Z" (inc %)) (range n)))

(defmacro with-repo
  "Temp JGit repo + temp work dir bound to the given symbols; the repo is
  closed and both trees deleted on exit."
  [[git-sym root-sym work-sym] & body]
  `(let [~root-sym (temp-dir "sim-repo")
         ~work-sym (temp-dir "sim-work")]
     (try
       (let [~git-sym (init-repo! ~root-sym)]
         (try ~@body
              (finally (.close ~git-sym))))
       (finally
         (delete-tree! ~root-sym)
         (delete-tree! ~work-sym)))))
