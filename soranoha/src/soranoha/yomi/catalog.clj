;; Official Aozora catalog access: the CSV inside
;; index_pages/list_person_all_extended_utf8.zip. Ported from
;; abc.tools.soranoha-build-publication (read-catalog-zip, catalog-index) and
;; abc.tools.aozora-csv (row parsing incl. BOM strip and the ragged marker) —
;; selection semantics must match abc byte-for-byte for the Slice-1
;; equivalence gate.
(ns soranoha.yomi.catalog
  (:require [charred.api :as charred]
            [clojure.java.io :as io]
            [clojure.string :as string]
            [soranoha.core.hash :as hash])
  (:import [java.util.zip ZipEntry ZipFile]))

(def ^:private bom-char (char 0xFEFF))

(defn- strip-bom [^String s]
  (if (and s (pos? (.length s)) (= bom-char (.charAt s 0)))
    (.substring s 1)
    s))

(def ragged-key
  "Marker key on a parsed row whose cell count differed from the header's."
  ::ragged?)

(defn read-rows-from-string [^String s]
  (let [rows (charred/read-csv s)]
    (when (seq rows)
      (let [header (mapv (fn [c] (strip-bom (or c ""))) (first rows))
            width (count header)]
        (mapv (fn [r]
                (cond-> (into {} (map (fn [k v] [k (or v "")]) header r))
                  (not= width (count r)) (assoc ragged-key true)))
              (rest rows))))))

(defn read-catalog-zip
  "Read the official catalog ZIP under `aozora-root`; returns
  {:csv-text :csv-bytes :catalog-csv-hash (bare hex)}."
  [aozora-root]
  (let [zip-file (io/file (str aozora-root) "index_pages"
                          "list_person_all_extended_utf8.zip")]
    (when-not (.isFile zip-file)
      (throw (ex-info "official catalog ZIP is missing"
                      {:path (str zip-file)})))
    (with-open [zf (ZipFile. zip-file)]
      (let [entry (->> (enumeration-seq (.entries zf))
                       (filter (fn [^ZipEntry e]
                                 (string/ends-with? (.getName e) ".csv")))
                       first)]
        (when-not entry
          (throw (ex-info "official catalog ZIP contains no CSV entry"
                          {:path (str zip-file)})))
        (let [bytes (with-open [in (.getInputStream zf entry)]
                      (.readAllBytes in))]
          {:csv-bytes bytes
           :csv-text (String. ^bytes bytes "UTF-8")
           :catalog-csv-hash (hash/sha256-bytes bytes)})))))

(defn text-url-basename [row]
  (some-> (get row "テキストファイルURL")
          string/trim
          (string/split #"/")
          last))

(defn row-work-id [row] (get row "作品ID"))
(defn row-person-id [row] (get row "人物ID"))

(defn catalog-index
  "Index rows by the basename of each row's text-file URL (later rows win,
  as in abc)."
  [rows]
  (reduce (fn [idx row]
            (if-let [basename (text-url-basename row)]
              (assoc idx basename row)
              idx))
          {}
          rows))
