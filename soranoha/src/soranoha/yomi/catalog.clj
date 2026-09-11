(ns soranoha.yomi.catalog
  "Read the official catalog archive and index rows by work text URL."
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [soranoha.core.hash :as hash])
  (:import [java.io ByteArrayInputStream]
           [java.util.zip ZipEntry ZipFile ZipInputStream]))

(defn csv-text-from-zip-bytes
  "The CSV entry of a catalog archive held in memory, for a catalog that is
  not the checkout's: one read out of git history. Nil when the archive
  holds no CSV entry."
  [^bytes zip-bytes]
  (with-open [in (ZipInputStream. (ByteArrayInputStream. zip-bytes))]
    (loop []
      (when-let [entry (.getNextEntry in)]
        (if (string/ends-with? (.getName entry) ".csv")
          (String. (.readAllBytes in) "UTF-8")
          (recur))))))

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
  "Index rows by text-file URL basename; later rows win."
  [rows]
  (reduce (fn [idx row]
            (if-let [basename (text-url-basename row)]
              (assoc idx basename row)
              idx))
          {}
          rows))
