(ns abc.load
  (:require [clojure.java.io :as io]
            [me.raynes.fs :as fs]
            [clojure.data.csv :as csv]
            [clojure.spec.alpha :as s]
            [expound.alpha :as expound]
            [clojure.string :as string]
            [clojure.java.io :as io]
            [abc.aozora :refer :all :as aozora])
  (:import [org.apache.commons.compress.archivers.zip ZipFile ZipArchiveEntry]
           [java.io File]
           [java.net URL UnknownHostException]
           [org.apache.commons.io.input BOMInputStream]))

(defn remove-empty-vals [m]
  (into {}
        (remove
         (fn [[_ v]]
           (let [v (string/trim v)]
             (if (or (nil? v)
                     (and (string? v)
                          (empty? v)))
               true
               false)))
         m)))

(s/fdef aozora-bunko-db
  :args (s/cat :path string?)
  :ret ::aozora/db-map)
(defn aozora-bunko-db
  "Loads bibilographic metadata into graph data structure."
  [path]
  (let [normalized-path (fs/expand-home path)
        db-file (fs/file normalized-path "index_pages" "list_person_all_extended_utf8.zip")

        all-records
        (with-open [z (ZipFile. db-file)]
          (let [csv-file (first (enumeration-seq (.getEntries z)))]
            (with-open [r (->> csv-file
                               (.getInputStream z)
                               BOMInputStream.
                               io/reader)]
              (doall (csv/read-csv r)))))

        [header & records] all-records

        entities
        (sequence
         (comp (map (partial zipmap header))
               (map remove-empty-vals)
               (map record-to-entities))
         records)]
    (merge-entities entities)))

(s/fdef aozora-bunko-db-coll
  :args (s/cat :db ::aozora/db-map)
  :ret ::aozora/entity-coll)
(defn aozora-bunko-db-coll [db]
  (let [{:keys [works persons]} db]
    (into (vals works) (vals persons))))

(defn aozora-bunko-text [path encoding] ;; ShiftJIS -> SJIS?
  (if path
    (if (not= ".zip" (fs/extension path))
      (slurp path)
      (try
        (with-open [z (ZipFile. (fs/file path))]
          (let [files (enumeration-seq (.getEntries z))
                texts
                (for [file files
                      :when (contains? #{".txt" ".html"}
                                       (try (string/lower-case
                                             (fs/extension (.getName ^ZipArchiveEntry file)))
                                            (catch Exception e (println e file (.getName ^ZipArchiveEntry file)))))]
                  (slurp (.getInputStream z file) :encoding encoding #_"SJIS"))]
            #_(when (> (count files) 1)
                (println "More than one file in zip!: " path files)) ;; FIXME Check all...
            (when (> (count texts) 1)
              (println (count texts) texts))
            (first texts)))
        (catch java.io.IOException e (println e ", skipping..."))))))

(defn sha-512 [^String data]
  (let [md (. java.security.MessageDigest getInstance "sha-512")]
    (. md update (.getBytes data))
    (let [bytes (. md digest)]
      (reduce #(str %1 (format "%02x" %2)) "" bytes))))

(defn local-cache [^URL url]
  ;; Most are zip, but some are HTML !index! files, so we need to scrape their links too...: ヴォルテールのザディッグ及び、日本の作品を外国語に訳したもの。 vvv
  ;; cache/71a53fc22d17c0ae67048d441599c9d2c00e6836af5616b5fba2d6fee37377bda02ffa583e20bb69176f8787bf8192943ff379f0c4c238dfda97a149fd98390d-etc.html
  (let [path "cache"
        url-hash (sha-512 (.toString url))
        url-path (fs/file path (str url-hash "-" (fs/base-name (.getFile url))))]
    (when (not (fs/directory? path))
      (fs/mkdir path))
    (when (not (fs/exists? url-path))
      (try
        (with-open [in (io/input-stream url)
                    out (io/output-stream url-path)]
          (io/copy in out)
          url-path)
        (catch Exception e (println e url ", skipping..."))))))

(defn web-to-git [^URL url path]
  (if (and (or (url? url) (string? url)) (not-empty (.toString url)))
    (let [match (re-seq #"https?://www\.aozora\.gr\.jp(/cards/\d+/.+)" (.toString url))]
      (if (seq match)
        (str path (second (first match)))
        (local-cache url)))))
;; FIXME map: http://www.01.246.ne.jp/~tnoumi/noumi1/etc.html => http://www.01.246.ne.jp/~tnoumi/noumi1/books/zadig.html

(s/fdef extract-text
  :args (s/cat :resource :abc.aozora/source)
  :ret (s/nilable string?))
(defn extract-text [resource]
  (if-let [{:keys [abc.aozora/url abc.aozora/encoding]} resource]
    (aozora-bunko-text (web-to-git url "../../Dependencies/aozorabunko") encoding)))

(s/fdef extract-texts
  :args (s/cat :entities :abc.aozora/db-map))
(defn extract-texts [entities]
  (let [work-entities (vals (:works entities))]
    (->> work-entities
         (mapcat :abc.aozora/sources)
         (filter (fn [m] (= (:dcterms/format m) "text/plain")))
         (map extract-text)
         (filter identity)
         (map (fn [s] (string/replace s "\r" ""))))))

(comment
  (dorun (extract-texts (aozora-bunko-db abc.config/aozora-bunko-path)))
  (extract-texts (aozora-bunko-db abc.config/aozora-bunko-path)))
