(ns abc.load
  (:require [clojure.java.io :as io]
            [me.raynes.fs :as fs]
            [charred.api :as charred]
            [com.climate.claypoole :as cp]
            [com.climate.claypoole.lazy :as cp-lazy]
            [clojure.string :as string]
            [abc.config :as config]
            [abc.aozora :as aozora]
            [abc.annotation :as annotation]
            [abc.xtdb :as xtdb]
            [xtdb.api :as xt]
            [taoensso.timbre :as timbre]
            [clojure.repl])
  (:import [org.apache.commons.compress.archivers.zip ZipFile ZipArchiveEntry]
           [java.net URL #_UnknownHostException #_ConnectException]
           [org.apache.commons.io.input BOMInputStream]
           [java.io IOException]
           [java.security MessageDigest]
           (clojure.lang PersistentQueue)))

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

#_(s/fdef aozora-bunko-db
    :args (s/cat :path string?)
    :ret ::aozora/db-map)
(defn aozora-bunko-db
  "Loads bibliographic metadata into graph data structure."
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
              (doall (charred/read-csv r)))))

        [header & records] all-records

        entities
        (into []
              (comp (map (partial zipmap header))
                    (map remove-empty-vals)
                    (map aozora/record-to-entities))
              records)]
    (aozora/merge-entities entities)))

(defn persist-db!
  ([] (persist-db! (xtdb/node)))
  ([node]
   (let [{:keys [works persons]} (aozora-bunko-db abc.config/aozora-bunko-path)]
     (timbre/debug "Persisting" (count works) "works to" node)
     (doseq [txs (into [] (comp (map second) (map (fn [d] [::xt/put d])) (partition-all 1000)) works)]
       #_(println (take 1 txs))
       (xtdb/submit-tx node txs))
     (timbre/debug "Persisting" (count persons) "persons to" node)
     (doseq [txs (into [] (comp (map second) (map (fn [d] [::xt/put d])) (partition-all 1000)) persons)]
       (xtdb/submit-tx node txs))
     (timbre/debug "Finished persisting DB to" node))))

(comment

  (xtdb/q '{:find          [(count ?work)]
            :where         [[?author :abc.aozora/family-name "夏目"]
                            [?work :abc.aozora/author ?author]]}))

#_(s/fdef aozora-bunko-db-coll
    :args (s/cat :db ::aozora/db-map)
    :ret (s/coll-of ::aozora/entity-map))
(defn aozora-bunko-db-coll [db]
  (let [{:keys [works persons]} db]
    (into (vals works) (vals persons))))

(defn parse-html [s])

(defn aozora-bunko-text [path encoding & file-extensions]                     ;; ShiftJIS -> SJIS?
  (let [file-extensions (if (seq file-extensions) (set file-extensions) #{".txt"})]
    (timbre/warn path encoding file-extensions)
    (when path
      (if (not= ".zip" (fs/extension path))
        (slurp path)
        (try
          (with-open [z (ZipFile. (fs/file path))]
            (let [files (enumeration-seq (.getEntries z))
                  texts
                  (for [file files
                        :when (contains? file-extensions
                                         (try (string/lower-case
                                               (fs/extension (.getName ^ZipArchiveEntry file)))
                                              (catch Exception e (println (ex-data e) file (.getName ^ZipArchiveEntry file)))))]
                    ;; FIXME we need to read html files as XML and convert them to the final EDN/TEI format
                    (slurp (.getInputStream z file) :encoding encoding #_"SJIS"))]
              #_(when (> (count files) 1)
                  (println "More than one file in zip!: " path files)) ;; FIXME Check all...
              (when (> (count texts) 1)
                (println "More than one text file in zip!: " path (count texts) texts))
              (first texts)))
          ;; TODO Some texts that are outside Aozora Bunko will fail due to abandoned hosts/etc. Have a look at mapping these URLs to fixed versions/upstreaming changes.
          (catch IOException e (timbre/error "File fetch failure:" (ex-data e) ", skipping...")))))))

(defn sha-512 [^String data]
  (let [md (. MessageDigest getInstance "sha-512")]
    (. md update (.getBytes data))
    (let [bytes (. md digest)]
      (reduce #(str %1 (format "%02x" %2)) "" bytes))))

(defn local-cache [^URL url]
  ;; Most are zip, but some are HTML !index! files, so we need to scrape their links too...: ヴォルテールのザディッグ及び、日本の作品を外国語に訳したもの。 vvv
  ;; cache/71a53fc22d17c0ae67048d441599c9d2c00e6836af5616b5fba2d6fee37377bda02ffa583e20bb69176f8787bf8192943ff379f0c4c238dfda97a149fd98390d-etc.html
  (let [path "cache"
        url-hash (sha-512 (.toString url))
        url-path (fs/file path (str url-hash "-" (fs/base-name (.getFile url))))]
    (timbre/debug url path url-hash url-path)
    (when (not (fs/directory? path))
      (fs/mkdir path))
    (if (not (fs/exists? url-path))
      (try
        (with-open [in (io/input-stream url)
                    out (io/output-stream url-path)]
          (io/copy in out)
          url-path)
        (catch Exception e (timbre/error "Cache fetch failure:" (ex-data e) url ", skipping...")))
      url-path)))

(defn web-to-git [^URL url path]
  (if (and (or (aozora/url? url) (string? url)) (not-empty (.toString url)))
    (let [match (re-seq #"https?://www\.aozora\.gr\.jp(/cards/\d+/.+)" (.toString url))]
      (if (seq match)
        (str path (second (first match)))
        (local-cache url)))))
;; FIXME map: http://www.01.246.ne.jp/~tnoumi/noumi1/etc.html => http://www.01.246.ne.jp/~tnoumi/noumi1/books/zadig.html

#_(s/fdef extract-text
    :args (s/cat :resource ::aozora/source)
    :ret (s/nilable string?))
(defn extract-text [resource]
  (timbre/debug resource)
  (when-let [{:keys [abc.aozora/url abc.aozora/encoding]} resource]
    (when-let [text (aozora-bunko-text (web-to-git url config/aozora-bunko-path) encoding)]
      (string/replace text "\r" ""))))

(defn build-lagging-transducer
  "creates a transducer that will always run n items behind.
   this is convenient if the pipeline contains futures, which you
   want to start deref-ing only when a certain number are in flight"
  [n]
  (fn [rf]
    (let [qv (volatile! PersistentQueue/EMPTY)]
      (fn
        ([] (rf))
        ([acc] (reduce rf acc @qv))
        ([acc v]
         (vswap! qv conj v)
         (if (< (count @qv) n)
           acc
           (let [h (peek @qv)]
             (vswap! qv pop)
             (rf acc h))))))))

(defn parallelising-map
  [f]
  (let [n 10000 #_(* 20 (.. Runtime getRuntime availableProcessors))]
    (comp (map #(fn [] (f %)))
          (map future-call)
          (build-lagging-transducer n)
          (map deref))))

#_(s/fdef extract-texts
    :args (s/cat :entities ::aozora/db-map)
    :ret (s/coll-of :document/body))
#_(defn extract-texts [entities]
    (let [work-entities (vals (:works entities))]
      ;; FIXME After extraction is stable, we need to fix below to happen WRT metadata or be linked to it somehow.
      ;; TODO I believe there is a large gap between short and long works, so the time taken per work can vary on the order of a magnitude or two.
      ;; 31749 works, max chars 5,558,144, min chars 233
      ;; Full run: 2009s = 33m (50 threads), 28m (96 threads), 23m (n = 192 threads), ..., 7.8m (10,000)
      (cp/with-shutdown!
        [pool (cp/threadpool (.. Runtime getRuntime availableProcessors))]
        (let [xf (comp
                   ;; FIXME fall back to html if missing
                  (filter (fn [m] (= (:dcterms/format m) "text/plain")))
                  (map extract-text)
                  (filter identity)
                  (map annotation/parse-text))]            ;; FIXME unused
          (into [] (remove nil?)
                (cp/pmap pool
                         (fn [m] (if (= (:dcterms/format m) "text/plain")
                                   (if-let [text (extract-text m)]
                                     (annotation/parse-text text))))
                         (mapcat ::aozora/sources work-entities)))
          #_(into [] xf work-entities)))))

(defn lazy-pmap [fn coll]
  (cp/with-shutdown!
    [pool (cp/threadpool (.. Runtime getRuntime availableProcessors))]
    (cp-lazy/pmap pool fn coll)))

(defn- pretty-demunge
  [f]
  (as-> (str f) $
    (clojure.repl/demunge $)
    (or (re-find #"(.+)--\d+@" $)
        (re-find #"(.+)@" $))
    (last $)))

(defn with-time-duration
  "Returns a map wrapping the result in :result and time elapsed in :duration (in seconds)."
  [expr & args]
  (let [start (. System (nanoTime))
        result (apply expr args)
        duration (/ (double (- (. System (nanoTime)) start)) 1000000000.0)]
    (timbre/debug (pretty-demunge expr) (into [] (filter keyword? args)) duration)
    {:duration duration :result result}))

(defn work-id->document [id]
  (when-let [text (extract-text (xtdb/work-id-to-url id))]
    (try (:result (with-time-duration annotation/parse-text text id))
         (catch Exception e (timbre/error (format "Failed parsing %s with error %s" id e))))))

(def !times (atom []))

(defn persist-texts!
  ([] (persist-texts! (xtdb/node)))
  ([a-node]
   (let [work-partitions (into [] (partition-all 100) (xtdb/all-works))]
     (timbre/debug "Persisting over" (count work-partitions) "partitions")
     (cp/with-shutdown!
       [pool (cp/threadpool (.. Runtime getRuntime availableProcessors))]
       (doseq [[idx part] (map-indexed vector work-partitions)]
         (timbre/debug "Persisting texts in partition" idx part)
         (xtdb/submit-tx
          a-node
          (->> part
               (filter (fn [id] (= id :abc.aozora/w043661 #_:abc.aozora/w018345 #_:abc.aozora/w043688)))
               (map #_pool
                (fn [id]
                  (timbre/debug id)
                  (let [{:keys [duration result]} (with-time-duration work-id->document id)]
                    (swap! !times conj duration)
                    (let [doc result]
                      (when (nil? (:document/metadata doc))
                        (timbre/error id result doc))
                      (let [doc-id (string/replace-first
                                    (->> doc :document/metadata ::aozora/work-id name)
                                    \w \d)
                            tx (assoc doc
                                      :xt/id (keyword "abc.aozora" doc-id))]
                        #_(timbre/debug [::xt/put tx])
                        [::xt/put tx]))))))))))
   (timbre/warn "Average document processing time" (/ (reduce + @!times) (count @!times)))
   (timbre/warn "Maximum document processing time" (apply max @!times))))

(comment
  (take 1 (aozora-bunko-db abc.config/aozora-bunko-path))
  (take 1 (extract-texts (aozora-bunko-db abc.config/aozora-bunko-path)))
  (dorun (extract-texts (aozora-bunko-db abc.config/aozora-bunko-path)))
  (extract-texts (aozora-bunko-db abc.config/aozora-bunko-path))
  (doseq [text (extract-texts (aozora-bunko-db abc.config/aozora-bunko-path))]
    ())

  (do (time (persist-db!))
      (time (persist-texts!))))
