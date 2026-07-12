(ns abc.tools.source-bundle-report
  (:require [abc.tools.json :as json]
            [abc.tools.source-bundle :as source-bundle]
            [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.string :as string])
  (:import [java.io FileNotFoundException IOException InterruptedIOException]
           [java.nio.charset Charset]
           [java.nio.file FileSystemException]
           [org.apache.commons.compress.archivers.zip ZipArchiveEntry ZipFile]))

(def pinned-aozorabunko-commit
  "0e9ea3e586eb0aa34039fabfc85a407d2f98b165")

(def ^:private legacy-name-charset (Charset/forName "windows-31j"))

(defn- corpus-zips [root]
  (let [cards (io/file root "cards")]
    (->> (or (.listFiles cards) (make-array java.io.File 0))
         (filter #(.isDirectory ^java.io.File %))
         (map #(io/file % "files"))
         (mapcat #(or (.listFiles ^java.io.File %)
                      (make-array java.io.File 0)))
         (filter #(and (.isFile ^java.io.File %)
                       (string/ends-with? (.getName ^java.io.File %) ".zip")))
         (sort-by #(.getPath ^java.io.File %)))))

(defn- relative-path [root file]
  (-> (.relativize (.toPath (io/file root)) (.toPath (io/file file)))
      str
      (string/replace "\\" "/")))

(defn- raw-zip-stats [zip-file]
  (with-open [archive (-> (ZipFile/builder)
                          (.setFile (io/file zip-file))
                          (.setCharset legacy-name-charset)
                          (.setUseUnicodeExtraFields true)
                          (.get))]
    (let [entries (->> (enumeration-seq (.getEntries archive))
                       (remove #(.isDirectory ^ZipArchiveEntry %))
                       vec)
          sizes (mapv #(.getSize ^ZipArchiveEntry %) entries)]
      {:member-count (count entries)
       :member-bytes (reduce max 0 sizes)
       :total-bytes (reduce + 0 sizes)
       :utf8-count (count (filter #(.usesUTF8ForNames
                                    (.getGeneralPurposeBit
                                     ^ZipArchiveEntry %))
                                  entries))
       :legacy-count (count (remove #(.usesUTF8ForNames
                                      (.getGeneralPurposeBit
                                       ^ZipArchiveEntry %))
                                    entries))})))

(defn- sevenzip-readable? [zip-file]
  (let [binary (or (System/getenv "ABC_7ZZ_BIN") "7zz")]
    (zero? (:exit (shell/sh binary "l" "-slt" (str zip-file))))))

(defn- empty-summary []
  {"readable_zip_count" 0
   "unreadable_zip_count" 0
   "semantic_text_member_counts" {}
   "utf8_flagged_entry_count" 0
   "legacy_flagged_entry_count" 0
   "nfc_collision_bundle_count" 0
   "unicode_case_collision_bundle_count" 0
   "max_member_count" 0
   "max_member_bytes" 0
   "max_total_bytes" 0
   "java_unreadable_7zz_recoverable_count" 0
   "java_unreadable_7zz_unrecoverable_count" 0
   "damaged_paths" []})

(defn- record-readable [summary zip-file raw]
  (let [metadata (source-bundle/inspect-zip-metadata zip-file)
        semantic-count (:semantic-text-member-count metadata)]
    (-> summary
        (update "readable_zip_count" inc)
        (update-in ["semantic_text_member_counts" (str semantic-count)]
                   (fnil inc 0))
        (update "utf8_flagged_entry_count" + (:utf8-count raw))
        (update "legacy_flagged_entry_count" + (:legacy-count raw))
        (update "max_member_count" max (:member-count raw))
        (update "max_member_bytes" max (:member-bytes raw))
        (update "max_total_bytes" max (:total-bytes raw))
        (cond-> (:nfc-collision? metadata)
          (update "nfc_collision_bundle_count" inc))
        (cond-> (:unicode-case-collision? metadata)
          (update "unicode_case_collision_bundle_count" inc)))))

(defn- record-unreadable [summary root zip-file]
  (let [recoverable? (sevenzip-readable? zip-file)]
    (-> summary
        (update "unreadable_zip_count" inc)
        (update (if recoverable?
                  "java_unreadable_7zz_recoverable_count"
                  "java_unreadable_7zz_unrecoverable_count") inc)
        (update "damaged_paths" conj (relative-path root zip-file)))))

(defn measure!
  "Measure only cards/*/files/*.zip beneath aozora-root. The returned map is
  deterministic and contains paths relative to aozora-root."
  [aozora-root]
  (reduce (fn [summary zip-file]
            (let [raw (try
                        (raw-zip-stats zip-file)
                        (catch InterruptedIOException e (throw e))
                        (catch FileNotFoundException e (throw e))
                        (catch FileSystemException e (throw e))
                        (catch IOException e e))]
              (if (instance? IOException raw)
                (record-unreadable summary aozora-root zip-file)
                (record-readable summary zip-file raw))))
          (empty-summary)
          (corpus-zips aozora-root)))

(defn -main [& args]
  (when-not (= 2 (count args))
    (throw (ex-info
            "usage: clojure -M -m abc.tools.source-bundle-report AOZORA_ROOT OUTPUT_JSON"
            {:args args})))
  (let [[aozora-root output] args]
    (json/write-deterministic-json-file!
     output
     (assoc (measure! aozora-root)
            "aozorabunko_commit" pinned-aozorabunko-commit))))
