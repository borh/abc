(ns abc.load
  (:require [clojure.java.io :as io]
            [me.raynes.fs :as fs]
            [charred.api :as charred]
            [clojure.string :as string]
            [abc.aozora :as aozora])
  (:import [org.apache.commons.compress.archivers.zip ZipFile]
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

(defn aozora-bunko-db-coll [db]
  (let [{:keys [works persons]} db]
    (into (vals works) (vals persons))))
