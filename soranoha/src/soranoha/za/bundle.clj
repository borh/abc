(ns soranoha.za.bundle
  "Bulk corpus selections as pre-built ZIP archives.

  Serving is a static tree with no application runtime, so a selection cannot
  be assembled when it is asked for. Every selection a reader can take is
  therefore built at export time and served as an ordinary file. The
  selections mirror the axes the site already browses by — the whole corpus,
  one person, one NDC class — so `everything by this author` is a link on
  that author's page rather than a program the reader has to write.

  Two artifact types are bulk-published; `soranoha.za.naming` says which and
  why, because the browse layer links the same archives.

  Each archive carries `catalog.csv` at its root: the structured citation
  fields, from `soranoha.za.citation`, for exactly the works inside it. The
  CSV is what removes the need for a mapping program: it opens in a
  spreadsheet, and a whole selection can be turned into a bibliography
  without opening a single TEI file. The identifier, source hash and release
  columns, not the filename, are the citable ones. It is a serving-time
  projection of the signed `/catalog.json`, not a second record.

  Deterministic, like everything else the browse layer generates: fixed entry
  order, fixed modification time, fixed compression level, no environment and
  no clock. That is what lets the exporter's reuse check compare an existing
  archive byte for byte rather than trusting that it is the right one. The
  archives are written as a stream and never held whole: a corpus-wide TEI
  archive is far larger than any byte array this process should allocate."
  (:require [clojure.string :as string]
            [soranoha.za.browse :as browse]
            [soranoha.za.citation :as citation]
            [soranoha.za.naming :as naming])
  (:import (java.io OutputStream)
           (java.nio.charset StandardCharsets)
           (java.time LocalDateTime)
           (java.util.zip ZipEntry ZipOutputStream)))

(def ^:private entry-time
  "A fixed date every entry carries. A build stamp would make the bytes
  depend on when they were produced, and these archives have to be
  reproducible from the release alone.

  `setTimeLocal` writes the MS-DOS date field directly, so the exporter's
  timezone does not reach the bytes — but only for a date strictly inside the
  DOS range. At its 1980-01-01 boundary the JDK falls back to an extended
  timestamp extra field holding an epoch second, which is computed through
  the default timezone and is therefore not reproducible. Hence a date well
  inside the range rather than the usual reproducible-build zero."
  (LocalDateTime/of 2000 1 1 0 0 0))

(def ^:private compression-level
  "Written out rather than taken from `Deflater/DEFAULT_COMPRESSION`, whose
  meaning is whatever the platform's default happens to be. Six is zlib's
  long-standing default and the level these archives are pinned to; a
  platform changing its mind must not change an archive's bytes."
  6)

(defn- csv-field
  "Every field is quoted, which RFC 4180 permits and which keeps a title
  containing a comma or a quotation mark from depending on the reader's
  guesswork."
  [value]
  (str "\"" (string/replace (str (or value "")) "\"" "\"\"") "\""))

(defn- csv-row [fields]
  ;; CRLF and a UTF-8 byte-order mark, because the stated use is opening this
  ;; in a spreadsheet: Excel reads a UTF-8 CSV as the local code page without
  ;; the mark, which turns every Japanese title into mojibake. A script reads
  ;; it with encoding utf-8-sig.
  (str (string/join "," (map csv-field fields)) "\r\n"))

(defn catalog-csv
  "The bundled works as a spreadsheet, in the archive's own order."
  ^String [release works artifact-type]
  (str "\ufeff"
       (csv-row citation/csv-columns)
       (apply str
              (map (fn [work]
                     (csv-row (citation/csv-values
                               release work
                               (naming/filename work artifact-type))))
                   works))))

(defn- put-entry! [^ZipOutputStream zip ^String name ^bytes content]
  (.putNextEntry zip (doto (ZipEntry. name)
                       (.setMethod ZipEntry/DEFLATED)
                       (.setTimeLocal entry-time)))
  (.write zip content 0 (alength content))
  (.closeEntry zip))

(defn write-archive!
  "Write one selection into `out`. `artifact` resolves a slug and type to that
  work's published bytes, so an archive holds the artifacts the manifest
  names rather than a copy made for it. The stream is finished but not
  closed: the caller opened it and decides what happens to it."
  [^OutputStream out {:keys [release works artifact-type artifact]}]
  (let [zip (ZipOutputStream. out StandardCharsets/UTF_8)]
    (.setLevel zip compression-level)
    (put-entry! zip "catalog.csv"
                (.getBytes (catalog-csv release works artifact-type)
                           StandardCharsets/UTF_8))
    (doseq [work works]
      (put-entry! zip
                  (naming/filename work artifact-type)
                  (artifact (get work "slug") artifact-type)))
    (.finish zip)
    (.flush zip)))

(defn- by-person
  "person id -> {:person contributor-record :works works-in-catalog-order}.
  A person who holds two relations to one work — author and collator, say —
  contributes it once; the contributors of a work are consecutive here, so
  the previous entry is enough to see that."
  [works]
  (reduce
   (fn [acc work]
     (reduce (fn [acc contributor]
               (let [id (get contributor "person_id")]
                 (-> acc
                     (assoc-in [id :person] (get-in acc [id :person] contributor))
                     (update-in [id :works]
                                (fn [ws]
                                  (if (identical? (peek ws) work)
                                    ws
                                    (conj (or ws []) work)))))))
             acc
             (get work "contributors")))
   (sorted-map)
   works))

(defn selections
  "Every bulk selection for one release as `{:path :works :artifact-type}`,
  without building any archive. The paths a page links are rendered from
  `soranoha.za.naming` rather than read back from here, so the browse layer
  does not have to know how an archive is built to point at one."
  [catalog]
  (let [works (vec (get catalog "works"))
        people (by-person works)
        by-ndc (group-by browse/ndc-class-key works)]
    (for [artifact-type naming/bulk-artifact-types
          selection (concat
                     [{:path (naming/corpus-bundle-path artifact-type)
                       :works works}]
                     (map (fn [[id {:keys [person works]}]]
                            {:path (naming/author-bundle-path person id artifact-type)
                             :works works})
                          people)
                     ;; every class, including the ones with nothing in them:
                     ;; a link that answers "none in this release" is better
                     ;; than a link that 404s
                     (map (fn [[key _ _]]
                            {:path (naming/ndc-bundle-path key artifact-type)
                             :works (vec (get by-ndc key))})
                          browse/ndc-classes))]
      (assoc selection :artifact-type artifact-type))))

(defn archives
  "Every bulk archive for one release as `[path produce]` pairs, where
  `produce` writes that archive into an OutputStream. Lazy, and nothing here
  holds an archive after it has been written."
  [{:keys [catalog artifact release]}]
  (map (fn [{:keys [path works artifact-type]}]
         [path (fn [^OutputStream out]
                 (write-archive! out {:release release
                                      :works works
                                      :artifact-type artifact-type
                                      :artifact artifact}))])
       (selections catalog)))
