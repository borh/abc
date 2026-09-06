(ns soranoha.za.corpus
  "Fixture corpus for release-assembly acceptance: a miniature aozorabunko
  checkout under git (cards/ work zips + the official catalog zip), driven
  through the real kernel — real provenance gate, catalog read, selection
  join, extract stage, and engine/trace/CAS. Every other stage (metadata,
  parse, convert, render, validate) is a deterministic in-process
  substitute whose wiring, output names, invalidation-relevant content
  dependencies, and record shapes match the production graph, so
  main/run-work! executes unchanged and trace invalidation is the real
  engine's."
  (:require [babashka.fs :as fs]
            [babashka.process :as process]
            [charred.api :as json]
            [clojure.string :as str]
            [soranoha.core.hash :as hash]
            [soranoha.kura.cas :as cas]
            [soranoha.kura.engine :as engine]
            [soranoha.kura.trace :as trace]
            [soranoha.main :as main]
            [soranoha.ori.stages :as stages]
            [soranoha.ported.json :as abc-json]
            [soranoha.yomi.catalog :as catalog]
            [soranoha.yomi.select :as select])
  (:import [java.io FileOutputStream]
           [java.util.zip ZipEntry ZipOutputStream]))

;; --- corpus checkout --------------------------------------------------------

(def ^:private fixed-entry-time
  ;; deterministic zip bytes; an output-preserving rezip passes a different
  ;; time so the archive bytes change while the member content does not
  1000000000000)

(defn write-zip!
  [path entries & {:keys [entry-time] :or {entry-time fixed-entry-time}}]
  (fs/create-dirs (fs/parent path))
  (with-open [out (ZipOutputStream. (FileOutputStream. (str path)))]
    (doseq [[name ^String content] entries]
      (.putNextEntry out (doto (ZipEntry. ^String name)
                           (.setTime (long entry-time))))
      (.write out (.getBytes content "UTF-8"))
      (.closeEntry out)))
  path)

(defn work-basename [{:keys [book n]}]
  (str book "_ruby_" n))

(defn work-zip-path [root {:keys [card] :as work}]
  (fs/path root "cards" card "files" (str (work-basename work) ".zip")))

(defn write-work!
  [root {:keys [text] :as work} & {:as opts}]
  (write-zip! (work-zip-path root work)
              [[(str (work-basename work) ".txt") text]]
              (or opts {})))

(defn delete-work! [root work]
  (fs/delete (work-zip-path root work)))

(defn work-slug [{:keys [work-id person-id card] :as work}]
  (select/slug work-id person-id
               (str "cards/" card "/files/" (work-basename work) ".zip")))

(defn write-catalog!
  "The official catalog zip for `works`: one row per (work, contributor)
  pair, joining each zip to its work/person ids through the text-file
  URL basename. This is the real catalog's shape — every row of a work
  repeats that work's URL, so a multi-contributor work has several rows
  sharing one basename. Each work's own :person-id 著者 row is emitted
  LAST, so the basename index (later rows win) resolves to it and the
  work's slug is independent of its other contributors; any
  :contributors entries ({:person-id :role}) precede it."
  [root works]
  (write-zip! (fs/path root "index_pages" "list_person_all_extended_utf8.zip")
              [["list_person_all_extended_utf8.csv"
                (str "作品ID,人物ID,役割フラグ,作品名,テキストファイルURL\n"
                     (str/join ""
                               (for [{:keys [work-id person-id card title
                                             contributors]
                                      :as work} works
                                     {p :person-id r :role}
                                     (concat contributors
                                             [{:person-id person-id
                                               :role "著者"}])]
                                 (str work-id "," p "," r "," title
                                      ",https://example.org/cards/" card
                                      "/files/" (work-basename work) ".zip\n"))))]]))

(defn- git! [root & args]
  (let [{:keys [exit err]} (apply process/sh {:dir (str root) :out :string
                                              :err :string}
                                  "git" args)]
    (when-not (zero? exit)
      (throw (ex-info "corpus git command failed" {:args args :err err})))))

(defn commit-corpus!
  "Commit the working tree and return the new revision (also the real
  provenance gate's answer)."
  [root]
  (git! root "add" "-A")
  (git! root "-c" "user.name=za-fixture" "-c" "user.email=za@localhost"
        "commit" "-q" "--allow-empty" "-m" "corpus revision")
  (main/source-provenance! root))

(defn init-corpus!
  "Fresh corpus checkout containing `works`, committed. Returns its root."
  [works]
  (let [root (str (fs/create-temp-dir {:prefix "za-corpus"}))]
    (git! root "init" "-q")
    (doseq [work works] (write-work! root work))
    (write-catalog! root works)
    (commit-corpus! root)
    root))

;; --- stage graph ------------------------------------------------------------

(def fixture-toolchain "za-fixture-toolchain-1")

(defn- json-bytes ^bytes [value]
  (.getBytes (abc-json/write-deterministic-json-str value) "UTF-8"))

(defn- blob-json [blob hex]
  (json/read-json (String. ^bytes (blob hex) "UTF-8")))

(def ^:private metadata-stage
  ;; catalog (by content hash) + work id -> record + persons, purely from
  ;; CAS content, so a catalog edit invalidates exactly this stage
  {:stage-id "metadata"
   :stage-version "1"
   :toolchain-id fixture-toolchain
   :f (fn [{:keys [blob]} inputs]
        (let [rows (catalog/read-rows-from-string
                    (String. ^bytes (blob (get inputs "catalog")) "UTF-8"))
              work-id (get inputs "work_id")
              row (or (first (filter #(= work-id (catalog/row-work-id %)) rows))
                      (throw (ex-info "work id absent from catalog"
                                      {:work-id work-id})))
              person-id (catalog/row-person-id row)]
          {"metadata-record" (json-bytes {"work_id" work-id
                                          "title" (get row "作品名")
                                          "contributors"
                                          [{"person_id" person-id
                                            "role" "author"}]})
           "persons" (json-bytes {person-id {"person_id" person-id
                                             "name" (str "person-"
                                                         person-id)}})}))})

(def ^:private parse-stage
  {:stage-id "parse"
   :stage-version "1"
   :toolchain-id fixture-toolchain
   :f (fn [{:keys [blob]} inputs]
        {"aat" (json-bytes {"schema" "fixture-aat/1"
                            "text" (String. ^bytes (blob (get inputs "source"))
                                            "UTF-8")})})})

(def ^:private convert-stage
  {:stage-id "convert"
   :stage-version "1"
   :toolchain-id fixture-toolchain
   :f (fn [{:keys [blob]} inputs]
        {"parser-ir" (json-bytes
                      {"schema" "fixture-parser-ir/1"
                       "work_content_hash" (get inputs "work_content_hash")
                       "text" (get (blob-json blob (get inputs "aat"))
                                   "text")})})})

(def ^:private render-stage
  {:stage-id "render"
   :stage-version "1"
   :toolchain-id fixture-toolchain
   :f (fn [{:keys [blob]} inputs]
        (let [ir (blob-json blob (get inputs "parser-ir"))
              record (blob-json blob (get inputs "metadata-record"))
              text (get ir "text")]
          {"tei" (.getBytes (str "<TEI><teiHeader><title>"
                                 (get record "title")
                                 "</title></teiHeader><text>" text
                                 "</text></TEI>")
                            "UTF-8")
           "plaintext" (.getBytes ^String text "UTF-8")}))})

(def ^:private validate-stage
  ;; include-and-flag: the record shape matches the kernel's real
  ;; tei-validation output; the fixture marker stands in for a schema
  ;; violation
  {:stage-id "validate-tei"
   :stage-version "1"
   :toolchain-id fixture-toolchain
   :f (fn [{:keys [blob]} inputs]
        (let [tei ^bytes (blob (get inputs "tei"))]
          {"tei-validation"
           (json-bytes {"status" (if (str/includes? (String. tei "UTF-8")
                                                    "fixture-invalid")
                                   "failed"
                                   "passed")
                        "validated_artifact" (str "sha256:"
                                                  (hash/sha256-bytes tei))
                        "layers" {"relax_ng" "fixture"
                                  "schematron" "fixture"}})}))})

(def stage-set
  {:extract (stages/extract-stage fixture-toolchain)
   :metadata metadata-stage
   :parse parse-stage
   :convert convert-stage
   :render render-stage
   :validate validate-stage
   :fidelity (stages/source-fidelity-stage fixture-toolchain)})

;; --- kernel run -------------------------------------------------------------

(defn run-corpus!
  "One kernel run at the corpus's current commit into the persistent store
  under `store-root` (a second run over the same store exercises real
  trace caching). Returns {:commit :catalog-hex :cas-dir :candidates
  :stage-coordinates :zip-hashes {slug hex} :results {slug {:outputs
  :cached :trace-keys}}}."
  [root store-root]
  (let [commit (main/source-provenance! root)
        {:keys [csv-text]} (catalog/read-catalog-zip root)
        rows (catalog/read-rows-from-string csv-text)
        {:keys [candidates]} (select/select-candidates root rows)
        store (engine/open-store! {:cas-dir (str (fs/path store-root "objects"))
                                   :db-path (str (fs/path store-root
                                                          "trace.sqlite"))})]
    (try
      (let [catalog-hex (cas/put-bytes! (:cas-dir store)
                                        (.getBytes ^String csv-text "UTF-8"))]
        {:commit commit
         :catalog-hex catalog-hex
         :cas-dir (:cas-dir store)
         :candidates candidates
         :stage-coordinates (trace/stage-coordinates stage-set)
         :zip-hashes (into {}
                           (map (fn [{:keys [slug file]}]
                                  [slug (hash/sha256-file file)]))
                           candidates)
         :results (into {}
                        (map (fn [candidate]
                               [(:slug candidate)
                                (main/run-work! store stage-set candidate
                                                catalog-hex)]))
                        candidates)})
      (finally (engine/close-store! store)))))

(defn source-facts [run slug]
  (json/read-json
   (String. ^bytes (cas/get-bytes (:cas-dir run)
                                  (get-in run [:results slug :outputs
                                               :extract "source-facts"]))
            "UTF-8")))

(defn works-for-assembly
  "The assembler's works map from one run's results."
  [run]
  (into {}
        (map (fn [[slug {:keys [outputs]}]]
               [slug {:plaintext (get-in outputs [:render "plaintext"])
                      :tei (get-in outputs [:render "tei"])
                      :tei-validation (get-in outputs [:validate
                                                       "tei-validation"])
                      :source-content-hash (get (source-facts run slug)
                                                "work_content_hash")}]))
        (:results run)))
