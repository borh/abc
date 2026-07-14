(ns abc.tools.files
  (:require [abc.tools.hash :as hash]
            [abc.tools.evidence-io :as evidence-io]
            [abc.tools.json :as abc-json]
            [babashka.fs :as fs]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as string])
  (:import [java.util.zip ZipFile]
           [javax.xml.parsers DocumentBuilderFactory]
           [org.apache.jena.riot RDFDataMgr]))

(def hash-pattern hash/hash-pattern)

(defn repo-root []
  (fs/file (fs/canonicalize ".")))

(defn path [& segments]
  (apply fs/file (repo-root) segments))

(defn read-json [file]
  (abc-json/read-json-file file))

(defn read-json-lines [file]
  (->> (string/split-lines (slurp (evidence-io/record-read! (fs/file file))))
       (remove string/blank?)
       (mapv abc-json/read-json-str)))

(defn read-edn [file]
  (edn/read-string (slurp (evidence-io/record-read! (fs/file file)))))

(defn read-text [file]
  (slurp (evidence-io/record-read! (fs/file file))))

(defn read-bytes [file]
  (fs/read-all-bytes (evidence-io/record-read! file)))

(defn input-stream [file]
  (io/input-stream (evidence-io/record-read! file)))

(defn reader [file]
  (io/reader (evidence-io/record-read! file)))

(defn list-files [directory]
  (let [files (->> (fs/list-dir directory)
                   (filter fs/regular-file?)
                   (sort-by str)
                   (mapv fs/file)
                   vec)]
    (doseq [file files] (evidence-io/record-read! file))
    files))

(defn list-files-if-directory [directory]
  (if (fs/directory? directory)
    (list-files directory)
    []))

(defn glob [root pattern]
  (let [matches (vec (fs/glob root pattern))]
    (doseq [file matches]
      (evidence-io/record-read! file))
    matches))

(defn create-dirs! [path]
  (fs/create-dirs path))

(defn create-parent-dirs! [path]
  (io/make-parents (io/file path)))

(defn write-bytes! [path bytes]
  (with-open [output (io/output-stream path)]
    (.write output ^bytes bytes)))

(defn write-text! [path text]
  (spit path text))

(defn delete-file! [path]
  (java.nio.file.Files/deleteIfExists (.toPath (fs/file path))))

(defn canonicalize [path]
  (fs/canonicalize (evidence-io/record-read! path)))

(defn exists? [path]
  (fs/exists? (evidence-io/record-read! path)))

(defn directory? [path]
  (fs/directory? (evidence-io/record-read! path)))

(defn file? [path]
  (fs/regular-file? (evidence-io/record-read! path)))

(defn executable? [path]
  (fs/executable? (evidence-io/record-read! path)))

(defn with-zip-file [archive f]
  (with-open [zip (ZipFile. (io/file (evidence-io/record-read! archive)))]
    (f zip)))

(defn load-jena-model [file]
  (RDFDataMgr/loadModel (str (evidence-io/record-read! file))))

(defn parse-xml-document [file]
  (let [factory (DocumentBuilderFactory/newInstance)]
    (.setNamespaceAware factory true)
    (.parse (.newDocumentBuilder factory)
            (io/file (evidence-io/record-read! file)))))

(defn relative-path [base file]
  (string/replace (str (fs/relativize (fs/path base) (fs/path file))) "\\" "/"))

(defn- sorted-path-seq*
  [root list-dir]
  (let [root (fs/path root)]
    (letfn [(walk [path descend?]
              (lazy-seq
               (cons path
                     (lazy-seq
                      (when (and descend? (fs/directory? path))
                        (mapcat #(walk % (not (fs/sym-link? %)))
                                (sort-by str (list-dir path))))))))]
      (walk root true))))

(defn sorted-path-seq
  "Return a deterministic lazy depth-first path sequence rooted at `root`.
  The root may be a directory symlink; descendant symlinks are yielded but
  never traversed. Directory-listing failures propagate."
  [root]
  (sorted-path-seq* root fs/list-dir))

(defn delete-tree!
  "Recursively delete `file`. No-op when it does not exist; throws on real
  filesystem failure (unlike the ignored .delete boolean it replaces)."
  [file]
  (when (fs/exists? file)
    (fs/delete-tree file)))

(defn copy-file! [source target]
  ;; Guard: (fs/parent bare-filename) is nil and (fs/create-dirs nil) throws NPE.
  (when-let [parent (fs/parent target)]
    (fs/create-dirs parent))
  (fs/copy (evidence-io/record-read! source) target {:replace-existing true})
  (fs/file target))

(defn bytes->hex [bytes]
  (hash/bytes->hex bytes))

(defn sha256-file [file]
  (hash/sha256-file file))

(defn example-hash [suffix]
  (str "sha256:"
       (apply str (repeat (- 64 (count suffix)) "0"))
       suffix))
