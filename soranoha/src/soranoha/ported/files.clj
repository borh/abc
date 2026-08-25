(ns soranoha.ported.files
  (:require [soranoha.ported.hash :as hash]
            [soranoha.ported.json :as abc-json]
            [babashka.fs :as fs]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as string])
  (:import [java.util.zip ZipFile]
           [javax.xml.parsers DocumentBuilderFactory]
))

(def hash-pattern hash/hash-pattern)

(defn repo-root []
  (fs/file (fs/canonicalize ".")))

(defn path [& segments]
  (apply fs/file (repo-root) segments))

(defn read-json [file]
  (abc-json/read-json-file file))

(defn read-json-lines [file]
  (->> (string/split-lines (slurp (fs/file file)))
       (remove string/blank?)
       (mapv abc-json/read-json-str)))

(defn read-edn [file]
  (edn/read-string (slurp (fs/file file))))

(defn read-text [file]
  (slurp (fs/file file)))

(defn read-bytes [file]
  (fs/read-all-bytes file))

(defn input-stream [file]
  (io/input-stream file))

(defn reader [file]
  (io/reader file))

(defn list-files [directory]
  (->> (fs/list-dir directory)
       (filter fs/regular-file?)
       (sort-by str)
       (mapv fs/file)))

(defn list-files-if-directory [directory]
  (if (fs/directory? directory)
    (list-files directory)
    []))

(defn glob [root pattern]
  (vec (fs/glob root pattern)))

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
  (fs/canonicalize path))

(defn exists? [path]
  (fs/exists? path))

(defn directory? [path]
  (fs/directory? path))

(defn file? [path]
  (fs/regular-file? path))

(defn executable? [path]
  (fs/executable? path))

(defn with-zip-file [archive f]
  (with-open [zip (ZipFile. (io/file archive))]
    (f zip)))

(defn parse-xml-document [file]
  (let [factory (DocumentBuilderFactory/newInstance)]
    (.setNamespaceAware factory true)
    (.parse (.newDocumentBuilder factory)
            (io/file file))))

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
  (fs/copy source target {:replace-existing true})
  (fs/file target))

(defn bytes->hex [bytes]
  (hash/bytes->hex bytes))

(defn sha256-file [file]
  (hash/sha256-file file))

(defn example-hash [suffix]
  (str "sha256:"
       (apply str (repeat (- 64 (count suffix)) "0"))
       suffix))
