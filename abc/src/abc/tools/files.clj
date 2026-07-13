(ns abc.tools.files
  (:require [abc.tools.hash :as hash]
            [abc.tools.json :as abc-json]
            [babashka.fs :as fs]
            [clojure.edn :as edn]
            [clojure.string :as string]))

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
