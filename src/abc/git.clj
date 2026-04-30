(ns abc.git
  (:require [clj-jgit.porcelain :as git :refer
             [load-repo git-init git-pull git-log git-blame git-add git-commit
              git-status git-tag-create git-tag-list]]
            [clojure.spec.alpha :as s]
            [abc.config :refer [aozora-bunko-path repo-path]])
  (:import [java.util Date]
           [org.eclipse.jgit.lib Repository]
           [org.eclipse.jgit.api Git]
           [org.eclipse.jgit.revwalk RevCommit RevWalk]
           [org.eclipse.jgit.treewalk TreeWalk]
           [org.eclipse.jgit.treewalk.filter PathFilter]))

(defn load-aozora-bunko-git []
  (load-repo aozora-bunko-path))

(defn load-repo-git []
  (try (load-repo repo-path)
       (catch java.io.FileNotFoundException e
         (git-init :dir repo-path))))

(def ^:dynamic ^:private *ab-repo* nil)
(def ^:dynamic ^:private *repo* nil)

(defn load-git-repo [path]
  (load-repo path))

(defn- repo-or-default []
  (or *repo* (load-repo-git)))

(defn- ab-repo-or-default []
  (or *ab-repo* (load-aozora-bunko-git)))

(defn update-aozora-bunko-repo [path]
  (if path
    (with-open [repo (load-git-repo path)]
      (git-pull repo))
    (git-pull (ab-repo-or-default))))

(defn get-commit-date [^RevCommit commit]
  (.. commit (getAuthorIdent) (getWhen)))

(s/fdef git-file-log
  :args (s/cat :repo #(instance? Git %) :path string?)
  :ret (s/coll-of map?))
(defn get-file-log [^Git repo path]
  (map :commit (sort-by
                (fn [commit-map] (get-commit-date (:commit commit-map)))
                (comp - compare)
                (git-blame repo path))))

;; commit.getAuthorIdent().getWhen()).reversed()

(defn current-tag-version [^Git repo]
  (first (git-tag-list repo)))

(defn current-commit [^Git repo]
  (git-log repo))

(defn commit-tei [file msg]
  (let [repo (repo-or-default)]
    (git-add repo file)
    (git-commit repo file msg)
    (git-tag-create repo msg)))

(defn resolve-ref
  "Resolve `ref` in `repo` to an object id, or throw ex-info."
  [^Git repo ref]
  (let [repository (.getRepository repo)]
    (or (.resolve repository ref)
        (throw (ex-info (str "git ref not found: " ref)
                        {:ref ref})))))

(defn blob-bytes-at
  "Return the bytes of `path` at commit-ish `ref` in `repo`.

  This is the shared git boundary for tools that need historical file
  content. It uses JGit directly rather than shelling out to `git show`."
  [^Git repo ref path]
  (let [^Repository repository (.getRepository repo)
        object-id (resolve-ref repo ref)]
    (with-open [walk (RevWalk. repository)
                tree-walk (TreeWalk. repository)]
      (let [commit (.parseCommit walk object-id)]
        (.addTree tree-walk (.getTree commit))
        (.setRecursive tree-walk true)
        (.setFilter tree-walk (PathFilter/create path))
        (if (.next tree-walk)
          (-> repository
              (.open (.getObjectId tree-walk 0))
              .getBytes)
          (throw (ex-info (str "git path not found at ref: " path)
                          {:ref ref :path path})))))))

(defn write-blob-at!
  "Write `path` at `ref` from `repo` to `output-file` and return the file."
  [^Git repo ref path output-file]
  (let [file (clojure.java.io/file output-file)]
    (clojure.java.io/make-parents file)
    (with-open [out (clojure.java.io/output-stream file)]
      (.write out (blob-bytes-at repo ref path)))
    file))

;; Time travel

(s/fdef file-time-span
  :args (s/cat :repo #(instance? Git %) :file string? :from-time inst? :to-time inst?)
  :ret (s/coll-of #(instance? RevCommit %)))
(defn file-time-span
  [^Git repo
   ^String file
   ^Date from-time
   ^Date to-time]
  (->> file
       (get-file-log repo)
       (drop-while (fn [commit]
                     #_(println to-time (get-commit-date commit) (compare (get-commit-date commit) to-time))
                     (pos? (compare (get-commit-date commit) to-time))))
       (take-while (fn [commit]
                     #_(println (compare from-time (get-commit-date commit)))
                     (neg? (compare from-time (get-commit-date commit)))))))

;; https://chris.beams.io/posts/git-commit/
;;
(defn to-conventional-commit
  "https://www.conventionalcommits.org/en/v1.0.0-beta.3/"
  [msg])

(comment
  ;; Look into custom formatters: word formatters
  ;; https://github.com/someteam/acha/blob/master/src-clj/acha/git_parser.clj
  (defn diff-formatter
    [^Git repo]
    (doto (DiffFormatter. DisabledOutputStream/INSTANCE)
      (.setRepository (.getRepository repo))
      (.setDiffComparator RawTextComparator/DEFAULT)
      (.setDetectRenames true))))
