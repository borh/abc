(ns abc.git
  (:require [clj-jgit.porcelain :refer :all
             #_[load-repo git-init git-pull git-log git-blame git-add-and-commit
                git-status git-tag-create git-tag-list]]
            [clojure.spec.alpha :as s]
            [abc.config :refer [aozora-bunko-path repo-path]])
  (:import [java.util Date]
           [org.eclipse.jgit.api Git]
           [org.eclipse.jgit.revwalk RevCommit]))

(defn load-aozora-bunko-git []
  (load-repo aozora-bunko-path))

(defn load-repo-git []
  (try (load-repo repo-path)
       (catch java.io.FileNotFoundException e
         (git-init repo-path))))

(def ^:dynamic ^:private *ab-repo* (load-aozora-bunko-git))
(def ^:dynamic ^:private *repo* (load-repo-git))

(defn update-aozora-bunko-repo [path]
  (git-pull *repo*))

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

(defn commit-tei [msg]
  (git-add-and-commit *repo* msg)
  (git-tag-create *repo* ))

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

(comment
  ;; Look into custom formatters: word formatters
  ;; https://github.com/someteam/acha/blob/master/src-clj/acha/git_parser.clj
  (defn diff-formatter
    [^Git repo]
    (doto (DiffFormatter. DisabledOutputStream/INSTANCE)
      (.setRepository (.getRepository repo))
      (.setDiffComparator RawTextComparator/DEFAULT)
      (.setDetectRenames true))))
