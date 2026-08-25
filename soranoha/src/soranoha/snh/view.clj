(ns soranoha.snh.view
  "Commit-scoped repository view: the single, non-fallback read surface the
  verifier uses. A view wraps exactly one git directory and exposes reads of
  the form read-at(commit, path) and parents-of(commit); a path or object the
  view cannot serve is a nil/failure, never completed from any other source.
  Live verification wraps the fetched authoritative repository; archive
  verification wraps only the archived snapshot; mirrors and clones wrap
  themselves."
  (:require [babashka.process :as process]
            [clojure.string :as str]))

(defn git-view
  "View over the repository at `dir` (a work tree or a bare/git directory)."
  [dir]
  {:dir (str dir)})

(defn- git-bytes [view & args]
  (apply process/sh {:dir (:dir view) :out :bytes :err :string} "git" args))

(defn- git-str [view & args]
  (apply process/sh {:dir (:dir view) :out :string :err :string} "git" args))

(defn read-at
  "Blob bytes at `path` in `commit`'s tree, or nil when absent. Only the tree
  of the named commit is consulted — presence of the same bytes elsewhere in
  the object graph does not satisfy a read."
  ^bytes [view commit path]
  (let [{:keys [exit out]} (git-bytes view "cat-file" "blob" (str commit ":" path))]
    (when (zero? exit) out)))

(defn commit-exists? [view commit]
  (zero? (:exit (git-str view "cat-file" "-e" (str commit "^{commit}")))))

(defn parents-of
  "Parent commit shas of `commit` (empty vector for a root commit). Throws
  when the commit itself cannot be read."
  [view commit]
  (let [{:keys [exit out err]} (git-str view "rev-list" "--parents" "-n" "1" commit)]
    (when-not (zero? exit)
      (throw (ex-info "commit unreadable in view" {:commit commit :err err})))
    (vec (rest (str/split (str/trim out) #"\s+")))))

(defn branch-head
  "Current sha of `branch`, or nil when the branch does not exist."
  [view branch]
  (let [{:keys [exit out]} (git-str view "rev-parse" "--verify" "--quiet"
                                    (str "refs/heads/" branch))]
    (when (zero? exit) (str/trim out))))
