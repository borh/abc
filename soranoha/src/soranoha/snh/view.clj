(ns soranoha.snh.view
  "Commit-scoped repository view: the single, non-fallback read surface the
  verifier uses. A view wraps exactly one git object store and exposes reads
  of the form read-at(commit, path) and parents-of(commit); a path or object
  the view cannot serve is a nil/failure, never completed from any other
  source. Non-fallback is enforced, not assumed: every git invocation runs
  with --no-replace-objects and --no-lazy-fetch (replacement refs and
  promisor fetches would silently substitute or import objects), alternate
  object directories are rejected at construction, and the alternates
  environment override is cleared per invocation. Live verification wraps
  the fetched authoritative repository; archive verification wraps only the
  archived snapshot; mirrors and clones wrap themselves."
  (:require [babashka.process :as process]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(def ^:private hardening-flags ["--no-replace-objects" "--no-lazy-fetch"])

(def ^:private hardening-env
  {"GIT_ALTERNATE_OBJECT_DIRECTORIES" ""
   "GIT_NO_REPLACE_OBJECTS" "1"})

(defn- run-git [view opts args]
  (apply process/sh
         (merge {:dir (:dir view) :err :string :extra-env hardening-env} opts)
         "git" (concat hardening-flags args)))

(defn git-view
  "View over the repository at `dir` (a work tree or a bare/git directory).
  Throws when the repository declares alternate object directories — an
  alternates file would make reads span more than one object store."
  [dir]
  (let [v {:dir (str dir)}
        {:keys [exit out err]} (run-git v {:out :string}
                                        ["rev-parse" "--absolute-git-dir"])]
    (when-not (zero? exit)
      (throw (ex-info "not a git repository" {:dir (str dir) :err err})))
    (let [alternates (io/file (str/trim out) "objects" "info" "alternates")]
      (when (.exists alternates)
        (throw (ex-info "repository uses alternate object directories"
                        {:dir (str dir) :alternates (str alternates)}))))
    v))

(defn read-at
  "Blob bytes at `path` in `commit`'s tree, or nil when absent. Only the tree
  of the named commit is consulted — presence of the same bytes elsewhere in
  the object graph does not satisfy a read."
  ^bytes [view commit path]
  (let [{:keys [exit out]} (run-git view {:out :bytes}
                                    ["cat-file" "blob" (str commit ":" path)])]
    (when (zero? exit) out)))

(defn commit-exists? [view commit]
  (zero? (:exit (run-git view {:out :string}
                         ["cat-file" "-e" (str commit "^{commit}")]))))

(defn parents-of
  "Parent commit shas of `commit` (empty vector for a root commit). Throws
  when the commit itself cannot be read."
  [view commit]
  (let [{:keys [exit out err]} (run-git view {:out :string}
                                        ["rev-list" "--parents" "-n" "1" commit])]
    (when-not (zero? exit)
      (throw (ex-info "commit unreadable in view" {:commit commit :err err})))
    (vec (rest (str/split (str/trim out) #"\s+")))))

(defn branch-head
  "Current sha of `branch`, or nil when the branch does not exist."
  [view branch]
  (let [{:keys [exit out]} (run-git view {:out :string}
                                    ["rev-parse" "--verify" "--quiet"
                                     (str "refs/heads/" branch)])]
    (when (zero? exit) (str/trim out))))
