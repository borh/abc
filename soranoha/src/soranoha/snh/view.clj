(ns soranoha.snh.view
  "Commit-scoped repository view: the single, non-fallback read surface the
  verifier uses. A view wraps exactly one git object store and exposes reads
  of the form read-at(commit, path) and parents-of(commit); a path or object
  the view cannot serve is a nil/failure, never completed from any other
  source. Non-fallback is enforced, not assumed: every git invocation runs
  with --no-replace-objects and --no-lazy-fetch (replacement refs and
  promisor fetches would silently substitute or import objects), under a
  sanitized environment with every GIT_-prefixed variable removed (inherited
  GIT_DIR / GIT_OBJECT_DIRECTORY / GIT_ALTERNATE_OBJECT_DIRECTORIES would
  redirect reads to a foreign object store), and bound explicitly to the git
  directory resolved at construction; linked worktrees (whose common
  directory differs from their git directory) and alternate object
  directories are rejected at construction. Live verification wraps the fetched
  authoritative repository; archive verification wraps only the archived
  snapshot; mirrors and clones wrap themselves."
  (:require [babashka.process :as process]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(def ^:private hardening-flags ["--no-replace-objects" "--no-lazy-fetch"])

(defn- sanitized-env
  "Copy of `base-env` with every GIT_-prefixed variable removed and object
  replacement disabled. The subprocess environment is replaced wholesale, so
  no inherited git redirect survives."
  [base-env]
  (assoc (into {}
               (remove (fn [[k _]] (str/starts-with? (str k) "GIT_")))
               base-env)
         "GIT_NO_REPLACE_OBJECTS" "1"))

(defn- run-git [view opts args]
  (apply process/sh
         (merge {:dir (:dir view) :err :string :env (:env view)} opts)
         "git" (concat (:bind view) hardening-flags args)))

(defn git-view
  "View over the repository at `dir` (a work tree or a bare/git directory).
  Discovery runs under the sanitized environment; the resolved absolute git
  directory is then bound with --git-dir on every read, so the view can only
  ever consult that one object store. Throws when the repository is a linked
  worktree — its per-worktree git directory hides the common directory's
  object store and alternates file — or declares alternate object
  directories, either of which would make reads span more than one object
  store. `base-env` defaults to the process environment and is a seam for
  probing the sanitization."
  ([dir] (git-view dir (into {} (System/getenv))))
  ([dir base-env]
   (let [v {:dir (str dir) :env (sanitized-env base-env) :bind []}
         {:keys [exit out err]} (run-git v {:out :string}
                                         ["rev-parse" "--path-format=absolute"
                                          "--git-dir" "--git-common-dir"])]
     (when-not (zero? exit)
       (throw (ex-info "not a git repository" {:dir (str dir) :err err})))
     (let [[git-dir common-dir] (str/split-lines (str/trim out))]
       (when (not= git-dir common-dir)
         (throw (ex-info "linked worktree: git and common directories differ"
                         {:dir (str dir) :git-dir git-dir
                          :common-dir common-dir})))
       (let [alternates (io/file git-dir "objects" "info" "alternates")]
         (when (.exists alternates)
           (throw (ex-info "repository uses alternate object directories"
                           {:dir (str dir) :alternates (str alternates)})))
         (assoc v :bind ["--git-dir" git-dir]))))))

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
