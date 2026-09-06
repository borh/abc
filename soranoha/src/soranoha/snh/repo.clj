(ns soranoha.snh.repo
  "Publication-repository plumbing: writing single-parent publication commits
  without a work tree, and the fast-forward-only push that serves as the
  compare-and-swap. Used by the transaction and by test fixtures (which also
  craft deliberately invalid commits through the same writer)."
  (:require [babashka.fs :as fs]
            [babashka.process :as process]
            [clojure.string :as str]))

(def ^:private ident-env
  ;; real commit time: two racers producing byte-identical content in the
  ;; same second may collide into one commit, and a push of that commit
  ;; reports up-to-date — which is state convergence, not a failure
  {"GIT_AUTHOR_NAME" "snh" "GIT_AUTHOR_EMAIL" "snh@localhost"
   "GIT_COMMITTER_NAME" "snh" "GIT_COMMITTER_EMAIL" "snh@localhost"})

(defn- git!
  [dir opts & args]
  (let [{:keys [exit err] :as r}
        (apply process/sh (merge {:dir (str dir) :out :string :err :string} opts)
               "git" args)]
    (when-not (zero? exit)
      (throw (ex-info (str "git " (first args) " failed")
                      {:args args :exit exit :err err})))
    r))

(defn init-origin!
  "Create a bare origin at `dir` with fast-forward-only, no-delete receive
  rules on the publication branch."
  [dir]
  (git! (fs/parent dir) {} "init" "-q" "--bare" (str (fs/file-name dir)))
  (git! dir {} "config" "receive.denyNonFastForwards" "true")
  (git! dir {} "config" "receive.denyDeletes" "true")
  (str dir))

(defn clone!
  [origin-dir dir]
  (git! (fs/parent dir) {} "clone" "-q" (str origin-dir) (str (fs/file-name dir)))
  (str dir))

(defn fetch!
  "Fetch `branch` from origin; returns its sha, or nil before genesis."
  [dir branch]
  (git! dir {} "fetch" "-q" "origin")
  (let [{:keys [exit out]}
        (process/sh {:dir (str dir) :out :string :err :string}
                    "git" "rev-parse" "--verify" "--quiet"
                    (str "refs/remotes/origin/" branch))]
    (when (zero? exit) (str/trim out))))

(defn- hash-blob!
  "Write `bytes` as a blob object; returns its git sha."
  [dir ^bytes bytes]
  (str/trim (:out (git! dir {:in bytes} "hash-object" "-w" "--stdin"))))

(defn write-commit!
  "Write a commit whose tree is `parent`'s tree (when given) with `files`
  applied on top, and `parents` (vector of shas; empty = root commit).
  `files` maps repo path -> byte array. Returns the commit sha. The work
  tree and real index are never touched."
  [dir {:keys [parents base-tree-of files message]
        :or {message "snh publication"}}]
  (let [index (str (fs/create-temp-file {:prefix "snh-index"}))]
    (try
      (fs/delete-if-exists (fs/path index))
      (let [env {"GIT_INDEX_FILE" index}]
        (when base-tree-of
          (git! dir {:extra-env env} "read-tree" (str base-tree-of "^{tree}")))
        (when (seq files)
          (let [entries (StringBuilder.)]
            (doseq [[path ^bytes bytes] (sort-by key files)]
              (when (str/includes? path "\u0000")
                (throw (ex-info "Git paths cannot contain NUL" {:path path})))
              (.append entries (str "100644 " (hash-blob! dir bytes) "\t" path "\u0000")))
            (let [{:keys [err]} (git! dir {:extra-env env
                                           :in (.getBytes (.toString entries) "UTF-8")}
                                      "update-index" "-z" "--index-info")]
              ;; Unlike --cacheinfo, --index-info silently skips invalid paths.
              (when-not (str/blank? err)
                (throw (ex-info "git update-index reported diagnostics" {:err err}))))))
        (let [tree (str/trim (:out (git! dir {:extra-env env} "write-tree")))
              args (concat ["commit-tree" tree]
                           (mapcat (fn [p] ["-p" p]) parents)
                           ["-m" message])]
          (str/trim (:out (apply git! dir {:extra-env (merge env ident-env)} args)))))
      (finally
        (fs/delete-if-exists (fs/path index))))))

(defn push!
  "Compare-and-swap push: advance origin's `branch` to `commit` iff the
  remote ref still equals `expected` (nil = branch must not exist yet).
  Returns :ok or :rejected."
  [dir branch commit expected]
  (let [lease (str "refs/heads/" branch ":" (or expected ""))
        {:keys [exit]}
        (process/sh {:dir (str dir) :out :string :err :string}
                    "git" "push" "-q" "origin"
                    (str commit ":refs/heads/" branch)
                    (str "--force-with-lease=" lease))]
    (if (zero? exit) :ok :rejected)))
