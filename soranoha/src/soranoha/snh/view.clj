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
  snapshot; mirrors and clones wrap themselves.

  Reads run one git subprocess per call, except inside with-batch, which
  serves read-at from a single persistent cat-file --batch subprocess
  started under the same binding, flags, and sanitized environment. A
  batched read still names commit:path per request, so it proves the same
  path reachability from that one commit's tree as a spawned read."
  (:require [babashka.process :as process]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:import (java.io BufferedInputStream ByteArrayOutputStream DataInputStream
                    EOFException OutputStream)))

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

(defn- header-line
  "One LF-terminated cat-file --batch header line as a string."
  [^DataInputStream in]
  (let [buf (ByteArrayOutputStream.)]
    (loop []
      (let [b (.read in)]
        (cond
          (neg? b) (throw (EOFException. "batch reader closed mid-header"))
          (= b 10) (String. (.toByteArray buf) "UTF-8")
          :else (do (.write buf b) (recur)))))))

(defn- batch-read-at
  "One request/response exchange on the persistent batch subprocess. The
  exchange is atomic under the process lock, and the payload is always
  drained even when the object is not a blob, so the stream never
  desynchronizes."
  ^bytes [{:keys [^Process process ^OutputStream in ^DataInputStream out]}
          commit path]
  (locking process
    (.write in (.getBytes (str commit ":" path "\n") "UTF-8"))
    (.flush in)
    (let [header (header-line out)]
      (when-not (or (str/ends-with? header " missing")
                    (str/ends-with? header " ambiguous"))
        (let [[_ type size] (str/split header #" ")
              payload (byte-array (Long/parseLong size))]
          (.readFully out payload)
          (when-not (= 10 (.read out))
            (throw (EOFException. "batch reader desynchronized after payload")))
          (when (= "blob" type) payload))))))

(defn with-batch
  "Run (f batched-view): read-at on the passed view is served by one
  persistent cat-file --batch subprocess instead of one subprocess per
  read; the subprocess starts under the view's binding, hardening flags,
  and sanitized environment, with stderr inherited so it can neither
  fill a pipe nor disappear. On success the request stream is closed and
  the subprocess must terminate cleanly — spawned reads check every git
  exit, and a batch pass ends with the same obligation; on failure the
  subprocess is destroyed."
  [view f]
  (let [pb (doto (ProcessBuilder.
                  ^java.util.List (vec (concat ["git"] (:bind view)
                                               hardening-flags
                                               ["cat-file" "--batch"])))
             (.redirectError java.lang.ProcessBuilder$Redirect/INHERIT))]
    (.directory pb (io/file (:dir view)))
    (doto (.environment pb) (.clear) (.putAll (:env view)))
    (let [p (.start pb)
          in (.getOutputStream p)]
      (try
        (let [result (f (assoc view :batch
                               {:process p
                                :in in
                                :out (DataInputStream.
                                      (BufferedInputStream.
                                       (.getInputStream p)))}))]
          (.close in)
          (when-not (and (.waitFor p 10 java.util.concurrent.TimeUnit/SECONDS)
                         (zero? (.exitValue p)))
            (throw (ex-info "batch reader did not terminate cleanly"
                            {:alive (.isAlive p)
                             :exit (when-not (.isAlive p) (.exitValue p))})))
          result)
        (finally (.destroy p))))))

(defn read-at
  "Blob bytes at `path` in `commit`'s tree, or nil when absent. Only the tree
  of the named commit is consulted — presence of the same bytes elsewhere in
  the object graph does not satisfy a read."
  ^bytes [view commit path]
  (if-let [batch (:batch view)]
    (batch-read-at batch commit path)
    (let [{:keys [exit out]} (run-git view {:out :bytes}
                                      ["cat-file" "blob" (str commit ":" path)])]
      (when (zero? exit) out))))

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

(defn changed-paths
  "Set of repo paths under `prefix` whose entries differ between the trees
  of `commit-a` and `commit-b`; additions and removals differ by
  definition. A path absent from this set therefore names bit-identical
  tree entries in both commits — the tree comparison is itself a
  reachability proof for the path in each commit where it exists. Paths
  are NUL-delimited on the wire, so no quoting ambiguity arises."
  [view commit-a commit-b prefix]
  (let [{:keys [exit out err]} (run-git view {:out :string}
                                        ["diff-tree" "-r" "--name-only" "-z"
                                         "--no-commit-id" commit-a commit-b
                                         "--" prefix])]
    (when-not (zero? exit)
      (throw (ex-info "tree diff unreadable in view"
                      {:commits [commit-a commit-b] :err err})))
    (into #{} (remove str/blank?) (str/split out #"\x00"))))
