(ns abc.tools.path-containment
  (:require [babashka.fs :as fs])
  (:import [java.nio.file InvalidPathException Paths]))

(defn- raw-path [path]
  (Paths/get path (make-array String 0)))

(defn- traversal? [path]
  (some #(= ".." (str %)) (iterator-seq (.iterator path))))

(defn path-state
  "Resolve a repository-relative path without assigning caller policy.
  Returns one of :ok, :path-traversal, :malformed-path, :missing, or
  :real-path-escape."
  [repo-root path]
  (try
    (let [relative (raw-path path)
          root (fs/normalize (fs/absolutize repo-root))
          resolved (-> root (.resolve relative) .normalize)]
      (cond
        (.isAbsolute relative) {:state :path-traversal}
        (traversal? relative) {:state :path-traversal}
        (not (.startsWith resolved root)) {:state :path-traversal}
        (not (fs/exists? resolved)) {:state :missing}
        :else
        (let [real-root (.toRealPath root (make-array java.nio.file.LinkOption 0))
              real-path (.toRealPath resolved (make-array java.nio.file.LinkOption 0))]
          (if (.startsWith real-path real-root)
            {:state :ok
             :path (fs/file resolved)
             :relative (str relative)}
            {:state :real-path-escape}))))
    (catch InvalidPathException _
      {:state :malformed-path})))
