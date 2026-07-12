(ns abc.tools.path-containment
  (:require [clojure.java.io :as io])
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
          root (-> (io/file repo-root) .toPath .toAbsolutePath .normalize)
          resolved (-> root (.resolve relative) .normalize)]
      (cond
        (.isAbsolute relative) {:state :path-traversal}
        (traversal? relative) {:state :path-traversal}
        (not (.startsWith resolved root)) {:state :path-traversal}
        (not (.exists (.toFile resolved))) {:state :missing}
        :else
        (let [real-root (.toRealPath root (make-array java.nio.file.LinkOption 0))
              real-path (.toRealPath resolved (make-array java.nio.file.LinkOption 0))]
          (if (.startsWith real-path real-root)
            {:state :ok
             :path (.toFile resolved)
             :relative (str relative)}
            {:state :real-path-escape}))))
    (catch InvalidPathException _
      {:state :malformed-path})))
