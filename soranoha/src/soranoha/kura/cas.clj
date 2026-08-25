;; Content-addressed store: objects/sha256/<hex[0:2]>/<hex>, append-only.
;; No GC exists in this kernel: a missing blob is a recoverable cache
;; miss, never corruption. Writes are atomic (temp file + ATOMIC_MOVE) so a
;; concurrent duplicate put is harmless.
(ns soranoha.kura.cas
  (:require [babashka.fs :as fs]
            [clojure.java.io :as io]
            [soranoha.core.hash :as hash])
  (:import [java.nio.file Files StandardCopyOption]))

(defn blob-path [cas-dir hex]
  (hash/assert-hex64 hex)
  (str (fs/path cas-dir (subs hex 0 2) hex)))

(defn has-blob? [cas-dir hex]
  (fs/exists? (blob-path cas-dir hex)))

(defn put-bytes!
  "Store bytes; returns their sha256 hex. Existing blobs are never rewritten."
  [cas-dir ^bytes bytes]
  (let [hex (hash/sha256-bytes bytes)
        target (blob-path cas-dir hex)]
    (when-not (fs/exists? target)
      (fs/create-dirs (fs/parent target))
      (let [tmp (fs/create-temp-file {:dir (fs/parent target)
                                      :prefix (str "." hex ".")})]
        (io/copy bytes (fs/file tmp))
        (Files/move (fs/path tmp) (fs/path target)
                    (into-array java.nio.file.CopyOption
                                [StandardCopyOption/ATOMIC_MOVE]))))
    hex))

(defn put-file!
  "Store a file's bytes by streaming copy; returns sha256 hex."
  [cas-dir source]
  (let [hex (hash/sha256-file source)
        target (blob-path cas-dir hex)]
    (when-not (fs/exists? target)
      (fs/create-dirs (fs/parent target))
      (let [tmp (fs/create-temp-file {:dir (fs/parent target)
                                      :prefix (str "." hex ".")})]
        (fs/copy source tmp {:replace-existing true})
        (Files/move (fs/path tmp) (fs/path target)
                    (into-array java.nio.file.CopyOption
                                [StandardCopyOption/ATOMIC_MOVE]))))
    hex))

(defn get-bytes
  "Blob bytes for a hash, or nil when absent (cache miss, not an error)."
  ^bytes [cas-dir hex]
  (let [path (blob-path cas-dir hex)]
    (when (fs/exists? path)
      (fs/read-all-bytes path))))
