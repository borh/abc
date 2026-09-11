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

(defn- commit-blob!
  "Write the blob for `hex` through a temp file in its own directory and move
  it into place atomically, unless it already exists. `write!` receives the
  temp file. Returns `hex`."
  [cas-dir hex write!]
  (let [target (blob-path cas-dir hex)]
    (when-not (fs/exists? target)
      (fs/create-dirs (fs/parent target))
      (let [tmp (fs/create-temp-file {:dir (fs/parent target)
                                      :prefix (str "." hex ".")})]
        (write! tmp)
        (Files/move (fs/path tmp) (fs/path target)
                    (into-array java.nio.file.CopyOption
                                [StandardCopyOption/ATOMIC_MOVE]))))
    hex))

(defn put-bytes!
  "Store bytes; returns their sha256 hex. Existing blobs are never rewritten."
  [cas-dir ^bytes bytes]
  (commit-blob! cas-dir (hash/sha256-bytes bytes) #(io/copy bytes (fs/file %))))

(defn put-file!
  "Store a file's bytes by streaming copy; returns sha256 hex."
  [cas-dir source]
  (commit-blob! cas-dir (hash/sha256-file source) #(fs/copy source % {:replace-existing true})))

(defn get-bytes
  "Blob bytes for a hash, or nil when absent (cache miss, not an error)."
  ^bytes [cas-dir hex]
  (let [path (blob-path cas-dir hex)]
    (when (fs/exists? path)
      (fs/read-all-bytes path))))
