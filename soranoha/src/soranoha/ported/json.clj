(ns soranoha.ported.json
  (:require [charred.api :as json]
            [clojure.java.io :as io]
            [clojure.string :as string])
  (:import [java.nio.file CopyOption Files StandardCopyOption]
           [java.nio.file.attribute FileAttribute PosixFilePermissions]))

(defn read-json-file [file]
  (json/read-json (io/file file)))

(defn prepare-deterministic-json [value]
  (cond
    (map? value)
    (into (sorted-map)
          (map (fn [[k v]]
                 [k (prepare-deterministic-json v)]))
          value)

    (vector? value)
    (mapv prepare-deterministic-json value)

    (sequential? value)
    (mapv prepare-deterministic-json value)

    :else
    value))

(defn write-deterministic-json-str
  "Deterministic sorted-key, indented, trailing-whitespace-trimmed JSON string
  with no trailing newline. Same normalization as write-deterministic-json-file!.
  Note: this is deterministic sorted-key JSON, not RFC 8785/JCS canonical JSON
  (see soranoha.ported.jcs for that)."
  [value]
  (string/replace
   (json/write-json-str (prepare-deterministic-json value) :indent-str "  ")
   #"[ \t]+(?=\r?\n)"
   ""))

(defn write-deterministic-json-file!
  "Atomically write deterministic JSON: content lands via a sibling temp file
  + ATOMIC_MOVE, so concurrent readers never observe a partial file and
  concurrent same-content writers race harmlessly (last move wins). The
  parallel per-work corpus loops depend on this: works sharing an author
  write the same persons/<person_id>.json."
  [file value]
  (io/make-parents file)
  (let [target (.toPath (io/file file))
        dir (or (.getParent target) (.toPath (io/file ".")))
        tmp (Files/createTempFile dir
                                  (str "." (.getFileName target) ".")
                                  ".tmp"
                                  (make-array FileAttribute 0))]
    (try
      (with-open [writer (io/writer (.toFile tmp))]
        (.write writer (write-deterministic-json-str value))
        (.write writer "\n"))
      ;; createTempFile creates owner-only (600) files; keep the historical
      ;; umask-style world-readable artifact bits.
      (try
        (Files/setPosixFilePermissions
         tmp (PosixFilePermissions/fromString "rw-r--r--"))
        (catch UnsupportedOperationException _))
      (Files/move tmp target
                  (into-array CopyOption [StandardCopyOption/ATOMIC_MOVE]))
      (catch Throwable t
        (Files/deleteIfExists tmp)
        (throw t))))
  file)
