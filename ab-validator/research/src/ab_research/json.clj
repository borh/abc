(ns ab-research.json
  (:require [charred.api :as json]
            [clojure.java.io :as io]
            [soranoha.core.json :as record-json])
  (:import [java.nio.file CopyOption Files StandardCopyOption]
           [java.nio.file.attribute FileAttribute PosixFilePermissions]))

(defn write-deterministic-jsonl-line
  "Deterministic sorted-key single-line JSON (no indent), for JSONL rows.
  Matches the non-indented charred call at the JSONL emit sites."
  [value]
  (json/write-json-str (record-json/prepare-deterministic-json value)))

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
        (.write writer (record-json/write-deterministic-json-str value))
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
