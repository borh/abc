(ns abc.tools.json
  (:require [charred.api :as json]
            [clojure.java.io :as io]
            [clojure.string :as string]))

(defn read-json-file [file]
  (json/read-json (io/file file)))

(defn read-json-str [s]
  (json/read-json s))

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
  (see abc.tools.jcs for that)."
  [value]
  (string/replace
   (json/write-json-str (prepare-deterministic-json value) :indent-str "  ")
   #"[ \t]+(?=\r?\n)"
   ""))

(defn write-deterministic-jsonl-line
  "Deterministic sorted-key single-line JSON (no indent), for JSONL rows.
  Matches the non-indented charred call at the JSONL emit sites."
  [value]
  (json/write-json-str (prepare-deterministic-json value)))

(defn write-deterministic-json-file! [file value]
  (io/make-parents file)
  (with-open [writer (io/writer file)]
    (.write writer (write-deterministic-json-str value))
    (.write writer "\n"))
  file)
