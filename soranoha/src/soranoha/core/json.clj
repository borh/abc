(ns soranoha.core.json
  (:require [charred.api :as json]
            [clojure.java.io :as io]
            [clojure.string :as string]))

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
  with no trailing newline.
  Note: this is deterministic sorted-key JSON, not RFC 8785/JCS canonical JSON
  (see soranoha.core.jcs for that)."
  [value]
  (string/replace
   (json/write-json-str (prepare-deterministic-json value) :indent-str "  ")
   #"[ \t]+(?=\r?\n)"
   ""))
