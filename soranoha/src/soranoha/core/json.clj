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

  Escaping is the minimum JSON requires. A solidus is written as itself, and a
  character outside ASCII is written as itself in UTF-8 rather than as a \\u
  pair: these files are read by people as well as by programs, and a Japanese
  title spelled `\\u8718\\u86db\\u306e\\u7cf8` is unreadable to the audience the
  corpus is for. It also matches what the rest of the publication emits: the
  signed protocol records go through `soranoha.core.canonical`, which is RFC
  8785 and escapes neither, and the source-accountability scanner writes raw
  UTF-8 too.

  Note: this is deterministic sorted-key JSON, not RFC 8785/JCS canonical JSON
  (see soranoha.core.jcs for that)."
  [value]
  (string/replace
   (json/write-json-str (prepare-deterministic-json value)
                        :indent-str "  " :escape-slash false :escape-unicode false)
   #"[ \t]+(?=\r?\n)"
   ""))
