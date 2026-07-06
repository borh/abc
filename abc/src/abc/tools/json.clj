(ns abc.tools.json
  (:require [charred.api :as json]
            [clojure.java.io :as io]))

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

(defn write-deterministic-json-file! [file value]
  (io/make-parents file)
  (with-open [writer (io/writer file)]
    (.write writer (json/write-json-str (prepare-deterministic-json value)
                                        :indent-str "  "))
    (.write writer "\n"))
  file)
