(ns abc.tools.files
  (:require [abc.tools.hash :as hash]
            [abc.tools.json :as abc-json]
            [charred.api :as json]
            [clojure.java.io :as io]
            [clojure.string :as string]))

(def hash-pattern hash/hash-pattern)

(defn repo-root []
  (.getCanonicalFile (io/file ".")))

(defn path [& segments]
  (apply io/file (repo-root) segments))

(defn read-json [file]
  (abc-json/read-json-file file))

(defn read-json-lines [file]
  (->> (string/split-lines (slurp (io/file file)))
       (remove string/blank?)
       (mapv json/read-json)))

(defn bytes->hex [bytes]
  (hash/bytes->hex bytes))

(defn sha256-file [file]
  (hash/sha256-file file))

(defn example-hash [suffix]
  (str "sha256:"
       (apply str (repeat (- 64 (count suffix)) "0"))
       suffix))
