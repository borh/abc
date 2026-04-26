(ns abc.tools.files
  (:require [charred.api :as json]
            [clojure.java.io :as io]
            [clojure.string :as string])
  (:import [java.security MessageDigest]))

(def hash-pattern #"^sha256:[0-9a-f]{64}$")

(defn repo-root []
  (.getCanonicalFile (io/file ".")))

(defn path [& segments]
  (apply io/file (repo-root) segments))

(defn read-json [file]
  (json/read-json (io/file file)))

(defn read-json-lines [file]
  (->> (string/split-lines (slurp (io/file file)))
       (remove string/blank?)
       (mapv json/read-json)))

(defn bytes->hex [bytes]
  (apply str (map #(format "%02x" (bit-and % 0xff)) bytes)))

(defn sha256-file [file]
  (with-open [in (io/input-stream (io/file file))]
    (let [digest (MessageDigest/getInstance "SHA-256")
          buffer (byte-array 8192)]
      (loop []
        (let [n (.read in buffer)]
          (when (pos? n)
            (.update digest buffer 0 n)
            (recur))))
      (bytes->hex (.digest digest)))))

(defn example-hash [suffix]
  (str "sha256:"
       (apply str (repeat (- 64 (count suffix)) "0"))
       suffix))
