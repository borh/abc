(ns abc.tools.hash
  (:require [abc.tools.jcs :as jcs]
            [clojure.java.io :as io])
  (:import [java.security MessageDigest]))

(def hash-pattern #"^sha256:[0-9a-f]{64}$")

(defn bytes->hex [bytes]
  (apply str (map #(format "%02x" (bit-and % 0xff)) bytes)))

(defn sha256-bytes [bytes]
  (let [digest (MessageDigest/getInstance "SHA-256")]
    (.update digest bytes)
    (bytes->hex (.digest digest))))

(defn sha256-string [s]
  (sha256-bytes (.getBytes s "UTF-8")))

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

(defn format-sha256 [hex]
  (let [value (str "sha256:" hex)]
    (when-not (re-matches hash-pattern value)
      (throw (ex-info (str "Invalid sha256 hash: " value)
                      {:hash value})))
    value))

(defn parse-sha256 [value]
  (when-not (and (string? value)
                 (re-matches hash-pattern value))
    (throw (ex-info (str "Invalid sha256 hash: " value)
                    {:hash value})))
  (subs value (count "sha256:")))

(defn sha256-json-jcs [value]
  (sha256-bytes (jcs/canonical-json-bytes value)))

(defn byte-length [file]
  (.length (io/file file)))
