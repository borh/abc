(ns abc.tools.hash
  (:require [abc.tools.jcs :as jcs]
            [abc.tools.evidence-io :as evidence-io]
            [charred.api :as json]
            [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.walk :as walk])
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
  (with-open [in (io/input-stream (io/file (evidence-io/record-read! file)))]
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

(defn sha256-json-rfc8785-safe-integer-v1 [value]
  (sha256-bytes (jcs/rfc8785-safe-integer-json-bytes-v1 value)))

(defn abc-legacy-json-c14n-v0
  "Encode the historical ABC compact sorted-key JSON representation."
  [value]
  (let [sorted (walk/postwalk #(if (map? %) (into (sorted-map) %) %) value)
        encoded (json/write-json-str sorted
                                     :escape-slash false
                                     :escape-unicode false)]
    (string/replace encoded "/" "\\/")))

(defn sha256-json-abc-legacy-v0
  "Hash historical ABC canonical JSON used by `abc_legacy_json_hash`.

  This protocol predates JCS. Keep it only for identities whose authoritative
  producer explicitly retains the legacy representation."
  [value]
  (format-sha256 (sha256-string (abc-legacy-json-c14n-v0 value))))

(defn byte-length [file]
  (.length (io/file (evidence-io/record-read! file))))
