(ns soranoha.core.hash
  (:require [clojure.java.io :as io]
            [soranoha.core.canonical :as canonical])
  (:import [java.security MessageDigest]))

(def hex-pattern #"^[0-9a-f]{64}$")

(defn bytes->hex [^bytes bytes]
  (.formatHex (java.util.HexFormat/of) bytes))

(defn sha256-bytes
  "Lowercase hex sha256 over a byte array."
  [^bytes bytes]
  (let [digest (MessageDigest/getInstance "SHA-256")]
    (.update digest bytes)
    (bytes->hex (.digest digest))))

(defn sha256-string [^String s]
  (sha256-bytes (.getBytes s "UTF-8")))

(defn sha256-file [file]
  (with-open [in (io/input-stream (io/file file))]
    (let [digest (MessageDigest/getInstance "SHA-256")
          buffer (byte-array 65536)]
      (loop []
        (let [n (.read in buffer)]
          (when (pos? n)
            (.update digest buffer 0 n)
            (recur))))
      (bytes->hex (.digest digest)))))

(defn sha256-canonical-json
  "sha256 hex over the canonical bytes of a JSON value under the one
  canonicalizer (rfc8785-safe-integer-json-bytes-v1)."
  [value]
  (sha256-bytes (canonical/rfc8785-safe-integer-json-bytes-v1 value)))

(defn assert-hex64 [value]
  (when-not (and (string? value) (re-matches hex-pattern value))
    (throw (ex-info "Expected a 64-char lowercase hex sha256"
                    {:value value})))
  value)

(def hash-pattern #"^sha256:[0-9a-f]{64}$")

(defn bare-sha256-hex
  "The 64 hex digits of a `sha256:<hex>` string, or nil when `value` is not
  one. Callers decide what a malformed value means for them."
  [value]
  (some->> value (re-matches #"sha256:([0-9a-f]{64})") second))

(defn format-sha256 [hex]
  (let [value (str "sha256:" hex)]
    (when-not (re-matches hash-pattern value)
      (throw (ex-info (str "Invalid sha256 hash: " value)
                      {:hash value})))
    value))
