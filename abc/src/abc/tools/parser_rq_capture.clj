(ns abc.tools.parser-rq-capture
  "Contracts for committed parser-qualification capture metadata and rebinding
  logical blob identities to an explicitly configured external store."
  (:require [abc.tools.hash :as hash]
            [clojure.java.io :as io]
            [malli.core :as m]))

(def sha256-schema
  [:fn {:error/message "must be a sha256 logical blob identity"}
   #(and (string? %) (re-matches hash/hash-pattern %))])

(def blob-reference-schema
  [:map {:closed true}
   [:sha256 sha256-schema]
   [:bytes [:int {:min 0}]]
   [:media_type [:string {:min 1}]]])

(def denominator-schema
  [:map {:closed true}
   [:value [:int {:min 0}]]
   [:unit [:string {:min 1}]]])

(def manifest-schema
  [:map {:closed true}
   [:blob blob-reference-schema]
   [:denominator {:optional true} denominator-schema]])

(def envelope-schema
  [:map {:closed true}
   [:value :any]
   [:identity_ref sha256-schema]])

(defn- validation-errors
  [schema value label]
  (if (m/validate schema value)
    []
    [(str label " violates its closed contract: "
          (pr-str (m/explain schema value)))]))

(defn manifest-errors
  [manifest]
  (validation-errors manifest-schema manifest "capture manifest; expected sha256 logical blob identity"))

(defn envelope-errors
  [envelope]
  (validation-errors envelope-schema envelope "observation envelope"))

(defn observation-value
  [envelope]
  (:value envelope))

(defn- unavailable
  [reason]
  {:status :unavailable :reason reason})

(defn verify-blob
  "Resolve `locator` below the runtime-configured store root, then stream and
  re-hash it. Store metadata is never trusted. Missing, escaping, or mismatched
  blobs are unavailable and cannot yield an observation."
  [{:keys [root]} blob-ref locator]
  (try
    (let [root-file (.getCanonicalFile (io/file root))
          blob-file (.getCanonicalFile (io/file root-file locator))
          root-path (.toPath root-file)
          blob-path (.toPath blob-file)]
      (cond
        (not (.startsWith blob-path root-path))
        (unavailable "blob locator escapes the configured store root")

        (not (.isFile blob-file))
        (unavailable "blob is absent from the configured store")

        (not= (:bytes blob-ref) (hash/byte-length blob-file))
        (unavailable "blob byte length does not match its logical identity")

        (not= (:sha256 blob-ref)
              (hash/format-sha256 (hash/sha256-file blob-file)))
        (unavailable "blob hash does not match its logical identity")

        :else
        {:status :ok}))
    (catch Exception error
      (unavailable (.getMessage error)))))
