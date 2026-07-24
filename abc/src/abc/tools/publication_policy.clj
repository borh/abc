(ns abc.tools.publication-policy
  "Fail-closed release rights policy.

  The rights state in data/publication-policy.edn is one governed fact that
  the shared release evaluation consumes. `load-rights-authority!` reads the
  policy bytes exactly once and derives both the parsed EDN value and its
  content hash from those same bytes, so a caller can never report a hash that
  disagrees with the value it evaluated. `release-problem` is the pure
  projection of the allowed-state semantics used by
  abc.tools.publication-release/release-problems.

  `assert-release-allowed!` / `rights-publication-state` remain as compatibility
  projections for callers not yet routed through the shared predicate."
  (:require [abc.tools.files :as files]
            [abc.tools.hash :as hash]
            [babashka.fs :as fs]
            [clojure.edn :as edn])
  (:import [java.nio ByteBuffer]
           [java.nio.charset CodingErrorAction StandardCharsets]))

(def policy-path "data/publication-policy.edn")

;; The one rights-publication state that authorizes release publication. Every
;; other or missing state is fail-closed. Moving this value is a deliberate
;; governance change, not an implementation detail.
(def allowed-rights-publication-state :assessment-required)

(defn rights-publication-state
  "Compatibility projection: the :rights-publication state from the policy
  file. Retained until every caller moves to the value-plus-hash envelope."
  ([] (rights-publication-state policy-path))
  ([path] (:rights-publication (files/read-edn path))))

(defn- decode-strict-utf8
  "Decode bytes as EDN source, failing (rather than substituting) on malformed
  or unmappable byte sequences."
  [^bytes bytes]
  (let [decoder (doto (.newDecoder StandardCharsets/UTF_8)
                  (.onMalformedInput CodingErrorAction/REPORT)
                  (.onUnmappableCharacter CodingErrorAction/REPORT))]
    (str (.decode decoder (ByteBuffer/wrap bytes)))))

(defn load-rights-authority!
  "Read the rights-policy file's bytes exactly once, decode them as strict
  UTF-8, and derive BOTH the parsed EDN value and the exact content hash from
  that same byte array. Returns {:policy value :content-hash sha256}. Throws on
  an unreadable path or malformed EDN — an unreadable or malformed authority
  value is an error, never an accepted status."
  [path]
  (let [bytes (fs/read-all-bytes path)
        value (edn/read-string (decode-strict-utf8 bytes))]
    {:policy value
     :content-hash (hash/format-sha256 (hash/sha256-bytes bytes))}))

(defn release-problem
  "PURE. nil when the parsed rights-policy value authorizes release; otherwise a
  problem map. Preserves the allowed-state semantics of assert-release-allowed!."
  [rights-policy]
  (let [state (:rights-publication rights-policy)]
    (when-not (= allowed-rights-publication-state state)
      {:code "release-rights-blocked"
       :message (str "release publication blocked by rights policy: "
                     (pr-str (or state :missing-rights-publication-policy)))
       :actual (pr-str (or state :missing-rights-publication-policy))})))

(defn assert-release-allowed!
  "Return :ok only while the rights migration authorizes assessed publication.
  Every other or missing state fails closed and names the policy reason.
  Compatibility boundary retained until its callers route through the shared
  release predicate."
  ([] (assert-release-allowed! policy-path))
  ([path]
   (let [state (rights-publication-state path)]
     (if (= allowed-rights-publication-state state)
       :ok
       (throw (ex-info
               (str "release publication blocked by rights policy: " state)
               {:reason (or state :missing-rights-publication-policy)
                :policy-path path}))))))
