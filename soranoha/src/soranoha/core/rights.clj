(ns soranoha.core.rights
  "The rights policy document and the vocabulary its grant is written in.

  One file states the terms: soranoha/data/publication-policy.edn, whose
  bytes the release manifest already hashes as `admission.policy_hash`. Both
  places the grant is published — the manifest's `rights` field and each
  work's TEI `publicationStmt/availability` — read that one document through
  this namespace, so the terms signed off, the terms in the release record and
  the terms travelling inside a detached TEI file cannot disagree.

  The policy stores identifiers, not sentences. Rendering an identifier into
  prose is code, and belongs here rather than in the stored value: a rendered
  sentence held beside the identifier it came from can drift from it, and a
  published TEI file cannot be corrected afterwards."
  (:require [clojure.edn :as edn])
  (:import (java.nio ByteBuffer)
           (java.nio.charset CodingErrorAction StandardCharsets)))

(defn- strict-utf8
  "Decode bytes as UTF-8, failing (rather than substituting) on malformed
  or unmappable byte sequences."
  [^bytes bytes]
  (let [decoder (doto (.newDecoder StandardCharsets/UTF_8)
                  (.onMalformedInput CodingErrorAction/REPORT)
                  (.onUnmappableCharacter CodingErrorAction/REPORT))]
    (str (.decode decoder (ByteBuffer/wrap bytes)))))

(defn- read-one-edn
  "Read exactly one EDN value spanning the whole of `text`: an empty
  document, a second form, or trailing garbage after the value all fail —
  a reader that stops at the first value would hash bytes it never
  evaluated."
  [^String text]
  (with-open [reader (java.io.PushbackReader. (java.io.StringReader. text))]
    (let [eof (Object.)
          value (edn/read {:eof eof} reader)]
      (when (identical? value eof)
        (throw (ex-info "empty policy document" {})))
      (when-not (identical? eof (try (edn/read {:eof eof} reader)
                                     (catch Exception _ nil)))
        (throw (ex-info "trailing input after the policy value" {})))
      value)))

(defn read-policy
  "Policy value from its exact bytes. Callers that also hash the policy must
  pass the same byte array to both, so the value evaluated and the hash
  recorded cannot describe different documents."
  [^bytes policy-bytes]
  (try
    (read-one-edn (strict-utf8 policy-bytes))
    (catch Exception e
      (throw (ex-info "rights policy unreadable"
                      {:reason :policy-unreadable :cause (ex-message e)})))))

(defn grant
  "The published rights grant carried by a policy value, in the manifest's
  own key spelling. Fail-closed — a policy that authorizes publication
  without stating terms publishes nothing."
  [value]
  (let [{:keys [works encoding statement-url]} (:rights-statement value)]
    (when-not (and (string? works) (seq works)
                   (string? encoding) (seq encoding)
                   (string? statement-url) (seq statement-url))
      (throw (ex-info "rights policy states no publishable rights grant"
                      {:reason :missing-rights-statement})))
    {"works" works
     "encoding" encoding
     "statement_url" statement-url}))

(defn grant-from-bytes
  "Grant from policy bytes, for callers that need the terms without the
  release-authorization check."
  [^bytes policy-bytes]
  (grant (read-policy policy-bytes)))

;; Closed vocabularies. A grant naming a licence or a standing this build
;; cannot render is a policy change that has outrun the code, so it stops the
;; build rather than publishing artifacts whose stated terms are guesswork.

(def ^:private licence-uris
  {"CC0-1.0" "https://creativecommons.org/publicdomain/zero/1.0/"})

(def ^:private works-standing
  {"public-domain"
   {:uri "https://creativecommons.org/publicdomain/mark/1.0/"
    :statement (str "The underlying work is in the public domain; Soranoha "
                    "asserts no rights over it.")}})

(defn licence-uri
  "Canonical URI of the licence the encoding layer is published under."
  [encoding]
  (or (get licence-uris encoding)
      (throw (ex-info "rights policy names an unrenderable encoding licence"
                      {:reason :unknown-encoding-licence :encoding encoding}))))

(defn licence-statement
  "Prose form of the encoding licence, for readers rather than resolvers."
  [encoding]
  (str "Soranoha's encoding of this work, and the artifacts derived from it, "
       "are dedicated to the public domain under " encoding
       ". Attribution is requested, not required."))

(defn- works-standing! [works]
  (or (get works-standing works)
      (throw (ex-info "rights policy names an unrenderable works standing"
                      {:reason :unknown-works-standing :works works}))))

(defn works-uri
  "Canonical URI for the underlying work's rights standing."
  [works]
  (:uri (works-standing! works)))

(defn works-statement
  "Prose form of the underlying work's rights standing."
  [works]
  (:statement (works-standing! works)))
