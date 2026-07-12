;; RFC 8785 JCS canonicalization for structured JSON values.
;;
;; LIMITATION: Number canonicalization delegates to charred's json/write-json-str.
;; RFC 8785 section 3.2.2.2 requires strict ES6 floating-point serialization
;; (handling of -0, NaN, Infinity, scientific notation). For identity objects
;; that contain only strings and nulls (the current v0 design), this limitation
;; is latent. If non-integer numbers, BigDecimal, or BigInteger enter the
;; identity object, cross-implementation hash agreement may break.
;;
;; Any change to canonicalization must be paired with a new manifest schema
;; hash per ADR 0001.
;;
(ns abc.tools.jcs
  (:require [charred.api :as json]
            [clojure.string :as string])
  (:import [java.nio.charset StandardCharsets]))

(declare canonical-json-string)

(defn- canonical-json-object [value]
  (str "{"
       (->> value
            (sort-by key)
            (map (fn [[k v]]
                   (when-not (string? k)
                     (throw (ex-info "JCS object keys must be strings"
                                     {:key k})))
                   (str (json/write-json-str k) ":"
                        (canonical-json-string v))))
            (string/join ","))
       "}"))

(defn canonical-json-string [value]
  (cond
    (nil? value) "null"
    (or (true? value) (false? value)) (if value "true" "false")
    (string? value) (json/write-json-str value)
    (number? value) (json/write-json-str value)
    (map? value) (canonical-json-object value)
    (sequential? value) (str "["
                             (->> value
                                  (map canonical-json-string)
                                  (string/join ","))
                             "]")
    :else (throw (ex-info "Unsupported JCS JSON value"
                          {:value value}))))

(defn canonical-json-bytes [value]
  (.getBytes (canonical-json-string value) StandardCharsets/UTF_8))

;; Historical ABC identities use canonical-json-* above, including Charred's
;; legacy slash and non-ASCII escaping defaults. Do not change that behavior in
;; place: doing so would reinterpret already-published schema and artifact
;; hashes. New string-only identity constructions that explicitly require RFC
;; 8785 use this separate path instead.
(declare rfc8785-string-domain-json-string)

(defn- rfc8785-string [value]
  (json/write-json-str value
                       :escape-slash false
                       :escape-unicode false
                       :escape-js-separators false))

(defn- rfc8785-string-domain-object [value]
  (str "{"
       (->> value
            (sort-by key)
            (map (fn [[k v]]
                   (when-not (string? k)
                     (throw (ex-info "JCS object keys must be strings"
                                     {:key k})))
                   (str (rfc8785-string k) ":"
                        (rfc8785-string-domain-json-string v))))
            (string/join ","))
       "}"))

(defn rfc8785-string-domain-json-string
  "RFC 8785 canonical JSON for identity objects whose scalar domain is strings.
  Numbers are rejected so callers cannot silently inherit the known ES6 number
  formatting gap in the historical canonicalizer."
  [value]
  (cond
    (string? value) (rfc8785-string value)
    (map? value) (rfc8785-string-domain-object value)
    (sequential? value) (str "["
                             (->> value
                                  (map rfc8785-string-domain-json-string)
                                  (string/join ","))
                             "]")
    :else (throw (ex-info "RFC 8785 string-domain identity contains a non-string scalar"
                          {:value value}))))

(defn rfc8785-string-domain-json-bytes [value]
  (.getBytes (rfc8785-string-domain-json-string value)
             StandardCharsets/UTF_8))
