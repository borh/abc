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
