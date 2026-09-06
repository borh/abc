;; Record identities retain this deterministic JSON encoding. Its number
;; encoding follows charred; protocol identities use core.canonical instead.
(ns soranoha.core.jcs
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
(declare rfc8785-string-domain-json-string*)

(defn- invalid-utf16! [path position string-index code-unit]
  (throw (ex-info "RFC 8785 string contains malformed UTF-16"
                  {:reason :invalid-utf16
                   :path path
                   :position position
                   :string-index string-index
                   :code-unit (format "0x%04x" (int code-unit))})))

(defn- validate-utf16! [value path position]
  (loop [index 0]
    (when (< index (.length ^String value))
      (let [code-unit (.charAt ^String value index)]
        (cond
          (Character/isHighSurrogate code-unit)
          (if (and (< (inc index) (.length ^String value))
                   (Character/isLowSurrogate
                    (.charAt ^String value (inc index))))
            (recur (+ index 2))
            (invalid-utf16! path position index code-unit))

          (Character/isLowSurrogate code-unit)
          (invalid-utf16! path position index code-unit)

          :else
          (recur (inc index))))))
  value)

(defn- rfc8785-string [value path position]
  (validate-utf16! value path position)
  (json/write-json-str value
                       :escape-slash false
                       :escape-unicode false
                       :escape-js-separators false))

(defn- rfc8785-string-domain-object [value path]
  (doseq [key (keys value)]
    (when-not (string? key)
      (throw (ex-info "RFC 8785 string-domain object key must be a string"
                      {:reason :non-string-object-key
                       :path path
                       :key key}))))
  (str "{"
       (->> value
            (sort-by key)
            (map (fn [[k v]]
                   (str (rfc8785-string k path :object-key) ":"
                        (rfc8785-string-domain-json-string*
                         v (conj path k)))))
            (string/join ","))
       "}"))

(defn- rfc8785-string-domain-json-string* [value path]
  (cond
    (string? value) (rfc8785-string value path :value)
    (map? value) (rfc8785-string-domain-object value path)
    (sequential? value) (str "["
                             (->> value
                                  (map-indexed
                                   (fn [index item]
                                     (rfc8785-string-domain-json-string*
                                      item (conj path index))))
                                  (string/join ","))
                             "]")
    :else (throw (ex-info "RFC 8785 string-domain identity contains an unsupported scalar"
                          {:reason :unsupported-scalar
                           :path path
                           :value value}))))

(defn rfc8785-string-domain-json-string
  "RFC 8785 canonical JSON for identity objects whose scalar domain is strings.
  Numbers are rejected so callers cannot silently inherit the known ES6 number
  formatting gap in the historical canonicalizer. Every string and object key
  is rejected before serialization unless its UTF-16 is well-formed."
  [value]
  (rfc8785-string-domain-json-string* value []))

(defn rfc8785-string-domain-json-bytes [value]
  (.getBytes (rfc8785-string-domain-json-string value)
             StandardCharsets/UTF_8))
