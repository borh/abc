(ns soranoha.core.rdf
  "Validated RDF terms and deterministic N-Quads lines."
  (:require [clojure.string :as str]
            [soranoha.core.canonical :as canonical])
  (:import [java.net URI URLEncoder]))

(def ^:private rdf "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
(def ^:private xsd "http://www.w3.org/2001/XMLSchema#")

(defn- validate-unicode! [s]
  ;; The canonicalizer rejects lone UTF-16 surrogates before Java can replace
  ;; them during UTF-8 encoding.
  (canonical/rfc8785-safe-integer-json-string-v1 s))

(defn iri [s]
  (validate-unicode! s)
  (when-not (and (string? s) (not (re-find #"[\x00-\x20<>\"{}|^`\\]" s))
                 (try (.isAbsolute (URI. s)) (catch Exception _ false)))
    (throw (ex-info "RDF requires an absolute IRI" {:reason :invalid-rdf-iri :value s})))
  (str "<" s ">"))

(defn component [s]
  (str/replace (URLEncoder/encode (str s) "UTF-8") "+" "%20"))

(defn literal [value datatype]
  (let [s (str value)]
    (validate-unicode! s)
    (str "\""
         (apply str (map (fn [c]
                           (case c
                             \" "\\\""
                             \\ "\\\\"
                             \newline "\\n"
                             \return "\\r"
                             \tab "\\t"
                             (if (< (int c) 32) (format "\\u%04X" (int c)) (str c))))
                         s))
         "\"^^" (iri datatype))))

(defn value-term [value]
  (cond
    (string? value) (literal value (str xsd "string"))
    (integer? value) (literal value (str xsd "integer"))
    (boolean? value) (literal value (str xsd "boolean"))
    :else (literal (canonical/rfc8785-safe-integer-json-string-v1 value)
                   (str rdf "JSON"))))

(defn quad [subject predicate object graph]
  (str (iri subject) " " (iri predicate) " " object
       (when graph (str " " (iri graph))) " .\n"))

