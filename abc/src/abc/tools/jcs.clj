;; Historical ABC canonical JSON serialization for structured JSON values.
;;
;; LIMITATION: Number canonicalization delegates to charred's json/write-json-str.
;; RFC 8785 section 3.2.2.2 requires strict ES6 floating-point serialization
;; (handling of -0, NaN, Infinity, scientific notation). For identity objects
;; that contain only strings and nulls (the current v0 design), this limitation
;; is latent. If non-integer numbers, BigDecimal, or BigInteger enter the
;; identity object, cross-implementation hash agreement may break.
;;
;; These functions predate the full RFC 8785 path below. Their bytes are frozen;
;; any change must be paired with a new manifest schema hash per ADR 0001.
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

;; Safe-integer JSON-domain RFC 8785 path for new protocols. Historical callers
;; of canonical-json-* above remain byte-for-byte frozen.
(declare rfc8785-safe-integer-json-string-v1*)

(def ^:private max-safe-json-integer 9007199254740991N)

(defn- unsupported-rfc8785-number! [value path]
  (throw (ex-info "RFC 8785 v1 number is outside the supported JSON domain"
                  {:reason :unsupported-rfc8785-number
                   :path path
                   :value value})))

(defn- rfc8785-number-v1 [value path]
  (if (and (integer? value)
           (<= (- max-safe-json-integer) value max-safe-json-integer))
    (str value)
    (unsupported-rfc8785-number! value path)))

(defn- rfc8785-object-v1 [value path]
  (doseq [key (keys value)]
    (when-not (string? key)
      (throw (ex-info "RFC 8785 object key must be a string"
                      {:reason :non-string-object-key
                       :path path
                       :key key}))))
  (str "{"
       (->> value
            (sort-by key)
            (map (fn [[key item]]
                   (str (rfc8785-string key path :object-key) ":"
                        (rfc8785-safe-integer-json-string-v1*
                         item (conj path key)))))
            (string/join ","))
       "}"))

(defn- rfc8785-safe-integer-json-string-v1* [value path]
  (cond
    (nil? value) "null"
    (true? value) "true"
    (false? value) "false"
    (string? value) (rfc8785-string value path :value)
    (number? value) (rfc8785-number-v1 value path)
    (map? value) (rfc8785-object-v1 value path)
    (sequential? value) (str "["
                             (->> value
                                  (map-indexed
                                   (fn [index item]
                                     (rfc8785-safe-integer-json-string-v1*
                                      item (conj path index))))
                                  (string/join ","))
                             "]")
    :else (throw (ex-info "Unsupported RFC 8785 JSON value"
                          {:reason :unsupported-json-value
                           :path path
                           :value value}))))

(defn rfc8785-safe-integer-json-string-v1
  "RFC 8785 canonical JSON restricted to integral numbers in the interoperable
  range [-9007199254740991, 9007199254740991]. Every other numeric host value
  fails closed."
  [value]
  (rfc8785-safe-integer-json-string-v1* value []))

(defn rfc8785-safe-integer-json-bytes-v1 [value]
  (.getBytes (rfc8785-safe-integer-json-string-v1 value)
             StandardCharsets/UTF_8))
