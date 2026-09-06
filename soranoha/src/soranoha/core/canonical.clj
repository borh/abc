(ns soranoha.core.canonical
  (:require [charred.api :as json]
            [clojure.string :as string])
  (:import [java.nio.charset StandardCharsets]))

(declare rfc8785-safe-integer-json-string-v1*)

(def ^:private max-safe-json-integer 9007199254740991N)

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
