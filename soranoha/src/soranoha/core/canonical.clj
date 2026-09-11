(ns soranoha.core.canonical
  (:import [java.nio.charset StandardCharsets]
           [java.util ArrayList]))

(def ^:private max-safe-json-integer 9007199254740991N)

;; The canonical form is written into one StringBuilder rather than assembled
;; from a string per object and array. The output is identical; what changes is
;; that a manifest is no longer rebuilt once per nesting level on the way out.
;;
;; `path` is the same idea applied to error reporting. It names where a
;; rejection happened, so it has to be exact, but a vector conjed at every node
;; is allocated for the whole document to describe the one node that fails.
;; Here it is a stack pushed and popped around each descent and read only when
;; something is thrown.

(defn- path-of [^ArrayList path] (vec path))

(defn- invalid-utf16! [path position string-index code-unit]
  (throw (ex-info "RFC 8785 string contains malformed UTF-16"
                  {:reason :invalid-utf16
                   :path (path-of path)
                   :position position
                   :string-index string-index
                   :code-unit (format "0x%04x" (int code-unit))})))

(defn validate-utf16!
  "Throw on a lone surrogate in `value`; return `value` otherwise. `path` and
  `position` locate the string in the error."
  [^String value path position]
  (let [length (.length value)]
    (loop [index 0]
      (when (< index length)
        (let [code-unit (.charAt value index)]
          (cond
            (Character/isHighSurrogate code-unit)
            (if (and (< (inc index) length)
                     (Character/isLowSurrogate (.charAt value (inc index))))
              (recur (+ index 2))
              (invalid-utf16! path position index code-unit))

            (Character/isLowSurrogate code-unit)
            (invalid-utf16! path position index code-unit)

            :else
            (recur (inc index)))))))
  value)

(def ^:private ^String hex-digits "0123456789abcdef")

(defn- escapes? [^long code]
  (or (= code 0x22) (= code 0x5c) (< code 0x20)))

(defn- append-escaped! [^StringBuilder sb ^String value ^long from]
  (let [length (.length value)]
    (loop [index from]
      (when (< index length)
        (let [c (.charAt value index)
              code (int c)]
          (cond
            (= c \") (.append sb "\\\"")
            (= c \\) (.append sb "\\\\")
            (= code 0x08) (.append sb "\\b")
            (= code 0x09) (.append sb "\\t")
            (= code 0x0a) (.append sb "\\n")
            (= code 0x0c) (.append sb "\\f")
            (= code 0x0d) (.append sb "\\r")
            (< code 0x20) (doto sb
                            (.append "\\u00")
                            (.append (.charAt hex-digits (bit-shift-right code 4)))
                            (.append (.charAt hex-digits (bit-and code 0xf))))
            :else (.append sb c)))
        (recur (inc index))))))

(defn- append-string!
  "Serialize one JSON string. RFC 8785 leaves this to RFC 8259's shortest
  form: escape only the quote, the reverse solidus and C0, with lowercase
  hex. Nothing else, which is why the solidus, DEL, U+2028 and U+2029 and
  every astral character go through as themselves.

  Written out here rather than delegated to a general JSON writer because a
  manifest at corpus scale holds a few hundred thousand strings and that
  writer pays its setup on each one. What keeps the two forms identical is
  `unicode-literals-match-the-pinned-escaping-engine`, which compares them
  over every BMP code point and an astral pair."
  [^StringBuilder sb ^String value path position]
  (validate-utf16! value path position)
  (.append sb \")
  (let [length (.length value)]
    (loop [index 0]
      (cond
        (= index length) (.append sb value)
        (escapes? (int (.charAt value index)))
        (do (.append sb value 0 index)
            (append-escaped! sb value index))
        :else (recur (inc index)))))
  (.append sb \"))

(defn- unsupported-rfc8785-number! [value path]
  (throw (ex-info "RFC 8785 v1 number is outside the supported JSON domain"
                  {:reason :unsupported-rfc8785-number
                   :path (path-of path)
                   :value value})))

(defn- append-number! [^StringBuilder sb value path]
  (if (and (integer? value)
           (<= (- max-safe-json-integer) value max-safe-json-integer))
    (.append sb (str value))
    (unsupported-rfc8785-number! value path)))

(declare append-value!)

(defn- append-object! [^StringBuilder sb value ^ArrayList path]
  (doseq [key (keys value)]
    (when-not (string? key)
      (throw (ex-info "RFC 8785 object key must be a string"
                      {:reason :non-string-object-key
                       :path (path-of path)
                       :key key}))))
  (.append sb "{")
  (let [depth (.size path)]
    (loop [entries (sort-by key value)
           first? true]
      (when-let [[k item] (first entries)]
        (when-not first? (.append sb ","))
        (append-string! sb k path :object-key)
        (.append sb ":")
        (.add path k)
        (append-value! sb item path)
        (.remove path depth)
        (recur (next entries) false))))
  (.append sb "}"))

(defn- append-array! [^StringBuilder sb value ^ArrayList path]
  (.append sb "[")
  (let [depth (.size path)]
    (loop [items (seq value)
           index 0]
      (when items
        (when (pos? index) (.append sb ","))
        (.add path index)
        (append-value! sb (first items) path)
        (.remove path depth)
        (recur (next items) (inc index)))))
  (.append sb "]"))

(defn- append-value! [^StringBuilder sb value ^ArrayList path]
  (cond
    (nil? value) (.append sb "null")
    (true? value) (.append sb "true")
    (false? value) (.append sb "false")
    (string? value) (append-string! sb value path :value)
    (number? value) (append-number! sb value path)
    (map? value) (append-object! sb value path)
    (sequential? value) (append-array! sb value path)
    :else (throw (ex-info "Unsupported RFC 8785 JSON value"
                          {:reason :unsupported-json-value
                           :path (path-of path)
                           :value value}))))

(defn rfc8785-safe-integer-json-string-v1
  "RFC 8785 canonical JSON restricted to integral numbers in the interoperable
  range [-9007199254740991, 9007199254740991]. Every other numeric host value
  fails closed."
  [value]
  (let [sb (StringBuilder.)]
    (append-value! sb value (ArrayList.))
    (.toString sb)))

(defn rfc8785-safe-integer-json-bytes-v1 ^bytes [value]
  (.getBytes ^String (rfc8785-safe-integer-json-string-v1 value)
             StandardCharsets/UTF_8))
