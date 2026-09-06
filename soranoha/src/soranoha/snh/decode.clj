(ns soranoha.snh.decode
  "Boundary decode: the one reusable operation applied to each of the four
  release-level protocol JSON objects and to nothing else, on both the
  assembler and verifier sides:

    1. reject duplicate object keys at parse, before schema validation;
    2. parse without coercion (integral JSON numbers only — any float or
       lexically non-integral number fails);
    3. validate the parsed value against the type's JSON Schema;
    4. apply the type's single-object semantic boundary rules (real calendar
       dates, absolute origin) — cross-object and transition invariants stay
       with the chain verifier;
    5. canonicalize the same value and require the stored bytes to equal the
       canonical bytes — equivalent-but-noncanonical JSON is invalid;
    6. recompute the id from the stored bytes: snh:1:<type>:<sha256hex>.

  All other artifacts (e.g. tei-validation JSON) are exact published bytes
  checked by hash alone; they never pass through here."
  (:require [soranoha.core.canonical :as canonical]
            [soranoha.core.hash :as hash]
            [soranoha.snh.schema :as schema]
            [soranoha.snh.semantic :as semantic])
  (:import (java.nio.charset StandardCharsets)
           (java.util Arrays)
           (tools.jackson.core StreamReadFeature)
           (tools.jackson.databind JsonNode)
           (tools.jackson.databind.json JsonMapper)))

(def ^:private ^JsonMapper strict-mapper
  (let [builder (JsonMapper/builder)]
    (.enable builder ^"[Ltools.jackson.core.StreamReadFeature;"
             (into-array StreamReadFeature
                         [StreamReadFeature/STRICT_DUPLICATE_DETECTION]))
    (.build builder)))

(defn- reject! [reason type detail]
  (throw (ex-info (str "boundary decode rejected " type " object: " (name reason))
                  (assoc detail :reason reason :type type))))

(defn- node->value [^JsonNode node type]
  (cond
    (.isNull node) nil
    (.isBoolean node) (.booleanValue node)
    (.isTextual node) (.textValue node)
    (.isNumber node)
    (if (.isIntegralNumber node)
      (if (.canConvertToLong node)
        (.longValue node)
        (.bigIntegerValue node))
      (reject! :non-integral-number type {:token (str node)}))
    (.isArray node) (mapv #(node->value % type) node)
    (.isObject node) (into {}
                           (map (fn [entry]
                                  [(key entry) (node->value (val entry) type)]))
                           (.properties node))
    :else (reject! :unsupported-json-node type {:node-type (str (class node))})))

(defn parse-value
  "Parse JSON with duplicate-key rejection and integral numbers only.
  Schema validation and canonical-byte checks belong to the caller."
  [type ^bytes stored-bytes]
  (let [node (try (.readTree strict-mapper stored-bytes)
                  (catch Exception e
                    (reject! :parse-invalid type {:cause (.getMessage e)})))]
    (node->value node type)))

(defn decode
  "Boundary-decode `stored-bytes` as a protocol object of `type` (one of the
  four release-level registry types). Returns
  {:value <parsed> :hex <sha256 hex of stored bytes> :id \"snh:1:<type>:<hex>\"}.
  Throws ex-info with :reason on any rejection:
  :parse-invalid (malformed JSON or duplicate object key — detected at parse,
  before schema validation), :non-integral-number, :schema-invalid,
  :invalid-upstream-origin, :invalid-effective-date, :noncanonical."
  [type ^bytes stored-bytes]
  (when-not (contains? schema/schema-resources type)
    (throw (ex-info "boundary decode applies only to the four protocol JSON objects"
                    {:type type :known (keys schema/schema-resources)})))
  (let [value (parse-value type stored-bytes)]
    (when-let [errors (schema/validation-errors type value)]
      (reject! :schema-invalid type {:errors errors}))
    ((semantic/check-for type) value)
    (let [canonical-bytes (canonical/rfc8785-safe-integer-json-bytes-v1 value)]
      (when-not (Arrays/equals canonical-bytes stored-bytes)
        (reject! :noncanonical type
                 {:stored-length (alength stored-bytes)
                  :canonical-length (alength canonical-bytes)}))
      (let [hex (hash/sha256-bytes stored-bytes)]
        {:value value
         :hex hex
         :id (str "snh:1:" type ":" hex)}))))

(defn encode
  "Assembler-side inverse: canonical bytes + id for a protocol value of
  `type`. Round-trips through `decode` so an emitted object is by
  construction exactly what the verifier will accept."
  [type value]
  (let [bytes (canonical/rfc8785-safe-integer-json-bytes-v1 value)
        decoded (decode type bytes)]
    (assoc decoded :bytes bytes)))

(defn decode-string
  "Convenience for tests and fixtures: decode a JSON string's UTF-8 bytes."
  [type ^String s]
  (decode type (.getBytes s StandardCharsets/UTF_8)))
