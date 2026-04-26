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
