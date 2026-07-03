(ns abc.tools.parser-ir-vocabulary
  (:require [clojure.set :as set]))

(defn- definition-node-type [definition]
  (get-in definition ["allOf" 1 "properties" "type" "const"]))

(defn node-types [parser-ir-schema]
  (->> (get parser-ir-schema "$defs")
       vals
       (keep definition-node-type)
       set))

(defn coverage-errors [expected-node-types renderer-name covered-node-types]
  (->> (set/difference expected-node-types covered-node-types)
       sort
       (mapv #(str renderer-name " renderer is missing parser-IR node policy for " %))))
