(ns ab-research.parser-ir-vocabulary
  (:require [clojure.set :as set]))

(defn- definition-node-type [definition]
  (get-in definition ["allOf" 1 "properties" "type" "const"]))

(defn node-types [parser-ir-schema]
  (->> (get parser-ir-schema "$defs")
       vals
       (keep definition-node-type)
       set))

(defn coverage-errors [expected-node-types renderer-name covered-node-types]
  (let [missing-node-types (set/difference expected-node-types covered-node-types)
        unexpected-node-types (set/difference covered-node-types expected-node-types)]
    (->> (concat
          (map #(str renderer-name " renderer is missing parser-IR node policy for " %)
               (sort missing-node-types))
          (map #(str renderer-name " renderer has unexpected parser-IR node policy for " %)
               (sort unexpected-node-types)))
         vec)))
