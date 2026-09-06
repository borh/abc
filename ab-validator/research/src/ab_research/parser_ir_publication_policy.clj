(ns ab-research.parser-ir-publication-policy
  (:require [ab-research.files :as files]
            [ab-research.hash :as hash]))

(defn load-policy [path]
  (files/read-json path))

(defn renderer-covered-node-types [policy renderer-name]
  (->> (get-in policy ["renderers" renderer-name "node_policies"])
       keys
       set))

(defn policy-hash [path]
  (hash/format-sha256
   (hash/sha256-json-jcs (load-policy path))))
