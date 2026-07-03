(ns abc.tools.aat-parser-ir-compat
  (:require [abc.tools.files :as files]
            [clojure.edn :as edn]))

(def registry-path
  (files/path "data" "aat-parser-ir-compatibility.edn"))

(def match-keys
  [:aat_version
   :aat_adapter
   :aat_adapter_version
   :mapping_id
   :mapping_version
   :mapping_hash
   :mapping_schema_hash
   :parser_ir_schema_id
   :parser_ir_schema_hash])

(defn load-registry
  []
  (edn/read-string (slurp registry-path)))

(defn compatible?
  [registry query]
  (boolean
   (some #(= (select-keys % match-keys)
             (select-keys query match-keys))
         (:entries registry))))
