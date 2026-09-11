(ns soranoha.links.main
  (:require [soranoha.core.config :as config]
            [soranoha.links.assertion :as assertion]))

(defn export-files!
  "Read explicit assertions and write internal N-Quads; refuse overwrite."
  [paths base-iri out-path]
  (when-not (seq paths)
    (throw (ex-info "At least one external-link file is required" {})))
  (let [assertions (into [] (mapcat #(assertion/read-assertions (slurp % :encoding "UTF-8"))) paths)
        ids (vec (sort (distinct (map assertion/assertion-id assertions))))
        ^String text (assertion/nquads base-iri assertions)]
    (config/write-new! out-path text)
    {"result" "written" "assertions" ids "output" (str out-path)}))
