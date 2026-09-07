(ns soranoha.links.main
  (:require [soranoha.links.assertion :as assertion])
  (:import [java.nio.file Files OpenOption Path StandardOpenOption]))

(defn export-files!
  "Read explicit assertions and write internal N-Quads; refuse overwrite."
  [paths base-iri out-path]
  (when-not (seq paths)
    (throw (ex-info "At least one external-link file is required" {})))
  (let [assertions (into [] (mapcat #(assertion/read-assertions (slurp % :encoding "UTF-8"))) paths)
        ids (vec (sort (distinct (map assertion/assertion-id assertions))))
        ^String text (assertion/nquads base-iri assertions)]
    (Files/write (Path/of (str out-path) (make-array String 0))
                 (.getBytes text "UTF-8")
                 ^"[Ljava.nio.file.OpenOption;" (into-array OpenOption [StandardOpenOption/CREATE_NEW StandardOpenOption/WRITE]))
    {"result" "written" "assertions" ids "output" (str out-path)}))
