(ns abc.tools.shacl
  "Wrap Jena SHACL validation. Returns structured violation maps;
  rendering for human output is the caller's responsibility."
  (:require [clojure.java.io :as io])
  (:import [org.apache.jena.rdf.model ModelFactory]
           [org.apache.jena.riot RDFDataMgr Lang]))

(def default-shapes-path "schemas/manifest.shacl.ttl")

(defn load-shapes-graph
  "Read the SHACL shapes file at `path` (default: schemas/manifest.shacl.ttl)
  into a Jena Graph."
  ([] (load-shapes-graph default-shapes-path))
  ([path]
   (let [model (ModelFactory/createDefaultModel)]
     (with-open [in (io/input-stream path)]
       (RDFDataMgr/read model in Lang/TURTLE))
     (.getGraph model))))
