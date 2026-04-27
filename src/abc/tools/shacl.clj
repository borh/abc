(ns abc.tools.shacl
  "Wrap Jena SHACL validation. Returns structured violation maps;
  rendering for human output is the caller's responsibility."
  (:require [clojure.java.io :as io])
  (:import [org.apache.jena.rdf.model ModelFactory]
           [org.apache.jena.riot RDFDataMgr Lang]
           [org.apache.jena.shacl ShaclValidator]
           [org.apache.jena.shacl.validation ReportEntry]))

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

(defn- entry->violation
  [^ReportEntry entry label]
  (let [severity (some-> (.severity entry) .getLocalName)
        focus    (some-> (.focusNode entry) str)
        path     (some-> (.resultPath entry) str)
        message  (.message entry)
        source   (some-> (.source entry) str)]
    (cond-> {:label label}
      severity (assoc :severity severity)
      focus    (assoc :focus-node focus)
      path     (assoc :path path)
      message  (assoc :message message)
      source   (assoc :source source))))

(defn validate!
  "Run SHACL validation of `data-graph` against `shapes-graph`. Returns :ok
  on conformance; throws ex-info with :errors set to a vector of structured
  violation maps when non-conformant.

  Required keys: :shapes-graph, :data-graph. Optional :label is attached
  to each violation map for caller-side rendering."
  [{:keys [shapes-graph data-graph label]}]
  (let [validator (ShaclValidator/get)
        report (.validate validator shapes-graph data-graph)]
    (if (.conforms report)
      :ok
      (let [violations (mapv #(entry->violation % label) (.getEntries report))]
        (throw (ex-info (str "SHACL validation failed: " label)
                        {:errors violations
                         :label label}))))))
