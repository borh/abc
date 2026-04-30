(ns abc.tools.rdf-prefixes
  "Single source of truth for Aristotle prefix bindings used across
  manifest-to-rdf and metadata-record. Idempotent registration; lazy
  via delay so the registry update happens once on first reference."
  (:require [arachne.aristotle.registry :as reg]))

(def prefix-bindings
  "Ordered seq of [prefix-symbol uri] pairs used as ABC's RDF
  vocabulary surface. Single source of truth: graph builders register
  these via Aristotle, and serializers emit @prefix declarations from
  the same list, so no usage can reference a prefix the header
  doesn't declare."
  [['abc     "https://w3id.org/abc/"]
   ['bibo    "http://purl.org/ontology/bibo/"]
   ['dc      "http://purl.org/dc/elements/1.1/"]
   ['dcterms "http://purl.org/dc/terms/"]
   ['dcndl   "http://ndl.go.jp/dcndl/terms/"]
   ['foaf    "http://xmlns.com/foaf/0.1/"]
   ['prov    "http://www.w3.org/ns/prov#"]
   ['rdag2   "http://RDVocab.info/ElementsGr2/"]
   ['rdf     "http://www.w3.org/1999/02/22-rdf-syntax-ns#"]
   ['rdfs    "http://www.w3.org/2000/01/rdf-schema#"]
   ['schema  "https://schema.org/"]
   ['xsd     "http://www.w3.org/2001/XMLSchema#"]])

(defonce ^:private installed
  (delay
    (doseq [[sym uri] prefix-bindings]
      (reg/prefix sym uri))
    true))

(defn ensure!
  "Ensure ABC's RDF prefix bindings are registered with Aristotle.
  Returns true on success. Safe to call repeatedly."
  []
  @installed)

(defn turtle-prefix-declarations
  "Return a vector of '@prefix p: <uri> .' declaration strings, one
  per registered binding."
  []
  (mapv (fn [[sym uri]]
          (str "@prefix " (name sym) ": <" uri "> ."))
        prefix-bindings))

(defn resolve-curie
  "Resolve `prefix:local` through ABC's registered RDF prefix bindings.
  Returns nil when the value is not a CURIE or the prefix is unknown."
  [s]
  (when-let [[_ prefix local] (and (string? s)
                                   (re-matches #"^([^:]+):(.+)$" s))]
    (when-let [base (get (into {} (map (fn [[sym uri]] [(name sym) uri])
                                       prefix-bindings))
                         prefix)]
      (str base local))))
