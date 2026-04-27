(ns abc.tools.rdf-prefixes
  "Single source of truth for Aristotle prefix bindings used across
  manifest-to-rdf and metadata-record. Idempotent registration; lazy
  via delay so the registry update happens once on first reference."
  (:require [arachne.aristotle.registry :as reg]))

(defonce ^:private installed
  (delay
    (reg/prefix 'abc     "https://w3id.org/abc/")
    (reg/prefix 'bibo    "http://purl.org/ontology/bibo/")
    (reg/prefix 'dc      "http://purl.org/dc/elements/1.1/")
    (reg/prefix 'dcterms "http://purl.org/dc/terms/")
    (reg/prefix 'dcndl   "http://ndl.go.jp/dcndl/terms/")
    (reg/prefix 'foaf    "http://xmlns.com/foaf/0.1/")
    (reg/prefix 'prov    "http://www.w3.org/ns/prov#")
    (reg/prefix 'rdag2   "http://RDVocab.info/ElementsGr2/")
    (reg/prefix 'rdfs    "http://www.w3.org/2000/01/rdf-schema#")
    (reg/prefix 'schema  "https://schema.org/")
    (reg/prefix 'xsd     "http://www.w3.org/2001/XMLSchema#")
    true))

(defn ensure!
  "Ensure ABC's RDF prefix bindings are registered with Aristotle.
  Returns true on success. Safe to call repeatedly."
  []
  @installed)
