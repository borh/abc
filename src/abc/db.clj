(ns abc.db
  (:require [clojure.java.io :as io]
            [abc.aozora :as a]
            [arachne.aristotle :as aa]
            [arachne.aristotle.registry :as reg]
            [arachne.aristotle.graph :as graph :refer [AsNode]]
            [arachne.aristotle.query :as q]
            [camel-snake-kebab.core :refer [->camelCaseString ->PascalCaseString]])
  (:import [clojure.lang Keyword]
           [java.time.format DateTimeFormatter]
           [org.apache.jena.riot RDFDataMgr RDFFormat]
           [org.apache.jena.datatypes.xsd XSDDatatype]
           [org.apache.jena.graph Graph NodeFactory]))

(defn to-noun [kw]
  (keyword (namespace kw) (->PascalCaseString (name kw))))

(defn to-predicate [kw]
  (keyword (namespace kw)
           (->camelCaseString (name kw))))

;; RDF (Aristotle) type coercion extensions
(extend-protocol AsNode
  Keyword
  (node [kw]
    (NodeFactory/createURI
     (reg/iri
      (cond
        (kw #{::a/work-id ::a/person-id}) (to-noun kw)
        :else (to-predicate kw)))))

  clojure.lang.PersistentVector
  (node [obj]
    (graph/rdf-list obj))

  java.time.LocalDate
  (node [obj]
    (NodeFactory/createLiteral
     (.format obj DateTimeFormatter/ISO_LOCAL_DATE)
     XSDDatatype/XSDdateTime)))

;; Is there a clear connection between RDF and DataScript components wrt. prefix/etc? vvv ?
(reg/prefix :abc.aozora.* "http://abc.com/0.1/")
(reg/prefix :dc "http://purl.org/dc/elements/1.1/")
(reg/prefix :dcndl "http://ndl.go.jp/dcndl/terms/")
(reg/prefix :dcterms "http://purl.org/dc/terms/")

;; How much do we want to directly map onto existing ontologies, and how much just link to their equivalent terms? cf. vvv
;; owl:equivalentProperty
;; owl:sameAs, owl:equivalentProperty and owl:equivalentClass
;; skos:narrower to indicate this relation.
;; It is more precise than skos:closeMatch

(defn to-triples [xs]
  (graph/triples xs))

(defn to-graph [triples]
  (aa/add (aa/graph :jena-mini) triples))

(defn save-graph! [filename ^Graph g & {:keys [format]}] ;; TURTLE_BLOCKS for speed
  (let [format (or format #_RDFFormat/TURTLE_BLOCKS RDFFormat/TURTLE_PRETTY)]
    (with-open [out (io/output-stream filename)]
      (RDFDataMgr/write out g format))))

(defn load-graph [filename]
  (aa/read (aa/graph :simple #_:jena-mini) filename))
