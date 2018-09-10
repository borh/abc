(ns abc.rdf
  (:require  [arachne.aristotle.registry :as reg]
             [arachne.aristotle :as aa]))

;; In-memory graph for dataset creation.

(reg/prefix 'abc.* "http://nlp.lang.osaka-u.ac.jp/abc/vocab/1.0/")

(def graph (aa/graph :jena-mini))

(defn to-rdf-db! [g]
  (aa/add graph g))
