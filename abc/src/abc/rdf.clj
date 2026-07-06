(ns abc.rdf
  #_(:require #_[arachne.aristotle.registry :as reg]
            #_[arachne.aristotle :as aa]))

;; In-memory graph for dataset creation.
(comment
  (reg/prefix 'abc.* "http://nlp.lang.osaka-u.ac.jp/abc/vocab/1.0/")

  (def graph (aa/graph :jena-mini))

  (defn to-rdf-db! [g]
    (aa/add graph g)))
