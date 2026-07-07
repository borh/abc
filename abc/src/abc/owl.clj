(ns abc.owl
  (:require [tawny
             [owl :refer :all]
             [reasoner :as r]]
            [clojure.java.io :as io])
  (:import [org.semanticweb.owlapi.model OWLOntologyID IRI]))

(comment
  (defn load-ontology-from-file [filename]
    (remove-ontology-maybe
     (OWLOntologyID. (IRI/create "http://nlp.lang.osaka-u.ac.jp/blue-skies")))
    (.loadOntologyFromOntologyDocument
     (owl-ontology-manager)
     (IRI/create (io/resource filename))))

  (defn get-version []
    ;; FIXME hook into git. Maybe use tag/commit id for this file.
    "Unreleased Version")

  (defontology blue-skies-ontology
    :iri "http://nlp.lang.osaka-u.ac.jp/blue-skies"
    :prefix "bs:"
    :comment "An experimental ontology for the Aozora Bunko"
    :versioninfo (get-version))

  (defclass NDC
    :label "NDC")

  ;; http://www.metabridge.jp/infolib/metabridge/show/description/view/?lang=&descriptionURI=http%3A%2F%2Fpurl.org%2Fnet%2Faozora%2Fdsp
  ;; Version: use version from git/build.boot, or, rather, the latest git hash/commit for all files that impact the ontology.

  (defclass Person)
  ;; These are not disjoint classes; in principle, an author can also be a translator, and in the future, a revisor can also be an author.
  (defclass Author :super Person)
  (defclass Translator :super Person)
  (defclass Revisor :super Person)                          ;; FIXME naming: editor?
  (defclass TitleID)
  (defclass Work                                            ;; cf. http://dbpedia.org/ontology/WrittenWork
    #_(owl-or))
  (defclass Book :super Work)
  (defclass Letter :super Work)
  (defclass Drama :super Work)
  (defclass Play :super Work)
  (defclass Poem :super Work)
  (defoproperty hasTitle :domain Work)
  (defoproperty hasSubtitle :domain Work)
  (defclass Genre)
  (defclass Horror :super Genre))

(comment
  (r/reasoner-factory :hermit)
  (println
   (r/coherent?)
   (set (r/unsatisfiable))
   (r/consistent?))
  (save-ontology "aozorabunko.owl" :owl)
  (save-ontology "aozorabunko.ttl" :ttl))
