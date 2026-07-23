(ns abc.annotation-schema-test
  (:require [abc.annotation.schema :as annotation-schema]
            [abc.test-utils]
            [clojure.test :refer [deftest is]]
            [malli.core :as m]
            [malli.registry :as mr]))

(def ^:private registry
  (mr/composite-registry (m/default-schemas) annotation-schema/registry))

(deftest registry-validates-document-body-shape
  (let [doc {:document/paragraphs
             [{:paragraph/tags #{}
               :paragraph/sentences
               [{:sentence/tags #{}
                 :sentence/annotated-text ["本文"]
                 :sentence/text "本文"}]}]
             :document/metadata
             #:abc.stats{:characters 2
                         :tokens 1
                         :types 1
                         :paragraphs 1
                         :sentences 1
                         :sentence-lengths-median 1.0
                         :hapax-legomenon 1
                         :yules-k 0.0
                         :sttr-500 nil}}]
    (is (schema-valid (m/schema :document/body {:registry registry}) doc))))
