(ns abc.tools.tei-header-unit-test
  (:require [abc.tools.files :as files]
            [abc.tools.tei-header :as th]
            [clojure.string :as string]
            [clojure.test :refer [deftest is testing]]))

(def example-record
  (delay (files/read-json "examples/v0/example-work/metadata-record.json")))

(def example-person
  (delay (files/read-json "examples/v0/example-persons/000879.json")))

(defn resolved-input []
  (let [persons-by-id {"000879" @example-person}]
    {:work (get @example-record "work")
     :contributors (mapv (fn [c]
                           {:relation-to-work (get c "relation_to_work")
                            :person (get persons-by-id (get c "person_id"))})
                         (get @example-record "contributors"))}))

(deftest char-declarations-are-input-driven-test
  (testing "header emits parser-IR supplied gaiji declarations"
    (let [input (assoc (resolved-input)
                       :char-declarations
                       [{:xml-id "gaiji-1"
                         :name "Unresolved gaiji"
                         :desc "Preserved unresolved source marker."
                         :unicode nil
                         :raw-marker "※［＃1-2-3］"}])
          s (th/emit-xml (th/build input))]
      (is (string/includes? s "xml:id=\"gaiji-1\""))
      (is (string/includes? s "Unresolved gaiji"))
      (is (not (string/includes? s "example-gaiji"))))))
