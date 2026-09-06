(ns soranoha.ori.render
  (:require [soranoha.ori.plaintext :as plaintext]
            [soranoha.ori.sentence-policy :as sentence-policy]
            [soranoha.ori.tei :as parser-ir-tei]
            [soranoha.ori.tei-header :as tei-header]))

(defn- header-input [metadata-record persons-by-id]
  {:work (get metadata-record "work")
   :contributors (mapv (fn [contributor]
                         (let [person-id (get contributor "person_id")
                               person (get persons-by-id person-id)]
                           (when-not person
                             (throw (ex-info "contributor person record missing"
                                             {:person_id person-id})))
                           {:relation-to-work (get contributor "relation_to_work")
                            :person person}))
                       (get metadata-record "contributors"))})

(defn- tei-document [header body]
  [:TEI {:xmlns/abc "https://w3id.org/abc/ns/tei"
         :abc/vocab-version "0"}
   header
   body])

(defn- orthographic-sentence-normalization? [parser-ir]
  (boolean
   (some (fn [sentence]
           (some #{"orthographic-katakana"}
                 (get sentence "tags" [])))
         (get parser-ir "sentences" []))))

(defn render-work
  "Render one work. Returns {:plaintext <string> :tei <string>}."
  [{:keys [parser-ir metadata-record persons-by-id]}]
  (let [parser-ir (sentence-policy/ensure-publication-sentence-evidence!
                   parser-ir)
        plaintext-result (plaintext/render parser-ir)
        tei-result (parser-ir-tei/render parser-ir)
        header (tei-header/build
                (assoc (header-input metadata-record persons-by-id)
                       :char-declarations (:char_declarations tei-result)
                       :orthographic-sentence-normalization?
                       (orthographic-sentence-normalization? parser-ir)))]
    {:plaintext (:text plaintext-result)
     :tei (tei-header/hiccup->pretty-xml-string
           (tei-document header (:body tei-result)))}))
