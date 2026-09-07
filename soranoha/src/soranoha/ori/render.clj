(ns soranoha.ori.render
  (:require [soranoha.ori.sentence-policy :as sentence-policy]
            [soranoha.core.json :as record-json]
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

(defn- interpretation-notes [parser-ir]
  (let [complete (get-in parser-ir ["derived_from" "parse_complete"])
        decoding (get-in parser-ir ["source" "decode_outcome"])
        diagnostics (concat (get parser-ir "warnings") (get parser-ir "errors"))]
    (into (into (cond-> []
                  (some? complete) (conj [:note {:type "parser-completion" :n (str complete)}
                                          "Parser completion does not establish exhaustive markup interpretation."])
                  decoding (conj [:note {:type "source-decoding"} decoding]))
                (map (fn [problem]
                       [:note {:type "interpretation-problem"}
                        (record-json/write-deterministic-json-str problem)]))
                (get parser-ir "interpretation_problems"))
          (mapcat (fn [index diagnostic]
                    (let [span (get diagnostic "span")
                          source-id (str "parser-source-" index)]
                      (cond-> []
                        span (conj [:note {:type "source-span" :xml/id source-id}
                                    (record-json/write-deterministic-json-str span)])
                        true (conj [:note (cond-> {:type "parser-diagnostic"
                                                   :subtype (get diagnostic "severity")
                                                   :n (get diagnostic "code")}
                                            span (assoc :source (str "#" source-id)))
                                    (get diagnostic "message")]))))
                  (range) diagnostics))))

(defn- with-interpretation [text parser-ir]
  (let [notes (interpretation-notes parser-ir)]
    (if (empty? notes) text
        (if-let [back-index (first (keep-indexed (fn [index child]
                                                   (when (and (vector? child) (= :back (first child))) index)) text))]
          (update text back-index into notes)
          (conj text (into [:back] notes))))))

(defn render-work
  "Render one work's canonical TEI transcription."
  [{:keys [parser-ir metadata-record persons-by-id]}]
  (let [parser-ir (sentence-policy/ensure-publication-sentence-evidence!
                   parser-ir)
        tei-result (parser-ir-tei/render parser-ir)
        header (tei-header/build
                (assoc (header-input metadata-record persons-by-id)
                       :char-declarations (:char_declarations tei-result)
                       :source-content-hash (get-in parser-ir ["source" "work_content_hash"])
                       :primary-text-hash (get-in parser-ir ["source" "primary_text_hash"])
                       :orthographic-sentence-normalization?
                       (orthographic-sentence-normalization? parser-ir)))]
    {:tei (tei-header/hiccup->pretty-xml-string
           (tei-document header (with-interpretation (:body tei-result) parser-ir)))}))
