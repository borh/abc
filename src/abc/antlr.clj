(ns abc.antlr
  (:require [clj-antlr.core :as antlr]
            [taoensso.timbre :as timbre]
            [clojure.string :as string]
            [abc.annotation :as annotation]
            [abc.text :as text]
            [malli.core :as m]
            [malli.error :as me]))

;; Look for inspiration:
;; https://github.com/juxt/grab/blob/master/src/juxt/grab/alpha/parser.clj

(def aozora-grammar (antlr/parser "resources/AozoraLexer.g4" "resources/AozoraParser.g4" {}))
(def sentencesplitter-grammar (antlr/parser "resources/SentenceSplitter.g4"))

(defn parse-ruby
  "Returns a text annotation and optionally the text fragment corresponding to the ruby."
  [xs]
  (case (count xs)
    5 #:text{:fragment (nth xs 1)
             :tags     #{{:annotation/type :ruby
                          :ruby/reading    (nth xs 3)}}}
    ;; We need to backtrack here to find a contiguous character type string to find the
    ;; corresponding sentence fragment.
    3 #:text{:tags #{{:annotation/type :ruby
                      :ruby/reading    (nth xs 1)}}}))

(defn parse-gaiji
  "Returns a map containing the string representing the Unicode character designated by the annotation."
  [xs]
  ;; TODO / FIXME : are there any annotations with >=2 gaiji inside?
  (let [gaiji
        (annotation/jis-unicode-map
         (some (fn [s]
                 (->> s (re-seq #"(\d-\d+-\d+|\d+-\d+)") first second))
               (filter string? (flatten (drop 2 xs)))))
        gaiji (if-not gaiji (annotation/gaiji-map (string/join (drop 2 xs))))]
    (when-not gaiji
      (timbre/error "Failed parsing gaiji:" xs))
    #:text{:fragment gaiji}))

(declare parse-quote)

(defn parse-annotation
  "Returns a vector of maps containing :text/fragment and/or various annotations."
  [b]
  (reduce
   (fn [a x]
     (if (seq? x)
       (let [[block-type & xs] x
             parsed-block
             (case block-type
               :quote (parse-quote xs)
               :gaiji (parse-gaiji xs)
               :ruby (parse-ruby xs))]
         (conj a parsed-block))
       (conj a {:text/fragment x})))
   []
   b))

(defn parse-quote
  ;; FIXME nesting will break this!
  "Returns a map containing tags and vector of text fragments."
  [xs]
  (let [lines (reduce (fn [a x]
                        (if (string? x)
                          (conj (pop a) (str (peek a) x))
                          (into a (parse-annotation x))))
                      [""]
                      xs)]
    #:text{:tags     #{#:annotation{:type    :quotation
                                    :content #:quotation{:type   :spoken
                                                         :direct true}}}
           :fragment (string/join "\n" lines)
           #_:sentences #_(annotation/lines->paragraph-sentences lines)}))

(defn paragraph->sentences [paragraph]
  (into []
        (comp (filter identity)
              (remove empty?))
        (text/split-japanese-sentence paragraph)))

(defn parse-block
  "Parse block into a paragraph and associated annotations."
  [b char-start]
  (reduce
   (fn [a fragments]
     (let [text (:text/fragment fragments) #_(string/join (map :text/fragment fragments))
           paragraph-chars (count text)
           sentences (paragraph->sentences text)
           char-end (+ char-start paragraph-chars)]
       (-> a
           (update ::annotation/fragments conj
                   #:paragraph{:sentences sentences
                               :char-start char-start
                               :char-end char-end
                               :annotated-text fragments})
           (update :paragraph/char-end + paragraph-chars)
           (assoc :document/char-end char-end))))
   {:document/char-start char-start
    :paragraph/char-start 0
    :paragraph/char-end 0
    ::annotation/fragments []}
   (reduce
    (fn [a x]
      (if (and (seq? x) (keyword? (first x)))
        (let [[block-type & xs] x
              parsed-block
              (case block-type
                :block (parse-block xs char-start)        ;; TODO check if char-start is correct
                :structure {:fragment/annotation {:annotation/type    :structure
                                                  :annotation/content (string/join xs)}}
                :annotation (parse-annotation xs))]
          (if (vector? parsed-block)
            (into a parsed-block)
            (conj a parsed-block)))
        (conj a {:text/fragment x})))
    []
    b)))

(defn parse [s]
  (reduce
   (fn [a block]
     (timbre/debug block)
     (let [[block-name & block-content] block]
       (if (= :block block-name)
         (let [parsed-block (parse-block block-content (:document/char-start a))]
           #_(timbre/debug parsed-block (:document/char-end parsed-block))
           (-> a
               (update :document/paragraphs conj parsed-block)
               (assoc :document/char-start (:document/char-end parsed-block))))
         (do
           (timbre/error "Discarding" block)
           a))))
   #:document{:paragraphs []
              :char-start 0}
   (eduction (remove keyword?) (remove string?)
             (antlr/parse aozora-grammar {:throw? true} s))))

(doseq [b (:document/paragraphs (time (parse (slurp "warazouri-utf-8.txt"))))]
  (when-let [e (->> b (m/explain [:schema {:registry annotation/registry} ::annotation/fragments]) me/humanize #_:errors)]
    (timbre/error e))
  (clojure.pprint/pprint b)
  (println))
