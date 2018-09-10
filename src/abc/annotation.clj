(ns abc.annotation
  (:require [clojure.spec.alpha :as s]
            [corpus-utils.text :as text]
            [clojure.string :as str]
            [abc.aozora :as aozora]
            [clojure.java.io :as io]
            [instaparse.core :as insta]))

(defn make-aozora-bunko-bnf []
  (insta/parser (io/resource "aozora-parser.bnf")))

(def parser (make-aozora-bunko-bnf))

(defn parse-bnf [s]
  (insta/transform
   {:String str}
   (parser s)))

(s/def :quotation/type #{"spoken" "thought"})
(s/def :quotation/direct boolean?)
(s/def :quotation/aloud boolean?)
(s/def :quotation/outside-referer boolean?)
(s/def ::quotation (s/keys :opt [:quotation/type :quotation/direct :quotation/aloud :quotation/outside-referer]))

(s/def :aozora/tag map?)

(s/def :document/sentence (s/coll-of (s/or :text string? :tagged-text :aozora/tag)))
(s/def :sentence/tags (s/coll-of map?)) #_#{:quotation/spoken :quotation/thought :quotation/direct}
(s/def :document/sentences (s/coll-of (s/keys :req [:sentence/tags :document/sentence])))
(s/def :paragraph/tags (s/coll-of map?))
(s/def :document/paragraph (s/keys :req [:paragraph/tags :document/sentences]))
(s/def :document/paragraphs (s/coll-of :document/paragraph))
(s/def :document.division/type (s/cat :type string? :n string?))
(s/def :document/division (s/keys :req [:document/paragraphs] :opt [:document.division/type]))
(s/def :document/text (s/coll-of :document/division))

(defn oov? [m] ;; FIXME
  (= "未知" (:mecab.features/goshu m)))

(s/fdef has-quotation?
  :args (s/cat :s :document/sentence)
  :ret :document/sentence)
(defn has-quotation? [s]
  (or false))

(s/fdef katakana-sentence?
  :args (s/cat :s :document/sentence)
  :ret boolean?)
(defn katakana-sentence? [s]
  (or true))

(s/def ::aozora/tag
  (s/cat :type (s/or :a "...") :tag-type (s/+ string?)))

(defn aozora-annotation->tags [s]
  (if-let [annotations (rseq (re-seq #"(.*)\[#([^〕]+)\](.*)" s))]
    (reduce
     (fn [v [_ l tag r]]
       (conj v r
             (s/conform ::aozora/tag tag)))
     []
     annotations)
    s))

(defn parse-with-tags [v])

(defn parse-paragraph [s]
  (->> s
       text/normalize-nfkc
       text/convert-half-to-fullwidth
       str/split-lines
       text/lines->paragraph-sentences)) ;; TODO Might need to define split-fn
