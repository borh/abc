(ns soranoha.ori.publication-whitespace
  (:require [clojure.string :as string]))

(defn- normalize-newlines [text]
  (string/replace (or text "") #"\r\n?|\n" "\n"))

(defn source-text->tei-inline
  "Render source text-node linebreak runs as TEI structure, not literal newline
  characters in mixed body text."
  [text]
  (interpose [:lb] (remove empty? (string/split (normalize-newlines text) #"\n"))))
