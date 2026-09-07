(ns ^:typed.clojure soranoha.ori.publication-whitespace
  (:require [clojure.string :as string]))

(defn- normalize-newlines [text]
  (string/replace (or text "") #"\r\n?|\n" "\n"))

(defn source-text->tei-inline
  "Render source text-node linebreak runs as TEI structure, not literal newline
  characters in mixed body text."
  [text]
  (let [normalized (-> text
                       normalize-newlines
                       (string/replace #"\n+" "\n")
                       (string/replace #"^\n+" "")
                       (string/replace #"\n+$" ""))]
    (interpose [:lb] (remove empty? (string/split normalized #"\n")))))
