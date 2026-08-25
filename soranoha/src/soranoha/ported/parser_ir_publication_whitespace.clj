(ns soranoha.ported.parser-ir-publication-whitespace
  (:require [clojure.string :as string]))

(defn- normalize-newlines [text]
  (string/replace (or text "") #"\r\n?|\n" "\n"))

(defn source-text->plaintext
  "Normalize line separators that arrived as literal source text.
  Parser-IR structural line/page break nodes are handled by their own renderers;
  this policy is only for newlines carried inside text nodes."
  [text at-output-start?]
  (let [normalized (-> text
                       normalize-newlines
                       (string/replace #"\n+" "\n"))
        normalized (if at-output-start?
                     (string/replace normalized #"^\n+" "")
                     normalized)]
    normalized))

(defn source-text-ends-with-newline? [text]
  (boolean (re-find #"\n$" (normalize-newlines text))))

(defn trim-trailing-newlines [text]
  (string/replace (or text "") #"\n+$" ""))

(defn source-text->tei-inline
  "Render source text-node linebreak runs as TEI structure, not literal newline
  characters in mixed body text."
  [text]
  (let [normalized (-> text
                       normalize-newlines
                       (string/replace #"\n+" "\n")
                       (string/replace #"^\n+" "")
                       (string/replace #"\n+$" ""))]
    (->> (re-seq #"\n|[^\n]+" normalized)
         (keep (fn [token]
                 (if (= "\n" token)
                   [:lb]
                   (when (seq token)
                     token)))))))
