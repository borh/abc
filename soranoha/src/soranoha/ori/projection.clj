(ns soranoha.ori.projection
  (:require [clojure.string :as string]
            [soranoha.annotations.view :as view])
  (:import [org.w3c.dom Element Node]))

(defn- paragraph [^Node node]
  (when node
    (if (= "p" (view/local-name node)) node
        (recur (.getParentNode node)))))

(defn- indent [^Element element]
  (when element
    (when-let [[_ width] (re-find #"(?:^|;)\s*text-indent:\s*(\d+)em(?:;|$)"
                                (.getAttribute element "style"))]
      (apply str (repeat (Long/parseLong width) "　")))))

(defn plaintext
  "Project body text, restoring encoded first-line indentation for plain display."
  [reading]
  (let [out (StringBuilder.)]
    (reduce (fn [seen segment]
              (let [p (paragraph (:view/node segment))]
                (when (and p (not (contains? seen p)))
                  (.append out ^String (or (indent p) "")))
                (.append out ^String (:view/text segment))
                (cond-> seen p (conj p))))
            #{} (:view/segments reading))
    (str out)))

(defn- html-text [text]
  (string/escape text {\& "&amp;" \< "&lt;" \> "&gt;" \" "&quot;"}))

(defn- markdown-text [text]
  (string/replace text #"([\\`*_{}\[\]()#+.!|>~-])" "\\$1"))

(declare inline)

(defn- inline-children [node html?]
  (apply str (map #(inline % html?) (view/selected-children node))))

(defn- inline [^Node node html?]
  (let [tag (view/local-name node)
        content #(inline-children node html?)]
    (cond
      (= Node/TEXT_NODE (.getNodeType node))
      ((if html? html-text markdown-text) (.getNodeValue node))

      (= "ruby" tag)
      (str "<ruby>"
           (apply str (for [child (view/children node)
                            :let [name (view/local-name child)]
                            :when (#{"rb" "rt"} name)]
                        (str "<" name ">" (inline-children child true) "</" name ">")))
           "</ruby>")

      (#{"lb" "pb"} tag) (if html? "<br>" "  \n")
      (= "g" tag) (if (empty? (.getTextContent node)) "�" (content))
      (= "hi" tag) (str "<em>" (inline-children node true) "</em>")
      (#{Node/COMMENT_NODE Node/PROCESSING_INSTRUCTION_NODE} (.getNodeType node)) ""
      :else (content))))

(def ^:private block-tags #{"p" "head" "l" "ab" "item" "figDesc"})

(defn- blocks [node]
  (cond
    (block-tags (view/local-name node)) [node]
    (#{"note" "fw"} (view/local-name node)) []
    :else (into [] (mapcat blocks) (view/selected-children node))))

(defn markdown
  "Markdown with HTML ruby; readers must permit ruby, rb, rt, em and br elements."
  [reading]
  (let [^org.w3c.dom.Document document (:view/document reading)
        body (.item (.getElementsByTagNameNS document view/tei-namespace "body") 0)]
    (string/join "\n\n"
                 (map (fn [node]
                        (str (when (= "head" (view/local-name node)) "## ")
                             (inline-children node false)))
                      (blocks body)))))
