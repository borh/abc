(ns soranoha.za.html
  "Hiccup-style vectors to an HTML5 string.

  Small on purpose. The browse layer is presentation over already-verified
  bytes, and a template engine would put a dependency, a cache and an
  execution model between the exporter and files it must be able to produce
  byte-identically on every activation. Rendering here is a pure function of
  the vector tree: same input, same bytes.

  Escaping is not optional and has no opt-out. Every string in the tree is
  text, never markup; attribute values are escaped separately from element
  content. Corpus metadata is upstream data, and an entity in a title must
  reach the reader as an entity rather than as markup."
  (:require [clojure.string :as string]))

(def ^:private void-elements
  #{:area :base :br :col :embed :hr :img :input :link :meta :source :track :wbr})

(defn escape-text [^String s]
  (-> s
      (string/replace "&" "&amp;")
      (string/replace "<" "&lt;")
      (string/replace ">" "&gt;")))

(defn escape-attr [^String s]
  (-> (escape-text s)
      (string/replace "\"" "&quot;")))

(defn- attrs->string [attrs]
  (apply str
         (for [k (sort (map name (keys attrs)))
               :let [v (get attrs (keyword k))]
               :when (and (some? v) (not (false? v)))]
           (if (true? v)
             (str " " k)
             (str " " k "=\"" (escape-attr (str v)) "\"")))))

(defn render
  "Render one hiccup node. Vectors are elements, strings are text, nil and
  false are nothing, and sequences splice."
  [node]
  (cond
    (or (nil? node) (false? node)) ""
    (string? node) (escape-text node)
    (vector? node)
    (let [[tag & body] node
          attrs (when (map? (first body)) (first body))
          content (if attrs (rest body) body)
          name* (name tag)]
      (if (contains? void-elements tag)
        (str "<" name* (attrs->string attrs) ">")
        (str "<" name* (attrs->string attrs) ">"
             (apply str (map render content))
             "</" name* ">")))
    (sequential? node) (apply str (map render node))
    :else (escape-text (str node))))

(defn document
  "A complete HTML5 document. `lang` is the primary language of the page's
  own text; per-element `lang` marks the passages that differ."
  [{:keys [lang]} head-nodes body-nodes]
  (str "<!DOCTYPE html>\n"
       (render [:html {:lang (or lang "ja")}
                (into [:head] head-nodes)
                (into [:body] body-nodes)])
       "\n"))
