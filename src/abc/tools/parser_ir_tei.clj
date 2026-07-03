(ns abc.tools.parser-ir-tei
  (:require [clojure.string :as string]))

(defn- present-text? [text]
  (seq text))

(defn- mark-omitted [acc node-type]
  (update acc :omitted conj {:type node-type :policy "omitted"}))

(defn- append-inline [acc node]
  (update acc :current-paragraph conj node))

(defn- flush-paragraph [acc]
  (if (seq (:current-paragraph acc))
    (-> acc
        (update :body-children conj (into [:p] (:current-paragraph acc)))
        (assoc :current-paragraph []))
    acc))

(defn- append-block [acc node]
  (update (flush-paragraph acc) :body-children conj node))

(defn- count-node [acc node-type]
  (update acc :node_counts update node-type (fnil inc 0)))

(defn- normalize-gaiji-id [reference span]
  (let [reference (some-> reference
                          (string/replace #"^#" ""))]
    (or reference
        (str "gaiji-" (get span "start") "-" (get span "end")))))

(defn- register-char-declaration [acc declaration]
  (if (contains? (:char-declaration-ids acc) (:xml-id declaration))
    acc
    (-> acc
        (update :char_declarations conj declaration)
        (update :char-declaration-ids conj (:xml-id declaration)))))

(defn- gaiji-declaration [node]
  (let [gaiji (get node "gaiji")
        span (get node "span")]
    (cond-> {:xml-id (normalize-gaiji-id (get gaiji "reference") span)}
      (contains? gaiji "unicode")
      (assoc :unicode (get gaiji "unicode"))

      (and (not (get gaiji "reference"))
           (contains? gaiji "raw_marker"))
      (assoc :raw-marker (get gaiji "raw_marker")))))

(defmulti ^:private render-node (fn [_acc node] (get node "type")))

(defmethod render-node "text" [acc node]
  (append-inline acc (get node "text")))

(defmethod render-node "ruby" [acc node]
  (let [ruby (get node "ruby")]
    (append-inline acc
                   (cond-> [:ruby]
                     (get ruby "direction")
                     (conj {:place (get ruby "direction")})
                     true
                     (conj [:rb (get ruby "base")]
                           [:rt (get ruby "reading")])))))

(defmethod render-node "gaiji" [acc node]
  (let [declaration (gaiji-declaration node)]
    (-> acc
        (register-char-declaration declaration)
        (append-inline [:g {:ref (str "#" (:xml-id declaration))}]))))

(defmethod render-node "editor-note" [acc node]
  (let [note (get node "note")]
    (append-inline acc
                   [:note {:type (get note "category")}
                    (get note "raw")])))

(defmethod render-node "emphasis" [acc node]
  (append-inline acc
                 [:hi {:rend (get node "style")}
                  (get node "text")]))

(defmethod render-node "heading" [acc node]
  (append-block acc
                [:head {:n (str (get node "level"))}
                 (get node "text")]))

(defmethod render-node "indentation" [acc node]
  (if (present-text? (get node "text"))
    (append-inline acc
                   [:seg {:type "indentation"
                          :n (str (get node "depth"))}
                    (get node "text")])
    (mark-omitted acc "indentation")))

(defmethod render-node "page-break" [acc node]
  (append-block acc
                (cond-> [:pb]
                  (some? (get node "page_number"))
                  (conj {:n (get node "page_number")}))))

(defmethod render-node "image" [acc node]
  (append-block acc
                (cond-> [:figure
                         [:graphic {:url (get node "src")}]]
                  (present-text? (get node "alt"))
                  (conj [:figDesc (get node "alt")]))))

(defmethod render-node "caption" [acc node]
  (append-block acc [:figDesc (get node "text")]))

(defmethod render-node "quote" [acc node]
  (if (present-text? (get node "text"))
    (append-inline acc [:quote (get node "text")])
    (mark-omitted acc "quote")))

(defmethod render-node :default [acc _node]
  acc)

(defn render [parser-ir]
  (let [result (reduce (fn [acc node]
                         (-> acc
                             (count-node (get node "type"))
                             (render-node node)))
                       {:body-children []
                        :current-paragraph []
                        :char_declarations []
                        :char-declaration-ids #{}
                        :node_counts {}
                        :omitted []}
                       (get parser-ir "nodes"))
        result (flush-paragraph result)]
    {:body [:text (into [:body] (:body-children result))]
     :char_declarations (:char_declarations result)
     :node_counts (:node_counts result)
     :omitted (:omitted result)}))

(def covered-node-types
  #{"caption"
    "editor-note"
    "emphasis"
    "gaiji"
    "heading"
    "image"
    "indentation"
    "page-break"
    "quote"
    "ruby"
    "text"})
