(ns abc.tools.parser-ir-tei
  (:require [clojure.string :as string]))

(defn- present-text? [text]
  (seq text))

(defn- mark-omitted [acc node-type]
  (update acc :omitted conj {:type node-type :policy "omitted"}))

(defn- append-inline [acc node]
  (update acc :current-paragraph conj node))

(defn- append-structural-child [acc node]
  (if (seq (:current-division acc))
    (update acc :current-division conj node)
    (update acc :body-children conj node)))

(defn- flush-paragraph [acc]
  (if (seq (:current-paragraph acc))
    (assoc (append-structural-child acc (into [:p] (:current-paragraph acc)))
           :current-paragraph [])
    acc))

(defn- flush-division [acc]
  (if (seq (:current-division acc))
    (-> acc
        (update :body-children conj (into [:div] (:current-division acc)))
        (assoc :current-division []))
    acc))

(defn- append-block [acc node]
  (append-structural-child (flush-paragraph acc) node))

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

      (contains? gaiji "raw_marker")
      (assoc :raw-marker (get gaiji "raw_marker")))))

(defn- render-text-node [acc node]
  (append-inline acc (get node "text")))

(defn- render-ruby-node [acc node]
  (let [ruby (get node "ruby")]
    (append-inline acc
                   (cond-> [:ruby]
                     (get ruby "direction")
                     (conj {:place (get ruby "direction")})
                     true
                     (conj [:rb (get ruby "base")]
                           [:rt (get ruby "reading")])))))

(defn- render-gaiji-node [acc node]
  (let [declaration (gaiji-declaration node)]
    (-> acc
        (register-char-declaration declaration)
        (append-inline [:g {:ref (str "#" (:xml-id declaration))}]))))

(defn- render-editor-note-node [acc node]
  (let [note (get node "note")]
    (append-inline acc
                   [:note {:type (get note "category")}
                    (get note "raw")])))

(defn- render-emphasis-node [acc node]
  (append-inline acc
                 [:hi {:rend (get node "style")}
                  (get node "text")]))

(defn- render-heading-node [acc node]
  (-> acc
      flush-paragraph
      flush-division
      (update :current-division conj
              [:head {:n (str (get node "level"))}
               (get node "text")])))

(defn- render-indentation-node [acc node]
  (if (present-text? (get node "text"))
    (append-inline acc
                   [:seg {:type "indentation"
                          :n (str (get node "depth"))}
                    (get node "text")])
    (mark-omitted acc "indentation")))

(defn- render-page-break-node [acc node]
  (append-block acc
                (cond-> [:pb]
                  (some? (get node "page_number"))
                  (conj {:n (get node "page_number")}))))

(defn- render-image-node [acc node]
  (append-block acc
                (cond-> [:figure
                         [:graphic {:url (get node "src")}]]
                  (present-text? (get node "alt"))
                  (conj [:figDesc (get node "alt")]))))

(defn- render-caption-node [acc node]
  (append-block acc [:figDesc (get node "text")]))

(defn- render-quote-node [acc node]
  (if (present-text? (get node "text"))
    (append-inline acc [:quote (get node "text")])
    (mark-omitted acc "quote")))

(def ^:private node-renderers
  {"text" render-text-node
   "ruby" render-ruby-node
   "gaiji" render-gaiji-node
   "editor-note" render-editor-note-node
   "emphasis" render-emphasis-node
   "heading" render-heading-node
   "indentation" render-indentation-node
   "page-break" render-page-break-node
   "image" render-image-node
   "caption" render-caption-node
   "quote" render-quote-node})

(defn- render-node [acc node]
  (let [node-type (get node "type")]
    (if-let [render-node-fn (get node-renderers node-type)]
      (render-node-fn acc node)
      (throw (ex-info "Unsupported TEI parser-IR node type"
                      {:node-type node-type})))))

(defn render [parser-ir]
  (let [result (reduce (fn [acc node]
                         (-> acc
                             (count-node (get node "type"))
                             (render-node node)))
                       {:body-children []
                        :current-division []
                        :current-paragraph []
                        :char_declarations []
                        :char-declaration-ids #{}
                        :node_counts {}
                        :omitted []}
                       (get parser-ir "nodes"))
        result (-> result
                   flush-paragraph
                   flush-division)]
    {:body [:text (into [:body] (:body-children result))]
     :char_declarations (:char_declarations result)
     :node_counts (:node_counts result)
     :omitted (:omitted result)}))

(def covered-node-types
  (set (keys node-renderers)))
