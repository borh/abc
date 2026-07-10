(ns abc.tools.diagram.graphviz
  (:require [clojure.string :as str]))

(defn escape-html [value]
  (-> (str value)
      (str/replace "&" "&amp;")
      (str/replace "\"" "&quot;")
      (str/replace "<" "&lt;")
      (str/replace ">" "&gt;")))

(defn escape-dot [value]
  (-> (str value)
      (str/replace "\\" "\\\\")
      (str/replace "\"" "\\\"")
      (str/replace "\n" "\\n")))

(defn- wrap-lines [value width]
  (let [tokens (->> (str/split (str value) #"\s+")
                    (remove str/blank?)
                    (mapcat (fn [token]
                              (map #(apply str %)
                                   (partition-all width token)))))]
    (reduce (fn [lines token]
              (let [line (peek lines)]
                (if (and line (<= (+ (count line) 1 (count token)) width))
                  (conj (pop lines) (str line " " token))
                  (conj lines token))))
            [] tokens)))

(defn- html-lines [value width]
  (str/join "<BR/>" (map escape-html (wrap-lines value width))))

(defn- id-string [value]
  (-> (name value)
      (str/replace #"[^A-Za-z0-9_]" "_")))

(defn- attr-value [value]
  (if (and (map? value) (contains? value :html))
    (:html value)
    (str "\"" (escape-dot value) "\"")))

(defn- attrs [m]
  (str "["
       (str/join ","
                 (for [[k v] (sort-by (comp name key) m)]
                   (str (name k) "=" (attr-value v))))
       "]"))

(defn- role-style [theme role]
  (cond
    (#{:identity :identity-formula :coordinate-family} role)
    {:color (:identity theme) :fontcolor (:text theme)}

    (#{:evidence :validation} role)
    {:color (:evidence theme) :fontcolor (:text theme)}

    (= :output role)
    {:color (:output theme) :fontcolor (:text theme)}

    :else
    {:color (:secondary theme) :fontcolor (:text theme)}))

(defn- visible-coordinates [{:keys [label coordinates]}]
  (if (and (= 1 (count coordinates))
           (= label (:label (first coordinates))))
    []
    coordinates))

(defn- html-label
  [{:keys [label subtitle label-wrap subtitle-wrap coordinate-columns]
    :as node}
   theme]
  (str "<"
       "<TABLE BORDER=\"0\" CELLBORDER=\"0\" CELLPADDING=\"1\">"
       "<TR><TD><FONT POINT-SIZE=\"" (:primary-size theme) "\"><B>"
       (html-lines label (or label-wrap 28)) "</B></FONT></TD></TR>"
       (when subtitle
         (str "<TR><TD><FONT COLOR=\"" (:secondary theme)
              "\" POINT-SIZE=\"" (:secondary-size theme) "\">"
              (html-lines subtitle (or subtitle-wrap 28)) "</FONT></TD></TR>"))
       (apply str
              (for [row (partition-all (or coordinate-columns 3)
                                       (visible-coordinates node))]
                (str "<TR>"
                     (apply str
                            (for [{:keys [label]} row]
                              (str "<TD ALIGN=\"LEFT\"><FONT COLOR=\""
                                   (:secondary theme) "\" POINT-SIZE=\""
                                   (:secondary-size theme) "\">· "
                                   (escape-html label) "</FONT></TD>")))
                     "</TR>")))
       "</TABLE>>"))

(defn- node-line [node theme primary-ids]
  (let [style (role-style theme (:role node))]
    (str "    \"" (id-string (:id node)) "\" "
         (attrs (merge {:label {:html (html-label node theme)}
                        :shape (if (= :validation (:role node)) "diamond" "rect")
                        :style "rounded"
                        :penwidth (:stroke-width theme)}
                       (when (primary-ids (:id node))
                         {:group "primary"})
                       style))
         ";")))

(defn- edge-line [edge theme]
  (let [style (role-style theme (:role edge))]
    (str "  \"" (id-string (:from edge)) "\" -> \""
         (id-string (:to edge)) "\" "
         (attrs (merge
                 {:color (:color style)
                  :fontcolor (:secondary theme)
                  :fontname "Noto Sans CJK JP"
                  :fontsize (:secondary-size theme)
                  :penwidth (if (= :thick (:style edge))
                              4
                              (:stroke-width theme))
                  :style (if (= :dashed (:style edge)) "dashed" "solid")}
                 (when-let [head-port (:head-port edge)]
                   {:headport (name head-port)})
                 (when-let [tail-port (:tail-port edge)]
                   {:tailport (name tail-port)})
                 (when-let [label (:label edge)]
                   {:xlabel (str/join "\n" (wrap-lines label 14))})))
         ";")))

(defn- id-collisions [nodes]
  (->> nodes
       (group-by (comp id-string :id))
       (keep (fn [[rendered matches]]
               (when (< 1 (count matches))
                 [rendered (mapv :id matches)])))
       (into {})))

(defn dot [{:keys [id direction theme groups nodes edges primary-order
                   concentrate?]}]
  (when-let [collisions (not-empty (id-collisions nodes))]
    (throw (ex-info "duplicate presentation node ids"
                    {:collisions collisions})))
  (let [grouped (group-by :group nodes)
        primary-ids (set primary-order)
        clustered-groups (remove #(false? (:cluster? %)) groups)
        flat-group-ids (set (map :id (filter #(false? (:cluster? %)) groups)))
        group-lines
        (mapcat
         (fn [{:keys [id label style label-location]}]
           (concat
            [(str "  subgraph \"cluster_" (id-string id) "\" {")
             (str "    label=\"" (escape-dot label) "\";")
             (str "    color=\"" (:secondary theme) "\";")
             (str "    penwidth=\"" (:stroke-width theme) "\";")
             "    fontname=\"Noto Sans CJK JP\";"
             (str "    fontcolor=\"" (:secondary theme) "\";")
             (str "    fontsize=\"" (:secondary-size theme) "\";")
             "    margin=\"0\";"
             (str "    style=\"rounded"
                  (when (= :dashed style) ",dashed") "\";")]
            (when label-location
              [(str "    labelloc=\""
                    ({:top "t" :bottom "b"} label-location)
                    "\";")])
            (map #(node-line % theme primary-ids)
                 (sort-by (comp id-string :id) (get grouped id)))
            ["  }"]))
         (sort-by (comp id-string :id) clustered-groups))
        ungrouped (sort-by (comp id-string :id)
                           (concat (get grouped nil)
                                   (mapcat grouped flat-group-ids)))
        order-lines
        (for [[from to] (partition 2 1 primary-order)]
          (str "  \"" (id-string from) "\" -> \"" (id-string to)
               "\" [style=\"invis\",weight=\"100\"];"))]
    (str
     "// GENERATED by clojure -M:abc/presentation-diagrams — do not edit\n"
     "digraph \"" (id-string id) "\" {\n"
     "  graph " (attrs (cond-> {:bgcolor "transparent"
                                 :fontname "Noto Sans CJK JP"
                                 :fontcolor (:text theme)
                                 :nodesep "0.02"
                                 :ranksep "0.10"
                                 :pad "0.05"
                                 :margin "0"
                                 :rankdir direction
                                 :splines "polyline"}
                          concentrate? (assoc :concentrate "true")))
     ";\n"
     "  node " (attrs {:fontname "Noto Sans CJK JP"
                       :margin "0.04,0.02"})
     ";\n"
     "  edge " (attrs {:arrowsize "0.75"}) ";\n"
     (str/join "\n" group-lines) "\n"
     (when (seq ungrouped)
       (str (str/join "\n" (map #(node-line % theme primary-ids) ungrouped)) "\n"))
     (when (seq order-lines)
       (str (str/join "\n" order-lines) "\n"))
     (str/join "\n"
               (map #(edge-line % theme)
                    (sort-by (juxt (comp id-string :from)
                                   (comp id-string :to)
                                   :label)
                             edges)))
     "\n}\n")))
