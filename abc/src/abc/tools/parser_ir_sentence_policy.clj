(ns abc.tools.parser-ir-sentence-policy)

(defn- sentence-row-error-prefix [sentence]
  (str "parser IR sentence " (get sentence "id")))

(defn- sentence-tiling-errors [paragraph sentences]
  (let [pid (get paragraph "id")
        paragraph-node-range (get paragraph "node_range")
        paragraph-span (get paragraph "span")
        paragraph-empty? (or (= (get paragraph-node-range "start")
                                (get paragraph-node-range "end"))
                             (= (get paragraph-span "start")
                                (get paragraph-span "end")))
        ordered (sort-by (juxt #(get-in % ["node_range" "start"])
                               #(get-in % ["span" "start"]))
                         sentences)]
    (cond
      (and paragraph-empty? (empty? ordered))
      []

      (empty? ordered)
      [(str "parser IR body paragraph " pid " has no sentence rows")]

      :else
      (let [node-errors
            (loop [remaining ordered
                   expected-start (get paragraph-node-range "start")
                   errors []]
              (if-let [sentence (first remaining)]
                (let [node-range (get sentence "node_range")
                      start (get node-range "start")
                      end (get node-range "end")
                      errors (cond-> errors
                               (not= start expected-start)
                               (conj (str (sentence-row-error-prefix sentence)
                                          " node_range starts at " start
                                          " but expected " expected-start)))]
                  (recur (rest remaining) end errors))
                (cond-> errors
                  (not= expected-start (get paragraph-node-range "end"))
                  (conj (str "parser IR body paragraph " pid
                             " sentence node_ranges end at " expected-start
                             " but paragraph node_range ends at "
                             (get paragraph-node-range "end"))))))
            span-errors
            (loop [remaining ordered
                   expected-start (get paragraph-span "start")
                   errors []]
              (if-let [sentence (first remaining)]
                (let [span (get sentence "span")
                      start (get span "start")
                      end (get span "end")
                      errors (cond-> errors
                               (not= start expected-start)
                               (conj (str (sentence-row-error-prefix sentence)
                                          " span starts at " start
                                          " but expected " expected-start)))]
                  (recur (rest remaining) end errors))
                (cond-> errors
                  (not= expected-start (get paragraph-span "end"))
                  (conj (str "parser IR body paragraph " pid
                             " sentence spans end at " expected-start
                             " but paragraph span ends at "
                             (get paragraph-span "end"))))))]
        (vec (concat node-errors span-errors))))))

(defn sentence-coherence-errors [parser-ir]
  (let [nodes (vec (get parser-ir "nodes" []))
        paragraphs (vec (get parser-ir "paragraphs" []))
        body-paragraphs (into {}
                              (keep (fn [paragraph]
                                      (when (= "body" (get paragraph "role"))
                                        [(get paragraph "id") paragraph])))
                              paragraphs)
        annotations (get-in parser-ir ["orthographic_annotations" "annotations"] [])
        sentences (vec (get parser-ir "sentences" []))
        sentences-by-pid (group-by #(get % "paragraph_id") sentences)]
    (vec
     (concat
      (mapcat
       (fn [[_idx sentence]]
         (let [sid (get sentence "id")
               pid (get sentence "paragraph_id")
               paragraph (get body-paragraphs pid)
               nr (get sentence "node_range")
               pr (get paragraph "node_range")
               span (get sentence "span")
               ps (get paragraph "span")
               tags (set (get sentence "tags" []))
               annotation-indices (get sentence "orthographic_annotation_indices" [])]
           (cond-> []
             (nil? paragraph)
             (conj (str "parser IR sentence " sid " references non-body paragraph " pid))

             (and paragraph
                  (not (<= (get pr "start") (get nr "start") (get nr "end") (get pr "end"))))
             (conj (str "parser IR sentence " sid " node_range "
                        (get nr "start") ".." (get nr "end")
                        " is outside paragraph " pid " node_range "
                        (get pr "start") ".." (get pr "end")))

             (and paragraph
                  (not (<= (get ps "start") (get span "start") (get span "end") (get ps "end"))))
             (conj (str "parser IR sentence " sid " span "
                        (get span "start") ".." (get span "end")
                        " is outside paragraph " pid " span "
                        (get ps "start") ".." (get ps "end")))

             (not (<= 0 (get nr "start" -1) (get nr "end" -1) (count nodes)))
             (conj (str "parser IR sentence " sid " node_range is outside nodes[] length "
                        (count nodes)))

             (and (contains? tags "orthographic-katakana")
                  (empty? annotation-indices))
             (conj (str "parser IR sentence " sid
                        " has orthographic-katakana tag without annotation indices"))

             (some #(or (not (integer? %)) (neg? %) (>= % (count annotations)))
                   annotation-indices)
             (conj (str "parser IR sentence " sid
                        " has orthographic annotation index outside annotations[]")))))
       (map-indexed vector sentences))
      (mapcat
       (fn [paragraph]
         (sentence-tiling-errors paragraph
                                 (get sentences-by-pid (get paragraph "id") [])))
       (filter #(= "body" (get % "role")) paragraphs))))))

(defn publication-sentence-evidence-errors [parser-ir]
  (let [coherence-errors (sentence-coherence-errors parser-ir)]
    (cond-> []
      (nil? (get parser-ir "sentence_segmentation"))
      (conj "parser IR publication requires sentence_segmentation")

      true
      (into coherence-errors))))

(defn ensure-publication-sentence-evidence! [parser-ir]
  (let [errors (publication-sentence-evidence-errors parser-ir)]
    (when (seq errors)
      (throw (ex-info "Parser-IR publication sentence evidence is invalid"
                      {:errors errors})))
    parser-ir))
