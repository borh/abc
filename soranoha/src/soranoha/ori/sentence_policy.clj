(ns soranoha.ori.sentence-policy)

(defn- fragment-field-errors [sentence]
  (let [part (get sentence "part")
        fragment-group (get sentence "fragment_group")
        next-id (get sentence "next_id")
        prev-id (get sentence "prev_id")
        sid (get sentence "id")]
    (cond-> []
      (and part (nil? fragment-group))
      (conj (str "parser IR sentence " sid " has part but no fragment_group"))

      (and (nil? part) fragment-group)
      (conj (str "parser IR sentence " sid " has fragment_group but no part"))

      (and (= part "I") (nil? next-id))
      (conj (str "parser IR sentence " sid " part=I but no next_id"))

      (and (= part "I") prev-id)
      (conj (str "parser IR sentence " sid " part=I but has prev_id"))

      (and (= part "M") (nil? next-id))
      (conj (str "parser IR sentence " sid " part=M but no next_id"))

      (and (= part "M") (nil? prev-id))
      (conj (str "parser IR sentence " sid " part=M but no prev_id"))

      (and (= part "F") next-id)
      (conj (str "parser IR sentence " sid " part=F but has next_id"))

      (and (= part "F") (nil? prev-id))
      (conj (str "parser IR sentence " sid " part=F but no prev_id"))

      (and (nil? part) next-id)
      (conj (str "parser IR sentence " sid " has next_id but no part"))

      (and (nil? part) prev-id)
      (conj (str "parser IR sentence " sid " has prev_id but no part")))))

(defn- sentence-row-error-prefix [sentence]
  (str "parser IR sentence " (get sentence "id")))

(defn- orthographic-tagged? [sentence]
  (boolean
   (some #{"orthographic-katakana"}
         (get sentence "tags" []))))

(defn- valid-annotation-index? [annotations index]
  (and (integer? index)
       (<= 0 index)
       (< index (count annotations))))

(defn- valid-range? [rng]
  (let [start (get rng "start")
        end (get rng "end")]
    (and (integer? start)
         (integer? end)
         (<= start end))))

(defn- range-label [rng]
  (str (get rng "start") ".." (get rng "end")))

(defn- ranges-overlap? [left right]
  (and (valid-range? left)
       (valid-range? right)
       (< (get left "start") (get right "end"))
       (< (get right "start") (get left "end"))))

(defn- annotation-span-errors [sentence annotations annotation-indices]
  (let [sid (get sentence "id")
        sentence-span (get sentence "span")]
    (vec
     (keep
      (fn [index]
        (when (valid-annotation-index? annotations index)
          (let [annotation-range (get (nth annotations index)
                                      "source_byte_range")]
            (cond
              (not (valid-range? annotation-range))
              (str "parser IR sentence " sid
                   " orthographic annotation index " index
                   " has invalid source_byte_range")

              (not (ranges-overlap? annotation-range sentence-span))
              (str "parser IR sentence " sid
                   " orthographic annotation index " index
                   " range " (range-label annotation-range)
                   " does not overlap sentence span "
                   (range-label sentence-span))))))
      annotation-indices))))

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

(defn- duplicate-paragraph-id-errors [paragraphs]
  (->> paragraphs
       (map #(get % "id"))
       frequencies
       (keep (fn [[paragraph-id count]]
               (when (> count 1)
                 (str "parser IR paragraphs[] contains duplicate id " paragraph-id))))
       sort))

(defn- node-range-label [start end]
  (str start ".." end))

(defn- source-note-node-in-range? [nodes start end]
  (boolean
   (some #(= "source-note" (get % "type"))
         (subvec nodes start end))))

(defn paragraph-coherence-errors [parser-ir]
  (let [nodes (vec (get parser-ir "nodes" []))
        paragraphs (vec (get parser-ir "paragraphs" []))]
    (vec
     (concat
      (duplicate-paragraph-id-errors paragraphs)
      (loop [remaining paragraphs
             previous-end (Long/valueOf 0)
             errors []]
        (if-let [paragraph (first remaining)]
          (let [paragraph-id (get paragraph "id")
                node-range (get paragraph "node_range")
                start (get node-range "start")
                end (get node-range "end")
                outside? (not (and (integer? start)
                                   (integer? end)
                                   (<= 0 start end (count nodes))))
                non-monotonic? (and (integer? start)
                                    (< start previous-end))
                source-note-missing? (and (not outside?)
                                          (= "source-note" (get paragraph "role"))
                                          (= "direct" (get paragraph "classification"))
                                          (not (source-note-node-in-range?
                                                nodes start end)))
                errors (cond-> errors
                         outside?
                         (conj (str "parser IR paragraph " paragraph-id
                                    " node_range " (node-range-label start end)
                                    " is outside nodes[] length " (count nodes)))

                         (and (not outside?) non-monotonic?)
                         (conj (str "parser IR paragraph " paragraph-id
                                    " node_range starts before previous paragraph end "
                                    previous-end))

                         source-note-missing?
                         (conj (str "parser IR paragraph " paragraph-id
                                    " has role source-note but no source-note node in node_range")))]
            (recur (rest remaining)
                   (if (and (integer? end) (not outside?))
                     end
                     previous-end)
                   errors))
          errors))))))

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
               tagged? (orthographic-tagged? sentence)
               annotation-indices (get sentence "orthographic_annotation_indices" [])
               invalid-annotation-index? (some #(not (valid-annotation-index?
                                                      annotations
                                                      %))
                                               annotation-indices)]
           (into
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

              (and tagged?
                   (empty? annotation-indices))
              (conj (str "parser IR sentence " sid
                         " has orthographic-katakana tag without annotation indices"))

              (and (seq annotation-indices)
                   (not tagged?))
              (conj (str "parser IR sentence " sid
                         " has orthographic annotation indices without orthographic-katakana tag"))

              invalid-annotation-index?
              (conj (str "parser IR sentence " sid
                         " has orthographic annotation index outside annotations[]")))
            (concat (when tagged?
                      (annotation-span-errors sentence
                                              annotations
                                              annotation-indices))
                    (fragment-field-errors sentence)))))
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

      (and (get parser-ir "sentence_segmentation")
           (not= "parser_text_utf8" (get-in parser-ir ["sentence_segmentation" "coordinate_system"])))
      (conj "sentence_segmentation requires parser_text_utf8 coordinates")

      (and (get parser-ir "orthographic_annotations")
           (not= "parser_text_utf8" (get-in parser-ir ["orthographic_annotations" "coordinate_system"])))
      (conj "orthographic_annotations requires parser_text_utf8 coordinates")

      true
      (into coherence-errors))))

(defn ensure-publication-sentence-evidence! [parser-ir]
  (let [errors (publication-sentence-evidence-errors parser-ir)]
    (when (seq errors)
      (throw (ex-info "Parser-IR publication sentence evidence is invalid"
                      {:errors errors})))
    parser-ir))
