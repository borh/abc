(ns abc.tools.edn-registry)

(defn missing-entry-key-errors
  [entry-label idx required-keys entry]
  (->> required-keys
       (remove #(contains? entry %))
       (mapv #(str entry-label " " idx " is missing " %))))

(defn registry-errors
  [{:keys [registry label entry-error-fn duplicate-error-fn]}]
  (vec
   (cond
     (not (map? registry))
     [(str label " must be an EDN map")]

     (not (contains? registry :entries))
     [(str label " is missing :entries")]

     (not (vector? (:entries registry)))
     [(str label " :entries must be a vector")]

     (empty? (:entries registry))
     [(str label " :entries must not be empty")]

     :else
     (concat
      (mapcat (fn [[idx entry]] (entry-error-fn idx entry))
              (map-indexed vector (:entries registry)))
      (when duplicate-error-fn
        (duplicate-error-fn (:entries registry)))))))
