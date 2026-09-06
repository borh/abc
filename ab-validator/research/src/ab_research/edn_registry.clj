(ns ab-research.edn-registry)

(defn call-entry-error [entry-error-fn idx entry]
  (entry-error-fn idx entry))

(defn call-duplicate-error [duplicate-error-fn entries]
  (duplicate-error-fn entries))

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
      (mapcat (fn [[idx entry]] (call-entry-error entry-error-fn idx entry))
              (map-indexed vector (:entries registry)))
      (when duplicate-error-fn
        (call-duplicate-error duplicate-error-fn (:entries registry)))))))
