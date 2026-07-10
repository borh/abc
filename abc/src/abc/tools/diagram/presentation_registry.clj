(ns abc.tools.diagram.presentation-registry
  (:refer-clojure :exclude [run!])
  (:require [abc.tools.diagram.graphviz :as graphviz]
            [abc.tools.diagram.presentation-figures :as figures]
            [abc.tools.diagram.presentation-svg :as svg]
            [babashka.fs :as fs])
  (:import [java.nio.file Files StandardCopyOption]
           [java.nio.file.attribute FileAttribute]))

(def registry
  [{:id :reproducibility
    :dot-path "docs/figures/soranoha-reproducibility-architecture.dot"
    :svg-path "docs/figures/soranoha-reproducibility-architecture.svg"}
   {:id :publication
    :dot-path "docs/figures/soranoha-publication-pipeline.dot"
    :svg-path "docs/figures/soranoha-publication-pipeline.svg"}])

(defn- atomic-spit! [target content]
  (let [parent (fs/path (fs/parent target))]
    (fs/create-dirs parent)
    (let [temp (Files/createTempFile parent ".presentation-" ".tmp"
                                     (make-array FileAttribute 0))]
      (try
        (spit (.toFile temp) content)
        (Files/move temp (fs/path target)
                    (into-array StandardCopyOption
                                [StandardCopyOption/ATOMIC_MOVE
                                 StandardCopyOption/REPLACE_EXISTING]))
        (finally
          (Files/deleteIfExists temp))))))

(defn- drift [path expected]
  (when (or (not (fs/regular-file? path))
            (not= expected (slurp (fs/file path))))
    (str "DRIFT: " path
         " is stale; run `clojure -M:abc/presentation-diagrams`")))

(defn run-with! [entries graphs {:keys [check?]}
                 {:keys [dot-render svg-render]}]
  (try
    (fs/with-temp-dir [temp {}]
      (let [by-id (into {} (map (juxt :id identity)) graphs)
            rendered
            (mapv (fn [{:keys [id] :as entry}]
                    (let [graph (or (by-id id)
                                    (throw (ex-info "presentation registry has no graph"
                                                    {:id id})))
                          dot (dot-render graph)
                          svg (svg-render graph dot (fs/file temp))]
                      (assoc entry :dot dot :svg svg)))
                  entries)]
        (if check?
          (let [drifts (vec
                        (mapcat (fn [{:keys [dot-path svg-path dot svg]}]
                                  (keep identity [(drift dot-path dot)
                                                  (drift svg-path svg)]))
                                rendered))]
            {:ok? (empty? drifts) :problems [] :drifts drifts :wrote []})
          (do
            (doseq [{:keys [dot-path svg-path dot svg]} rendered]
              (atomic-spit! dot-path dot)
              (atomic-spit! svg-path svg))
            {:ok? true :problems [] :drifts []
             :wrote (vec (mapcat (juxt :dot-path :svg-path) rendered))}))))
    (catch Exception ex
      {:ok? false
       :problems [(or (ex-message ex) (str ex))]
       :exception ex :drifts [] :wrote []})))

(defn run! [options]
  (run-with! registry (figures/graphs) options
             {:dot-render graphviz/dot
              :svg-render svg/render-svg!}))

(defn -main [& args]
  (let [unknown (remove #{"--check"} args)]
    (when (seq unknown)
      (binding [*out* *err*]
        (println "usage: clojure -M:abc/presentation-diagrams [--check]"))
      (System/exit 2))
    (let [{:keys [ok? problems drifts wrote]}
          (run! {:check? (boolean (some #{"--check"} args))})]
      (binding [*out* *err*]
        (doseq [problem problems] (println "ERROR:" problem))
        (doseq [item drifts] (println item)))
      (doseq [path wrote] (println "wrote" path))
      (when (and ok? (empty? wrote))
        (println "all presentation diagrams current"))
      (System/exit (if ok? 0 1)))))
