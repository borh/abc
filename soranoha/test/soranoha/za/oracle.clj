(ns soranoha.za.oracle
  "The three-set delta acceptance oracle over two kernel runs and their
  published manifests: (a) the source/selection delta, (b) the stages
  invalidated and executed, (c) the artifact/manifest delta — with the
  explanation invariant that every executed stage of the second run is
  accounted for by a changed declared input. Manifest set-difference alone
  is not a delta oracle: source edits can preserve output bytes, and a
  catalog edit fans out to every work's metadata stage without touching
  artifacts."
  (:require [soranoha.yomi.catalog :as catalog]
            [soranoha.za.corpus :as corpus]))

(defn source-delta
  "(a) per-slug zip-content delta between two runs."
  [run-a run-b]
  (let [a (:zip-hashes run-a)
        b (:zip-hashes run-b)]
    {:added (set (remove a (keys b)))
     :removed (set (remove b (keys a)))
     :changed (set (for [[slug hex] b
                         :when (and (contains? a slug) (not= hex (a slug)))]
                     slug))}))

(defn executed-stages
  "(b) slug -> set of stage keys the engine actually executed in `run`."
  [run]
  (into {}
        (map (fn [[slug {:keys [cached]}]]
               [slug (set (for [[stage cached?] cached
                                :when (false? cached?)]
                            stage))]))
        (:results run)))

(defn works-delta
  "(c) per-slug works-entry delta between two decoded manifest values."
  [manifest-a manifest-b]
  (let [by-slug (fn [m] (into {}
                              (map (juxt #(get % "slug") identity))
                              (get m "works")))
        a (by-slug manifest-a)
        b (by-slug manifest-b)]
    {:added (set (remove a (keys b)))
     :removed (set (remove b (keys a)))
     :changed (set (for [[slug entry] b
                         :when (and (contains? a slug)
                                    (not= entry (a slug)))]
                     slug))
     :retained (set (for [[slug entry] b
                          :when (= entry (a slug))]
                      slug))}))

(defn work-ids [run]
  (into {}
        (map (fn [{:keys [slug row]}] [slug (catalog/row-work-id row)]))
        (:candidates run)))

(defn- stage-inputs
  "The declared inputs of every stage for `slug` in `run`, mirroring
  run-work!'s wiring: each entry is the exact derivation-key material a
  changed value of which must explain a re-execution."
  [run slug work-id]
  (let [outputs (get-in run [:results slug :outputs])]
    {:extract [(get-in run [:zip-hashes slug])]
     :metadata [(:catalog-hex run) work-id]
     :parse [(get-in outputs [:extract "primary-text"])]
     :convert [(get-in outputs [:parse "aat"])
               (get (corpus/source-facts run slug) "work_content_hash")]
     :render [(get-in outputs [:convert "parser-ir"])
              (get-in outputs [:metadata "metadata-record"])
              (get-in outputs [:metadata "persons"])]
     :validate [(get-in outputs [:render "tei"])]}))

(defn unexplained-executions
  "Invariant check: every stage executed in `run-b` for a work already
  present in `run-a` must have at least one changed declared input (a work
  new to run-b explains all its executions). Returns violations as
  [{:slug :stage}]; the oracle passes when this is empty."
  [run-a run-b]
  (let [ids (work-ids run-b)]
    (vec (for [[slug stages] (executed-stages run-b)
               :when (contains? (:results run-a) slug)
               :let [inputs-a (stage-inputs run-a slug (ids slug))
                     inputs-b (stage-inputs run-b slug (ids slug))]
               stage stages
               :when (= (get inputs-a stage) (get inputs-b stage))]
           {:slug slug :stage stage}))))
