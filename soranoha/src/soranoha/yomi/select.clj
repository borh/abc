;; Work selection over an aozorabunko checkout: enumerate work ZIPs under
;; cards/NNNNNN/files/, join against the catalog by text-file basename, and
;; assert slug injectivity BEFORE anything slug-addressed happens (the
;; 17,602-vs-17,595 silent-overwrite incident is the reason this assert
;; exists and runs first). Ported from abc.tools.soranoha-build-publication;
;; the slug function and ordering must not drift.
(ns soranoha.yomi.select
  (:require [babashka.fs :as fs]
            [clojure.java.io :as io]
            [clojure.string :as string]
            [soranoha.yomi.catalog :as catalog]))

(defn- normalized-path [f]
  (string/replace (str f) java.io.File/separator "/"))

(defn- normalized-abs-path [f]
  (fs/normalize (fs/absolutize f)))

(defn aozora-work-zip?
  "The work-ZIP shape: cards/<6 digits>/files/<name>.zip. Returns the
  normalized relpath when it matches, else nil."
  [root file]
  (let [rel (normalized-path
             (fs/relativize (normalized-abs-path root)
                            (normalized-abs-path file)))]
    (when (re-matches #"^cards/[0-9]{6}/files/[^/]+\.zip$" rel)
      rel)))

(defn work-zip-files
  "Every .zip under cards/, sorted by path, as {:file :relpath} where
  :relpath is nil for zips outside the work-ZIP shape."
  [aozora-root]
  (->> (fs/glob (fs/path (str aozora-root) "cards") "**.zip")
       (map fs/file)
       (sort-by str)
       (mapv (fn [file]
               {:file file
                :relpath (aozora-work-zip? aozora-root file)}))))

(defn card-directory
  "The contributor card directory a work ZIP lives under. The only element
  distinguishing one work_id filed under several contributor cards; fails
  closed on a relpath that names none."
  [relpath]
  (or (second (re-matches #"^cards/([0-9]{6})/files/[^/]+\.zip$" relpath))
      (throw (ex-info "work ZIP relpath names no card directory"
                      {:code "unslugifiable-source-relpath"
                       :text_zip_relpath relpath}))))

(defn slug
  "Publication identity for one source: a function of that source's own
  coordinates ALONE, so unrelated corpus changes can never move it."
  [work-id person-id relpath]
  (let [basename (.getName (io/file relpath))
        stem (subs basename 0 (- (count basename) (count ".zip")))]
    (str work-id "_" person-id "_" (card-directory relpath) "_" stem)))

(defn candidate-slug-collisions
  "PURE. Slugs claimed by more than one candidate, with their sources."
  [candidates]
  (->> candidates
       (map (fn [{:keys [row relpath]}]
              {"slug" (slug (catalog/row-work-id row)
                            (catalog/row-person-id row)
                            relpath)
               "text_zip_relpath" relpath}))
       (group-by #(get % "slug"))
       (filter (fn [[_ claims]] (< 1 (count claims))))
       (sort-by key)
       (mapv (fn [[work-slug claims]]
               {"slug" work-slug
                "sources" (mapv (fn [claim]
                                  {"text_zip_relpath"
                                   (get claim "text_zip_relpath")})
                                (sort-by #(get % "text_zip_relpath") claims))}))))

(defn assert-candidate-slugs-unique!
  "Return `candidates` when every candidate claims a distinct slug; throw
  before any slug-addressed write otherwise. continue-on-failure must never
  resolve a collision."
  [candidates]
  (let [collisions (candidate-slug-collisions candidates)]
    (when (seq collisions)
      (throw (ex-info "selected sources claim duplicate publication slugs"
                      {:code "publication-slug-collision"
                       :collisions collisions})))
    candidates))

(defn select-candidates
  "The selection join: work ZIPs × catalog rows by text-file basename,
  sorted by relpath, injectivity-asserted. Returns
  {:candidates [{:file :relpath :row :slug}] :rejected [{path reason}]}."
  [aozora-root rows]
  (let [rows-by-basename (catalog/catalog-index rows)
        candidates (work-zip-files aozora-root)
        selected (->> candidates
                      (keep (fn [{:keys [file relpath]}]
                              (when relpath
                                (when-let [row (get rows-by-basename
                                                    (.getName ^java.io.File file))]
                                  {:file file :relpath relpath :row row}))))
                      (sort-by :relpath)
                      vec)
        _ (assert-candidate-slugs-unique! selected)
        selected (mapv (fn [{:keys [row relpath] :as candidate}]
                         (assoc candidate
                                :slug (slug (catalog/row-work-id row)
                                            (catalog/row-person-id row)
                                            relpath)))
                       selected)
        selected-relpaths (set (map :relpath selected))
        rejected (->> candidates
                      (remove #(contains? selected-relpaths (:relpath %)))
                      (mapv (fn [{:keys [file relpath]}]
                              {"path" (or relpath
                                          (normalized-path
                                           (fs/relativize
                                            (normalized-abs-path aozora-root)
                                            (normalized-abs-path file))))
                               "reason" (cond
                                          (nil? relpath)
                                          "not-under-cards-files"

                                          (not (contains? rows-by-basename
                                                          (.getName ^java.io.File file)))
                                          "not-catalog-text-zip"

                                          :else
                                          "not-selected")})))]
    {:candidates selected
     :rejected rejected}))
