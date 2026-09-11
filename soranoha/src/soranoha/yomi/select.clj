(ns soranoha.yomi.select
  "Join work ZIPs against catalog text-file basenames, admit each on the rights
  standing it can be published under, and reject slug collisions before
  producing any slug-addressed result.

  Rights admission is done here because the manifest, the corpus delta and
  the assessment all determine corpus membership by calling this function. A
  separate step that a caller could skip would let the delta report a work the
  manifest never published."
  (:require [babashka.fs :as fs]
            [clojure.string :as string]
            [soranoha.aozora.rights-notice :as notice]
            [soranoha.aozora.source-bundle :as source-bundle]
            [soranoha.yomi.catalog :as catalog])
  (:import (java.nio ByteBuffer)
           (java.nio.charset Charset CodingErrorAction)))

(defn- normalized-path [f]
  (string/replace (str f) java.io.File/separator "/"))

(defn- normalized-abs-path [f]
  (fs/normalize (fs/absolutize f)))

(defn- work-zip-files
  "Every .zip under cards/, sorted by path, as {:file :rel :relpath}.
  :rel is the path relative to the root; :relpath repeats it when it
  has the work-ZIP shape cards/<6 digits>/files/<name>.zip and is nil
  otherwise."
  [aozora-root]
  (let [root (normalized-abs-path aozora-root)]
    (->> (fs/glob (fs/path (str aozora-root) "cards") "**.zip")
         (map fs/file)
         (sort-by str)
         (mapv (fn [file]
                 (let [rel (normalized-path
                            (fs/relativize root (normalized-abs-path file)))]
                   {:file file
                    :rel rel
                    :relpath (when (re-matches #"^cards/[0-9]{6}/files/[^/]+\.zip$" rel)
                               rel)}))))))

(defn- card-directory
  "The contributor card directory a work ZIP lives under. The only element
  distinguishing one work_id filed under several contributor cards; fails
  closed on a relpath that names none."
  [relpath]
  (or (second (re-matches #"^cards/([0-9]{6})/files/[^/]+\.zip$" relpath))
      (throw (ex-info "work ZIP relpath names no card directory"
                      {:code "unslugifiable-source-relpath"
                       :text_zip_relpath relpath}))))

(defn- work-identifier
  "The catalog work id in the six-digit form Aozora Bunko files works under. Fails
  closed rather than padding a short value: this is half of a permanent public
  identifier, so a row that does not carry the documented shape must stop the
  build instead of having a shape guessed for it."
  [work-id]
  (or (re-matches #"[0-9]{6}" (str work-id))
      (throw (ex-info "catalog work id is not the six-digit form"
                      {:code "unslugifiable-work-id"
                       :work_id work-id}))))

(defn slug
  "The name of the work one source publishes under: a function of that
  source's own coordinates alone, so unrelated corpus changes can never move
  it. It names the work and never its bytes, which is why it survives a
  re-proofread or a repackaged archive unchanged.

  `<work-id>_<card-directory>`, six digits each. Both components identify the
  work; neither describes how the build reached it. The archive filename stem
  and format variants are omitted because they record fetch details rather than
  text identity: changing edition handling would otherwise move the identifier
  of an unchanged text or freeze a stale one. Exact edition identity is carried
  per work by `source_content_hash` in the manifest.

  The card directory is retained even though work id alone is unique across
  the present corpus: it is the documented disambiguator for one work_id filed
  under several contributor cards, so keeping it makes the scheme collision-
  safe by construction rather than by observation."
  [work-id relpath]
  (str (work-identifier work-id) "_" (card-directory relpath)))

(defn- candidate-slug-collisions
  "Pure: slugs claimed by more than one candidate, with their sources."
  [candidates]
  (->> candidates
       (map (fn [{:keys [row relpath]}]
              {"slug" (slug (catalog/row-work-id row) relpath)
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

(def ^:private source-charsets
  "The encodings the catalog declares for a work's text file. A row that
  declares nothing is Shift_JIS; only ten catalog rows declare UTF-8."
  {"UTF-8" "UTF-8"})

(defn- primary-text
  "The work's primary text member, decoded with the encoding its catalog row
  declares.

  Unmappable bytes are replaced rather than raised, so one bad byte does not
  stop a 17,000-work build. A file that is not the encoding it declares loses
  its notice and is refused admission, which is the correct outcome."
  [file row]
  (let [charset (Charset/forName
                 (get source-charsets
                      (get row "テキストファイル符号化方式")
                      "windows-31j"))
        bytes (:primary-text-bytes (source-bundle/inspect-zip file))]
    (str (.decode (doto (.newDecoder charset)
                    (.onMalformedInput CodingErrorAction/REPLACE)
                    (.onUnmappableCharacter CodingErrorAction/REPLACE))
                  (ByteBuffer/wrap ^bytes bytes)))))

(defn- admit
  "The rights standing a candidate is publishable under, or the reason it is
  not. `rows-by-work-id` supplies every catalog row describing the work, since
  a work filed under several contributor cards has a row under each and they
  have to agree about whether a right subsists."
  [rows-by-work-id {:keys [file row]}]
  (notice/standing (get rows-by-work-id (catalog/row-work-id row))
                   #(primary-text file row)))

(defn select-candidates
  "The selection join: work ZIPs × catalog rows by text-file basename, sorted
  by relpath, injectivity-asserted, then admitted on rights. Returns
  {:candidates [{:file :relpath :row :slug :rights}] :rejected [{path reason}]}.

  Each admitted candidate carries the standing it is published under, so no
  later stage derives the terms again and reaches a different answer."
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
                                :slug (slug (catalog/row-work-id row) relpath)))
                       selected)
        rows-by-work-id (group-by catalog/row-work-id rows)
        assessed (mapv #(assoc % :rights (admit rows-by-work-id %)) selected)
        selected (into [] (keep (fn [{:keys [rights] :as candidate}]
                                  (when-let [standing (:standing rights)]
                                    (assoc candidate :rights standing))))
                       assessed)
        ;; Rights refusals record the licence as well as the reason, so the
        ;; build report shows which terms were declined without reopening the
        ;; archive.
        refused-on-rights (->> assessed
                               (keep (fn [{:keys [relpath rights]}]
                                       (when-let [reason (:refused rights)]
                                         (cond-> {"path" relpath
                                                  "reason" (str "rights-" (name reason))}
                                           (:licence rights)
                                           (assoc "licence" (:licence rights))
                                           (:flags rights)
                                           (assoc "copyright_flags" (:flags rights))))))
                               vec)
        ;; Admitted and rights-refused works are both accounted for here, so
        ;; neither reaches the cond below and gets reported as not-selected.
        accounted (into (set (map :relpath selected))
                        (map #(get % "path"))
                        refused-on-rights)
        rejected (->> candidates
                      (remove #(contains? accounted (:relpath %)))
                      (mapv (fn [{:keys [file rel relpath]}]
                              {"path" rel
                               "reason" (cond
                                          (nil? relpath)
                                          "not-under-cards-files"

                                          (not (contains? rows-by-basename
                                                          (.getName ^java.io.File file)))
                                          "not-catalog-text-zip"

                                          :else
                                          "not-selected")})))]
    {:candidates selected
     :rejected (vec (sort-by #(get % "path") (into rejected refused-on-rights)))}))
