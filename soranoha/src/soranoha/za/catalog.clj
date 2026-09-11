(ns soranoha.za.catalog
  "The release-level bibliographic catalog: one entry per published work,
  built from the same kura CAS blobs the release publishes from.

  Carries bibliographic facts only (such as `archive_stem` and romanized name
  parts) without rendered presentation strings. Filename rules and citation
  formatting belong to the serving layer."
  (:require [clojure.string :as string]
            [soranoha.core.hash :as hash]
            [soranoha.core.json :as record-json]
            [soranoha.kura.cas :as cas]))

(defn- fail! [reason data]
  (throw (ex-info (str "catalog assembly failed: " (name reason))
                  (assoc data :reason reason))))

(defn- cas-json [cas-dir hex reason data]
  (when-not hex (fail! reason data))
  (let [bytes (or (cas/get-bytes cas-dir hex)
                  (fail! :catalog-input-missing-from-cas (assoc data :hex hex)))]
    (record-json/read-json-bytes bytes)))

(defn- archive-stem
  "The Aozora Bunko archive's own name for the work's primary text member, without
  its extension: `92_ruby_164.zip` holds `kumono_ito.txt`, so the stem is
  `kumono_ito` and not the archive's own name. Aozora Bunko volunteers
  hand-curated these names with word boundaries, which is why the serving
  layer renders download filenames from this rather than from a mechanical
  romanization of the title reading. Every published work has exactly one
  primary text member: the source bundle fails closed on none and on
  several, so no fallback rule is reachable here.

  Normalized to the character set the catalog's schema admits, which ten
  archives in the corpus need. The word boundaries are what the field is for,
  and both foldings preserve them, so nothing the serving layer reads off this
  changes. Stems are not unique to begin with, 17,602 works carrying 16,489 of
  them, and folding these ten collides none that were not already equal."
  [slug primary-text-member]
  (when-not (string? primary-text-member)
    (fail! :missing-primary-text-member {:slug slug}))
  (let [name (last (string/split primary-text-member #"/"))
        dot (string/last-index-of name ".")
        stem (-> (if (and dot (pos? dot)) (subs name 0 dot) name)
                 ;; Hepburn's n' disambiguation, as in `kan'yaku_igakushi`.
                 ;; 204 stems spell it with an apostrophe and four with the
                 ;; typographic quotation mark, which is the same mark and the
                 ;; same word boundary.
                 (string/replace "\u2019" "'")
                 string/trim
                 ;; a word boundary is an underscore everywhere else in these
                 ;; names, and six archives write one of theirs as a space.
                 ;; Underscores beside the space belong to the same boundary,
                 ;; so `hakubutsushi _atogaki` has one and not two; the two
                 ;; stems that carry a real `__` have no space near it and
                 ;; keep it.
                 (string/replace #"[\s_]*\s[\s_]*" "_"))]
    ;; The schema admits printable ASCII with no space, so a stem that still
    ;; does not is refused by name here rather than by position in the works
    ;; array when the whole catalog is decoded.
    (when-not (re-matches #"[!-~]+" stem)
      (fail! :archive-stem-outside-catalog-character-set
             {:slug slug :member primary-text-member :stem stem}))
    stem))

(defn- contributor-entry [slug persons {:strs [person_id relation_to_work]}]
  (let [person (or (get persons person_id)
                   (fail! :contributor-person-record-missing
                          {:slug slug :person_id person_id}))]
    {"person_id" person_id
     "family_name" (get person "family_name")
     "given_name" (get person "given_name")
     "family_name_romaji" (get person "family_name_romaji")
     "given_name_romaji" (get person "given_name_romaji")
     "relation_to_work" relation_to_work}))

(defn- source-edition-entry [{:strs [title publisher first_edition_year]}]
  {"title" title
   "publisher" publisher
   "first_edition_year" first_edition_year})

(defn work-entry
  "One catalog entry from a work's metadata record, person records and the
  source facts its extract stage produced.

  `rights` is the standing the underlying work is published under. The catalog
  carries it so that selecting works by their terms costs one file rather than
  one file per work: the only other published copy is inside each TEI header.

  `trailing-bytes-after-archive` records the trailing garbage byte count for archives
  that carry trailing bytes after the zip end-of-central-directory record. Enables
  readers and web views to distinguish upstream format anomalies from file corruption."
  [slug source-content-hash rights metadata-record persons primary-text-member
   trailing-bytes-after-archive]
  (let [work (get metadata-record "work")]
    (cond-> {"slug" slug
             "source_content_hash" source-content-hash
             "rights" rights
             "title" (get work "title")
             "title_reading" (get work "title_reading")
             "subtitle" (get work "subtitle")
             "original_title" (get work "original_title")
             "first_published" (get work "first_published")
             "orthographic_style" (get work "orthographic_style")
             "ndc" (get work "ndc")
             "card_url" (get work "card_url")
             "archive_stem" (archive-stem slug primary-text-member)
             ;; by the pair, because the pair is the entry's identity: sorting on the
             ;; person alone left two relations of one person in whatever order the
             ;; metadata record happened to carry them, which is not an order
             "contributors" (vec (sort-by (juxt #(get % "person_id") #(get % "relation_to_work"))
                                          (map #(contributor-entry slug persons %)
                                               (get metadata-record "contributors"))))
             "source_editions" (mapv source-edition-entry (get work "source_editions"))}
      trailing-bytes-after-archive
      (assoc "trailing_bytes_after_archive" trailing-bytes-after-archive))))

(defn catalog-value
  "The snh-catalog/1 value for `published` slugs, in the manifest's order.
  `outputs` is the assembler's per-slug map; every field is read back out of
  the CAS rather than carried alongside, so the catalog describes the same
  bytes the release publishes."
  [cas-dir published outputs]
  {"schema" "snh-catalog/1"
   "works"
   (mapv (fn [slug]
           (let [{:keys [metadata-record persons primary-text-member
                         source-content-hash rights trailing-bytes-after-archive]}
                 (or (get outputs slug)
                     (fail! :catalog-work-not-built {:slug slug}))]
             (work-entry slug
                         (or (hash/bare-sha256-hex source-content-hash)
                             (fail! :malformed-source-content-hash
                                    {:slug slug :value source-content-hash}))
                         (or rights (fail! :catalog-work-rights-missing
                                           {:slug slug}))
                         (cas-json cas-dir metadata-record
                                   :missing-metadata-record {:slug slug})
                         (cas-json cas-dir persons
                                   :missing-person-records {:slug slug})
                         primary-text-member
                         trailing-bytes-after-archive)))
         published)})
