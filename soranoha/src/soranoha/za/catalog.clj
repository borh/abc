(ns soranoha.za.catalog
  "The release-level bibliographic catalog: one entry per published work,
  built from the same kura CAS blobs the release publishes from.

  Carries bibliographic facts only (such as `archive_stem` and romanized name
  parts) without rendered presentation strings. Filename rules and citation
  formatting belong to the serving layer."
  (:require [charred.api :as json]
            [clojure.string :as string]
            [soranoha.kura.cas :as cas]))

(defn- fail! [reason data]
  (throw (ex-info (str "catalog assembly failed: " (name reason))
                  (assoc data :reason reason))))

(defn- cas-json [cas-dir hex reason data]
  (when-not hex (fail! reason data))
  (let [bytes (or (cas/get-bytes cas-dir hex)
                  (fail! :catalog-input-missing-from-cas (assoc data :hex hex)))]
    (json/read-json (String. ^bytes bytes "UTF-8"))))

(defn- archive-stem
  "The Aozora Bunko archive's own name for the work's primary text member, without
  its extension: `92_ruby_164.zip` holds `kumono_ito.txt`, so the stem is
  `kumono_ito` and not the archive's own name. Aozora Bunko volunteers
  hand-curated these names with word boundaries, which is why the serving
  layer renders download filenames from this rather than from a mechanical
  romanization of the title reading. Every published work has exactly one
  primary text member: the source bundle fails closed on none and on
  several, so no fallback rule is reachable here."
  [slug primary-text-member]
  (when-not (string? primary-text-member)
    (fail! :missing-primary-text-member {:slug slug}))
  (let [name (last (string/split primary-text-member #"/"))
        dot (string/last-index-of name ".")]
    (if (and dot (pos? dot)) (subs name 0 dot) name)))

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
  source facts its extract stage produced."
  [slug source-content-hash metadata-record persons primary-text-member]
  (let [work (get metadata-record "work")]
    {"slug" slug
     "source_content_hash" source-content-hash
     "title" (get work "title")
     "title_reading" (get work "title_reading")
     "subtitle" (get work "subtitle")
     "original_title" (get work "original_title")
     "first_published" (get work "first_published")
     "orthographic_style" (get work "orthographic_style")
     "ndc" (get work "ndc")
     "card_url" (get work "card_url")
     "archive_stem" (archive-stem slug primary-text-member)
     "contributors" (vec (sort-by #(get % "person_id")
                                  (map #(contributor-entry slug persons %)
                                       (get metadata-record "contributors"))))
     "source_editions" (mapv source-edition-entry (get work "source_editions"))}))

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
                         source-content-hash]}
                 (or (get outputs slug)
                     (fail! :catalog-work-not-built {:slug slug}))]
             (work-entry slug
                         (or (some->> source-content-hash
                                      (re-matches #"sha256:([0-9a-f]{64})")
                                      second)
                             (fail! :malformed-source-content-hash
                                    {:slug slug :value source-content-hash}))
                         (cas-json cas-dir metadata-record
                                   :missing-metadata-record {:slug slug})
                         (cas-json cas-dir persons
                                   :missing-person-records {:slug slug})
                         primary-text-member)))
         published)})
