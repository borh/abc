(ns soranoha.za.naming
  "How the serving layer names things a reader saves: people, files and bulk
  archives.

  Renderings, not facts. The signed catalog carries `archive_stem` and the
  romanized name parts; it excludes the assembled filename they render into,
  because a derived value stored beside its own inputs can disagree with them
  and the chain is append-only. The rule lives here so it can be corrected in a
  later release without rewriting a signed record.

  A download filename is a convenience. The citable thing is the identifier,
  which is why it is a component of every filename rather than an alternative
  to one: 470 author-and-title pairs are shared by 2357 works in the Aozora Bunko
  catalog, so a name built from author and title alone would silently
  overwrite more than one work in ten of any bulk extraction.

  Pure: same catalog entry in, same name out, with no clock, environment or
  filesystem in the path. The browse layer's pages carry these names, and the
  exporter's reuse check compares those pages byte for byte."
  (:require [clojure.string :as string])
  (:import (java.text Normalizer Normalizer$Form)))

(defn person-name-ja [{:strs [family_name given_name]}]
  (let [parts (remove string/blank? [family_name given_name])]
    (when (seq parts) (string/join " " parts))))

(defn person-name-romaji [{:strs [family_name_romaji given_name_romaji]}]
  (let [parts (remove string/blank? [family_name_romaji given_name_romaji])]
    (when (seq parts) (string/join " " parts))))

(defn person-label
  "Japanese name where there is one, romaji otherwise. A catalog entry can
  carry either alone: of the catalog's 1334 people, 73 have no given-name
  romaji, so neither script is required."
  [person]
  (or (person-name-ja person) (person-name-romaji person) "—"))

(defn authors-of [work]
  (filterv #(= "著者" (get % "relation_to_work")) (get work "contributors")))

(defn byline [work]
  (let [authors (authors-of work)]
    (when (seq authors)
      (string/join "、" (map person-label authors)))))

(def ^:private transliterations
  "Letters that survive NFKD intact but have a settled Latin spelling. Kept
  small on purpose: it covers what the Aozora Bunko person catalog actually
  contains plus its immediate neighbours, and anything else falls through to
  the person-id fallback rather than to a guess. Measured over the whole
  catalog, the only romanized name with nothing Latin in it at all is person
  000361, Толстой."
  {\ł "l" \Ł "L" \ø "o" \Ø "O" \æ "ae" \Æ "Ae" \œ "oe" \Œ "Oe"
   \ß "ss" \đ "d" \Đ "D" \ð "d" \Ð "D" \þ "th" \Þ "Th"
   \ı "i" \İ "I" \u2019 "'"})

(def ^:private component-limit
  "Characters kept from one rendered component. No Aozora Bunko author component
  reaches it — the longest measured is 38 — and no stem is known to, but a
  filename has to stay under the 255-byte limit every filesystem in play
  imposes, and truncation must be a rule rather than an accident. Uniqueness
  does not depend on it: the identifier component is never truncated."
  64)

(defn- ascii-component
  "One filename component: Latin letters, digits and apostrophes, with word
  boundaries as underscores.

  Apostrophes are kept because they are meaningful: 204 Aozora Bunko stems use
  Hepburn's n' disambiguation, as in `ippon'ashino_heitai`, and shells handle
  them with ordinary quoting. Hyphens fold to underscores, which is what keeps the
  three components of a filename separable by splitting on the hyphen."
  [s]
  (if (string/blank? s)
    ""
    (-> (Normalizer/normalize s Normalizer$Form/NFKD)
        (string/replace #"\p{M}+" "")
        (string/escape transliterations)
        (string/replace #"[^A-Za-z0-9']+" "_")
        (string/replace #"^_|_$" "")
        (as-> folded (subs folded 0 (min (count folded) component-limit)))
        (string/replace #"[_']+$" ""))))

(defn- author-component
  "The first 著者 in the catalog's own order, or the first contributor of any
  relation when a work records none. One name, not all of them: 422 works
  have several authors, and a filename that grew with the contributor list
  would be unbounded. The person id is the fallback when nothing Latin
  survives, so the name still resolves through /authors/<id>."
  [work]
  (let [contributors (get work "contributors")
        person (or (first (authors-of work)) (first contributors))
        parts (remove string/blank?
                      [(ascii-component (get person "family_name_romaji"))
                       (ascii-component (get person "given_name_romaji"))])]
    (if (seq parts)
      (string/join "_" parts)
      (or (get person "person_id") "unknown"))))

(def extensions
  "Artifact type to the extension a reader expects for those bytes. The
  predictable routes name the type and carry no extension, which is why a
  browser save or `curl -O` there produces a file called `tei`."
  {"tei" "xml"
   "plaintext" "txt"
   "markdown" "md"
   "tei-validation" "validation.json"})

(defn filename
  "<Author_Romaji>-<aozora-stem>-<identifier>.<ext>, as in
  `Shiraki_Shizu-sanjusanno_shi-000002_000012.xml`.

  The middle component is Aozora Bunko's own name for the work's primary text
  member. Volunteers hand-curated those names with word boundaries for the
  whole archive: the longest unbroken run is a median of 8 characters against
  a median of 14 for a mechanical romanization of the title reading, which
  has no word boundaries to segment on without a morphological analyzer."
  [work artifact-type]
  (let [extension (or (get extensions artifact-type)
                      (throw (ex-info "no download extension for artifact type"
                                      {:reason :unknown-artifact-type
                                       :type artifact-type})))
        stem (ascii-component (get work "archive_stem"))]
    (str (author-component work)
         "-" (if (string/blank? stem) "text" stem)
         "-" (get work "slug")
         "." extension)))

(def bulk-artifact-types
  "The types published as bulk archives, in the order their archives are
  generated. TEI and plain text are what a corpus is wanted for; Markdown and
  the validation reports are per-work conveniences, and bulk-publishing them
  would double the archive cost for a use nobody has stated."
  ["tei" "plaintext"])

(defn- bundle-extension [artifact-type]
  (when-not (contains? extensions artifact-type)
    (throw (ex-info "no bulk archive for artifact type"
                    {:reason :unknown-artifact-type :type artifact-type})))
  artifact-type)

(defn corpus-bundle-path [artifact-type]
  (str "bulk/soranoha-" (bundle-extension artifact-type) ".zip"))

(defn author-bundle-path [person person-id artifact-type]
  (let [name (ascii-component (person-name-romaji person))]
    (str "bulk/authors/soranoha-"
         (if (string/blank? name) person-id (str name "-" person-id))
         "-" (bundle-extension artifact-type) ".zip")))

(defn ndc-bundle-path [class-key artifact-type]
  (str "bulk/ndc/soranoha-ndc-" class-key "-" (bundle-extension artifact-type) ".zip"))
