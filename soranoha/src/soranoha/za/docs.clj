(ns soranoha.za.docs
  "The repository files the browse layer serves, and how a link between them
  resolves to a served URL.

  Two kinds. A document is Markdown, rendered to a page at its route. A
  verbatim file is served byte for byte at its own extension, because it is
  something a reader feeds to a tool: a schema, a licence text.

  Every entry names a repository-relative path, which is the same path the
  documents already use to link to each other. That is what lets a link be
  resolved without a second table: normalise the target against the linking
  document's own path and look the result up here.

  The files are read from a root directory rather than the classpath because
  they are ordinary repository documents that must stay editable and
  reviewable where they are. `SORANOHA_SITE_DOCS` names that root when the Nix
  wrapper runs the exporter, where there is no repository around the store
  path; the default suits a checkout, where the kernel runs from the
  `soranoha` directory and the repository root is one level up."
  (:require [babashka.fs :as fs]
            [clojure.string :as string]))

(def documents
  "The documents the site serves.

  One, and it is here because published bytes name it. Every TEI root declares
  the `snh:` namespace as `https://w3id.org/soranoha/ns/tei` and every header
  carries a pointer to it, so the namespace has to dereference to the
  vocabulary that defines it or the files point at nothing. The route is short
  and stable because it is cited.

  The project's other reader-facing documents stay in the repository until
  they are trimmed and checked. A served page is a claim the corpus makes in
  public; a repository document is one a reader has gone looking for.

  Holding one back is a staging state rather than a decision to keep it off
  the site. Each is expected to return here as it is verified, one at a time,
  and the operator judges when a document has cleared that bar."
  [{:route "ns/tei" :path "soranoha/docs/tei-vocabulary.md"
    :ja "TEI 拡張語彙"}])

(def generated
  "Pages the browse layer assembles itself, because each states facts it reads
  from the release: the rights grant the head manifest carries, and the head
  and DOI a citation has to name.

  `/rights` is not optional. Every manifest carries
  `rights.statement_url` and every published TEI header carries a `<ptr>` to
  the same URL, so the grant those bytes cite has to resolve.

  Each page carries its own statement rather than an excerpt of one. A reader
  who followed `rights.statement_url` out of a detached TEI file has to reach
  the answer here, so a forwarding address to a repository document would not
  discharge what the signed bytes promise.

  `:source` names the repository document that treats the subject at length.
  It is background a reader may want, not the page's substance."
  [{:route "rights" :ja "権利について"
    :source "docs/rights.md"}
   {:route "citation" :ja "引用のしかた"
    :source "docs/citation.md"}])

(def verbatim
  "Files served as themselves rather than as pages. Their routes keep the
  extension a reader's tool needs to recognise them."
  [{:route "license/apache-2.0.txt" :path "LICENSE"}
   {:route "license/cc0-1.0.txt" :path "LICENSE-CC0"}
   {:route "schemas/tei-profile.odd" :path "soranoha/schemas/tei-profile.odd"}
   {:route "schemas/tei-profile.rng" :path "soranoha/schemas/tei-profile.rng"}
   {:route "schemas/tei-profile.sch" :path "soranoha/schemas/tei-profile.sch"}
   {:route "schemas/tei-profile-generation.json"
    :path "soranoha/schemas/tei-profile-generation.json"}
   {:route "schemas/source-1.schema.json"
    :path "soranoha/resources/assessment/source-1.schema.json"}
   ;; these two are dereferenceable identifiers rather than documents a reader
   ;; follows: every person and metadata record names its schema by an IRI
   ;; under `w3id.org/soranoha/schemas/`, which redirects here
   {:route "schemas/person-record.schema.json"
    :path "soranoha/schemas/person-record.schema.json"}
   {:route "schemas/metadata-record.schema.json"
    :path "soranoha/schemas/metadata-record.schema.json"}
   ;; the same kind of entry for the toolchain's research layer: a mapping or
   ;; policy document carries its own IRI as `mapping_id` or `policy_id`, and
   ;; parser-IR output names the mapping it was derived from. The route is the
   ;; IRI's path, extension and all, because that is the string the bytes hold
   {:route "mappings/aat-v1-to-parser-ir-v1/generated-probe"
    :path "ab-validator/data/aat-to-parser-ir-mapping-v1.json"}
   {:route "mappings/aat-v2-to-parser-ir-v1/generated-probe"
    :path "ab-validator/data/aat-to-parser-ir-mapping-v2.json"}
   {:route "policies/source-region-publication-v0"
    :path "ab-validator/research/data/source-region-publication-policy-v0.json"}
   ;; five schemas also declare a versioned `schema_id` without an extension,
   ;; which records of that kind carry instead of the file name. Each resolves
   ;; to the schema that defines it
   {:route "schemas/parser-comparison-result-v2"
    :path "ab-validator/schemas/parser-comparison-result-v2.schema.json"}
   {:route "schemas/parser-study-axis-evidence-v1"
    :path "ab-validator/schemas/parser-study-axis-evidence.schema.json"}
   {:route "schemas/parser-study-axis-policy-v1"
    :path "ab-validator/schemas/parser-study-axis-policy.schema.json"}
   {:route "schemas/parser-study-diagnostic-lanes-v1"
    :path "ab-validator/schemas/parser-study-diagnostic-lanes.schema.json"}
   {:route "schemas/parser-study-evidence-index-v1"
    :path "ab-validator/schemas/parser-study-evidence-index.schema.json"}])

(defn root
  "The directory the served files are read from."
  []
  (or (System/getenv "SORANOHA_SITE_DOCS") ".."))

(def ^:private schema-directories
  "Directories whose every `*.schema.json` is served at `schemas/<file name>`.
  Each file's `$id` is that IRI, so the rule and the identifiers cannot
  disagree, and a schema added to either directory is served without an
  entry here."
  ["ab-validator/schemas" "ab-validator/research/schemas"])

(defn schema-files
  "One verbatim entry per schema file under `schema-directories`, read from
  the document root at call time so the served set follows the checkout or
  the store path in use."
  []
  (vec
   (for [dir schema-directories
         file (sort (map str (fs/list-dir (fs/file (root) dir) "*.schema.json")))]
     {:route (str "schemas/" (fs/file-name file))
      :path (str dir "/" (fs/file-name file))})))

(defn verbatim-files
  "Every file served as itself: the listed entries and the schema directories."
  []
  (into verbatim (schema-files)))

(defn- by-path
  ;; A generated page is keyed by the repository document it stands for, so a
  ;; link to `rights.md` still reaches `/rights`. The page no longer carries
  ;; that document's text, but it is still the route the project publishes for
  ;; it, and a link to a served route must not degrade into a bare path.
  []
  (into {} (map (juxt #(or (:path %) (:source %)) identity))
        (concat documents generated (verbatim-files))))

(defn read-text [path]
  (slurp (fs/file (root) path) :encoding "UTF-8"))

(defn read-bytes ^bytes [path]
  (fs/read-all-bytes (fs/file (root) path)))

(defn- external? [target]
  (or (string/starts-with? target "http://")
      (string/starts-with? target "https://")
      (string/starts-with? target "mailto:")))

(defn resolve-link
  "Resolve one Markdown link target found in the document at `from`.

  Returns a hiccup attribute map when the target is reachable from the served
  site, and the repository path as a string when it is a repository file the
  site does not serve. `soranoha.za.markdown` renders the second form as the
  path in text, so a reader is told where the file is instead of following an
  anchor to nothing."
  [from target]
  (cond
    (external? target) {:href target}
    (string/starts-with? target "#") {:href target}
    :else
    (let [[path fragment] (string/split target #"#" 2)
          resolved (str (fs/normalize (fs/path (fs/parent from) path)))]
      (if-let [{:keys [route]} (get (by-path) resolved)]
        {:href (str "/" route (when fragment (str "#" fragment)))}
        resolved))))
