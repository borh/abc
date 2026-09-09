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
  "The reader-facing documents, in the order the landing page lists them.

  Route names are short and stable because they are cited. `/ns/tei` is the
  local form of the namespace IRI `https://w3id.org/soranoha/ns/tei` that every
  published TEI root declares: the permanent identifier redirects to it, so a
  consumer who dereferences the namespace arrives at the vocabulary that
  defines it.

  Each entry carries a Japanese label for the site's own bilingual chrome. The
  English side is the document's own first heading, read from the file, so the
  two cannot drift apart."
  [{:route "start-here" :path "docs/start-here.md"
    :ja "はじめに"}
   {:route "example" :path "docs/worked-example.md"
    :ja "一作品を読み解く"}
   {:route "glossary" :path "docs/user-glossary.md"
    :ja "用語集"}
   {:route "identifiers" :path "soranoha/docs/work-identifiers.md"
    :ja "作品識別子"}
   {:route "ns/tei" :path "soranoha/docs/tei-vocabulary.md"
    :ja "TEI 拡張語彙"}
   {:route "validation" :path "soranoha/docs/tei-validation.md"
    :ja "TEI の検証"}
   {:route "accountability" :path "soranoha/docs/source-accountability.md"
    :ja "原文の説明責任"}
   {:route "assessment" :path "soranoha/docs/assessment-evaluation.md"
    :ja "公開可否の判断"}
   {:route "annotation-layers" :path "docs/annotation-layers.md"
    :ja "注釈の層"}
   {:route "parser-invariants" :path "docs/parser-invariants.md"
    :ja "パーサの不変条件"}
   {:route "external-links" :path "soranoha/docs/external-links.md"
    :ja "外部リンク"}
   {:route "protocol" :path "docs/design/snh-protocol-v1.md"
    :ja "snh プロトコル v1"}])

(def generated
  "Documents whose page the browse layer assembles itself, because the page
  states facts it reads from the release: the rights grant the head manifest
  carries, and the head and DOI a citation has to name.

  The page opens in its own words and then carries the document from `:from`
  onward, so the opening is stated once. These resolve like any other served
  document: a link to `rights.md` reaches `/rights`."
  [{:route "rights" :path "docs/rights.md"
    :ja "権利について" :from "two-distinct-rights-layers"}
   {:route "citation" :path "docs/citation.md"
    :ja "引用のしかた" :from "cite-a-release-not-the-corpus"}])

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
    :path "soranoha/schemas/metadata-record.schema.json"}])

(def ^:private by-path
  (into {} (map (juxt :path identity)) (concat documents generated verbatim)))

(defn root
  "The directory the served files are read from."
  []
  (or (System/getenv "SORANOHA_SITE_DOCS") ".."))

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
      (if-let [{:keys [route]} (get by-path resolved)]
        {:href (str "/" route (when fragment (str "#" fragment)))}
        resolved))))
