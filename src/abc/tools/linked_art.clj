(ns abc.tools.linked-art
  "Linked Art-compatible JSON-LD publication-view harness (ADR 0013).

  The canonical ABC manifest stays JCS-canonical JSON. This namespace
  derives a Linked Art-flavored JSON-LD view from a manifest and a
  metadata-record, runs deterministic JSON-LD 1.1 expansion via
  titanium-json-ld, and writes the canonical bytes to disk.

  Hard rule: nothing in this harness may change `manifest_identity_object`
  or the manifest `artifact_id`. The expanded form preserves
  `abc:artifactId` literally; the harness verifies that as an explicit
  identity-invariant check."
  (:require [abc.tools.files :as files]
            [abc.tools.hash :as hash]
            [abc.tools.jcs :as jcs]
            [abc.tools.json :as abc-json]
            [charred.api :as json]
            [clojure.java.io :as io]
            [clojure.string :as string])
  (:import [com.apicatalog.jsonld JsonLd]
           [com.apicatalog.jsonld.document JsonDocument]
           [com.apicatalog.jsonld.http.media MediaType]
           [com.apicatalog.jsonld.loader DocumentLoader]
           [java.io ByteArrayInputStream]
           [java.net URI]))

(def ^:const context-public-uri
  "https://w3id.org/abc/contexts/abc-v0.jsonld")

(def ^:const linked-art-engine
  "titanium-json-ld:1.7.0")

(def ^:const candidate-canonical-name
  "linked-art-candidate.jsonld")

(def ^:const expanded-canonical-name
  "linked-art-expanded.normalized.json")

(def ^:const candidate-base-uri
  "https://w3id.org/abc/example/work/")

(def ^:const artifact-base-uri
  "https://w3id.org/abc/artifact/")

(def ^:const aozora-work-id-type-uri
  "https://w3id.org/abc/AozoraWorkId")

(def ^:const japanese-language-aat-uri
  "http://vocab.getty.edu/aat/300388277")

(defn context-hash
  "Return `sha256:<hex>` over the JCS-canonicalised context document.
  The on-disk pretty-printed bytes are not hashed directly so that
  whitespace does not enter the hash."
  [context-path]
  (-> (files/read-json context-path)
      jcs/canonical-json-bytes
      hash/sha256-bytes
      hash/format-sha256))

(defn- read-bytes ^bytes [path]
  (with-open [in (io/input-stream (io/file path))]
    (.readAllBytes in)))

(defn- artifact-uri [artifact-id]
  (str artifact-base-uri (string/replace artifact-id #"^sha256:" "sha256-")))

(defn record->linked-art
  "Build the deterministic Linked Art candidate map from `manifest`
  and `metadata-record` (raw JSON values as read by abc.tools.files).
  The shape mirrors a small Linked Art HumanMadeObject view: a Name,
  an Aozora-work-id Identifier, a card_url DigitalObject, and an
  explicit non-canonical disclaimer LinguisticObject."
  [{:keys [manifest metadata-record]}]
  (let [work (get metadata-record "work")
        work-id (get work "work_id")
        title (get work "title")
        card-url (get work "card_url")
        artifact-id (get manifest "artifact_id")]
    {"@context" context-public-uri
     "id" (str candidate-base-uri work-id "/linked-art-candidate")
     "type" "HumanMadeObject"
     "_label" (str title " derived publication view candidate")
     "canonicalManifest" (artifact-uri artifact-id)
     "artifactId" artifact-id
     "identified_by" [{"type" "Name"
                       "content" title
                       "language" [{"id" japanese-language-aat-uri
                                    "type" "Language"
                                    "_label" "Japanese"}]}
                      {"type" "Identifier"
                       "content" work-id
                       "classified_as" [{"id" aozora-work-id-type-uri
                                         "type" "Type"
                                         "_label" "Aozora work ID"}]}]
     "referred_to_by" [{"type" "LinguisticObject"
                        "content" "Candidate Linked Art-compatible view generated from ABC metadata; not canonical for v0."}]
     "subject_of" [{"id" card-url
                    "type" "DigitalObject"
                    "_label" "Aozora Bunko card"}]}))

(defn- local-context-loader
  "Return a titanium DocumentLoader that resolves only the ABC public
  context URI from in-memory bytes and refuses every other URL. This
  pins the harness to the committed `contexts/abc-v0.jsonld` and
  prevents network fetches in the sandbox."
  ^DocumentLoader
  [^bytes context-bytes]
  (let [allowed (URI/create context-public-uri)]
    (reify DocumentLoader
      (loadDocument [_ uri _options]
        (if (= uri allowed)
          (let [doc (JsonDocument/of MediaType/JSON_LD
                                     (ByteArrayInputStream. context-bytes))]
            (.setDocumentUrl doc allowed)
            doc)
          (throw (ex-info "Linked Art harness refuses to fetch external JSON-LD contexts"
                          {:requested (str uri)
                           :allowed [context-public-uri]})))))))

(defn- json-structure->clj
  "Convert a jakarta.json.JsonStructure (titanium expand output) into
  Clojure data via JSON serialisation. This is acceptable for v0
  because the expanded form contains only strings, IRIs, and arrays."
  [structure]
  (json/read-json (str structure)))

(defn expand-document
  "Run titanium JSON-LD expansion on `candidate-bytes`, resolving the
  ABC public context from `context-bytes` only. Returns the expanded
  array as Clojure data with deterministic key ordering."
  [^bytes candidate-bytes ^bytes context-bytes ^String document-uri]
  (let [doc (JsonDocument/of MediaType/JSON_LD
                             (ByteArrayInputStream. candidate-bytes))
        api (-> (JsonLd/expand doc)
                (.loader (local-context-loader context-bytes))
                (.base document-uri))
        expanded (.get api)]
    (abc-json/prepare-deterministic-json
     (json-structure->clj expanded))))

(defn- expanded-artifact-id
  "Extract the literal value at the expanded `abc:artifactId` predicate.
  Returns nil if the predicate is absent."
  [expanded]
  (let [predicate "https://w3id.org/abc/artifactId"]
    (some-> expanded
            first
            (get predicate)
            first
            (get "@value"))))

(defn write-publication-view!
  "Regenerate the Linked Art candidate, expanded view, and validation
  result for the example work. Returns a map summarising what was
  written, including the canonical context hash and the identity-
  invariant outcome."
  [{:keys [manifest-path metadata-record-path context-path
           candidate-path expanded-path result-path]}]
  (let [manifest (files/read-json manifest-path)
        metadata-record (files/read-json metadata-record-path)
        manifest-artifact-id (get manifest "artifact_id")
        candidate (record->linked-art {:manifest manifest
                                       :metadata-record metadata-record})
        _ (abc-json/write-deterministic-json-file! candidate-path candidate)
        candidate-bytes (read-bytes candidate-path)
        context-bytes (read-bytes context-path)
        candidate-doc-uri (get candidate "id")
        expanded (expand-document candidate-bytes context-bytes candidate-doc-uri)
        ctx-hash (context-hash context-path)
        invariant-id (expanded-artifact-id expanded)
        identity-preserved? (= manifest-artifact-id invariant-id)
        expanded-doc {"context" context-public-uri
                      "context_hash" ctx-hash
                      "engine" linked-art-engine
                      "expanded" expanded
                      "source" candidate-canonical-name}
        result {"candidate" candidate-canonical-name
                "context" context-public-uri
                "context_hash" ctx-hash
                "engine" linked-art-engine
                "expanded" expanded-canonical-name
                "identity_invariant" {"manifest_artifact_id" manifest-artifact-id
                                      "expanded_artifact_id" invariant-id
                                      "preserved_after_expansion" identity-preserved?}
                "status" (if identity-preserved? "ok" "identity_invariant_violation")}]
    (abc-json/write-deterministic-json-file! expanded-path expanded-doc)
    (abc-json/write-deterministic-json-file! result-path result)
    (when-not identity-preserved?
      (throw (ex-info "Linked Art expansion violated the artifactId identity invariant"
                      {:manifest-artifact-id manifest-artifact-id
                       :expanded-artifact-id invariant-id})))
    {:candidate-path (str candidate-path)
     :expanded-path (str expanded-path)
     :result-path (str result-path)
     :context-hash ctx-hash
     :identity-preserved identity-preserved?}))
