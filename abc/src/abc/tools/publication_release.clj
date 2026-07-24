(ns abc.tools.publication-release
  "The release SAFETY boundary.

  Two layers, deliberately separated:

  - `release-problems` / `release-admissible?` are PURE projections over
    already-loaded values (an index value, a pre-computed closure problem
    vector, an authenticated parser-authority value, and a parsed rights
    policy). They read no files, write no files, and — because a pure helper
    can always be called with fabricated inputs — confer NO release authority.

  - `verify-release-root!` is the IMPURE, fail-closed orchestrator. It reads
    the index from a completed candidate root, recomputes the closure, loads
    and hashes the authority envelopes, authenticates the candidate the index
    names, and calls `release-problems` exactly ONCE. Its fresh result is the
    only value the release-facing build may trust. It accepts paths only —
    never a precomputed problem vector, parsed authority value, hash, or
    verdict.

  Problem maps have exactly :code and :message plus optional :path, :expected,
  and :actual. Tests assert codes, not prose."
  (:require [abc.tools.files :as files]
            [abc.tools.parser-release-authority :as parser-release-authority]
            [abc.tools.publication-policy :as publication-policy]
            [abc.tools.snapshot-index :as snapshot-index]
            [babashka.fs :as fs]
            [clojure.java.io :as io]))

(def ^:private git-commit-pattern #"[0-9a-f]{40}")

(def parser-derived-manifest-kinds
  "Per-work publication manifest kinds that carry authenticated parser
  coordinates (the plaintext and TEI renderer outputs)."
  #{"plaintext" "tei"})

(defn- problem [code message extra]
  (merge {:code code :message message} extra))

;; ── Source trust (pure, index-only) ─────────────────────────────────────────

(defn source-problems
  "A release requires official-git source trust with a proven Aozora commit.
  Fixture mode and an unproven official checkout are both inadmissible."
  [index]
  (let [source (get index "source_selection_identity_object")
        trust (get source "trust_mode")
        commit (get source "aozora_git_commit")]
    (cond-> []
      (not= "official-git" trust)
      (conj (problem "release-source-not-official"
                     "Release requires official-git source trust"
                     {:expected "official-git" :actual trust}))

      (and (= "official-git" trust)
           (not (and (string? commit)
                     (re-matches git-commit-pattern commit))))
      (conj (problem "release-source-commit-missing"
                     "Official-git source requires a proven 40-hex Aozora commit"
                     {:actual commit})))))

;; ── Parser authority + runtime identity (pure) ──────────────────────────────

(defn- runtime-object [index] (get index "parser_runtime_identity_object"))
(defn- identity-object [index] (get index "snapshot_index_identity_object"))

(defn parser-null-coordinate-problems
  "The index must carry non-null authority references for a release. This keys
  on candidate_ref / qualification_identity_ref (null only for a diagnostic
  parser profile), NOT on parser_ir_schema_hash,
  which is non-null even for a non-release adapter and has no discriminating
  power."
  [index]
  (let [id (identity-object index)]
    (cond-> []
      (nil? (get id "candidate_ref"))
      (conj (problem "release-candidate-ref-null"
                     "Release requires a non-null candidate_ref" {}))

      (nil? (get id "qualification_identity_ref"))
      (conj (problem "release-qualification-ref-null"
                     "Release requires a non-null qualification_identity_ref" {})))))

(defn parser-config-hash-problems
  "parser_config_hash must equal SHA-256(JCS(parser_runtime_identity_object))."
  [index]
  (let [recomputed (snapshot-index/parser-config-hash (runtime-object index))
        declared (get (identity-object index) "parser_config_hash")]
    (when (not= recomputed declared)
      [(problem "release-parser-config-hash-mismatch"
                "parser_config_hash does not equal SHA-256(JCS(parser_runtime_identity_object))"
                {:expected recomputed :actual declared})])))

(defn- executable-hash [parser-authority name]
  (:sha256 (some #(when (= name (:name %)) %)
                 (:executables (:executable-provenance parser-authority)))))

(defn parser-authority-problems
  "Compare the index's parser runtime object and authority references against
  the freshly authenticated parser-release authority value.

  When authentication failed, parser-authority carries {:problems [...]} (or is
  nil); every such failure surfaces as one unauthenticated problem so the
  release is inadmissible rather than crashing."
  [index parser-authority]
  (if (or (nil? parser-authority) (contains? parser-authority :problems))
    (mapv (fn [p]
            (problem "release-parser-authority-unauthenticated"
                     (str "parser release authority did not authenticate: "
                          (or (:message p) (:kind p) (pr-str p)))
                     {}))
          (or (seq (:problems parser-authority))
              [{:message "no authenticated parser authority"}]))
    (let [runtime (runtime-object index)
          id (identity-object index)
          qualification (:qualification-identity parser-authority)
          candidate-ref (get id "candidate_ref")
          qualification-ref (get id "qualification_identity_ref")]
      (vec
       (keep
        identity
        [(when (and candidate-ref
                    (not= candidate-ref (:candidate-ref parser-authority)))
           (problem "release-candidate-ref-mismatch"
                    "candidate_ref disagrees with the authenticated authority"
                    {:expected (:candidate-ref parser-authority)
                     :actual candidate-ref}))
         (when (and qualification-ref
                    (not= qualification-ref
                          (:qualification-identity-ref parser-authority)))
           (problem "release-qualification-ref-mismatch"
                    "qualification_identity_ref disagrees with the authenticated authority"
                    {:expected (:qualification-identity-ref parser-authority)
                     :actual qualification-ref}))
         (when (not= (:aat_adapter qualification) (get runtime "adapter_id"))
           (problem "release-parser-adapter-mismatch"
                    "adapter_id disagrees with the authenticated qualification"
                    {:expected (:aat_adapter qualification)
                     :actual (get runtime "adapter_id")}))
         (when (not= (executable-hash parser-authority (get runtime "adapter_id"))
                     (get runtime "parser_build_hash"))
           (problem "release-parser-build-hash-mismatch"
                    "parser_build_hash disagrees with the authenticated provenance"
                    {:expected (executable-hash parser-authority
                                                (get runtime "adapter_id"))
                     :actual (get runtime "parser_build_hash")}))
         (when (not= (executable-hash parser-authority "ab-aat-to-parser-ir")
                     (get runtime "converter_build_hash"))
           (problem "release-converter-build-hash-mismatch"
                    "converter_build_hash disagrees with the authenticated provenance"
                    {:expected (executable-hash parser-authority "ab-aat-to-parser-ir")
                     :actual (get runtime "converter_build_hash")}))
         (when (not= (:mapping_hash qualification)
                     (get runtime "aat_parser_ir_mapping_hash"))
           (problem "release-mapping-hash-mismatch"
                    "aat_parser_ir_mapping_hash disagrees with the authenticated qualification"
                    {:expected (:mapping_hash qualification)
                     :actual (get runtime "aat_parser_ir_mapping_hash")}))
         (when (not= (:parser_ir_schema_hash qualification)
                     (get runtime "parser_ir_schema_hash"))
           (problem "release-parser-schema-hash-mismatch"
                    "parser_ir_schema_hash disagrees with the authenticated qualification"
                    {:expected (:parser_ir_schema_hash qualification)
                     :actual (get runtime "parser_ir_schema_hash")}))])))))

;; ── Per-work manifest parser coordinates (pure over loaded manifests) ───────

(defn expected-manifest-coordinates
  "The flat parser coordinates each parser-derived per-work manifest must carry,
  derived from the single runtime object and the index config hash."
  [index]
  (let [runtime (runtime-object index)
        id (identity-object index)]
    {"parser_build_hash" (get runtime "parser_build_hash")
     "parser_config_hash" (get id "parser_config_hash")
     "mapping_hash" (get runtime "aat_parser_ir_mapping_hash")
     "parser_ir_schema_hash" (get runtime "parser_ir_schema_hash")}))

(defn manifest-coordinate-problems
  "PURE. Given the index-derived `expected` coordinates and a seq of already
  loaded parser-derived manifests ({:work-slug :kind :identity-object}), reject
  any null coordinate on the discriminating triple (parser_build_hash /
  parser_config_hash / mapping_hash — NOT parser_ir_schema_hash, which is
  non-null even for a non-release adapter) and any coordinate that disagrees
  with the runtime object."
  [expected manifests]
  (let [triple ["parser_build_hash" "parser_config_hash" "mapping_hash"]
        all (conj triple "parser_ir_schema_hash")]
    (vec
     (mapcat
      (fn [{:keys [work-slug identity-object]}]
        (concat
         (for [k triple :when (nil? (get identity-object k))]
           (problem "release-manifest-parser-coordinate-null"
                    (str "Release manifest has a null parser coordinate " k)
                    {:path work-slug :actual k}))
         (for [k all
               :let [exp (get expected k)
                     act (get identity-object k)]
               :when (and (some? act) (not= exp act))]
           (problem "release-manifest-parser-coordinate-mismatch"
                    (str "Release manifest coordinate " k
                         " disagrees with the runtime object")
                    {:path work-slug :expected exp :actual act}))))
      manifests))))

;; ── Failures + rights (pure) ────────────────────────────────────────────────

(defn failure-problems
  "A release closes over a complete corpus: any recorded failure is inadmissible."
  [index]
  (when (seq (get index "failures"))
    [(problem "release-nonempty-failure-set"
              "Release requires an empty failure set"
              {:actual (count (get index "failures"))})]))

(defn rights-problems [rights-policy]
  (when-let [p (publication-policy/release-problem rights-policy)]
    [p]))

;; ── Pure projection ─────────────────────────────────────────────────────────

(defn- dedup-key [p]
  [(str (:code p)) (str (:path p)) (str (:expected p)) (str (:actual p))])

(defn release-problems
  "PURE. Concatenate the already-loaded closure problems with pure source,
  parser, failure, and rights comparisons over the loaded values, then
  sort and deduplicate by [:code :path :expected :actual]. Reads no files."
  [{:keys [index closure-problems parser-authority rights-policy]}]
  (->> (concat (vec (or closure-problems []))
               (source-problems index)
               (parser-null-coordinate-problems index)
               (parser-authority-problems index parser-authority)
               (parser-config-hash-problems index)
               (failure-problems index)
               (rights-problems rights-policy))
       (remove nil?)
       (sort-by dedup-key)
       (partition-by dedup-key)
       (map first)
       vec))

(defn release-admissible?
  "PURE. Exactly (empty? (release-problems loaded-closure)). Public for focused
  tests and projections; NOT an authorization boundary."
  [loaded-closure]
  (empty? (release-problems loaded-closure)))

;; ── Impure fail-closed orchestrator ─────────────────────────────────────────

(defn- authenticate-parser-authority
  "Authenticate the candidate the index names. Any authentication failure
  (unreadable/malformed decision or registry, missing slug, wrong authority,
  stale registry, wrong candidate) is captured as {:problems [...]} so the
  release is inadmissible rather than throwing; the authority hashes are then
  simply never reported."
  [{:keys [runs-root registry-path measurements-path qualification-report-path
           provenance-path decisions-path]}
   candidate-ref]
  (let [provenance (or provenance-path
                       (when candidate-ref
                         (str runs-root "/"
                              (subs candidate-ref (count "sha256:"))
                              "/executable-provenance.json")))]
    (try
      (parser-release-authority/authenticate
       {:runs_root runs-root
        :candidate_ref candidate-ref
        :registry_path registry-path
        :measurements_path measurements-path
        :report_path qualification-report-path
        :provenance_path provenance
        :decisions_path decisions-path})
      (catch clojure.lang.ExceptionInfo error
        {:problems (or (:problems (ex-data error))
                       [{:message (ex-message error)}])}))))

(defn- read-parser-derived-manifests
  "Read each parser-derived per-work manifest the index references, projecting
  its manifest_identity_object. Impure: this is the file-reading step the pure
  manifest-coordinate-problems helper consumes."
  [root index]
  (vec
   (for [reference (get index "artifact_references" [])
         :when (contains? parser-derived-manifest-kinds
                          (get reference "artifact_kind"))
         :let [path (get-in reference ["locator" "path"])
               file (io/file root path)]
         :when (and path (fs/regular-file? file))]
     {:work-slug (get reference "work_slug")
      :kind (get reference "artifact_kind")
      :identity-object (get (files/read-json file) "manifest_identity_object")})))

(defn verify-release-root!
  "IMPURE, fail-closed release boundary. Reads the index from the completed
  candidate root, recomputes the closure (including per-work parser coordinate
  agreement), authenticates the candidate the index names, loads the rights
  envelope, and calls the pure `release-problems` exactly once. Packages the
  problem vector, boolean verdict, and loader-derived authority hashes itself.
  Accepts paths only — no precomputed problems, authority values, hashes, or
  verdict."
  [{:keys [root parser-authority-sources rights-policy-path]}]
  (let [index (snapshot-index/read-valid-snapshot-index root)
        closure (snapshot-index/closure-problems root index)
        candidate-ref (get-in index ["snapshot_index_identity_object"
                                     "candidate_ref"])
        parser-authority (authenticate-parser-authority parser-authority-sources
                                                        candidate-ref)
        rights-envelope (publication-policy/load-rights-authority! rights-policy-path)
        manifest-coordinate
        (manifest-coordinate-problems
         (expected-manifest-coordinates index)
         (read-parser-derived-manifests root index))
        problems (release-problems
                  {:index index
                   :closure-problems (vec (concat closure manifest-coordinate))
                   :parser-authority parser-authority
                   :rights-policy (:policy rights-envelope)})]
    {:admissible? (empty? problems)
     :problems problems
     :authority-hashes
     {:decisions-file (get-in parser-authority [:authority-hashes :decisions-file])
      :registry-file (get-in parser-authority [:authority-hashes :registry-file])
      :rights-policy-file (:content-hash rights-envelope)}}))
