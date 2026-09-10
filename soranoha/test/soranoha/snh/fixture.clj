(ns soranoha.snh.fixture
  "Shared machinery for verifier/transaction tests: a throwaway origin +
  clone pair, the fixture signing keys, a parameterized corpus assembler,
  and a low-level crafter for invalid publication commits used in negative tests."
  (:require [babashka.fs :as fs]
            [charred.api :as json]
            [clojure.java.io :as io]
            [soranoha.core.canonical :as canonical]
            [soranoha.core.hash :as hash]
            [soranoha.ori.fixture :as ori-fixture]
            [soranoha.ori.render :as render]
            [soranoha.snh.decode :as decode]
            [soranoha.snh.admission :as admission]
            [soranoha.snh.repo :as repo]
            [soranoha.snh.sign :as sign]
            [soranoha.snh.transact :as transact]
            [soranoha.snh.verify :as verify]
            [soranoha.snh.view :as view]))

(def keys*
  (delay (get (json/read-json (slurp (io/resource "snh/vectors/signature-vectors.json")))
              "keys")))

(defn pinned-keys []
  {:release (get-in @keys* ["release" "pub"])
   :governance (get-in @keys* ["governance" "pub"])})

(defn- seed [role] (sign/hex->bytes (get-in @keys* [role "seed"])))

(defn sign-release [manifest-hex]
  (sign/sign (seed "release") (sign/manifest-message manifest-hex)))

(defn sign-event [event-hex]
  (sign/sign (seed "governance") (sign/event-message event-hex)))

(defn sign-event-with-release-key [event-hex]
  (sign/sign (seed "release") (sign/event-message event-hex)))

(def branch "main")

(defn make-repos!
  "Fresh origin + clone under a temp dir with the pre-genesis initial commit
  pushed. Returns {:dir :origin :clone :init-commit}."
  []
  (let [dir (str (fs/create-temp-dir {:prefix "snh-fixture"}))
        origin (repo/init-origin! (fs/path dir "origin.git"))
        clone (repo/clone! origin (fs/path dir "clone"))
        init (transact/init-publication-branch! clone branch)]
    {:dir dir :origin origin :clone clone :init-commit init}))

(defn second-clone! [{:keys [dir origin]}]
  (repo/clone! origin (fs/path dir "clone2")))

(defn- fact [basis]
  {"status" "public-domain" "jurisdiction" "jp"
   "effective_date" "2026-08-01" "basis" basis})

(defn- candidate [slug]
  {"slug" slug
   "work_assessment" (fact (str "edition:" slug))
   "contributions" [(assoc (fact (str "author:" slug)) "contribution_id"
                           (str "author:" slug))]})

(defn work-blob-bytes
  "Artifact bytes for one work in one release. The variant distinguishes
  releases, so the same slug published twice carries different bytes.

  The TEI artifact is real TEI, rendered by the publication path rather than
  spelled out here, because the serving layer reads it: an export renders
  every published work for its reading view, so a marker string would no
  longer be a stand-in for a TEI artifact."
  [kind slug variant]
  (if (= "tei" kind)
    (.getBytes ^String (:tei (render/render-work
                              {:rights @ori-fixture/grant
                               :parser-ir {"nodes" [{"type" "text"
                                                     "text" (str "fixture:" slug ":" variant)}]}
                               :metadata-record {"work" {"title" slug} "contributors" []}
                               :persons-by-id {}}))
               "UTF-8")
    (.getBytes (str "fixture:" kind ":" slug ":" variant) "UTF-8")))

(defn validation-blob-bytes
  "A representative tei-validation record carrying the consumed projection:
  status and validated_artifact naming the work's TEI bytes."
  ^bytes [tei-hex failed?]
  (.getBytes ^String (json/write-json-str
                      {"status" (if failed? "failed" "passed")
                       "validated_artifact" (str "sha256:" tei-hex)
                       "layers" {"relax_ng" "fixture"}})
             "UTF-8"))

(defn- work-entry [slug variant failed?]
  (let [markdown ^bytes (work-blob-bytes "markdown" slug variant)
        plaintext ^bytes (work-blob-bytes "plaintext" slug variant)
        tei ^bytes (work-blob-bytes "tei" slug variant)
        validation (validation-blob-bytes (hash/sha256-bytes tei) failed?)
        entry-for (fn [kind ^bytes bytes]
                    {"type" kind
                     "id" (str "snh:1:" kind ":" (hash/sha256-bytes bytes))
                     "bytes" (alength bytes)})
        ;; bytewise ascending by type, as the manifest schema pins positionally
        parts [["markdown" markdown] ["plaintext" plaintext]
               ["tei" tei] ["tei-validation" validation]]]
    {:blobs (into {} (map (fn [[_ ^bytes bytes]] [(hash/sha256-bytes bytes) bytes]))
                  parts)
     :entry {"slug" slug
             "source_content_hash" (hash/sha256-string (str "fixture:source:" slug))
             "rights" "public-domain"
             "artifacts" (mapv (fn [[kind bytes]] (entry-for kind bytes)) parts)
             "layers" []}}))

(def policy-hash (hash/sha256-string "fixture:policy"))

(def rights
  {"encoding" "CC0-1.0"
   "statement_url" "https://soranoha.example/rights"})

(defn catalog-for
  "The fixture catalog: one entry per manifest work, in the manifest's order
  and bound to the same source hashes, which is what the verifier checks."
  [entries]
  {"schema" "snh-catalog/1"
   "works" (mapv (fn [{:strs [slug source_content_hash]}]
                   {"slug" slug
                    "source_content_hash" source_content_hash
                    "title" (str "fixture:" slug)
                    "title_reading" nil
                    "subtitle" nil
                    "original_title" nil
                    "first_published" nil
                    "orthographic_style" "新字新仮名"
                    "ndc" nil
                    "card_url" (str "https://www.aozora.gr.jp/cards/000001/card"
                                    slug ".html")
                    "archive_stem" (str "fixture_" slug)
                    "contributors" [{"person_id" "000001"
                                     "family_name" "試験"
                                     "given_name" nil
                                     "family_name_romaji" "Shiken"
                                     "given_name_romaji" nil
                                     "relation_to_work" "著者"}]
                    "source_editions" []})
                 entries)})
(def rule-hash (hash/sha256-canonical-json admission/inclusion-rule))

(defn make-assemble
  "Assembler over a parameterized fixture corpus. `admitted`, `excluded`,
  `quarantined` are slug vectors; `selection-params` a string map; `variant`
  changes every work's artifact bytes (same projection, different derived
  content). The returned fn derives works = admitted minus the head's
  withdrawn set, as the transaction contract requires. `drop-candidate`
  (test hook) omits one slug from the snapshot to violate totality."
  [{:keys [admitted excluded quarantined selection-params variant drop-candidate
           invalid upstream-rev]
    :or {excluded [] quarantined [] variant "v1" invalid #{}
         selection-params {"config" "fixture"}
         upstream-rev "0e9ea3e586eb0aa34039fabfc85a407d2f98b165"}}]
  (fn [head-manifest]
    (let [withdrawn (set (map #(get % "slug") (get head-manifest "withdrawn")))
          live (vec (sort (remove withdrawn admitted)))
          ;; a scalar variant moves every work at once, which is what a
          ;; toolchain change does; a function of slug lets a caller move only
          ;; the few works an upstream commit actually touches
          variant-of (if (fn? variant) variant (constantly variant))
          works (mapv #(work-entry % (variant-of %) (contains? (set invalid) %)) live)
          all-candidates (vec (sort (concat admitted excluded quarantined)))
          snapshot {"schema" "snh-assessment-snapshot/2"
                    "candidates" (mapv (fn [slug]
                                         (let [status (cond (some #{slug} excluded) "in-copyright"
                                                            (some #{slug} quarantined) "undetermined"
                                                            :else "public-domain")]
                                           (update (candidate slug) "work_assessment" assoc "status" status)))
                                       (remove #{drop-candidate} all-candidates))}
          snapshot-enc (decode/encode "assessment-snapshot" snapshot)
          report {"schema" "snh-admission-report/1"
                  "assessment_snapshot" (:id snapshot-enc)
                  "policy_hash" policy-hash
                  "inclusion_rule_id" (get admission/inclusion-rule "id")
                  "inclusion_rule_hash" rule-hash
                  "admitted" (vec (sort admitted))
                  "excluded" (mapv (fn [slug] {"slug" slug "reason_code" "in-copyright"})
                                   (sort excluded))
                  "quarantined" (mapv (fn [slug] {"slug" slug "reason_code" "not-fully-evaluated"})
                                      (sort quarantined))}
          report-enc (decode/encode "admission-report" report)
          catalog-enc (decode/encode "catalog" (catalog-for (mapv :entry works)))]
      {:core {"schema" "snh-manifest/3"
              "corpus" {"upstream_origin" "https://github.com/aozorabunko/aozorabunko.git"
                        "upstream_rev" upstream-rev
                        ;; the producer side of the covers_from invariant,
                        ;; restated here rather than borrowed: this fixture is
                        ;; an independent implementation of the assembler
                        ;; contract, and one that imported the real assembler
                        ;; would stop being evidence about it
                        "covers_from"
                        (let [head-corpus (get head-manifest "corpus")]
                          (cond
                            (nil? head-manifest) nil
                            (= upstream-rev (get head-corpus "upstream_rev"))
                            (get head-corpus "covers_from")
                            :else (get head-corpus "upstream_rev")))}
              "toolchain" {"render" {"nix_closure_hash" (hash/sha256-string "fixture:render")
                                     "stage_code_version" "1"}}
              "selection_params" selection-params
              "admission" {"policy_id" "fixture-policy"
                           "policy_hash" policy-hash
                           "inclusion_rule_id" (get admission/inclusion-rule "id")
                           "inclusion_rule_hash" rule-hash
                           "assessment_snapshot" (:id snapshot-enc)
                           "admission_report" (:id report-enc)}
              "catalog" (:id catalog-enc)
              "rights" rights
              "works" (mapv :entry works)
              "validation_summary" (let [failed (vec (filter (set invalid) live))]
                                     {"invalid_count" (count failed)
                                      "invalid_slugs" failed})}
       :blobs (into {(:hex snapshot-enc) (:bytes snapshot-enc)
                     (:hex report-enc) (:bytes report-enc)
                     (:hex catalog-enc) (:bytes catalog-enc)}
                    (map :blobs works))
       :selection (set all-candidates)})))

(defn publish!
  "Publish one build on `clone` with the fixture keys. `verified-head`, when
  given, is the proof the caller carried from its previous publication."
  ([clone assemble-opts] (publish! clone assemble-opts nil))
  ([clone assemble-opts verified-head]
   (transact/publish-build! (cond-> {:clone clone :branch branch
                                     :pinned-keys (pinned-keys)
                                     :assemble (make-assemble assemble-opts)
                                     :sign-release sign-release}
                              verified-head (assoc :verified-head verified-head)))))

(defn event-value [kind entries]
  {"schema" "snh-governance-event/1" "kind" kind "entries" entries})

(defn publish-event!
  [clone value]
  (let [{:keys [hex bytes]} (decode/encode "governance-event" value)]
    (transact/publish-governance! {:clone clone :branch branch
                                   :pinned-keys (pinned-keys)
                                   :sign-release sign-release
                                   :event-bytes bytes
                                   :event-sig (sign-event hex)})))

(defn head-of
  "Current origin head sha as seen from `clone` (after fetch)."
  [clone]
  (repo/fetch! clone branch))

(defn- catalog-rebinding
  "A catalog matching `manifest-value`'s works, plus its blob file. Crafted
  releases get one by default: the manifest-to-catalog binding is checked on
  every commit, so a crafted manifest without a matching catalog would fail
  there and mask the rule the test is actually about, exactly as a real
  forger would have to supply one. Returns nil when the planted works cannot
  produce a valid catalog (duplicate or unsorted slugs), leaving the
  manifest's own catalog reference in place."
  [manifest-value]
  (try
    (let [{:keys [id hex bytes]}
          (decode/encode "catalog" (catalog-for (get manifest-value "works")))]
      {:id id :files {(verify/blob-path hex) bytes}})
    (catch clojure.lang.ExceptionInfo _ nil)))

(defn craft-release!
  "Write (without pushing) a commit on `parents` carrying `manifest-value`
  as the release: manifest json + signature + advanced head + `extra-files`.
  `sign-fn` defaults to the release key. `raw` skips the boundary decode so
  semantically invalid manifests can be planted; the default round-trips
  through decode. The catalog is rebound to the planted works unless
  `keep-catalog` is set (set it to exercise the binding rule itself).
  Returns {:commit :hex}."
  [clone {:keys [parents base-tree-of manifest-value extra-files sign-fn raw
                 keep-catalog]
          :or {sign-fn sign-release}}]
  (let [rebinding (when-not keep-catalog (catalog-rebinding manifest-value))
        manifest-value (cond-> manifest-value
                         rebinding (assoc "catalog" (:id rebinding)))
        bytes (if raw
                (canonical/rfc8785-safe-integer-json-bytes-v1 manifest-value)
                (:bytes (decode/encode "release-manifest" manifest-value)))
        hex (hash/sha256-bytes bytes)
        files (merge (:files rebinding)
                     {(verify/manifest-path hex) bytes
                      (verify/manifest-sig-path hex) (sign-fn hex)
                      verify/head-path (sign/hex64-lf-bytes hex)}
                     extra-files)]
    {:commit (repo/write-commit! clone {:parents parents
                                        :base-tree-of base-tree-of
                                        :files files
                                        :message (str "crafted " hex)})
     :hex hex}))

(defn raw-event
  "Canonical bytes, id, signature, and repo files for an event value that
  may violate single-object semantics (planted directly, bypassing decode)."
  [value]
  (let [bytes (canonical/rfc8785-safe-integer-json-bytes-v1 value)
        hex (hash/sha256-bytes bytes)]
    {:id (str "snh:1:governance-event:" hex)
     :hex hex
     :bytes bytes
     :files {(verify/blob-path hex) bytes
             (verify/event-path hex) bytes
             (verify/event-sig-path hex) (sign-event hex)}}))

(defn manifest-at
  "Decoded manifest value + hex at `commit` in `clone`."
  [clone commit]
  (let [v (view/git-view clone)
        head (sign/parse-hex64-lf (view/read-at v commit verify/head-path))]
    {:hex head
     :value (:value (decode/decode "release-manifest"
                                   (view/read-at v commit
                                                 (verify/manifest-path head))))}))
