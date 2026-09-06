(ns soranoha.snh.fixture
  "Shared machinery for verifier/transaction tests: a throwaway origin +
  clone pair, the fixture signing keys, a parameterized corpus assembler,
  and a low-level crafter for deliberately invalid publication commits."
  (:require [babashka.fs :as fs]
            [charred.api :as json]
            [clojure.java.io :as io]
            [soranoha.core.canonical :as canonical]
            [soranoha.core.hash :as hash]
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

(defn work-blob-bytes [kind slug variant]
  (.getBytes (str "fixture:" kind ":" slug ":" variant) "UTF-8"))

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
  (let [plaintext ^bytes (work-blob-bytes "plaintext" slug variant)
        tei ^bytes (work-blob-bytes "tei" slug variant)
        validation (validation-blob-bytes (hash/sha256-bytes tei) failed?)
        entry-for (fn [kind ^bytes bytes]
                    {"type" kind
                     "id" (str "snh:1:" kind ":" (hash/sha256-bytes bytes))
                     "bytes" (alength bytes)})
        parts [["plaintext" plaintext] ["tei" tei] ["tei-validation" validation]]]
    {:blobs (into {} (map (fn [[_ ^bytes bytes]] [(hash/sha256-bytes bytes) bytes]))
                  parts)
     :entry {"slug" slug
             "source_content_hash" (hash/sha256-string (str "fixture:source:" slug))
             "artifacts" (mapv (fn [[kind bytes]] (entry-for kind bytes)) parts)}}))

(def policy-hash (hash/sha256-string "fixture:policy"))
(def rule-hash (hash/sha256-canonical-json admission/inclusion-rule))

(defn make-assemble
  "Assembler over a parameterized fixture corpus. `admitted`, `excluded`,
  `quarantined` are slug vectors; `selection-params` a string map; `variant`
  changes every work's artifact bytes (same projection, different derived
  content). The returned fn derives works = admitted minus the head's
  withdrawn set, as the transaction contract requires. `drop-candidate`
  (test hook) omits one slug from the snapshot to violate totality."
  [{:keys [admitted excluded quarantined selection-params variant drop-candidate
           invalid]
    :or {excluded [] quarantined [] variant "v1" invalid #{}
         selection-params {"config" "fixture"}}}]
  (fn [head-manifest]
    (let [withdrawn (set (map #(get % "slug") (get head-manifest "withdrawn")))
          live (vec (sort (remove withdrawn admitted)))
          works (mapv #(work-entry % variant (contains? (set invalid) %)) live)
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
          report-enc (decode/encode "admission-report" report)]
      {:core {"schema" "snh-manifest/1"
              "corpus" {"upstream_origin" "https://github.com/aozorabunko/aozorabunko.git"
                        "upstream_rev" "0e9ea3e586eb0aa34039fabfc85a407d2f98b165"}
              "toolchain" {"render" {"nix_closure_hash" (hash/sha256-string "fixture:render")
                                     "stage_code_version" "1"}}
              "selection_params" selection-params
              "admission" {"policy_id" "fixture-policy"
                           "policy_hash" policy-hash
                           "inclusion_rule_id" (get admission/inclusion-rule "id")
                           "inclusion_rule_hash" rule-hash
                           "assessment_snapshot" (:id snapshot-enc)
                           "admission_report" (:id report-enc)}
              "works" (mapv :entry works)
              "validation_summary" (let [failed (vec (filter (set invalid) live))]
                                     {"invalid_count" (count failed)
                                      "invalid_slugs" failed})}
       :blobs (into {(:hex snapshot-enc) (:bytes snapshot-enc)
                     (:hex report-enc) (:bytes report-enc)}
                    (map :blobs works))
       :selection (set all-candidates)})))

(defn publish!
  "Publish one build on `clone` with the fixture keys."
  [clone assemble-opts]
  (transact/publish-build! {:clone clone :branch branch
                            :pinned-keys (pinned-keys)
                            :assemble (make-assemble assemble-opts)
                            :sign-release sign-release}))

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

(defn craft-release!
  "Write (without pushing) a commit on `parents` carrying `manifest-value`
  as the release: manifest json + signature + advanced head + `extra-files`.
  `sign-fn` defaults to the release key. `raw` skips the boundary decode so
  semantically invalid manifests can be planted; the default round-trips
  through decode. Returns {:commit :hex}."
  [clone {:keys [parents base-tree-of manifest-value extra-files sign-fn raw]
          :or {sign-fn sign-release}}]
  (let [bytes (if raw
                (canonical/rfc8785-safe-integer-json-bytes-v1 manifest-value)
                (:bytes (decode/encode "release-manifest" manifest-value)))
        hex (hash/sha256-bytes bytes)
        files (merge {(verify/manifest-path hex) bytes
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
