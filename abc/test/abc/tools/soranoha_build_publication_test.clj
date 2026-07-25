(ns abc.tools.soranoha-build-publication-test
  "Adapter/profile resolution for soranoha-build-publication: each
  parser_profile resolves its own adapter binary AND its own aat→parser-IR
  mapping pin (the mapping selects the AAT schema version, so it is part of
  the adapter tuple, never a build-wide global).

  Also: the version 0.2.0 config contract and the explicit source-trust
  boundary (`source-provenance!`). Official-Git mode must prove clean relevant
  source state and fail closed otherwise; `fixture` mode is an explicit,
  recorded non-release value that never invokes Git."
  (:require [abc.tools.soranoha-build-publication :as build-publication]
            [babashka.fs :as fs]
            [babashka.process :as process]
            [charred.api :as charred]
            [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.test :refer [deftest is testing]])
  (:import [java.util.zip ZipEntry ZipOutputStream]))

(defn- env-stub [m]
  (fn [k] (get m k)))

(deftest resolve-adapter-ab-aozora-profile-test
  (binding [build-publication/*env*
            (env-stub {"AB_AOZORA_BIN" "/nix/store/x/bin/ab-aozora"
                       "AB_AAT_TO_PARSER_IR_MAPPING_V2"
                       "/nix/store/y/aat-to-parser-ir-mapping-v2.json"})]
    (let [adapter (#'build-publication/resolve-adapter "ab-aozora")]
      (testing "the native stdin→AAT binary is the whole adapter"
        (is (= "ab-aozora" (:adapter-id adapter)))
        (is (= "/nix/store/x/bin/ab-aozora" (:wrapper adapter)))
        (is (= {} (:extra-env adapter))))
      (testing "the profile pins the v2 mapping (AAT schema 2)"
        (is (= "/nix/store/y/aat-to-parser-ir-mapping-v2.json"
               (:mapping adapter)))))))

(deftest resolve-adapter-ab-aozora-requires-binary-test
  (binding [build-publication/*env*
            (env-stub {"AB_AAT_TO_PARSER_IR_MAPPING_V2" "/m/v2.json"})]
    (let [ex (try
               (#'build-publication/resolve-adapter "ab-aozora")
               nil
               (catch clojure.lang.ExceptionInfo ex ex))]
      (is (some? ex))
      (is (= "AB_AOZORA_BIN" (:env_var (ex-data ex)))))))

(deftest resolve-adapter-ab-aozora-requires-v2-mapping-test
  (binding [build-publication/*env*
            (env-stub {"AB_AOZORA_BIN" "/bin/ab-aozora"
                       ;; A build-wide v1 mapping must NOT satisfy the
                       ;; ab-aozora profile: the v1 mapping rejects AAT v2.
                       "AB_AAT_TO_PARSER_IR_MAPPING" "/m/v1.json"})]
    (let [ex (try
               (#'build-publication/resolve-adapter "ab-aozora")
               nil
               (catch clojure.lang.ExceptionInfo ex ex))]
      (is (some? ex))
      (is (= "AB_AAT_TO_PARSER_IR_MAPPING_V2" (:env_var (ex-data ex)))))))

(deftest resolve-adapter-aozora2html-pins-v1-mapping-test
  (binding [build-publication/*env*
            (env-stub {"AB_AOZORA2HTML_ADAPTER" "/a/wrapper"
                       "AB_AOZORA2HTML_BIN" "/a/aozora2html"
                       "AB_AOZORA2HTML_MAPPER_BIN" "/a/mapper"
                       "AB_AAT_TO_PARSER_IR_MAPPING" "/m/v1.json"})]
    (let [adapter (#'build-publication/resolve-adapter "aozora2html")]
      (is (= "aozora2html" (:adapter-id adapter)))
      (is (= "/a/wrapper" (:wrapper adapter)))
      (is (= {"AB_AOZORA2HTML_BIN" "/a/aozora2html"
              "AB_AOZORA2HTML_MAPPER_BIN" "/a/mapper"}
             (:extra-env adapter)))
      (is (= "/m/v1.json" (:mapping adapter))))))

(deftest resolve-adapter-unknown-profile-lists-both-supported-test
  (binding [build-publication/*env* (env-stub {})]
    (let [ex (try
               (#'build-publication/resolve-adapter "mystery-parser")
               nil
               (catch clojure.lang.ExceptionInfo ex ex))]
      (is (some? ex))
      (is (= "mystery-parser" (:parser_profile (ex-data ex))))
      (is (= ["aozora2html" "ab-aozora"] (:supported (ex-data ex)))))))

(deftest custom-parser-config-selects-ab-aozora-profile-test
  (let [config (#'build-publication/read-config
                "config/full-corpus-publication-custom-parser-ja.json")]
    (is (= "ab-aozora" (get config "parser_profile")))
    (is (= "full-corpus" (get config "materialization_scope")))
    (is (true? (get config "continue_on_failure")))))

;; --- config schema 0.2.0 -------------------------------------------------

(def ^:private record-path "data/release-parser-identity-v1.edn")

(deftest checked-in-configs-are-version-0-2-0-test
  (testing "the custom-parser config binds the release-parser-identity record, ab-aozora, official-git"
    (let [config (#'build-publication/read-config
                  "config/full-corpus-publication-custom-parser-ja.json")]
      (is (= "0.2.0" (get config "config_schema_version")))
      (is (= "official-git" (get config "source_trust_mode")))
      (is (= record-path (get config "release_parser_identity")))))
  (testing "the diagnostic aozora2html full-corpus config declares no release parser identity"
    (let [config (#'build-publication/read-config
                  "config/full-corpus-publication-basic-ja.json")]
      (is (= "0.2.0" (get config "config_schema_version")))
      (is (= "official-git" (get config "source_trust_mode")))
      (is (nil? (get config "release_parser_identity")))
      (is (= "aozora2html" (get config "parser_profile")))))
  (testing "the fixture diagnostic config is an explicit non-release source-trust value"
    (let [config (#'build-publication/read-config
                  "config/publication-basic-ja.json")]
      (is (= "0.2.0" (get config "config_schema_version")))
      (is (= "fixture" (get config "source_trust_mode")))
      (is (nil? (get config "release_parser_identity"))))))

(defn- read-config-error [config-value]
  (let [path (str (fs/file (fs/create-temp-dir {:prefix "bp-config-test"})
                           "config.json"))]
    (spit path (charred/write-json-str config-value))
    (try
      (#'build-publication/read-config path)
      nil
      (catch clojure.lang.ExceptionInfo error error))))

(def ^:private valid-0-2-0-config
  {"config_schema_id" "https://w3id.org/abc/schemas/soranoha-publication-build-config.schema.json"
   "config_schema_version" "0.2.0"
   "source_trust_mode" "fixture"
   "parser_profile" "aozora2html"
   "publication_profile" "tei-publication-basic-ja-v1"
   "continue_on_failure" true
   "materialization_scope" "smoke"})

(deftest config-schema-accepts-only-version-0-2-0-test
  (testing "a well-formed 0.2.0 config validates"
    (is (nil? (read-config-error valid-0-2-0-config))))
  (testing "the deprecated 0.1.0 shape (request_set_label/snapshot_scope) is rejected"
    (let [legacy {"config_schema_id" "https://w3id.org/abc/schemas/soranoha-publication-build-config.schema.json"
                  "request_set_label" "full-corpus-publication-basic-ja"
                  "snapshot_scope" "full-corpus"
                  "parser_profile" "aozora2html"
                  "publication_profile" "tei-profile-v0"
                  "continue_on_failure" true
                  "materialization_scope" "full-corpus"}]
      (is (some? (read-config-error legacy)))))
  (testing "a config missing config_schema_version is rejected"
    (is (some? (read-config-error (dissoc valid-0-2-0-config "config_schema_version")))))
  (testing "any config_schema_version other than 0.2.0 is rejected"
    (is (some? (read-config-error (assoc valid-0-2-0-config
                                         "config_schema_version" "0.1.0")))))
  (testing "an unknown source_trust_mode is rejected"
    (is (some? (read-config-error (assoc valid-0-2-0-config
                                         "source_trust_mode" "skip-check")))))
  (testing "a leftover deprecated key is rejected (additionalProperties false)"
    (is (some? (read-config-error (assoc valid-0-2-0-config
                                         "request_set_label" "x"))))))

;; --- source-trust boundary: source-provenance! ---------------------------

(defn- git! [dir & args]
  (let [{:keys [exit err]}
        (process/sh (into ["git" "-C" (str dir)] args)
                    {:extra-env {"GIT_AUTHOR_NAME" "t" "GIT_AUTHOR_EMAIL" "t@t"
                                 "GIT_COMMITTER_NAME" "t" "GIT_COMMITTER_EMAIL" "t@t"}})]
    (when-not (zero? exit)
      (throw (ex-info (str "git " (vec args) " failed") {:exit exit :err err})))))

(defn- init-official-repo! [dir]
  (fs/create-dirs (fs/file dir "cards"))
  (fs/create-dirs (fs/file dir "index_pages"))
  (spit (str (fs/file dir "cards" "seed.txt")) "seed\n")
  (spit (str (fs/file dir "index_pages" "seed.txt")) "seed\n")
  (git! dir "init" "-q")
  (git! dir "add" "-A")
  (git! dir "commit" "-q" "-m" "seed")
  dir)

(deftest source-provenance-official-git-proves-clean-relevant-state-test
  (fs/with-temp-dir [root {:prefix "bp-source-official"}]
    (init-official-repo! root)
    (let [provenance (#'build-publication/source-provenance!
                      {:source-trust-mode "official-git" :aozora-root (str root)})]
      (is (= "official-git" (get provenance "source_trust_mode")))
      (is (re-matches #"[0-9a-f]{40}" (get provenance "aozora_git_commit")))
      (is (false? (get provenance "aozora_git_dirty")))
      (is (true? (get provenance "release_source"))))))

(deftest source-provenance-official-git-rejects-dirty-relevant-paths-test
  (fs/with-temp-dir [root {:prefix "bp-source-dirty"}]
    (init-official-repo! root)
    ;; An untracked file under a relevant path (cards) makes status non-blank.
    (spit (str (fs/file root "cards" "unstaged.txt")) "dirty\n")
    (let [error (try (#'build-publication/source-provenance!
                      {:source-trust-mode "official-git" :aozora-root (str root)})
                     nil
                     (catch clojure.lang.ExceptionInfo e e))]
      (is (some? error))
      (is (= "source-git-dirty" (:code (ex-data error)))))))

(deftest source-provenance-official-git-unavailable-fails-closed-test
  (testing "a non-Git directory is unknown, never clean"
    (fs/with-temp-dir [root {:prefix "bp-source-nogit"}]
      (fs/create-dirs (fs/file root "cards"))
      (let [error (try (#'build-publication/source-provenance!
                        {:source-trust-mode "official-git" :aozora-root (str root)})
                       nil
                       (catch clojure.lang.ExceptionInfo e e))]
        (is (some? error))
        (is (= "source-git-unavailable" (:code (ex-data error))))))))

;; --- parser identity authentication: by binary + mapping + schema --------
;; The exact committed P5 qualification coordinates (see
;; parser_release_authority_test); the runtime identity object is compared
;; against them by BINARY (build hashes) + mapping + parser-IR schema. The
;; invocation argv (parser `--mode`, converter `convert` vs the provenance's
;; self-identify `qualify`) is bound into parser_config_hash as OUTPUT identity
;; but is deliberately NOT part of the authentication comparison.

;; The release-parser-identity record's real executable sha256 (Task 3): the
;; build's resolved runtime identity is authenticated by binary against these.
(def ^:private p5-parser-build-hash
  "sha256:7f75b8f94de9170bf913e6081790ee20e741526027003414034a97ce8a0451dd")
(def ^:private p5-converter-build-hash
  "sha256:8073c1dc520f2a829375d43473d61a3b9a3dc03bc26df60f0cdd3011a0d1b81f")
(def ^:private p5-mapping-hash
  "sha256:9be58ff3fea272c2a94ae16f05e3e362425e8bcdd20c482a4a842c13fe067142")
(def ^:private p5-parser-ir-schema-hash
  "sha256:43a6a6d86ca5eca062508e6cae633d19bf5248f15c5bb46153a6d8580ea916ec")

;; The runtime identity object is the CANONICAL parser_runtime_identity_object
;; (the exact shape the snapshot index and release verifier recompute), so the
;; build stamps ONE parser_config_hash into the index and every per-work
;; manifest. The argv templates are recorded (they bind parser_config_hash) but
;; the authentication comparison is by binary build hash + mapping + schema.
(defn- correct-ab-aozora-identity-object []
  {"adapter_id" "ab-aozora"
   "adapter_argv_template" ["{executable}" "--mode" "{mode}"]
   "converter_argv_template" ["{executable}" "convert"]
   "parser_build_hash" p5-parser-build-hash
   "converter_build_hash" p5-converter-build-hash
   "aat_parser_ir_mapping_hash" p5-mapping-hash
   "parser_ir_schema_hash" p5-parser-ir-schema-hash})

(deftest correct-ab-aozora-build-records-no-coordinate-problem-test
  (testing "matching binary+mapping+schema authenticate with no problem even though the converter argv is `convert`, not the provenance `qualify`"
    (let [result (#'build-publication/authenticate-runtime
                  "ab-aozora"
                  (correct-ab-aozora-identity-object)
                  record-path)]
      (is (= [] (:problems result)))
      (is (some? (:candidate-ref result)))
      (is (some? (:qualification-identity-ref result))))))

(deftest divergent-build-hash-or-mapping-records-coordinate-problem-test
  (let [mismatched (fn [k v]
                     (:problems
                      (#'build-publication/authenticate-runtime
                       "ab-aozora"
                       (assoc (correct-ab-aozora-identity-object) k v)
                       record-path)))
        wrong-hash "sha256:0000000000000000000000000000000000000000000000000000000000000000"]
    (testing "a divergent parser build hash is a coordinate mismatch"
      (let [problems (mismatched "parser_build_hash" wrong-hash)]
        (is (some #(and (= :parser-runtime-coordinate-mismatch (:kind %))
                        (= "parser_build_hash" (:coordinate %)))
                  problems))))
    (testing "a divergent converter build hash is a coordinate mismatch"
      (let [problems (mismatched "converter_build_hash" wrong-hash)]
        (is (some #(and (= :parser-runtime-coordinate-mismatch (:kind %))
                        (= "converter_build_hash" (:coordinate %)))
                  problems))))
    (testing "a divergent mapping hash is a coordinate mismatch"
      (let [problems (mismatched "aat_parser_ir_mapping_hash" wrong-hash)]
        (is (some #(and (= :parser-runtime-coordinate-mismatch (:kind %))
                        (= "aat_parser_ir_mapping_hash" (:coordinate %)))
                  problems))))
    (testing "a divergent parser-IR schema hash is a coordinate mismatch"
      (let [problems (mismatched "parser_ir_schema_hash" wrong-hash)]
        (is (some #(and (= :parser-runtime-coordinate-mismatch (:kind %))
                        (= "parser_ir_schema_hash" (:coordinate %)))
                  problems))))))

(deftest aozora2html-without-candidate-records-absent-candidate-problem-test
  (let [result (#'build-publication/authenticate-runtime
                "aozora2html" {"adapter_id" "aozora2html"} nil)]
    (is (= 1 (count (:problems result))))
    (is (= :absent-parser-candidate (:kind (first (:problems result)))))
    (is (nil? (:candidate-ref result)))))

(deftest source-provenance-fixture-records-null-commit-and-never-calls-git-test
  (fs/with-temp-dir [root {:prefix "bp-source-fixture"}]
    ;; No .git at all: fixture mode must not consult Git.
    (fs/create-dirs (fs/file root "cards"))
    (let [called (atom false)
          provenance (with-redefs [process/sh (fn [& _]
                                                (reset! called true)
                                                {:exit 0 :out "" :err ""})]
                       (#'build-publication/source-provenance!
                        {:source-trust-mode "fixture" :aozora-root (str root)}))]
      (is (false? @called) "fixture mode must never shell out to git")
      (is (= "fixture" (get provenance "source_trust_mode")))
      (is (nil? (get provenance "aozora_git_commit")))
      (is (false? (get provenance "release_source"))))))

;; ── Work-slug identity ──────────────────────────────────────────────────────

(deftest slug-is-injective-over-source-directories
  (testing "two sources differing only by card directory derive DISTINCT slugs"
    ;; The contributor card directory is the ONLY element distinguishing two
    ;; Aozora copies of one work_id, so it is part of publication identity.
    ;; Injectivity is unconditional: a source's slug is a function of its own
    ;; coordinates alone, never of what else the corpus contains.
    (let [slug-fn #'build-publication/slug
          a (slug-fn "047896" "000075" "cards/000075/files/47896_ruby_49619.zip")
          b (slug-fn "047896" "000075" "cards/001030/files/47896_ruby_49619.zip")]
      (is (not= a b))
      (is (= "047896_000075_000075_47896_ruby_49619" a))
      (is (= "047896_000075_001030_47896_ruby_49619" b)))))

(deftest slug-requires-a-card-path
  (testing "a relpath that names no card directory fails closed"
    (let [slug-fn #'build-publication/slug
          thrown (try (slug-fn "047896" "000075" "support/tools.zip") nil
                      (catch clojure.lang.ExceptionInfo e e))]
      (is (some? thrown))
      (is (= "unslugifiable-source-relpath" (:code (ex-data thrown)))))))

(defn- candidate-stub [work-id person-id relpath]
  {:row {"作品ID" work-id "人物ID" person-id} :relpath relpath :file nil})

;; The guard below is now a defence-in-depth invariant check rather than a
;; filter that fires in normal operation. Because the slug includes the card
;; directory, it is injective in `relpath` alone, and distinct sources have
;; distinct relpaths — so no *distinct* pair of candidates can collide. The
;; guard stays because it converts any future weakening of the formula into a
;; loud failure instead of a silently overwritten publication, and because a
;; duplicated candidate still must not produce two writes to one directory.

(deftest candidate-slug-collisions-detects-duplicate-claims
  (let [collisions #'build-publication/candidate-slug-collisions]
    (testing "injective candidates yield no collisions"
      (is (= [] (collisions [(candidate-stub "1" "9" "cards/000009/files/a.zip")
                             (candidate-stub "2" "9" "cards/000009/files/b.zip")]))))
    (testing "the same basename in two card dirs no longer collides"
      ;; Regression guard for the injectivity mechanism itself: this is the
      ;; exact shape that silently destroyed 7 publications before the card
      ;; directory entered the slug.
      (is (= [] (collisions
                 [(candidate-stub "047896" "000075" "cards/001030/files/47896_ruby_49619.zip")
                  (candidate-stub "047896" "000075" "cards/000075/files/47896_ruby_49619.zip")]))))
    (testing "a duplicated candidate is still a duplicate slug claim"
      (is (= [{"slug" "047896_000075_000075_47896_ruby_49619"
               "sources" [{"text_zip_relpath" "cards/000075/files/47896_ruby_49619.zip"}
                          {"text_zip_relpath" "cards/000075/files/47896_ruby_49619.zip"}]}]
             (collisions
              [(candidate-stub "047896" "000075" "cards/000075/files/47896_ruby_49619.zip")
               (candidate-stub "047896" "000075" "cards/000075/files/47896_ruby_49619.zip")]))))
    (testing "same basename under a different work id does NOT collide"
      (is (= [] (collisions
                 [(candidate-stub "047896" "000075" "cards/000075/files/x.zip")
                  (candidate-stub "047897" "000075" "cards/001030/files/x.zip")]))))))

(deftest assert-candidate-slugs-unique-fails-closed
  (let [assert-fn #'build-publication/assert-candidate-slugs-unique!
        duplicated (repeat 2 (candidate-stub "047896" "000075"
                                             "cards/000075/files/x.zip"))
        thrown (try (assert-fn (vec duplicated)) nil
                    (catch clojure.lang.ExceptionInfo e e))]
    (is (some? thrown))
    (is (= "publication-slug-collision" (:code (ex-data thrown))))
    (is (= 1 (count (:collisions (ex-data thrown)))))
    (testing "an injective candidate set passes through unchanged"
      (let [ok [(candidate-stub "1" "9" "cards/000009/files/a.zip")]]
        (is (= ok (assert-fn ok)))))
    (testing "two card copies of one work pass through unchanged"
      (let [ok [(candidate-stub "047896" "000075" "cards/000075/files/x.zip")
                (candidate-stub "047896" "000075" "cards/001030/files/x.zip")]]
        (is (= ok (assert-fn ok)))))))

(defn- write-zip!
  "Write a ZIP at `target` containing `entries` ({name -> content-string}).
  Deterministic entry order; no external `zip` binary required."
  [target entries]
  (fs/create-dirs (fs/parent target))
  (with-open [out (ZipOutputStream. (io/output-stream (fs/file target)))]
    (doseq [[entry-name content] (sort-by key entries)]
      (.putNextEntry out (ZipEntry. ^String entry-name))
      (.write out (.getBytes ^String content "UTF-8"))
      (.closeEntry out))))

(defn- two-card-colliding-aozora-root!
  "Build a minimal aozora root in which ONE catalog row matches the SAME zip
  basename in TWO card directories — the real collision shape. Catalog matching
  keys solely on the basename, so both copies match the single row and derive
  one slug. `second-card-content` differing from the first makes this Class A;
  passing identical content makes it Class B. Returns the aozora root path."
  [root basename second-card-content]
  (let [csv-source (fs/path "examples/v0/example-work/aozora-csv"
                            "list_person_all_extended_utf8_127.csv")
        csv-text (-> (slurp (fs/file csv-source))
                     (string/replace "127_ruby_150.zip" basename))]
    (write-zip! (fs/path root "index_pages" "list_person_all_extended_utf8.zip")
                {"list_person_all_extended_utf8.csv" csv-text})
    ;; Person 000879 is the catalog fixture's person; the SECOND card directory
    ;; is deliberately a different person dir, which the slug discards.
    (write-zip! (fs/path root "cards" "000879" "files" basename)
                {"000001.txt" "吾輩《わがはい》は猫である。\n"})
    (write-zip! (fs/path root "cards" "000880" "files" basename)
                {"000001.txt" second-card-content})
    root))

(defn- materialize-fixture! [aozora-root output-root]
  (let [materialize #'build-publication/materialize-selected-sources!]
    (materialize {:aozora-root (str aozora-root)
                  :output-root (str output-root)
                  :snapshot-date "2026-07-25"
                  :source-trust-mode "fixture"
                  :aozora-git-commit nil
                  :continue-on-failure true
                  :concurrency 2})))

(deftest two-card-copies-materialize-as-distinct-publications
  (testing "one work_id under two cards yields TWO publications, not one"
    ;; Before the card directory entered the slug this fixture produced a
    ;; single works/<slug> directory: the second write silently overwrote the
    ;; first. Both copies must now survive with distinct identities.
    (fs/with-temp-dir [tmp {:prefix "slug-identity-"}]
      (let [aozora-root (two-card-colliding-aozora-root!
                         (fs/path tmp "aozora")
                         "000001_ruby_fixture.zip"
                         "こちらは別の本文である。\n")
            output-root (fs/path tmp "out")
            result (materialize-fixture! aozora-root output-root)
            slugs (set (map :slug (:selected result)))
            works-dir (fs/path output-root "materialized-root" "works")]
        (is (= 2 (count (:selected result))) "both card copies are selected")
        (is (= [] (:derive-failures result)) "neither archive fails to derive")
        (is (= #{"000127_000879_000879_000001_ruby_fixture"
                 "000127_000879_000880_000001_ruby_fixture"}
               slugs)
            "slugs differ only by card directory")
        (testing "each slug owns its own output directory"
          (is (= 2 (count (fs/list-dir works-dir))))
          (doseq [work-slug slugs]
            (is (fs/exists? (fs/path works-dir work-slug))
                (str "missing works/" work-slug))))))))

(deftest unreadable-archive-fails-only-its-own-source
  (testing "a corrupt archive fails itself and does not affect the other copy"
    ;; The admission rule's post-governance form: identity is claimed from the
    ;; catalog before inspection, so a corrupt claimant neither blocks nor
    ;; renames its healthy sibling. Under continue_on_failure it becomes one
    ;; recorded derive failure, and exactly ONE publication is written.
    (fs/with-temp-dir [tmp {:prefix "slug-identity-corrupt-"}]
      (let [aozora-root (two-card-colliding-aozora-root!
                         (fs/path tmp "aozora")
                         "000001_ruby_fixture.zip"
                         "irrelevant — overwritten below\n")]
        (spit (fs/file (fs/path aozora-root "cards" "000880" "files"
                                "000001_ruby_fixture.zip"))
              "not a zip at all")
        (let [output-root (fs/path tmp "out")
              result (materialize-fixture! aozora-root output-root)
              works-dir (fs/path output-root "materialized-root" "works")]
          (is (= 1 (count (:selected result))) "only the healthy copy is selected")
          (is (= ["000127_000879_000879_000001_ruby_fixture"]
                 (mapv :slug (:selected result)))
              "the healthy copy keeps its own identity, unchanged by the failure")
          (is (= 1 (count (:derive-failures result)))
              "the corrupt copy is recorded as a derive failure")
          (is (= "cards/000880/files/000001_ruby_fixture.zip"
                 (get (first (:derive-failures result)) "text_zip_relpath"))
              "the failure names the corrupt source, not its sibling")
          (testing "exactly one publication directory exists"
            (is (= 1 (count (fs/list-dir works-dir))))))))))

(def ^:private known-2026-07-25-collisions
  "All seven duplicate slug claims from the 2026-07-25 full-corpus run on
  aozorabunko 0e9ea3e586eb0aa34039fabfc85a407d2f98b165. Class A = differing
  work_content_hash (visible to closure verification); Class B = identical
  (invisible to it, because sort-artifact-references applies distinct)."
  [{:class :b :work-id "045183" :person-id "000107" :basename "45183_ruby_23453.zip"
    :cards ["000019" "000107"]
    :hashes ["sha256:81b1b92c912d78fdb85dd46cd04a65cb8dbbad5b01e86b5cc63653f5e621c419"
             "sha256:81b1b92c912d78fdb85dd46cd04a65cb8dbbad5b01e86b5cc63653f5e621c419"]}
   {:class :a :work-id "047896" :person-id "000075" :basename "47896_ruby_49619.zip"
    :cards ["000075" "001030"]
    :hashes ["sha256:91ec677857fe17aa46afae0c0a95886d2d33a41f28b0a4c2359abdf6bdc161d7"
             "sha256:87cba36cfd6793da678e870c1a3c93ea888ac6a90185f1dac00be8887230ebd4"]}
   {:class :a :work-id "047957" :person-id "001030" :basename "47957_ruby_40644.zip"
    :cards ["001030" "001769"]
    :hashes ["sha256:478d4cbebbe7ac2069878d8773feec93a858a50408881ce0e33564fa44ff0955"
             "sha256:7ed77464f724f62cd76c7616530a64483ed350f1f02d3a34455b77a2d8b8f109"]}
   {:class :a :work-id "047959" :person-id "000075" :basename "47959_ruby_40639.zip"
    :cards ["000075" "001030"]
    :hashes ["sha256:0f331c885b929ed915e130052d0bbe169bca33d3bb24d2367be48b484adb1d90"
             "sha256:998a07914ac4f617772c82edb8c9fab0bc0da3297b768f05d9d6291752c16c84"]}
   {:class :a :work-id "047971" :person-id "000075" :basename "47971_txt_40650.zip"
    :cards ["000075" "001030"]
    :hashes ["sha256:9ba20d7e099f6256d5c5534224fbb3fc7407ee472283431d3b1f1f37139a8b1d"
             "sha256:eef28bf7e798e78802612f129cdb44107b0a7419960e3b85aa7cf953d418b0d9"]}
   {:class :b :work-id "050558" :person-id "000975" :basename "50558_ruby_61314.zip"
    :cards ["000150" "000975"]
    :hashes ["sha256:c16514dfb963b0d8c347ab1925d579287e5d4e0a3f21d43e987ae728c743be9e"
             "sha256:c16514dfb963b0d8c347ab1925d579287e5d4e0a3f21d43e987ae728c743be9e"]}
   {:class :b :work-id "062694" :person-id "002402" :basename "62694_ruby_78206.zip"
    :cards ["001085" "002385"]
    :hashes ["sha256:f8ae61ea7efc561ef02c5df483389e605e1ea7a53734821687e9c14a92d964ba"
             "sha256:f8ae61ea7efc561ef02c5df483389e605e1ea7a53734821687e9c14a92d964ba"]}])

(deftest all-seven-known-collisions-resolve-injectively
  (let [assert-fn #'build-publication/assert-candidate-slugs-unique!
        slug-fn #'build-publication/slug
        relpath (fn [card basename] (str "cards/" card "/files/" basename))]
    (testing "the table matches the observed corpus state"
      (is (= 7 (count known-2026-07-25-collisions)))
      (is (= 4 (count (filter #(= :a (:class %)) known-2026-07-25-collisions))))
      (is (= 3 (count (filter #(= :b (:class %)) known-2026-07-25-collisions))))
      (doseq [{:keys [class hashes]} known-2026-07-25-collisions]
        (is (= (= :b class) (apply = hashes))
            "Class B iff both work_content_hashes are equal")))
    (doseq [{:keys [work-id person-id basename cards class]} known-2026-07-25-collisions]
      (testing (str work-id " (class " (name class) ")")
        (let [candidates (mapv #(candidate-stub work-id person-id (relpath % basename))
                               cards)
              slugs (mapv #(slug-fn work-id person-id (relpath % basename)) cards)]
          (is (= 2 (count (set slugs)))
              (str work-id ": both card copies must derive distinct slugs"))
          (is (= candidates (assert-fn candidates))
              (str work-id ": an injective pair must pass the guard unchanged")))))
    (testing "all 14 coordinates across the table are globally distinct"
      ;; Per-entry distinctness is not enough: two different table entries must
      ;; not collide with each other either.
      (let [all-slugs (for [{:keys [work-id person-id basename cards]}
                            known-2026-07-25-collisions
                            card cards]
                        (slug-fn work-id person-id (relpath card basename)))]
        (is (= 14 (count all-slugs)))
        (is (= 14 (count (set all-slugs))))))))
