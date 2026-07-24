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
            [clojure.test :refer [deftest is testing]]))

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

(def ^:private p5-candidate-ref
  "sha256:15affdfb677cc6a94a4a5364da68ca2d11441f899737651e727dbac90eddc5ab")

(deftest checked-in-configs-are-version-0-2-0-test
  (testing "the custom-parser config carries the exact P5 candidate ref, ab-aozora, official-git"
    (let [config (#'build-publication/read-config
                  "config/full-corpus-publication-custom-parser-ja.json")]
      (is (= "0.2.0" (get config "config_schema_version")))
      (is (= "official-git" (get config "source_trust_mode")))
      (is (= p5-candidate-ref (get config "parser_candidate_ref")))))
  (testing "the diagnostic aozora2html full-corpus config carries a null candidate ref"
    (let [config (#'build-publication/read-config
                  "config/full-corpus-publication-basic-ja.json")]
      (is (= "0.2.0" (get config "config_schema_version")))
      (is (= "official-git" (get config "source_trust_mode")))
      (is (nil? (get config "parser_candidate_ref")))
      (is (= "aozora2html" (get config "parser_profile")))))
  (testing "the fixture diagnostic config is an explicit non-release source-trust value"
    (let [config (#'build-publication/read-config
                  "config/publication-basic-ja.json")]
      (is (= "0.2.0" (get config "config_schema_version")))
      (is (= "fixture" (get config "source_trust_mode")))
      (is (nil? (get config "parser_candidate_ref"))))))

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
   "parser_candidate_ref" nil
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

(def ^:private p5-parser-build-hash
  "sha256:482728cad5bc663c0742ca9e8c6d6fa7031c1a84117d024921cd48628e2eb034")
(def ^:private p5-converter-build-hash
  "sha256:a2656fc9404be9a8eb08e5ba16667db814c8a22ad45bdf684d78973befaf5936")
(def ^:private p5-mapping-hash
  "sha256:9be58ff3fea272c2a94ae16f05e3e362425e8bcdd20c482a4a842c13fe067142")
(def ^:private p5-parser-ir-schema-hash
  "sha256:43a6a6d86ca5eca062508e6cae633d19bf5248f15c5bb46153a6d8580ea916ec")

(defn- correct-ab-aozora-identity-object []
  {"adapter_id" "ab-aozora"
   ;; argv templates are recorded (they bind parser_config_hash) but the
   ;; converter here runs `convert`, not the provenance's `qualify`.
   "parser_argv_template" ["{executable}" "--mode" "{mode}"]
   "converter_argv_template" ["{executable}" "convert"]
   "parser_executable_hash" p5-parser-build-hash
   "converter_executable_hash" p5-converter-build-hash
   "mapping_hash" p5-mapping-hash
   "parser_ir_schema_hash" p5-parser-ir-schema-hash})

(deftest correct-ab-aozora-build-records-no-coordinate-problem-test
  (testing "matching binary+mapping+schema authenticate with no problem even though the converter argv is `convert`, not the provenance `qualify`"
    (let [problems (#'build-publication/authentication-problems
                    (correct-ab-aozora-identity-object)
                    p5-candidate-ref)]
      (is (= [] problems)))))

(deftest divergent-build-hash-or-mapping-records-coordinate-problem-test
  (let [mismatched (fn [k v]
                     (#'build-publication/authentication-problems
                      (assoc (correct-ab-aozora-identity-object) k v)
                      p5-candidate-ref))
        wrong-hash "sha256:0000000000000000000000000000000000000000000000000000000000000000"]
    (testing "a divergent parser build hash is a coordinate mismatch"
      (let [problems (mismatched "parser_executable_hash" wrong-hash)]
        (is (some #(and (= :parser-runtime-coordinate-mismatch (:kind %))
                        (= "parser_executable_hash" (:coordinate %)))
                  problems))))
    (testing "a divergent converter build hash is a coordinate mismatch"
      (let [problems (mismatched "converter_executable_hash" wrong-hash)]
        (is (some #(and (= :parser-runtime-coordinate-mismatch (:kind %))
                        (= "converter_executable_hash" (:coordinate %)))
                  problems))))
    (testing "a divergent mapping hash is a coordinate mismatch"
      (let [problems (mismatched "mapping_hash" wrong-hash)]
        (is (some #(and (= :parser-runtime-coordinate-mismatch (:kind %))
                        (= "mapping_hash" (:coordinate %)))
                  problems))))
    (testing "a divergent parser-IR schema hash is a coordinate mismatch"
      (let [problems (mismatched "parser_ir_schema_hash" wrong-hash)]
        (is (some #(and (= :parser-runtime-coordinate-mismatch (:kind %))
                        (= "parser_ir_schema_hash" (:coordinate %)))
                  problems))))))

(deftest aozora2html-without-candidate-records-absent-candidate-problem-test
  (let [problems (#'build-publication/parser-runtime-problems
                  "aozora2html" {"adapter_id" "aozora2html"} nil)]
    (is (= 1 (count problems)))
    (is (= :absent-parser-candidate (:kind (first problems))))))

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
