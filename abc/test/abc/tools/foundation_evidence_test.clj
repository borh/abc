(ns abc.tools.foundation-evidence-test
  (:require [abc.tools.files :as files]
            [abc.tools.materialize-import :as materialize]
            [abc.tools.schema :as schema]
            [babashka.fs :as fs]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]))

(def expected-generated-manifest-kinds #{:parser-ir :warnings})

(def expected-failure-identity-coordinates
  {"manifest_schema_hash" "sha256:2222222222222222222222222222222222222222222222222222222222222222"
   "corpus_snapshot_hash" "sha256:1111111111111111111111111111111111111111111111111111111111111111"
   "work_content_hash" "sha256:7777777777777777777777777777777777777777777777777777777777777777"
   "parser_build_hash" "sha256:4444444444444444444444444444444444444444444444444444444444444444"
   "parser_config_hash" "sha256:5555555555555555555555555555555555555555555555555555555555555555"
   "parser_ir_schema_hash" "sha256:6666666666666666666666666666666666666666666666666666666666666666"
   "output_format_spec_hash" "sha256:3333333333333333333333333333333333333333333333333333333333333333"})

(def ^:private turtle-prefix-paths
  ["examples/v0/example-work/failure-manifest.example.ttl"
   "examples/v0/example-work/lod/manifest.prov.ttl"
   "examples/v0/example-work/manifest.ttl"
   "examples/v0/example-work/metadata-record.ttl"
   "fixtures/v0/invalid/drift/merge-cardinality-one-predecessor/graph.ttl"
   "fixtures/v0/invalid/drift/rdf-participant-prov-mismatch/graph.ttl"
   "fixtures/v0/invalid/drift/shacl-missing-date/graph.ttl"
   "fixtures/v0/invalid/drift/split-cardinality-one-successor/graph.ttl"
   "fixtures/v0/invalid/drift/typing-missing-activity/graph.ttl"
   "fixtures/v0/invalid/drift/typing-missing-subclass/graph.ttl"
   "resources/abc/tools/manifest_to_rdf/example_manifest.ttl"
   "schemas/manifest.shacl.ttl"])

(def expected-design-bundle-command
  ["nix" "run"
   "--override-input" "local-pkgs"
   "path:${GITHUB_WORKSPACE}/nix/ci-empty-local-pkgs"
   ".#validate-design-bundle"])

(defn- generated-manifest-set-errors [generated]
  (when-not (= expected-generated-manifest-kinds (set (keys generated)))
    [{:expected expected-generated-manifest-kinds
      :actual (set (keys generated))}]))

(defn- workflow-error? [errors key]
  (boolean (some #(contains? % key) errors)))

(defn- indentation [line]
  (count (re-find #"^\s*" line)))

(defn- yaml-step-field [lines key]
  (let [field-name (name key)]
    (some (fn [line]
            (or (second (re-matches
                         (re-pattern (str "^      -\\s+" field-name
                                          ":\\s*(.*?)\\s*$"))
                         line))
                (second (re-matches
                         (re-pattern (str "^        " field-name
                                          ":\\s*(.*?)\\s*$"))
                         line))))
          lines)))

(defn- shell-token [token]
  (if (and (< 1 (count token))
           (#{\" \'} (first token))
           (= (first token) (last token)))
    (subs token 1 (dec (count token)))
    token))

(defn- run-command-tokens [run-lines]
  (->> run-lines
       (map str/trim)
       (remove str/blank?)
       (map #(str/replace % #"\\\s*$" ""))
       (str/join " ")
       (re-seq #"\"(?:\\.|[^\"])*\"|'(?:\\.|[^'])*'|[^\s]+")
       (mapv shell-token)))

(defn- workflow-step [lines]
  (let [run-lines (->> lines
                       (drop-while #(not (re-matches #"^        run:\s*\|\s*$" %)))
                       rest
                       (take-while #(or (str/blank? %) (< 8 (indentation %)))))
        command (run-command-tokens run-lines)]
    (cond-> {:name (yaml-step-field lines :name)
             :uses (yaml-step-field lines :uses)
             :working-directory (yaml-step-field lines :working-directory)}
      (seq command) (assoc :command command))))

(defn- job-default-working-directory [job-lines]
  (let [defaults-lines (->> job-lines
                            (drop-while #(not (re-matches #"^    defaults:\s*$" %)))
                            rest
                            (take-while #(or (str/blank? %) (< 4 (indentation %)))))
        run-defaults (->> defaults-lines
                          (drop-while #(not (re-matches #"^      run:\s*$" %)))
                          rest
                          (take-while #(or (str/blank? %) (< 6 (indentation %)))))]
    (some (fn [line]
            (second (re-matches #"^        working-directory:\s*(.*?)\s*$" line)))
          run-defaults)))

(defn- workflow-steps [job-lines]
  (let [starts (->> job-lines
                    (keep-indexed (fn [index line]
                                    (when (re-matches #"^      -\s+.*$" line)
                                      index)))
                    vec)]
    (mapv (fn [[start end]]
            (workflow-step (subvec job-lines start end)))
          (map vector starts (concat (rest starts) [(count job-lines)])))))

(defn- workflow-jobs [text]
  (let [lines (vec (str/split-lines text))
        jobs-index (first (keep-indexed (fn [index line]
                                          (when (= "jobs:" line) index))
                                        lines))
        job-lines (if jobs-index (subvec lines (inc jobs-index)) [])
        starts (->> job-lines
                    (keep-indexed
                     (fn [index line]
                       (when-let [[_ job] (re-matches #"^  ([A-Za-z0-9_-]+):\s*$" line)]
                         [index job])))
                    vec)]
    (mapv (fn [[[start job] end]]
            (let [body (subvec job-lines (inc start) end)]
              {:job job
               :default-working-directory (job-default-working-directory body)
               :steps (workflow-steps body)}))
          (map vector starts (concat (map first (rest starts)) [(count job-lines)])))))

(defn- design-bundle-workflow-errors [text]
  (let [jobs (workflow-jobs text)
        commands (for [{:keys [job steps default-working-directory]} jobs
                       [index step] (map-indexed vector steps)
                       :when (some #(= ".#validate-design-bundle" %) (:command step))]
                   {:job job
                    :index index
                    :step step
                    :steps steps
                    :effective-working-directory (or (:working-directory step)
                                                     default-working-directory)})
        command (first commands)
        preceding-steps (take (:index command 0) (:steps command))]
    (cond-> []
      (not= 1 (count commands))
      (conj {:expected-command-count 1 :actual-command-count (count commands)})

      (and command
           (not-any? #(str/starts-with? (or (:uses %) "") "actions/checkout@")
                     preceding-steps))
      (conj {:expected :preceding-checkout-in-command-job
             :actual-job (:job command)})

      (and command
           (not (contains? #{nil "."} (:effective-working-directory command))))
      (conj {:expected-working-directory :repository-root
             :actual-working-directory (:effective-working-directory command)})

      (and command
           (not= expected-design-bundle-command (get-in command [:step :command])))
      (conj {:expected-command expected-design-bundle-command
             :actual-command (get-in command [:step :command])}))))

(deftest broken-manifest-is-rejected-test
  (let [schema (files/read-json "schemas/manifest.schema.json")]
    (is (seq (schema/validation-errors schema {})))))

(deftest bounded-turtle-prefix-inventory-test
  (doseq [path turtle-prefix-paths]
    (is (str/includes? (files/read-text path)
                       "@prefix abc: <https://w3id.org/abc/> ."))))

(deftest committed-manifest-schema-conformance-test
  (let [manifest-schema (files/read-json "schemas/manifest.schema.json")]
    (is (nil? (schema/validation-errors
               manifest-schema
               (files/read-json "examples/v0/example-work/manifest.json"))))
    (is (nil? (schema/validation-errors
               manifest-schema
               (files/read-json "examples/v0/example-work/failure-manifest.example.json"))))))

(deftest failure-manifest-semantics-test
  (let [failure (files/read-json "examples/v0/example-work/failure-manifest.example.json")]
    (is (= "failure" (get failure "artifact_kind")))
    (is (= "failed" (get failure "validation_status")))
    (is (nil? (get failure "content")))
    (is (= [{"role" "errors"
             "hash" "sha256:9999999999999999999999999999999999999999999999999999999999999999"
             "media_type" "application/jsonl"
             "path_hint" "errors.jsonl"}]
           (get failure "sidecars")))))

(deftest failure-manifest-identity-coordinates-test
  (let [identity (get (files/read-json
                       "examples/v0/example-work/failure-manifest.example.json")
                      "manifest_identity_object")]
    (is (= expected-failure-identity-coordinates
           (select-keys identity (keys expected-failure-identity-coordinates))))
    (is (not= expected-failure-identity-coordinates
              (select-keys
               (assoc identity
                      "tokenizer_build_hash"
                      "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")
               (conj (vec (keys expected-failure-identity-coordinates))
                     "tokenizer_build_hash"))))))

(deftest failure-manifest-artifact-id-scope-test
  (let [failure (files/read-json "examples/v0/example-work/failure-manifest.example.json")]
    (is (contains? failure "artifact_id"))
    (is (not (contains? (get failure "manifest_identity_object")
                        "artifact_id")))))

(deftest generated-import-manifest-set-test
  (fs/with-temp-dir [output {:prefix "foundation-evidence-manifests-"}]
    (let [generated (materialize/materialize-import!
                     {:input-dir "examples/ab-validator-output"
                      :output-dir output
                      :generated-at materialize/default-generated-at})]
      (is (empty? (generated-manifest-set-errors generated)))
      (is (seq (generated-manifest-set-errors (dissoc generated :warnings)))))))

(deftest generated-import-manifest-schema-conformance-test
  (fs/with-temp-dir [output {:prefix "abc-test-"}]
    (let [manifest-schema (files/read-json "schemas/manifest.schema.json")
          generated (materialize/materialize-import!
                     {:input-dir "examples/ab-validator-output"
                      :output-dir output
                      :generated-at materialize/default-generated-at})]
      (is (nil? (schema/validation-errors
                 manifest-schema
                 (files/read-json (:parser-ir generated)))))
      (is (nil? (schema/validation-errors
                 manifest-schema
                 (files/read-json (:warnings generated))))))))

(deftest validate-design-bundle-wrapper-delegation-test
  (let [wrapper (files/read-text "bin/validate-design-bundle.sh")]
    (is (= ["#!/usr/bin/env bash"
            "set -euo pipefail"
            "cd \"$(dirname \"${BASH_SOURCE[0]}\")/..\""
            "exec clojure -M:abc/validate-design-bundle \"$@\""]
           (->> (str/split-lines wrapper) (remove str/blank?) vec)))))

(deftest validation-workflow-wiring-test
  (let [workflow (files/read-text ".github/workflows/validation.yml")
        wrong-defaults (str/replace workflow
                                    "    runs-on: ubuntu-latest\n"
                                    (str "    runs-on: ubuntu-latest\n"
                                         "    defaults:\n"
                                         "      run:\n"
                                         "        working-directory: abc\n"))
        root-step-override (str/replace wrong-defaults
                                        "      - name: Validate design bundle\n"
                                        (str "      - name: Validate design bundle\n"
                                             "        working-directory: .\n"))
        wrong-step-override (str/replace workflow
                                         "      - name: Validate design bundle\n"
                                         (str "      - name: Validate design bundle\n"
                                              "        working-directory: abc\n"))
        inline-cd (str/replace workflow "          nix run \\\n"
                               "          cd abc && nix run \\\n")
        chained-suffix (str/replace workflow
                                    "            .#validate-design-bundle\n"
                                    "            .#validate-design-bundle && echo accepted\n")
        reversed (str/replace workflow
                              (str "      - uses: actions/checkout@v7\n\n"
                                   "      - uses: cachix/install-nix-action@v31\n\n"
                                   "      - name: Validate design bundle")
                              "      - name: Validate design bundle")
        reversed (str reversed "\n      - uses: actions/checkout@v7\n")]
    (is (empty? (design-bundle-workflow-errors workflow)))
    (is (workflow-error? (design-bundle-workflow-errors wrong-defaults)
                         :expected-working-directory))
    (is (empty? (design-bundle-workflow-errors root-step-override)))
    (is (workflow-error? (design-bundle-workflow-errors wrong-step-override)
                         :expected-working-directory))
    (is (workflow-error? (design-bundle-workflow-errors inline-cd)
                         :expected-command))
    (is (workflow-error? (design-bundle-workflow-errors chained-suffix)
                         :expected-command))
    (is (some #(= :preceding-checkout-in-command-job (:expected %))
              (design-bundle-workflow-errors reversed)))
    (is (= [{:expected :preceding-checkout-in-command-job
             :actual-job "validate"}]
           (design-bundle-workflow-errors
            (str "jobs:\n"
                 "  checkout-only:\n"
                 "    steps:\n"
                 "      - uses: actions/checkout@v4\n"
                 "  validate:\n"
                 "    steps:\n"
                 "      - name: Validate design bundle\n"
                 "        run: |\n"
                 "          nix run \\\n"
                 "            --override-input local-pkgs \"path:${GITHUB_WORKSPACE}/nix/ci-empty-local-pkgs\" \\\n"
                 "            .#validate-design-bundle\n"))))))
