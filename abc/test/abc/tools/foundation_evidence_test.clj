(ns abc.tools.foundation-evidence-test
  (:require [abc.tools.files :as files]
            [abc.tools.materialize-import :as materialize]
            [abc.tools.schema :as schema]
            [babashka.fs :as fs]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]))

(def expected-generated-manifest-kinds #{:parser-ir :warnings})

(def expected-failure-identity-coordinates
  {"manifest_schema_hash" "sha256:2222222222222222222222222222222222222222222222222222222222222222"
   "corpus_snapshot_hash" "sha256:1111111111111111111111111111111111111111111111111111111111111111"
   "work_content_hash" "sha256:7777777777777777777777777777777777777777777777777777777777777777"
   "parser_build_hash" "sha256:4444444444444444444444444444444444444444444444444444444444444444"
   "parser_config_hash" "sha256:5555555555555555555555555555555555555555555555555555555555555555"
   "parser_ir_schema_hash" "sha256:6666666666666666666666666666666666666666666666666666666666666666"
   "output_format_spec_hash" "sha256:3333333333333333333333333333333333333333333333333333333333333333"})

(defn- generated-manifest-set-errors [generated]
  (when-not (= expected-generated-manifest-kinds (set (keys generated)))
    [{:expected expected-generated-manifest-kinds
      :actual (set (keys generated))}]))

(defn- failure-coordinate-errors [identity]
  (let [non-null (into {} (remove (comp nil? val)) identity)
        other-coordinates (apply dissoc identity (keys expected-failure-identity-coordinates))]
    (cond-> []
      (not= expected-failure-identity-coordinates non-null)
      (conj {:expected-non-null expected-failure-identity-coordinates
             :actual-non-null non-null})

      (not-every? nil? (vals other-coordinates))
      (conj {:expected-other-coordinates :nil
             :actual-other-coordinates other-coordinates}))))

(defn- indentation [line]
  (count (re-find #"^\s*" line)))

(defn- workflow-step [lines]
  (let [field (fn [key]
                (some (fn [line]
                        (second (re-matches
                                 (re-pattern (str "^\\s*(?:-\\s+)?"
                                                  (name key)
                                                  ":\\s*(.*?)\\s*$"))
                                 line)))
                      lines))
        run-lines (->> lines
                       (drop-while #(not (re-matches #"^\s*run:\s*\|\s*$" %)))
                       rest
                       (take-while #(or (str/blank? %) (< 8 (indentation %))))
                       (map str/trim)
                       (remove str/blank?))]
    (cond-> {:name (field :name)
             :uses (field :uses)
             :working-directory (field :working-directory)}
      (seq run-lines) (assoc :run (str/join " " run-lines)))))

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
            {:job job
             :steps (workflow-steps (subvec job-lines (inc start) end))})
          (map vector starts (concat (map first (rest starts)) [(count job-lines)])))))

(defn- design-bundle-workflow-errors [text]
  (let [jobs (workflow-jobs text)
        commands (for [{:keys [job steps]} jobs
                       [index step] (map-indexed vector steps)
                       :when (str/includes? (or (:run step) "")
                                            ".#validate-design-bundle")]
                   {:job job :index index :step step :steps steps})
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

      (and command (some? (get-in command [:step :working-directory])))
      (conj {:expected-working-directory :repository-root
             :actual-working-directory (get-in command [:step :working-directory])})

      (and command
           (not (str/includes?
                 (get-in command [:step :run])
                 "--override-input local-pkgs \"path:${GITHUB_WORKSPACE}/nix/ci-empty-local-pkgs\"")))
      (conj {:expected :repository-root-local-pkgs-override})

      (and command
           (not (str/ends-with? (get-in command [:step :run])
                                ".#validate-design-bundle")))
      (conj {:expected :design-bundle-command-at-run-end}))))

(defn- artifact-id-paths [value]
  (letfn [(walk [path item]
            (cond
              (map? item) (mapcat (fn [[key child]]
                                    (let [child-path (conj path key)]
                                      (concat (when (= "artifact_id" key) [child-path])
                                              (walk child-path child))))
                                  item)
              (sequential? item) (mapcat (fn [[index child]]
                                           (walk (conj path index) child))
                                         (map-indexed vector item))
              :else []))]
    (vec (walk [] value))))

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
                      "manifest_identity_object")
        non-null (into {} (remove (comp nil? val)) identity)
        other-coordinates (apply dissoc identity (keys expected-failure-identity-coordinates))]
    (is (= expected-failure-identity-coordinates non-null))
    (is (every? nil? (vals other-coordinates)))
    (is (seq (failure-coordinate-errors
              (assoc identity
                     "tokenizer_build_hash"
                     "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"))))))

(deftest failure-manifest-artifact-id-scope-test
  (let [failure (files/read-json "examples/v0/example-work/failure-manifest.example.json")]
    (is (= [["artifact_id"]] (artifact-id-paths failure)))))

(deftest generated-import-manifest-set-test
  (fs/with-temp-dir [output {:prefix "foundation-evidence-manifests-"}]
    (let [generated (materialize/materialize-import!
                     {:input-dir "examples/ab-validator-output"
                      :output-dir output
                      :generated-at materialize/default-generated-at})]
      (is (= expected-generated-manifest-kinds (set (keys generated))))
      (is (seq (generated-manifest-set-errors (dissoc generated :warnings)))))))

(deftest generated-import-manifest-schema-conformance-test
  (fs/with-temp-dir [output {:prefix "foundation-evidence-manifests-"}]
    (let [manifest-schema (files/read-json "schemas/manifest.schema.json")
          generated (materialize/materialize-import!
                     {:input-dir "examples/ab-validator-output"
                      :output-dir output
                      :generated-at materialize/default-generated-at})]
      (testing "parser IR manifest"
        (is (nil? (schema/validation-errors
                   manifest-schema
                   (files/read-json (:parser-ir generated))))))
      (testing "warnings manifest"
        (is (nil? (schema/validation-errors
                   manifest-schema
                   (files/read-json (:warnings generated)))))))))

(deftest validate-design-bundle-wrapper-delegation-test
  (let [wrapper (files/read-text "bin/validate-design-bundle.sh")]
    (is (= ["#!/usr/bin/env bash"
            "set -euo pipefail"
            "cd \"$(dirname \"${BASH_SOURCE[0]}\")/..\""
            "exec clojure -M:abc/validate-design-bundle \"$@\""]
           (->> (str/split-lines wrapper) (remove str/blank?) vec)))))

(deftest validation-workflow-wiring-test
  (let [workflow (files/read-text ".github/workflows/validation.yml")]
    (is (empty? (design-bundle-workflow-errors workflow)))
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
