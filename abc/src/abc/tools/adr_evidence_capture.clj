(ns abc.tools.adr-evidence-capture
  (:require [abc.tools.adr-evidence-bundle :as bundle]
            [abc.tools.adr-evidence-runtime-inputs :as runtime-inputs]
            [abc.tools.cli :as abc-cli]
            [abc.tools.files :as files]
            [abc.tools.hash :as hash]
            [abc.tools.json :as json]
            [abc.tools.path-containment :as containment]
            [babashka.fs :as fs]
            [babashka.process :as process]
            [clojure.string :as str]))

(def ^:private descriptor-v1-keys
  #{:schema-version :tool :argv :input-profile :observation-key})
(def ^:private descriptor-v2-keys
  (conj descriptor-v1-keys :runtime-input-manifest))

(defn- run-process [repo-root argv]
  (let [{:keys [exit out err]}
        @(process/process argv
                          {:dir (str repo-root) :out :string :err :string})]
    {:exit-code exit :stdout out :stderr err}))

(defn- git-output [repo-root & args]
  (let [result (run-process repo-root (into ["git"] args))]
    (when-not (zero? (:exit-code result))
      (throw (ex-info "git command failed during evidence capture" result)))
    (str/trim (:stdout result))))

(defn- require-clean! [repo-root phase]
  (when (seq (git-output repo-root "status" "--porcelain" "--untracked-files=all"))
    (throw (ex-info "evidence capture requires a clean Git worktree"
                    {:phase phase :exit-code 2}))))

(defn- normalized-component-root [profile]
  (-> (:component-root profile) fs/path fs/normalize str (str/replace "\\" "/")))

(defn- expected-runner [profile]
  (if (= "component-clojure-test-v1" (:kind profile))
    (-> (fs/path (normalized-component-root profile) "bin/kaocha")
        fs/normalize
        str
        (str/replace "\\" "/"))
    "bin/kaocha"))

(defn- focus-vars! [descriptor]
  (let [profile (:input-profile descriptor)
        runner (expected-runner profile)
        argv (:argv descriptor)
        focus-args (when (vector? argv) (subvec argv (min 1 (count argv))))
        pairs (when (and (seq focus-args) (even? (count focus-args)))
                (partition 2 focus-args))
        focuses (when (and (= runner (:tool descriptor))
                           (= runner (first argv))
                           (contains? (set (:explicit profile)) runner)
                           pairs
                           (every? (fn [[flag value]]
                                     (and (= "--focus" flag)
                                          (string? value)
                                          (qualified-symbol? (symbol value))))
                                   pairs)
                           (= (count pairs) (count (distinct pairs))))
                  (mapv (comp symbol second) pairs))]
    (when-not (seq focuses)
      (throw (ex-info "version-2 capture requires a bound repository Kaocha runner and exact focuses"
                      {:exit-code 2 :kind :invalid-nix-clojure-closure})))
    focuses))

(defn- validate-runner! [repo-root descriptor]
  (let [runner (expected-runner (:input-profile descriptor))
        state (containment/path-state repo-root runner)]
    (when-not (and (= :ok (:state state))
                   (fs/regular-file? (:path state))
                   (fs/executable? (:path state)))
      (throw (ex-info "version-2 Kaocha runner must be a contained executable file"
                      {:exit-code 2 :kind :invalid-nix-clojure-closure
                       :runner runner :state (:state state)})))
    runner))

(def ^:private kaocha-summary-pattern
  #"(\d+) tests?, (\d+) assertions?(?:, (\d+) errors?)?, (\d+) failures?\.")

(defn- validate-v2-command-result! [focuses command-result]
  (let [summaries (re-seq kaocha-summary-pattern (:stdout command-result))
        test-count (some-> summaries first second parse-long)
        expected (count (distinct focuses))]
    (when-not (and (= 1 (count summaries))
                   (pos? expected)
                   (= expected test-count))
      (throw (ex-info "Kaocha did not execute each focused evidence test exactly once"
                      {:exit-code 2 :kind :invalid-nix-clojure-closure
                       :expected-tests expected :actual-tests test-count})))
    command-result))

(defn- validate-descriptor! [descriptor descriptor-path]
  (let [version (:schema-version descriptor)
        profile (:input-profile descriptor)
        clojure-profile? (contains? #{"clojure-test-v1" "component-clojure-test-v1"}
                                    (:kind profile))
        profile-kind (:kind profile)
        expected-profile-keys (case profile-kind
                                "component-clojure-test-v1"
                                #{:kind :component-root :roots :explicit}
                                ("clojure-test-v1" "repo-files-v1")
                                #{:kind :roots :explicit}
                                nil)
        expected-keys (case version
                        "abc-adr-evidence-capture-v1" descriptor-v1-keys
                        "abc-adr-evidence-capture-v2" descriptor-v2-keys
                        nil)]
    (when-not expected-keys
      (throw (ex-info "capture descriptor schema version is unsupported"
                      {:exit-code 2})))
    (when-not (= expected-keys (set (keys descriptor)))
      (throw (ex-info "capture descriptor has an invalid versioned key set"
                      {:exit-code 2 :keys (keys descriptor)})))
    (when-not (and expected-profile-keys
                   (= expected-profile-keys (set (keys profile)))
                   (vector? (:roots profile))
                   (every? #(and (string? %) (seq %)) (:roots profile))
                   (= (count (:roots profile)) (count (distinct (:roots profile))))
                   (vector? (:explicit profile))
                   (every? #(and (string? %) (seq %)) (:explicit profile))
                   (= (count (:explicit profile)) (count (distinct (:explicit profile))))
                   (or (not= "component-clojure-test-v1" profile-kind)
                       (and (string? (:component-root profile))
                            (seq (:component-root profile)))))
      (throw (ex-info "capture descriptor input profile is not closed"
                      {:exit-code 2 :kind :invalid-evidence-artifact})))
    (when (and (= version "abc-adr-evidence-capture-v2") (not clojure-profile?))
      (throw (ex-info "version 2 is restricted to Clojure input profiles" {:exit-code 2})))
    (when (= version "abc-adr-evidence-capture-v2")
      (let [manifest (:runtime-input-manifest descriptor)
            explicit (set (:explicit profile))
            descriptor-name (some-> descriptor-path fs/file-name str)
            manifest-name (some-> manifest fs/file-name str)
            manifest-prefix (if (= "component-clojure-test-v1" (:kind profile))
                              (str (normalized-component-root profile)
                                   "/docs/evidence/adr-inputs/")
                              "docs/evidence/adr-inputs/")]
        (when-not (and (string? manifest)
                       (str/starts-with? manifest manifest-prefix)
                       (contains? explicit manifest)
                       descriptor-name
                       (= descriptor-name manifest-name))
          (throw (ex-info "version-2 runtime manifest must be a bound ADR input"
                          {:exit-code 2 :kind :invalid-runtime-input-manifest}))))
      (focus-vars! descriptor)))
  (when-not (and (string? (:tool descriptor))
                 (seq (:tool descriptor))
                 (vector? (:argv descriptor))
                 (every? string? (:argv descriptor))
                 (seq (:argv descriptor))
                 (string? (:observation-key descriptor)))
    (throw (ex-info "capture descriptor fields are invalid" {:exit-code 2})))
  descriptor)

(defn- input-bindings [repo-root profile extra-paths]
  (into (sorted-map)
        (map (fn [path]
               [path (hash/format-sha256
                      (hash/sha256-file (fs/file repo-root path)))]))
        (into (bundle/derive-minimum-inputs repo-root profile) extra-paths)))

(defn- contained-relative-path! [repo-root path]
  (let [file (fs/file (fs/canonicalize path))
        root (fs/file (fs/canonicalize repo-root))
        relative (if (fs/absolute? (fs/path path))
                   (-> (fs/relativize root file) str (str/replace "\\" "/"))
                   path)
        state (containment/path-state root relative)]
    (when-not (= :ok (:state state))
      (throw (ex-info "descriptor path must be a contained repository file"
                      {:exit-code 2 :state (:state state)})))
    relative))

(defn capture! [{:keys [repo-root descriptor descriptor-path output]}]
  (validate-descriptor! descriptor descriptor-path)
  (when (= "abc-adr-evidence-capture-v2" (:schema-version descriptor))
    (validate-runner! repo-root descriptor))
  (let [component? (= "component-clojure-test-v1" (get-in descriptor [:input-profile :kind]))
        descriptor-path (when descriptor-path (contained-relative-path! repo-root descriptor-path))
        component-root (when component?
                         (normalized-component-root (:input-profile descriptor)))
        analysis (when (= "abc-adr-evidence-capture-v2" (:schema-version descriptor))
                   (let [manifest-options {:repo-root repo-root
                                           :workspace-root (when component? repo-root)
                                           :descriptor {:path descriptor-path :value descriptor}}
                         _ (runtime-inputs/validate-runtime-input-manifest! manifest-options)
                         analysis-root (if component? (fs/file repo-root component-root) repo-root)]
                     (runtime-inputs/validate-focused-deftests!
                      analysis-root (focus-vars! descriptor))
                     (-> (runtime-inputs/analyze-reachable-vars
                          analysis-root (focus-vars! descriptor))
                         runtime-inputs/assert-v2-boundary-ownership!)))
        prefix (if component? (str (str/replace component-root #"/+$" "") "/") "")
        analyzed-paths (when analysis
                         (map #(str prefix %) (concat (:paths analysis) (:contract-paths analysis))))]
    (require-clean! repo-root :before-command)
    (let [command-result (run-process repo-root (:argv descriptor))]
      (require-clean! repo-root :after-command)
      (let [command-result (if (= "abc-adr-evidence-capture-v2"
                                  (:schema-version descriptor))
                             (validate-v2-command-result! (focus-vars! descriptor)
                                                          command-result)
                             command-result)
            profile (:input-profile descriptor)
            revision (git-output repo-root "rev-parse" "--verify" "HEAD")
            value {"schema_version" "abc-adr-evidence-run-v1"
                   "producer" {"tool" (:tool descriptor)
                               "command" (str/join " " (map pr-str (:argv descriptor)))
                               "revision" revision}
                   "input_profile" (cond-> {"kind" (:kind profile)
                                            "roots" (vec (:roots profile))
                                            "explicit" (vec (:explicit profile))}
                                     (= "component-clojure-test-v1" (:kind profile))
                                     (assoc "component_root" (:component-root profile)))
                   "inputs" (input-bindings repo-root profile analyzed-paths)
                   "observations" {(:observation-key descriptor)
                                   {"value" (zero? (:exit-code command-result))
                                    "details" {"exit_code" (:exit-code command-result)}}}}
            problems (bundle/validate-bundle-value value)]
        (when (seq problems)
          (throw (ex-info "captured evidence bundle is invalid"
                          {:exit-code 2 :problems problems})))
        (json/write-deterministic-json-file! output value)
        {:bundle value
         :exit-code (if (zero? (:exit-code command-result)) 0 1)
         :output (fs/file output)}))))

(def cli-options
  [[nil "--descriptor PATH"]
   [nil "--output PATH"]
   [nil "--repo-root PATH" :default "."]])

(defn usage [_]
  "Usage: clojure -M:abc/adr-evidence-capture --descriptor PATH --output PATH [--repo-root PATH]")

(defn -main [& args]
  (abc-cli/run-cli!
   args
   {:cli-options cli-options
    :required [:descriptor :output]
    :max-args 0
    :usage-fn usage
    :run (fn [{:keys [options]}]
           (let [repo-root (fs/file (fs/canonicalize (:repo-root options)))
                 git-root (fs/file
                           (fs/canonicalize
                            (git-output repo-root "rev-parse" "--show-toplevel")))]
             (when-not (= repo-root git-root)
               (throw (ex-info "--repo-root must be the exact Git worktree root"
                               {:exit-code 2})))
             (capture! {:repo-root repo-root
                        :descriptor (files/read-edn (:descriptor options))
                        :descriptor-path (:descriptor options)
                        :output (:output options)})))
    :fail? (comp pos? :exit-code)}))
