(ns abc.tools.adr-evidence-capture
  (:require [abc.tools.adr-evidence-bundle :as bundle]
            [abc.tools.adr-evidence-operational :as operational]
            [abc.tools.adr-evidence-runtime-inputs :as runtime-inputs]
            [abc.tools.cli :as abc-cli]
            [abc.tools.evidence-output :as evidence-output]
            [abc.tools.files :as files]
            [abc.tools.hash :as hash]
            [abc.tools.path-containment :as containment]
            [babashka.fs :as fs]
            [babashka.process :as process]
            [clojure.string :as str]))

(def ^:private descriptor-v1-keys
  #{:schema-version :tool :argv :input-profile :observation-key})
(def ^:private descriptor-v2-keys
  (conj descriptor-v1-keys :runtime-input-manifest))

(defn- run-process
  ([repo-root argv] (run-process repo-root argv nil))
  ([repo-root argv environment]
   (let [{:keys [exit out err]}
         @(process/process argv
                           (cond-> {:dir (str repo-root) :out :string :err :string}
                             environment (assoc :env environment)))]
     {:exit-code exit :stdout out :stderr err})))

(defn- git-output [repo-root & args]
  (let [result (run-process repo-root (into ["git"] args))]
    (when-not (zero? (:exit-code result))
      (throw (ex-info "git command failed during evidence capture" result)))
    (str/trim (:stdout result))))

(defn- require-clean! [repo-root phase]
  (when (seq (git-output repo-root "status" "--porcelain" "--untracked-files=all"))
    (throw (ex-info "evidence capture requires a clean Git worktree"
                    {:phase phase :exit-code 2}))))

(def ^:private nix-environment-keys
  ["PATH" "NIX_REMOTE" "NIX_SSL_CERT_FILE" "SSL_CERT_FILE"])

(defn- nix-local-environment [home]
  (merge (into {} (keep (fn [key]
                          (when-let [value (System/getenv key)] [key value])))
               nix-environment-keys)
         {"HOME" (str home) "LANG" "C.UTF-8" "LC_ALL" "C.UTF-8"}))

(defn- require-successful-infrastructure! [label result]
  (when-not (zero? (:exit-code result))
    (throw (ex-info (str label " failed during evidence capture")
                    (assoc result :exit-code 2 :kind :evidence-infrastructure-failure))))
  (str/trim (:stdout result)))

(defn- run-operational-process [repo-root context argv]
  (let [policy (get-in context [:catalog-row :environment-policy])]
    (when-not (= :nix-local-v1 policy)
      (throw (ex-info "operational environment policy is unsupported"
                      {:exit-code 2 :kind :invalid-evidence-descriptor
                       :environment-policy policy})))
    (fs/with-temp-dir [home {:prefix "abc-evidence-home-"}]
      (let [environment (nix-local-environment home)
            nix-version (require-successful-infrastructure!
                         "nix --version"
                         (run-process repo-root ["nix" "--version"] environment))
            nix-system (require-successful-infrastructure!
                        "nix system discovery"
                        (run-process repo-root
                                     ["nix" "eval" "--raw" "--impure" "--expr"
                                      "builtins.currentSystem"]
                                     environment))
            result (run-process repo-root argv environment)]
        (assoc result :operational-details
               {"nix_system" nix-system
                "nix_version" nix-version
                "command_id" (name (get-in context [:catalog-row :command-id]))
                "environment_policy" (name policy)})))))

(defn- normalized-component-root [profile]
  (-> (:component-root profile) fs/path fs/normalize str (str/replace "\\" "/")))

(defn- expected-runner [profile]
  (if (= "component-clojure-test-v1" (:kind profile))
    (-> (fs/path (normalized-component-root profile) "bin/kaocha")
        fs/normalize
        str
        (str/replace "\\" "/"))
    "bin/kaocha"))

(defn- wrapped-v2-contract [descriptor]
  (let [profile (:input-profile descriptor)
        component? (= "component-clojure-test-v1" (:kind profile))
        component (when component? (normalized-component-root profile))
        argv (:argv descriptor)
        command (when (= ["bash" "-lc"] (subvec argv 0 (min 2 (count argv))))
                  (nth argv 2 nil))
        kaocha-prefix (str "cd " component " && bin/kaocha --focus ")
        bootstrap-command
        (str "cd " component
             " && clojure -M:abc/adr-evidence-bootstrap -- --repo-root ."
             " --workspace-root .. --verify"
             " docs/evidence/adr-bootstrap/pre-promotion.json")]
    (cond
      (and component?
           (= "bash" (:tool descriptor))
           (string? command)
           (str/starts-with? command kaocha-prefix))
      (let [focus (subs command (count kaocha-prefix))]
        (when (and (qualified-symbol? (symbol focus))
                   (not (str/includes? focus " ")))
          {:kind :kaocha :vars [(symbol focus)]}))

      (and component?
           (= "bash" (:tool descriptor))
           (= command bootstrap-command))
      {:kind :cli :vars ['abc.tools.adr-evidence-bootstrap/-main]}

      :else nil)))

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
                  (mapv (comp symbol second) pairs))
        contract (or (when (seq focuses) {:kind :kaocha :vars focuses})
                     (wrapped-v2-contract descriptor))]
    (when-not contract
      (throw (ex-info "version-2 capture requires a bound repository Kaocha runner and exact focuses"
                      {:exit-code 2 :kind :invalid-focused-evidence-runner})))
    contract))

(defn- validate-runner! [repo-root descriptor]
  (let [wrapped (wrapped-v2-contract descriptor)
        runner (expected-runner (:input-profile descriptor))
        state (containment/path-state repo-root runner)]
    (when-not (or wrapped
                  (and (= :ok (:state state))
                       (fs/regular-file? (:path state))
                       (fs/executable? (:path state))))
      (throw (ex-info "version-2 Kaocha runner must be a contained executable file"
                      {:exit-code 2 :kind :invalid-focused-evidence-runner
                       :runner runner :state (:state state)})))
    runner))

(def ^:private kaocha-summary-pattern
  #"(\d+) tests?, (\d+) assertions?(?:, (\d+) errors?)?, (\d+) failures?\.")

(defn- validate-v2-command-result! [contract command-result]
  (let [contract (if (map? contract) contract {:kind :kaocha :vars contract})
        summaries (re-seq kaocha-summary-pattern (:stdout command-result))
        test-count (some-> summaries first second parse-long)
        expected (count (distinct (:vars contract)))
        valid? (case (:kind contract)
                 :kaocha (and (= 1 (count summaries))
                              (pos? expected)
                              (= expected test-count))
                 :cli (and (zero? (:exit-code command-result))
                           (empty? summaries))
                 false)]
    (when-not valid?
      (throw (ex-info "Kaocha did not execute each focused evidence test exactly once"
                      {:exit-code 2 :kind :invalid-focused-evidence-runner
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

(defn- input-bindings [repo-root workspace-root profile extra-paths]
  (let [input-root (if (= "component-clojure-test-v1" (:kind profile))
                     workspace-root repo-root)]
    (into (sorted-map)
          (map (fn [path]
                 [path (hash/format-sha256
                        (hash/sha256-file (fs/file input-root path)))]))
          (into (bundle/derive-minimum-inputs input-root profile) extra-paths))))

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

(defn- prefixed-component-path [profile path]
  (if (= "component-clojure-test-v1" (:kind profile))
    (str (str/replace (normalized-component-root profile) #"/+$" "") "/" path)
    path))

(defn- validate-focused-runner! [repo-root descriptor]
  (let [runner (:tool descriptor)
        state (containment/path-state repo-root runner)]
    (when-not (and (= :ok (:state state))
                   (fs/regular-file? (:path state))
                   (fs/executable? (:path state)))
      (throw (ex-info "focused Kaocha runner must be a contained executable file"
                      {:exit-code 2 :kind :invalid-focused-evidence-runner
                       :runner runner :state (:state state)})))
    runner))

(defn capture!
  [{:keys [repo-root workspace-root staging-root descriptor-path output] :as options}]
  (when (contains? options :descriptor)
    (throw (ex-info "capture descriptors must be loaded from their contained coordinate"
                    {:exit-code 2 :kind :invalid-evidence-descriptor})))
  (let [repo-root (fs/file (fs/canonicalize repo-root))
        workspace-root (fs/file (fs/canonicalize (or workspace-root repo-root)))
        descriptor-path (contained-relative-path! repo-root descriptor-path)
        context (operational/load-descriptor-context!
                 {:repo-root repo-root :workspace-root workspace-root
                  :descriptor-path descriptor-path})
        descriptor (get-in context [:descriptor :value])
        version (:schema-version descriptor)
        _ (when (contains? #{"abc-adr-evidence-capture-v1"
                             "abc-adr-evidence-capture-v2"} version)
            (validate-descriptor! descriptor descriptor-path))
        _ (when (or (not= repo-root workspace-root)
                    (contains? #{"abc-adr-evidence-capture-v3"
                                 "abc-adr-evidence-capture-operational-v1"} version))
            (runtime-inputs/validate-workspace-root! repo-root workspace-root))
        destination (evidence-output/validated-destination
                     {:repo-root repo-root :workspace-root workspace-root
                      :staging-root staging-root :output output})
        component? (= "component-clojure-test-v1" (get-in descriptor [:input-profile :kind]))
        component-root (when component?
                         (normalized-component-root (:input-profile descriptor)))
        focused? (contains? #{"abc-adr-evidence-capture-v2"
                              "abc-adr-evidence-capture-v3"} version)
        contract (when focused?
                   (if (= version "abc-adr-evidence-capture-v3")
                     {:kind :kaocha :vars [(:focus-var (:catalog-row context))]}
                     (focus-vars! descriptor)))
        _ (when focused?
            (if (= version "abc-adr-evidence-capture-v3")
              (validate-focused-runner! repo-root descriptor)
              (validate-runner! repo-root descriptor)))
        analysis (when focused?
                   (let [runtime-descriptor
                         (if component?
                           {:path (prefixed-component-path (:input-profile descriptor)
                                                           descriptor-path)
                            :value (update descriptor :runtime-input-manifest
                                           #(prefixed-component-path
                                             (:input-profile descriptor) %))}
                           {:path descriptor-path :value descriptor})
                         manifest-options {:repo-root repo-root
                                           :workspace-root workspace-root
                                           :descriptor runtime-descriptor}
                         _ (runtime-inputs/validate-runtime-input-manifest! manifest-options)
                         analysis-root repo-root]
                     (when (= :kaocha (:kind contract))
                       (runtime-inputs/validate-focused-deftests!
                        analysis-root (:vars contract)))
                     (-> (runtime-inputs/analyze-reachable-vars
                          analysis-root (:vars contract))
                         runtime-inputs/assert-v2-boundary-ownership!)))
        prefix (if component? (str (str/replace component-root #"/+$" "") "/") "")
        analyzed-paths (when analysis
                         (map #(str prefix %) (concat (:paths analysis) (:contract-paths analysis))))]
    (when (= version "abc-adr-evidence-capture-operational-v1")
      (operational/validate-operational-manifest! context))
    (require-clean! workspace-root :before-command)
    (let [command-result (if (= version "abc-adr-evidence-capture-operational-v1")
                           (run-operational-process repo-root context (:argv descriptor))
                           (run-process repo-root (:argv descriptor)))]
      (require-clean! workspace-root :after-command)
      (let [command-result (if focused?
                             (validate-v2-command-result! contract
                                                          command-result)
                             command-result)
            profile (:input-profile descriptor)
            revision (git-output workspace-root "rev-parse" "--verify" "HEAD")
            value {"schema_version" "abc-adr-evidence-run-v1"
                   "producer" {"tool" (:tool descriptor)
                               "command" (str/join " " (map pr-str (:argv descriptor)))
                               "revision" revision}
                   "input_profile" (cond-> {"kind" (:kind profile)
                                            "roots" (vec (:roots profile))
                                            "explicit" (vec (:explicit profile))}
                                     (= "component-clojure-test-v1" (:kind profile))
                                     (assoc "component_root" (:component-root profile)))
                   "inputs" (input-bindings repo-root workspace-root profile analyzed-paths)
                   "observations" {(:observation-key descriptor)
                                   {"value" (zero? (:exit-code command-result))
                                    "details" (merge {"exit_code" (:exit-code command-result)}
                                                     (:operational-details command-result))}}}
            problems (bundle/validate-bundle-value value)]
        (when (seq problems)
          (throw (ex-info "captured evidence bundle is invalid"
                          {:exit-code 2 :problems problems})))
        (evidence-output/write-json-exclusive! destination value)
        {:bundle value
         :exit-code (if (zero? (:exit-code command-result)) 0 1)
         :output (fs/file (:output destination))}))))

(def cli-options
  [[nil "--descriptor PATH"]
   [nil "--output PATH"]
   [nil "--repo-root PATH" :default "."]
   [nil "--workspace-root PATH"]
   [nil "--staging-root PATH"]])

(defn usage [_]
  "Usage: clojure -M:abc/adr-evidence-capture --descriptor PATH --output PATH --staging-root PATH [--repo-root PATH] [--workspace-root PATH]")

(defn -main [& args]
  (abc-cli/run-cli!
   args
   {:cli-options cli-options
    :required [:descriptor :output :staging-root]
    :max-args 0
    :usage-fn usage
    :run (fn [{:keys [options]}]
           (let [repo-root (fs/file (fs/canonicalize (:repo-root options)))]
             (capture! {:repo-root repo-root
                        :workspace-root (or (:workspace-root options) repo-root)
                        :staging-root (:staging-root options)
                        :descriptor-path (:descriptor options)
                        :output (:output options)})))
    :fail? (comp pos? :exit-code)}))
