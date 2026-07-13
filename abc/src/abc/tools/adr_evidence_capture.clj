(ns abc.tools.adr-evidence-capture
  (:require [abc.tools.adr-evidence-bundle :as bundle]
            [abc.tools.adr-evidence-runtime-inputs :as runtime-inputs]
            [abc.tools.hash :as hash]
            [abc.tools.json :as json]
            [babashka.process :as process]
            [abc.tools.path-containment :as containment]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.tools.cli :as cli]))

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

(defn- validate-descriptor! [descriptor descriptor-path]
  (let [version (:schema-version descriptor)
        profile (:input-profile descriptor)
        clojure-profile? (contains? #{"clojure-test-v1" "component-clojure-test-v1"}
                                    (:kind profile))
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
    (when (and (= version "abc-adr-evidence-capture-v2") (not clojure-profile?))
      (throw (ex-info "version 2 is restricted to Clojure input profiles" {:exit-code 2})))
    (when (= version "abc-adr-evidence-capture-v2")
      (let [manifest (:runtime-input-manifest descriptor)
            explicit (set (:explicit profile))
            descriptor-name (some-> descriptor-path io/file .getName)
            manifest-name (some-> manifest io/file .getName)
            manifest-prefix (if (= "component-clojure-test-v1" (:kind profile))
                              (str (str/replace (:component-root profile) #"/+$" "")
                                   "/docs/evidence/adr-inputs/")
                              "docs/evidence/adr-inputs/")]
        (when-not (and (string? manifest)
                       (str/starts-with? manifest manifest-prefix)
                       (contains? explicit manifest)
                       descriptor-name
                       (= descriptor-name manifest-name))
          (throw (ex-info "version-2 runtime manifest must be a bound ADR input"
                          {:exit-code 2 :kind :invalid-runtime-input-manifest}))))))
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
                      (hash/sha256-file (io/file repo-root path)))]))
        (into (bundle/derive-minimum-inputs repo-root profile) extra-paths)))

(defn- contained-relative-path! [repo-root path]
  (let [file (.getCanonicalFile (io/file path))
        root (.getCanonicalFile (io/file repo-root))
        relative (if (.isAbsolute (io/file path))
                   (-> (.relativize (.toPath root) (.toPath file)) str (str/replace "\\" "/"))
                   path)
        state (containment/path-state root relative)]
    (when-not (= :ok (:state state))
      (throw (ex-info "descriptor path must be a contained repository file"
                      {:exit-code 2 :state (:state state)})))
    relative))

(defn- focus-vars! [descriptor]
  (let [argv (:argv descriptor)
        focuses (->> (map vector argv (rest argv))
                     (keep (fn [[arg value]] (when (= "--focus" arg) value)))
                     (map symbol)
                     vec)]
    (when-not (and (seq focuses) (every? qualified-symbol? focuses))
      (throw (ex-info "version-2 capture requires exact qualified --focus Vars"
                      {:exit-code 2 :kind :invalid-nix-clojure-closure})))
    focuses))

(defn capture! [{:keys [repo-root descriptor descriptor-path output]}]
  (validate-descriptor! descriptor descriptor-path)
  (let [component? (= "component-clojure-test-v1" (get-in descriptor [:input-profile :kind]))
        descriptor-path (when descriptor-path (contained-relative-path! repo-root descriptor-path))
        component-root (get-in descriptor [:input-profile :component-root])
        analysis (when (= "abc-adr-evidence-capture-v2" (:schema-version descriptor))
                   (let [manifest-options {:repo-root repo-root
                                           :workspace-root (when component? repo-root)
                                           :descriptor {:path descriptor-path :value descriptor}}
                         _ (runtime-inputs/validate-runtime-input-manifest! manifest-options)
                         analysis-root (if component? (io/file repo-root component-root) repo-root)]
                     (runtime-inputs/analyze-reachable-vars analysis-root (focus-vars! descriptor))))
        prefix (if component? (str (str/replace component-root #"/+$" "") "/") "")
        analyzed-paths (when analysis
                         (map #(str prefix %) (concat (:paths analysis) (:contract-paths analysis))))]
    (require-clean! repo-root :before-command)
    (let [command-result (run-process repo-root (:argv descriptor))]
      (require-clean! repo-root :after-command)
      (let [profile (:input-profile descriptor)
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
         :output (io/file output)}))))

(def cli-options
  [[nil "--descriptor PATH"]
   [nil "--output PATH"]
   [nil "--repo-root PATH" :default "."]])

(defn- cli-args [args]
  (if (= "--" (first args)) (rest args) args))

(defn -main [& args]
  (let [{:keys [options errors]} (cli/parse-opts (cli-args args) cli-options)]
    (try
      (when (or (seq errors) (nil? (:descriptor options)) (nil? (:output options)))
        (throw (ex-info "invalid evidence capture arguments" {:exit-code 2})))
      (let [repo-root (.getCanonicalFile (io/file (:repo-root options)))
            git-root (.getCanonicalFile (io/file (git-output repo-root "rev-parse" "--show-toplevel")))
            _ (when-not (= repo-root git-root)
                (throw (ex-info "--repo-root must be the exact Git worktree root" {:exit-code 2})))
            descriptor (edn/read-string (slurp (:descriptor options)))
            result (capture! {:repo-root repo-root :descriptor descriptor
                              :descriptor-path (:descriptor options)
                              :output (:output options)})]
        (System/exit (:exit-code result)))
      (catch Exception exception
        (binding [*out* *err*]
          (println (.getMessage exception)))
        (System/exit (or (:exit-code (ex-data exception)) 2))))))
