(ns abc.tools.adr-evidence-capture
  (:require [abc.tools.adr-evidence-bundle :as bundle]
            [abc.tools.hash :as hash]
            [abc.tools.json :as json]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.tools.cli :as cli]))

(def ^:private descriptor-keys
  #{:schema-version :tool :argv :input-profile :observation-key})

(defn- run-process [repo-root argv]
  (let [process (.start (doto (ProcessBuilder. argv)
                          (.directory (io/file repo-root))))
        stdout (future (slurp (.getInputStream process)))
        stderr (future (slurp (.getErrorStream process)))
        exit-code (.waitFor process)]
    {:exit-code exit-code :stdout @stdout :stderr @stderr}))

(defn- git-output [repo-root & args]
  (let [result (run-process repo-root (into ["git"] args))]
    (when-not (zero? (:exit-code result))
      (throw (ex-info "git command failed during evidence capture" result)))
    (str/trim (:stdout result))))

(defn- require-clean! [repo-root phase]
  (when (seq (git-output repo-root "status" "--porcelain" "--untracked-files=all"))
    (throw (ex-info "evidence capture requires a clean Git worktree"
                    {:phase phase :exit-code 2}))))

(defn- validate-descriptor! [descriptor]
  (when-not (= descriptor-keys (set (keys descriptor)))
    (throw (ex-info "capture descriptor must have the exact version-1 key set"
                    {:exit-code 2 :keys (keys descriptor)})))
  (when-not (= "abc-adr-evidence-capture-v1" (:schema-version descriptor))
    (throw (ex-info "capture descriptor schema version is unsupported"
                    {:exit-code 2})))
  (when-not (and (string? (:tool descriptor))
                 (seq (:tool descriptor))
                 (vector? (:argv descriptor))
                 (every? string? (:argv descriptor))
                 (seq (:argv descriptor))
                 (string? (:observation-key descriptor)))
    (throw (ex-info "capture descriptor fields are invalid" {:exit-code 2})))
  descriptor)

(defn- input-bindings [repo-root profile]
  (into (sorted-map)
        (map (fn [path]
               [path (hash/format-sha256
                      (hash/sha256-file (io/file repo-root path)))]))
        (bundle/derive-minimum-inputs repo-root profile)))

(defn capture! [{:keys [repo-root descriptor output]}]
  (validate-descriptor! descriptor)
  (require-clean! repo-root :before-command)
  (let [command-result (run-process repo-root (:argv descriptor))]
    (require-clean! repo-root :after-command)
    (let [profile (:input-profile descriptor)
          revision (git-output repo-root "rev-parse" "--verify" "HEAD")
          value {"schema_version" "abc-adr-evidence-run-v1"
                 "producer" {"tool" (:tool descriptor)
                             "command" (str/join " " (map pr-str (:argv descriptor)))
                             "revision" revision}
                 "input_profile" {"kind" (:kind profile)
                                  "roots" (vec (:roots profile))
                                  "explicit" (vec (:explicit profile))}
                 "inputs" (input-bindings repo-root profile)
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
       :output (io/file output)})))

(def cli-options
  [[nil "--descriptor PATH"]
   [nil "--output PATH"]])

(defn -main [& args]
  (let [{:keys [options errors]} (cli/parse-opts args cli-options)]
    (try
      (when (or (seq errors) (nil? (:descriptor options)) (nil? (:output options)))
        (throw (ex-info "invalid evidence capture arguments" {:exit-code 2})))
      (let [descriptor (edn/read-string (slurp (:descriptor options)))
            result (capture! {:repo-root "." :descriptor descriptor
                              :output (:output options)})]
        (System/exit (:exit-code result)))
      (catch Exception exception
        (binding [*out* *err*]
          (println (.getMessage exception)))
        (System/exit (or (:exit-code (ex-data exception)) 2))))))
