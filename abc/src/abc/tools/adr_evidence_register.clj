(ns abc.tools.adr-evidence-register
  (:require [abc.tools.adr :as adr]
            [abc.tools.adr-evidence :as evidence]
            [abc.tools.adr-evidence-bundle :as bundle]
            [abc.tools.adr-evidence-runtime-inputs :as runtime-inputs]
            [abc.tools.cli :as abc-cli]
            [abc.tools.files :as files]
            [abc.tools.path-containment :as containment]
            [babashka.fs :as fs]
            [clojure.set :as set]))

(def ^:private template-keys #{:schema-version :entries})
(def ^:private entry-keys
  #{:claim-id :claim-kind :evidence-kind :artifact-path :observation-id
    :observation-key :expected})
(def ^:private forbidden-inline-keys #{:artifact-hash :observed :inputs :verdict})

(defn- problem [kind message & {:as data}]
  (merge {:kind kind :message message} data))

(defn- entry-shape-problems [entry]
  (if-not (map? entry)
    [(problem :invalid-registration-entry "registration entry must be a map")]
    (let [keys-present (set (keys entry))
          forbidden (set/intersection forbidden-inline-keys keys-present)
          unknown (set/difference keys-present entry-keys forbidden-inline-keys)
          missing (set/difference entry-keys keys-present)]
      (vec
       (concat
        (when (seq forbidden)
          [(problem :invalid-registration-entry "registration entry contains forbidden inline values"
                    :claim-id (:claim-id entry) :keys (vec (sort forbidden)))])
        (when (seq unknown)
          [(problem :invalid-registration-entry "registration entry contains unknown keys"
                    :claim-id (:claim-id entry) :keys (vec (sort unknown)))])
        (when (seq missing)
          [(problem :invalid-registration-entry "registration entry is missing required keys"
                    :claim-id (:claim-id entry) :keys (vec (sort missing)))]))))))

(defn materialize-template [repo-root template]
  (let [template-map? (map? template)
        template-problems
        (if-not template-map?
          [(problem :invalid-registration-template "registration template must be a map")]
          (vec
           (concat
            (when-not (= template-keys (set (keys template)))
              [(problem :invalid-registration-template "registration template must have the exact key set")])
            (when-not (= :abc-adr-evidence-registration-v1 (:schema-version template))
              [(problem :invalid-registration-template "registration template schema version is unsupported")])
            (when-not (vector? (:entries template))
              [(problem :invalid-registration-template "registration template entries must be a vector")]))))
        entries (if (vector? (:entries template)) (:entries template) [])
        shape-problems (mapcat entry-shape-problems entries)
        results
        (for [entry entries
              :when (empty? (entry-shape-problems entry))]
          (let [loaded (bundle/load-bundle repo-root (:artifact-path entry))
                observation (when-let [value (:value loaded)]
                              (get-in value ["observations" (:observation-key entry)]))]
            (cond
              (seq (:problems loaded)) {:problems (:problems loaded)}
              (nil? observation)
              {:problems [(problem :missing-observation
                                   "named observation is absent from the evidence artifact"
                                   :claim-id (:claim-id entry)
                                   :artifact-path (:artifact-path entry))]}
              :else {:entry (-> entry
                                (dissoc :observation-id)
                                (assoc :artifact-hash (:canonical-hash loaded)))})))
        problems (vec (concat template-problems shape-problems (mapcat :problems results)))]
    (if (seq problems)
      {:problems problems}
      {:entries (mapv :entry results) :problems []})))

(defn- entry-order [entry]
  [(:claim-id entry) (:artifact-path entry) (:observation-key entry)
   (pr-str (:claim-kind entry)) (pr-str (:evidence-kind entry))
   (pr-str (:expected entry)) (:artifact-hash entry)])

(defn candidate-registry [registry template-entries]
  (when-let [duplicate (first (for [[item n] (frequencies template-entries) :when (> n 1)] item))]
    (throw (ex-info "duplicate registration template entry" {:entry duplicate})))
  (let [owned (set (map :claim-id template-entries))]
    {:entries (->> (concat template-entries
                           (remove #(contains? owned (:claim-id %)) (:entries registry)))
                   (sort-by entry-order)
                   vec)}))

(defn current-claims [repo-root]
  (vec
   (for [{:keys [file status criteria]} (adr/parse-all (fs/file repo-root "docs/adr"))
         :when (= "Accepted" status)
         {:keys [claim-id] :as criterion} criteria
         :when claim-id]
     (assoc criterion :file file :status status))))

(defn- contained-file [repo-root path]
  (let [state (containment/path-state repo-root path)]
    (if (= :ok (:state state))
      (:path state)
      (throw (ex-info "registration path is not a contained existing repository file"
                      {:problems [(problem :invalid-registration-path
                                           "path is missing or escapes the repository"
                                           :path path :state (:state state))]})))))

(def ^:private diagnostic-problem-keys
  #{:message :line :column :criterion-index :expected :actual :detail
    :errors-humanized})

(defn- problem-identity [problem]
  (apply dissoc problem diagnostic-problem-keys))

(defn- problem-claim-ids [problem]
  (cond-> (set (:affected-claim-ids problem))
    (:claim-id problem) (conj (:claim-id problem))))

(defn- allowed-migration-problem? [owned baseline-identities problem]
  (and (empty? (set/intersection owned (problem-claim-ids problem)))
       (contains? baseline-identities (problem-identity problem))))

(defn- atomic-write! [registry-file value]
  (let [target (fs/path registry-file)
        temp (fs/create-temp-file {:dir (fs/parent target)
                                   :prefix ".adr-evidence-"
                                   :suffix ".tmp"})]
    (try
      (spit (fs/file temp) (str (pr-str value) "\n"))
      (fs/move temp target {:atomic-move true :replace-existing true})
      (finally
        (fs/delete-if-exists temp)))))

(defn register! [{:keys [repo-root workspace-root entries-path registry-path]}]
  (try
    (let [_ (when (and workspace-root
                       (not= (fs/canonicalize repo-root)
                             (fs/canonicalize workspace-root)))
              (runtime-inputs/validate-workspace-root! repo-root workspace-root))
          entries-file (contained-file repo-root entries-path)
          registry-file (contained-file repo-root registry-path)
          template (files/read-edn entries-file)
          materialized (materialize-template repo-root template)]
      (if (seq (:problems materialized))
        {:ok? false :problems (:problems materialized)}
        (let [entries (:entries materialized)
              owned (set (map :claim-id entries))
              registry (files/read-edn registry-file)
              candidate (candidate-registry registry entries)
              validation-context
              {:repo-root repo-root
               :workspace-root workspace-root
               :claims (current-claims repo-root)
               :matrix (files/read-edn
                        (contained-file repo-root evidence/matrix-path))
               :as-of (:as-of (files/read-edn
                               (contained-file repo-root evidence/as-of-path)))}
              baseline-problems
              (evidence/validate-registry (assoc validation-context :registry registry))
              baseline-identities (set (map problem-identity baseline-problems))
              problems
              (evidence/validate-registry (assoc validation-context :registry candidate))
              blocking (vec (remove #(allowed-migration-problem?
                                      owned baseline-identities %)
                                    problems))]
          (if (seq blocking)
            {:ok? false :problems problems}
            (do (atomic-write! registry-file candidate)
                {:ok? true :registered (count entries) :problems problems})))))
    (catch Exception exception
      {:ok? false
       :problems (or (:problems (ex-data exception))
                     [(problem :registration-failed (.getMessage exception))])})))

(def cli-options [[nil "--entries PATH"] [nil "--registry PATH"]
                  [nil "--workspace-root PATH"]])

(defn usage [_]
  "Usage: clojure -M:abc/adr-evidence-register --entries PATH --registry PATH [--workspace-root PATH]")

(defn -main [& args]
  (abc-cli/run-cli!
   args
   {:cli-options cli-options
    :required [:entries :registry]
    :max-args 0
    :usage-fn usage
    :run (fn [{:keys [options]}]
           (let [result (register! {:repo-root "."
                                    :entries-path (:entries options)
                                    :workspace-root (:workspace-root options)
                                    :registry-path (:registry options)})]
             (if (:ok? result)
               (println "Registered" (:registered result) "evidence entries")
               (binding [*out* *err*] (println (pr-str (:problems result)))))
             result))
    :fail? (complement :ok?)}))
