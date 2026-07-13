(ns abc.tools.adr-evidence-register
  (:require [abc.tools.adr :as adr]
            [abc.tools.adr-evidence :as evidence]
            [abc.tools.adr-evidence-bundle :as bundle]
            [abc.tools.path-containment :as containment]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.tools.cli :as cli])
  (:import [java.nio.file CopyOption Files StandardCopyOption]))

(def ^:private template-keys #{:schema-version :entries})
(def ^:private entry-keys
  #{:claim-id :claim-kind :evidence-kind :artifact-path :observation-key :expected})
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
              :else {:entry (assoc entry :artifact-hash (:canonical-hash loaded))})))
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
   (for [{:keys [file status criteria]} (adr/parse-all (io/file repo-root "docs/adr"))
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

(defn- allowed-migration-problem? [owned problem]
  (and (= :missing-claim-evidence (:kind problem))
       (not (contains? owned (:claim-id problem)))))

(defn- atomic-write! [registry-file value]
  (let [target (.toPath registry-file)
        temp (Files/createTempFile (.getParent target) ".adr-evidence-" ".tmp"
                                   (make-array java.nio.file.attribute.FileAttribute 0))]
    (try
      (spit (.toFile temp) (str (pr-str value) "\n"))
      (Files/move temp target
                  (into-array CopyOption [StandardCopyOption/ATOMIC_MOVE
                                          StandardCopyOption/REPLACE_EXISTING]))
      (finally
        (Files/deleteIfExists temp)))))

(defn register! [{:keys [repo-root workspace-root entries-path registry-path]}]
  (try
    (let [entries-file (contained-file repo-root entries-path)
          registry-file (contained-file repo-root registry-path)
          template (edn/read-string (slurp entries-file))
          materialized (materialize-template repo-root template)]
      (if (seq (:problems materialized))
        {:ok? false :problems (:problems materialized)}
        (let [entries (:entries materialized)
              owned (set (map :claim-id entries))
              candidate (candidate-registry (edn/read-string (slurp registry-file)) entries)
              problems (evidence/validate-registry
                        {:repo-root repo-root
                         :workspace-root (or workspace-root repo-root)
                         :claims (current-claims repo-root)
                         :registry candidate
                         :matrix (edn/read-string
                                  (slurp (contained-file repo-root evidence/matrix-path)))
                         :as-of (:as-of (edn/read-string
                                         (slurp (contained-file repo-root evidence/as-of-path))))})
              blocking (vec (remove #(allowed-migration-problem? owned %) problems))]
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

(defn- cli-args [args]
  (if (= "--" (first args)) (rest args) args))

(defn -main [& args]
  (let [{:keys [options errors]} (cli/parse-opts (cli-args args) cli-options)
        result (if (or (seq errors) (nil? (:entries options)) (nil? (:registry options)))
                 {:ok? false :problems [(problem :invalid-cli "--entries and --registry are required")]}
                 (register! {:repo-root "." :entries-path (:entries options)
                             :workspace-root (or (:workspace-root options) ".")
                             :registry-path (:registry options)}))]
    (if (:ok? result)
      (println "Registered" (:registered result) "evidence entries")
      (binding [*out* *err*] (println (pr-str (:problems result)))))
    (System/exit (if (:ok? result) 0 1))))
