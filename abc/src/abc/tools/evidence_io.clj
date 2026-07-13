(ns abc.tools.evidence-io
  (:require [abc.tools.path-containment :as containment]
            [babashka.fs :as fs]
            [clojure.string :as str]))

(def ^:dynamic *read-trace* nil)
(def ^:dynamic *ephemeral-roots* [])

(defn- canonical-file [file]
  (fs/file (fs/canonicalize file)))

(defn- below? [root file]
  (fs/starts-with? (canonical-file file) (canonical-file root)))

(defn- relative-key [root file]
  (-> (fs/relativize (canonical-file root) (canonical-file file))
      str
      (str/replace "\\" "/")))

(defn record-read!
  "Record a physical read in the invocation-local evidence trace, if active."
  [path]
  (when *read-trace*
    (let [{:keys [identity-root cwd-root repository ephemeral]} *read-trace*
          supplied (fs/path path)
          file (canonical-file (if (fs/absolute? supplied)
                                 supplied
                                 (fs/path cwd-root (str path))))]
      (cond
        (below? identity-root file)
        (let [key (relative-key identity-root file)
              state (containment/path-state identity-root key)]
          (when-not (= :ok (:state state))
            (throw (ex-info "repository read is not a contained existing path"
                            {:kind (case (:state state)
                                     :missing :missing-runtime-input
                                     :external-read-denied)
                             :path key :state (:state state)})))
          (swap! repository conj key))

        (some #(below? % file) *ephemeral-roots*)
        (swap! ephemeral conj (str file))

        :else
        (throw (ex-info "external read is not authorized by this evidence boundary"
                        {:kind :external-read-denied :path (str file)})))))
  path)

(defn with-read-trace [{:keys [identity-root cwd-root workspace-root]} thunk]
  (let [identity-root (canonical-file identity-root)
        cwd-root (canonical-file cwd-root)
        workspace-root (canonical-file (or workspace-root identity-root))
        repository (atom (sorted-set))
        ephemeral (atom (sorted-set))]
    (binding [*read-trace* {:identity-root identity-root :cwd-root cwd-root
                            :workspace-root workspace-root
                            :repository repository :ephemeral ephemeral}
              *ephemeral-roots* []]
      {:value (thunk)
       :repository-paths (vec @repository)
       :ephemeral-paths (vec @ephemeral)})))

(defn- validate-external-root! [{:keys [identity-root workspace-root]} root]
  (let [root (canonical-file root)]
    (when (some #(or (below? % root) (below? root %))
                [identity-root workspace-root])
      (throw (ex-info "ephemeral root must be outside the identity and workspace trees"
                      {:kind :invalid-ephemeral-root :path (str root)})))))

(defn with-ephemeral-root [temp-root thunk]
  (if-not *read-trace*
    (throw (ex-info "ephemeral reads require an active read trace"
                    {:kind :invalid-ephemeral-root}))
    (let [root (canonical-file temp-root)]
      (validate-external-root! *read-trace* root)
      (binding [*ephemeral-roots* (conj *ephemeral-roots* root)]
        (thunk)))))

(defn with-owned-ephemeral-root [thunk]
  (when-not *read-trace*
    (throw (ex-info "owned ephemeral roots require an active read trace"
                    {:kind :invalid-ephemeral-root})))
  (fs/with-temp-dir [root {:prefix "abc-evidence-"}]
    (validate-external-root! *read-trace* root)
    (with-ephemeral-root root #(thunk root))))
