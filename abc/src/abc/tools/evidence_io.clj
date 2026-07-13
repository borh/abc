(ns abc.tools.evidence-io
  (:require [abc.tools.path-containment :as containment]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(def ^:dynamic *read-trace* nil)
(def ^:dynamic *ephemeral-roots* [])

(defn- canonical-file [file]
  (.getCanonicalFile (io/file file)))

(defn- below? [root file]
  (.startsWith (.toPath (canonical-file file)) (.toPath (canonical-file root))))

(defn- relative-key [root file]
  (-> (.relativize (.toPath (canonical-file root)) (.toPath (canonical-file file)))
      str
      (str/replace "\\" "/")))

(defn record-read!
  "Record a physical read in the invocation-local evidence trace, if active."
  [path]
  (when *read-trace*
    (let [{:keys [identity-root cwd-root repository ephemeral]} *read-trace*
          supplied (io/file path)
          file (canonical-file (if (.isAbsolute supplied)
                                 supplied
                                 (io/file cwd-root (str path))))]
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
        (swap! ephemeral conj (.getPath file))

        :else
        (throw (ex-info "external read is not authorized by this evidence boundary"
                        {:kind :external-read-denied :path (.getPath file)})))))
  path)

(defn with-read-trace [{:keys [identity-root cwd-root]} thunk]
  (let [identity-root (canonical-file identity-root)
        cwd-root (canonical-file cwd-root)
        repository (atom (sorted-set))
        ephemeral (atom (sorted-set))]
    (binding [*read-trace* {:identity-root identity-root :cwd-root cwd-root
                            :repository repository :ephemeral ephemeral}
              *ephemeral-roots* []]
      {:value (thunk)
       :repository-paths (vec @repository)
       :ephemeral-paths (vec @ephemeral)})))

(defn with-ephemeral-root [temp-root thunk]
  (if-not *read-trace*
    (throw (ex-info "ephemeral reads require an active read trace"
                    {:kind :invalid-ephemeral-root}))
    (let [root (canonical-file temp-root)
          identity-root (:identity-root *read-trace*)]
      (when (or (below? identity-root root)
                (below? root identity-root))
        (throw (ex-info "ephemeral root must be outside the identity tree"
                        {:kind :invalid-ephemeral-root :path (.getPath root)})))
      (binding [*ephemeral-roots* (conj *ephemeral-roots* root)]
        (thunk)))))
