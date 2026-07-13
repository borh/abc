(ns abc.tools.evidence-output
  (:require [abc.tools.json :as json]
            [abc.tools.path-containment :as containment]
            [babashka.fs :as fs])
  (:import [java.nio ByteBuffer]
           [java.nio.channels FileChannel]
           [java.nio.charset StandardCharsets]
           [java.nio.file FileAlreadyExistsException Files LinkOption OpenOption
            StandardOpenOption]
           [java.util UUID]))

(defrecord ValidatedDestination [staging-root output])

(defn- canonical-file [path]
  (fs/file (fs/canonicalize path)))

(defn- normalized-file [path]
  (fs/file (fs/normalize (fs/absolutize path))))

(defn- no-follow-exists? [path]
  (Files/exists (fs/path path)
                (into-array LinkOption [LinkOption/NOFOLLOW_LINKS])))

(defn- overlapping? [left right]
  (or (fs/starts-with? left right)
      (fs/starts-with? right left)))

(defn- invalid-destination! [message data]
  (throw (ex-info message (assoc data :kind :invalid-evidence-destination))))

(defn- canonical-existing-root [label root]
  (when-not (= :ok (:state (containment/path-state root ".")))
    (invalid-destination! "evidence destination root must exist"
                          {:root label :path (str root)}))
  (let [canonical (canonical-file root)]
    (when-not (fs/directory? canonical)
      (invalid-destination! "evidence destination root must be a directory"
                            {:root label :path (str canonical)}))
    {:lexical (normalized-file root)
     :canonical canonical}))

(defn validated-destination
  [{:keys [repo-root workspace-root staging-root output]}]
  (let [repo (canonical-existing-root :repo-root repo-root)
        workspace (canonical-existing-root :workspace-root workspace-root)
        staging (canonical-existing-root :staging-root staging-root)
        identity-paths (mapcat (juxt :lexical :canonical) [repo workspace])
        staging-paths ((juxt :lexical :canonical) staging)]
    (when (some (fn [staging-path]
                  (some #(overlapping? staging-path %) identity-paths))
                staging-paths)
      (invalid-destination!
       "staging root must be outside the repository and workspace trees"
       {:staging-root (str (:canonical staging))}))
    (when (no-follow-exists? output)
      (throw (ex-info "evidence output already exists"
                      {:kind :evidence-output-exists :path (str output)})))
    (let [lexical-output (normalized-file output)
          canonical-output (canonical-file output)
          lexical-parent (some-> lexical-output fs/parent normalized-file)
          canonical-parent (some-> canonical-output fs/parent canonical-file)]
      (when-not (and (= (:lexical staging) lexical-parent)
                     (= (:canonical staging) canonical-parent))
        (invalid-destination! "evidence output must be a direct staging child"
                              {:staging-root (str (:canonical staging))
                               :output (str canonical-output)}))
      (let [relative (str (fs/relativize (:canonical staging)
                                         canonical-output))
            state (:state (containment/path-state (:canonical staging)
                                                  relative))]
        (when-not (= :missing state)
          (invalid-destination! "evidence output is not a missing contained path"
                                {:output (str canonical-output) :state state})))
      (->ValidatedDestination (:canonical staging) canonical-output))))

(defn- fresh-uuid []
  (UUID/randomUUID))

(defn- force-channel! [^FileChannel channel metadata?]
  (.force channel metadata?))

(defn- create-link! [output temp]
  (Files/createLink output temp))

(defn- allocate-sibling! [parent output-name bytes]
  (loop []
    (let [candidate (fs/path parent
                             (str "." output-name "." (fresh-uuid) ".tmp"))
          created (try
                    (with-open [channel (FileChannel/open
                                         candidate
                                         (into-array
                                          OpenOption
                                          [StandardOpenOption/CREATE_NEW
                                           StandardOpenOption/WRITE]))]
                      (let [buffer (ByteBuffer/wrap bytes)]
                        (while (.hasRemaining buffer)
                          (.write channel buffer)))
                      (force-channel! channel true))
                    candidate
                    (catch FileAlreadyExistsException _
                      nil)
                    (catch Throwable throwable
                      (Files/deleteIfExists candidate)
                      (throw throwable)))]
      (if created created (recur)))))

(defn write-json-exclusive! [destination value]
  (when-not (instance? ValidatedDestination destination)
    (throw (ex-info "exclusive evidence writes require a validated destination"
                    {:kind :unvalidated-evidence-destination})))
  (let [output (fs/path (:output destination))
        parent (fs/parent output)
        bytes (.getBytes (json/write-deterministic-json-str value)
                         StandardCharsets/UTF_8)
        temp (allocate-sibling! parent (fs/file-name output) bytes)]
    (try
      (create-link! output temp)
      (fs/file output)
      (finally
        (when (fs/exists? temp)
          (fs/delete temp))))))
