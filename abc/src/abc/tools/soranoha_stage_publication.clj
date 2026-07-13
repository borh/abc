(ns abc.tools.soranoha-stage-publication
  (:require [abc.tools.files :as files]
            [abc.tools.manifest :as manifest]
            [abc.tools.snapshot-index :as snapshot-index]
            [abc.tools.tar :as tar]
            [babashka.fs :as fs]
            [clojure.java.io :as io]
            [clojure.string :as string]))

(defn- staged-work-slug [locator-path]
  (or (second (re-find #"^artifacts/works/([^/]+)/" locator-path))
      "single"))

(defn- manifest-file [snapshot-root reference]
  (case (get-in reference ["locator" "kind"])
    "loose" (io/file snapshot-root (get-in reference ["locator" "path"]))
    (throw (ex-info "stage-publication only accepts loose input manifest locators"
                    {:locator (get reference "locator")
                     :artifact_id (get reference "artifact_id")}))))

(defn- manifest-content-file [manifest-file manifest-value]
  (fs/file (fs/parent manifest-file)
           (get-in manifest-value ["content" "path_hint"])))

(defn- loose-kind? [layout-policy artifact-kind]
  (contains? (set (get layout-policy "loose_artifact_kinds" []))
             artifact-kind))

(defn- batched-kind? [layout-policy artifact-kind]
  (contains? (set (get layout-policy "batched_artifact_kinds" []))
             artifact-kind))

(defn- staged-manifest-name [artifact-kind]
  (case artifact-kind
    "parser-ir" "parser-ir.manifest.json"
    "plaintext" "plaintext.manifest.json"
    "tei" "tei.manifest.json"
    "analysis" "analysis.manifest.json"
    (str artifact-kind ".manifest.json")))

(defn- staged-content-name [manifest-value]
  (get-in manifest-value ["content" "path_hint"]))

(defn- staged-loose-reference
  [{:keys [staged-root slug reference manifest-file manifest-value]}]
  (let [artifact-kind (get reference "artifact_kind")
        content-name (staged-content-name manifest-value)
        manifest-target (io/file staged-root
                                 "manifests"
                                 "by-work"
                                 slug
                                 (staged-manifest-name artifact-kind))
        content-target (io/file staged-root
                                "artifacts"
                                artifact-kind
                                "by-work"
                                slug
                                content-name)]
    (files/copy-file! manifest-file manifest-target)
    (files/copy-file! (manifest-content-file manifest-file manifest-value)
                      content-target)
    (assoc reference
           "locator" {"kind" "loose"
                      "path" (string/replace
                              (str (fs/relativize staged-root manifest-target))
                              "\\" "/")})))

(defn- archive-extension [archive-format]
  (case archive-format
    "tar.zst" ".tar.zst"
    (throw (ex-info "Unsupported publication archive format"
                    {:archive_format archive-format
                     :supported_archive_formats ["tar.zst"]}))))

(defn- batch-archive-path [layout-policy artifact-kind]
  (str "artifacts/"
       artifact-kind
       "/batches/"
       artifact-kind
       "-batch-0001"
       (archive-extension (get layout-policy "archive_format"))))

(defn- batch-member-path [slug artifact-kind]
  (str slug "/" (staged-manifest-name artifact-kind)))

(defn- batched-reference-entry [layout-policy slug reference manifest-file manifest-value]
  (let [artifact-kind (get reference "artifact_kind")
        content-file (manifest-content-file manifest-file manifest-value)]
    {:reference (assoc reference
                       "locator" {"kind" "archive-member"
                                  "archive_path" (batch-archive-path
                                                  layout-policy
                                                  artifact-kind)
                                  "member_path" (batch-member-path
                                                 slug
                                                 artifact-kind)})
     :entries [{:member-path (batch-member-path slug artifact-kind)
                :source-file manifest-file}
               {:member-path (str slug "/" (staged-content-name
                                            manifest-value))
                :source-file content-file}]}))

(defn- stage-reference
  [{:keys [snapshot-root layout-policy batched] :as opts}
   reference]
  (let [locator-path (get-in reference ["locator" "path"])
        slug (staged-work-slug locator-path)
        manifest-file (manifest-file snapshot-root reference)
        manifest-value (files/read-json manifest-file)
        artifact-kind (get reference "artifact_kind")
        base (assoc opts
                    :slug slug
                    :reference reference
                    :manifest-file manifest-file
                    :manifest-value manifest-value)]
    (cond
      (batched-kind? layout-policy artifact-kind)
      (let [{:keys [reference entries]} (batched-reference-entry
                                         layout-policy
                                         slug
                                         reference
                                         manifest-file
                                         manifest-value)]
        {:reference reference
         :batched (update batched artifact-kind into entries)})

      (loose-kind? layout-policy artifact-kind)
      {:reference (staged-loose-reference base)
       :batched batched}

      :else
      {:reference (staged-loose-reference base)
       :batched batched})))

(defn stage-publication!
  [{:keys [snapshot-root staged-root snapshot]}]
  (let [snapshot-root (io/file snapshot-root)
        staged-root (io/file staged-root)
        layout-policy (get snapshot "layout_policy")
        references (get snapshot "artifact_references" [])]
    (files/delete-tree! staged-root)
    (fs/create-dirs staged-root)
    (let [staged (reduce (fn [{:keys [references batched]} reference]
                           (let [{staged-reference :reference
                                  staged-batched :batched}
                                 (stage-reference
                                  {:snapshot-root snapshot-root
                                   :staged-root staged-root
                                   :layout-policy layout-policy
                                   :batched batched}
                                  reference)]
                             {:references (conj references staged-reference)
                              :batched staged-batched}))
                         {:references []
                          :batched {}}
                         references)]
      (doseq [[artifact-kind entries] (:batched staged)]
        (tar/write-tar! (io/file staged-root
                                 (batch-archive-path layout-policy
                                                     artifact-kind))
                        entries))
      (when (fs/regular-file? (fs/file snapshot-root "run-summary.json"))
        (files/copy-file! (io/file snapshot-root "run-summary.json")
                          (io/file staged-root "run-summary.json")))
      (let [staged-snapshot (assoc snapshot
                                   "artifact_references"
                                   (snapshot-index/sort-artifact-references
                                    (:references staged)))
            index-file (io/file staged-root "index.json")]
        (manifest/write-json-file! index-file staged-snapshot)
        {:staged-root staged-root
         :index-file index-file
         :snapshot staged-snapshot
         :archive-count (count (:batched staged))}))))
