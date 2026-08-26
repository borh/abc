(ns soranoha.za.serve
  "The serving tree: blobs/, releases/, governance/, and a derived
  history.json, exported from a fully verified publication chain. Every
  exported byte is either chain content read through the restricted view
  at the verified head commit, or — history.json alone — a projection of
  the verified chain; nothing is exported before the whole chain
  verifies, and only what the chain references is exported. Withdrawn
  works stay served under their historical manifests (the protocol
  promises absence from current works, never byte erasure); removal from
  work-facing routes is the service's own obligation, on top of this
  tree."
  (:require [babashka.fs :as fs]
            [soranoha.ported.json :as abc-json]
            [soranoha.snh.decode :as decode]
            [soranoha.snh.repo :as repo]
            [soranoha.snh.sign :as sign]
            [soranoha.snh.verify :as verify]
            [soranoha.snh.view :as view]))

(defn- referenced-ids
  "Every typed artifact id one manifest roots: work artifacts, the two
  admission evidence artifacts, and the governing event when present."
  [manifest]
  (concat (for [work (get manifest "works")
                artifact (get work "artifacts")]
            (get artifact "id"))
          [(get-in manifest ["admission" "assessment_snapshot"])
           (get-in manifest ["admission" "admission_report"])]
          (some-> (get manifest "governance_event") vector)))

(defn export-tree!
  "Export the serving tree for the current chain of `branch` into
  `out-dir`. Fetches, fully verifies the head with the published checker,
  then writes every chain manifest + signature, every referenced blob,
  every governing event + signature, releases/HEAD, and history.json (the
  verified chain head-first, one entry per release). Idempotent: a
  re-export of the same chain rewrites identical bytes. Returns
  {:head :releases :blobs}."
  [{:keys [clone branch pinned-keys out-dir]}]
  (let [v (view/git-view clone)
        commit (or (repo/fetch! clone branch)
                   (throw (ex-info "no publication branch"
                                   {:reason :no-publication-branch
                                    :branch branch})))
        chain-result (verify/verify-repository-at v commit pinned-keys)]
    (when (:empty chain-result)
      (throw (ex-info "nothing to serve before the first release"
                      {:reason :no-published-release})))
    (let [read! (fn [path]
                  (or (view/read-at v commit path)
                      (throw (ex-info "verified chain path unreadable"
                                      {:reason :path-unreadable :path path}))))
          write! (fn [rel ^bytes bytes]
                   (let [path (fs/path out-dir rel)]
                     (fs/create-dirs (fs/parent path))
                     (fs/write-bytes path bytes)))
          chain (:chain chain-result)
          manifests (mapv (fn [hex]
                            (:value (decode/decode
                                     "release-manifest"
                                     (read! (verify/manifest-path hex)))))
                          chain)
          blob-hexes (into (sorted-set)
                           (comp (mapcat referenced-ids)
                                 (map verify/id->hex))
                           manifests)
          event-hexes (into (sorted-set)
                            (keep #(some-> (get % "governance_event")
                                           verify/id->hex))
                            manifests)]
      (doseq [hex chain]
        (write! (verify/manifest-path hex) (read! (verify/manifest-path hex)))
        (write! (verify/manifest-sig-path hex)
                (read! (verify/manifest-sig-path hex))))
      (doseq [hex blob-hexes]
        (write! (verify/blob-path hex) (read! (verify/blob-path hex))))
      (doseq [hex event-hexes]
        (write! (verify/event-path hex) (read! (verify/event-path hex)))
        (write! (verify/event-sig-path hex)
                (read! (verify/event-sig-path hex))))
      (write! verify/head-path (sign/hex64-lf-bytes (:head chain-result)))
      (write! "history.json"
              (.getBytes ^String
               (abc-json/write-deterministic-json-str
                {"head" (:head chain-result)
                 "releases"
                 (mapv (fn [hex manifest]
                         {"manifest_id" hex
                          "prev_manifest" (get manifest
                                               "prev_manifest")
                          "governance_event" (get manifest
                                                  "governance_event")
                          "work_count" (count (get manifest "works"))
                          "withdrawn_count"
                          (count (get manifest "withdrawn"))})
                       chain manifests)})
                         "UTF-8"))
      {:head (:head chain-result)
       :releases (count chain)
       :blobs (count blob-hexes)})))
