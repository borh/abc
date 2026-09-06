(ns soranoha.za.serve
  "The serving tree: blobs/, releases/, and governance/ hold the chain
  content; works/, withdrawn/, and releases/latest are the work-facing
  layer — relative symlinks into that content (current works' artifacts
  by slug and type, each withdrawal statement by slug, the head
  manifest as the short-cache pointer), so the static file server needs
  no resolution logic. Everything is exported from a fully verified
  publication chain. Nothing is exported before the whole
  chain verifies, and only what the chain references is exported: the
  destination must not already exist, and the tree is built aside and
  installed in one rename, so a serving path never holds a partial tree
  or bytes from outside the chain. The destination's parent directory
  is exporter-owned: one cooperating exporter writes into it and
  nothing else creates the destination — the existence check and the
  exact-target rename enforce nothing against an uncooperative
  concurrent writer. The installed tree keeps the staging directory's
  owner-only mode, so exporter and resolver run as the same OS
  principal. Pointing routes at the installed tree is the service's own
  activation step, separate from this export.
  Withdrawn works stay served under their historical manifests (the
  protocol promises absence from current works, never byte erasure);
  removal from work-facing routes is the service's own obligation, on
  top of this tree."
  (:require [babashka.fs :as fs]
            [soranoha.snh.decode :as decode]
            [soranoha.snh.repo :as repo]
            [soranoha.snh.sign :as sign]
            [soranoha.snh.verify :as verify]
            [soranoha.snh.view :as view])
  (:import (java.nio.file CopyOption Files StandardCopyOption)
           (java.nio.file.attribute FileAttribute)))

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
  `out-dir`, which must not yet exist. Fetches, fully verifies the head
  with the published checker, then builds the tree — every chain
  manifest + signature, every referenced blob, every governing event +
  signature, releases/HEAD, and the work-facing symlink layer — in a
  uniquely named sibling staging
  directory this invocation alone creates, then installs it at
  `out-dir` with a single exact-target atomic rename. A failed export
  leaves no tree at `out-dir`, and cleanup touches only this
  invocation's own staging directory, never a directory another
  process made. Returns {:head :releases :blobs}."
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
    (when (fs/exists? out-dir)
      (throw (ex-info "serving-tree destination already exists"
                      {:reason :destination-exists :out-dir (str out-dir)})))
    (let [out-path (fs/absolutize out-dir)
          parent (fs/create-dirs (fs/parent out-path))
          staging (Files/createTempDirectory
                   (fs/path parent)
                   (str (fs/file-name out-path) ".staging.")
                   (make-array FileAttribute 0))
          write! (fn [rel ^bytes bytes]
                   (let [path (fs/path staging rel)]
                     (fs/create-dirs (fs/parent path))
                     (fs/write-bytes path bytes)))
          chain (:chain chain-result)]
      (try
        (let [result (view/with-batch
                       v
                       (fn [reader]
                         (let [read! (fn [path]
                                       (or (view/read-at reader commit path)
                                           (throw (ex-info "verified chain path unreadable"
                                                           {:reason :path-unreadable :path path}))))
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
                             (write! (verify/manifest-path hex)
                                     (read! (verify/manifest-path hex)))
                             (write! (verify/manifest-sig-path hex)
                                     (read! (verify/manifest-sig-path hex))))
                           (doseq [hex blob-hexes]
                             (write! (verify/blob-path hex) (read! (verify/blob-path hex))))
                           (doseq [hex event-hexes]
                             (write! (verify/event-path hex) (read! (verify/event-path hex)))
                             (write! (verify/event-sig-path hex)
                                     (read! (verify/event-sig-path hex))))
                           (write! verify/head-path (sign/hex64-lf-bytes (:head chain-result)))
                           ;; the work-facing layer is relative symlinks into chain content
                           ;; — human URLs for the current corpus, slug-addressed withdrawal
                           ;; statements, and the short-cache head pointer — so it adds
                           ;; names, never bytes
                           (let [head-manifest (first manifests)]
                             (fs/create-sym-link (fs/path staging "releases" "latest")
                                                 (str (:head chain-result) ".json"))
                             (doseq [work (get head-manifest "works")
                                     :let [dir (fs/path staging "works" (get work "slug"))]
                                     artifact (get work "artifacts")]
                               (fs/create-dirs dir)
                               (fs/create-sym-link
                                (fs/path dir (get artifact "type"))
                                (str "../../" (verify/blob-path
                                               (verify/id->hex (get artifact "id"))))))
                             (doseq [entry (get head-manifest "withdrawn")]
                               (fs/create-dirs (fs/path staging "withdrawn"))
                               (fs/create-sym-link
                                (fs/path staging "withdrawn" (str (get entry "slug") ".json"))
                                (str "../" (verify/event-path
                                            (verify/id->hex (get entry "event")))))))
                           {:head (:head chain-result)
                            :releases (count chain)
                            :blobs (count blob-hexes)})))]
          ;; the rename targets the exact destination path, never a
          ;; directory to nest under; atomicity here is atomic namespace
          ;; visibility — not no-clobber or crash durability — and the
          ;; exporter-owned parent is what keeps the destination from
          ;; appearing concurrently
          (Files/move (fs/path staging) out-path
                      (into-array CopyOption [StandardCopyOption/ATOMIC_MOVE]))
          result)
        (finally
          (when (fs/exists? staging)
            (fs/delete-tree staging)))))))
