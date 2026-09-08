(ns soranoha.za.serve
  "Verified publication exports and atomic activation. New trees are built
  beside their destination and installed by rename; existing trees are reused
  only after checking every expected byte, link and path. Work-facing routes
  are relative symlinks into the verified chain content. Withdrawn works have
  no current work route, but remain available through historical manifests
  and through a generated page explaining the withdrawal.

  An export holds two kinds of file. Chain content (manifests, signatures,
  blobs, governance events, the head pointer) is copied byte for byte, and
  the work-facing routes are names over it. Each artifact gets two of those
  names: the type-named route a citation points at, and a readable filename
  so that saving one is not saving a file called `tei`. The browse layer is
  generated: static pages that make the corpus reachable without a runtime,
  including a reading view rendered from each work's own published TEI bytes,
  and pre-built ZIP archives for the bulk selections a static tree cannot
  assemble on request. It is a pure function of this release, so the reuse
  check covers it exactly as it covers chain content, but nothing in it is
  named by a manifest or checked by a verifier, and a reader who wants the
  published record follows its links to the catalog, the manifests and the
  blobs.

  Deployment provisions publisher-owned parents with the serving group.
  Activation serializes cooperating writers and switches current last; the
  filesystem is trusted against mutation by other processes with that owner."
  (:require [babashka.fs :as fs]
            [soranoha.snh.decode :as decode]
            [soranoha.snh.repo :as repo]
            [soranoha.snh.sign :as sign]
            [soranoha.snh.verify :as verify]
            [soranoha.snh.view :as view]
            [soranoha.za.browse :as browse]
            [soranoha.za.bundle :as bundle]
            [soranoha.za.naming :as naming])
  (:import (java.io BufferedInputStream BufferedOutputStream InputStream OutputStream)
           (java.nio.channels FileChannel)
           (java.nio.file CopyOption Files OpenOption StandardOpenOption StandardCopyOption)
           (java.util Arrays)
           (java.nio.file.attribute PosixFilePermissions)
           (java.nio.file.attribute FileAttribute)))

(defn- referenced-ids
  "Every typed artifact id one manifest roots: work artifacts, the two
  admission evidence artifacts, the release catalog, and the governing event
  when present."
  [manifest]
  (concat (for [work (get manifest "works")
                artifact (get work "artifacts")]
            (get artifact "id"))
          [(get-in manifest ["admission" "assessment_snapshot"])
           (get-in manifest ["admission" "admission_report"])
           (get manifest "catalog")]
          (some-> (get manifest "governance_event") vector)))

(defn- artifact-blob-paths
  "slug -> artifact type -> blob path, for the head release. The reading view
  and the bulk archives both render from these bytes rather than from a copy
  of their own, so what a reader sees or unzips is the artifact the manifest
  names."
  [manifest]
  (into {}
        (for [work (get manifest "works")]
          [(get work "slug")
           (into {}
                 (for [artifact (get work "artifacts")]
                   [(get artifact "type")
                    (verify/blob-path (verify/id->hex (get artifact "id")))]))])))

(defn- verifying-stream
  "An OutputStream that checks what is written against `in` instead of
  writing it. The bulk archives are larger than any byte array this process
  should hold, so the reuse check compares them a buffer at a time; the
  guarantee is the same one `write!` gives, byte for byte."
  ^OutputStream [^InputStream in mismatch!]
  (let [buffer (byte-array 65536)
        check! (fn [^bytes source offset length]
                 (loop [offset offset remaining length]
                   (when (pos? remaining)
                     (let [want (int (min remaining (alength buffer)))
                           got (.readNBytes in buffer 0 want)
                           from (int offset)]
                       (when (or (not= got want)
                                 (not (Arrays/equals buffer (int 0) want
                                                     source from (int (+ from want)))))
                         (mismatch!))
                       (recur (+ offset want) (- remaining want))))))]
    (proxy [OutputStream] []
      (write
        ([b]
         (if (bytes? b)
           (check! b 0 (alength ^bytes b))
           (check! (byte-array 1 (unchecked-byte b)) 0 1)))
        ([b offset length] (check! b offset length))))))

(defn- tree-paths [root]
  (with-open [paths (Files/walk (fs/path root) (make-array java.nio.file.FileVisitOption 0))]
    (into #{} (iterator-seq (.iterator paths)))))

(defn- export-at!
  "`release-doi` is the release's Zenodo version DOI. It is deployment
  configuration rather than chain content: Zenodo mints it after the release
  exists, so nothing signed can carry it, and before the first deposit there
  is none. Every citation this export renders names it when it is known.

  It is therefore part of what the generated pages are a function of, and the
  reuse check compares those pages byte for byte. Setting or changing a DOI
  for a commit that has already been exported makes reuse fail closed with
  :serving-tree-mismatch rather than serve two different citations for one
  tree; the operator removes that tree and re-exports."
  [{:keys [clone pinned-keys out-dir release-doi]} commit reuse?]
  (let [v (view/git-view clone)
        chain-result (verify/verify-repository-at v commit pinned-keys)]
    (when (:empty chain-result)
      (throw (ex-info "nothing to serve before the first release"
                      {:reason :no-published-release})))
    (when (and (not reuse?) (fs/exists? out-dir))
      (throw (ex-info "serving-tree destination already exists"
                      {:reason :destination-exists :out-dir (str out-dir)})))
    (let [out-path (fs/absolutize out-dir)
          parent (fs/create-dirs (fs/parent out-path))
          staging (if reuse? out-path
                      (Files/createTempDirectory
                       (fs/path parent)
                       (str (fs/file-name out-path) ".staging.")
                       (make-array FileAttribute 0)))
          expected (when reuse? (volatile! (transient #{staging})))
          mismatch! (fn [path]
                      (throw (ex-info "Existing serving tree differs from verified export"
                                      {:reason :serving-tree-mismatch :path (str path)})))
          directory! (fn [path]
                       (if reuse?
                         (doseq [dir (take-while #(and % (not= parent %))
                                                 (iterate fs/parent path))]
                           (vswap! expected conj! dir)
                           (when (or (fs/sym-link? dir) (not (fs/directory? dir)))
                             (mismatch! dir)))
                         (fs/create-dirs path)))
          write! (fn [rel ^bytes bytes]
                   (let [path (fs/path staging rel)]
                     (directory! (fs/parent path))
                     (when reuse? (vswap! expected conj! path))
                     (if reuse?
                       (when-not (and (not (fs/sym-link? path))
                                      (fs/regular-file? path)
                                      (Arrays/equals bytes ^bytes (fs/read-all-bytes path)))
                         (mismatch! path))
                       (fs/write-bytes path bytes))))
          link! (fn [path target]
                  (directory! (fs/parent path))
                  (when reuse? (vswap! expected conj! path))
                  (if reuse?
                    (when-not (and (fs/sym-link? path)
                                   (= target (str (fs/read-link path))))
                      (mismatch! path))
                    (fs/create-sym-link path target)))
          stream! (fn [rel produce]
                    (let [path (fs/path staging rel)]
                      (directory! (fs/parent path))
                      (when reuse? (vswap! expected conj! path))
                      (if reuse?
                        (do
                          (when (or (fs/sym-link? path) (not (fs/regular-file? path)))
                            (mismatch! path))
                          (with-open [in (BufferedInputStream.
                                          (Files/newInputStream path (make-array OpenOption 0)))]
                            (produce (verifying-stream in #(mismatch! path)))
                            ;; a prefix match is not a match: the existing file
                            ;; must also end where the produced bytes end
                            (when-not (neg? (.read in)) (mismatch! path))))
                        (with-open [out (BufferedOutputStream.
                                         (Files/newOutputStream
                                          path
                                          (into-array OpenOption
                                                      [StandardOpenOption/CREATE_NEW
                                                       StandardOpenOption/WRITE])))]
                          (produce out)))))
          chain (:chain chain-result)
          page-count (volatile! 0)
          archive-count (volatile! 0)]
      (try
        (let [result (view/with-batch
                       v
                       (fn [reader]
                         (let [read! (fn [path]
                                       (or (view/read-at reader commit path)
                                           (throw (ex-info "verified chain path unreadable"
                                                           {:reason :path-unreadable :path path}))))
                               manifests (mapv (fn [hex]
                                                 [hex
                                                  (:value (decode/decode
                                                           "release-manifest"
                                                           (read! (verify/manifest-path hex))))])
                                               chain)
                               blob-hexes (into (sorted-set)
                                                (comp (map second)
                                                      (mapcat referenced-ids)
                                                      (map verify/id->hex))
                                                manifests)
                               event-hexes (into (sorted-set)
                                                 (keep #(some-> (get (second %) "governance_event")
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
                           ;; Relative symlinks provide stable human routes into chain content without duplicating bytes.
                           (let [head-manifest (second (first manifests))
                                 head-catalog (:value
                                               (decode/decode
                                                "catalog"
                                                (read! (verify/blob-path
                                                        (verify/id->hex
                                                         (get head-manifest "catalog"))))))
                                 blobs (artifact-blob-paths head-manifest)
                                 artifact! (fn [slug type]
                                             (read! (or (get-in blobs [slug type])
                                                        (throw (ex-info "release work has no artifact of this type"
                                                                        {:reason :missing-artifact
                                                                         :slug slug :type type})))))]
                             (link! (fs/path staging "releases" "latest")
                                    (str (:head chain-result) ".json"))
                             (link! (fs/path staging "catalog.json")
                                    (verify/blob-path
                                     (verify/id->hex (get head-manifest "catalog"))))
                             ;; two names over one blob. The type-named route is
                             ;; constructible from the identifier and is what a
                             ;; citation points at; the readable name is what a
                             ;; browser save or `curl -O` writes to disk, which
                             ;; without it is a file called `tei`.
                             (let [catalog-by-slug
                                   (into {} (map (juxt #(get % "slug") identity))
                                         (get head-catalog "works"))]
                               (doseq [work (get head-manifest "works")
                                       :let [slug (get work "slug")
                                             dir (fs/path staging "works" slug)
                                             entry (or (get catalog-by-slug slug)
                                                       (throw (ex-info "release work is not in the catalog"
                                                                       {:reason :work-not-in-catalog
                                                                        :slug slug})))]
                                       artifact (get work "artifacts")
                                       :let [type (get artifact "type")
                                             target (str "../../"
                                                         (verify/blob-path
                                                          (verify/id->hex (get artifact "id"))))]]
                                 (link! (fs/path dir type) target)
                                 (link! (fs/path dir (naming/filename entry type)) target)))
                             (doseq [entry (get head-manifest "withdrawn")]
                               (link!
                                (fs/path staging "withdrawn" (str (get entry "slug") ".json"))
                                (str "../" (verify/event-path
                                            (verify/id->hex (get entry "event"))))))
                             ;; Presentation over verified bytes written as static files.
                             ;; Reading pages hold a whole rendered work each, so the sequence
                             ;; is consumed one page at a time.
                             (doseq [[path bytes]
                                     (browse/pages
                                      {:head-hex (:head chain-result)
                                       :manifests manifests
                                       :catalog head-catalog
                                       :events (into {}
                                                     (map (fn [hex]
                                                            [hex (:value (decode/decode
                                                                          "governance-event"
                                                                          (read! (verify/event-path hex))))]))
                                                     event-hexes)
                                       :tei (fn [slug] (artifact! slug "tei"))
                                       :doi release-doi})]
                               (write! path bytes)
                               (vswap! page-count inc))
                             ;; Stream bulk archives into destination or compare stream on reuse path.
                             (doseq [[path produce] (bundle/archives
                                                     {:catalog head-catalog
                                                      :artifact artifact!
                                                      :release {:head-hex (:head chain-result)
                                                                :doi release-doi}})]
                               (stream! path produce)
                               (vswap! archive-count inc)))
                           {:head (:head chain-result)
                            :releases (count chain)
                            :blobs (count blob-hexes)
                            :pages @page-count
                            :archives @archive-count})))]
          ;; Atomic rename into the exporter-owned parent ensures atomic namespace visibility.
          (if reuse?
            (when-not (= (persistent! @expected) (tree-paths staging)) (mismatch! staging))
            (Files/move (fs/path staging) out-path
                        (into-array CopyOption [StandardCopyOption/ATOMIC_MOVE])))
          result)
        (finally
          (when (and (not reuse?) (fs/exists? staging))
            (fs/delete-tree staging)))))))

(defn- fetch-head! [clone branch]
  (or (repo/fetch! clone branch)
      (throw (ex-info "no publication branch"
                      {:reason :no-publication-branch :branch branch}))))

(defn export-tree!
  "Export a fully verified chain into a new directory using an atomic rename.
  Failure removes only this invocation's staging tree. Returns head and counts."
  [{:keys [clone branch] :as opts}]
  (export-at! opts (fetch-head! clone branch) false))

(defn activate!
  "Verify and install the fetched publication under serve-root/trees/COMMIT,
  then atomically switch current. Existing trees must match every exported
  byte, link and path. The provisioned root and trees directories must be
  publisher-owned and inherit the serving group. A file lock serializes
  cooperating activators; failures leave current unchanged."
  [{:keys [clone branch serve-root] :as opts}]
  (let [root (fs/absolutize serve-root)
        trees (fs/path root "trees")]
    (when-not (and (fs/directory? root) (fs/directory? trees)
                   (not (fs/sym-link? root)) (not (fs/sym-link? trees)))
      (throw (ex-info "Serving directories must be provisioned before activation"
                      {:reason :serving-root-not-provisioned})))
    (with-open [channel (FileChannel/open
                         (fs/path root ".activation.lock")
                         (into-array OpenOption [StandardOpenOption/CREATE StandardOpenOption/WRITE]))
                _lock (.lock channel)]
      (let [commit (fetch-head! clone branch)
            target (fs/path trees commit)
            reused? (fs/exists? target {:nofollow-links true})
            result (export-at! (assoc opts :out-dir target) commit reused?)
            current (fs/path root "current")]
        (doseq [path (tree-paths target) :when (not (fs/sym-link? path))]
          (Files/setPosixFilePermissions
           path (PosixFilePermissions/fromString
                 (if (fs/directory? path) "rwxr-x---" "rw-r-----"))))
        (when-not (= commit (fetch-head! clone branch))
          (throw (ex-info "Publication head changed during export; current unchanged"
                          {:reason :publication-head-changed})))
        (let [relative (str "trees/" commit)]
          (when-not (and (fs/sym-link? current) (= relative (str (fs/read-link current))))
            (let [staging (Files/createTempDirectory root ".activation."
                                                     (make-array FileAttribute 0))
                  pointer (fs/path staging "current")]
              (try
                (fs/create-sym-link pointer relative)
                (Files/move pointer current
                            (into-array CopyOption [StandardCopyOption/ATOMIC_MOVE
                                                    StandardCopyOption/REPLACE_EXISTING]))
                (finally (fs/delete-tree staging))))))
        (assoc result :commit commit :reused? reused?)))))
