(ns abc.tools.adr-evidence-runtime-inputs
  (:require [abc.tools.evidence-io :as evidence-io]
            [abc.tools.files :as files]
            [abc.tools.hash :as hash]
            [abc.tools.path-containment :as containment]
            [babashka.fs :as fs]
            [babashka.process :as process]
            [clojure.edn :as edn]
            [clojure.set :as set]
            [clojure.string :as str]
            [clojure.tools.reader :as reader]
            [clojure.tools.reader.reader-types :as reader-types]))

(defn- fail! [kind message & {:as data}]
  (throw (ex-info message (assoc data :kind kind))))

(def ^:private workspace-sentinels
  ["flake.nix" "justfile" "abc/flake.nix" "ab-validator/flake.nix"])

(defn validate-workspace-root!
  "Require the monorepo identity root, not merely a containing directory.
  The Git identity and the four owned entry-point sentinels must agree."
  ([workspace-root] (validate-workspace-root! nil workspace-root))
  ([repo-root workspace-root]
   (let [root (fs/file (fs/canonicalize workspace-root))
         {:keys [exit out]} @(process/process
                              ["git" "-C" (str root) "rev-parse" "--show-toplevel"]
                              {:out :string :err :out})
         output (str/trim out)
         git-root (when (zero? exit) (fs/file (fs/canonicalize output)))
         missing (->> workspace-sentinels
                      (remove #(fs/regular-file? (fs/path root %)))
                      vec)
         expected-abc (fs/file (fs/canonicalize (fs/path root "abc")))
         actual-abc (some-> repo-root fs/canonicalize fs/file)]
     (when-not (and (zero? exit) (= root git-root) (empty? missing)
                    (or (nil? actual-abc) (= expected-abc actual-abc)))
       (fail! :invalid-workspace-root
              "workspace root must be the exact Soranoha Git root"
              :workspace-root (str root)
              :git-root (some-> git-root str)
              :abc-root (some-> actual-abc str)
              :missing missing))
     root)))

(defn- validate-path! [root path]
  (when-not (and (string? path) (seq path))
    (fail! :invalid-runtime-input-manifest "runtime input path must be non-empty" :path path))
  (let [state (containment/path-state root path)]
    (when-not (and (= :ok (:state state)) (fs/regular-file? (:path state)))
      (fail! (if (= :missing (:state state))
               :missing-runtime-input
               :invalid-runtime-input-manifest)
             "runtime input path is missing or not contained"
             :path path :state (:state state)))))

(defn- component-relative-path! [component-root path]
  (let [prefix (str (str/replace component-root #"/+$" "") "/")]
    (when-not (and (string? path) (str/starts-with? path prefix))
      (fail! :invalid-runtime-input-manifest
             "workspace runtime input is outside the selected component"
             :path path :component-root component-root))
    (subs path (count prefix))))

(defn- load-manifest!
  ([root path] (load-manifest! root path nil))
  ([root path component-root]
   (validate-path! root path)
   (let [manifest (try (files/read-edn (fs/file root path))
                       (catch Exception e
                         (fail! :invalid-runtime-input-manifest
                                "runtime input manifest is unreadable"
                                :path path :detail (.getMessage e))))
         paths (:paths manifest)
         runtime-paths (if component-root
                         (mapv #(component-relative-path! component-root %) paths)
                         paths)]
     (when-not (= #{:schema-version :paths} (set (keys manifest)))
       (fail! :invalid-runtime-input-manifest "runtime input manifest must have exactly two keys"))
     (when-not (= :abc-adr-runtime-inputs-v1 (:schema-version manifest))
       (fail! :invalid-runtime-input-manifest "runtime input manifest schema is unsupported"))
     (when-not (and (vector? paths)
                    (= paths (vec (sort paths)))
                    (= (count paths) (count (distinct paths))))
       (fail! :invalid-runtime-input-manifest "runtime input paths must be sorted and unique"))
     (doseq [input runtime-paths] (validate-path! root input))
     (assoc manifest :paths runtime-paths))))

(defn assert-runtime-input-closure!
  [{:keys [repo-root workspace-root descriptor repository-paths component-root]}]
  (let [{descriptor-path :path value :value} descriptor
        profile (:input-profile value)
        component? (= "component-clojure-test-v1" (:kind profile))
        input-root (if component? workspace-root repo-root)]
    (when (and component? (nil? workspace-root))
      (fail! :missing-runtime-input "component evidence requires a workspace root"))
    (let [manifest-path (:runtime-input-manifest value)
          manifest (load-manifest! input-root manifest-path component-root)
          ;; Runtime equality is deliberately only the data-read boundary.
          ;; Descriptor, manifest, and statically-derived source inputs are
          ;; independently hash-bound, but are not required to be contrived
          ;; physical reads by the focused test.
          expected (into (sorted-set) (:paths manifest))
          explicit (set (get-in value [:input-profile :explicit]))
          observed (into (sorted-set) repository-paths)
          missing (set/difference expected observed)
          extra (set/difference observed expected)]
      (when-not (set/subset? (conj expected descriptor-path manifest-path) explicit)
        (fail! :invalid-runtime-input-manifest
               "descriptor must bind its runtime manifest and every runtime-data input"
               :paths (vec (sort (set/difference
                                  (conj expected descriptor-path manifest-path)
                                  explicit)))))
      (when (seq missing)
        (fail! :undeclared-runtime-input "declared runtime inputs were not observed"
               :paths (vec (sort missing))))
      (when (seq extra)
        (fail! :missing-runtime-input "observed repository reads are absent from the manifest"
               :paths (vec (sort extra))))
      true)))

(defn with-validated-read-trace!
  "Run a physical-read boundary and validate that boundary's completed trace
  against the descriptor runtime-input manifest before returning its value."
  [{:keys [identity-root cwd-root workspace-root] :as options} thunk]
  (let [{:keys [value repository-paths]}
        (evidence-io/with-read-trace {:identity-root identity-root
                                      :cwd-root cwd-root
                                      :workspace-root workspace-root}
          thunk)]
    (assert-runtime-input-closure! (assoc options :repository-paths repository-paths))
    value))

(defn validate-runtime-input-manifest!
  "Validate the static v2 descriptor/manifest binding before executing it."
  [{:keys [repo-root workspace-root descriptor component-root]}]
  (let [{descriptor-path :path value :value} descriptor
        component? (= "component-clojure-test-v1" (get-in value [:input-profile :kind]))
        input-root (if component? workspace-root repo-root)]
    (when (and component? (nil? workspace-root))
      (fail! :missing-runtime-input "component evidence requires a workspace root"))
    (let [manifest-path (:runtime-input-manifest value)
          manifest (load-manifest! input-root manifest-path component-root)
          required (into #{descriptor-path manifest-path} (:paths manifest))
          explicit (set (get-in value [:input-profile :explicit]))]
      (when-not (set/subset? required explicit)
        (fail! :invalid-runtime-input-manifest
               "descriptor omits its manifest or a runtime-data path"
               :paths (vec (sort (set/difference required explicit)))))
      manifest)))

(def ^:private contract-basenames
  {'abc.tools.manifest/content "manifest-content.edn"
   'abc.tools.adr/validate-repository* "adr-validate-repository-star.edn"
   'abc.tools.edn-registry/call-entry-error "edn-registry-call-entry-error.edn"
   'abc.tools.edn-registry/call-duplicate-error "edn-registry-call-duplicate-error.edn"
   'abc.tools.parser-evidence/duplicate-values "parser-evidence-duplicate-errors.edn"
   'abc.tools.parallel/ordered-pmap "parallel-ordered-pmap.edn"
   'abc.tools.workflow/invoke-clock "workflow-invoke-clock.edn"
   'abc.tools.workflow/invoke-step-run "workflow-invoke-step-run.edn"})

(def ^:private forbidden-vars
  '#{clojure.core/slurp clojure.core/line-seq clojure.core/file-seq
     clojure.core/eval clojure.core/read-string clojure.core/load-string
     clojure.core/load-file clojure.core/resolve clojure.core/ns-resolve
     clojure.core/requiring-resolve
     clojure.edn/read clojure.java.io/reader clojure.java.io/input-stream
     clojure.java.shell/sh babashka.process/process babashka.process/shell})

(def ^:private code-loading-vars
  '#{clojure.core/eval clojure.core/read-string clojure.core/load-string
     clojure.core/load-file clojure.core/resolve clojure.core/ns-resolve
     clojure.core/requiring-resolve})

(def ^:private forbidden-simple
  '#{FileReader ProcessBuilder ZipFile readAllBytes readString newInputStream
     listFiles loadModel readDataset inputStream input-stream reader URL openStream
     sh process shell})

(def ^:private trusted-adapter-vars
  '#{abc.tools.evidence-io/with-read-trace
     abc.tools.evidence-io/with-ephemeral-root
     abc.tools.evidence-io/with-owned-ephemeral-root
     abc.tools.evidence-io/record-read!
     abc.tools.adr-evidence-runtime-inputs/assert-runtime-input-closure!
     abc.tools.adr-evidence-runtime-inputs/with-validated-read-trace!
     abc.tools.adr-evidence-runtime-inputs/validate-workspace-root!
     abc.tools.adr-evidence-bootstrap/git-command
     abc.tools.cli/run-cli!
     abc.tools.json/write-deterministic-json-file!
     abc.tools.files/read-text abc.tools.files/read-bytes
     abc.tools.files/input-stream abc.tools.files/reader abc.tools.files/list-files
     abc.tools.files/list-files-if-directory
     abc.tools.files/glob
     abc.tools.files/exists? abc.tools.files/directory? abc.tools.files/file?
     abc.tools.files/executable?
     abc.tools.files/read-edn abc.tools.files/read-json-lines
     abc.tools.files/with-zip-file abc.tools.files/load-jena-model
     abc.tools.files/parse-xml-document abc.tools.files/copy-file!
     abc.tools.files/create-dirs! abc.tools.files/create-parent-dirs!
     abc.tools.files/write-bytes! abc.tools.files/write-text! abc.tools.files/delete-file!
     abc.tools.files/canonicalize
     abc.tools.files/sorted-path-seq
     abc.tools.validate-design-bundle/load-turtle-graph
     abc.sim.render/init-repo!
     abc.git/load-git-repo
     abc.git/add-file! abc.git/commit! abc.git/commit-at!
     abc.git/write-blob-at!
     abc.tools.logging/log!
     abc.tools.shacl/load-shapes-graph
     abc.tools.source-bundle/open-zip-archive
     abc.tools.source-bundle/decoded-entries
     abc.tools.source-bundle/read-member!
     abc.tools.source-bundle/stage-archive!
     abc.tools.soranoha-build-publication/git-sh
     abc.tools.parallel/ordered-pmap
     abc.tools.materialize-publication/materialize-publication!
     abc.tools.soranoha-build-publication/prepare-output-root!
     abc.tools.soranoha-build-publication/promote-output-root!
     abc.tools.soranoha-build-publication/read-catalog-zip
     abc.tools.soranoha-build-publication/resolve-invocation-path
     abc.tools.soranoha-build-publication/invoke-derive-parser-ir!
     abc.tools.workflow/now-utc
     abc.tools.aozora-history-audit/prepare-owned-path!
     abc.tools.aozora-ingest/open-zip
     abc.tools.aozora-ingest/read-zip-csv
     abc.tools.malli/cached-schema
     abc.tools.json/read-json-file
     abc.tools.hash/sha256-file
     abc.tools.hash/byte-length})

(def ^:private audited-structural-leaf-vars
  '#{abc.tools.adr-evidence-bootstrap/decoded-bytes
     abc.tools.adr-evidence-bootstrap/encoded-string
     abc.tools.adr-evidence-bundle/ns-requires
     abc.tools.adr-evidence-operational/ns-declaration
     abc.tools.hash/sha256-bytes
     abc.sim.content-sim-test/no-text-zip-bytes
     abc.sim.content-sim-test/unsafe-path-zip-bytes
     abc.sim.content-sim-test/named-text-zip-bytes
     abc.sim.render/rows->csv
     abc.sim.render/content->zip-bytes
     abc.tools.source-bundle-test/write-zip!
     abc.tools.source-bundle-test/understate-first-central-size!
     abc.tools.source-snapshot-fixture/write-source-zip!
     abc.tools.jcs/canonical-json-string
     abc.tools.jcs/rfc8785-string-domain-json-bytes
     abc.tools.malli/explain-contract
     abc.tools.malli/explanation-messages
     abc.tools.path-containment/path-state
     abc.tools.schema/validation-errors
     abc.tools.workflow/validate-run
     abc.tools.linked-art/expand-document
     abc.tools.manifest-to-rdf/manifest->graph
     abc.tools.manifest-to-rdf/graph->ttl
     abc.sim.content-sim-test/ex-chain
     abc.sim.render/csv->zip-bytes
     abc.sim.render/content-sources
     abc.sim.render/model->rows
     abc.sim.oracle/expected-selection
     abc.tools.parser-maintenance-evidence/problems})

(defn- trusted-leaf-var? [var]
  (or (contains? trusted-adapter-vars var)
      (contains? audited-structural-leaf-vars var)))

(def ^:private legacy-foundation-terminal-vars
  "The exact broad terminal frontier used by foundation captures before the
  focused-v3 adapter audit. It is available only through the foundation
  compatibility entry point; none of these Vars becomes globally trusted."
  '#{abc.tools.aat-parser-ir-compat/load-registry
     abc.tools.aat-parser-ir-compat/compatible?
     abc.tools.materialize-import/materialize-import!
     abc.tools.materialize-import/parser-ir-manifest
     abc.tools.linked-art/write-publication-view!
     abc.tools.manifest-to-rdf/manifest->ttl
     abc.tools.metadata-record/record->graph
     abc.tools.metadata-record/record+persons->graph
     abc.tools.metadata-record/record+persons->ttl
     abc.tools.shacl/validate!
     abc.tools.materialize-source-snapshot/materialize-source-snapshot!
     abc.tools.source-snapshot-fixture/legacy-workset-entry!
     abc.tools.source-snapshot-fixture/workset-entry!
     abc.tools.source-bundle/inspect-zip
     abc.tools.source-bundle/write-manifest!
     abc.tools.source-snapshot-workset/write-workset!
     abc.tools.validate-design-bundle/validate-json-schemas!
     abc.tools.validate-design-bundle/validate-canonicalization!
     abc.tools.validate-design-bundle/validate-metadata-bundle!
     abc.sim.content-sim-test/overwrite-zip!
     abc.sim.content-sim-test/pin-chain-work-checks
     abc.sim.content-sim-test/run-build!
     abc.sim.content-sim-test/synthetic-state
     abc.sim.content-sim-test/unsafe-path-zip-bytes
     abc.sim.render/write-aozora-root!
     abc.tools.materialize-source-snapshot-test/workset!
     abc.tools.source-bundle-test/understate-first-central-size!
     abc.tools.source-bundle-test/write-zip!
     abc.tools.validate-design-bundle-test/aat-parser-ir-compatibility-assertions
     abc.tools.schema-validation-evidence-test/write-canonicalization-fixtures!})

(def foundation-catalog-contract
  {:path "data/adr-evidence/foundation-observation-catalog.edn"
   :sha256 "3306784360eadf21784de59da7849016a43dc1c04443755e77d535c267b08385"})

(defn- exact-foundation-focus-vars! [repo-root]
  (let [{:keys [path sha256]} foundation-catalog-contract
        state (containment/path-state repo-root path)
        file (:path state)
        actual-sha256 (when (and (= :ok (:state state))
                                 (fs/regular-file? file))
                        (hash/sha256-file file))]
    (when-not (= sha256 actual-sha256)
      (fail! :invalid-foundation-conformance-profile
             "foundation compatibility requires the immutable catalog contract"
             :path path :expected-sha256 sha256 :actual-sha256 actual-sha256))
    (into #{} (map :focus-var)
          (:focused-observations (files/read-edn file)))))

(declare raw-capability-operation traced-loader-path-index)

(def ^:private trusted-adapter-operation-counts
  '{abc.git/add-file! {addFilepattern 1}
    abc.git/commit! {setMessage 1 setAuthor 1 setCommitter 1}
    abc.git/commit-at! {PersonIdent 1 from 1 parse 1 getTimeZone 1}
    abc.git/load-git-repo {load-repo 1}
    abc.git/write-blob-at! {make-parents 1 output-stream 1 write 1}
    abc.sim.content-sim-test/ex-chain {}
    abc.sim.content-sim-test/no-text-zip-bytes
    {ByteArrayOutputStream 1 ZipEntry 1 setTime 1 ZipOutputStream 1
     putNextEntry 1 write 1 closeEntry 1 toByteArray 1}
    abc.sim.content-sim-test/unsafe-path-zip-bytes
    {ByteArrayOutputStream 1 ZipEntry 1 setTime 1 ZipOutputStream 1
     putNextEntry 1 write 1 closeEntry 1 toByteArray 1}
    abc.sim.content-sim-test/named-text-zip-bytes
    {ByteArrayOutputStream 1 ZipEntry 1 setTime 1 ZipOutputStream 1
     putNextEntry 1 write 1 closeEntry 1 toByteArray 1}
    abc.sim.oracle/expected-selection {}
    abc.sim.render/content-sources {}
    abc.sim.render/rows->csv {}
    abc.sim.render/content->zip-bytes
    {ZipEntry 1 ZipOutputStream 1 putNextEntry 1 getValue 1 setCompressedSize 1
     setMethod 1 closeEntry 1 update 1 setSize 1 setTime 1 write 1 finish 1
     ByteArrayOutputStream 1 setComment 1 setCrc 1 toByteArray 1 CRC32 1}
    abc.tools.source-bundle-test/write-zip!
    {ZipArchiveEntry 1 putArchiveEntry 1 closeArchiveEntry 1 setEncoding 1
     setCreateUnicodeExtraFields 1 addExtraField 1 getValue 1 setMethod 1
     update 1 setSize 1 setTime 1 write 1 ZipArchiveOutputStream 1
     setUseLanguageEncodingFlag 1 setComment 1 setCrc 1 CRC32 1}
    abc.tools.source-bundle-test/understate-first-central-size!
    {wrap 1 order 1 putInt 1}
    abc.tools.source-snapshot-fixture/write-source-zip!
    {ZipOutputStream 1 output-stream 1 ZipEntry 1 setTime 1
     putNextEntry 1 write 1 closeEntry 1}
    abc.sim.render/csv->zip-bytes
    {ByteArrayOutputStream 1 ZipEntry 1 setTime 1 ZipOutputStream 1
     putNextEntry 2 write 2 closeEntry 1 toByteArray 1}
    abc.sim.render/init-repo! {init 1 setDirectory 1}
    abc.sim.render/model->rows {}
    abc.tools.adr-evidence-bootstrap/decoded-bytes {getDecoder 1 decode 1}
    abc.tools.adr-evidence-bootstrap/encoded-string {getEncoder 1 encodeToString 1}
    abc.tools.adr-evidence-bootstrap/git-command {}
    abc.tools.cli/run-cli! {exit 1 parse 1}
    abc.tools.adr-evidence-bundle/ns-requires
    {LineNumberingPushbackReader 1 reader 1 read 1}
    abc.tools.adr-evidence-operational/ns-declaration {reader 1 read 1}
    abc.tools.adr-evidence-runtime-inputs/assert-runtime-input-closure! {}
    abc.tools.adr-evidence-runtime-inputs/validate-workspace-root!
    {canonicalize 3 process 1 regular-file? 1}
    abc.tools.adr-evidence-runtime-inputs/with-validated-read-trace! {}
    abc.tools.aozora-history-audit/prepare-owned-path! {exists? 1 delete-tree 1}
    abc.tools.aozora-ingest/open-zip {ZipFile 1}
    abc.tools.aozora-ingest/read-zip-csv
    {entries 1 getInputStream 1 readAllBytes 1 getLastModifiedTime 1
     toInstant 1}
    abc.tools.evidence-io/record-read! {}
    abc.tools.evidence-io/with-ephemeral-root {}
    abc.tools.evidence-io/with-owned-ephemeral-root {with-temp-dir 1}
    abc.tools.evidence-io/with-read-trace {}
    abc.tools.files/copy-file! {create-dirs 1 copy 1}
    abc.tools.files/create-dirs! {create-dirs 1}
    abc.tools.files/create-parent-dirs! {make-parents 1}
    abc.tools.files/directory? {directory? 1}
    abc.tools.files/exists? {exists? 1}
    abc.tools.files/executable? {executable? 1}
    abc.tools.files/file? {regular-file? 1}
    abc.tools.files/input-stream {input-stream 1}
    abc.tools.files/glob {glob 1}
    abc.tools.files/list-files {list-dir 1}
    abc.tools.files/list-files-if-directory {directory? 1}
    abc.tools.files/load-jena-model {loadModel 1}
    abc.tools.files/parse-xml-document
    {newInstance 1 setNamespaceAware 1 parse 1 newDocumentBuilder 1}
    abc.tools.files/read-bytes {read-all-bytes 1}
    abc.tools.files/read-edn {read-string 1 slurp 1}
    abc.tools.files/read-json-lines {slurp 1}
    abc.tools.files/read-text {slurp 1}
    abc.tools.files/reader {reader 1}
    abc.tools.files/with-zip-file {ZipFile 1}
    abc.tools.files/write-bytes! {output-stream 1 write 1}
    abc.tools.files/write-text! {spit 1}
    abc.tools.files/delete-file! {deleteIfExists 1 toPath 1}
    abc.tools.files/canonicalize {canonicalize 1}
    abc.tools.files/sorted-path-seq {}
    abc.tools.hash/byte-length {}
    abc.tools.hash/sha256-bytes {update 1 digest 1}
    abc.tools.hash/sha256-file {input-stream 1 read 1 update 1 digest 1}
    abc.tools.jcs/canonical-json-string {}
    abc.tools.jcs/rfc8785-string-domain-json-bytes {}
    abc.tools.json/read-json-file {read-json 1}
    abc.tools.json/write-deterministic-json-file!
    {getParent 1 make-parents 1 toFile 1 fromString 1 getFileName 1 toPath 2
     createTempFile 1 setPosixFilePermissions 1 move 1 write 2 writer 1
     deleteIfExists 1}
    abc.tools.linked-art/expand-document {expand 1 loader 1 base 1 get 1}
    abc.tools.logging/log! {log! 1}
    abc.tools.malli/explain-contract {}
    abc.tools.malli/explanation-messages {}
    abc.tools.malli/cached-schema {}
    abc.tools.manifest-to-rdf/graph->ttl {}
    abc.tools.manifest-to-rdf/manifest->graph {}
    abc.tools.path-containment/path-state
    {resolve 1 startsWith 2 exists? 1 toRealPath 2}
    abc.tools.schema/validation-errors {}
    abc.tools.workflow/validate-run {}
    abc.tools.shacl/load-shapes-graph {input-stream 1 read 1 getGraph 1}
    abc.tools.source-bundle/open-zip-archive
    {builder 1 setFile 1 setCharset 1 setUseUnicodeExtraFields 1 get 1}
    abc.tools.source-bundle/decoded-entries {isDirectory 1}
    abc.tools.source-bundle/read-member!
    {DigestInputStream 1 usesUTF8ForNames 1 read 1
     getGeneralPurposeBit 1 digest 1 write 1 ByteArrayOutputStream 1
     getInputStream 1 toByteArray 1}
    abc.tools.source-bundle/stage-archive!
    {createTempFile 2 toPath 2 copy 1 setReadOnly 1 toFile 2 IOException 1
     deleteIfExists 1}
    abc.tools.soranoha-build-publication/git-sh {sh 1}
    abc.tools.parallel/ordered-pmap
    {newFixedThreadPool 1 invokeAll 1 get 1 shutdown 1}
    abc.tools.materialize-publication/materialize-publication! {read-json 2}
    abc.tools.soranoha-build-publication/prepare-output-root! {exists? 1 nanoTime 1}
    abc.tools.soranoha-build-publication/promote-output-root! {exists? 1 move 1 toPath 2}
    abc.tools.soranoha-build-publication/read-catalog-zip
    {ZipFile 1 entries 1 getInputStream 1 readAllBytes 1}
    abc.tools.soranoha-build-publication/resolve-invocation-path {getenv 1}
    abc.tools.soranoha-build-publication/invoke-derive-parser-ir! {}
    abc.tools.workflow/now-utc {now 1}
    abc.tools.validate-design-bundle/load-turtle-graph {read 1}
    abc.tools.parser-maintenance-evidence/problems {}})

(def ^:private trusted-adapter-traced-loaders
  '{abc.git/load-git-repo {load-repo 0}
    abc.tools.aozora-ingest/open-zip {ZipFile 0}
    abc.tools.files/input-stream {input-stream 0}
    abc.tools.files/load-jena-model {loadModel 0}
    abc.tools.files/parse-xml-document {parse 1}
    abc.tools.files/read-bytes {read-all-bytes 0}
    abc.tools.files/read-edn {slurp 0}
    abc.tools.files/read-json-lines {slurp 0}
    abc.tools.files/read-text {slurp 0}
    abc.tools.files/reader {reader 0}
    abc.tools.files/with-zip-file {ZipFile 0}
    abc.tools.hash/sha256-file {input-stream 0}
    abc.tools.json/read-json-file {read-json 0}})

(def ^:private traced-path-wrappers
  '#{str clojure.core/str io/file clojure.java.io/file
     fs/file babashka.fs/file})

(def ^:private traced-record-read-heads
  '#{evidence-io/record-read! abc.tools.evidence-io/record-read!})

(defn- traced-path-expression? [form]
  (when (and (seq? form) (symbol? (first form)) (= 2 (count form)))
    (let [[head argument] form
          wrapper? (contains? traced-path-wrappers head)]
      (or (contains? traced-record-read-heads head)
          (and wrapper?
               (traced-path-expression? argument))))))

(defn- audit-trusted-adapter-form! [var form]
  (let [actual (->> (tree-seq coll? seq form)
                    (keep #(when (seq? %)
                             (raw-capability-operation (first %))))
                    frequencies)
        expected (get trusted-adapter-operation-counts var {})]
    (when-not (= expected actual)
      (fail! :forbidden-evidence-capability
             "trusted adapter capability inventory differs from its audited shape"
             :var var :expected expected :actual actual))
    (doseq [candidate (tree-seq coll? seq form)
            :when (seq? candidate)
            :let [path-index (traced-loader-path-index var candidate)]
            :when (some? path-index)]
      (when-not (traced-path-expression? (nth (rest candidate)
                                              path-index
                                              nil))
        (fail! :forbidden-evidence-capability
               "trusted path loader must consume its traced path"
               :var var :form (pr-str candidate)))))
  true)

(def ^:private audited-higher-order-signatures
  '{abc.tools.adr-evidence-runtime-inputs/with-validated-read-trace! #{1}
    abc.tools.evidence-io/with-read-trace #{1}
    abc.tools.evidence-io/with-ephemeral-root #{1}
    abc.tools.evidence-io/with-owned-ephemeral-root #{0}
    abc.tools.files/with-zip-file #{1}
    clojure.core/apply :first
    clojure.core/map :first
    clojure.core/mapv :first
    clojure.core/mapcat :first
    clojure.core/map-indexed :first
    clojure.core/filter :first
    clojure.core/filterv :first
    clojure.core/remove :first
    clojure.core/keep :first
    clojure.core/keep-indexed :first
    clojure.core/reduce :first
    clojure.core/reduce-kv :first
    clojure.core/group-by :first
    clojure.core/some :first
    clojure.core/every? :first
    clojure.core/not-any? :first
    clojure.core/merge-with :first
    clojure.core/complement :first
    clojure.core/partial :first
    clojure.core/comp :all
    clojure.core/juxt :all
    clojure.core/update #{2}
    clojure.core/update-in #{2}
    clojure.core/swap! #{1}
    clojure.core/sort-by {2 #{0} 3 #{0 1}}
    clojure.core/repeatedly {1 #{0} 2 #{1}}
    clojure.core/into {3 #{1}}})

(defn- callable-argument-indexes [target argument-count]
  (let [signature (get audited-higher-order-signatures target)]
    (cond
      (= :first signature) #{0}
      (= :all signature) (set (range argument-count))
      (map? signature) (get signature argument-count #{})
      :else signature)))

;; External executable Vars are capabilities, not harmless names. This is an
;; exact reviewed inventory of pure operations needed by current evidence
;; boundaries; additions require a focused positive test and review.
(def ^:private audited-safe-external-vars
  '#{clojure.core/= clojure.core/not= clojure.core/not clojure.core/< clojure.core/<=
     clojure.core/> clojure.core/>= clojure.core/+ clojure.core/- clojure.core/max
     clojure.core/* clojure.core// clojure.core/inc clojure.core/dec clojure.core/compare
     clojure.core/identity clojure.core/constantly clojure.core/if-not
     clojure.core/atom clojure.core/volatile! clojure.core/deref clojure.core/swap! clojure.core/vswap! clojure.core/ex-data
     clojure.core/ex-info clojure.core/ex-message clojure.core/instance? clojure.core/class
     clojure.core/enumeration-seq clojure.core/iterator-seq clojure.core/make-array
     clojure.core/str clojure.core/pr-str clojure.core/format
     clojure.core/println
     clojure.core/name clojure.core/namespace
     clojure.core/symbol clojure.core/keyword clojure.core/boolean
     clojure.core/int clojure.core/long
     clojure.core/key clojure.core/val
     clojure.core/count clojure.core/empty? clojure.core/seq clojure.core/first
     clojure.core/second clojure.core/ffirst clojure.core/rest clojure.core/next clojure.core/last clojure.core/nth
     clojure.core/peek clojure.core/pop
     clojure.core/get clojure.core/get-in clojure.core/find clojure.core/contains?
     clojure.core/keys clojure.core/vals clojure.core/select-keys
     clojure.core/assoc clojure.core/assoc-in clojure.core/dissoc
     clojure.core/conj clojure.core/disj
     clojure.core/merge
     clojure.core/distinct clojure.core/dedupe clojure.core/sort
     clojure.core/frequencies
     clojure.core/vector clojure.core/vec clojure.core/set clojure.core/hash-map clojure.core/zipmap
     clojure.core/sorted-map clojure.core/sorted-set clojure.core/range
     clojure.core/partition
     clojure.core/take clojure.core/drop clojure.core/take-while
     clojure.core/drop-while clojure.core/subvec
     clojure.core/take-nth clojure.core/concat clojure.core/cons clojure.core/reverse clojure.core/repeat
     clojure.core/byte-array clojure.core/unchecked-byte clojure.core/alength clojure.core/aget
     clojure.core/string? clojure.core/symbol? clojure.core/keyword? clojure.core/bytes?
     clojure.core/boolean?
     clojure.core/map? clojure.core/set? clojure.core/vector? clojure.core/seq?
     clojure.core/coll? clojure.core/integer? clojure.core/number?
     clojure.core/sequential? clojure.core/qualified-symbol?
     clojure.core/nil? clojure.core/some? clojure.core/true? clojure.core/false?
     clojure.core/zero? clojure.core/pos? clojure.core/neg?
     clojure.core/re-pattern clojure.core/re-find clojure.core/re-matches clojure.core/re-seq
     clojure.core/subs
     clojure.test/is clojure.string/includes? clojure.string/starts-with?
     clojure.string/ends-with? clojure.string/blank? clojure.string/split
     clojure.string/index-of
     clojure.string/lower-case
     clojure.string/split-lines clojure.string/replace clojure.string/replace-first
     clojure.string/join clojure.string/trim
     clojure.set/union clojure.set/difference clojure.set/intersection
     clojure.set/subset? clojure.java.io/file
     arachne.aristotle/graph
     charred.api/read-csv charred.api/read-json-str charred.api/write-json-str
     babashka.fs/absolute? babashka.fs/absolutize babashka.fs/file
     babashka.fs/file-name babashka.fs/normalize babashka.fs/parent babashka.fs/path
     babashka.fs/relativize arachne.aristotle/add})

(def ^:private audited-special-heads
  '#{fn* fn if if-not let* let loop* loop recur do throw try catch finally
     -> ->> some-> some->> cond-> cond->>
     set! monitor-enter monitor-exit case* deftype* reify*})

(def ^:private thread-first-heads '#{-> some-> cond->})
(def ^:private thread-last-heads '#{->> some->> cond->>})

(defn- thread-step-form [direction step]
  (let [parts (if (seq? step) step (list step))]
    (case direction
      :first (with-meta (list* (first parts) nil (rest parts)) (meta step))
      :last (with-meta (apply list (concat parts [nil])) (meta step)))))

(def ^:private audited-core-macros
  '#{and or when when-not if-let when-let if-some when-some cond condp case with-redefs
     doseq for dotimes letfn binding with-open lazy-seq doto assert})

(def ^:private audited-noncore-macros
  '#{clojure.test/is clojure.test/testing})

;; Exact object-pure operations only. Filesystem predicates and metadata,
;; constructors, static JVM I/O, network, and process APIs go through named
;; adapters instead.
(def ^:private audited-safe-jvm-heads
  '#{.add .availableProcessors .close .contains .deref .find .getCause .getSize .getBlankNodeLabel .getBytes .getLiteralDatatypeURI
     .getLiteralLanguage .getLiteralLexicalForm .getName .getObject
     .focusNode .getEntries .getMessage .getPredicate .getSafeTypeByName .getSchema .getScheme
     .getSubject .getURI .lastIndexOf .level .message .resultPath .severity .source
     .charAt .conforms .isAbsolute .isBefore .isBlank .length .substring .toMillis .validate
     .isLiteral .isURI})

(def ^:private audited-safe-jvm-vars
  '#{BaseDatatype. ByteArrayInputStream. String. StringWriter. java.io.StringWriter. Integer/parseInt JsonDocument/of LocalDate/parse java.time.Instant/parse java.time.Duration/between Normalizer/normalize
     Character/isHighSurrogate Character/isLowSurrogate ShaclValidator/get YearMonth/parse
     UCharacter/foldCase Runtime/getRuntime java.net.URI.
     MessageDigest/getInstance GraphUtil/addInto ModelFactory/createDefaultModel NodeFactory/createBlankNode
     NodeFactory/createLiteral NodeFactory/createURI Triple/create TypeMapper/getInstance})

(defn- forbidden-head? [head]
  (let [simple (-> (name head)
                   (str/replace #"^\." "")
                   (str/replace #"\.$" "")
                   (str/split #"\.")
                   last
                   symbol)]
    (contains? forbidden-simple simple)))

(def ^:private raw-namespace-aliases
  '#{fs io Files RDFDataMgr tel git})

(def ^:private raw-namespace-names
  #{"babashka.fs" "clojure.java.io" "java.nio.file.Files"
    "org.apache.jena.riot.RDFDataMgr"})

(def ^:private raw-simple-heads
  '#{slurp spit line-seq file-seq eval read-string load-string load-file
     resolve ns-resolve requiring-resolve sh shell process
     read-all-bytes readAllBytes readString newInputStream input-stream inputStream
     reader FileReader ZipFile listFiles list-dir directory? regular-file? exists?
     loadModel loadGraph readDataset read read-json openStream URL parse
     create-dirs delete-tree copy output-stream make-parents
     load-repo})

(defn- head-simple [head]
  (-> (name head)
      (str/replace #"^\." "")
      (str/replace #"\.$" "")
      (str/split #"\.")
      last
      symbol))

(defn- jvm-invocation-head? [head]
  (or (= 'new head)
      (= '. head)
      (str/starts-with? (name head) ".")
      (str/ends-with? (name head) ".")
      (and (namespace head)
           (re-matches #"[A-Z].*" (last (str/split (namespace head) #"\."))))))

(defn- raw-capability-operation [head]
  (when (symbol? head)
    (let [simple (head-simple head)
          alias (some-> (namespace head) symbol)]
      (when (or (contains? raw-simple-heads simple)
                (contains? forbidden-vars head)
                (and (or (contains? raw-namespace-aliases alias)
                         (contains? raw-namespace-names (namespace head)))
                     (not (contains? '#{file path parent normalize relativize file-name
                                        absolute? absolutize}
                                     simple)))
                (and (jvm-invocation-head? head)
                     (not (contains? audited-safe-jvm-heads head))
                     (not (contains? audited-safe-jvm-vars head))))
        simple))))

(defn- traced-loader-path-index [var form]
  (get-in trusted-adapter-traced-loaders
          [var (raw-capability-operation (first form))]))

(defn- kondo-analysis! [repo-root]
  (let [config (pr-str {:output {:format :edn}
                        :analysis {:var-definitions true :var-usages true
                                   :locals true :local-usages true}})
        {:keys [exit out]} @(process/process
                             ["clj-kondo" "--cache" "false" "--fail-level" "error"
                              "--lint" "src" "test" "--config" config]
                             {:dir (str repo-root) :out :string :err :out})
        output out]
    (when-not (zero? exit)
      (fail! :invalid-focused-evidence-analysis "clj-kondo analysis failed" :output output))
    (edn/read-string output)))

(defn- ns-require-aliases [form]
  (if (and (seq? form) (= 'ns (first form)))
    (->> (drop 2 form)
         (filter #(and (seq? %) (= :require (first %))))
         (mapcat rest)
         (keep (fn [libspec]
                 (when (vector? libspec)
                   (let [options (apply hash-map (rest libspec))]
                     (when-let [alias (:as options)]
                       [alias (first libspec)])))))
         (into {}))
    {}))

(defn read-source-forms! [file]
  (with-open [r (files/reader file)]
    (let [r (reader-types/indexing-push-back-reader r)]
      (binding [reader/*read-eval* false]
        (let [options {:eof ::eof :read-cond :allow :features #{:clj}}
              first-form (reader/read options r)]
          (if (= ::eof first-form)
            []
            (binding [reader/*alias-map* (ns-require-aliases first-form)]
              (loop [forms [first-form]]
                (let [form (reader/read options r)]
                  (if (= ::eof form)
                    forms
                    (recur (conj forms form))))))))))))

(defn- qvar [m] (symbol (str (:ns m)) (str (:name m))))
(defn- concrete-definition? [definition]
  (not= 'clojure.core/declare (:defined-by definition)))
(defn- span [x] [(:line (meta x)) (:column (meta x))])
(defn- usage-span [x] [(:name-row x) (:name-col x)])

(defn- defn-form [forms name]
  (some (fn [form]
          (when (and (seq? form) (#{'defn 'defn- 'deftest} (first form))
                     (= name (second form)))
            form))
        (tree-seq coll? seq forms)))

(def ^:private audited-static-code-loading-edges
  '{abc.tools.malli/cached-schema-hash
    abc.tools.manifest/schema-hash})

(defn- audit-static-code-loading-edge! [var form]
  (let [expected (get audited-static-code-loading-edges var)
        capabilities (->> (tree-seq coll? seq form)
                          (keep #(when (seq? %)
                                   (raw-capability-operation (first %))))
                          frequencies)
        actual (->> (tree-seq coll? seq form)
                    (keep (fn [candidate]
                            (when (and (seq? candidate)
                                       (= 'requiring-resolve (first candidate)))
                              (let [[_ quoted & extra] candidate]
                                (when-not (and (empty? extra)
                                               (seq? quoted)
                                               (= 'quote (first quoted))
                                               (= 2 (count quoted))
                                               (qualified-symbol? (second quoted)))
                                  (fail! :forbidden-evidence-capability
                                         "static code-loading edge is malformed"
                                         :var var :form (pr-str candidate)))
                                (second quoted)))))
                    vec)]
    (when-not (= {'requiring-resolve 1} capabilities)
      (fail! :forbidden-evidence-capability
             "static code-loading bridge contains another raw capability"
             :var var :actual capabilities))
    (when-not (= [expected] actual)
      (fail! :forbidden-evidence-capability
             "static code-loading edge differs from its exact audited target"
             :var var :expected expected :actual actual))
    expected))

(defn validate-focused-deftests!
  "Require each v2 focus to resolve uniquely to a deftest source form."
  [repo-root focused-vars]
  (let [repo-root (fs/file (fs/canonicalize repo-root))
        definitions (group-by qvar (filter concrete-definition?
                                           (get-in (kondo-analysis! repo-root)
                                                   [:analysis :var-definitions])))
        forms (memoize #(read-source-forms! (fs/file repo-root %)))]
    (doseq [var focused-vars]
      (let [items (get definitions var)]
        (when-not (= 1 (count items))
          (fail! (if (seq items) :duplicate-var-definition :unresolved-focused-var)
                 "focused v2 Var must resolve exactly once"
                 :var var))
        (let [definition (first items)
              form (defn-form (forms (:filename definition)) (:name definition))]
          (when-not (= 'deftest (first form))
            (fail! :focused-var-not-deftest
                   "focused v2 Var must resolve to a deftest source form"
                   :var var :actual-form (first form))))))
    true))

(defn- parse-defn [form]
  (if (= 'deftest (first form))
    [{:params [] :body (drop 2 form)}]
    (let [[_ _ & tail] form
          tail (cond-> tail (string? (first tail)) rest (map? (first tail)) rest)
          arities (if (vector? (first tail)) [(cons (first tail) (rest tail))] tail)]
      (when-not (and (seq arities)
                     (every? #(vector? (first %)) arities)
                     (or (= 1 (count arities))
                         (and (every? #(not-any? #{'&} (first %)) arities)
                              (= (count arities)
                                 (count (distinct (map #(count (first %)) arities)))))))
        (fail! :unsupported-evidence-call-graph
               "defn must have distinct finite fixed arities"
               :form (pr-str form)))
      (mapv (fn [arity]
              {:params (first arity) :body (rest arity)})
            arities))))

(defn- parse-literal-fn [form]
  (let [[_ & tail] form
        tail (if (symbol? (first tail)) (rest tail) tail)
        arities (if (vector? (first tail)) [(cons (first tail) (rest tail))] tail)]
    (when-not (and (= 1 (count arities))
                   (vector? (first (first arities))))
      (fail! :unsupported-evidence-call-graph
             "higher-order literal function must have exactly one arity"
             :form (pr-str form)))
    {:params (first (first arities)) :body (rest (first arities))}))

(defn- binding-symbols [binding-form]
  (->> (tree-seq coll? seq binding-form)
       (filter symbol?)
       (remove '#{&})
       set))

(defn- contract-path [caller]
  (when-let [basename (get contract-basenames caller)]
    (str "data/evidence-higher-order-calls/" basename)))

(defn- load-contract! [repo-root caller]
  (let [path (contract-path caller)]
    (when-not path
      (fail! :unregistered-higher-order-call "function parameter invocation has no contract"
             :caller caller))
    (when-not (= :ok (:state (containment/path-state repo-root path)))
      (fail! :invalid-higher-order-contract "higher-order contract is missing"
             :caller caller :path path))
    (let [value (try (files/read-edn (fs/file repo-root path))
                     (catch Exception e
                       (fail! :invalid-higher-order-contract "higher-order contract is unreadable"
                              :caller caller :path path :detail (.getMessage e))))]
      (when-not (and (= #{:schema-version :caller :parameters} (set (keys value)))
                     (= :abc-evidence-higher-order-call-v1 (:schema-version value))
                     (= caller (:caller value))
                     (map? (:parameters value))
                     (every? symbol? (keys (:parameters value)))
                     (every? #(and (vector? %) (seq %) (= (count %) (count (distinct %)))
                                   (every? qualified-symbol? %))
                             (vals (:parameters value))))
        (fail! :invalid-higher-order-contract "higher-order contract is malformed"
               :caller caller :path path))
      [path value])))

(defn- analyze-reachable-vars*
  [repo-root focused-vars additional-terminal-vars]
  (let [repo-root (fs/file (fs/canonicalize repo-root))
        kondo (kondo-analysis! repo-root)
        analysis (:analysis kondo)
        definitions-grouped (group-by qvar (filter concrete-definition?
                                                   (:var-definitions analysis)))
        definitions (into {} (map (fn [[var items]] [var (first items)])) definitions-grouped)
        defined-namespaces (set (keep namespace (keys definitions)))
        usages (group-by (juxt :filename :from-var) (:var-usages analysis))
        locals (group-by :filename (:local-usages analysis))
        findings (group-by :filename (:findings kondo))
        forms-cache (atom {})
        consulted (atom (sorted-set))
        reachable (atom (sorted-set))
        resolved-calls (atom (sorted-set))
        direct-body-calls (atom {})]
    (letfn [(definition! [var]
              (let [items (get definitions-grouped var)]
                (cond
                  (nil? items)
                  (fail! :unresolved-focused-var "focused or target Var does not resolve" :var var)
                  (not= 1 (count items))
                  (fail! :duplicate-var-definition "reachable Var has duplicate definitions" :var var)
                  :else (first items))))
            (forms! [filename]
              (or (get @forms-cache filename)
                  (let [forms (read-source-forms! (fs/file repo-root filename))]
                    (swap! forms-cache assoc filename forms) forms)))
            (audit-trusted-adapter! [var]
              (when (trusted-leaf-var? var)
                (let [definition (definition! var)
                      form (defn-form (forms! (:filename definition))
                             (:name definition))]
                  (when-not form
                    (fail! :unsupported-evidence-call-graph
                           "trusted adapter is not a direct defn" :var var))
                  (audit-trusted-adapter-form! var form))))
            (direct-body-call-target [definition form]
              (when (and (seq? form) (symbol? (first form)))
                (let [head (first form)
                      matches (filter #(= (span head) (usage-span %))
                                      (get usages [(:filename definition)
                                                   (:name definition)]))]
                  (when (= 1 (count matches))
                    (let [usage (first matches)]
                      (symbol (str (:to usage)) (str (:name usage))))))))
            (admit-callable-target! [caller target pending]
              (swap! resolved-calls conj target)
              (cond
                (or (contains? forbidden-vars target)
                    (forbidden-head? target))
                (fail! (if (contains? code-loading-vars target)
                         :forbidden-evidence-capability
                         :forbidden-evidence-io)
                       "higher-order target is a forbidden capability"
                       :caller caller :target target)

                (contains? additional-terminal-vars target)
                (swap! reachable conj target)

                (trusted-leaf-var? target)
                (do (audit-trusted-adapter! target)
                    (swap! reachable conj target))

                (contains? definitions target)
                (let [target-definition (definition! target)]
                  (when (:macro target-definition)
                    (fail! :unsupported-evidence-call-graph
                           "higher-order target cannot be a macro"
                           :caller caller :target target))
                  (swap! pending conj target))

                (contains? audited-safe-external-vars target) nil

                (contains? defined-namespaces (namespace target))
                (definition! target)

                :else
                (fail! :forbidden-evidence-capability
                       "higher-order target is not an exact reviewed executable Var"
                       :caller caller :target target)))
            (resolve-callable-argument! [caller definition params argument pending]
              (cond
                (or (keyword? argument) (set? argument) (map? argument)) nil

                (and (seq? argument) (#{'fn 'fn*} (first argument)))
                (let [{literal-params :params body :body} (parse-literal-fn argument)
                      lambda-params (binding-symbols literal-params)]
                  (doseq [body-form body]
                    (walk! caller definition (set/union params lambda-params)
                           body-form pending)))

                (not (symbol? argument))
                (fail! :unsupported-evidence-call-graph
                       "higher-order callable must be a direct symbol or literal function"
                       :caller caller :form (pr-str argument))

                (contains? params argument)
                (let [[path contract] (load-contract! repo-root caller)
                      targets (get-in contract [:parameters argument])]
                  (when-not targets
                    (fail! :invalid-higher-order-contract
                           "callable parameter is absent from its caller contract"
                           :caller caller :parameter argument))
                  (when-not (some #(= (span argument) (usage-span %))
                                  (get locals (:filename definition)))
                    (fail! :reader-kondo-span-mismatch
                           "callable parameter does not match clj-kondo local usage"
                           :caller caller :parameter argument))
                  (swap! consulted conj path)
                  (doseq [target targets]
                    (admit-callable-target! caller target pending)))

                :else
                (let [matches (filter #(= (span argument) (usage-span %))
                                      (get usages [(:filename definition)
                                                   (:name definition)]))]
                  (when-not (= 1 (count matches))
                    (fail! :unsupported-evidence-call-graph
                           "higher-order callable must resolve to exactly one Var"
                           :caller caller :form (pr-str argument)
                           :matches (count matches)))
                  (let [usage (first matches)
                        target (symbol (str (:to usage)) (str (:name usage)))]
                    (admit-callable-target! caller target pending)))))
            (validate-higher-order-call! [caller definition params target form pending]
              (let [arguments (vec (rest form))]
                (doseq [index (callable-argument-indexes target (count arguments))]
                  (when (>= index (count arguments))
                    (fail! :unsupported-evidence-call-graph
                           "higher-order call omits its callable argument"
                           :caller caller :target target))
                  (resolve-callable-argument! caller definition params
                                              (nth arguments index) pending))))
            (walk! [caller definition params form pending]
              (cond
                (seq? form)
                (let [head (first form)]
                  (when-not (or (symbol? head) (keyword? head) (set? head) (map? head))
                    (fail! :unsupported-evidence-call-graph "computed invocation is forbidden"
                           :caller caller :form (pr-str form)))
                  (cond
                    (#{'quote 'var} head) nil
                    (or (keyword? head) (set? head) (map? head)) nil
                    (contains? audited-special-heads head)
                    (when (or (contains? thread-first-heads head)
                              (contains? thread-last-heads head))
                      (walk! caller definition params (second form) pending)
                      (let [conditional? (#{'cond-> 'cond->>} head)
                            steps (if conditional?
                                    (take-nth 2 (drop 3 form))
                                    (drop 2 form))
                            conditions (when conditional?
                                         (take-nth 2 (drop 2 form)))
                            direction (if (contains? thread-first-heads head)
                                        :first
                                        :last)]
                        (doseq [condition conditions]
                          (walk! caller definition params condition pending))
                        (doseq [step steps]
                          (walk! caller definition params
                                 (thread-step-form direction step) pending))))
                    (or (= 'new head)
                        (= '. head)
                        (str/starts-with? (name head) ".")
                        (str/ends-with? (name head) ".")
                        (and (namespace head)
                             (re-matches #"[A-Z].*" (last (str/split (namespace head) #"\.")))))
                    (when-not (or (contains? audited-safe-jvm-heads head)
                                  (contains? audited-safe-jvm-vars head))
                      (fail! :forbidden-evidence-io
                             "reachable JVM invocation is outside a named traced adapter"
                             :caller caller :target head))
                    (forbidden-head? head)
                    (fail! :forbidden-evidence-io "reachable raw I/O, network, or process call"
                           :caller caller :target head)
                    (contains? params head)
                    (let [[path contract] (load-contract! repo-root caller)
                          targets (get-in contract [:parameters head])]
                      (when-not targets
                        (fail! :invalid-higher-order-contract "invoked parameter is absent from contract"
                               :caller caller :parameter head))
                      (when-not (some #(= (span head) (usage-span %))
                                      (get locals (:filename definition)))
                        (fail! :reader-kondo-span-mismatch "parameter head does not match clj-kondo local usage"
                               :caller caller :parameter head))
                      (swap! consulted conj path)
                      (doseq [target targets]
                        (admit-callable-target! caller target pending)))
                    :else
                    (let [matches (filter #(= (span head) (usage-span %))
                                          (get usages [(:filename definition) (:name definition)]))]
                      (when-not (= 1 (count matches))
                        (fail! :reader-kondo-span-mismatch "list head must match exactly one resolved Var"
                               :caller caller :head head :matches (count matches)))
                      (let [usage (first matches)
                            target (symbol (str (:to usage)) (str (:name usage)))]
                        (swap! resolved-calls conj target)
                        (when (and (:macro usage)
                                   (not (or (and (= 'clojure.core (:to usage))
                                                 (contains? audited-core-macros (:name usage)))
                                            (contains? audited-noncore-macros target))))
                          (fail! :unsupported-evidence-call-graph
                                 "reachable macro is outside the audited subset"
                                 :caller caller :target target))
                        (when (or (contains? forbidden-vars target)
                                  (forbidden-head? (:name usage)))
                          (fail! (if (contains? code-loading-vars target)
                                   :forbidden-evidence-capability
                                   :forbidden-evidence-io)
                                 "reachable code-loading, I/O, network, or process capability"
                                 :caller caller :target target))
                        (when (contains? audited-higher-order-signatures target)
                          (validate-higher-order-call! caller definition params
                                                       target form pending))
                        (cond
                          (contains? additional-terminal-vars target)
                          (swap! reachable conj target)

                          (trusted-leaf-var? target)
                          (do (audit-trusted-adapter! target)
                              (swap! reachable conj target))
                          (contains? definitions target)
                          (do
                            (when (and (:macro (get definitions target))
                                       (not= 'clojure.core (:to usage)))
                              (fail! :unsupported-evidence-call-graph
                                     "user macro is forbidden" :target target))
                            (swap! pending conj target))

                          (or (:macro usage)
                              (contains? audited-safe-external-vars target)
                              (contains? audited-higher-order-signatures target)) nil

                          :else
                          (fail! :forbidden-evidence-capability
                                 "resolved external executable Var is not in the reviewed pure allowlist"
                                 :caller caller :target target)))))
                  ;; A forbidden Var passed as data is still an executable capability
                  ;; (for example `(apply slurp args)`). Resolve every symbol argument
                  ;; at its reader span and reject it before descending.
                  (doseq [argument (rest form)
                          :when (symbol? argument)
                          usage (filter #(= (span argument) (usage-span %))
                                        (get usages [(:filename definition) (:name definition)]))
                          :let [target (symbol (str (:to usage)) (str (:name usage)))]]
                    (cond
                      (contains? forbidden-vars target)
                      (fail! (if (contains? code-loading-vars target)
                               :forbidden-evidence-capability
                               :forbidden-evidence-io)
                             "raw capability Var passed as a higher-order argument"
                             :caller caller :target target)

                      (and (contains? definitions target)
                           (let [target-definition (get definitions target)]
                             (and (not (:macro target-definition))
                                  (or (:fixed-arities target-definition)
                                      (:varargs-min-arity target-definition))))
                           (not (or (contains? additional-terminal-vars target)
                                    (trusted-leaf-var? target))))
                      ;; A directly resolved Var is already a finite target: add
                      ;; its graph. Only function-valued parameters need the
                      ;; caller-scoped contract handled above.
                      (swap! pending conj target)))
                  (when-not (or (contains? thread-first-heads head)
                                (contains? thread-last-heads head))
                    (doseq [x (rest form)]
                      (walk! caller definition params x pending))))
                (coll? form) (doseq [x form] (walk! caller definition params x pending))))
            (visit! [var pending]
              (when-not (contains? @reachable var)
                (let [definition (definition! var)
                      filename (:filename definition)
                      form (defn-form (forms! filename) (:name definition))]
                  (when-not form
                    (fail! :unsupported-evidence-call-graph "reachable Var is not a direct defn" :var var))
                  (when (some #(and (<= (:row definition) (:row %))
                                    (<= (:row %) (:end-row definition))
                                    (= :error (:level %)))
                              (get findings filename))
                    (fail! :unresolved-call-edge "clj-kondo found an error in reachable Var" :var var))
                  (swap! reachable conj var)
                  (if (contains? audited-static-code-loading-edges var)
                    (let [target (audit-static-code-loading-edge! var form)]
                      (swap! resolved-calls conj target)
                      (swap! pending conj target))
                    (let [arities (parse-defn form)
                          body (mapcat :body arities)]
                      (swap! direct-body-calls assoc var
                             (into (sorted-set)
                                   (keep #(direct-body-call-target definition %) body)))
                      (doseq [{:keys [params body]} arities
                              x body]
                        (walk! var definition (set (filter symbol? params)) x pending)))))))]
      (let [pending (atom (into (sorted-set) focused-vars))]
        (loop []
          (when-let [var (first @pending)]
            (swap! pending disj var)
            (visit! var pending)
            (recur))))
      {:focused-vars (vec (sort focused-vars))
       :reachable-vars (vec @reachable)
       :resolved-call-vars (vec @resolved-calls)
       :direct-body-call-vars @direct-body-calls
       :paths (->> @reachable (map #(-> definitions (get %) :filename)) distinct sort vec)
       :contract-paths (vec @consulted)})))

(defn analyze-reachable-vars
  "Return the exact call graph admitted by the evidence closed subset. clj-kondo
  resolves Vars; tools.reader identifies list-head and defn-parameter spans."
  [repo-root focused-vars]
  (analyze-reachable-vars* repo-root focused-vars #{}))

(defn analyze-foundation-reachable-vars
  "Reproduce the frozen pre-focused-v3 terminal frontier for the immutable
  foundation catalog. Callers must first validate that exact catalog contract."
  [repo-root focused-vars]
  (let [admitted (exact-foundation-focus-vars! repo-root)]
    (when-not (and (seq focused-vars)
                   (set/subset? (set focused-vars) admitted))
      (fail! :invalid-foundation-conformance-profile
             "foundation compatibility accepts only immutable foundation focuses"
             :focused-vars (vec (sort focused-vars))))
    (analyze-reachable-vars* repo-root focused-vars
                             legacy-foundation-terminal-vars)))

(def ^:private required-v2-boundary-vars
  '#{abc.tools.adr-evidence-runtime-inputs/with-validated-read-trace!})

(defn assert-v2-boundary-ownership!
  "Require every focused v2 wrapper to own a direct unconditional boundary."
  [{:keys [focused-vars direct-body-call-vars] :as analysis}]
  (let [missing (->> focused-vars
                     (remove #(set/subset? required-v2-boundary-vars
                                           (get direct-body-call-vars % #{})))
                     vec)]
    (when (seq missing)
      (fail! :missing-evidence-boundary-owner
             "each v2 focus must directly call the trace-and-validate owner"
             :vars missing))
    analysis))
