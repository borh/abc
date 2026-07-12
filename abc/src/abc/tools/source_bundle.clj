(ns abc.tools.source-bundle
  (:require [abc.tools.files :as files]
            [abc.tools.hash :as hash]
            [abc.tools.jcs :as jcs]
            [abc.tools.json :as json]
            [clojure.java.io :as io]
            [clojure.string :as string])
  (:import [com.ibm.icu.lang UCharacter]
           [java.io ByteArrayOutputStream FileNotFoundException IOException
            InterruptedIOException]
           [java.nio ByteBuffer]
           [java.nio.channels ClosedByInterruptException]
           [java.nio.charset Charset CharacterCodingException CodingErrorAction
            StandardCharsets]
           [java.nio.file FileSystemException Files StandardCopyOption]
           [java.security DigestInputStream MessageDigest]
           [java.text Normalizer Normalizer$Form]
           [org.apache.commons.compress.archivers.zip
            UnicodePathExtraField ZipArchiveEntry ZipArchiveEntry$NameSource
            ZipFile]))

(def construction "abc-source-bundle-v1")
(def schema-id "https://w3id.org/abc/schemas/source-bundle.schema.json")
(def bundle-hash-algorithm "sha256-rfc8785-jcs-abc-source-bundle-v1")
(def default-limits {:max-members 1024
                     :max-member-bytes 16777216
                     :max-total-bytes 33554432})
(def ^:private legacy-name-charset (Charset/forName "windows-31j"))

(defn bundle-identity-canonical-bytes
  "Canonical UTF-8 bytes for the string-only abc-source-bundle-v1 identity."
  [identity-object]
  (jcs/rfc8785-string-domain-json-bytes identity-object))

(defn bundle-identity-hash [identity-object]
  (hash/format-sha256
   (hash/sha256-bytes (bundle-identity-canonical-bytes identity-object))))

(defn- fail! [reason archive-path data]
  (throw (ex-info (str "source bundle admission failed: " (name reason))
                  (merge {::admission-error true
                          :reason reason
                          :archive-path (str archive-path)}
                         data))))

(defn- unicode-fold [s]
  (UCharacter/foldCase ^String s true))

(defn- strict-decode [charset raw]
  (str (.decode (doto (.newDecoder ^Charset charset)
                  (.onMalformedInput CodingErrorAction/REPORT)
                  (.onUnmappableCharacter CodingErrorAction/REPORT))
                (ByteBuffer/wrap raw))))

(defn- decoded-entry-name [archive-path ^ZipArchiveEntry entry]
  (try
    (let [source (.getNameSource entry)]
      (cond
        (= source ZipArchiveEntry$NameSource/UNICODE_EXTRA_FIELD)
        (let [unicode-path (cast UnicodePathExtraField
                                 (.getExtraField
                                  entry UnicodePathExtraField/UPATH_ID))]
          (when-not unicode-path
            (throw (IllegalArgumentException.
                    "Unicode Path name source has no Unicode Path field")))
          (strict-decode StandardCharsets/UTF_8
                         (.getUnicodeName unicode-path)))

        (= source ZipArchiveEntry$NameSource/NAME_WITH_EFS_FLAG)
        (strict-decode StandardCharsets/UTF_8 (.getRawName entry))

        (= source ZipArchiveEntry$NameSource/NAME)
        (strict-decode legacy-name-charset (.getRawName entry))

        :else
        (throw (IllegalArgumentException.
                (str "unknown ZIP name source: " source)))))
    (catch CharacterCodingException t
      (fail! :invalid-member-name-encoding archive-path
             {:name-source (str (.getNameSource entry))
              :cause (.getMessage t)}))))

(defn- normalize-member-path [archive-path decoded]
  (let [nfc (Normalizer/normalize (string/replace decoded "\\" "/")
                                  Normalizer$Form/NFC)
        segments (string/split nfc #"/" -1)]
    (when (or (string/starts-with? nfc "/")
              (re-find #"^[A-Za-z]:" nfc)
              (some #{"" "." ".."} segments))
      (fail! :unsafe-member-path archive-path
             {:decoded-path decoded :normalized-path nfc}))
    nfc))

(defn- packaging-metadata? [path]
  (or (string/starts-with? path "__MACOSX/")
      (string/starts-with? (last (string/split path #"/")) "._")))

(defn- primary-candidate? [path]
  (and (string/ends-with? (string/lower-case path) ".txt")
       (not (packaging-metadata? path))))

(defn- identity-fail! [reason data]
  (throw (ex-info (str "invalid abc-source-bundle-v1 identity: " (name reason))
                  (merge {::identity-error true :reason reason} data))))

(defn- identity-path-valid? [path]
  (and (string? path)
       (not (string/blank? path))
       (= path (Normalizer/normalize path Normalizer$Form/NFC))
       (not (string/includes? path "\\"))
       (not (string/starts-with? path "/"))
       (not (re-find #"^[A-Za-z]:" path))
       (not-any? #{"" "." ".."} (string/split path #"/" -1))))

(defn validate-identity-object!
  "Validate the complete structural policy of an abc-source-bundle-v1
  persisted identity and return it unchanged. This validation is independent
  of JSON Schema so callers cannot re-sign a structurally non-canonical member
  projection."
  [identity-object]
  (when-not (and (map? identity-object)
                 (= #{"construction" "members" "primary_text_member"}
                    (set (keys identity-object)))
                 (vector? (get identity-object "members"))
                 (string? (get identity-object "primary_text_member")))
    (identity-fail! :identity-fields {}))
  (when-not (= construction (get identity-object "construction"))
    (identity-fail! :identity-construction
                    {:construction (get identity-object "construction")}))
  (let [members (get identity-object "members")]
    (doseq [[index member] (map-indexed vector members)]
      (when-not (and (map? member)
                     (= #{"path" "member_hash"} (set (keys member)))
                     (string? (get member "path")))
        (identity-fail! :identity-member-fields {:member-index index}))
      (when-not (and (string? (get member "member_hash"))
                     (re-matches hash/hash-pattern
                                 (get member "member_hash")))
        (identity-fail! :identity-member-hash {:member-index index}))
      (when-not (identity-path-valid? (get member "path"))
        (identity-fail! :identity-member-path
                        {:member-index index :path (get member "path")})))
    (let [paths (mapv #(get % "path") members)
          sorted-paths (vec (sort paths))]
      (when-not (= paths sorted-paths)
        (identity-fail! :identity-member-order
                        {:paths paths :expected-paths sorted-paths}))
      (when-not (= (count paths) (count (distinct paths)))
        (identity-fail! :identity-member-path-collision {:paths paths}))
      (let [folded (mapv unicode-fold paths)]
        (when-not (= (count folded) (count (distinct folded)))
          (identity-fail! :identity-member-case-fold-collision
                          {:paths paths})))
      (let [candidates (filterv primary-candidate? paths)
            primary (get identity-object "primary_text_member")]
        (when-not (= 1 (count candidates))
          (identity-fail! :identity-primary-cardinality
                          {:candidates candidates}))
        (when-not (= primary (first candidates))
          (identity-fail! :identity-primary-match
                          {:primary-text-member primary
                           :expected-primary-text-member
                           (first candidates)})))))
  identity-object)

(defn validate-persisted-manifest!
  "Validate a persisted source-bundle manifest's authoritative identity,
  member projection, and signature. Return the manifest unchanged."
  [manifest-value]
  (let [identity-object (validate-identity-object!
                         (get manifest-value "identity_object"))
        authoritative-projection
        (mapv #(select-keys % ["path" "member_hash"])
              (get manifest-value "members"))]
    (when-not (= (get identity-object "members") authoritative-projection)
      (identity-fail! :identity-member-projection
                      {:identity-members (get identity-object "members")
                       :member-projection authoritative-projection}))
    (let [expected (bundle-identity-hash identity-object)]
      (when-not (= expected (get manifest-value "bundle_hash"))
        (identity-fail! :identity-bundle-hash
                        {:bundle-hash (get manifest-value "bundle_hash")
                         :expected-bundle-hash expected})))
    manifest-value))

(defn- name-source [^ZipArchiveEntry entry]
  (let [source (.getNameSource entry)]
    (cond
      (= source ZipArchiveEntry$NameSource/UNICODE_EXTRA_FIELD) "unicode-extra"
      (= source ZipArchiveEntry$NameSource/NAME_WITH_EFS_FLAG) "efs-utf8"
      (= source ZipArchiveEntry$NameSource/NAME) "windows-31j"
      :else (throw (IllegalArgumentException.
                    (str "unknown ZIP name source: " source))))))

(defn- validate-declared-limits!
  [archive-path entries {:keys [max-members max-member-bytes max-total-bytes]}]
  (when (> (count entries) max-members)
    (fail! :too-many-members archive-path
           {:member-count (count entries) :limit max-members}))
  (doseq [{:keys [path entry]} entries
          :let [size (.getSize ^ZipArchiveEntry entry)]]
    (when (and (not (neg? size)) (> size max-member-bytes))
      (fail! :member-too-large archive-path
             {:path path :declared-bytes size :limit max-member-bytes})))
  (let [known-sizes (map #(.getSize ^ZipArchiveEntry (:entry %)) entries)]
    (when (and (every? #(not (neg? %)) known-sizes)
               (> (reduce + known-sizes) max-total-bytes))
      (fail! :total-too-large archive-path
             {:declared-bytes (reduce + known-sizes)
              :limit max-total-bytes}))))

(defn- decoded-entries [archive-path archive]
  (->> (enumeration-seq (.getEntries ^ZipFile archive))
       (remove #(.isDirectory ^ZipArchiveEntry %))
       (mapv (fn [entry]
               (let [decoded (decoded-entry-name archive-path entry)]
                 {:entry entry
                  :decoded-path decoded
                  :path (normalize-member-path archive-path decoded)})))))

(defn- collision-evidence [entries]
  (let [by-path (group-by :path entries)
        nfc-collision? (boolean (some #(> (count %) 1) (vals by-path)))
        by-fold (group-by #(unicode-fold (:path %)) entries)
        case-collision? (boolean
                         (some #(> (count (distinct (map :path %))) 1)
                               (vals by-fold)))]
    {:nfc-collision? nfc-collision?
     :unicode-case-collision? case-collision?}))

(defn- validate-entry-collisions! [archive-path entries]
  (let [by-path (group-by :path entries)]
    (when-let [[path duplicates]
               (first (sort-by key (filter #(> (count (val %)) 1) by-path)))]
      (fail! :duplicate-member-path archive-path
             {:path path :member-count (count duplicates)}))
    (let [by-fold (group-by #(unicode-fold (:path %)) entries)]
      (when-let [[folded collisions]
                 (first (sort-by key
                                 (filter #(> (count (val %)) 1) by-fold)))]
        (fail! :case-fold-member-path-collision archive-path
               {:folded-path folded
                :paths (->> collisions (map :path) sort vec)})))
    entries))

(defn- choose-primary! [archive-path entries]
  (let [candidates (filterv #(primary-candidate? (:path %)) entries)]
    (case (count candidates)
      0 (fail! :no-primary-text-member archive-path {:candidates []})
      1 (:path (first candidates))
      (fail! :multiple-primary-text-members archive-path
             {:candidates (mapv :path candidates)}))))

(defn- read-member!
  [archive-path archive {:keys [entry path decoded-path]} primary-path
   total-bytes {:keys [max-member-bytes max-total-bytes]}]
  (let [digest (MessageDigest/getInstance "SHA-256")
        primary? (= path primary-path)
        retained (when primary? (ByteArrayOutputStream.))
        buffer (byte-array 8192)]
    (with-open [input (DigestInputStream. (.getInputStream ^ZipFile archive entry)
                                          digest)]
      (loop [member-bytes 0]
        (let [n (.read input buffer)]
          (if (neg? n)
            {:metadata {"path" path
                        "decoded_path" decoded-path
                        "name_source" (name-source entry)
                        "byte_length" member-bytes
                        "member_hash" (hash/format-sha256
                                       (hash/bytes->hex (.digest digest)))}
             :primary-bytes (when primary? (.toByteArray retained))}
            (let [next-member (+ member-bytes n)
                  next-total (+ @total-bytes n)]
              (when (> next-member max-member-bytes)
                (fail! :member-too-large archive-path
                       {:path path :actual-bytes next-member
                        :limit max-member-bytes}))
              (when (> next-total max-total-bytes)
                (fail! :total-too-large archive-path
                       {:path path :actual-bytes next-total
                        :limit max-total-bytes}))
              (vreset! total-bytes next-total)
              (when primary?
                (.write retained buffer 0 n))
              (recur next-member))))))))

(defn- caused-by? [class throwable]
  (loop [cause throwable]
    (cond
      (nil? cause) false
      (instance? class cause) true
      :else (recur (.getCause cause)))))

(defn- parser-boundary-io? [t]
  (and (instance? IOException t)
       (not (caused-by? InterruptedIOException t))
       (not (caused-by? ClosedByInterruptException t))
       (not (caused-by? FileNotFoundException t))
       (not (caused-by? FileSystemException t))))

(defn- unreadable-zip! [archive-path stable-file t]
  (let [message (.getMessage ^Throwable t)
        cause (if (and message stable-file)
                (string/replace message (str stable-file) (str archive-path))
                message)]
    (fail! :unreadable-zip archive-path {:cause cause})))

(defn- open-zip-archive [archive-path stable-file]
  (try
    (-> (ZipFile/builder)
        (.setFile (io/file stable-file))
        (.setCharset legacy-name-charset)
        (.setUseUnicodeExtraFields true)
        (.get))
    (catch IOException t
      (cond
        (caused-by? CharacterCodingException t)
        (fail! :invalid-member-name-encoding archive-path
               {:cause (.getMessage t)})

        (parser-boundary-io? t) (unreadable-zip! archive-path stable-file t)
        :else (throw t)))))

(defn- parser-decoded-entries [archive-path archive]
  (try
    (decoded-entries archive-path archive)
    (catch IOException t
      (if (parser-boundary-io? t)
        (unreadable-zip! archive-path nil t)
        (throw t)))))

(defn- validated-parser-entries [archive-path archive limits]
  (let [entries (parser-decoded-entries archive-path archive)]
    (validate-entry-collisions! archive-path entries)
    (validate-declared-limits! archive-path entries limits)
    (sort-by :path entries)))

(defn- inspect-open-zip [archive-path stable-file limits]
  (with-open [archive (open-zip-archive archive-path stable-file)]
    (let [entries (validated-parser-entries archive-path archive limits)
          primary-path (choose-primary! archive-path entries)
          total-bytes (volatile! 0)
          read-results (mapv #(read-member! archive-path archive % primary-path
                                            total-bytes limits)
                             entries)
          members (mapv :metadata read-results)
          primary-bytes (:primary-bytes
                         (first (filter :primary-bytes read-results)))
          primary-hash (get (some #(when (= primary-path (get % "path")) %)
                                  members)
                            "member_hash")
          identity-object
          (validate-identity-object!
           {"construction" construction
            "members" (mapv #(select-keys % ["path" "member_hash"])
                            members)
            "primary_text_member" primary-path})]
      {:identity-object identity-object
       :bundle-hash (bundle-identity-hash identity-object)
       :archive-hash (hash/format-sha256 (files/sha256-file stable-file))
       :members members
       :primary-text-member primary-path
       :primary-text-hash primary-hash
       :primary-text-bytes primary-bytes})))

(defn- stage-archive! [zip-file]
  (let [staged (Files/createTempFile
                "abc-source-bundle-staged-" ".zip"
                (make-array java.nio.file.attribute.FileAttribute 0))]
    (try
      (Files/copy (.toPath (io/file zip-file)) staged
                  (into-array java.nio.file.CopyOption
                              [StandardCopyOption/REPLACE_EXISTING]))
      (when-not (.setReadOnly (.toFile staged))
        (throw (IOException. "could not make staged source archive read-only")))
      (.toFile staged)
      (catch Throwable t
        (Files/deleteIfExists staged)
        (throw t)))))

(defn inspect-zip
  ([zip-file]
   (inspect-zip zip-file default-limits))
  ([zip-file limits]
   (let [staged (stage-archive! zip-file)]
     (try
       (inspect-open-zip zip-file staged (merge default-limits limits))
       (finally
         (Files/deleteIfExists (.toPath staged)))))))

(defn inspect-zip-metadata
  "Apply the source-bundle name decoding, normalization, collision, and primary
  candidate construction without reading or hashing member bodies. Intended for
  bounded corpus evidence; admission must still use inspect-zip."
  [zip-file]
  (with-open [archive (open-zip-archive zip-file zip-file)]
    (let [entries (parser-decoded-entries zip-file archive)]
      (merge
       {:semantic-text-member-count
        (count (filter #(primary-candidate? (:path %)) entries))}
       (collision-evidence entries)))))

(defn write-manifest! [path inspection]
  (let [file (io/file path)
        manifest-value
        {"source_bundle_schema_id" schema-id
         "bundle_hash_algorithm" bundle-hash-algorithm
         "bundle_hash" (:bundle-hash inspection)
         "archive_hash" (:archive-hash inspection)
         "identity_object" (:identity-object inspection)
         "members" (:members inspection)}]
    (validate-persisted-manifest! manifest-value)
    (json/write-deterministic-json-file!
     file manifest-value)
    file))
