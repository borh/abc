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

(defn admission-error? [throwable]
  (and (instance? clojure.lang.ExceptionInfo throwable)
       (true? (::admission-error (ex-data throwable)))))

(defn- unicode-fold [s]
  (UCharacter/foldCase ^String s true))

(defn- path-collision-analysis [paths]
  (let [by-path (group-by identity paths)
        by-fold (group-by unicode-fold paths)]
    {:nfc-collisions
     (->> by-path
          (filter #(> (count (val %)) 1))
          (sort-by key)
          (mapv (fn [[path duplicates]]
                  {:path path :member-count (count duplicates)})))
     :unicode-case-collisions
     (->> by-fold
          (keep (fn [[folded folded-paths]]
                  (let [distinct-paths (vec (sort (distinct folded-paths)))]
                    (when (> (count distinct-paths) 1)
                      {:folded-path folded :paths distinct-paths}))))
          (sort-by :folded-path)
          vec)}))

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
      (let [{:keys [nfc-collisions unicode-case-collisions]}
            (path-collision-analysis paths)]
        (when (seq nfc-collisions)
          (identity-fail! :identity-member-path-collision {:paths paths}))
        (when (seq unicode-case-collisions)
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

(defn- read-member!
  [archive-path archive {:keys [entry path decoded-path]} retained-path
   total-bytes {:keys [max-member-bytes max-total-bytes]}]
  (let [digest (MessageDigest/getInstance "SHA-256")
        retained? (= path retained-path)
        retained (when retained? (ByteArrayOutputStream.))
        declared-bytes (.getSize ^ZipArchiveEntry entry)
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
             :declared-bytes declared-bytes
             :actual-bytes member-bytes
             :efs-utf8-flag?
             (.usesUTF8ForNames (.getGeneralPurposeBit ^ZipArchiveEntry entry))
             :primary-bytes (when retained? (.toByteArray retained))}
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
              (when retained? (.write retained buffer 0 n))
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

(defn- decoded-parser-entries [archive-path archive limits]
  (let [entries (sort-by :path
                         (parser-decoded-entries archive-path archive))]
    (validate-declared-limits! archive-path entries limits)
    entries))

(defn- scan-open-zip [archive-path stable-file limits]
  (with-open [archive (open-zip-archive archive-path stable-file)]
    (let [entries (decoded-parser-entries archive-path archive limits)
          candidates (filterv #(primary-candidate? (:path %)) entries)
          collision-analysis
          (path-collision-analysis (mapv :path entries))
          retained-path (when (= 1 (count candidates))
                          (:path (first candidates)))
          total-bytes (volatile! 0)
          reads (mapv #(read-member! archive-path archive % retained-path
                                     total-bytes limits)
                      entries)
          members (mapv :metadata reads)
          actuals (mapv :actual-bytes reads)]
      {:archive-path (str archive-path)
       :archive-hash (hash/format-sha256 (files/sha256-file stable-file))
       :members members
       :semantic-text-candidates (mapv :path candidates)
       :collision-evidence
       {:nfc-collision? (boolean (seq (:nfc-collisions
                                       collision-analysis)))
        :unicode-case-collision?
        (boolean (seq (:unicode-case-collisions collision-analysis)))}
       :stats
       {:member-count (count members)
        :max-member-bytes (reduce max 0 actuals)
        :total-bytes (.deref ^clojure.lang.IDeref total-bytes)
        :utf8-count (count (filter :efs-utf8-flag? reads))
        :legacy-count (count (remove :efs-utf8-flag? reads))
        :declared-actual-size-mismatches
        (->> reads
             (keep (fn [{:keys [metadata declared-bytes actual-bytes]}]
                     (when (and (not (neg? declared-bytes))
                                (not= declared-bytes actual-bytes))
                       {:member-path (get metadata "path")
                        :declared-bytes declared-bytes
                        :actual-bytes actual-bytes})))
             vec)}
       :primary-text-bytes
       (:primary-bytes (first (filter :primary-bytes reads)))})))

(defn- stage-archive! [zip-file]
  (let [attributes (make-array java.nio.file.attribute.FileAttribute 0)
        staged (Files/createTempFile
                "abc-source-bundle-staged-" ".zip" attributes)]
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

(defn- eocd-candidate-ends
  "Byte offsets just past each plausible end-of-central-directory record
  in the archive, scanning backward (candidate end = signature + fixed 22
  bytes + declared comment length, kept only when it fits the file)."
  [^bytes data]
  (let [n (alength data)]
    (loop [i (- n 22)
           ends []]
      (if (neg? i)
        ends
        (if (and (= 80 (aget data i))
                 (= 75 (aget data (inc i)))
                 (= 5 (aget data (+ i 2)))
                 (= 6 (aget data (+ i 3))))
          (let [comment-len (bit-or (bit-and (aget data (+ i 20)) 0xff)
                                    (bit-shift-left
                                     (bit-and (aget data (+ i 21)) 0xff) 8))
                end (+ i 22 comment-len)]
            (recur (dec i) (if (<= end n) (conj ends end) ends)))
          (recur (dec i) ends))))))

(defn- stage-truncated-archive! [^bytes data end]
  (let [attributes (make-array java.nio.file.attribute.FileAttribute 0)
        staged (Files/createTempFile
                "abc-source-bundle-trimmed-" ".zip" attributes)]
    (try
      (Files/write staged (java.util.Arrays/copyOfRange data 0 (int end))
                   (make-array java.nio.file.OpenOption 0))
      (when-not (.setReadOnly (.toFile staged))
        (throw (IOException. "could not make trimmed source archive read-only")))
      (.toFile staged)
      (catch Throwable t
        (Files/deleteIfExists staged)
        (throw t)))))

(defn- unreadable-zip-error? [t]
  (and (instance? clojure.lang.ExceptionInfo t)
       (= :unreadable-zip (:reason (ex-data t)))))

(defn- scan-zip-with-trailing-garbage-recovery
  "Retry an unreadable archive at earlier end-of-central-directory
  candidates: some shipped archives (e.g. Aozora's 58100_txt_60357.zip)
  carry trailing bytes with a decoy EOCD after the intact archive, which
  the zip reader trusts and then rejects. A successful retry records the
  trimmed byte count; the archive identity stays the hash of the file as
  shipped, garbage included."
  [zip-file staged limits original-error]
  (let [data (Files/readAllBytes (.toPath ^java.io.File staged))
        n (alength data)
        candidates (->> (eocd-candidate-ends data)
                        (filter #(< % n))
                        (sort-by -)
                        (take 3))]
    (or (some (fn [end]
                (let [trimmed (stage-truncated-archive! data end)]
                  (try
                    (-> (scan-open-zip zip-file trimmed limits)
                        (assoc :archive-hash (hash/format-sha256
                                              (files/sha256-file staged))
                               :trailing-garbage-trimmed (- n end)))
                    (catch clojure.lang.ExceptionInfo e
                      (when-not (unreadable-zip-error? e) (throw e))
                      nil)
                    (finally (files/delete-file! trimmed)))))
              candidates)
        (throw original-error))))

(defn scan-zip
  ([zip-file] (scan-zip zip-file default-limits))
  ([zip-file limits]
   (let [staged (stage-archive! zip-file)
         limits (merge default-limits limits)]
     (try
       (try
         (scan-open-zip zip-file staged limits)
         (catch clojure.lang.ExceptionInfo e
           (if (unreadable-zip-error? e)
             (scan-zip-with-trailing-garbage-recovery zip-file staged limits e)
             (throw e))))
       (finally (files/delete-file! staged))))))

(defn- validate-admission-collisions! [archive-path members]
  (let [{:keys [nfc-collisions unicode-case-collisions]}
        (path-collision-analysis (mapv #(get % "path") members))]
    (when-let [collision (first nfc-collisions)]
      (fail! :duplicate-member-path archive-path collision))
    (when-let [collision (first unicode-case-collisions)]
      (fail! :case-fold-member-path-collision archive-path collision)))
  members)

(defn admit-scan! [scan]
  (let [archive-path (:archive-path scan)
        members (validate-admission-collisions! archive-path (:members scan))
        candidates (:semantic-text-candidates scan)
        primary-path (case (count candidates)
                       0 (fail! :no-primary-text-member archive-path
                                {:candidates []})
                       1 (first candidates)
                       (fail! :multiple-primary-text-members archive-path
                              {:candidates candidates}))
        primary-hash (get (some #(when (= primary-path (get % "path")) %)
                                members)
                          "member_hash")
        primary-bytes (:primary-text-bytes scan)
        retained-hash (when (some? primary-bytes)
                        (hash/format-sha256
                         (hash/sha256-bytes primary-bytes)))
        identity-object
        (validate-identity-object!
         {"construction" construction
          "members" (mapv #(select-keys % ["path" "member_hash"]) members)
          "primary_text_member" primary-path})]
    (when-not (and primary-hash (= primary-hash retained-hash))
      (fail! :primary-text-retention-mismatch archive-path
             {:primary-text-member primary-path}))
    (cond-> {:identity-object identity-object
             :bundle-hash (bundle-identity-hash identity-object)
             :archive-hash (:archive-hash scan)
             :members members
             :primary-text-member primary-path
             :primary-text-hash primary-hash
             :primary-text-bytes primary-bytes}
      (:trailing-garbage-trimmed scan)
      (assoc :trailing-garbage-trimmed (:trailing-garbage-trimmed scan)))))

(defn inspect-zip
  ([zip-file] (inspect-zip zip-file default-limits))
  ([zip-file limits] (admit-scan! (scan-zip zip-file limits))))

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
