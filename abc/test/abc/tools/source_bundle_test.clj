(ns abc.tools.source-bundle-test
  (:require [abc.tools.hash :as hash]
            [abc.tools.json :as json]
            [abc.tools.schema :as schema]
            [abc.tools.source-bundle :as source-bundle]
            [clojure.test :refer [deftest is testing]])
  (:import [java.io FileNotFoundException IOException InterruptedIOException]
           [java.nio ByteBuffer ByteOrder]
           [java.nio.channels ClosedByInterruptException]
           [java.nio.charset StandardCharsets]
           [java.nio.file AccessDeniedException Files NoSuchFileException]
           [java.util.zip CRC32]
           [org.apache.commons.compress.archivers.zip
            UnicodePathExtraField ZipArchiveEntry ZipArchiveOutputStream
            ZipArchiveOutputStream$UnicodeExtraFieldPolicy]))

(defn- utf8-bytes [s]
  (.getBytes s StandardCharsets/UTF_8))

(defn- hex-bytes [s]
  (byte-array
   (map #(unchecked-byte (Integer/parseInt % 16))
        (re-seq #".." s))))

(defn- temp-file [suffix]
  (.toFile (Files/createTempFile "abc-source-bundle-" suffix
                                 (make-array java.nio.file.attribute.FileAttribute 0))))

(defmacro ^{:clj-kondo/lint-as 'clojure.core/let} with-zips [bindings & body]
  (let [files (take-nth 2 bindings)]
    `(let ~bindings
       (try
         ~@body
         (finally
           (doseq [file# [~@files]]
             (Files/deleteIfExists (.toPath file#))))))))

(defn- write-zip!
  ([file members] (write-zip! file members {}))
  ([file members {:keys [comment compression encoding efs unicode-extra time]
                  :or {compression ZipArchiveOutputStream/DEFLATED
                       encoding "UTF-8"
                       efs true
                       unicode-extra ZipArchiveOutputStream$UnicodeExtraFieldPolicy/NEVER
                       time 0}}]
   (with-open [out (ZipArchiveOutputStream. file)]
     (.setEncoding out encoding)
     (.setUseLanguageEncodingFlag out efs)
     (.setCreateUnicodeExtraFields out unicode-extra)
     (.setComment out (or comment ""))
     (doseq [[path content extra-field] members]
       (let [entry (ZipArchiveEntry. path)]
         (.setTime entry (long time))
         (.setMethod entry compression)
         (when extra-field
           (.addExtraField entry extra-field))
         (when (= compression ZipArchiveOutputStream/STORED)
           (let [crc (doto (CRC32.) (.update content))]
             (.setSize entry (alength content))
             (.setCrc entry (.getValue crc))))
         (.putArchiveEntry out entry)
         (.write out content 0 (alength content))
         (.closeArchiveEntry out))))
   file))

(defn- inspection [members]
  (let [file (write-zip! (temp-file ".zip") members)]
    (try
      (source-bundle/inspect-zip file)
      (finally
        (Files/deleteIfExists (.toPath file))))))

(defn- reason [f]
  (try
    (f)
    nil
    (catch clojure.lang.ExceptionInfo e
      (:reason (ex-data e)))))

(defn- admission-data [f]
  (try
    (f)
    nil
    (catch clojure.lang.ExceptionInfo e
      (ex-data e))))

(defn- member-by-path [inspection path]
  (some #(when (= path (get % "path")) %) (:members inspection)))

(defn- replace-bytes! [file old replacement]
  (let [data (Files/readAllBytes (.toPath file))]
    (loop [start 0]
      (when (<= (+ start (alength old)) (alength data))
        (if (every? true?
                    (map-indexed (fn [i b] (= b (aget data (+ start i)))) old))
          (do
            (System/arraycopy replacement 0 data start (alength replacement))
            (recur (+ start (alength old))))
          (recur (inc start)))))
    (Files/write (.toPath file) data (make-array java.nio.file.OpenOption 0))
    file))

(defn- understate-first-central-size! [file declared-size]
  (let [data (Files/readAllBytes (.toPath file))
        signature (byte-array [0x50 0x4b 0x01 0x02])
        offset (first
                (for [start (range (inc (- (alength data) (alength signature))))
                      :when (every? true?
                                    (map-indexed
                                     (fn [i b] (= b (aget data (+ start i))))
                                     signature))]
                  start))]
    (when-not offset
      (throw (ex-info "central directory signature not found" {:file file})))
    (-> (ByteBuffer/wrap data)
        (.order ByteOrder/LITTLE_ENDIAN)
        (.putInt (+ offset 24) (int declared-size)))
    (Files/write (.toPath file) data (make-array java.nio.file.OpenOption 0))
    file))

(deftest inspect-zip-separates-identities-test
  (with-zips [zip (write-zip! (temp-file ".zip")
                              [["work.txt" (utf8-bytes "本文")]
                               ["fig/one.png" (byte-array [1 2 3])]])]
    (let [{:keys [identity-object bundle-hash archive-hash members
                  primary-text-member primary-text-hash primary-text-bytes]}
          (source-bundle/inspect-zip zip)
          by-path (into {} (map (juxt #(get % "path") identity) members))
          expected-identity
          {"construction" "abc-source-bundle-v1"
           "members" [{"path" "fig/one.png"
                       "member_hash" (hash/format-sha256
                                      (hash/sha256-bytes (byte-array [1 2 3])))}
                      {"path" "work.txt"
                       "member_hash" (hash/format-sha256
                                      (hash/sha256-bytes
                                       (utf8-bytes "本文")))}]
           "primary_text_member" "work.txt"}]
      (is (= "abc-source-bundle-v1" (get identity-object "construction")))
      (is (= expected-identity identity-object))
      (is (= (source-bundle/bundle-identity-hash expected-identity)
             bundle-hash))
      (is (= (hash/format-sha256 (hash/sha256-file zip)) archive-hash))
      (is (= "work.txt" primary-text-member))
      (is (= primary-text-hash (get-in by-path ["work.txt" "member_hash"])))
      (is (= primary-text-hash
             (hash/format-sha256 (hash/sha256-bytes primary-text-bytes))))
      (is (= (seq (utf8-bytes "本文")) (seq primary-text-bytes)))
      (is (not= archive-hash bundle-hash))
      (is (every? #(not (contains? % "byte_length"))
                  (get identity-object "members"))))))

(deftest source-bundle-v1-known-answer-test
  (let [known-answer
        (json/read-json-file
         "fixtures/source-bundle/abc-source-bundle-v1-known-answer.json")
        members
        (mapv (fn [member]
                [(get member "path")
                 (if-let [content (get member "content_utf8")]
                   (utf8-bytes content)
                   (hex-bytes (get member "content_hex")))])
              (get known-answer "input_members"))]
    (with-zips [zip (write-zip! (temp-file ".zip") members)]
      (let [{:keys [identity-object bundle-hash]}
            (source-bundle/inspect-zip zip)
            expected-canonical-bytes
            (utf8-bytes (get known-answer "canonical_identity_utf8"))]
        (is (= (seq expected-canonical-bytes)
               (seq (source-bundle/bundle-identity-canonical-bytes
                     identity-object))))
        (is (= (get known-answer "bundle_hash") bundle-hash))))))

(deftest repacking-does-not-change-bundle-identity-test
  (let [members [["work.txt" (utf8-bytes "same")]
                 ["fig.png" (byte-array [1 2 3])]]]
    (with-zips [a (write-zip! (temp-file ".zip") members
                              {:time 0 :comment "a"})
                b (write-zip! (temp-file ".zip") (reverse members)
                              {:time 1700000000000
                               :comment "b"
                               :compression ZipArchiveOutputStream/STORED})]
      (let [ia (source-bundle/inspect-zip a)
            ib (source-bundle/inspect-zip b)]
        (is (= (:identity-object ia) (:identity-object ib)))
        (is (= (:bundle-hash ia) (:bundle-hash ib)))
        (is (not= (:archive-hash ia) (:archive-hash ib)))))))

(deftest member-changes-rotate-bundle-hash-test
  (let [base [["work.txt" (utf8-bytes "text")] ["fig.png" (byte-array [1])]]
        variants [(conj base ["extra.bin" (byte-array [2])])
                  (vec (butlast base))
                  [["work.txt" (utf8-bytes "text")] ["renamed.png" (byte-array [1])]]
                  [["work.txt" (utf8-bytes "text")] ["fig.png" (byte-array [2])]]]
        base-hash (:bundle-hash (inspection base))]
    (doseq [members variants]
      (is (not= base-hash (:bundle-hash (inspection members)))))))

(deftest packaging-metadata-is-retained-but-not-primary-test
  (let [result (inspection [["work.txt" (utf8-bytes "body")]
                            ["__MACOSX/other.txt" (utf8-bytes "metadata")]
                            ["._shadow.txt" (utf8-bytes "metadata")]])]
    (is (= "work.txt" (:primary-text-member result)))
    (is (= #{"work.txt" "__MACOSX/other.txt" "._shadow.txt"}
           (set (map #(get % "path") (:members result)))))))

(deftest semantic-primary-cardinality-is-enforced-test
  (is (= :no-primary-text-member
         (reason #(inspection [["image.png" (byte-array [1])]]))))
  (is (= :multiple-primary-text-members
         (reason #(inspection [["one.txt" (utf8-bytes "1")]
                               ["two.TXT" (utf8-bytes "2")]])))))

(deftest multiple-primary-diagnostics-are-deterministic-test
  (let [members [["two.txt" (utf8-bytes "2")]
                 ["one.txt" (utf8-bytes "1")]]
        forward (admission-data #(inspection members))
        reverse-order (admission-data #(inspection (reverse members)))]
    (is (= :multiple-primary-text-members (:reason forward)))
    (is (= ["one.txt" "two.txt"] (:candidates forward)))
    (is (= (:candidates forward) (:candidates reverse-order)))))

(deftest entry-name-decoding-precedence-test
  (testing "EFS names are strict UTF-8"
    (with-zips [zip (write-zip! (temp-file ".zip") [["作品.txt" (utf8-bytes "x")]])]
      (let [member (member-by-path (source-bundle/inspect-zip zip) "作品.txt")]
        (is (= "efs-utf8" (get member "name_source"))))))
  (testing "a valid Unicode Path extra field wins over the legacy name"
    (with-zips [zip (write-zip! (temp-file ".zip") [["作品.txt" (utf8-bytes "x")]]
                                {:encoding "windows-31j"
                                 :efs false
                                 :unicode-extra ZipArchiveOutputStream$UnicodeExtraFieldPolicy/ALWAYS})]
      (is (= "unicode-extra"
             (get (member-by-path (source-bundle/inspect-zip zip) "作品.txt")
                  "name_source")))))
  (testing "an invalid Unicode Path CRC is ignored and legacy bytes are decoded"
    (let [bad-extra (UnicodePathExtraField. "作品.txt" (utf8-bytes "not-legacy"))]
      (with-zips [zip (write-zip! (temp-file ".zip")
                                  [["legacy.txt" (utf8-bytes "x") bad-extra]]
                                  {:encoding "windows-31j" :efs false})]
        (is (= "windows-31j"
               (get (member-by-path (source-bundle/inspect-zip zip) "legacy.txt")
                    "name_source"))))))
  (testing "a CRC-valid Unicode Path field still requires strict UTF-8"
    (let [raw-name (utf8-bytes "legacy.txt")
          crc (doto (CRC32.) (.update raw-name))
          malformed-extra
          (doto (UnicodePathExtraField.)
            (.setNameCRC32 (.getValue crc))
            (.setUnicodeName
             (byte-array [(unchecked-byte 0xc3) 0x28 0x2e 0x74 0x78 0x74])))]
      (with-zips [zip (write-zip! (temp-file ".zip")
                                  [["legacy.txt" (utf8-bytes "x") malformed-extra]]
                                  {:encoding "windows-31j" :efs false})]
        (is (= :invalid-member-name-encoding
               (reason #(source-bundle/inspect-zip zip)))))))
  (testing "strict windows-31j is the final fallback"
    (with-zips [zip (write-zip! (temp-file ".zip") [["作品.txt" (utf8-bytes "x")]]
                                {:encoding "windows-31j" :efs false})]
      (is (= "windows-31j"
             (get (member-by-path (source-bundle/inspect-zip zip) "作品.txt")
                  "name_source")))))
  (testing "malformed legacy bytes are rejected rather than replaced"
    (with-zips [zip (write-zip! (temp-file ".zip") [["aa.txt" (utf8-bytes "x")]]
                                {:encoding "windows-31j" :efs false})]
      (replace-bytes! zip (utf8-bytes "aa.txt")
                      (byte-array [(unchecked-byte 0x81) 0x30 0x2e 0x74 0x78 0x74]))
      (is (= :invalid-member-name-encoding
             (reason #(source-bundle/inspect-zip zip)))))))

(deftest nfc-normalization-and-collision-admission-test
  (let [result (inspection [["é.txt" (utf8-bytes "x")]])]
    (is (= "é.txt" (:primary-text-member result)))
    (is (= "é.txt" (get (first (:members result)) "decoded_path"))))
  (is (= :duplicate-member-path
         (reason #(inspection [["é.txt" (utf8-bytes "1")]
                               ["é.txt" (utf8-bytes "2")]]))))
  (doseq [members [[["É.txt" (utf8-bytes "1")] ["é.txt" (utf8-bytes "2")]]
                   [["Straße.txt" (utf8-bytes "1")] ["STRASSE.txt" (utf8-bytes "2")]]]]
    (is (= :case-fold-member-path-collision
           (reason #(inspection members))))))

(deftest unicode-casefold-collision-diagnostics-are-deterministic-test
  (let [members [["é.txt" (utf8-bytes "2")]
                 ["É.txt" (utf8-bytes "1")]]
        forward (admission-data #(inspection members))
        reverse-order (admission-data #(inspection (reverse members)))]
    (is (= :case-fold-member-path-collision (:reason forward)))
    (is (= "é.txt" (:folded-path forward)))
    (is (= ["É.txt" "é.txt"] (:paths forward)))
    (is (= (select-keys forward [:folded-path :paths])
           (select-keys reverse-order [:folded-path :paths])))))

(deftest unsafe-paths-are-rejected-test
  (doseq [path ["/work.txt" "C:/work.txt" "a//work.txt" "./work.txt"
                "a/../work.txt" "a\\..\\work.txt"]]
    (is (= :unsafe-member-path
           (reason #(inspection [[path (utf8-bytes "x")]])))
        path)))

(deftest unreadable-zip-is-an-admission-error-test
  (with-zips [file (temp-file ".zip")]
    (spit file "not a ZIP")
    (let [data (try
                 (source-bundle/inspect-zip file)
                 nil
                 (catch clojure.lang.ExceptionInfo e (ex-data e)))]
      (is (= :unreadable-zip (:reason data)))
      (is (= (str file) (:archive-path data))))))

(deftest pinned-damaged-archive-plain-io-is-an-admission-error-test
  (with-zips [zip (write-zip! (temp-file ".zip")
                              [["work.txt" (utf8-bytes "body")]])]
    (let [failure (IOException.
                   (str "Error reading Zip content from " zip)
                   (IOException.
                    "Central directory is empty, can't expand corrupt archive."))
          data (try
                 (with-redefs-fn
                   {#'source-bundle/decoded-entries (fn [& _] (throw failure))}
                   #(source-bundle/inspect-zip zip))
                 nil
                 (catch clojure.lang.ExceptionInfo e (ex-data e)))]
      (is (= :unreadable-zip (:reason data)))
      (is (= (.getMessage failure) (:cause data))))))

(deftest parser-boundary-propagates-wrapped-operational-io-test
  (with-zips [zip (write-zip! (temp-file ".zip")
                              [["work.txt" (utf8-bytes "body")]])]
    (doseq [cause [(InterruptedIOException. "interrupted")
                   (ClosedByInterruptException.)
                   (FileNotFoundException. "missing")
                   (NoSuchFileException. "missing")
                   (AccessDeniedException. "denied")]]
      (let [failure (IOException. "parser boundary wrapper" cause)]
        (is (identical?
             failure
             (try
               (with-redefs-fn
                 {#'source-bundle/decoded-entries (fn [& _] (throw failure))}
                 #(source-bundle/inspect-zip zip))
               (catch Throwable t t)))
            (str (class cause)))))))

(deftest inspection-stages-one-stable-archive-test
  (let [before-members [["work.txt" (utf8-bytes "before")]
                        ["figure.png" (byte-array [1 2 3])]]
        after-members [["work.txt" (utf8-bytes "after")]
                       ["figure.png" (byte-array [9 8 7])]]]
    (with-zips [zip (write-zip! (temp-file ".zip") before-members)]
      (let [expected (inspection before-members)
            original-inspect @#'source-bundle/scan-open-zip
            staged-path (atom nil)
            actual
            (with-redefs-fn
              {#'source-bundle/scan-open-zip
               (fn [archive-path staged-file limits]
                 (reset! staged-path staged-file)
                 (write-zip! zip after-members)
                 (original-inspect archive-path staged-file limits))}
              #(source-bundle/inspect-zip zip))]
        (is (= (select-keys expected [:identity-object :bundle-hash
                                      :archive-hash :primary-text-hash])
               (select-keys actual [:identity-object :bundle-hash
                                    :archive-hash :primary-text-hash])))
        (is (some? @staged-path))
        (is (not (Files/exists (.toPath @staged-path)
                               (make-array java.nio.file.LinkOption 0))))))))

(deftest staged-archive-is-cleaned-after-later-failure-test
  (with-zips [zip (write-zip! (temp-file ".zip")
                              [["work.txt" (utf8-bytes "body")]])]
    (let [failure (IOException. "later member read failed")
          staged-path (atom nil)]
      (is (identical?
           failure
           (try
             (with-redefs-fn
               {#'source-bundle/scan-open-zip
                (fn [_archive-path staged-file _limits]
                  (reset! staged-path staged-file)
                  (throw failure))}
               #(source-bundle/inspect-zip zip))
             (catch Throwable t t))))
      (is (some? @staged-path))
      (is (not (Files/exists (.toPath @staged-path)
                             (make-array java.nio.file.LinkOption 0)))))))

(deftest full-inspection-propagates-non-archive-failures-test
  (with-zips [zip (write-zip! (temp-file ".zip")
                              [["work.txt" (utf8-bytes "body")]])]
    (doseq [failure [(AssertionError. "programming")
                     (LinkageError. "linkage")
                     (InterruptedException. "interrupted")
                     (InterruptedIOException. "interrupted")
                     (FileNotFoundException. "missing")
                     (NoSuchFileException. "missing")
                     (AccessDeniedException. "denied")
                     (RuntimeException. "programming")]]
      (is (identical?
           failure
           (try
             (with-redefs-fn
               {#'source-bundle/scan-open-zip (fn [& _] (throw failure))}
               #(source-bundle/inspect-zip zip))
             (catch Throwable t t)))))))

(deftest entry-name-decoding-propagates-programming-failures-test
  (with-zips [zip (write-zip! (temp-file ".zip")
                              [["work.txt" (utf8-bytes "body")]])]
    (let [failure (AssertionError. "strict decoder bug")]
      (is (identical?
           failure
           (try
             (with-redefs-fn
               {#'source-bundle/strict-decode (fn [& _] (throw failure))}
               #(source-bundle/inspect-zip zip))
             (catch Throwable t t)))))))

(deftest declared-and-streamed-limits-are-enforced-test
  (with-zips [zip (write-zip! (temp-file ".zip")
                              [["work.txt" (utf8-bytes "12345")]
                               ["image.bin" (utf8-bytes "67890")]])]
    (is (= :too-many-members
           (reason #(source-bundle/inspect-zip zip {:max-members 1
                                                    :max-member-bytes 100
                                                    :max-total-bytes 100}))))
    (is (= :member-too-large
           (reason #(source-bundle/inspect-zip zip {:max-members 10
                                                    :max-member-bytes 4
                                                    :max-total-bytes 100}))))
    (is (= :total-too-large
           (reason #(source-bundle/inspect-zip zip {:max-members 10
                                                    :max-member-bytes 100
                                                    :max-total-bytes 9})))))
  (with-zips [zip (write-zip! (temp-file ".zip")
                              [["work.txt" (utf8-bytes "12345")]])]
    (understate-first-central-size! zip 1)
    (let [member-data (admission-data
                       #(source-bundle/inspect-zip
                         zip {:max-members 10
                              :max-member-bytes 4
                              :max-total-bytes 100}))
          total-data (admission-data
                      #(source-bundle/inspect-zip
                        zip {:max-members 10
                             :max-member-bytes 100
                             :max-total-bytes 4}))]
      (is (= :member-too-large (:reason member-data)))
      (is (= 5 (:actual-bytes member-data)))
      (is (= :total-too-large (:reason total-data)))
      (is (= 5 (:actual-bytes total-data))))))

(deftest manifest-validates-against-source-bundle-schema-test
  (with-zips [zip (write-zip! (temp-file ".zip") [["work.txt" (utf8-bytes "本文")]])
              manifest (temp-file ".json")]
    (source-bundle/write-manifest! manifest (source-bundle/inspect-zip zip))
    (let [value (json/read-json-file manifest)
          source-schema (schema/read-schema "schemas/source-bundle.schema.json")]
      (is (nil? (schema/validation-errors source-schema value)))
      (is (every? #(not (contains? % "byte_length"))
                  (get-in value ["identity_object" "members"]))))))

(deftest persisted-v1-identity-policy-corruptions-have-stable-reasons-test
  (let [base (:identity-object
              (inspection [["work.txt" (utf8-bytes "body")]
                           ["figure.png" (byte-array [1])]]))
        h (get-in base ["members" 0 "member_hash"])
        member (fn [path] {"path" path "member_hash" h})
        cases
        [{:reason :identity-fields
          :value (dissoc base "primary_text_member")}
         {:reason :identity-construction
          :value (assoc base "construction" "abc-source-bundle-v2")}
         {:reason :identity-member-fields
          :value (assoc-in base ["members" 0 "extra"] "no")}
         {:reason :identity-member-hash
          :value (assoc-in base ["members" 0 "member_hash"] "not-a-hash")}
         {:reason :identity-member-path
          :value (assoc base "members" [(member "../work.txt")]
                        "primary_text_member" "../work.txt")}
         {:reason :identity-member-path
          :value (assoc base "members" [(member "é.txt")]
                        "primary_text_member" "é.txt")}
         {:reason :identity-member-order
          :value (update base "members" (comp vec reverse))}
         {:reason :identity-member-path-collision
          :value (assoc base "members" [(member "work.txt")
                                        (member "work.txt")]
                        "primary_text_member" "work.txt")}
         {:reason :identity-member-case-fold-collision
          :value (assoc base "members" [(member "A.png")
                                        (member "a.png")
                                        (member "work.txt")]
                        "primary_text_member" "work.txt")}
         {:reason :identity-primary-cardinality
          :value (assoc base "members" [(member "one.txt")
                                        (member "two.txt")]
                        "primary_text_member" "one.txt")}
         {:reason :identity-primary-match
          :value (assoc base "primary_text_member" "figure.png")}]]
    (is (= base (source-bundle/validate-identity-object! base)))
    (doseq [{:keys [reason value]} cases]
      (is (= reason
             (:reason (admission-data
                       #(source-bundle/validate-identity-object! value))))
          (name reason)))))

(deftest persisted-manifest-rejects-coordinated-reversal-and-resign-test
  (with-zips [zip (write-zip! (temp-file ".zip")
                              [["work.txt" (utf8-bytes "body")]
                               ["figure.png" (byte-array [1])]])]
    (let [inspection (source-bundle/inspect-zip zip)
          manifest {"source_bundle_schema_id" source-bundle/schema-id
                    "bundle_hash_algorithm" source-bundle/bundle-hash-algorithm
                    "archive_hash" (:archive-hash inspection)
                    "identity_object" (update (:identity-object inspection)
                                              "members" (comp vec reverse))
                    "members" (vec (reverse (:members inspection)))}
          resigned (assoc manifest "bundle_hash"
                          (source-bundle/bundle-identity-hash
                           (get manifest "identity_object")))]
      (is (= :identity-member-order
             (:reason (admission-data
                       #(source-bundle/validate-persisted-manifest!
                         resigned))))))))

(deftest bounded-scan-streams-rejected-bundles-test
  (with-zips
    [asset (write-zip! (temp-file ".zip")
                       [["image.png" (utf8-bytes "12345")]])
     collision (write-zip! (temp-file ".zip")
                           [["A.png" (utf8-bytes "a")]
                            ["a.png" (utf8-bytes "bb")]
                            ["work.txt" (utf8-bytes "body")]])]
    (let [asset-scan (source-bundle/scan-zip asset)
          collision-scan (source-bundle/scan-zip collision)]
      (is (= {:member-count 1 :max-member-bytes 5 :total-bytes 5
              :utf8-count 1 :legacy-count 0
              :declared-actual-size-mismatches []}
             (:stats asset-scan)))
      (is (= [] (:semantic-text-candidates asset-scan)))
      (is (nil? (:primary-text-bytes asset-scan)))
      (is (= :no-primary-text-member
             (:reason (admission-data
                       #(source-bundle/admit-scan! asset-scan)))))
      (is (= 3 (get-in collision-scan [:stats :member-count])))
      (is (= ["work.txt"] (:semantic-text-candidates collision-scan)))
      (is (= "body" (String. ^bytes (:primary-text-bytes collision-scan)
                             StandardCharsets/UTF_8)))
      (is (true? (get-in collision-scan
                         [:collision-evidence :unicode-case-collision?])))
      (let [collision-error
            (try
              (source-bundle/admit-scan! collision-scan)
              nil
              (catch clojure.lang.ExceptionInfo t t))]
        (is (source-bundle/admission-error? collision-error))
        (is (= :case-fold-member-path-collision
               (:reason (ex-data collision-error))))))))

(deftest admission-rejects-mutated-retained-primary-bytes-test
  (with-zips [zip (write-zip! (temp-file ".zip")
                              [["work.txt" (utf8-bytes "body")]])]
    (let [scan (source-bundle/scan-zip zip)
          retained ^bytes (:primary-text-bytes scan)
          _ (aset-byte retained 0 (byte (int \B)))
          failure (try
                    (source-bundle/admit-scan! scan)
                    nil
                    (catch clojure.lang.ExceptionInfo t t))]
      (is (source-bundle/admission-error? failure))
      (is (= :primary-text-retention-mismatch
             (:reason (ex-data failure)))))))

(deftest efs-count-is-independent-of-decoder-name-source-test
  (with-zips
    [efs-zip (write-zip!
              (temp-file ".zip")
              [["作品.txt" (utf8-bytes "本文")]]
              {:efs true
               :unicode-extra
               ZipArchiveOutputStream$UnicodeExtraFieldPolicy/NEVER})
     extra-zip (write-zip!
                (temp-file ".zip")
                [["作品.txt" (utf8-bytes "本文")]]
                {:efs false
                 :unicode-extra
                 ZipArchiveOutputStream$UnicodeExtraFieldPolicy/ALWAYS})]
    (let [efs-scan (source-bundle/scan-zip efs-zip)
          extra-scan (source-bundle/scan-zip extra-zip)]
      (is (= "efs-utf8" (get-in efs-scan [:members 0 "name_source"])))
      (is (= [1 0] [(get-in efs-scan [:stats :utf8-count])
                    (get-in efs-scan [:stats :legacy-count])]))
      (is (= "unicode-extra"
             (get-in extra-scan [:members 0 "name_source"])))
      (is (= [0 1] [(get-in extra-scan [:stats :utf8-count])
                    (get-in extra-scan [:stats :legacy-count])])))))

(deftest bounded-scan-pins-declared-versus-actual-bytes-test
  (with-zips [zip (write-zip! (temp-file ".zip")
                              [["work.txt" (utf8-bytes "12345")]])]
    (understate-first-central-size! zip 1)
    (let [scan (source-bundle/scan-zip zip)]
      (is (= 5 (get-in scan [:stats :max-member-bytes])))
      (is (= 5 (get-in scan [:stats :total-bytes])))
      (is (= [{:member-path "work.txt" :declared-bytes 1 :actual-bytes 5}]
             (get-in scan [:stats :declared-actual-size-mismatches]))))))

(deftest inspect-zip-is-scan-plus-admission-test
  (with-zips [zip (write-zip! (temp-file ".zip")
                              [["work.txt" (utf8-bytes "body")]
                               ["figure.png" (byte-array [1 2 3])]])]
    (let [scanned (source-bundle/admit-scan!
                   (source-bundle/scan-zip zip))
          inspected (source-bundle/inspect-zip zip)]
      ;; Clojure byte-array equality is identity, not content equality.
      (is (= (dissoc scanned :primary-text-bytes)
             (dissoc inspected :primary-text-bytes)))
      (is (java.util.Arrays/equals
           ^bytes (:primary-text-bytes scanned)
           ^bytes (:primary-text-bytes inspected))))))

(deftest bounded-scan-failures-precede-logical-admission-test
  (with-zips [zip (write-zip! (temp-file ".zip")
                              [["image.png" (utf8-bytes "12345")]])]
    ;; Avoid the declared-size pre-check so the actual-byte stream proves the
    ;; precedence over the bundle's simultaneous no-primary defect.
    (understate-first-central-size! zip 1)
    (is (= :member-too-large
           (:reason
            (admission-data
             #(source-bundle/scan-zip
               zip {:max-members 10
                    :max-member-bytes 4
                    :max-total-bytes 100})))))))
