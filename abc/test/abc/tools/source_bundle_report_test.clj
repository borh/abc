(ns abc.tools.source-bundle-report-test
  (:require [abc.tools.json :as json]
            [abc.tools.source-bundle :as source-bundle]
            [abc.tools.source-bundle-report :as report]
            [abc.test-fs :refer [with-temp-dir]]
            [babashka.fs :as fs]
            [babashka.process :as process]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is]])
  (:import [java.io FileNotFoundException IOException InterruptedIOException]
           [java.nio ByteBuffer ByteOrder]
           [java.nio.charset StandardCharsets]
           [java.nio.file NoSuchFileException]
           [java.nio.file Files]
           [org.apache.commons.compress.archivers.zip
            ZipArchiveEntry ZipArchiveOutputStream]))

(deftest corpus-zip-listing-selects-zips-in-path-order-test
  (with-temp-dir [root]
    (doseq [path [["cards" "b" "files" "z.zip"]
                  ["cards" "a" "files" "b.zip"]
                  ["cards" "a" "files" "a.zip"]]]
      (fs/create-dirs (apply fs/file root (butlast path)))
      (spit (apply fs/file root path) "zip"))
    (fs/create-dirs (fs/file root "cards" "a" "files" "directory.zip"))
    (spit (fs/file root "cards" "a" "files" "ignore.txt") "x")
    (is (= ["a.zip" "b.zip" "z.zip"]
           (mapv (comp str fs/file-name) (#'report/corpus-zips root))))))

(defn- understate-first-central-size! [file declared-size]
  (let [data (Files/readAllBytes (.toPath file))
        signature (byte-array [0x50 0x4b 0x01 0x02])
        offset (first
                (for [start (range (inc (- (alength data)
                                           (alength signature))))
                      :when (every? true?
                                    (map-indexed
                                     (fn [i b]
                                       (= b (aget data (+ start i))))
                                     signature))]
                  start))]
    (when-not offset
      (throw (ex-info "central directory signature not found" {:file file})))
    (-> (ByteBuffer/wrap data)
        (.order ByteOrder/LITTLE_ENDIAN)
        (.putInt (+ offset 24) (int declared-size)))
    (Files/write (.toPath file) data
                 (make-array java.nio.file.OpenOption 0))
    file))

(defn- content-bytes [s]
  (.getBytes s StandardCharsets/UTF_8))

(defn- write-zip!
  ([file members] (write-zip! file members false))
  ([file members efs?]
   (io/make-parents file)
   (with-open [out (ZipArchiveOutputStream. file)]
     (.setEncoding out "UTF-8")
     (.setUseLanguageEncodingFlag out efs?)
     (doseq [[path content] members]
       (let [entry (doto (ZipArchiveEntry. path) (.setTime 0))]
         (.putArchiveEntry out entry)
         (.write out content 0 (alength content))
         (.closeArchiveEntry out))))
   file))

(defn- card-zip
  ([root card filename members]
   (card-zip root card filename members false))
  ([root card filename members efs?]
   (write-zip! (io/file root "cards" card "files" filename) members efs?)))

(defn- one-card-root []
  (let [root (.toFile (Files/createTempDirectory
                       "abc-source-bundle-report-taxonomy-"
                       (make-array java.nio.file.attribute.FileAttribute 0)))]
    (card-zip root "1" "work.zip" [["work.txt" (content-bytes "body")]])
    root))

(deftest sevenzip-listable-exit-status-test
  (let [calls (atom [])
        binary (or (System/getenv "ABC_7ZZ_BIN") "7zz")
        zip-file (io/file "fixture.zip")]
    (with-redefs [process/sh (fn [args]
                               (swap! calls conj args)
                               {:exit 0 :out "" :err ""})]
      (is (true? (#'report/sevenzip-listable? zip-file))))
    (with-redefs [process/sh (fn [args]
                               (swap! calls conj args)
                               {:exit 1 :out "" :err "not listable"})]
      (is (false? (#'report/sevenzip-listable? zip-file))))
    (is (= [[binary "l" "-slt" "fixture.zip"]
            [binary "l" "-slt" "fixture.zip"]]
           @calls))))

(deftest sevenzip-listable-process-start-failure-propagates-test
  (let [failure (IOException. "7zz executable unavailable")
        thrown (with-redefs [process/sh (fn [_args] (throw failure))]
                 (try
                   (#'report/sevenzip-listable? (io/file "fixture.zip"))
                   nil
                   (catch IOException e e)))]
    (is (identical? failure thrown))
    (is (= IOException (type thrown)))
    (is (= "7zz executable unavailable" (.getMessage thrown)))))

(deftest programming-failures-do-not-invoke-sevenzip-test
  (let [sevenzip-calls (atom 0)]
    (doseq [failure [(AssertionError. "programming failure")
                     (InterruptedIOException. "interrupted")
                     (LinkageError. "linkage failure")
                     (FileNotFoundException. "missing")
                     (NoSuchFileException. "missing")]]
      (is (identical?
           failure
           (try
             (with-redefs-fn
               {#'source-bundle/scan-zip (fn [_] (throw failure))
                #'report/sevenzip-listable?
                (fn [_] (swap! sevenzip-calls inc) false)}
               #(report/measure! (one-card-root)))
             (catch Throwable t t)))))
    (is (zero? @sevenzip-calls))))

(deftest evidence-collector-trusts-only-marked-admission-errors-test
  (let [spoof (ex-info "spoof" {:reason :unreadable-zip})]
    (is (identical?
         spoof
         (try
           (with-redefs [source-bundle/scan-zip (fn [_] (throw spoof))]
             (report/measure! (one-card-root)))
           (catch Throwable t t))))))

(deftest production-limit-failures-abort-corpus-evidence-test
  (let [sevenzip-calls (atom 0)
        thrown
        (try
          (with-redefs [source-bundle/default-limits
                        {:max-members 10
                         :max-member-bytes 3
                         :max-total-bytes 100}
                        report/sevenzip-listable?
                        (fn [_] (swap! sevenzip-calls inc) false)]
            (report/measure! (one-card-root)))
          nil
          (catch clojure.lang.ExceptionInfo t t))]
    (is (source-bundle/admission-error? thrown))
    (is (= :member-too-large (:reason (ex-data thrown))))
    (is (zero? @sevenzip-calls))))

(deftest measure-pinned-corpus-shape-test
  (let [root (.toFile (Files/createTempDirectory
                       "abc-source-bundle-report-"
                       (make-array java.nio.file.attribute.FileAttribute 0)))]
    (card-zip root "1" "normal.zip"
              [["work.txt" (content-bytes "body")]
               ["image.png" (content-bytes "12")]])
    (card-zip root "2" "appledouble.zip"
              [["__MACOSX/._shadow.txt" (content-bytes "x")]
               ["main.txt" (content-bytes "abc")]])
    (card-zip root "3" "asset-only.zip"
              [["image.png" (content-bytes "12345")]])
    (card-zip root "4" "nfc-collision.zip"
              [["café.png" (content-bytes "a")]
               ["café.png" (content-bytes "b")]]
              true)
    (card-zip root "5" "case-collision.zip"
              [["A.png" (content-bytes "a")]
               ["a.png" (content-bytes "b")]])
    (let [limit-zip (card-zip root "6" "limit.zip"
                              [["work.txt" (byte-array 11)]
                               ["a.bin" (byte-array 7)]
                               ["b.bin" (byte-array 3)]])]
      (understate-first-central-size! limit-zip 1))
    (let [damaged (io/file root "cards" "7" "files" "damaged.zip")]
      (io/make-parents damaged)
      (spit damaged "not a zip"))
    ;; Files outside cards/*/files/*.zip are deliberately invisible.
    (write-zip! (io/file root "other" "ignored.zip")
                [["ignored.txt" (content-bytes "ignored")]])
    (is (= {"measurement_construction"
            "abc-source-bundle-streamed-evidence-v1"
            "readable_zip_count" 6
            "unreadable_zip_count" 1
            "admitted_zip_count" 3
            "rejected_zip_count" 4
            "rejection_reason_counts"
            {"case-fold-member-path-collision" 1
             "duplicate-member-path" 1
             "no-primary-text-member" 1
             "unreadable-zip" 1}
            "semantic_text_member_counts" {"0" 3 "1" 3}
            "utf8_flagged_entry_count" 2
            "legacy_flagged_entry_count" 10
            "nfc_collision_bundle_count" 1
            "unicode_case_collision_bundle_count" 1
            "max_member_count" 3
            "max_member_bytes" 11
            "max_total_bytes" 21
            "declared_actual_size_mismatch_member_count" 1
            "declared_actual_size_mismatches"
            [{"archive_path" "cards/6/files/limit.zip"
              "member_path" "work.txt"
              "declared_bytes" 1
              "actual_bytes" 11}]
            "java_unreadable_7zz_listable_count" 0
            "java_unreadable_7zz_unlistable_count" 1
            "damaged_paths" ["cards/7/files/damaged.zip"]}
           (report/measure! root)))))

(deftest checked-pinned-evidence-test
  (is (= {"aozorabunko_commit" "0e9ea3e586eb0aa34039fabfc85a407d2f98b165"
          "measurement_construction"
          "abc-source-bundle-streamed-evidence-v1"
          "readable_zip_count" 17884
          "unreadable_zip_count" 3
          "admitted_zip_count" 17879
          "rejected_zip_count" 8
          "rejection_reason_counts"
          {"no-primary-text-member" 5 "unreadable-zip" 3}
          "semantic_text_member_counts" {"0" 5 "1" 17879}
          "utf8_flagged_entry_count" 0
          "legacy_flagged_entry_count" 22860
          "nfc_collision_bundle_count" 0
          "unicode_case_collision_bundle_count" 0
          "max_member_count" 778
          "max_member_bytes" 12631833
          "max_total_bytes" 27874310
          "declared_actual_size_mismatch_member_count" 1
          "declared_actual_size_mismatches"
          [{"archive_path" "cards/001393/files/50710_ruby_36965.zip"
            "member_path" "fushigino_kunino_alice_musical.txt"
            "declared_bytes" 68007
            "actual_bytes" 68497}]
          "java_unreadable_7zz_listable_count" 1
          "java_unreadable_7zz_unlistable_count" 2
          "damaged_paths" ["cards/001154/files/chihobunkano_shinkensetsu.zip"
                           "cards/001505/files/58100_txt_60357.zip"
                           "cards/001562/files/56151_ruby_60063.zip"]}
         (json/read-json-file
          "data/source-bundle/aozorabunko-0e9ea3e-summary.json"))))
