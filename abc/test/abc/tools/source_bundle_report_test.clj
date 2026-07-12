(ns abc.tools.source-bundle-report-test
  (:require [abc.tools.json :as json]
            [abc.tools.source-bundle-report :as report]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is]])
  (:import [java.nio.charset StandardCharsets]
           [java.nio.file Files]
           [org.apache.commons.compress.archivers.zip
            ZipArchiveEntry ZipArchiveOutputStream]))

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
    (card-zip root "6" "limit.zip"
              [["work.txt" (byte-array 11)] ["a.bin" (byte-array 7)]
               ["b.bin" (byte-array 3)]])
    (let [damaged (io/file root "cards" "7" "files" "damaged.zip")]
      (io/make-parents damaged)
      (spit damaged "not a zip"))
    ;; Files outside cards/*/files/*.zip are deliberately invisible.
    (write-zip! (io/file root "other" "ignored.zip")
                [["ignored.txt" (content-bytes "ignored")]])
    (is (= {"readable_zip_count" 6
            "unreadable_zip_count" 1
            "semantic_text_member_counts" {"0" 3 "1" 3}
            "utf8_flagged_entry_count" 2
            "legacy_flagged_entry_count" 10
            "nfc_collision_bundle_count" 1
            "unicode_case_collision_bundle_count" 1
            "max_member_count" 3
            "max_member_bytes" 11
            "max_total_bytes" 21
            "java_unreadable_7zz_recoverable_count" 0
            "java_unreadable_7zz_unrecoverable_count" 1
            "damaged_paths" ["cards/7/files/damaged.zip"]}
           (report/measure! root)))))

(deftest checked-pinned-evidence-test
  (is (= {"aozorabunko_commit" "0e9ea3e586eb0aa34039fabfc85a407d2f98b165"
          "readable_zip_count" 17884
          "unreadable_zip_count" 3
          "semantic_text_member_counts" {"0" 5 "1" 17879}
          "utf8_flagged_entry_count" 0
          "legacy_flagged_entry_count" 22860
          "nfc_collision_bundle_count" 0
          "unicode_case_collision_bundle_count" 0
          "max_member_count" 778
          "max_member_bytes" 12631833
          "max_total_bytes" 27874310
          "java_unreadable_7zz_recoverable_count" 1
          "java_unreadable_7zz_unrecoverable_count" 2
          "damaged_paths" ["cards/000035/files/258_ruby_5404.zip"
                           "cards/001779/files/56651_ruby_57934.zip"
                           "cards/002016/files/59475_ruby_70415.zip"]}
         (json/read-json-file
          "data/source-bundle/aozorabunko-0e9ea3e-summary.json"))))
