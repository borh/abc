(ns abc.tools.validate-corpus-test
  (:require [abc.tools.aozora-ingest :as ingest]
            [abc.tools.aozora-ingest-test :as ingest-test]
            [abc.tools.files :as files]
            [abc.tools.validate-corpus :as vc]
            [charred.api :as charred]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(defn- temp-dir [prefix]
  (.toFile (Files/createTempDirectory prefix (make-array FileAttribute 0))))

(defn- delete-recursive [^java.io.File f]
  (when (.isDirectory f)
    (doseq [child (.listFiles f)] (delete-recursive child)))
  (.delete f))

(deftest validate-corpus-clean-test
  (testing "validate-corpus! reports zero failures on a freshly ingested corpus"
    (let [dir (temp-dir "abc-vc-clean")]
      (try
        (ingest/run-corpus!
         {:rows ingest-test/synthetic-corpus-rows
          :output-dir (str dir)})
        (let [{:keys [works-checked failed first-failures]}
              (vc/validate-corpus! {:input-dir (str dir)})]
          (is (= 3 works-checked))
          (is (= 0 failed))
          (is (empty? first-failures)))
        (finally (delete-recursive dir))))))

(deftest validate-corpus-detects-corrupt-person-test
  (testing "validate-corpus! reports the offending work-id when a referenced person body is corrupt"
    ;; Note: this exercises the upstream JSON-schema validate! gate in
    ;; the validate-corpus pipeline, not the SHACL pass downstream of
    ;; it. Removing family_name fails person-record/validate! first;
    ;; SHACL never runs for the broken person body. The end-to-end
    ;; SHACL path is covered by abc.tools.shacl-test against curated
    ;; failure shapes.
    (let [dir (temp-dir "abc-vc-corrupt")]
      (try
        (ingest/run-corpus!
         {:rows ingest-test/synthetic-corpus-rows
          :output-dir (str dir)})
        (let [pid "000879"
              path (io/file dir "persons" (str pid ".json"))
              body (files/read-json (str path))
              broken (dissoc body "family_name")]
          (spit path (charred/write-json-str broken)))
        (let [{:keys [failed first-failures]}
              (vc/validate-corpus! {:input-dir (str dir)})]
          ;; Both 000127 and 000128 reference 000879, so both fail; 000129
          ;; references the untouched 000888 and is unaffected.
          (is (= 2 failed))
          (is (every? #(re-find #"person-record validation failed" (:error %))
                      first-failures))
          (is (every? #(#{"000127" "000128"} (:work-id %)) first-failures)))
        (finally (delete-recursive dir))))))
