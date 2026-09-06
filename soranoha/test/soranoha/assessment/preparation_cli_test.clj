(ns soranoha.assessment.preparation-cli-test
  (:require [babashka.fs :as fs]
            [charred.api :as json]
            [clojure.test :refer [deftest is testing]]
            [soranoha.assessment.aozora :as aozora]
            [soranoha.assessment.records :as records]
            [soranoha.main :as main]))

(deftest batch-refresh-preserves-owner-records
  (let [dir (fs/create-temp-dir {:prefix "reliance-preparation"})
        input (str (fs/path dir "source.json"))
        output (str (fs/path dir "prepared.json"))
        revision (apply str (repeat 40 "a"))
        digest (apply str (repeat 64 "b"))
        record (fn [slug]
                 {"slug" slug "source_revision" revision
                  "source_content_hash" (str "sha256:" digest)
                  "observed_at" "2026-09-06" "decision_date" "2026-09-06"
                  "basis" "Synthetic official assertion for preparation coverage."
                  "catalog_sha256" digest "card_sha256" digest
                  "file_sha256" digest "rules_sha256" digest "exception" nil})
        protected (assoc (record "000092_000879_000879_92_ruby_164")
                         "exception" "Specific unresolved restriction.")
        unavailable (record "000472_000081_000081_472_ruby_654")
        acquired (record "001567_000035_000035_1567_ruby_4948")
        options {:aozora-root "unused" :evidence-root "unused" :all true
                 :assessment-source input :out output}]
    (try
      (fs/write-bytes input (:bytes (records/encode
                                     (assoc records/empty-source "reliances"
                                            [protected unavailable]))))
      (with-redefs [main/source-provenance! (constantly revision)
                    aozora/prepare-batch!
                    (fn [_ _ slugs _]
                      (is (nil? slugs))
                      {:records [(assoc protected "exception" nil "basis" "Changed assertion.") acquired]
                       :unavailable [{:slug (get unavailable "slug") :reason :aozora/http-status}]})]
        (is (= [{"slug" (get unavailable "slug") "reason" "http-status"}]
               (get (json/read-json (with-out-str (main/aozora-reliance-prepare! options))) "unavailable")))
        (is (= #{protected unavailable acquired}
               (set (get (:value (records/decode (fs/read-all-bytes output))) "reliances")))))
      (testing "selection must be explicit and unambiguous"
        (doseq [opts [(dissoc options :all) (assoc options :slug (get acquired "slug"))]]
          (is (= :invalid-preparation-selection
                 (try (main/aozora-reliance-prepare! opts)
                      (catch clojure.lang.ExceptionInfo e (:reason (ex-data e))))))))
      (finally (fs/delete-tree dir)))))
