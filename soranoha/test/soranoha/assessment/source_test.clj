(ns soranoha.assessment.source-test
  (:require [babashka.fs :as fs]
            [clojure.test :refer [deftest is testing]]
            [soranoha.assessment.evaluate :as evaluate]
            [soranoha.assessment.records :as records]
            [soranoha.assessment.snapshot :as snapshot]
            [soranoha.assessment.source :as source]
            [soranoha.core.hash :as hash]
            [soranoha.kura.engine :as engine]
            [soranoha.main :as main]
            [soranoha.za.corpus :as corpus]))

(def work {:work-id "000100" :person-id "000001" :card "000001"
           :book "100" :n "1001" :title "synthetic edition"
           :text "A synthetic edition with reviewed contributor reconciliation.\n"
           :contributors [{:person-id "000009" :role "編者"}]})

(defn- reason [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))

(defn- observed-source [slug]
  (assoc records/empty-source "observations"
         [{"id" "edition" "selector" "canonical-source-bundle" "slug" slug}
          {"id" "catalog" "selector" "catalog-contributors" "slug" slug}]))

(defn- corrected-source [root]
  (let [slug (corpus/work-slug work)
        input (observed-source slug)
        captured (:observations (source/capture-checkout root input {}))
        identity {"id" "soranoha-example" "assessor" "synthetic reviewer"
                  "basis" "Synthetic identity exercised by this fixture only."
                  "evidence" ["edition"]}]
    (assoc input
           "identities" [identity]
           "findings"
           [{"id" "reconciled-contributors"
             "fact" (records/fact-key slug "contribution-set")
             "value" ["author:000001" "translator:soranoha-example"]
             "effective_date" "2026-09-01" "reviewed_at" "2026-09-01"
             "assessor" "synthetic reviewer" "method" "synthetic-completeness"
             "basis" "Synthetic fixture discharges listed editor and adds an unlisted translator."
             "premises" (conj (mapv (fn [[id value]]
                                      {"kind" "observation" "ref" id
                                       "fingerprint" (records/fingerprint value)})
                                    (sort-by key captured))
                              {"kind" "identity" "ref" "soranoha-example"
                               "fingerprint" (evaluate/identity-fingerprint identity captured)})}])))

(defn- evaluated-snapshot [store root input]
  (snapshot/encode
   (evaluate/evaluate! store input
                       (assoc (source/capture-checkout root input {})
                              :as-of "2026-09-05" :toolchain-id "source-test"))))

(deftest checkout-bound-reconciliation-and-local-observations
  (let [root (corpus/init-corpus! [work])
        dir (fs/create-temp-dir {:prefix "assessment-source-test"})
        store (engine/open-store! {:cas-dir (str (fs/path dir "objects"))
                                   :db-path (str (fs/path dir "trace.sqlite"))})
        input (corrected-source root)
        source-file (str (fs/path dir "source.json"))
        snapshot-file (str (fs/path dir "snapshot.json"))
        baseline (evaluated-snapshot store root input)
        opts {:root (str (fs/path dir "preflight-store")) :aozora-root root
              :assessment-source source-file :assessment snapshot-file
              :as-of "2026-09-05" :clj-toolchain-id "source-test"}]
    (try
      (fs/write-bytes source-file (:bytes (records/encode input)))
      (fs/write-bytes snapshot-file (:bytes baseline))
      (testing "a justified corrected set need not equal catalog rows"
        (is (= ["author:000001" "translator:soranoha-example"]
               (mapv #(get % "contribution_id")
                     (get-in baseline [:value "candidates" 0 "contributions"]))))
        (is (nil? (main/release-preflight-drift opts))))
      (testing "descriptive catalog metadata does not invalidate reconciliation"
        (corpus/write-catalog! root [(assoc work :title "changed descriptive title")])
        (corpus/commit-corpus! root)
        (is (= (:hex baseline) (:hex (evaluated-snapshot store root input))))
        (is (nil? (main/release-preflight-drift opts))))
      (doseq [[label changed] [["added candidate" (update work :contributors conj
                                                          {:person-id "000008" :role "翻訳者"})]
                               ["changed role" (assoc work :contributors
                                                      [{:person-id "000009" :role "校訂者"}])]]]
        (testing label
          (corpus/write-catalog! root [changed])
          (corpus/commit-corpus! root)
          (is (some? (main/release-preflight-drift opts)))))
      (testing "a changed edition under the same slug invalidates its completeness"
        (corpus/write-catalog! root [work])
        (corpus/write-work! root (assoc work :text "Changed edition content.\n"))
        (corpus/commit-corpus! root)
        (is (some? (main/release-preflight-drift opts)))
        (is (= "not-evaluated"
               (get-in (evaluated-snapshot store root input)
                       [:value "candidates" 0 "work_assessment" "status"]))))
      (testing "a removed edition makes its findings unavailable without corrupting the view"
        (corpus/delete-work! root work)
        (corpus/commit-corpus! root)
        (is (= [] (get-in (evaluated-snapshot store root input) [:value "candidates"])))
        (is (some? (main/release-preflight-drift opts))))
      (finally (engine/close-store! store)))))

(deftest retained-evidence-is-durable-input-not-cache-state
  (let [dir (fs/create-temp-dir {:prefix "assessment-evidence-test"})
        file (fs/path dir "evidence.txt")
        _ (spit (str file) "retained source")
        digest (hash/sha256-file (str file))
        observation {"id" "evidence" "selector" "retained-evidence"
                     "path" "evidence.txt" "sha256" digest}
        input (assoc records/empty-source "observations" [observation])]
    (is (= {"evidence" (str "sha256:" digest)}
           (source/retained-observations input dir)))
    (is (= :missing-evidence-root
           (reason #(source/retained-observations input nil))))
    (is (= :evidence-path-escape
           (reason #(source/retained-observations
                     (assoc input "observations" [(assoc observation "path" "../outside")]) dir))))
    (spit (str file) "changed source")
    (is (= :evidence-digest-mismatch
           (reason #(source/retained-observations input dir))))
    (fs/delete file)
    (is (= :missing-evidence
           (reason #(source/retained-observations input dir))))))
