(ns soranoha.za.assemble-test
  "Release-assembly acceptance against the real kernel: a fixture corpus
  checkout built through the real selection/extract/engine path, assembled
  into snh-manifest/1 with real admission evidence, published through the
  publication transaction, and verified. Covers the include-and-flag path
  for an invalid work, manifest round-trip stability, the second-revision
  three-set delta oracle (addition, deletion, content edit, catalog
  locality, output-preserving rezip), the assessment-only delta, and the
  totality gate."
  (:require [babashka.fs :as fs]
            [clojure.test :refer [deftest is testing]]
            [soranoha.core.hash :as hash]
            [soranoha.snh.decode :as decode]
            [soranoha.snh.admission :as admission]
            [soranoha.snh.fixture :as fx]
            [soranoha.snh.transact :as transact]
            [soranoha.snh.verify :as verify]
            [soranoha.snh.view :as view]
            [soranoha.za.assemble :as za]
            [soranoha.za.corpus :as corpus]
            [soranoha.za.oracle :as oracle]))

(def merosu {:work-id "000100" :person-id "000001" :card "000001"
             :book "100" :n "1001" :title "hashire-merosu"
             :text "メロスは激怒した。\nfixture line two\n"})
(def kumo {:work-id "000300" :person-id "000002" :card "000002"
           :book "300" :n "2002" :title "kumo-no-ito"
           :text "蜘蛛の糸 fixture text\n"})
(def flagged {:work-id "000500" :person-id "000003" :card "000003"
              :book "500" :n "3003" :title "flagged-work"
              :text "fixture-invalid body\n"})
(def guarded {:work-id "000700" :person-id "000004" :card "000004"
              :book "700" :n "4004" :title "guarded-work"
              :text "in-copyright candidate\n"})
(def unevaluated {:work-id "000900" :person-id "000005" :card "000005"
                  :book "900" :n "5005" :title "unevaluated-work"
                  :text "not yet assessed\n"})
(def added-work {:work-id "001100" :person-id "000006" :card "000006"
                 :book "1100" :n "6006" :title "added-work"
                 :text "arrives at the second revision\n"})

(def slug-of corpus/work-slug)

(defn- fact [status basis]
  (if (= "not-evaluated" status)
    {"status" "not-evaluated" "jurisdiction" nil
     "effective_date" nil "basis" nil}
    {"status" status "jurisdiction" "jp"
     "effective_date" "2026-08-01" "basis" basis}))

(defn- candidate [work work-status contribution-status]
  (let [slug (slug-of work)]
    {"slug" slug
     "work_assessment" (fact work-status (str "edition:" slug))
     "contributions" [(assoc (fact contribution-status
                                   (str "author:" (:person-id work)))
                             "contribution_id"
                             (str "author:" (:person-id work)))]}))

(defn- pd [work] (candidate work "public-domain" "public-domain"))

(def policy-hash (hash/sha256-string "za fixture policy"))

(defn- publish-release! [clone run candidates]
  (transact/publish-build!
   {:clone clone :branch fx/branch :pinned-keys (fx/pinned-keys)
    :sign-release fx/sign-release
    :assemble (za/release-assembler
               {:cas-dir (:cas-dir run)
                :corpus {"upstream_origin"
                         "https://forge.example/za/fixture-corpus.git"
                         "upstream_rev" (:commit run)}
                :toolchain (za/toolchain-value (vals corpus/stage-set))
                :selection-params {"config" "za-fixture" "concurrency" 1}
                :policy-id "za-fixture-policy-v1"
                :policy-hash policy-hash
                :candidates candidates
                :works (corpus/works-for-assembly run)
                :selection (map :slug (:candidates run))})}))

(defn- verified-chain [clone]
  (verify/verify-repository-at (view/git-view clone)
                               (fx/head-of clone)
                               (fx/pinned-keys)))

(defn- temp-store! [] (str (fs/create-temp-dir {:prefix "za-store"})))

(defn- repo-artifact
  "Decoded protocol artifact fetched from the published repository by id."
  [clone type artifact-id]
  (let [v (view/git-view clone)
        commit (fx/head-of clone)]
    (:value (decode/decode type
                           (view/read-at v commit
                                         (verify/blob-path
                                          (verify/id->hex artifact-id)))))))

(deftest kernel-backed-genesis-release
  (let [root (corpus/init-corpus! [merosu kumo flagged guarded unevaluated])
        run (corpus/run-corpus! root (temp-store!))
        {:keys [clone]} (fx/make-repos!)
        candidates [(pd merosu) (pd kumo) (pd flagged)
                    (candidate guarded "public-domain" "in-copyright")
                    (candidate unevaluated "not-evaluated" "not-evaluated")]
        result (publish-release! clone run candidates)]
    (is (= :published (:outcome result)))
    (let [chain (verified-chain clone)
          head (:head-manifest chain)]
      (is (= 1 (:chain-length chain)))
      (is (= (:manifest-id result) (:head chain)))

      (testing "works are the admitted slugs; admission is total"
        (is (= (sort (map slug-of [merosu kumo flagged]))
               (mapv #(get % "slug") (get head "works"))))
        (let [report (repo-artifact clone "admission-report"
                                    (get-in head ["admission"
                                                  "admission_report"]))]
          (is (= [{"slug" (slug-of guarded) "reason_code" "in-copyright"}]
                 (get report "excluded")))
          (is (= [{"slug" (slug-of unevaluated)
                   "reason_code" "not-fully-evaluated"}]
                 (get report "quarantined")))
          (testing "inclusion_rule_hash binds the executable rule value"
            (is (= (hash/sha256-canonical-json admission/inclusion-rule)
                   (get report "inclusion_rule_hash")))
            (is (= (get admission/inclusion-rule "id")
                   (get report "inclusion_rule_id"))))))

      (testing "include-and-flag: the invalid work publishes and is flagged"
        (is (some #(= (slug-of flagged) (get % "slug")) (get head "works")))
        (is (= {"invalid_count" 1 "invalid_slugs" [(slug-of flagged)]}
               (get head "validation_summary"))))

      (testing "artifact ids are the kernel's content hashes"
        (let [entry (first (filter #(= (slug-of merosu) (get % "slug"))
                                   (get head "works")))
              outputs (get-in run [:results (slug-of merosu) :outputs])]
          (is (= (str "snh:1:tei:" (get-in outputs [:render "tei"]))
                 (get-in entry ["artifacts" 1 "id"])))
          (is (= (str "snh:1:plaintext:" (get-in outputs [:render "plaintext"]))
                 (get-in entry ["artifacts" 0 "id"])))
          (is (= (str "snh:1:tei-validation:"
                      (get-in outputs [:validate "tei-validation"]))
                 (get-in entry ["artifacts" 2 "id"])))))

      (testing "the manifest round-trips with a stable id"
        (let [reencoded (decode/encode "release-manifest" head)]
          (is (= (:head chain) (:hex reencoded)))
          (is (= (:hex reencoded)
                 (:hex (decode/encode "release-manifest"
                                      (:value reencoded)))))))

      (testing "an identical scheduled build is a no-op"
        (is (= :already-published
               (:outcome (publish-release! clone run candidates))))))))

(deftest second-revision-delta-oracle
  (let [root (corpus/init-corpus! [merosu kumo flagged guarded unevaluated])
        store (temp-store!)
        run-a (corpus/run-corpus! root store)
        {:keys [clone]} (fx/make-repos!)
        candidates-a [(pd merosu) (pd kumo) (pd flagged)
                      (candidate guarded "public-domain" "in-copyright")
                      (candidate unevaluated "not-evaluated" "not-evaluated")]
        result-a (publish-release! clone run-a candidates-a)
        manifest-a (:head-manifest (verified-chain clone))
        merosu-b (assoc merosu :text "メロスは激怒した。edited at rev B\n")
        works-b [merosu-b flagged guarded unevaluated added-work]]
    (is (= :published (:outcome result-a)))
    ;; revision B: content edit, deletion, addition, and an
    ;; output-preserving rezip (same member bytes, new archive bytes)
    (corpus/write-work! root merosu-b)
    (corpus/delete-work! root kumo)
    (corpus/write-work! root flagged :entry-time 1200000000000)
    (corpus/write-work! root added-work)
    (corpus/write-catalog! root works-b)
    (corpus/commit-corpus! root)
    (let [run-b (corpus/run-corpus! root store)
          candidates-b [(pd merosu) (pd flagged) (pd added-work)
                        (candidate guarded "public-domain" "in-copyright")
                        (candidate unevaluated "not-evaluated"
                                   "not-evaluated")]
          result-b (publish-release! clone run-b candidates-b)
          manifest-b (:head-manifest (verified-chain clone))]
      (is (= :published (:outcome result-b)))
      (is (not= (:manifest-id result-a) (:manifest-id result-b)))

      (testing "(a) source/selection delta"
        (is (= {:added #{(slug-of added-work)}
                :removed #{(slug-of kumo)}
                :changed #{(slug-of merosu) (slug-of flagged)}}
               (oracle/source-delta run-a run-b))))

      (testing "(b) stages invalidated and executed"
        (let [executed (oracle/executed-stages run-b)]
          (is (= #{:extract :metadata :parse :convert :render :validate :fidelity}
                 (executed (slug-of added-work)))
              "a new work executes the full chain")
          (is (= #{:extract :parse :convert :render :validate :fidelity}
                 (executed (slug-of merosu)))
              "a content edit leaves unchanged metadata cached")
          (is (= #{:extract}
                 (executed (slug-of flagged)))
              "an output-preserving rezip re-extracts and stops")
          (is (= #{} (executed (slug-of guarded)))
              "unrelated catalog edits leave this work cached")
          (is (= #{} (executed (slug-of unevaluated))))))

      (testing "(c) artifact/manifest delta"
        (is (= {:added #{(slug-of added-work)}
                :removed #{(slug-of kumo)}
                :changed #{(slug-of merosu)}
                :retained #{(slug-of flagged)}}
               (oracle/works-delta manifest-a manifest-b))))

      (testing "every executed stage is explained by a changed input"
        (is (= [] (oracle/unexplained-executions run-a run-b))))

      (testing "every artifact change corresponds to a byte change"
        (let [entry (fn [m slug]
                      (first (filter #(= slug (get % "slug"))
                                     (get m "works"))))
              changed-a (entry manifest-a (slug-of merosu))
              changed-b (entry manifest-b (slug-of merosu))]
          (is (not= (get-in changed-a ["artifacts" 1 "id"])
                    (get-in changed-b ["artifacts" 1 "id"])))
          (testing "unchanged bytes retain ids despite the source rezip"
            (is (= (entry manifest-a (slug-of flagged))
                   (entry manifest-b (slug-of flagged))))))))))

(deftest assessment-only-delta-is-a-new-release
  ;; same store, same revision, same works: an enlarged assessment snapshot
  ;; changes the admission evidence and must publish, never no-op
  (let [root (corpus/init-corpus! [merosu unevaluated])
        run (corpus/run-corpus! root (temp-store!))
        {:keys [clone]} (fx/make-repos!)
        result-1 (publish-release!
                  clone run
                  [(pd merosu)
                   (candidate unevaluated "not-evaluated" "not-evaluated")])
        manifest-1 (:head-manifest (verified-chain clone))
        result-2 (publish-release!
                  clone run
                  [(pd merosu)
                   (candidate unevaluated "undetermined" "undetermined")])
        chain (verified-chain clone)
        manifest-2 (:head-manifest chain)]
    (is (= :published (:outcome result-1)))
    (is (= :published (:outcome result-2)))
    (is (not= (:manifest-id result-1) (:manifest-id result-2)))
    (is (= 2 (:chain-length chain)))
    (is (= (get manifest-1 "works") (get manifest-2 "works"))
        "the works and their artifact bytes are untouched")
    (is (not= (get-in manifest-1 ["admission" "assessment_snapshot"])
              (get-in manifest-2 ["admission" "assessment_snapshot"])))))

(deftest oracle-refuses-incomparable-evidence
  ;; a changed trace key means a changed declared input only under
  ;; complete, identical stage evidence: divergent or absent coordinate
  ;; tables, an uncovered analyzed stage, and a missing trace key are all
  ;; refused rather than read as explained executions (trace invalidation
  ;; under coordinate changes itself is proven by the engine tests)
  (let [root (corpus/init-corpus! [merosu])
        store (temp-store!)
        run-a (corpus/run-corpus! root store)
        run-b (corpus/run-corpus! root store)
        slug (slug-of merosu)
        ;; run-b evidence claiming a :parse execution (its trace key is
        ;; unchanged, so with complete evidence this is a violation)
        executed (assoc-in run-b [:results slug :cached :parse] false)
        refused (fn [a b] (try (oracle/unexplained-executions a b) nil
                               (catch clojure.lang.ExceptionInfo e
                                 (:reason (ex-data e)))))]
    (testing "identical complete evidence is comparable"
      (is (= #{} ((oracle/executed-stages run-b) slug)))
      (is (= [] (oracle/unexplained-executions run-a run-b))))
    (testing "a same-key execution under complete evidence is a violation"
      (is (= [{:slug slug :stage :parse
               :trace-key (get-in run-b [:results slug :trace-keys :parse])}]
             (oracle/unexplained-executions run-a executed))))
    (doseq [[label a b]
            [["toolchain id changed"
              (assoc-in run-a [:stage-coordinates :parse "toolchain_id"]
                        "za-fixture-toolchain-2")
              executed]
             ["stage version changed"
              (assoc-in run-a [:stage-coordinates :render "stage_version"] "2")
              executed]
             ["coordinate table absent"
              (dissoc run-a :stage-coordinates)
              executed]
             ["equal empty coordinate tables with an execution"
              (assoc run-a :stage-coordinates {})
              (assoc executed :stage-coordinates {})]
             ["trace key missing from the earlier run"
              (update-in run-a [:results slug :trace-keys] dissoc :parse)
              executed]]]
      (testing label
        (is (= :runs-incomparable (refused a b)))))))

(deftest unassessed-selection-blocks-emission
  ;; a selected work missing from the assessment snapshot violates the
  ;; totality gate before anything is emitted
  (let [root (corpus/init-corpus! [merosu kumo])
        run (corpus/run-corpus! root (temp-store!))
        {:keys [clone]} (fx/make-repos!)]
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"totality"
                          (publish-release! clone run [(pd merosu)])))))
