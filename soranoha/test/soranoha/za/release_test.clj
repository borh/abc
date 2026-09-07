(ns soranoha.za.release-test
  "Automated release-driver acceptance over a local origin: consecutive
  scheduled invocations across real upstream movement publish releases the
  published checker verifies end-to-end; an unmoved upstream is the
  scheduled no-op; a malformed assessment snapshot publishes nothing."
  (:require [babashka.fs :as fs]
            [babashka.process :as process]
            [charred.api :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [soranoha.assessment.records :as records]
            [soranoha.assessment.snapshot :as snapshot]
            [soranoha.assessment.aozora :as aozora]
            [soranoha.core.hash :as hash]
            [soranoha.main :as main]
            [soranoha.snh.decode :as decode]
            [soranoha.snh.fixture :as fx]
            [soranoha.snh.sign :as sign]
            [soranoha.snh.repo :as repo]
            [soranoha.snh.verify :as verify]
            [soranoha.snh.view :as view]
            [soranoha.aozora.csv :as csv]
            [soranoha.yomi.catalog :as catalog]
            [soranoha.yomi.select :as select]
            [soranoha.za.corpus :as corpus]
            [soranoha.za.release :as release]
            [soranoha.za.scaffold :as scaffold]))

(def ^:private merosu {:work-id "000100" :person-id "000001" :card "000001"
                       :book "100" :n "1001" :title "hashire-merosu"
                       :text "メロスは激怒した。\nfixture line two\n"})
(def ^:private kumo {:work-id "000300" :person-id "000002" :card "000002"
                     :book "300" :n "2002" :title "kumo-no-ito"
                     :text "蜘蛛の糸 fixture text\n"})
(def ^:private added {:work-id "001100" :person-id "000006" :card "000006"
                      :book "1100" :n "6006" :title "added-work"
                      :text "arrives at the second revision\n"})

(def ^:private slug-of corpus/work-slug)

(defn- pd [work]
  (let [slug (slug-of work)
        fact (fn [basis] {"status" "public-domain" "jurisdiction" "jp"
                          "effective_date" "2026-08-01" "basis" basis})]
    {"slug" slug
     "work_assessment" (fact (str "edition:" slug))
     "contributions" [(assoc (fact (str "author:" (:work-id work)))
                             "contribution_id"
                             (str "author:" (:work-id work)))]}))

(def ^:private policy-hash (hash/sha256-string "za fixture policy"))

(defn- snapshot-bytes ^bytes [candidates]
  (:bytes (decode/encode "assessment-snapshot"
                         {"schema" "snh-assessment-snapshot/2"
                          "candidates" (vec (sort-by #(get % "slug")
                                                     candidates))})))

(defn- run->report
  "The build run report the driver consumes, projected from a fixture
  kernel run — the same fields main/build! exports, including the
  selected slugs captured from the selection join rather than the works."
  [run]
  {"aozora_git_commit" (:commit run)
   "selected_slugs" (vec (sort (map :slug (:candidates run))))
   "stages" (into {}
                  (map (fn [[stage coordinate]] [(name stage) coordinate]))
                  (dissoc (:stage-coordinates run) :accountability :coverage))
   "works" (into {}
                 (map (fn [[slug {:keys [outputs]}]]
                        [slug {"plaintext" (get-in outputs [:plaintext "plaintext"])
                               "tei" (get-in outputs [:render "tei"])
                               "tei-validation"
                               (get-in outputs [:validate "tei-validation"])
                               "source_content_hash"
                               (get (corpus/source-facts run slug)
                                    "work_content_hash")}]))
                 (:results run))})

(defn- driver-inputs [clone report cas-dir snapshot]
  {:selection (get report "selected_slugs")
   :source-hashes (into {} (map (fn [[slug work]] [slug (get work "source_content_hash")])) (get report "works"))
   :build-works! (fn [slugs] (assoc report "selected_slugs" slugs "works" (select-keys (get report "works") slugs)))
   :cas-dir cas-dir
   :upstream-origin "https://forge.example/za/fixture-corpus.git"
   :selection-params {}
   :policy-id "za-fixture-policy-v1"
   :policy-hash policy-hash
   :snapshot-bytes snapshot
   :clone clone
   :branch fx/branch
   :pinned-keys (fx/pinned-keys)
   :sign-release fx/sign-release})

(defn- drive-report! [clone report cas-dir snapshot]
  (release/release! (driver-inputs clone report cas-dir snapshot)))

(defn- drive! [clone run snapshot]
  (drive-report! clone (run->report run) (:cas-dir run) snapshot))

(defn- vector-keys []
  (get (json/read-json (slurp (io/resource "snh/vectors/signature-vectors.json")))
       "keys"))

(defn- verified-chain [clone]
  (verify/verify-repository-at (view/git-view clone)
                               (fx/head-of clone)
                               (fx/pinned-keys)))

(defn- temp-store! [] (str (fs/create-temp-dir {:prefix "za-release-store"})))

(deftest consecutive-automated-releases-over-a-local-origin
  (let [root (corpus/init-corpus! [merosu kumo])
        store (temp-store!)
        run-1 (corpus/run-corpus! root store)
        {:keys [clone]} (fx/make-repos!)
        result-1 (drive! clone run-1 (snapshot-bytes [(pd merosu) (pd kumo)]))]
    (is (= :published (:outcome result-1)))

    (testing "upstream movement publishes a successor release"
      (corpus/write-work! root added)
      (corpus/write-catalog! root [merosu kumo added])
      (corpus/commit-corpus! root)
      (let [run-2 (corpus/run-corpus! root store)
            result-2 (drive! clone run-2
                             (snapshot-bytes [(pd merosu) (pd kumo) (pd added)]))
            chain (verified-chain clone)
            head (:head-manifest chain)]
        (is (= :published (:outcome result-2)))
        (is (= [(:manifest-id result-2) (:manifest-id result-1)] (:chain chain)))
        (is (= (:manifest-id result-1) (get head "prev_manifest")))
        (is (= #{(slug-of merosu) (slug-of kumo) (slug-of added)}
               (into #{} (map #(get % "slug")) (get head "works"))))
        (is (= (:commit run-2) (get-in head ["corpus" "upstream_rev"])))

        (testing "an unmoved upstream is the scheduled no-op"
          (let [run-3 (corpus/run-corpus! root store)
                result-3 (drive! clone run-3
                                 (snapshot-bytes
                                  [(pd merosu) (pd kumo) (pd added)]))]
            (is (= :already-published (:outcome result-3)))
            (is (= (:manifest-id result-2) (:manifest-id result-3)))
            (is (= [(:manifest-id result-2) (:manifest-id result-1)]
                   (:chain (verified-chain clone))))))))))

(deftest malformed-assessment-snapshot-publishes-nothing
  (let [root (corpus/init-corpus! [merosu])
        run (corpus/run-corpus! root (temp-store!))
        {:keys [clone]} (fx/make-repos!)
        good (snapshot-bytes [(pd merosu)])
        good-str (String. good "UTF-8")
        head-before (fx/head-of clone)
        rejected (fn [^String s]
                   (try (drive! clone run (.getBytes s "UTF-8"))
                        nil
                        (catch clojure.lang.ExceptionInfo e
                          (:reason (ex-data e)))))]
    (testing "duplicate keys die at parse, before anything is assembled"
      (is (= :parse-invalid
             (rejected (str/replace good-str "{\"candidates\""
                                    "{\"candidates\":[],\"candidates\"")))))
    (testing "non-canonical bytes are rejected"
      (is (some? (rejected (str " " good-str)))))
    (testing "nothing reached the origin"
      (is (= head-before (fx/head-of clone))))
    (testing "the same run publishes once the snapshot is contractual"
      (is (= :published (:outcome (drive! clone run good)))))))

(deftest a-work-omitted-from-works-and-snapshot-cannot-pass-totality
  ;; the co-omission failure: kumo vanishes from the built works AND from
  ;; the snapshot, but the selected population captured before execution
  ;; still names it, so the driver refuses before assembly
  (let [root (corpus/init-corpus! [merosu kumo])
        run (corpus/run-corpus! root (temp-store!))
        {:keys [clone]} (fx/make-repos!)
        head-before (fx/head-of clone)
        broken (update (run->report run) "works" dissoc (slug-of kumo))
        outcome (try (release/release!
                      (assoc (driver-inputs clone broken (:cas-dir run)
                                            (snapshot-bytes [(pd merosu)]))
                             :build-works! (fn [_] (throw (ex-info "must refuse before building" {})))))
                     nil
                     (catch clojure.lang.ExceptionInfo e (ex-data e)))]
    (is (= :totality-violation (:reason outcome)))
    (is (= [(slug-of kumo)] (:only-in-selection outcome)))
    (is (= :selection-works-mismatch
           (try (drive-report! clone broken (:cas-dir run)
                               (snapshot-bytes [(pd merosu) (pd kumo)]))
                nil
                (catch clojure.lang.ExceptionInfo e (:reason (ex-data e))))))
    (is (= head-before (fx/head-of clone)))))

(deftest governance-withdrawal-executes-end-to-end
  ;; the full production path: a driver-published release, then a
  ;; withdrawal event signed offline by the governance key, submitted as
  ;; files through the governance CLI, with the transition invariants
  ;; verified by the published checker
  (let [root (corpus/init-corpus! [merosu kumo])
        run (corpus/run-corpus! root (temp-store!))
        {:keys [clone]} (fx/make-repos!)
        release-result (drive! clone run
                               (snapshot-bytes [(pd merosu) (pd kumo)]))
        dir (fs/create-temp-dir {:prefix "za-governance-cli"})
        ks (vector-keys)
        event-value (fx/event-value "withdrawal"
                                    [{"slug" (slug-of kumo)
                                      "reason_code" "takedown-request"
                                      "statement" "Documented request."}])
        {event-hex :hex event-bytes :bytes event-id :id}
        (decode/encode "governance-event" event-value)
        write-bytes! (fn [name ^bytes bytes]
                       (let [path (str (fs/path dir name))]
                         (fs/write-bytes path bytes)
                         path))
        base {:chain-clone (str clone)
              :branch fx/branch
              :event (write-bytes! "event.json" event-bytes)
              :event-sig (write-bytes! "event.sig" (fx/sign-event event-hex))
              :release-pub (write-bytes!
                            "release.pub"
                            (sign/hex64-lf-bytes (get-in ks ["release" "pub"])))
              :governance-pub (write-bytes!
                               "governance.pub"
                               (sign/hex64-lf-bytes
                                (get-in ks ["governance" "pub"])))
              :release-key (write-bytes!
                            "release.seed"
                            (.getBytes ^String (get-in ks ["release" "seed"])
                                       "UTF-8"))}
        outcome (main/governance! base)]
    (is (= :published (:outcome release-result)))
    (is (= :published (:outcome outcome)))
    (is (= event-id (:event outcome)))
    (let [chain (verified-chain clone)
          head (:head-manifest chain)]
      (is (= [(:manifest-id outcome) (:manifest-id release-result)]
             (:chain chain)))
      (is (= [(slug-of merosu)]
             (mapv #(get % "slug") (get head "works"))))
      (is (= [{"slug" (slug-of kumo) "event" event-id}]
             (get head "withdrawn"))))
    (testing "replaying the same signed event converges without a release"
      (is (= :already-applied (:outcome (main/governance! base)))))
    (testing "an event signed by the release key is refused"
      (is (= :event-signature-invalid
             (try (main/governance!
                   (assoc base :event-sig
                          (write-bytes! "wrong-role.sig"
                                        (fx/sign-event-with-release-key
                                         event-hex))))
                  nil
                  (catch clojure.lang.ExceptionInfo e
                    (:reason (ex-data e)))))))))

(deftest artifact-demand-follows-the-verified-head-after-a-rejected-push
  (let [root (corpus/init-corpus! [merosu kumo])
        run (corpus/run-corpus! root (temp-store!))
        {:keys [clone] :as repos} (fx/make-repos!)
        competitor (fx/second-clone! repos)
        snapshot (snapshot-bytes [(pd merosu) (pd kumo)])
        _ (drive! clone run snapshot)
        inputs (driver-inputs clone (run->report run) (:cas-dir run) snapshot)
        calls (atom [])
        next-release (assoc inputs :selection-params {"round" 2}
                            :build-works! (fn [slugs]
                                            (swap! calls conj slugs)
                                            ((:build-works! inputs) slugs)))
        event (fx/event-value "withdrawal"
                              [{"slug" (slug-of kumo) "reason_code" "rights" "statement" ""}])
        outcome (release/release!
                 (assoc next-release :push-fn
                        (fn [dir branch commit expected]
                          (is (= :published (:outcome (fx/publish-event! competitor event))))
                          (repo/push! dir branch commit expected))))]
    (is (= :requeue (:outcome outcome)))
    (is (= [(vec (sort [(slug-of merosu) (slug-of kumo)])) [(slug-of merosu)]] @calls))
    (is (= :published (:outcome (release/release! next-release))))
    (is (= [(slug-of merosu)]
           (mapv #(get % "slug") (get (:head-manifest (verified-chain clone)) "works"))))))

(deftest rights-policy-must-be-one-whole-document
  ;; a reader stopping at the first value would authorize — and hash —
  ;; bytes it never evaluated
  (let [reason (fn [^String s]
                 (try (release/rights-authority! (.getBytes s "UTF-8"))
                      :accepted
                      (catch clojure.lang.ExceptionInfo e
                        (:reason (ex-data e)))))]
    (is (= :policy-unreadable
           (reason (str "{:rights-publication :assessment-required} "
                        "{:rights-publication :blocked}"))))
    (is (= :policy-unreadable
           (reason "{:rights-publication :assessment-required} %%%")))
    (is (= :policy-unreadable
           (reason "{:rights-publication :assessment-required} garbage")))
    (is (= :policy-unreadable (reason "")))
    (testing "the exact single-value document still authorizes"
      (is (= "rights-publication-policy-v1"
             (:policy-id (release/rights-authority!
                          (.getBytes "{:rights-publication :assessment-required}\n"
                                     "UTF-8"))))))))

(defn- git-inputs! [dir & args]
  (let [result (apply process/sh {:dir (str dir) :out :string :err :string}
                      "git" args)]
    (when-not (zero? (:exit result))
      (throw (ex-info "assessment fixture git failed" {:args args :err (:err result)})))))

(defn- commit-assessment-inputs! [dir]
  (git-inputs! dir "init" "-q")
  (git-inputs! dir "add" "--" "source.json" "snapshot.json")
  (git-inputs! dir "-c" "user.name=assessment-fixture"
               "-c" "user.email=assessment@localhost"
               "commit" "-qm" "Record assessment inputs"))

(deftest release-input-errors-win-before-the-build-runs
  ;; every file input is preflighted; the aozora-root sentinel does not
  ;; exist, so any row that reached the build would fail with the
  ;; provenance gate's distinct error instead of its own
  (let [dir (fs/create-temp-dir {:prefix "za-release-cli"})
        write! (fn [name ^String content]
                 (let [path (str (fs/path dir name))]
                   (spit path content)
                   path))
        ks (vector-keys)
        snapshot-file (write! "snapshot.json"
                              (String. ^bytes (snapshot-bytes [(pd merosu)])
                                       "UTF-8"))
        base {:root (str (fs/path dir "store"))
              :aozora-root (str (fs/path dir "no-such-checkout"))
              :clj-toolchain-id "za-cli-fixture"
              :chain-clone (str (fs/path dir "no-such-clone"))
              :branch "main"
              :upstream-origin "https://forge.example/za/fixture-corpus.git"
              :assessment-source (write! "source.json"
                                         (String. ^bytes (:bytes (records/encode records/empty-source))
                                                  "UTF-8"))
              :as-of "2026-09-05"
              :assessment snapshot-file
              :policy (write! "policy.edn"
                              "{:rights-publication :assessment-required}")
              :release-pub (write! "release.pub"
                                   (str (get-in ks ["release" "pub"]) "\n"))
              :governance-pub (write! "governance.pub"
                                      (str (get-in ks ["governance" "pub"]) "\n"))
              :release-key (write! "release.seed"
                                   (get-in ks ["release" "seed"]))}
        error (fn [opts]
                (try (main/release! opts)
                     nil
                     (catch clojure.lang.ExceptionInfo e
                       (or (:reason (ex-data e)) (:option (ex-data e))
                           (ex-message e)))))]
    (commit-assessment-inputs! dir)
    (testing "the blocking production rights state refuses release"
      (is (= :rights-blocked
             (error (assoc base :policy
                           (write! "blocked.edn"
                                   "{:rights-publication :blocked-pending-assessment-migration}"))))))
    (testing "a malformed snapshot dies at decode"
      (is (= :parse-invalid
             (error (assoc base :assessment
                           (write! "dup.json"
                                   (str/replace (slurp snapshot-file)
                                                "{\"candidates\""
                                                "{\"candidates\":[],\"candidates\"")))))))
    (testing "malformed assessment source fails before source checkout access"
      (is (= :parse-invalid
             (error (assoc base :assessment-source (write! "broken-source.json" "{"))))))
    (testing "overlapping role keys are an invalid configuration"
      (is (= :overlapping-roles
             (error (assoc base :governance-pub (:release-pub base))))))
    (testing "a seed for the wrong role fails the correspondence probe"
      (is (= :seed-key-mismatch
             (error (assoc base :release-key
                           (write! "governance.seed"
                                   (get-in ks ["governance" "seed"])))))))
    (testing "a relative upstream origin is refused"
      (is (= :invalid-upstream-origin
             (error (assoc base :upstream-origin "mirrors/corpus.git")))))
    (testing "--limit is refused for release"
      (is (= "--limit" (error (assoc base :limit 5)))))
    (testing "a malformed signing seed is rejected without echoing the secret"
      (let [thrown (try (main/release!
                         (assoc base :release-key
                                (write! "sentinel.seed"
                                        "SECRETSENTINEL-this-is-not-hex")))
                        nil
                        (catch clojure.lang.ExceptionInfo e e))
            ;; the exact line the CLI prints to stderr on failure
            printed (str "error: " (ex-message thrown) " "
                         (pr-str (ex-data thrown)))]
        (is (= :malformed-release-key (:reason (ex-data thrown))))
        (is (not (str/includes? printed "SECRETSENTINEL")))))
    (testing "valid draft source and snapshot copies are not release authority"
      (doseq [[option original name] [[:assessment-source (:assessment-source base) "draft-source.json"]
                                      [:assessment snapshot-file "draft-snapshot.json"]]]
        (is (= :uncommitted-assessment-input
               (error (assoc base option (write! name (slurp original))))))))
    (testing "modified tracked inputs require committing their new bytes"
      (let [source-path (:assessment-source base)
            original (slurp source-path)
            changed (assoc records/empty-source "observations"
                           [{"id" "draft" "selector" "catalog-contributors" "slug" "draft"}])]
        (try
          (write! "source.json" (String. ^bytes (:bytes (records/encode changed)) "UTF-8"))
          (is (= :uncommitted-assessment-input (error base)))
          (finally (spit source-path original)))))
    (testing "a changed tracked snapshot also requires a new commit"
      (let [original (slurp snapshot-file)]
        (try
          (spit snapshot-file (str/replace original "2026-08-01" "2026-08-02"))
          (is (= :uncommitted-assessment-input (error base)))
          (finally (spit snapshot-file original)))))
    (testing "inputs from different owner repositories cannot form one release view"
      (let [other (fs/create-temp-dir {:prefix "assessment-other-repo"})]
        (fs/copy (:assessment-source base) (fs/path other "source.json"))
        (fs/copy snapshot-file (fs/path other "snapshot.json"))
        (commit-assessment-inputs! other)
        (is (= :assessment-input-revisions-differ
               (error (assoc base :assessment-source (str (fs/path other "source.json"))))))))
    (testing "unrelated tracked edits do not invalidate committed assessment authority"
      (write! "notes.txt" "unrelated tracked data")
      (git-inputs! dir "add" "--" "notes.txt")
      (git-inputs! dir "-c" "user.name=assessment-fixture"
                   "-c" "user.email=assessment@localhost"
                   "commit" "-qm" "Record unrelated fixture data")
      (write! "notes.txt" "unrelated local edit")
      (is (= "source git unavailable" (error base))))
    (testing "with every file input valid, the first failure is the
      provenance gate the corpus-reading checks sit behind — proof every
      file input was preflighted first"
      (is (= "source git unavailable" (error base))))))

(deftest stale-snapshot-is-refused-before-publication
  ;; the case slug totality alone cannot see: the catalog loses a
  ;; contributor while every slug survives, so the committed snapshot no
  ;; longer describes the corpus under release
  (let [translated (assoc merosu :contributors [{:person-id "000009"
                                                 :role "翻訳者"}])
        root (corpus/init-corpus! [translated kumo])
        dir (fs/create-temp-dir {:prefix "za-release-drift"})
        write! (fn [name ^String content]
                 (let [path (str (fs/path dir name))]
                   (spit path content)
                   path))
        ks (vector-keys)
        scaffold-bytes (fn []
                         (let [rows (csv/read-rows-from-string
                                     (:csv-text (catalog/read-catalog-zip
                                                 root)))]
                           (:bytes (snapshot/encode
                                    {:facts {}
                                     :candidates (scaffold/projection
                                                  rows (:candidates (select/select-candidates root rows)))}))))
        opts {:root (str (fs/path dir "store"))
              :aozora-root root
              :clj-toolchain-id "za-drift-fixture"
              :chain-clone (str (fs/path dir "no-such-clone"))
              :branch "main"
              :upstream-origin "https://forge.example/za/fixture-corpus.git"
              :assessment-source (write! "source.json"
                                         (String. ^bytes (:bytes (records/encode records/empty-source))
                                                  "UTF-8"))
              :as-of "2026-09-05"
              :assessment (let [path (str (fs/path dir "snapshot.json"))]
                            (fs/write-bytes path (scaffold-bytes))
                            path)
              :policy (write! "policy.edn"
                              "{:rights-publication :assessment-required}")
              :release-pub (write! "release.pub"
                                   (str (get-in ks ["release" "pub"]) "\n"))
              :governance-pub (write! "governance.pub"
                                      (str (get-in ks ["governance" "pub"]) "\n"))
              :release-key (write! "release.seed"
                                   (get-in ks ["release" "seed"]))}]
    (commit-assessment-inputs! dir)
    (testing "the scaffolded snapshot matches the corpus it came from"
      (is (nil? (main/release-preflight-drift opts))))
    (testing "a withdrawal committed during the build prevents publishing the captured view"
      (let [source-path (:assessment-source opts)
            original (fs/read-all-bytes source-path)
            finding {"id" "synthetic-death" "fact" (records/fact-key "person:000001" "death-year")
                     "value" 1900 "effective_date" "2020-01-01" "reviewed_at" "2020-01-01"
                     "assessor" "synthetic reviewer" "method" "synthetic evidence"
                     "basis" "Synthetic assertion for concurrent withdrawal coverage." "premises" []}
            reviewed (assoc records/empty-source "findings" [finding])
            current-source (main/source-provenance! root)]
        (try
          (fs/write-bytes source-path (:bytes (records/encode reviewed)))
          (commit-assessment-inputs! dir)
          (is (= :uncommitted-assessment-input
                 (with-redefs [main/publication-stages (constantly corpus/stage-set)
                               release/release! (fn [{:keys [build-works!]}] (build-works! []))
                               main/execute-build!
                               (fn [& _]
                                 (fs/write-bytes
                                  source-path
                                  (:bytes (records/encode
                                           (assoc reviewed "controls"
                                                  [{"id" "withdraw-synthetic" "kind" "withdrawal"
                                                    "target" "synthetic-death"}]))))
                                 (commit-assessment-inputs! dir)
                                 {"aozora_git_commit" current-source "works" {}})]
                   (try (main/release! opts)
                        nil
                        (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))))
          (finally
            (fs/write-bytes source-path original)
            (commit-assessment-inputs! dir)))))
    (testing "source movement between preflight and publication is refused"
      (let [before (main/source-provenance! root)
            error (with-redefs [main/publication-stages (constantly corpus/stage-set)
                                release/release! (fn [{:keys [build-works!]}] (build-works! []))
                                main/execute-build!
                                (fn [& _]
                                  (corpus/write-catalog! root
                                                         [(assoc translated :title "changed during build") kumo])
                                  (corpus/commit-corpus! root)
                                  {"aozora_git_commit" before "works" {}})]
                    (try (main/release! opts)
                         nil
                         (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))]
        (is (= :assessment-source-changed-during-build error))))
    (testing "dropping a contributor under a surviving slug is refused"
      (corpus/write-catalog! root [merosu kumo])
      (corpus/commit-corpus! root)
      (let [drift (main/release-preflight-drift opts)]
        (is (= 1 (:contributions-differ-count drift)))
        (is (= [(slug-of merosu)] (:contributions-differ-sample drift)))
        (is (= 0 (:only-in-checkout-count drift)))
        (is (= 0 (:only-in-snapshot-count drift))))
      (let [built? (atom false)
            published? (atom false)]
        (with-redefs [main/execute-build! (fn [& _]
                                            (reset! built? true)
                                            {"aozora_git_commit" (main/source-provenance! root)
                                             "works" {}})
                      release/release! (fn [{:keys [build-works!]}]
                                         (build-works! [])
                                         (reset! published? true))]
          (is (= :snapshot-regeneration-drift
                 (try (main/release! opts)
                      nil
                      (catch clojure.lang.ExceptionInfo e
                        (:reason (ex-data e))))))
          (is (true? @built?))
          (is (false? @published?)))))))

(deftest current-reliance-is-checked-once-after-every-build-attempt
  (let [root (corpus/init-corpus! [merosu kumo])
        dir (fs/create-temp-dir {:prefix "reliance-cli"})
        run (corpus/run-corpus! root (str (fs/create-dirs (fs/path dir "build"))))
        slug (slug-of merosu)
        report (run->report run)
        ks (vector-keys)
        write! (fn [name content]
                 (let [path (str (fs/path dir name))] (spit path content) path))
        declaration {"slug" slug
                     "source_content_hash" (get-in report ["works" slug "source_content_hash"])
                     "source_revision" (:commit run)
                     "observed_at" "2026-09-05" "decision_date" "2026-09-05"
                     "basis" "Synthetic official assertion for release-boundary coverage."
                     "catalog_sha256" (hash/sha256-string "catalog")
                     "card_sha256" (hash/sha256-string "card")
                     "file_sha256" (hash/sha256-string "file")
                     "rules_sha256" (hash/sha256-string "rules") "exception" nil}
        {:keys [clone]} (fx/make-repos!)
        opts {:concurrency 1 :root (str (fs/path dir "store")) :aozora-root root
              :evidence-root (str (fs/path dir "evidence"))
              :clj-toolchain-id "reliance-cli-test" :as-of "2026-09-06"
              :assessment-source (write! "source.json"
                                         (String. ^bytes (:bytes (records/encode
                                                                  (assoc records/empty-source "reliances" [declaration])))
                                                  "UTF-8"))
              :assessment (str (fs/path dir "snapshot.json"))
              :policy (write! "policy.edn" "{:rights-publication :assessment-required}")
              :release-pub (write! "release.pub" (str (get-in ks ["release" "pub"]) "\n"))
              :governance-pub (write! "governance.pub" (str (get-in ks ["governance" "pub"]) "\n"))
              :release-key (write! "release.seed" (get-in ks ["release" "seed"]))
              :chain-clone clone :branch fx/branch
              :upstream-origin "https://forge.example/fixture.git"}
        current (atom {slug {:state :aozora/available :reason nil}})
        checks (atom 0)
        published? (atom false)]
    (testing "refreshing official evidence preserves a recorded exception"
      (let [original (fs/read-all-bytes (:assessment-source opts))
            output (str (fs/path dir "refreshed.json"))]
        (try
          (fs/write-bytes (:assessment-source opts)
                          (:bytes (records/encode
                                   (assoc records/empty-source "reliances"
                                          [(assoc declaration "exception" "Unresolved specific restriction.")]))))
          (with-redefs [aozora/prepare! (fn [& _] declaration)]
            (main/aozora-reliance-prepare! (assoc opts :slug slug :out output)))
          (is (= "Unresolved specific restriction."
                 (get-in (:value (records/decode (fs/read-all-bytes output)))
                         ["reliances" 0 "exception"])))
          (finally (fs/write-bytes (:assessment-source opts) original)))))
    (with-redefs [aozora/check! (fn [& _] (swap! checks inc) @current)]
      (main/assessment-evaluate! (assoc opts :out (:assessment opts)))
      (commit-assessment-inputs! dir)
      (is (nil? (main/release-preflight-drift opts)))
      (testing "release builds only admitted artifacts; research stages and quarantined conversion are independent"
        (let [convert (get-in corpus/stage-set [:convert :f])
              selected-stages (atom (-> corpus/stage-set
                                        (assoc-in [:convert :stage-version] "quarantine-fault")
                                        (assoc-in [:convert :f]
                                                  (fn [{:keys [blob] :as context} inputs]
                                                    (when (str/includes? (get (json/read-json (String. ^bytes (blob (get inputs "aat")) "UTF-8")) "text")
                                                                         "蜘蛛")
                                                      (throw (ex-info "synthetic invalid converter input"
                                                                      {:reason :invalid-converter-input})))
                                                    (convert context inputs)))
                                        (assoc-in [:accountability :f]
                                                  (fn [& _] (throw (ex-info "research-only stage executed" {}))))))
              exported (str (fs/path dir "release-export"))]
          (with-redefs-fn {#'main/publication-stages (fn [_] @selected-stages)
                           #'main/build-stages (fn [_] @selected-stages)}
            (fn []
              (reset! checks 0)
              (is (= :published (:outcome (main/release! (assoc opts :out exported)))))
              (is (= 1 @checks))
              (let [manifest (:head-manifest (verified-chain clone))
                    report (json/read-json (slurp (str (fs/path exported "build.json"))))]
                (is (= [slug] (mapv #(get % "slug") (get manifest "works"))))
                (is (= [slug] (get report "selected_slugs")))
                (is (= #{slug} (set (keys (get report "works")))))
                (is (not (contains? (get report "stages") "accountability")))
                (is (not (contains? (get-in report ["works" slug]) "source-accountability")))
                (is (not (fs/exists? (fs/path exported slug "source-accountability.json"))))
                (is (not (contains? (get manifest "toolchain") "source-accountability"))))
              (swap! selected-stages assoc-in [:accountability :stage-version] "research-only-change")
              (reset! checks 0)
              (is (= :already-published (:outcome (main/release! opts))))
              (is (= 1 @checks))
              (swap! selected-stages assoc :accountability (:accountability corpus/stage-set))
              (is (= [:invalid-converter-input]
                     (try (main/build! opts) nil
                          (catch clojure.lang.ExceptionInfo e
                            (mapv #(get-in % [:data :reason]) (:failures (ex-data e)))))))))))
      (testing "an entirely withdrawn artifact demand does not resolve unused toolchains"
        (is (= :published
               (:outcome (fx/publish-event!
                          clone (fx/event-value "withdrawal"
                                                [{"slug" slug "reason_code" "rights" "statement" ""}])))))
        (with-redefs [main/publication-stages (fn [_] (throw (ex-info "unused toolchain resolved" {})))]
          (is (= :published (:outcome (main/release! opts)))))
        (let [manifest (:head-manifest (verified-chain clone))]
          (is (empty? (get manifest "works")))
          (is (empty? (get manifest "toolchain")))))
      (testing "every assembly attempt receives its own post-build live check"
        (let [events (atom [])]
          (with-redefs [main/publication-stages (constantly corpus/stage-set)
                        main/execute-build! (fn [& _] (swap! events conj :build) report)
                        aozora/check! (fn [& _] (swap! events conj :check) @current)
                        release/release! (fn [{:keys [build-works!]}]
                                           (build-works! [slug])
                                           (swap! events conj :retry)
                                           (build-works! [slug])
                                           (swap! events conj :publish)
                                           {:outcome :published})]
            (is (= :published (:outcome (main/release! opts)))))
          (is (= [:build :check :retry :build :check :publish] @events))))
      (testing "withdrawal after preflight refuses before publication"
        (with-redefs [main/publication-stages (constantly corpus/stage-set)
                      main/execute-build! (fn [& _]
                                            (reset! current {slug {:state :aozora/unavailable :reason :aozora/protected-card}})
                                            report)
                      release/release! (fn [{:keys [build-works!]}]
                                         (build-works! [slug])
                                         (reset! published? true))]
          (is (= :snapshot-regeneration-drift
                 (try (main/release! opts) nil
                      (catch clojure.lang.ExceptionInfo e (or (:reason (ex-data e)) (ex-message e))))))
          (is (false? @published?))))
      (testing "a stale accepted snapshot builds before the final check refuses publication"
        (let [events (atom [])]
          (with-redefs [main/publication-stages (constantly corpus/stage-set)
                        main/execute-build! (fn [& _] (swap! events conj :build) report)
                        aozora/check! (fn [& _] (swap! events conj :check) @current)
                        release/release! (fn [{:keys [build-works!]}]
                                           (build-works! [slug])
                                           (swap! events conj :publish)
                                           {:outcome :published})]
            (is (= :snapshot-regeneration-drift
                   (try (main/release! opts) nil
                        (catch clojure.lang.ExceptionInfo e (or (:reason (ex-data e)) (ex-message e)))))))
          (is (= [:build :check] @events)))))))
