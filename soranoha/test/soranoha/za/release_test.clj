(ns soranoha.za.release-test
  "Automated release-driver acceptance over a local origin: consecutive
  scheduled invocations across real upstream movement publish releases the
  published checker verifies end-to-end; an unmoved upstream is the
  scheduled no-op; a malformed assessment snapshot publishes nothing."
  (:require [babashka.fs :as fs]
            [charred.api :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [soranoha.core.hash :as hash]
            [soranoha.main :as main]
            [soranoha.snh.decode :as decode]
            [soranoha.snh.fixture :as fx]
            [soranoha.snh.verify :as verify]
            [soranoha.snh.view :as view]
            [soranoha.za.corpus :as corpus]
            [soranoha.za.release :as release]))

;; --- fixture corpus works ---------------------------------------------------

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

;; --- driver inputs -----------------------------------------------------------

(def ^:private policy-hash (hash/sha256-string "za fixture policy"))

(defn- snapshot-bytes ^bytes [candidates]
  (:bytes (decode/encode "assessment-snapshot"
                         {"schema" "snh-assessment-snapshot/1"
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
                  (:stage-coordinates run))
   "works" (into {}
                 (map (fn [[slug {:keys [outputs]}]]
                        [slug {"plaintext" (get-in outputs [:render "plaintext"])
                               "tei" (get-in outputs [:render "tei"])
                               "tei-validation"
                               (get-in outputs [:validate "tei-validation"])
                               "source_content_hash"
                               (get (corpus/source-facts run slug)
                                    "work_content_hash")}]))
                 (:results run))})

(defn- drive-report! [clone report cas-dir snapshot]
  (release/release!
   {:report report
    :cas-dir cas-dir
    :upstream-origin "https://forge.example/za/fixture-corpus.git"
    :selection-params {}
    :policy-id "za-fixture-policy-v1"
    :policy-hash policy-hash
    :snapshot-bytes snapshot
    :clone clone
    :branch fx/branch
    :pinned-keys (fx/pinned-keys)
    :sign-release fx/sign-release}))

(defn- drive! [clone run snapshot]
  (drive-report! clone (run->report run) (:cas-dir run) snapshot))

(defn- verified-chain [clone]
  (verify/verify-repository-at (view/git-view clone)
                               (fx/head-of clone)
                               (fx/pinned-keys)))

(defn- temp-store! [] (str (fs/create-temp-dir {:prefix "za-release-store"})))

;; --- acceptance --------------------------------------------------------------

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
        outcome (try (drive-report! clone broken (:cas-dir run)
                                    (snapshot-bytes [(pd merosu)]))
                     nil
                     (catch clojure.lang.ExceptionInfo e (ex-data e)))]
    (is (= :selection-works-mismatch (:reason outcome)))
    (is (= [(slug-of kumo)] (:selected-only outcome)))
    (is (= head-before (fx/head-of clone)))))

;; --- CLI boundary ------------------------------------------------------------

(defn- vector-keys []
  (get (json/read-json (slurp (io/resource "snh/vectors/signature-vectors.json")))
       "keys"))

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
    (testing "with every input valid, the first failure is the build's own
      provenance gate — proof the preflight ran to completion first"
      (is (= "source git unavailable" (error base))))))
