(ns abc.tools.aozora-replay-test
  (:require [abc.git :as abc-git]
            [abc.sim.model]
            [abc.sim.render :as sim-render]
            [abc.tools.aozora-replay :as replay]
            [abc.tools.json :as abc-json]
            [clojure.java.io :as io]
            [clojure.java.shell]
            [clojure.string]
            [clojure.test :refer [deftest is testing]])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(defn- temp-dir [prefix]
  (.toFile (Files/createTempDirectory prefix (make-array FileAttribute 0))))

(defn- delete-recursive [^java.io.File f]
  (when (.isDirectory f)
    (doseq [child (.listFiles f)] (delete-recursive child)))
  (.delete f))

(deftest git-star-honors-dir-and-captures-nonzero-test
  (let [dir (temp-dir "abc-replay-git-star")
        git* (ns-resolve 'abc.tools.aozora-replay 'git*)]
    (try
      (is (zero? (:exit (git* ["init" "--quiet"] {:dir dir}))))
      (let [{:keys [exit out]} (git* ["rev-parse" "--show-toplevel"]
                                     {:dir dir})]
        (is (zero? exit))
        (is (= (.getCanonicalPath dir)
               (.getCanonicalPath (io/file (clojure.string/trim out))))))
      (is (pos? (:exit (git* ["rev-parse" "--verify" "missing-ref"]
                             {:dir dir}))))
      (finally
        (delete-recursive dir)))))

(deftest locked-pin-test
  (let [dir (temp-dir "abc-replay-lock")
        write! (fn [name value]
                 (let [f (io/file dir name)]
                   (abc-json/write-deterministic-json-file! f value)
                   (str f)))]
    (try
      (is (= (apply str (repeat 40 "a"))
             (replay/locked-pin
              (write! "good.lock"
                      {"nodes" {"aozorabunko-src"
                                {"locked" {"rev" (apply str (repeat 40 "a"))}}}}))))
      (is (thrown? clojure.lang.ExceptionInfo
                   (replay/locked-pin (write! "bad.lock" {"nodes" {}}))))
      (finally (delete-recursive dir)))))

(deftest catalog-bytes-fault-test
  (let [m (abc.sim.model/bootstrap 1)
        good-csv (sim-render/rows->csv (sim-render/model->rows m))]
    (is (nil? (replay/catalog-bytes-fault (sim-render/csv->zip-bytes good-csv))))
    (is (= "no-data-rows"
           (replay/catalog-bytes-fault
            (sim-render/csv->zip-bytes (sim-render/rows->csv [])))))
    (is (= "no-csv-entry"
           (replay/catalog-bytes-fault
            (sim-render/csv->zip-bytes "x" {:no-entry? true}))))
    (is (= "unreadable-zip"
           (replay/catalog-bytes-fault (.getBytes "this is not a zip" "UTF-8"))))))

(defn- pair [prev cur period split-count]
  {"previous_ref" prev "current_ref" cur "period" period
   "status" "ok"
   "drift_summary" {"split_candidates" split-count}
   "ingest" {"works_written" 1 "works_skipped" 0 "skipped_work_ids" []
             "persons_written" 1 "person_conflicts" []}})

(defn- doc [pin pairs excluded]
  {"baseline_format" replay/baseline-format
   "remote_url" replay/default-remote-url
   "pin_rev" pin
   "zip_path" replay/default-zip-path
   "sample_period" "year"
   "excluded" excluded
   "pairs" pairs})

(deftest classify-diff-test
  (let [p1 (pair "r0" "r1" "2023" 0)
        p2 (pair "r1" "r2" "2024" 1)
        old (doc "pinA" [p1 p2] [])]
    (testing "unchanged"
      (is (= :unchanged (:verdict (replay/classify-diff old old)))))
    (testing "configuration-change wins over everything"
      (is (= :configuration-change
             (:verdict (replay/classify-diff
                        old (assoc (doc "pinB" [p1 p2] []) "sample_period" "month"))))))
    (testing "any change with unchanged pin is behavioral"
      (is (= :behavioral-change
             (:verdict (replay/classify-diff
                        old (doc "pinA" [p1 (pair "r1" "r2" "2024" 2)] []))))))
    (testing "pin-bump-shaped: strict final replacement + append"
      (is (= :pin-bump-shaped
             (:verdict (replay/classify-diff
                        old (doc "pinB" [p1 (pair "r1" "r3" "2024" 1)
                                         (pair "r3" "r4" "2025" 0)] []))))))
    (testing "digest change on unchanged final refs is NOT pin-bump-shaped"
      (is (= :behavioral-change
             (:verdict (replay/classify-diff
                        old (doc "pinB" [p1 (pair "r1" "r2" "2024" 9)] []))))))
    (testing "final replacement that moves to a different period is NOT pin-bump-shaped"
      (is (= :behavioral-change
             (:verdict (replay/classify-diff
                        old (doc "pinB" [p1 (pair "r1" "r3" "2025" 1)] []))))))
    (testing "historical pair change is behavioral even with pin bump"
      (is (= :behavioral-change
             (:verdict (replay/classify-diff
                        old (doc "pinB" [(pair "r0" "r1" "2023" 5) p2] []))))))
    (testing "exclusion for a historical period is behavioral"
      (is (= :behavioral-change
             (:verdict (replay/classify-diff
                        old (doc "pinB" [p1 p2]
                                 [{"ref" "rX" "period" "2023" "reason" "no-csv-entry"}]))))))
    (testing "exclusion for a NEW period is pin-bump-shaped"
      (is (= :pin-bump-shaped
             (:verdict (replay/classify-diff
                        old (doc "pinB" [p1 p2]
                                 [{"ref" "rX" "period" "2025" "reason" "no-csv-entry"}]))))))
    (testing "pair-changes classification"
      (is (= ["unchanged" "replaced" "added"]
             (mapv #(get % "change")
                   (:pair-changes (replay/classify-diff
                                   old (doc "pinB" [p1 (pair "r1" "r3" "2024" 1)
                                                    (pair "r3" "r4" "2025" 0)] [])))))))))

(deftest pair-digest-test
  (let [digest (replay/pair-digest
                {"cur-sha" "2024"}
                {:previous_ref "prev-sha" :current_ref "cur-sha" :status "ok"
                 :drift {"summary" {"split_candidates" 2}}
                 :current_ingest {:works-written 3 :works-skipped 1
                                  :skipped-work-ids ["000101"]
                                  :persons-written 4
                                  :person-conflicts [{"person_id" "000009"
                                                      "chosen_work_id" "000101"
                                                      "work_ids" ["000101" "000102"]}]}})]
    (is (= {"previous_ref" "prev-sha" "current_ref" "cur-sha" "period" "2024"
            "status" "ok"
            "drift_summary" {"split_candidates" 2}
            "ingest" {"works_written" 3 "works_skipped" 1
                      "skipped_work_ids" ["000101"] "persons_written" 4
                      "person_conflicts" ["000009"]}}
           digest))))

(defn- commit-state! [git dir family instant]
  (sim-render/commit-zip-at!
   git dir
   (sim-render/csv->zip-bytes
    (sim-render/rows->csv
     (sim-render/model->rows
      (:model (abc.sim.model/apply-event
               (abc.sim.model/bootstrap 1)
               {:event/type :edit-person :pid "000001"
                :field :family_name :value family})))))
   (str "state " family) instant))

(deftest replay-plumbing-integration-test
  (let [repo-dir (temp-dir "abc-replay-repo")
        work-dir (temp-dir "abc-replay-work")
        baseline (io/file (temp-dir "abc-replay-base") "baseline.json")
        git (sim-render/init-repo! repo-dir)]
    (try
      (commit-state! git repo-dir "壱" "2023-03-01T00:00:00Z")
      (commit-state! git repo-dir "弐" "2024-03-01T00:00:00Z")
      (commit-state! git repo-dir "参" "2025-03-01T00:00:00Z")
      (let [opts {:aozora-repo (str repo-dir)
                  :remote-url replay/default-remote-url
                  :sample-period "year"
                  :zip-path sim-render/zip-path
                  :work-dir (str work-dir)}
            doc1 (replay/replay-doc! opts)]
        (testing "--update writes a well-formed baseline; immediate re-run is :unchanged"
          (is (= ["2024" "2025"] (mapv #(get % "period") (get doc1 "pairs"))))
          (is (= [] (get doc1 "excluded")))
          (abc-json/write-deterministic-json-file! baseline doc1)
          (is (= :unchanged
                 (:verdict (replay/classify-diff
                            (abc-json/read-json-file (str baseline))
                            (replay/replay-doc! opts))))))
        (testing "a new upstream-like commit classifies as pin-bump-shaped"
          (commit-state! git repo-dir "肆" "2026-03-01T00:00:00Z")
          (is (= :pin-bump-shaped
                 (:verdict (replay/classify-diff
                            doc1 (replay/replay-doc! opts))))))
        (testing "a doctored historical pair classifies as behavioral-change"
          (let [doctored (update-in doc1 ["pairs" 0 "drift_summary"
                                          "metadata_corrections"]
                                    (fnil inc 0))
                fresh (replay/replay-doc! opts)]
            (is (= :behavioral-change
                   (:verdict (replay/classify-diff doctored fresh)))))))
      (finally
        (.close git)
        (delete-recursive repo-dir)
        (delete-recursive work-dir)
        (delete-recursive (.getParentFile baseline))))))

(deftest ensure-clone-provenance-test
  (let [origin (temp-dir "abc-replay-origin")
        cache (temp-dir "abc-replay-cache")]
    (try
      (let [git (sim-render/init-repo! origin)]
        (commit-state! git origin "壱" "2023-03-01T00:00:00Z")
        (.close git))
      ;; a cache cloned from one URL...
      (clojure.java.shell/sh "git" "clone" "--no-checkout"
                             (str "file://" origin) (str (io/file cache "clone")))
      ;; ...must be refused when the replay expects another
      (let [e (try (replay/ensure-clone!
                    {:cache-dir (str (io/file cache "clone"))
                     :remote-url "https://example.invalid/other.git"})
                   nil
                   (catch clojure.lang.ExceptionInfo e e))]
        (is (some? e))
        (is (= "https://example.invalid/other.git"
               (:expected-url (ex-data e))))
        (is (string? (:actual-url (ex-data e)))))
      (finally
        (delete-recursive origin)
        (delete-recursive cache)))))

(deftest ensure-clone-preserves-git-entry-presence-test
  (let [root (temp-dir "abc-replay-git-kind")
        directory-cache (io/file root "directory-cache")
        file-cache (io/file root "file-cache")
        calls (atom [])
        verify-var (ns-resolve 'abc.tools.aozora-replay 'verify-origin!)
        git-var (ns-resolve 'abc.tools.aozora-replay 'git!)]
    (try
      (.mkdirs (io/file directory-cache ".git"))
      (.mkdirs file-cache)
      (spit (io/file file-cache ".git") "not a directory")
      (with-redefs-fn
        {verify-var (fn [& args] (swap! calls conj [:verify args]))
         git-var (fn [& args] (swap! calls conj [:git args]))}
        #(do
           (replay/ensure-clone! {:cache-dir (str directory-cache)
                                  :remote-url "https://example.invalid/repo.git"})
           (replay/ensure-clone! {:cache-dir (str file-cache)
                                  :remote-url "https://example.invalid/repo.git"})))
      (is (= [:verify :verify] (mapv first @calls)))
      (finally
        (delete-recursive root)))))

(deftest resolve-options-canonicalizes-symlinked-parent-test
  (let [root (temp-dir "abc-replay-canonical")
        real-parent (io/file root "real")
        linked-parent (io/file root "linked")
        baseline (io/file real-parent "baseline.json")]
    (try
      (.mkdirs real-parent)
      (spit baseline "{}")
      (Files/createSymbolicLink (.toPath linked-parent) (.toPath real-parent)
                                (make-array FileAttribute 0))
      (with-redefs-fn
        {#'replay/default-baseline-path (str baseline)
         #'replay/locked-pin (constantly (apply str (repeat 40 "a")))}
        #(is (thrown? clojure.lang.ExceptionInfo
                      (replay/resolve-options
                       {:update true
                        :sample-period "month"
                        :baseline (str (io/file linked-parent "baseline.json"))}))))
      (finally
        (.delete linked-parent)
        (delete-recursive root)))))

(deftest resolve-options-guards-test
  (let [pin (replay/locked-pin "flake.lock")]
    (testing "exactly one of --check/--update"
      (is (thrown? clojure.lang.ExceptionInfo
                   (replay/resolve-options {:sample-period "year"})))
      (is (thrown? clojure.lang.ExceptionInfo
                   (replay/resolve-options {:check true :update true
                                            :sample-period "year"}))))
    (testing "default-baseline --update refuses non-default sampling/window"
      (is (thrown? clojure.lang.ExceptionInfo
                   (replay/resolve-options {:update true
                                            :sample-period "month"})))
      (is (thrown? clojure.lang.ExceptionInfo
                   (replay/resolve-options {:update true
                                            :sample-period "year"
                                            :from-ref "somewhere"})))
      (is (thrown? clojure.lang.ExceptionInfo
                   (replay/resolve-options {:update true
                                            :sample-period "year"
                                            :to-ref (apply str (repeat 40 "d"))}))))
    (testing "explicit ad-hoc baseline is permitted with any flags"
      (is (= "month" (:sample-period
                      (replay/resolve-options {:update true
                                               :sample-period "month"
                                               :baseline "/tmp/adhoc.json"})))))
    (testing "defaults resolve from runtime context"
      (let [r (replay/resolve-options {:check true :sample-period "year"})]
        (is (= pin (:to-ref r)))
        (is (= replay/default-baseline-path (:baseline r)))
        (is (= replay/default-remote-url (:remote-url r)))
        (is (= replay/default-zip-path (:zip-path r)))))))

(deftest replay-missing-at-ref-exclusion-test
  (let [repo-dir (temp-dir "abc-replay-noref")
        work-dir (temp-dir "abc-replay-noref-work")
        git (sim-render/init-repo! repo-dir)]
    (try
      ;; from-ref commit predates the ZIP path entirely
      (let [c-nozip (sim-render/commit-file-at! git repo-dir "README.md"
                                                "no zip yet" "init"
                                                "2022-01-01T00:00:00Z")]
        (commit-state! git repo-dir "壱" "2023-03-01T00:00:00Z")
        (commit-state! git repo-dir "弐" "2024-03-01T00:00:00Z")
        (let [doc (replay/replay-doc! {:aozora-repo (str repo-dir)
                                       :remote-url replay/default-remote-url
                                       :from-ref (.getName c-nozip)
                                       :sample-period "year"
                                       :zip-path sim-render/zip-path
                                       :work-dir (str work-dir)})]
          (is (= [{"ref" (.getName c-nozip) "period" nil
                   "reason" "missing-at-ref"}]
                 (get doc "excluded"))
              "path absent in the historical tree is a pinned exclusion")
          (is (= ["2024"] (mapv #(get % "period") (get doc "pairs")))
              "pairing runs over the survivors")))
      (finally
        (.close git)
        (delete-recursive repo-dir)
        (delete-recursive work-dir)))))

(deftest ensure-blob-bytes-missing-object-is-loud-test
  ;; Deleting the loose blob fabricates a promised-but-absent object; the
  ;; repo has no promisor remote, so the tier must be LOUD, never an
  ;; exclusion.
  (let [repo-dir (temp-dir "abc-replay-missing-obj")
        git (sim-render/init-repo! repo-dir)]
    (try
      (let [c (commit-state! git repo-dir "壱" "2023-03-01T00:00:00Z")
            blob-sha (clojure.string/trim
                      (:out (clojure.java.shell/sh
                             "git" "-C" (str repo-dir) "rev-parse"
                             (str (.getName c) ":" sim-render/zip-path))))
            obj (io/file repo-dir ".git" "objects"
                         (subs blob-sha 0 2) (subs blob-sha 2))]
        (is (.exists obj) "fresh commits leave loose objects")
        (is (.delete obj))
        (let [repo (abc-git/load-git-repo (str repo-dir))]
          (try
            (is (= :missing-object
                   (replay/blob-availability repo (.getName c)
                                             sim-render/zip-path)))
            (let [e (try (replay/ensure-blob-bytes repo (str repo-dir)
                                                   (.getName c)
                                                   sim-render/zip-path)
                         nil
                         (catch clojure.lang.ExceptionInfo e e))]
              (is (some? e))
              (is (= :missing-local-object (:cause-tier (ex-data e)))))
            (finally (.close repo)))))
      (finally
        (.close git)
        (delete-recursive repo-dir)))))

(def ^:private drift-summary-required-keys
  ["persons_previous" "persons_current" "works_previous" "works_current"
   "added_person_ids" "removed_person_ids" "metadata_corrections"
   "contributor_edge_additions" "contributor_edge_removals"
   "contributor_edge_replacements" "split_candidates" "merge_candidates"
   "ambiguous_replacements"])

(deftest baseline-pin-coupling-test
  (let [doc (abc-json/read-json-file replay/default-baseline-path)
        pairs (vec (get doc "pairs"))
        excluded (vec (get doc "excluded"))
        sha? (fn [s] (and (string? s) (re-matches #"[0-9a-f]{40}" s)))
        count? (fn [v] (and (int? v) (<= 0 v)))]
    (testing "header shape"
      (is (= replay/baseline-format (get doc "baseline_format")))
      (is (= replay/default-zip-path (get doc "zip_path")))
      (is (= replay/default-remote-url (get doc "remote_url")))
      (is (= "year" (get doc "sample_period")))
      (is (sha? (get doc "pin_rev"))))
    (testing "pair shape"
      (is (seq pairs) "committed baseline must contain pairs")
      (is (= (mapv #(get % "period") pairs)
             (vec (sort (mapv #(get % "period") pairs))))
          "pairs sorted by period")
      (doseq [p pairs]
        (is (sha? (get p "previous_ref")) (pr-str p))
        (is (sha? (get p "current_ref")) (pr-str p))
        (is (re-matches #"\d{4}(-\d{2})?" (or (get p "period") ""))
            "committed pairs carry non-nil period keys")
        (is (contains? #{"ok" "validation_failed"} (get p "status")))
        (doseq [k drift-summary-required-keys]
          (is (count? (get-in p ["drift_summary" k]))
              (str "drift_summary." k " in " (pr-str (get p "period")))))
        (let [ingest (get p "ingest")]
          (is (count? (get ingest "works_written")) (pr-str p))
          (is (count? (get ingest "works_skipped")) (pr-str p))
          (is (count? (get ingest "persons_written")) (pr-str p))
          (is (vector? (get ingest "skipped_work_ids")) (pr-str p))
          (is (every? string? (get ingest "skipped_work_ids")) (pr-str p))
          (is (vector? (get ingest "person_conflicts")) (pr-str p))
          (is (every? string? (get ingest "person_conflicts")) (pr-str p)))))
    (testing "exclusion shape: unique, complete, known reasons"
      (is (= (count excluded) (count (distinct (map #(get % "ref") excluded))))
          "no duplicate excluded refs")
      (doseq [e excluded]
        (is (sha? (get e "ref")) (pr-str e))
        (is (contains? e "period") (pr-str e))
        (is (contains? #{"missing-at-ref" "unreadable-zip"
                         "no-csv-entry" "no-data-rows"}
                       (get e "reason")))))
    (testing "pair-chain contiguity and pin tail"
      (is (= (get doc "pin_rev") (get (peek pairs) "current_ref"))
          "the pin must be the final pair's current_ref")
      (doseq [[a b] (partition 2 1 pairs)]
        (is (= (get a "current_ref") (get b "previous_ref"))
            (str "chain break between " (get a "period")
                 " and " (get b "period")))))
    (testing "pin coupling: baseline pin == abc/flake.lock pin"
      (is (= (replay/locked-pin "flake.lock") (get doc "pin_rev"))
          (str "baseline pin_rev disagrees with abc/flake.lock — after a pin "
               "bump, re-run `just replay-aozora-update`, adjudicate the "
               "diff, and commit the new baseline")))
    (testing "root flake.lock agrees with abc/flake.lock"
      (is (= (replay/locked-pin "flake.lock")
             (replay/locked-pin "../flake.lock"))
          "partial pin bump: abc/flake.lock and the root flake.lock carry different aozorabunko-src revs"))))
