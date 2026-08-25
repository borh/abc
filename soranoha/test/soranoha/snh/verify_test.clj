(ns soranoha.snh.verify-test
  "Verifier invariant fixtures: for each chain rule a passing case (the
  lifecycle chains built through the transaction) and a failing case (a
  deliberately crafted commit). Covers merge rejection, chain replacement,
  genesis form, head linkage, tree reachability at prescribed paths,
  signature role binding, withdrawal monotonicity, coordinate immutability
  under governance, and the archive-verification report contract."
  (:require [clojure.test :refer [deftest is testing]]
            [soranoha.core.hash :as hash]
            [soranoha.snh.fixture :as fx]
            [soranoha.snh.repo :as repo]
            [soranoha.snh.sign :as sign]
            [soranoha.snh.verify :as verify]
            [soranoha.snh.view :as view]))

(def slugs ["hashire_merosu_000035_1567" "kumo_no_ito_000879_92"])
(def base {:admitted slugs})

(defn- reason-at
  "Verification outcome at `commit`: the failure reason keyword, or :valid."
  [clone commit]
  (try (verify/verify-repository-at (view/git-view clone) commit (fx/pinned-keys))
       :valid
       (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))

(defn- published-repos!
  "Origin/clone with a two-release chain; returns the fixture map plus
  {:head-commit :head}."
  []
  (let [{:keys [clone] :as repos} (fx/make-repos!)]
    (fx/publish! clone base)
    (fx/publish! clone (assoc base :selection-params {"config" "fixture" "round" 2}))
    (let [head-commit (fx/head-of clone)]
      (assoc repos :head-commit head-commit
             :head (fx/manifest-at clone head-commit)))))

(deftest merge-commits-are-rejected
  ;; a merge whose second parent carries the old authoritative head would
  ;; otherwise let a replacement chain ride the first-parent line
  (let [{:keys [clone head-commit head init-commit]} (published-repos!)
        {:keys [commit]} (fx/craft-release!
                          clone {:parents [head-commit init-commit]
                                 :base-tree-of head-commit
                                 :manifest-value
                                 (assoc (:value head) "prev_manifest" (:hex head)
                                        "selection_params" {"config" "merge"})})]
    (is (= :merge-commit (reason-at clone commit)))))

(deftest chain-replacement-with-zero-prev-is-rejected
  (let [{:keys [clone head-commit head]} (published-repos!)
        {:keys [commit]} (fx/craft-release!
                          clone {:parents [head-commit]
                                 :base-tree-of head-commit
                                 :manifest-value
                                 (assoc (:value head)
                                        "prev_manifest" sign/zero-head-hex
                                        "withdrawn" []
                                        "governance_event" nil
                                        "selection_params" {"config" "replacement"})})]
    (is (= :prev-manifest-mismatch (reason-at clone commit)))))

(deftest genesis-must-carry-explicit-empty-state
  (let [{:keys [clone init-commit]} (fx/make-repos!)
        assemble (fx/make-assemble base)
        {:keys [core blobs]} (assemble nil)
        bad-genesis (assoc core
                           "prev_manifest" sign/zero-head-hex
                           "governance_event" nil
                           "withdrawn" [{"slug" "phantom_000000_1"
                                         "event" (str "snh:1:governance-event:"
                                                      (apply str (repeat 64 "a")))}])
        {:keys [commit]} (fx/craft-release!
                          clone {:parents [init-commit]
                                 :base-tree-of init-commit
                                 :manifest-value bad-genesis
                                 :extra-files
                                 (into {} (map (fn [[hex bytes]]
                                                 [(verify/blob-path hex) bytes]))
                                       blobs)})]
    ;; the crafted withdrawn entry also has no event blob; the closure check
    ;; fires before the genesis-form check, and either way the commit fails
    (is (contains? #{:genesis-has-withdrawn :missing-blob :works-not-admitted-minus-withdrawn}
                   (reason-at clone commit)))))

(deftest head-must-advance-and-match-the-stored-manifest
  (let [{:keys [clone head-commit head]} (published-repos!)]
    (testing "a commit whose head does not advance"
      (let [commit (repo/write-commit!
                    clone {:parents [head-commit]
                           :base-tree-of head-commit
                           :files {"unrelated.txt" (.getBytes "x" "UTF-8")}})]
        (is (= :head-not-advanced (reason-at clone commit)))))
    (testing "a head naming a manifest that is not stored"
      (let [ghost (apply str (repeat 64 "b"))
            commit (repo/write-commit!
                    clone {:parents [head-commit]
                           :base-tree-of head-commit
                           :files {verify/head-path (sign/hex64-lf-bytes ghost)}})]
        (is (= :missing-manifest (reason-at clone commit)))))
    (testing "a zero head after genesis"
      (let [commit (repo/write-commit!
                    clone {:parents [head-commit]
                           :base-tree-of head-commit
                           :files {verify/head-path
                                   (sign/hex64-lf-bytes sign/zero-head-hex)}})]
        (is (= :zero-head-after-genesis (reason-at clone commit)))))))

(deftest blobs-count-only-at-their-prescribed-path
  ;; the same bytes reachable elsewhere in the same tree do not satisfy a
  ;; read at the prescribed sharded path; variant v3 guarantees the work
  ;; blobs are absent from the accumulated tree of earlier releases
  (let [{:keys [clone head-commit head]} (published-repos!)
        assemble (fx/make-assemble (assoc base :variant "v3" :selection-params
                                          {"config" "fixture" "round" 3}))
        {:keys [core blobs]} (assemble (:value head))
        manifest (assoc core "prev_manifest" (:hex head)
                        "withdrawn" [] "governance_event" nil)
        stray-hex (hash/sha256-bytes
                   (fx/work-blob-bytes "plaintext" (first slugs) "v3"))
        files (into {"attic/misplaced-blob" (get blobs stray-hex)}
                    (map (fn [[hex bytes]] [(verify/blob-path hex) bytes]))
                    (dissoc blobs stray-hex))
        {:keys [commit]} (fx/craft-release!
                          clone {:parents [head-commit]
                                 :base-tree-of head-commit
                                 :manifest-value manifest
                                 :extra-files files})]
    (is (= :missing-blob (reason-at clone commit)))))

(deftest signatures-are-role-bound-per-commit
  (let [{:keys [clone head-commit head]} (published-repos!)
        next-manifest (assoc (:value head)
                             "prev_manifest" (:hex head)
                             "selection_params" {"config" "fixture" "round" 3})]
    (testing "a manifest signed by the governance key fails"
      (let [{:keys [commit]} (fx/craft-release!
                              clone {:parents [head-commit]
                                     :base-tree-of head-commit
                                     :manifest-value next-manifest
                                     :sign-fn (fn [hex]
                                                (fx/sign-event-with-release-key hex))})]
        ;; release key signing the event-domain message: wrong domain
        (is (= :signature-invalid (reason-at clone commit)))))
    (testing "a truncated signature file fails"
      (let [{:keys [commit hex]} (fx/craft-release!
                                  clone {:parents [head-commit]
                                         :base-tree-of head-commit
                                         :manifest-value next-manifest})]
        (let [broken (repo/write-commit!
                      clone {:parents [head-commit]
                             :base-tree-of commit
                             :files {(verify/manifest-sig-path hex)
                                     (byte-array 63)}})]
          (is (= :malformed-signature (reason-at clone broken))))))))

(deftest withdrawn-set-is-monotonic-and-event-backed
  (let [{:keys [clone]} (published-repos!)
        _ (fx/publish-event! clone (fx/event-value
                                    "withdrawal"
                                    [{"slug" (second slugs)
                                      "reason_code" "rights"
                                      "statement" ""}]))
        withdrawal-head (fx/manifest-at clone (fx/head-of clone))
        original-event (get-in (:value withdrawal-head) ["withdrawn" 0 "event"])
        _ (fx/publish-event! clone (fx/event-value
                                    "event-amendment"
                                    [{"slug" (second slugs)
                                      "reason_code" "rights"
                                      "statement" "Corrected."
                                      "amends" original-event}]))
        head-commit (fx/head-of clone)
        head (fx/manifest-at clone head-commit)]
    (testing "the lifecycle chain with withdrawal + amendment verifies"
      (is (= :valid (reason-at clone head-commit))))
    (testing "silently dropping a withdrawn entry fails"
      (let [assemble (fx/make-assemble (assoc base :selection-params
                                              {"config" "fixture" "round" 4}))
            {:keys [core]} (assemble nil) ;; nil head: works include slug-b
            manifest (assoc core "prev_manifest" (:hex head)
                            "withdrawn" [] "governance_event" nil)
            {:keys [commit]} (fx/craft-release!
                              clone {:parents [head-commit]
                                     :base-tree-of head-commit
                                     :manifest-value manifest})]
        (is (= :withdrawn-not-monotonic (reason-at clone commit)))))
    (testing "changing a withdrawn entry without a governing event fails"
      ;; reverting the entry to the superseded (still tree-present) event id
      ;; keeps every per-commit closure check green; only the transition rule
      ;; can catch it
      (let [manifest (-> (:value head)
                         (assoc "prev_manifest" (:hex head)
                                "governance_event" nil
                                "withdrawn" [{"slug" (second slugs)
                                              "event" original-event}]))
            {:keys [commit]} (fx/craft-release!
                              clone {:parents [head-commit]
                                     :base-tree-of head-commit
                                     :manifest-value manifest})]
        (is (= :withdrawn-changed-without-event (reason-at clone commit)))))
    (testing "a governance manifest changing coordinates fails"
      (let [manifest (-> (:value head)
                         (assoc "prev_manifest" (:hex head)
                                "governance_event" (get-in (:value head)
                                                           ["withdrawn" 0 "event"])
                                "selection_params" {"config" "smuggled-build"}))
            {:keys [commit]} (fx/craft-release!
                              clone {:parents [head-commit]
                                     :base-tree-of head-commit
                                     :manifest-value manifest})]
        (is (= :governance-changed-coordinates (reason-at clone commit)))))))

(deftest works-and-summary-structural-rules
  (let [{:keys [clone head-commit head]} (published-repos!)
        value (:value head)]
    (testing "works/withdrawn overlap fails"
      (let [manifest (assoc value "prev_manifest" (:hex head)
                            "withdrawn" [{"slug" (first slugs)
                                          "event" (str "snh:1:governance-event:"
                                                       (apply str (repeat 64 "c")))}]
                            "governance_event" (str "snh:1:governance-event:"
                                                    (apply str (repeat 64 "c"))))
            {:keys [commit]} (fx/craft-release!
                              clone {:parents [head-commit]
                                     :base-tree-of head-commit
                                     :manifest-value manifest})]
        (is (= :works-withdrawn-overlap (reason-at clone commit)))))
    (testing "an invalid-count mismatch fails"
      (let [manifest (assoc value "prev_manifest" (:hex head)
                            "validation_summary" {"invalid_count" 1
                                                  "invalid_slugs" []})
            {:keys [commit]} (fx/craft-release!
                              clone {:parents [head-commit]
                                     :base-tree-of head-commit
                                     :manifest-value manifest})]
        (is (= :invalid-count-mismatch (reason-at clone commit)))))))

(deftest archive-verification-is-a-total-report
  (let [{:keys [clone head-commit init-commit]} (published-repos!)
        v (view/git-view clone)
        opts {:snapshot-id "swh:1:snp:fixture"}]
    (testing "success report at the publication head"
      (let [report (verify/archive-verification v head-commit (fx/pinned-keys) opts)]
        (is (= :success (:result report)))
        (is (= 2 (:chain-length report)))
        (is (= "swh:1:snp:fixture" (:snapshot-id report)))
        (is (= (set [:release :governance])
               (set (keys (:pinned-fingerprints report)))))))
    (testing "the pre-genesis commit is not a citable publication commit"
      (let [report (verify/archive-verification v init-commit (fx/pinned-keys) opts)]
        (is (= :failed (:result report)))
        (is (= :not-a-publication-commit (:reason report)))))
    (testing "an absent commit yields a failed report, not an exception"
      (let [report (verify/archive-verification
                    v (apply str (repeat 40 "d")) (fx/pinned-keys) opts)]
        (is (= :failed (:result report)))
        (is (= :commit-missing (:reason report)))))))

(deftest archived-view-lacking-required-bytes-fails-without-fallback
  ;; a view reads only its own repository: an archive missing a signature the
  ;; live origin still holds must fail, never complete from elsewhere
  (let [{:keys [dir clone head-commit head]} (published-repos!)
        archive (repo/clone! (str dir "/origin.git") (str dir "/archive"))
        ;; an incomplete archive: the head names the latest manifest but its
        ;; tree (rebuilt from the previous release) lacks the manifest bytes
        parent-commit (first (view/parents-of (view/git-view archive) head-commit))
        forged (repo/write-commit!
                archive {:parents [parent-commit]
                         :base-tree-of parent-commit
                         :files {verify/head-path
                                 (sign/hex64-lf-bytes (:hex head))}})]
    (is (= :missing-manifest
           (try (verify/verify-repository-at (view/git-view archive) forged
                                             (fx/pinned-keys))
                :valid
                (catch clojure.lang.ExceptionInfo e (:reason (ex-data e))))))
    ;; the same head verifies against the complete origin clone: the failure
    ;; above is a property of the deficient view, and nothing fell back to it
    (is (map? (verify/verify-repository-at (view/git-view clone) head-commit
                                           (fx/pinned-keys))))))
