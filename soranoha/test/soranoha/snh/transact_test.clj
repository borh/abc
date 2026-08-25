(ns soranoha.snh.transact-test
  "The publication transaction against a local fixture origin: genesis and
  ordinary builds, governance withdrawal and amendment, the totality check,
  the lost-ack unknown-push path, and every current-state reconciliation
  outcome (requeue, already-published, determinism halt, conflicting
  withdrawal, stale amends)."
  (:require [clojure.test :refer [deftest is testing]]
            [soranoha.snh.decode :as decode]
            [soranoha.snh.fixture :as fx]
            [soranoha.snh.repo :as repo]
            [soranoha.snh.transact :as transact]
            [soranoha.snh.verify :as verify]
            [soranoha.snh.view :as view]))

(def slugs-a ["hashire_merosu_000035_1567" "kumo_no_ito_000879_92"])
(def excluded ["wagahai_wa_neko_de_aru_000148_789"])
(def base {:admitted slugs-a :excluded excluded})

(defn- verified-chain [clone]
  (verify/verify-repository-at (view/git-view clone)
                               (fx/head-of clone)
                               (fx/pinned-keys)))

(deftest build-governance-lifecycle
  (let [{:keys [clone init-commit]} (fx/make-repos!)]
    (testing "pre-genesis initial commit is the valid empty state"
      (is (= {:empty true}
             (verify/verify-repository-at (view/git-view clone) init-commit
                                          (fx/pinned-keys)))))

    (testing "genesis build publishes and verifies"
      (let [r (fx/publish! clone base)]
        (is (= :published (:outcome r)))
        (let [chain (verified-chain clone)]
          (is (= 1 (count (:chain chain))))
          (is (= (:manifest-id r) (:head chain))))))

    (testing "second build with a changed projection extends the chain"
      (let [r (fx/publish! clone (assoc base :selection-params
                                        {"config" "fixture" "round" 2}))]
        (is (= :published (:outcome r)))
        (is (= 2 (count (:chain (verified-chain clone)))))))

    (testing "withdrawal removes the work and extends withdrawn"
      (let [event (fx/event-value "withdrawal"
                                  [{"slug" (second slugs-a)
                                    "reason_code" "takedown-request"
                                    "statement" "Documented request."}])
            r (fx/publish-event! clone event)]
        (is (= :published (:outcome r)))
        (let [chain (verified-chain clone)
              head (:head-manifest chain)]
          (is (= 3 (count (:chain chain))))
          (is (= [(first slugs-a)]
                 (mapv #(get % "slug") (get head "works"))))
          (is (= [(second slugs-a)]
                 (mapv #(get % "slug") (get head "withdrawn"))))
          (testing "replaying the same event converges without a new release"
            (is (= :already-applied (:outcome (fx/publish-event! clone event)))))
          (testing "amendment replaces the governing event id only"
            (let [amendment (fx/event-value
                             "event-amendment"
                             [{"slug" (second slugs-a)
                               "reason_code" "takedown-request"
                               "statement" "Corrected statement."
                               "amends" (get-in head ["withdrawn" 0 "event"])}])
                  ra (fx/publish-event! clone amendment)]
              (is (= :published (:outcome ra)))
              (let [chain2 (verified-chain clone)
                    head2 (:head-manifest chain2)]
                (is (= 4 (count (:chain chain2))))
                (is (= (get head "works") (get head2 "works")))
                (is (= [(second slugs-a)]
                       (mapv #(get % "slug") (get head2 "withdrawn"))))
                (is (not= (get-in head ["withdrawn" 0 "event"])
                          (get-in head2 ["withdrawn" 0 "event"])))
                (testing "replaying the same amendment converges"
                  (is (= :already-applied
                         (:outcome (fx/publish-event! clone amendment)))))
                (testing "a fresh amendment naming the superseded event is stale"
                  (let [stale (fx/event-value
                               "event-amendment"
                               [{"slug" (second slugs-a)
                                 "reason_code" "takedown-request"
                                 "statement" "Second correction, stale target."
                                 "amends" (get-in head ["withdrawn" 0 "event"])}])]
                    (is (= {:outcome :halt :reason :stale-amends
                            :slugs [(second slugs-a)]}
                           (fx/publish-event! clone stale))))))))
          (testing "withdrawing an already-withdrawn slug halts"
            (let [again (fx/event-value "withdrawal"
                                        [{"slug" (second slugs-a)
                                          "reason_code" "rights"
                                          "statement" ""}])]
              (is (= {:outcome :halt :reason :conflicting-withdrawal
                      :slugs [(second slugs-a)]}
                     (fx/publish-event! clone again)))))
          (testing "withdrawing a slug outside works halts"
            (let [ghost (fx/event-value "withdrawal"
                                        [{"slug" "no_such_work_000000_1"
                                          "reason_code" "rights"
                                          "statement" ""}])]
              (is (= {:outcome :halt :reason :slug-not-in-works
                      :slugs ["no_such_work_000000_1"]}
                     (fx/publish-event! clone ghost))))))))))

(deftest invalid-candidate-never-reaches-the-origin
  ;; a zeroed release signature must be caught by the pre-push verification,
  ;; leaving the origin ref unchanged
  (let [{:keys [clone]} (fx/make-repos!)
        _ (fx/publish! clone base)
        head-before (fx/head-of clone)]
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo #"signature-invalid"
         (transact/publish-build! {:clone clone :branch fx/branch
                                   :pinned-keys (fx/pinned-keys)
                                   :assemble (fx/make-assemble
                                              (assoc base :selection-params
                                                     {"config" "fixture" "round" 2}))
                                   :sign-release (fn [_hex] (byte-array 64))})))
    (is (= head-before (fx/head-of clone)))))

(deftest uncontended-duplicate-is-a-no-op
  ;; the scheduled no-op decision runs before any push: publishing identical
  ;; state twice yields one release
  (let [{:keys [clone]} (fx/make-repos!)
        first-run (fx/publish! clone base)
        second-run (fx/publish! clone base)]
    (is (= :published (:outcome first-run)))
    (is (= {:outcome :already-published :manifest-id (:manifest-id first-run)}
           second-run))
    (is (= 1 (count (:chain (verified-chain clone)))))))

(deftest uncontended-same-projection-divergence-halts
  ;; same coordinates, different derived bytes: a determinism defect halts
  ;; before any commit is created, even with no competing publisher
  (let [{:keys [clone]} (fx/make-repos!)
        _ (fx/publish! clone (assoc base :variant "v1"))
        r (fx/publish! clone (assoc base :variant "v2"))]
    (is (= :determinism-halt (:outcome r)))
    (is (= 1 (count (:chain (verified-chain clone)))))))

(deftest totality-check-blocks-emission
  (let [{:keys [clone]} (fx/make-repos!)]
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo #"totality"
         (fx/publish! clone (assoc base :drop-candidate (first excluded)))))))

(deftest event-signed-by-release-key-is-refused
  (let [{:keys [clone]} (fx/make-repos!)
        _ (fx/publish! clone base)
        {:keys [hex bytes]} (decode/encode
                             "governance-event"
                             (fx/event-value "withdrawal"
                                             [{"slug" (first slugs-a)
                                               "reason_code" "rights"
                                               "statement" ""}]))]
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo #"event-signature-invalid"
         (transact/publish-governance!
          {:clone clone :branch fx/branch
           :pinned-keys (fx/pinned-keys)
           :sign-release fx/sign-release
           :event-bytes bytes
           :event-sig (fx/sign-event-with-release-key hex)})))))

(deftest unknown-push-result-converges-exactly-once
  (let [{:keys [clone]} (fx/make-repos!)
        _ (fx/publish! clone base)
        lossy-push (fn [dir branch commit expected]
                     (let [r (repo/push! dir branch commit expected)]
                       (if (= :ok r) :unknown r)))
        r (transact/publish-build! {:clone clone :branch fx/branch
                                    :pinned-keys (fx/pinned-keys)
                                    :assemble (fx/make-assemble
                                               (assoc base :selection-params
                                                      {"config" "fixture" "round" 2}))
                                    :sign-release fx/sign-release
                                    :push-fn lossy-push})]
    (is (= :published (:outcome r)))
    (is (= 2 (count (:chain (verified-chain clone)))))))

;; --- current-state reconciliation races ------------------------------------

(defn- race!
  "Publish `winner-opts` from a second clone between the loser's assembly and
  push, then return the loser's outcome."
  [loser-opts winner-optss]
  (let [{:keys [clone] :as repos} (fx/make-repos!)
        clone2 (fx/second-clone! repos)
        _ (fx/publish! clone base)
        _ (repo/fetch! clone fx/branch)
        interpose-push (fn [dir branch commit expected]
                         ;; the intervening publications land first
                         (doseq [opts winner-optss]
                           (assert (= :published (:outcome (fx/publish! clone2 opts)))))
                         (repo/push! dir branch commit expected))]
    (transact/publish-build! {:clone clone :branch fx/branch
                              :pinned-keys (fx/pinned-keys)
                              :assemble (fx/make-assemble loser-opts)
                              :sign-release fx/sign-release
                              :push-fn interpose-push})))

(deftest rejected-push-requeues-when-projection-changed
  (let [r (race! (assoc base :selection-params {"config" "fixture" "loser" 1})
                 [(assoc base :selection-params {"config" "fixture" "winner" 1})])]
    (is (= :requeue (:outcome r)))))

(deftest rejected-push-converges-when-desired-state-already-published
  ;; the winner lands the identical state between the loser's pre-push check
  ;; and its push; depending on whether the two byte-identical commits share
  ;; a timestamp the loser sees an up-to-date push (:published — the same
  ;; commit) or a rejection reconciled to :already-published; both are the
  ;; same convergence
  (let [same (assoc base :selection-params {"config" "fixture" "round" 2})
        r (race! same [same])]
    (is (contains? #{:published :already-published} (:outcome r)))))

(deftest rejected-push-halts-on-same-projection-different-content
  ;; the winner publishes the same projection with different artifact bytes;
  ;; the loser's recomputation disagrees with the accepted head
  (let [proj {"config" "fixture" "round" 2}
        r (race! (assoc base :selection-params proj :variant "v1")
                 [(assoc base :selection-params proj :variant "v2")])]
    (is (= :determinism-halt (:outcome r)))))

(deftest reconciliation-consults-only-the-current-head-after-multiple-commits
  (let [final {"config" "fixture" "winner" 2}
        r (race! (assoc base :selection-params {"config" "fixture" "loser" 1})
                 [(assoc base :selection-params {"config" "fixture" "winner" 1})
                  (assoc base :selection-params final)])]
    (is (= :requeue (:outcome r))))
  (testing "loser converges when the latest of several commits matches"
    (let [same (assoc base :selection-params {"config" "fixture" "round" 9})
          r (race! same
                   [(assoc base :selection-params {"config" "fixture" "mid" 1})
                    same])]
      (is (= :already-published (:outcome r))))))

(deftest rejected-push-requeues-after-intervening-withdrawal
  ;; an intervening withdrawal changes head admission-independent state; the
  ;; loser's rebuild inherits the head's withdrawn set by construction and
  ;; requeues or converges rather than resurrecting the withdrawn work
  (let [{:keys [clone] :as repos} (fx/make-repos!)
        clone2 (fx/second-clone! repos)
        _ (fx/publish! clone base)
        event (fx/event-value "withdrawal"
                              [{"slug" (second slugs-a)
                                "reason_code" "rights" "statement" ""}])
        interpose-push (fn [dir branch commit expected]
                         (assert (= :published (:outcome (fx/publish-event! clone2 event))))
                         (repo/push! dir branch commit expected))
        r (transact/publish-build! {:clone clone :branch fx/branch
                                    :pinned-keys (fx/pinned-keys)
                                    :assemble (fx/make-assemble
                                               (assoc base :selection-params
                                                      {"config" "fixture" "round" 2}))
                                    :sign-release fx/sign-release
                                    :push-fn interpose-push})]
    (is (= :requeue (:outcome r)))
    (testing "the requeued build from the new head keeps the withdrawal"
      (let [r2 (fx/publish! clone (assoc base :selection-params
                                         {"config" "fixture" "round" 2}))
            chain (verified-chain clone)
            head (:head-manifest chain)]
        (is (= :published (:outcome r2)))
        (is (= [(first slugs-a)] (mapv #(get % "slug") (get head "works"))))
        (is (= [(second slugs-a)]
               (mapv #(get % "slug") (get head "withdrawn"))))))))
