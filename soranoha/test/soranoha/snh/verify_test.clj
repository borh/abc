(ns soranoha.snh.verify-test
  "Verifier invariant fixtures, table-driven: each chain rule has a passing
  case (the lifecycle chains built through the transaction) and an isolating
  mutation whose exact rejection reason is asserted — a deleted check turns
  its rows red. Scenario tests cover the archive report contract, the
  deficient-view failure, and git replacement-ref immunity."
  (:require [clojure.test :refer [deftest is testing]]
            [babashka.process :as process]
            [soranoha.core.hash :as hash]
            [soranoha.snh.fixture :as fx]
            [soranoha.snh.repo :as repo]
            [soranoha.snh.sign :as sign]
            [soranoha.snh.verify :as verify]
            [soranoha.snh.view :as view]))

(def slug-a "hashire_merosu_000035_1567")
(def slug-b "kumo_no_ito_000879_92")
(def slugs [slug-a slug-b])
(def base {:admitted slugs})

(defn- reason-at
  "Verification outcome at `commit`: the failure reason keyword, or :valid."
  [clone commit]
  (try (verify/verify-repository-at (view/git-view clone) commit (fx/pinned-keys))
       :valid
       (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))

(defn- blob-files [blobs]
  (into {} (map (fn [[hex bytes]] [(verify/blob-path hex) bytes])) blobs))

(defn- assembled
  "Core+blobs for the fixture corpus against `head-value` (nil = fresh)."
  [opts head-value]
  ((fx/make-assemble (merge base opts)) head-value))

;; --- context: a two-release build chain ------------------------------------

(defn- build-ctx []
  (let [{:keys [clone init-commit] :as repos} (fx/make-repos!)]
    (fx/publish! clone base)
    (fx/publish! clone (assoc base :selection-params {"config" "fixture" "round" 2}))
    (let [head-commit (fx/head-of clone)]
      (merge repos {:head-commit head-commit
                    :head (fx/manifest-at clone head-commit)
                    :init-commit init-commit}))))

(defn- next-value
  "A valid successor manifest value for the context head (fresh projection)."
  [{:keys [head]} & {:keys [round] :or {round 3}}]
  (let [{:keys [core]} (assembled {:selection-params {"config" "fixture" "round" round}}
                                  (:value head))]
    (assoc core "prev_manifest" (:hex head) "withdrawn" [] "governance_event" nil)))

(def build-mutations
  [{:name "merge commit carrying the old head on its second parent"
    :expect :merge-commit
    :craft (fn [{:keys [clone head-commit init-commit] :as ctx}]
             (:commit (fx/craft-release!
                       clone {:parents [head-commit init-commit]
                              :base-tree-of head-commit
                              :manifest-value (next-value ctx)})))}

   {:name "chain replacement: prev_manifest zero on a non-initial commit"
    :expect :prev-manifest-mismatch
    :craft (fn [{:keys [clone head-commit] :as ctx}]
             (:commit (fx/craft-release!
                       clone {:parents [head-commit]
                              :base-tree-of head-commit
                              :manifest-value
                              (assoc (next-value ctx)
                                     "prev_manifest" sign/zero-head-hex)})))}

   {:name "commit that does not advance the head"
    :expect :head-not-advanced
    :craft (fn [{:keys [clone head-commit]}]
             (repo/write-commit!
              clone {:parents [head-commit]
                     :base-tree-of head-commit
                     :files {"unrelated.txt" (.getBytes "x" "UTF-8")}}))}

   {:name "head naming a manifest that is not stored"
    :expect :missing-manifest
    :craft (fn [{:keys [clone head-commit]}]
             (repo/write-commit!
              clone {:parents [head-commit]
                     :base-tree-of head-commit
                     :files {verify/head-path
                             (sign/hex64-lf-bytes (apply str (repeat 64 "b")))}}))}

   {:name "zero head reappearing after genesis"
    :expect :zero-head-after-genesis
    :craft (fn [{:keys [clone head-commit]}]
             (repo/write-commit!
              clone {:parents [head-commit]
                     :base-tree-of head-commit
                     :files {verify/head-path
                             (sign/hex64-lf-bytes sign/zero-head-hex)}}))}

   {:name "referenced blob present only outside its prescribed path"
    :expect :missing-blob
    :craft (fn [{:keys [clone head-commit head] :as ctx}]
             (let [{:keys [core blobs]} (assembled {:variant "v3"
                                                    :selection-params
                                                    {"config" "fixture" "round" 3}}
                                                   (:value head))
                   manifest (assoc core "prev_manifest" (:hex head)
                                   "withdrawn" [] "governance_event" nil)
                   stray (hash/sha256-bytes (fx/work-blob-bytes "plaintext" slug-a "v3"))]
               (:commit (fx/craft-release!
                         clone {:parents [head-commit]
                                :base-tree-of head-commit
                                :manifest-value manifest
                                :extra-files
                                (assoc (blob-files (dissoc blobs stray))
                                       "attic/misplaced-blob" (get blobs stray))}))))}

   {:name "declared artifact byte length disagreeing with the stored blob"
    :expect :blob-length-mismatch
    :craft (fn [{:keys [clone head-commit] :as ctx}]
             (let [value (update-in (next-value ctx)
                                    ["works" 0 "artifacts" 0 "bytes"] inc)]
               (:commit (fx/craft-release!
                         clone {:parents [head-commit]
                                :base-tree-of head-commit
                                :manifest-value value
                                :raw true}))))}

   {:name "manifest signed over the wrong domain"
    :expect :signature-invalid
    :craft (fn [{:keys [clone head-commit] :as ctx}]
             (:commit (fx/craft-release!
                       clone {:parents [head-commit]
                              :base-tree-of head-commit
                              :manifest-value (next-value ctx)
                              :sign-fn fx/sign-event-with-release-key})))}

   {:name "signature file not exactly 64 bytes"
    :expect :malformed-signature
    :craft (fn [{:keys [clone head-commit] :as ctx}]
             (let [{:keys [commit hex]} (fx/craft-release!
                                         clone {:parents [head-commit]
                                                :base-tree-of head-commit
                                                :manifest-value (next-value ctx)})]
               (repo/write-commit!
                clone {:parents [head-commit]
                       :base-tree-of commit
                       :files {(verify/manifest-sig-path hex) (byte-array 63)}})))}

   {:name "validation record naming other TEI bytes"
    :expect :validation-artifact-mismatch
    :craft (fn [{:keys [clone head-commit head]}]
             (let [{:keys [core blobs]} (assembled {:variant "v3"
                                                    :selection-params
                                                    {"config" "fixture" "round" 3}}
                                                   (:value head))
                   manifest (assoc core "prev_manifest" (:hex head)
                                   "withdrawn" [] "governance_event" nil)
                   wrong (fx/validation-blob-bytes (apply str (repeat 64 "e")) false)
                   wrong-hex (hash/sha256-bytes wrong)
                   ;; swap slug-a's validation artifact for one naming ghost
                   ;; TEI bytes, keeping manifest and blobs consistent
                   manifest (update-in manifest ["works" 0 "artifacts" 2]
                                       assoc "id" (str "snh:1:tei-validation:" wrong-hex)
                                       "bytes" (alength wrong))]
               (:commit (fx/craft-release!
                         clone {:parents [head-commit]
                                :base-tree-of head-commit
                                :manifest-value manifest
                                :extra-files (blob-files (assoc blobs wrong-hex wrong))}))))}

   {:name "summary claiming an invalid slug whose record passed"
    :expect :validation-summary-mismatch
    :craft (fn [{:keys [clone head-commit] :as ctx}]
             (let [value (assoc (next-value ctx)
                                "validation_summary" {"invalid_count" 1
                                                      "invalid_slugs" [slug-a]})]
               (:commit (fx/craft-release!
                         clone {:parents [head-commit]
                                :base-tree-of head-commit
                                :manifest-value value}))))}

   {:name "works out of order (single-object rule enforced on the chain path)"
    :expect :works-not-sorted-unique
    :craft (fn [{:keys [clone head-commit] :as ctx}]
             (:commit (fx/craft-release!
                       clone {:parents [head-commit]
                              :base-tree-of head-commit
                              :manifest-value (update (next-value ctx)
                                                      "works" (comp vec reverse))
                              :raw true})))}

   {:name "governance event with reverse-sorted entries"
    :expect :entries-not-sorted-unique
    :craft (fn [{:keys [clone head-commit head]}]
             (let [event (fx/raw-event
                          (fx/event-value "withdrawal"
                                          [{"slug" slug-b "reason_code" "rights"
                                            "statement" ""}
                                           {"slug" slug-a "reason_code" "rights"
                                            "statement" ""}]))
                   head-value (:value head)
                   manifest (assoc head-value
                                   "prev_manifest" (:hex head)
                                   "governance_event" (:id event)
                                   "works" []
                                   "withdrawn" [{"slug" slug-a "event" (:id event)}
                                                {"slug" slug-b "event" (:id event)}]
                                   "validation_summary" {"invalid_count" 0
                                                         "invalid_slugs" []})]
               (:commit (fx/craft-release!
                         clone {:parents [head-commit]
                                :base-tree-of head-commit
                                :manifest-value manifest
                                :extra-files (:files event)}))))}])

;; --- context: a governance chain (withdrawal of slug-b, then amendment) ----

(defn- gov-ctx []
  (let [{:keys [clone] :as repos} (fx/make-repos!)]
    (fx/publish! clone base)
    (fx/publish-event! clone (fx/event-value
                              "withdrawal" [{"slug" slug-b
                                             "reason_code" "rights"
                                             "statement" ""}]))
    (let [original (get-in (:value (fx/manifest-at clone (fx/head-of clone)))
                           ["withdrawn" 0 "event"])]
      (fx/publish-event! clone (fx/event-value
                                "event-amendment"
                                [{"slug" slug-b
                                  "reason_code" "rights"
                                  "statement" "Corrected."
                                  "amends" original}]))
      (let [head-commit (fx/head-of clone)]
        (merge repos {:head-commit head-commit
                      :head (fx/manifest-at clone head-commit)
                      :original-event original})))))

(defn- gov-head-value [{:keys [head]}] (:value head))

(def gov-mutations
  [{:name "withdrawn entry silently dropped"
    :expect :withdrawn-not-monotonic
    :craft (fn [{:keys [clone head-commit head]}]
             (let [{:keys [core]} (assembled {:selection-params
                                              {"config" "fixture" "round" 4}}
                                             nil)
                   manifest (assoc core "prev_manifest" (:hex head)
                                   "withdrawn" [] "governance_event" nil)]
               (:commit (fx/craft-release!
                         clone {:parents [head-commit]
                                :base-tree-of head-commit
                                :manifest-value manifest}))))}

   {:name "withdrawn entry changed with no governing event"
    :expect :withdrawn-changed-without-event
    :craft (fn [{:keys [clone head-commit head original-event] :as ctx}]
             (let [manifest (assoc (gov-head-value ctx)
                                   "prev_manifest" (:hex head)
                                   "governance_event" nil
                                   "withdrawn" [{"slug" slug-b
                                                 "event" original-event}])]
               (:commit (fx/craft-release!
                         clone {:parents [head-commit]
                                :base-tree-of head-commit
                                :manifest-value manifest}))))}

   {:name "governance manifest smuggling a coordinate change"
    :expect :governance-changed-coordinates
    :craft (fn [{:keys [clone head-commit head] :as ctx}]
             (let [manifest (assoc (gov-head-value ctx)
                                   "prev_manifest" (:hex head)
                                   "governance_event"
                                   (get-in (gov-head-value ctx) ["withdrawn" 0 "event"])
                                   "selection_params" {"config" "smuggled"})]
               (:commit (fx/craft-release!
                         clone {:parents [head-commit]
                                :base-tree-of head-commit
                                :manifest-value manifest}))))}

   {:name "withdrawal event and added slugs disagreeing"
    :expect :withdrawal-slugs-mismatch
    :craft (fn [{:keys [clone head-commit head] :as ctx}]
             (let [event (fx/raw-event (fx/event-value
                                        "withdrawal" [{"slug" slug-a
                                                       "reason_code" "rights"
                                                       "statement" ""}]))
                   manifest (assoc (gov-head-value ctx)
                                   "prev_manifest" (:hex head)
                                   "governance_event" (:id event))]
               (:commit (fx/craft-release!
                         clone {:parents [head-commit]
                                :base-tree-of head-commit
                                :manifest-value manifest
                                :extra-files (:files event)}))))}

   {:name "added withdrawn entry bound to a different event than the governing one"
    :expect :withdrawn-entry-wrong-event
    :craft (fn [{:keys [clone head-commit head] :as ctx}]
             (let [gov (fx/raw-event (fx/event-value
                                      "withdrawal" [{"slug" slug-a
                                                     "reason_code" "rights"
                                                     "statement" ""}]))
                   other (fx/raw-event (fx/event-value
                                        "withdrawal" [{"slug" slug-a
                                                       "reason_code" "other"
                                                       "statement" "different event"}]))
                   value (gov-head-value ctx)
                   manifest (assoc value
                                   "prev_manifest" (:hex head)
                                   "governance_event" (:id gov)
                                   "works" []
                                   "withdrawn" (vec (sort-by #(get % "slug")
                                                             (conj (get value "withdrawn")
                                                                   {"slug" slug-a
                                                                    "event" (:id other)})))
                                   "validation_summary" {"invalid_count" 0
                                                         "invalid_slugs" []})]
               (:commit (fx/craft-release!
                         clone {:parents [head-commit]
                                :base-tree-of head-commit
                                :manifest-value manifest
                                :extra-files (merge (:files gov) (:files other))}))))}

   {:name "pre-existing withdrawn entry rewritten during a withdrawal"
    :expect :withdrawn-entry-rewritten
    :craft (fn [{:keys [clone head-commit head original-event] :as ctx}]
             (let [gov (fx/raw-event (fx/event-value
                                      "withdrawal" [{"slug" slug-a
                                                     "reason_code" "rights"
                                                     "statement" ""}]))
                   manifest (assoc (gov-head-value ctx)
                                   "prev_manifest" (:hex head)
                                   "governance_event" (:id gov)
                                   "works" []
                                   ;; slug-b reverts to the superseded event,
                                   ;; which still names it, so closure passes
                                   "withdrawn" [{"slug" slug-a "event" (:id gov)}
                                                {"slug" slug-b
                                                 "event" original-event}]
                                   "validation_summary" {"invalid_count" 0
                                                         "invalid_slugs" []})]
               (:commit (fx/craft-release!
                         clone {:parents [head-commit]
                                :base-tree-of head-commit
                                :manifest-value manifest
                                :extra-files (:files gov)}))))}

   {:name "amendment growing the withdrawn set"
    :expect :amendment-changed-withdrawn-set
    :craft (fn [{:keys [clone head-commit head] :as ctx}]
             (let [value (gov-head-value ctx)
                   current (get-in value ["withdrawn" 0 "event"])
                   ghost (str "snh:1:governance-event:" (apply str (repeat 64 "a")))
                   event (fx/raw-event
                          (fx/event-value "event-amendment"
                                          [{"slug" slug-a "reason_code" "rights"
                                            "statement" "" "amends" ghost}
                                           {"slug" slug-b "reason_code" "rights"
                                            "statement" "" "amends" current}]))
                   manifest (assoc value
                                   "prev_manifest" (:hex head)
                                   "governance_event" (:id event)
                                   "works" []
                                   "withdrawn" [{"slug" slug-a "event" (:id event)}
                                                {"slug" slug-b "event" (:id event)}]
                                   "validation_summary" {"invalid_count" 0
                                                         "invalid_slugs" []})]
               (:commit (fx/craft-release!
                         clone {:parents [head-commit]
                                :base-tree-of head-commit
                                :manifest-value manifest
                                :extra-files (:files event)}))))}

   {:name "amendment naming a superseded rather than current event"
    :expect :amendment-not-linear
    :craft (fn [{:keys [clone head-commit head original-event] :as ctx}]
             (let [event (fx/raw-event
                          (fx/event-value "event-amendment"
                                          [{"slug" slug-b "reason_code" "rights"
                                            "statement" "stale target"
                                            "amends" original-event}]))
                   manifest (assoc (gov-head-value ctx)
                                   "prev_manifest" (:hex head)
                                   "governance_event" (:id event)
                                   "withdrawn" [{"slug" slug-b "event" (:id event)}])]
               (:commit (fx/craft-release!
                         clone {:parents [head-commit]
                                :base-tree-of head-commit
                                :manifest-value manifest
                                :extra-files (:files event)}))))}

   {:name "amendment changing works"
    :expect :amendment-changed-works
    :craft (fn [{:keys [clone head-commit head] :as ctx}]
             (let [value (gov-head-value ctx)
                   current (get-in value ["withdrawn" 0 "event"])
                   event (fx/raw-event
                          (fx/event-value "event-amendment"
                                          [{"slug" slug-b "reason_code" "rights"
                                            "statement" "works tamper"
                                            "amends" current}]))
                   {:keys [blobs core]} (assembled {:variant "v3"} (:value head))
                   manifest (assoc value
                                   "prev_manifest" (:hex head)
                                   "governance_event" (:id event)
                                   "works" (get core "works")
                                   "withdrawn" [{"slug" slug-b "event" (:id event)}])]
               (:commit (fx/craft-release!
                         clone {:parents [head-commit]
                                :base-tree-of head-commit
                                :manifest-value manifest
                                :extra-files (merge (:files event)
                                                    (blob-files blobs))}))))}

   {:name "governance convenience copy diverging from the CAS blob"
    :expect :event-copy-diverges
    :craft (fn [{:keys [clone head-commit head] :as ctx}]
             ;; a fresh withdrawal whose convenience copy carries tampered
             ;; bytes while the authoritative CAS blob is intact
             (let [gov (fx/raw-event (fx/event-value
                                      "withdrawal" [{"slug" slug-a
                                                     "reason_code" "rights"
                                                     "statement" ""}]))
                   value (gov-head-value ctx)
                   manifest (assoc value
                                   "prev_manifest" (:hex head)
                                   "governance_event" (:id gov)
                                   "works" []
                                   "withdrawn" (vec (sort-by #(get % "slug")
                                                             (conj (get value "withdrawn")
                                                                   {"slug" slug-a
                                                                    "event" (:id gov)})))
                                   "validation_summary" {"invalid_count" 0
                                                         "invalid_slugs" []})]
               (:commit (fx/craft-release!
                         clone {:parents [head-commit]
                                :base-tree-of head-commit
                                :manifest-value manifest
                                :extra-files
                                (assoc (:files gov)
                                       (verify/event-path (:hex gov))
                                       (.getBytes "{\"tampered\":true}" "UTF-8"))}))))}])

(deftest build-chain-mutations-fail-with-exact-reasons
  (let [ctx (build-ctx)]
    (testing "the untouched context verifies"
      (is (= :valid (reason-at (:clone ctx) (:head-commit ctx)))))
    (doseq [{:keys [name expect craft]} build-mutations]
      (testing name
        (is (= expect (reason-at (:clone ctx) (craft ctx))))))))

(deftest governance-chain-mutations-fail-with-exact-reasons
  (let [ctx (gov-ctx)]
    (testing "the untouched context verifies"
      (is (= :valid (reason-at (:clone ctx) (:head-commit ctx)))))
    (doseq [{:keys [name expect craft]} gov-mutations]
      (testing name
        (is (= expect (reason-at (:clone ctx) (craft ctx))))))))

(deftest withdrawal-must-only-remove-the-affected-works
  ;; three admitted works; the withdrawal of one may not touch the others
  (let [{:keys [clone] :as repos} (fx/make-repos!)
        slug-c "wagahai_wa_neko_de_aru_000148_789"
        admitted [slug-a slug-b slug-c]
        _ (fx/publish! clone {:admitted admitted})
        head-commit (fx/head-of clone)
        head (fx/manifest-at clone head-commit)
        gov (fx/raw-event (fx/event-value
                           "withdrawal" [{"slug" slug-b "reason_code" "rights"
                                          "statement" ""}]))
        {:keys [blobs core]} ((fx/make-assemble {:admitted admitted :variant "v3"}) nil)
        v3-entry (first (filter #(= slug-c (get % "slug")) (get core "works")))
        manifest (assoc (:value head)
                        "prev_manifest" (:hex head)
                        "governance_event" (:id gov)
                        "withdrawn" [{"slug" slug-b "event" (:id gov)}]
                        "works" (vec (sort-by #(get % "slug")
                                              [(first (get (:value head) "works"))
                                               v3-entry])))
        {:keys [commit]} (fx/craft-release!
                          clone {:parents [head-commit]
                                 :base-tree-of head-commit
                                 :manifest-value manifest
                                 :extra-files (merge (:files gov)
                                                     (blob-files blobs))})]
    (is (= :withdrawal-works-mismatch (reason-at clone commit)))))

(deftest superseded-single-entry-rule-is-subsumed-by-entry-uniqueness
  ;; an event with several distinct slugs still has exactly one entry per
  ;; slug, so amending one of them is legal; a duplicate-slug event can no
  ;; longer enter a valid chain because boundary decode rejects it — the
  ;; verifier's check remains as defense in depth
  (let [{:keys [clone]} (fx/make-repos!)]
    (fx/publish! clone base)
    (is (= :published
           (:outcome (fx/publish-event!
                      clone (fx/event-value
                             "withdrawal" [{"slug" slug-a "reason_code" "rights"
                                            "statement" ""}
                                           {"slug" slug-b "reason_code" "rights"
                                            "statement" ""}])))))
    (let [current (get-in (:value (fx/manifest-at clone (fx/head-of clone)))
                          ["withdrawn" 0 "event"])]
      (is (= :published
             (:outcome (fx/publish-event!
                        clone (fx/event-value
                               "event-amendment"
                               [{"slug" slug-a "reason_code" "rights"
                                 "statement" "Corrected." "amends" current}]))))))))

(deftest genesis-mutations
  (let [{:keys [clone init-commit]} (fx/make-repos!)
        {:keys [core blobs]} (assembled {} nil)
        genesis-base (assoc core "prev_manifest" sign/zero-head-hex
                            "governance_event" nil "withdrawn" [])
        craft (fn [value]
                (:commit (fx/craft-release!
                          clone {:parents [init-commit]
                                 :base-tree-of init-commit
                                 :manifest-value value
                                 :extra-files (blob-files blobs)})))]
    (testing "the well-formed genesis verifies"
      (is (= :valid (reason-at clone (craft genesis-base)))))
    (testing "genesis with a governance event"
      (is (= :genesis-has-governance-event
             (reason-at clone (craft (assoc genesis-base "governance_event"
                                            (str "snh:1:governance-event:"
                                                 (apply str (repeat 64 "a")))))))))
    (testing "genesis with a withdrawn entry"
      (is (= :genesis-has-withdrawn
             (reason-at clone
                        (craft (assoc genesis-base "withdrawn"
                                      [{"slug" "phantom_000000_1"
                                        "event" (str "snh:1:governance-event:"
                                                     (apply str (repeat 64 "a")))}])
                               )))))
    (testing "a root commit with a nonzero head"
      (let [{:keys [commit]} (fx/craft-release!
                              clone {:parents []
                                     :manifest-value genesis-base
                                     :extra-files (blob-files blobs)})]
        (is (= :nonzero-head-at-root (reason-at clone commit)))))))

(deftest replacement-refs-do-not-alter-view-reads
  ;; git replace would silently substitute objects under plain plumbing; the
  ;; hardened view must keep reporting the invalid commit's own state
  (let [{:keys [clone head-commit] :as ctx} (build-ctx)
        bad (repo/write-commit!
             clone {:parents [head-commit]
                    :base-tree-of head-commit
                    :files {"unrelated.txt" (.getBytes "x" "UTF-8")}})]
    (is (= :head-not-advanced (reason-at clone bad)))
    (process/sh {:dir clone :out :string :err :string}
                "git" "replace" "-f" bad head-commit)
    (is (= :head-not-advanced (reason-at clone bad))
        "a replacement ref must not substitute the commit under verification")
    (process/sh {:dir clone :out :string :err :string}
                "git" "replace" "-d" bad)))

(deftest archive-verification-is-a-total-report
  (let [{:keys [clone head-commit init-commit]} (build-ctx)
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
  ;; a view reads only its own repository: an archive missing bytes the live
  ;; origin still holds must fail, never complete from elsewhere
  (let [{:keys [dir clone head-commit head]} (build-ctx)
        archive (repo/clone! (str dir "/origin.git") (str dir "/archive"))
        parent-commit (first (view/parents-of (view/git-view archive) head-commit))
        forged (repo/write-commit!
                archive {:parents [parent-commit]
                         :base-tree-of parent-commit
                         :files {verify/head-path
                                 (sign/hex64-lf-bytes (:hex head))}})]
    (is (= :missing-manifest (reason-at archive forged)))
    (is (map? (verify/verify-repository-at (view/git-view clone) head-commit
                                           (fx/pinned-keys))))))
