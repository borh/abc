(ns soranoha.za.archive-test
  "Archival-observation acceptance at the CLI boundary: a filesystem copy
  of the origin stands in as the archived view and is the sole read
  source. A readable view always yields a report (success or failed with
  the verifier's reason); an unreadable view throws: a failure to
  perform the observation, never an observation."
  (:require [babashka.fs :as fs]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [soranoha.main :as main]
            [soranoha.snh.fixture :as fx]))

(def ^:private slug-a "hashire_merosu_000035_1567")

(defn- archived-copy!
  "A filesystem copy of the bare origin (the archived view)."
  [origin]
  (let [dir (fs/path (fs/create-temp-dir {:prefix "za-archive"})
                     "snapshot.git")]
    (fs/copy-tree origin dir)
    (str dir)))

(defn- key-files! []
  (let [dir (fs/create-temp-dir {:prefix "za-archive-keys"})
        pinned (fx/pinned-keys)
        write! (fn [name hex]
                 (let [path (str (fs/path dir name))]
                   (spit path (str hex "\n"))
                   path))]
    {:release-pub (write! "release.pub" (:release pinned))
     :governance-pub (write! "governance.pub" (:governance pinned))}))

(deftest archival-observation-over-a-sole-archived-view
  (let [{:keys [clone origin]} (fx/make-repos!)
        _ (fx/publish! clone {:admitted [slug-a]})
        commit (fx/head-of clone)
        archive (archived-copy! origin)
        keys* (key-files!)
        observe (fn [opts]
                  (main/archive-verify!
                   (merge {:archive archive :commit commit} keys* opts)))
        report (observe {})]
    (is (= :success (:result report)))
    (is (= commit (:commit report)))
    (is (= (str (fs/canonicalize archive)) (:archive-view report)))
    (is (= 1 (:chain-length report)))
    (is (= #{:release :governance} (set (keys (:pinned-fingerprints report)))))

    (testing "a commit absent from the archive is a failed observation"
      (let [failed (observe {:commit (apply str (repeat 40 "d"))})]
        (is (= :failed (:result failed)))
        (is (= :commit-missing (:reason failed)))))

    (testing "only the concrete lowercase commit id names the observation:
      aliases and revision expressions are refused before any read"
      (doseq [not-an-identity [(str commit "^{commit}")
                               (str/upper-case commit)
                               "HEAD"]]
        (let [refused (observe {:commit not-an-identity})]
          (is (= :failed (:result refused)))
          (is (= :malformed-commit (:reason refused))))))

    (testing "the archive alone decides: origin movement after the snapshot
      is invisible to the observation"
      (fx/publish! clone {:admitted [slug-a]
                          :selection-params {"config" "fixture" "round" 2}})
      (let [head-2 (fx/head-of clone)]
        (is (= :success (:result (observe {}))))
        (is (= :failed (:result (observe {:commit head-2}))))))

    (testing "an unconstructible view is an operational failure, not a report"
      (is (thrown? Exception
                   (observe {:archive (str (fs/path archive "missing"))}))))))
