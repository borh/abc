(ns abc.sim.ingest-sim-test
  "Pure-layer ingest properties P6–P9: rendered CSV → run-corpus! against
  projection(model). Divergence-linked cases D1/D4 run as expected
  failures via abc.sim.divergences."
  (:require [abc.sim.divergences :as div]
            [abc.sim.gen :as sgen]
            [abc.sim.harness :as harness]
            [abc.sim.model :as model]
            [abc.sim.oracle :as oracle]
            [abc.sim.render :as render]
            [abc.tools.aozora-csv :as ac]
            [abc.tools.aozora-ingest :as ingest]
            [abc.tools.files :as files]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]))

(defn- ingest-rows! [rows dir]
  (ingest/run-corpus! {:rows rows :output-dir (str dir) :overwrite true}))

(defn- ingest-model
  "Render model (with optional corruptions) → parse → ingest into a temp
  dir; returns {:dir :result}. Caller must delete-tree! :dir."
  [m corruptions]
  (let [rows (render/corrupt-rows (render/model->rows m) corruptions)
        csv (render/rows->csv rows)
        parsed (ac/read-rows-from-string csv)
        dir (render/temp-dir "sim-ingest")]
    (try
      {:dir dir :result (ingest-rows! parsed dir)}
      (catch Exception e
        (render/delete-tree! dir)
        (throw e)))))

(defn- ingested-person [dir pid]
  (let [f (io/file dir "persons" (str pid ".json"))]
    (when (.exists f) (files/read-json f))))

(defn- person-faithful? [dir pid p]
  (let [r (ingested-person dir pid)]
    (and r
         (= (:family_name p) (get r "family_name"))
         (= (:given_name p) (get r "given_name"))
         (= (:date_of_birth p) (get r "date_of_birth"))
         (= (:copyright_expired p) (get r "person_copyright_expired")))))

(defn- work-faithful? [dir wid w edges]
  (let [f (io/file dir "works" (str wid ".json"))]
    (and (.exists f)
         (let [r (files/read-json f)]
           (and (= (:title w) (get-in r ["work" "title"]))
                (= (:ndc w) (get-in r ["work" "ndc"]))
                (= (set (for [[[ewid rel] pids] edges :when (= ewid wid) pid pids]
                          [pid rel]))
                   (set (map (juxt #(get % "person_id") #(get % "relation_to_work"))
                             (get r "contributors")))))))))

;; P6.clean-faithfulness
(deftest p6-clean-faithfulness-sim-test
  (harness/check! "P6.clean-faithfulness" 40
                  (prop/for-all [hist (sgen/benign-history-gen {:length [3 8] :works [3 8]})]
                                (let [{:keys [states]} (model/fold-history hist)
                                      m (peek states)
                                      {:keys [persons works edges]} (oracle/projection m)
                                      {:keys [dir result]} (ingest-model m [])]
                                  (try
                                    (and (zero? (:works-skipped result))
                                         (= (count works) (:works-written result))
                                         (every? (fn [[pid p]] (person-faithful? dir pid p)) persons)
                                         (every? (fn [[wid w]] (work-faithful? dir wid w edges)) works))
                                    (finally (render/delete-tree! dir)))))))

;; P6.divergent-work-fields — D1 (desired: detected, not first-row-wins)
(deftest p6-divergent-work-fields-sim-test
  (let [m0 (model/bootstrap 1)
        ;; second row for the same work via a second contributor
        m (:model (model/apply-event m0 {:event/type :add-person-with-edge
                                         :pid "000002" :person (model/base-person)
                                         :wid "000101" :relation "翻訳者"}))
        {:keys [dir result]} (ingest-model m [{:corrupt/type :divergent-work-fields
                                               :wid "000101" :column "作品名"
                                               :value "別名"}])]
    (try
      (let [work-skipped? (pos? (:works-skipped result))
            ingested-title (when-not work-skipped?
                             (get-in (files/read-json (io/file dir "works" "000101.json"))
                                     ["work" "title"]))]
        (div/expected-failure :D1 "P6.divergent-work-fields"
          ;; DESIRED: the divergence is detected — work skipped or audited,
          ;; i.e. NOT a silently written work carrying first-row title.
                              (or work-skipped?
                                  (not= "作品000101" ingested-title))))
      (finally (render/delete-tree! dir)))))

;; P7.date-classes — every parse-date input class: normalized EDTF value +
;; the exact correction rules (carried in source_csv_provenance), verbatim
;; admission, or passthrough+skip; never a crash or silent third outcome.
;; Corrections are only emitted when provenance is supplied (ADR 0015
;; two-mode contract), so the ingest here passes a provenance map.
(def date-cases
  ;; [cell normalized rules outcome]; outcome ∈ :corrected :verbatim :skipped
  [["1900. 1. 1" "1900-01-01"
    #{"strip-whitespace" "normalize-date-separator" "pad-month" "pad-day"} :corrected]
   ["1900 - 01 - 01" "1900-01-01" #{"strip-whitespace"} :corrected]
   ["1900--01" "1900-01" #{"collapse-multi-dash"} :corrected]
   ["不詳" nil #{"unknown-marker"} :corrected]
   ["未詳" nil #{"unknown-marker"} :corrected]
   ["前5" "-0004" #{"bce-astronomical"} :corrected]
   ["紀元前5世紀初頭" "-04XX" #{"century-prose"} :corrected]
   ["192X" "192X" #{} :verbatim]
   ["645-01-01" "0645-01-01" #{"pad-year"} :corrected]
   ["2020-02-31" nil nil :skipped]      ;; impossible date → schema reject
   ["こんにちは" nil nil :skipped]])     ;; unparseable shape → schema reject

(def ^:private test-provenance
  {"source_url" nil
   "retrieved_at" nil
   "original_file_hash" (str "sha256:" (apply str (repeat 64 "a")))})

(deftest p7-date-classes-sim-test
  (doseq [[cell normalized rules outcome] date-cases]
    (testing (pr-str cell)
      (let [m (model/bootstrap 2) ;; work 000101 dirty, 000102 clean
            rows (render/corrupt-rows (render/model->rows m)
                                      [{:corrupt/type :cell :wid "000101"
                                        :pid "000001"
                                        :column "生年月日" :value cell}])
            parsed (ac/read-rows-from-string (render/rows->csv rows))
            dir (render/temp-dir "sim-p7")
            result (ingest/run-corpus! {:rows parsed :output-dir (str dir)
                                        :overwrite true
                                        :source-csv-provenance test-provenance})]
        (try
          (if (= :skipped outcome)
            (is (= ["000101"] (:skipped-work-ids result)) (pr-str cell))
            (let [r (ingested-person dir "000001")
                  corrs (filter #(= "date_of_birth" (get % "field"))
                                (get-in r ["source_csv_provenance"
                                           "parse_corrections"]))]
              (is (zero? (:works-skipped result)) (pr-str cell))
              (is (= normalized (get r "date_of_birth")) (pr-str cell))
              (is (= rules (set (map #(get % "rule") corrs))) (pr-str cell))))
          ;; the clean work always survives
          (is (some? (ingested-person dir "000002")))
          (finally (render/delete-tree! dir)))))))

;; P6.encoding-equivalence — BOM presence and header permutation must not
;; change ingest output at all (byte-level snapshot comparison).
(deftest p6-encoding-equivalence-sim-test
  (harness/check! "P6.encoding-equivalence" 15
                  (prop/for-all [hist (sgen/benign-history-gen {:length [2 5] :works [2 5]})
                                 permuted (gen/shuffle render/headers)]
                                (let [m (peek (:states (model/fold-history hist)))
                                      rows (render/model->rows m)
                                      snap (fn [csv]
                                             (let [dir (render/temp-dir "sim-enc")]
                                               (try
                                                 (ingest-rows! (ac/read-rows-from-string csv) dir)
                                                 (into {} (for [f (file-seq (io/file dir))
                                                                :when (.isFile ^java.io.File f)]
                                                            [(subs (str f) (count (str dir))) (slurp f)]))
                                                 (finally (render/delete-tree! dir)))))
                                      canonical (snap (render/rows->csv rows))]
                                  (and (= canonical (snap (render/rows->csv rows {:bom? true})))
                                       (= canonical (snap (render/rows->csv rows {:header-cells (vec permuted)}))))))))

;; P6.quoting — quoted commas/newlines/quotes must survive INGEST, not just
;; CSV parsing (the render-layer test only proves the latter).
(deftest p6-quoting-through-ingest-sim-test
  (let [title "旅,\"新\"\n行"
        m (:model (model/apply-event (model/bootstrap 1)
                                     {:event/type :edit-work :wid "000101"
                                      :field :title :value title}))
        {:keys [dir result]} (ingest-model m [])]
    (try
      (is (zero? (:works-skipped result)))
      (is (= title (get-in (files/read-json (io/file dir "works" "000101.json"))
                           ["work" "title"])))
      (finally (render/delete-tree! dir)))))

;; P6.duplicate-dedup — an exact duplicate row changes nothing.
(deftest p6-duplicate-dedup-sim-test
  (let [m (model/bootstrap 2)
        run (fn [corruptions]
              (let [{:keys [dir result]} (ingest-model m corruptions)]
                (try {:result result
                      :work (files/read-json (io/file dir "works" "000101.json"))
                      :person (ingested-person dir "000001")}
                     (finally (render/delete-tree! dir)))))]
    (is (= (run [])
           (run [{:corrupt/type :duplicate-row :wid "000101" :pid "000001"}])))))

;; P8.skip-counted — generative: a within-work person divergence skips
;; exactly that work; clean works' records stay present and correct.
;; Fault shape: duplicate one contributor row of the chosen work, then
;; diverge the duplicate's 姓 — same pid, divergent bodies, one work.
;; build-record-fragment-from-rows throws BEFORE any writes for this fault,
;; so no cross-work contamination is possible and the case passes today.
(deftest p8-skip-counted-sim-test
  (harness/check! "P8.skip-counted" 30
                  (prop/for-all [hist (sgen/benign-history-gen {:length [3 6] :works [4 8]})]
                                (let [{:keys [states]} (model/fold-history hist)
                                      m (peek states)
                                      {:keys [works edges]} (oracle/projection m)
                                      [wid _] (first works)
                                      pid (first (sort (mapcat val (filter #(= wid (ffirst %)) edges))))
                                      {:keys [dir result]} (ingest-model m
                                                                         [{:corrupt/type :duplicate-row :wid wid :pid pid}
                                                                          {:corrupt/type :divergent-person :wid wid :pid pid
                                                                           :column "姓" :value "×"}])]
                                  (try
                                    (and (= [wid] (:skipped-work-ids result))
                                         (every? (fn [[owid ow]]
                                                   (or (= owid wid) (work-faithful? dir owid ow edges)))
                                                 works))
                                    (finally (render/delete-tree! dir)))))))

;; P8.atomicity — D4 fixed: a skipped work leaves no person records behind.
;; The fault fails LATE in the old flow (second contributor's schema
;; validation); the build-before-write structure must keep both files off
;; disk regardless.
(deftest p8-atomicity-sim-test
  (let [m0 (model/bootstrap 1)
        m (:model (model/apply-event m0 {:event/type :add-person-with-edge
                                         :pid "000002" :person (model/base-person)
                                         :wid "000101" :relation "翻訳者"}))
        {:keys [dir result]} (ingest-model m [{:corrupt/type :cell
                                               :wid "000101" :pid "000002"
                                               :column "生年月日"
                                               :value "2020-02-31"}])]
    (try
      (is (= ["000101"] (:skipped-work-ids result)))
      (div/expected-failure :D4 "P8.atomicity"
        ;; DESIRED (holds since D4 fix): the skipped work wrote no person
        ;; files at all.
                            (and (nil? (ingested-person dir "000001"))
                                 (nil? (ingested-person dir "000002"))))
      (finally (render/delete-tree! dir)))))

;; P8.order-independence — D4: a dirty work must not damage a clean work
;; that shares a person, in EITHER processing order. run-corpus! processes
;; works sorted by work id, so both orders are exercised by mirroring which
;; logical work carries the divergent row: dirty=000101 makes the dirty work
;; ingest first (clean work then hits the refuse-to-overwrite guard);
;; dirty=000102 makes the clean work ingest first. The desired contract is
;; the CONJUNCTION over both orders, gated once.
(deftest p8-order-independence-sim-test
  (let [run-order
        (fn [dirty-wid]
          (let [m0 (model/bootstrap 2)
                ;; shared person: 000001 contributes to both works
                m (:model (model/apply-event m0 {:event/type :add-edge
                                                 :wid "000102"
                                                 :relation "翻訳者" :pid "000001"}))
                ;; valid-but-different 姓 only in the dirty work's row
                rows (render/corrupt-rows (render/model->rows m)
                                          [{:corrupt/type :cell
                                            :wid dirty-wid :pid "000001"
                                            :column "姓" :value "別"}])
                parsed (ac/read-rows-from-string (render/rows->csv rows))
                dir (render/temp-dir "sim-isolation")
                result (ingest/run-corpus! {:rows parsed
                                            :output-dir (str dir)})] ;; NO :overwrite
            (try
              (let [clean-wid (if (= dirty-wid "000101") "000102" "000101")]
                (not-any? #{clean-wid} (:skipped-work-ids result)))
              (finally (render/delete-tree! dir)))))
        dirty-first-ok? (run-order "000101")
        clean-first-ok? (run-order "000102")]
    (div/expected-failure :D4 "P8.order-independence"
      ;; DESIRED: clean work unaffected in both orders. Holds since the D4
      ;; fix: conflicts resolve deterministically at the corpus level; no
      ;; work is skipped for a cross-work conflict.
                          (and dirty-first-ok? clean-first-ok?))))

;; P9.byte-stable — re-ingest is byte-identical, no overwrite errors.
(deftest p9-byte-stable-sim-test
  (harness/check! "P9.byte-stable" 30
                  (prop/for-all [hist (sgen/benign-history-gen {:length [3 6] :works [3 6]})]
                                (let [{:keys [states]} (model/fold-history hist)
                                      m (peek states)
                                      rows (ac/read-rows-from-string (render/rows->csv (render/model->rows m)))
                                      dir (render/temp-dir "sim-idem")]
                                  (try
                                    (ingest-rows! rows dir)
                                    (let [snapshot (fn []
                                                     (into {} (for [f (file-seq (io/file dir))
                                                                    :when (.isFile ^java.io.File f)]
                                                                [(str f) (slurp f)])))
                                          before (snapshot)
                ;; second ingest without :overwrite must not throw
                                          _ (ingest/run-corpus! {:rows rows :output-dir (str dir)})
                                          after (snapshot)]
                                      (= before after))
                                    (finally (render/delete-tree! dir)))))))
