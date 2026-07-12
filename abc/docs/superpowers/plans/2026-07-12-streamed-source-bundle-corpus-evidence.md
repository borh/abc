# Streamed Source-Bundle Corpus Evidence Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make production source-bundle admission and the pinned Aozora corpus report consume one bounded member-body scan, so checked sizes are actual streamed bytes and declared-size mismatches are explicit evidence.

**Architecture:** Extract the existing staged `inspect-open-zip` body traversal into public `scan-zip`, then make pure `admit-scan!` construct or reject `abc-source-bundle-v1`. Production `inspect-zip` composes those functions; the corpus report consumes the same scan, records admission disposition and actual-byte statistics, and uses `7zz` only for listability of structurally unreadable archives.

**Tech Stack:** Clojure 1.12, Apache Commons Compress `ZipFile`, SHA-256 `DigestInputStream`, Nix flake checks, Kaocha/clojure.test, deterministic JSON.

## Global Constraints

- Preserve `abc-source-bundle-v1` identity bytes and successful `inspect-zip` return values.
- Preserve decoder precedence: EFS UTF-8, valid Info-ZIP Unicode Path, then strict windows-31j.
- Derive all facts from the existing private read-only staged archive.
- Enforce 1,024 members, 16,777,216 bytes/member, and 33,554,432 bytes total.
- Keep `7zz` listability-only; never use it for identity or admission.
- Propagate protected interruption, filesystem, linkage, programming, and later operational I/O failures unchanged.
- Pin Aozora commit `0e9ea3e586eb0aa34039fabfc85a407d2f98b165`.
- Actual maxima remain 12,631,833 member bytes and 27,874,310 total bytes.
- Pin `cards/001393/files/50710_ruby_36965.zip` / `fushigino_kunino_alice_musical.txt`: declared 68,007, actual 68,497 bytes.
- Do not add archive recovery, new limits, parser/publication behavior, or an identity migration.
- Compare `:primary-text-bytes` with `java.util.Arrays/equals`; Clojure `=` on
  two separately allocated byte arrays compares object identity.

---

### Task 1: Extract the existing bounded scan from production admission

**Files:**
- Modify: `abc/src/abc/tools/source_bundle.clj`
- Modify: `abc/test/abc/tools/source_bundle_test.clj`

**Interfaces:**
- Produces `(scan-zip zip-file)` and `(scan-zip zip-file limits)`.
- Produces `(admit-scan! scan)` with the existing inspection result/failures.
- Produces `(admission-error? throwable)` for direct marker-authenticated errors.
- Preserves `inspect-zip` as `scan-zip` followed by `admit-scan!`.

The scan shape is:

```clojure
{:archive-path string
 :archive-hash string-matching-hash/hash-pattern
 :members [{"path" string "decoded_path" string "name_source" string
            "byte_length" integer
            "member_hash" string-matching-hash/hash-pattern}]
 :semantic-text-candidates [string]
 :collision-evidence {:nfc-collision? boolean
                      :unicode-case-collision? boolean}
 :stats {:member-count integer :max-member-bytes integer
         :total-bytes integer :utf8-count integer :legacy-count integer
         :declared-actual-size-mismatches
         [{:member-path string :declared-bytes integer :actual-bytes integer}]}
 :primary-text-bytes byte-array-or-nil}
```

- [ ] **Step 1: Record the pre-change gate wall time**

```sh
/usr/bin/time -f 'baseline_source_bundle_corpus_seconds=%e' \
  nix build ./abc#checks.x86_64-linux.source-bundle-corpus \
  --rebuild --print-build-logs
```

Expected: exit 0. Retain the duration for Task 3.

- [ ] **Step 2: Pre-flight the extraction against current source**

```sh
rg -n "defn- (inspect-open-zip|read-member!|primary-candidate?)|defn fail!|defn- stage-archive!" \
  abc/src/abc/tools/source_bundle.clj
rg -n "defmacro.*with-zips|defn- admission-data|defn- understate-first-central-size!" \
  abc/test/abc/tools/source_bundle_test.clj
```

Expected: every named definition has exactly one match. Read the complete
current definitions before replacing them; stop and revise the plan if entry
keys, arities, or helper names differ.

- [ ] **Step 3: Write failing scan/admission tests**

Append to `abc/test/abc/tools/source_bundle_test.clj`:

```clojure
(deftest bounded-scan-streams-rejected-bundles-test
  (with-zips
    [asset (write-zip! (temp-file ".zip")
                       [["image.png" (utf8-bytes "12345")]])
     collision (write-zip! (temp-file ".zip")
                           [["A.png" (utf8-bytes "a")]
                            ["a.png" (utf8-bytes "bb")]
                            ["work.txt" (utf8-bytes "body")]])]
    (let [asset-scan (source-bundle/scan-zip asset)
          collision-scan (source-bundle/scan-zip collision)]
      (is (= {:member-count 1 :max-member-bytes 5 :total-bytes 5
              :utf8-count 1 :legacy-count 0
              :declared-actual-size-mismatches []}
             (:stats asset-scan)))
      (is (= [] (:semantic-text-candidates asset-scan)))
      (is (nil? (:primary-text-bytes asset-scan)))
      (is (= :no-primary-text-member
             (:reason (admission-data
                       #(source-bundle/admit-scan! asset-scan)))))
      (is (= 3 (get-in collision-scan [:stats :member-count])))
      (is (= ["work.txt"] (:semantic-text-candidates collision-scan)))
      (is (= "body" (String. ^bytes (:primary-text-bytes collision-scan)
                               StandardCharsets/UTF_8)))
      (is (true? (get-in collision-scan
                         [:collision-evidence :unicode-case-collision?])))
      (let [collision-error
            (try
              (source-bundle/admit-scan! collision-scan)
              nil
              (catch clojure.lang.ExceptionInfo t t))]
        (is (source-bundle/admission-error? collision-error))
        (is (= :case-fold-member-path-collision
               (:reason (ex-data collision-error))))))))

(deftest efs-count-is-independent-of-decoder-name-source-test
  (with-zips
    [efs-zip (write-zip!
              (temp-file ".zip")
              [["作品.txt" (utf8-bytes "本文")]]
              {:efs true
               :unicode-extra
               ZipArchiveOutputStream$UnicodeExtraFieldPolicy/NEVER})
     extra-zip (write-zip!
                (temp-file ".zip")
                [["作品.txt" (utf8-bytes "本文")]]
                {:efs false
                 :unicode-extra
                 ZipArchiveOutputStream$UnicodeExtraFieldPolicy/ALWAYS})]
    (let [efs-scan (source-bundle/scan-zip efs-zip)
          extra-scan (source-bundle/scan-zip extra-zip)]
      (is (= "efs-utf8" (get-in efs-scan [:members 0 "name_source"])))
      (is (= [1 0] [(get-in efs-scan [:stats :utf8-count])
                    (get-in efs-scan [:stats :legacy-count])]))
      (is (= "unicode-extra"
             (get-in extra-scan [:members 0 "name_source"])))
      (is (= [0 1] [(get-in extra-scan [:stats :utf8-count])
                    (get-in extra-scan [:stats :legacy-count])]))))

(deftest bounded-scan-pins-declared-versus-actual-bytes-test
  (with-zips [zip (write-zip! (temp-file ".zip")
                              [["work.txt" (utf8-bytes "12345")]])]
    (understate-first-central-size! zip 1)
    (let [scan (source-bundle/scan-zip zip)]
      (is (= 5 (get-in scan [:stats :max-member-bytes])))
      (is (= 5 (get-in scan [:stats :total-bytes])))
      (is (= [{:member-path "work.txt" :declared-bytes 1 :actual-bytes 5}]
             (get-in scan [:stats :declared-actual-size-mismatches]))))))

(deftest inspect-zip-is-scan-plus-admission-test
  (with-zips [zip (write-zip! (temp-file ".zip")
                              [["work.txt" (utf8-bytes "body")]
                               ["figure.png" (byte-array [1 2 3])]])]
    (let [scanned (source-bundle/admit-scan!
                   (source-bundle/scan-zip zip))
          inspected (source-bundle/inspect-zip zip)]
      ;; Clojure byte-array equality is identity, not content equality.
      (is (= (dissoc scanned :primary-text-bytes)
             (dissoc inspected :primary-text-bytes)))
      (is (java.util.Arrays/equals
           ^bytes (:primary-text-bytes scanned)
           ^bytes (:primary-text-bytes inspected))))))

(deftest bounded-scan-failures-precede-logical-admission-test
  (with-zips [zip (write-zip! (temp-file ".zip")
                              [["image.png" (utf8-bytes "12345")]])]
    ;; Avoid the declared-size pre-check so the actual-byte stream proves the
    ;; precedence over the bundle's simultaneous no-primary defect.
    (understate-first-central-size! zip 1)
    (is (= :member-too-large
           (:reason
            (admission-data
             #(source-bundle/scan-zip
               zip {:max-members 10
                    :max-member-bytes 4
                    :max-total-bytes 100})))))))
```

Change staging-cleanup test redefinitions from
`#'source-bundle/inspect-open-zip` to `#'source-bundle/scan-open-zip` after the
new private function exists. Keep the metadata-only test until Task 2.

- [ ] **Step 4: Verify RED**

```sh
cd abc
clojure -M:test:kaocha -m kaocha.runner --focus abc.tools.source-bundle-test
```

Expected: missing `scan-zip` and `admit-scan!` compilation failures.

- [ ] **Step 5: Expose marker authentication and one collision analysis**

Add `admission-error?` after `fail!`:

```clojure
(defn admission-error? [throwable]
  (and (instance? clojure.lang.ExceptionInfo throwable)
       (true? (::admission-error (ex-data throwable)))))
```

Add `path-collision-analysis` after the existing `unicode-fold` definition so
the var resolves in Clojure source order:

```clojure
(defn- path-collision-analysis [paths]
  (let [by-path (group-by identity paths)
        by-fold (group-by unicode-fold paths)]
    {:nfc-collisions
     (->> by-path
          (filter #(> (count (val %)) 1))
          (sort-by key)
          (mapv (fn [[path duplicates]]
                  {:path path :member-count (count duplicates)})))
     :unicode-case-collisions
     (->> by-fold
          (keep (fn [[folded folded-paths]]
                  (let [distinct-paths (vec (sort (distinct folded-paths)))]
                    (when (> (count distinct-paths) 1)
                      {:folded-path folded :paths distinct-paths}))))
          (sort-by :folded-path)
          vec)}))
```

Replace the duplicate/case-fold blocks inside `validate-identity-object!` with
calls to this analysis, retaining the existing identity reasons:

```clojure
(let [{:keys [nfc-collisions unicode-case-collisions]}
      (path-collision-analysis paths)]
  (when (seq nfc-collisions)
    (identity-fail! :identity-member-path-collision {:paths paths}))
  (when (seq unicode-case-collisions)
    (identity-fail! :identity-member-case-fold-collision {:paths paths})))
```

- [ ] **Step 6: Extend `read-member!` without changing persisted members**

Replace it with the complete function below. The extra declared/actual keys are
siblings of `:metadata` and therefore never enter persisted manifests.

```clojure
(defn- read-member!
  [archive-path archive {:keys [entry path decoded-path]} retained-path
   total-bytes {:keys [max-member-bytes max-total-bytes]}]
  (let [digest (MessageDigest/getInstance "SHA-256")
        retained? (= path retained-path)
        retained (when retained? (ByteArrayOutputStream.))
        declared-bytes (.getSize ^ZipArchiveEntry entry)
        buffer (byte-array 8192)]
    (with-open [input (DigestInputStream. (.getInputStream ^ZipFile archive entry)
                                         digest)]
      (loop [member-bytes 0]
        (let [n (.read input buffer)]
          (if (neg? n)
            {:metadata {"path" path
                        "decoded_path" decoded-path
                        "name_source" (name-source entry)
                        "byte_length" member-bytes
                        "member_hash" (hash/format-sha256
                                       (hash/bytes->hex (.digest digest)))}
             :declared-bytes declared-bytes
             :actual-bytes member-bytes
             :efs-utf8-flag?
             (.usesUTF8ForNames (.getGeneralPurposeBit ^ZipArchiveEntry entry))
             :primary-bytes (when retained? (.toByteArray retained))}
            (let [next-member (+ member-bytes n)
                  next-total (+ @total-bytes n)]
              (when (> next-member max-member-bytes)
                (fail! :member-too-large archive-path
                       {:path path :actual-bytes next-member
                        :limit max-member-bytes}))
              (when (> next-total max-total-bytes)
                (fail! :total-too-large archive-path
                       {:path path :actual-bytes next-total
                        :limit max-total-bytes}))
              (vreset! total-bytes next-total)
              (when retained? (.write retained buffer 0 n))
              (recur next-member))))))))
```

Do not add declared size to `:metadata`, the persisted manifest, or the
identity object.

- [ ] **Step 7: Extract `scan-open-zip` and `scan-zip`**

Replace `validated-parser-entries` and `inspect-open-zip` with:

```clojure
(defn- decoded-parser-entries [archive-path archive limits]
  (let [entries (sort-by :path
                         (parser-decoded-entries archive-path archive))]
    (validate-declared-limits! archive-path entries limits)
    entries))

(defn- scan-open-zip [archive-path stable-file limits]
  (with-open [archive (open-zip-archive archive-path stable-file)]
    (let [entries (decoded-parser-entries archive-path archive limits)
          candidates (filterv #(primary-candidate? (:path %)) entries)
          collision-analysis
          (path-collision-analysis (mapv :path entries))
          retained-path (when (= 1 (count candidates))
                          (:path (first candidates)))
          total-bytes (volatile! 0)
          reads (mapv #(read-member! archive-path archive % retained-path
                                     total-bytes limits)
                      entries)
          members (mapv :metadata reads)
          actuals (mapv :actual-bytes reads)]
      {:archive-path (str archive-path)
       :archive-hash (hash/format-sha256 (files/sha256-file stable-file))
       :members members
       :semantic-text-candidates (mapv :path candidates)
       :collision-evidence
       {:nfc-collision? (boolean (seq (:nfc-collisions
                                      collision-analysis)))
        :unicode-case-collision?
        (boolean (seq (:unicode-case-collisions collision-analysis)))}
       :stats
       {:member-count (count members)
        :max-member-bytes (reduce max 0 actuals)
        :total-bytes @total-bytes
        :utf8-count (count (filter :efs-utf8-flag? reads))
        :legacy-count (count (remove :efs-utf8-flag? reads))
        :declared-actual-size-mismatches
        (->> reads
             (keep (fn [{:keys [metadata declared-bytes actual-bytes]}]
                     (when (and (not (neg? declared-bytes))
                                (not= declared-bytes actual-bytes))
                       {:member-path (get metadata "path")
                        :declared-bytes declared-bytes
                        :actual-bytes actual-bytes})))
             vec)}
       :primary-text-bytes
       (:primary-bytes (first (filter :primary-bytes reads)))})))
```

Keep the existing `stage-archive!` immediately after `scan-open-zip`, then add
the public wrapper after `stage-archive!` so Clojure resolves the var in source
order:

```clojure
(defn scan-zip
  ([zip-file] (scan-zip zip-file default-limits))
  ([zip-file limits]
   (let [staged (stage-archive! zip-file)]
     (try
       (scan-open-zip zip-file staged (merge default-limits limits))
       (finally (Files/deleteIfExists (.toPath staged)))))))
```

- [ ] **Step 8: Extract pure admission with shared collision analysis**

Add:

```clojure
(defn- validate-admission-collisions! [archive-path members]
  (let [{:keys [nfc-collisions unicode-case-collisions]}
        (path-collision-analysis (mapv #(get % "path") members))]
    (when-let [collision (first nfc-collisions)]
      (fail! :duplicate-member-path archive-path collision))
    (when-let [collision (first unicode-case-collisions)]
      (fail! :case-fold-member-path-collision archive-path collision)))
  members)

(defn admit-scan! [scan]
  (let [archive-path (:archive-path scan)
        members (validate-admission-collisions! archive-path (:members scan))
        candidates (:semantic-text-candidates scan)
        primary-path (case (count candidates)
                       0 (fail! :no-primary-text-member archive-path
                                {:candidates []})
                       1 (first candidates)
                       (fail! :multiple-primary-text-members archive-path
                              {:candidates candidates}))
        primary-hash (get (some #(when (= primary-path (get % "path")) %)
                                members)
                          "member_hash")
        identity-object
        (validate-identity-object!
         {"construction" construction
          "members" (mapv #(select-keys % ["path" "member_hash"]) members)
          "primary_text_member" primary-path})]
    (when-not (and primary-hash (some? (:primary-text-bytes scan)))
      (fail! :primary-text-retention-mismatch archive-path
             {:primary-text-member primary-path}))
    {:identity-object identity-object
     :bundle-hash (bundle-identity-hash identity-object)
     :archive-hash (:archive-hash scan)
     :members members
     :primary-text-member primary-path
     :primary-text-hash primary-hash
     :primary-text-bytes (:primary-text-bytes scan)}))

(defn inspect-zip
  ([zip-file] (inspect-zip zip-file default-limits))
  ([zip-file limits] (admit-scan! (scan-zip zip-file limits))))
```

Delete old `collision-evidence`, `validate-entry-collisions!`,
`choose-primary!`, `validated-parser-entries`, and `inspect-open-zip` only after
the new callers compile. Keep `inspect-zip-metadata` until Task 2; temporarily
make it derive booleans through `path-collision-analysis` rather than the
deleted helper:

```clojure
(defn inspect-zip-metadata [zip-file]
  (with-open [archive (open-zip-archive zip-file zip-file)]
    (let [entries (parser-decoded-entries zip-file archive)
          analysis (path-collision-analysis (mapv :path entries))]
      {:semantic-text-member-count
       (count (filter #(primary-candidate? (:path %)) entries))
       :nfc-collision? (boolean (seq (:nfc-collisions analysis)))
       :unicode-case-collision?
       (boolean (seq (:unicode-case-collisions analysis)))})))
```

- [ ] **Step 9: Verify GREEN and commit**

```sh
cd abc
clojure -M:test:kaocha -m kaocha.runner \
  --focus abc.tools.source-bundle-test \
  --focus abc.tools.soranoha-test \
  --focus abc.tools.materialize-source-snapshot-test
clj-kondo --lint src/abc/tools/source_bundle.clj \
  test/abc/tools/source_bundle_test.clj
cljfmt check src/abc/tools/source_bundle.clj \
  test/abc/tools/source_bundle_test.clj
```

Expected: all commands exit 0 and identity fixtures remain byte-identical.

```sh
git add abc/src/abc/tools/source_bundle.clj \
  abc/test/abc/tools/source_bundle_test.clj
git commit -m "refactor(source): expose bounded bundle scan"
```

---

### Task 2: Replace declared-size corpus evidence with the shared scan

**Files:**
- Modify: `abc/src/abc/tools/source_bundle.clj`
- Modify: `abc/src/abc/tools/source_bundle_report.clj`
- Modify: `abc/test/abc/tools/source_bundle_test.clj`
- Modify: `abc/test/abc/tools/source_bundle_report_test.clj`
- Modify: `abc/data/source-bundle/aozorabunko-0e9ea3e-summary.json`

**Interfaces:**
- Consumes `scan-zip`, `admit-scan!`, and `admission-error?` from Task 1.
- Removes `inspect-zip-metadata`, `raw-zip-stats`, and their archive-only dependencies.
- Produces report construction `abc-source-bundle-streamed-evidence-v1`.

- [ ] **Step 1: Add a real declared-size patch helper to report tests**

Add `ByteBuffer` and `ByteOrder` to the imports, then copy the tested helper:

```clojure
(defn- understate-first-central-size! [file declared-size]
  (let [data (Files/readAllBytes (.toPath file))
        signature (byte-array [0x50 0x4b 0x01 0x02])
        offset (first
                (for [start (range (inc (- (alength data)
                                           (alength signature))))
                      :when (every? true?
                                    (map-indexed
                                     (fn [i b]
                                       (= b (aget data (+ start i))))
                                     signature))]
                  start))]
    (when-not offset
      (throw (ex-info "central directory signature not found" {:file file})))
    (-> (ByteBuffer/wrap data)
        (.order ByteOrder/LITTLE_ENDIAN)
        (.putInt (+ offset 24) (int declared-size)))
    (Files/write (.toPath file) data
                 (make-array java.nio.file.OpenOption 0))
    file))
```

- [ ] **Step 2: Write the failing report contract**

In `measure-pinned-corpus-shape-test`, bind `limit.zip`, call
`(understate-first-central-size! limit-zip 1)`, and expect:

The existing `nfc-collision.zip` call passes `true` to `card-zip`, so its two
entries have raw EFS bit 11 set and make the expected flagged count reachable.

```clojure
{"measurement_construction" "abc-source-bundle-streamed-evidence-v1"
 "readable_zip_count" 6
 "unreadable_zip_count" 1
 "admitted_zip_count" 3
 "rejected_zip_count" 4
 "rejection_reason_counts"
 {"case-fold-member-path-collision" 1
  "duplicate-member-path" 1
  "no-primary-text-member" 1
  "unreadable-zip" 1}
 "semantic_text_member_counts" {"0" 3 "1" 3}
 "utf8_flagged_entry_count" 2
 "legacy_flagged_entry_count" 10
 "nfc_collision_bundle_count" 1
 "unicode_case_collision_bundle_count" 1
 "max_member_count" 3
 "max_member_bytes" 11
 "max_total_bytes" 21
 "declared_actual_size_mismatch_member_count" 1
 "declared_actual_size_mismatches"
 [{"archive_path" "cards/6/files/limit.zip"
   "member_path" "work.txt"
   "declared_bytes" 1
   "actual_bytes" 11}]
 "java_unreadable_7zz_listable_count" 0
 "java_unreadable_7zz_unlistable_count" 1
 "damaged_paths" ["cards/7/files/damaged.zip"]}
```

Replace the metadata-spoof test with:

```clojure
(deftest evidence-collector-trusts-only-marked-admission-errors-test
  (let [spoof (ex-info "spoof" {:reason :unreadable-zip})]
    (is (identical?
         spoof
         (try
           (with-redefs [source-bundle/scan-zip (fn [_] (throw spoof))]
             (report/measure! (one-card-root)))
           (catch Throwable t t))))))

(deftest production-limit-failures-abort-corpus-evidence-test
  (let [sevenzip-calls (atom 0)
        thrown
        (try
          (with-redefs [source-bundle/default-limits
                        {:max-members 10
                         :max-member-bytes 3
                         :max-total-bytes 100}
                        report/sevenzip-listable?
                        (fn [_] (swap! sevenzip-calls inc) false)]
            (report/measure! (one-card-root)))
          nil
          (catch clojure.lang.ExceptionInfo t t))]
    (is (source-bundle/admission-error? thrown))
    (is (= :member-too-large (:reason (ex-data thrown))))
    (is (zero? @sevenzip-calls))))
```

Update the protected-failure test to redefine `source-bundle/scan-zip` and
`report/sevenzip-listable?`, asserting that `7zz` is never called.

- [ ] **Step 3: Verify RED**

```sh
cd abc
clojure -M:test:kaocha -m kaocha.runner \
  --focus abc.tools.source-bundle-report-test
```

Expected: failures for absent construction/admission fields, declared size 1
instead of actual 11, and old recoverability field names.

- [ ] **Step 4: Replace the report accumulator**

Delete the Commons Compress/charset imports, `legacy-name-charset`, and
`raw-zip-stats`. Rename `sevenzip-readable?` to `sevenzip-listable?`.

Replace `empty-summary` and add disposition helpers:

```clojure
(def measurement-construction
  "abc-source-bundle-streamed-evidence-v1")

(defn- empty-summary []
  {"measurement_construction" measurement-construction
   "readable_zip_count" 0 "unreadable_zip_count" 0
   "admitted_zip_count" 0 "rejected_zip_count" 0
   "rejection_reason_counts" (sorted-map)
   "semantic_text_member_counts" {}
   "utf8_flagged_entry_count" 0 "legacy_flagged_entry_count" 0
   "nfc_collision_bundle_count" 0
   "unicode_case_collision_bundle_count" 0
   "max_member_count" 0 "max_member_bytes" 0 "max_total_bytes" 0
   "declared_actual_size_mismatch_member_count" 0
   "declared_actual_size_mismatches" []
   "java_unreadable_7zz_listable_count" 0
   "java_unreadable_7zz_unlistable_count" 0
   "damaged_paths" []})

(defn- record-rejection [summary reason]
  (-> summary
      (update "rejected_zip_count" inc)
      (update-in ["rejection_reason_counts" (name reason)] (fnil inc 0))))

(defn- admission-reason [scan]
  (try
    (source-bundle/admit-scan! scan)
    nil
    (catch clojure.lang.ExceptionInfo t
      (if (source-bundle/admission-error? t)
        (:reason (ex-data t))
        (throw t)))))

(defn- report-mismatches [root zip-file scan]
  (mapv (fn [{:keys [member-path declared-bytes actual-bytes]}]
          {"archive_path" (relative-path root zip-file)
           "member_path" member-path
           "declared_bytes" declared-bytes
           "actual_bytes" actual-bytes})
        (get-in scan [:stats :declared-actual-size-mismatches])))
```

- [ ] **Step 5: Record readable and unreadable dispositions**

Add:

```clojure
(defn- record-readable [summary root zip-file scan reason]
  (let [{:keys [member-count max-member-bytes total-bytes utf8-count
                legacy-count]} (:stats scan)
        mismatches (report-mismatches root zip-file scan)
        semantic-count (count (:semantic-text-candidates scan))]
    (cond->
     (-> summary
         (update "readable_zip_count" inc)
         (update-in ["semantic_text_member_counts" (str semantic-count)]
                    (fnil inc 0))
         (update "utf8_flagged_entry_count" + utf8-count)
         (update "legacy_flagged_entry_count" + legacy-count)
         (update "max_member_count" max member-count)
         (update "max_member_bytes" max max-member-bytes)
         (update "max_total_bytes" max total-bytes)
         (update "declared_actual_size_mismatch_member_count"
                 + (count mismatches))
         (update "declared_actual_size_mismatches" into mismatches)
         (cond-> (get-in scan [:collision-evidence :nfc-collision?])
           (update "nfc_collision_bundle_count" inc))
         (cond-> (get-in scan
                         [:collision-evidence :unicode-case-collision?])
           (update "unicode_case_collision_bundle_count" inc)))
      (nil? reason) (update "admitted_zip_count" inc)
      reason (record-rejection reason))))

(defn- record-unreadable [summary root zip-file]
  (let [listable? (sevenzip-listable? zip-file)]
    (-> summary
        (update "unreadable_zip_count" inc)
        (record-rejection :unreadable-zip)
        (update (if listable?
                  "java_unreadable_7zz_listable_count"
                  "java_unreadable_7zz_unlistable_count") inc)
        (update "damaged_paths" conj (relative-path root zip-file)))))
```

- [ ] **Step 6: Make `measure!` consume the scan**

```clojure
(defn measure! [aozora-root]
  (reduce
   (fn [summary zip-file]
     (try
       (let [scan (source-bundle/scan-zip zip-file)]
         (record-readable summary aozora-root zip-file scan
                          (admission-reason scan)))
       (catch clojure.lang.ExceptionInfo t
         (if-not (source-bundle/admission-error? t)
           (throw t)
           (if (= :unreadable-zip (:reason (ex-data t)))
             (record-unreadable summary aozora-root zip-file)
             ;; No complete scan means no actual-byte evidence. Fail closed.
             (throw t))))))
   (empty-summary)
   (corpus-zips aozora-root)))
```

Do not catch `Throwable`, and do not classify protected failures through
`7zz`.

- [ ] **Step 7: Delete duplicate metadata APIs**

Delete `inspect-zip-metadata` and its metadata-only test. Verify:

```sh
rg -n "inspect-zip-metadata|raw-zip-stats|sevenzip-readable|recoverable_count" \
  abc/src abc/test abc/data/source-bundle
```

Expected: no matches after the pinned JSON/test migration in Step 9.

- [ ] **Step 8: Verify synthetic GREEN**

```sh
cd abc
clojure -M:test:kaocha -m kaocha.runner \
  --focus abc.tools.source-bundle-test \
  --focus abc.tools.source-bundle-report-test
```

Expected: synthetic tests pass; the old checked-pinned literal may fail until
the next step.

- [ ] **Step 9: Generate and adjudicate the full pinned report**

Resolve the input and run from `abc/` without a machine-local path:

```sh
aozora_root="$(nix eval --raw --impure --expr \
  '(builtins.getFlake (toString ./.)).inputs.aozorabunko-src.outPath')"
/usr/bin/time -f 'streamed_source_bundle_corpus_seconds=%e' \
  clojure -M -m abc.tools.source-bundle-report \
  "$aozora_root" \
  data/source-bundle/aozorabunko-0e9ea3e-summary.json
```

Expected; stop rather than re-bless on any difference:

```text
readable=17884, unreadable=3, admitted=17879, rejected=8
reasons={no-primary-text-member 5, unreadable-zip 3}
max member=12631833, max total=27874310
mismatch count=1
cards/001393/files/50710_ruby_36965.zip /
fushigino_kunino_alice_musical.txt / declared 68007 / actual 68497
7zz-listable=1, 7zz-unlistable=2
```

Update `checked-pinned-evidence-test` with the exact generated map, retaining
all prior pin/readability/encoding/collision/semantic/damaged fields and adding:

```clojure
{"measurement_construction" "abc-source-bundle-streamed-evidence-v1"
 "admitted_zip_count" 17879 "rejected_zip_count" 8
 "rejection_reason_counts"
 {"no-primary-text-member" 5 "unreadable-zip" 3}
 "declared_actual_size_mismatch_member_count" 1
 "declared_actual_size_mismatches"
 [{"archive_path" "cards/001393/files/50710_ruby_36965.zip"
   "member_path" "fushigino_kunino_alice_musical.txt"
   "declared_bytes" 68007 "actual_bytes" 68497}]
 "java_unreadable_7zz_listable_count" 1
 "java_unreadable_7zz_unlistable_count" 2}
```

- [ ] **Step 10: Verify and commit**

```sh
cd abc
clojure -M:test:kaocha -m kaocha.runner \
  --focus abc.tools.source-bundle-test \
  --focus abc.tools.source-bundle-report-test
clj-kondo --lint src/abc/tools/source_bundle.clj \
  src/abc/tools/source_bundle_report.clj \
  test/abc/tools/source_bundle_test.clj \
  test/abc/tools/source_bundle_report_test.clj
cljfmt check src/abc/tools/source_bundle.clj \
  src/abc/tools/source_bundle_report.clj \
  test/abc/tools/source_bundle_test.clj \
  test/abc/tools/source_bundle_report_test.clj
```

Expected: exit 0.

```sh
nix build ./abc#checks.x86_64-linux.source-bundle-corpus \
  --rebuild --print-build-logs
```

Expected: byte-for-byte report equality.

```sh
git add abc/src/abc/tools/source_bundle.clj \
  abc/src/abc/tools/source_bundle_report.clj \
  abc/test/abc/tools/source_bundle_test.clj \
  abc/test/abc/tools/source_bundle_report_test.clj \
  abc/data/source-bundle/aozorabunko-0e9ea3e-summary.json
git commit -m "test(source): stream pinned corpus evidence"
```

---

### Task 3: Record evidence and run complete verification

**Files:**
- Modify: `abc/docs/adr/0033-source-bundle-identity.md`
- Modify: `abc/docs/superpowers/specs/2026-07-12-source-bundle-identity-design.md`
- Modify: `abc/docs/superpowers/specs/2026-07-12-streamed-source-bundle-corpus-evidence-design.md`

**Interfaces:**
- Consumes the checked streamed report and Task 1/2 wall times.
- Produces accepted documentation of actual-byte evidence without changing identity policy.

- [ ] **Step 1: Update evidence prose**

Add this evidence to ADR 0033 and the original source-bundle design:

```markdown
The checked corpus report uses the same staged, bounded body scan as production
admission. Actual streamed maxima remain 12,631,833 bytes per member and
27,874,310 bytes per bundle. It also pins one understated central-directory
declaration: `cards/001393/files/50710_ruby_36965.zip` declares 68,007 bytes for
`fushigino_kunino_alice_musical.txt`, while the bounded stream reads 68,497.
```

Do not change ADR status, construction, limits, damaged-archive posture, or
identity roles.

In the new streamed-evidence spec:

- set `Status: Implemented`;
- add `Implementation Evidence` naming `scan-zip`, `admit-scan!`, the deleted
  APIs, checked JSON, focused tests, and Nix check;
- record the baseline and streamed wall times from Tasks 1 and 2;
- state that the first timing is evidence, not a permanent CI threshold.

- [ ] **Step 2: Run the full verification matrix**

From the root:

```sh
just python-quality
just nix-format-check
nix build ./abc#checks.x86_64-linux.clj-kondo \
  ./abc#checks.x86_64-linux.clj-nix-focused-tests \
  ./abc#checks.x86_64-linux.source-bundle-corpus \
  --print-build-logs
just validate-migration
```

From `abc/`:

```sh
env TEI_SCHEMA_PATH=$PWD/schemas/tei-profile.rng \
  clojure -M:test:kaocha -m kaocha.runner \
  --focus abc.tools.source-bundle-test \
  --focus abc.tools.source-bundle-report-test \
  --focus abc.tools.soranoha-test \
  --focus abc.tools.materialize-source-snapshot-test \
  --focus abc.sim.content-sim-test
```

Expected: every command exits 0. Identity fixture bytes, reuse, snapshot
validation, admission disposition, and D7 composition remain unchanged.

- [ ] **Step 3: Run negative audits**

```sh
rg -n "inspect-zip-metadata|raw-zip-stats|sevenzip-readable|recoverable_count" \
  abc/src abc/test abc/data/source-bundle
rg -n "7zz x" abc/src/abc/tools/source_bundle.clj \
  abc/src/abc/tools/source_bundle_report.clj
git diff --check
```

Expected: no `rg` matches and a clean diff check.

- [ ] **Step 4: Run governance and commit documentation**

```sh
cd abc
clojure -M:abc/adr-governance
```

Expected: exit 0.

```sh
git add abc/docs/adr/0033-source-bundle-identity.md \
  abc/docs/superpowers/specs/2026-07-12-source-bundle-identity-design.md \
  abc/docs/superpowers/specs/2026-07-12-streamed-source-bundle-corpus-evidence-design.md
git commit -m "docs(source): record streamed corpus evidence"
```

- [ ] **Step 5: Request whole-change review**

Review the complete implementation range for unchanged v1 identity bytes, one
shared staged scan, actual maxima, exact mismatch evidence, bounded rejection
cost, protected error taxonomy, listability-only `7zz`, duplicate-path
deletion, runtime evidence, and all required gates. Do not merge with any
Critical or Important finding open.
