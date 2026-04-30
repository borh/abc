# Upstream Ingest Drift Awareness Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Extend the after-upstream `aozora-history-audit` workflow so accepted drift sidecars can flag ordinary upstream edits to already drift-sensitive persons, without making raw ingest rewrite Aozora identities.

**Architecture:** Keep `aozora-ingest` source-faithful. Add a small drift-participant comparison layer that loads validated drift indexes from an optional persons directory, compares previous/current generated person hashes for those indexed person IDs, and appends the review queue to the audit report.

**Tech Stack:** Clojure, existing `abc.tools.person-drift` validation, existing `abc.tools.person-drift-history` corpus readers/hash logic, deterministic JSON output, `clojure.test`, Nix app checks.

---

## File Structure

- Modify: `src/abc/tools/aozora_history_audit.clj` — add CLI flags, load drift sidecars, compute participant update entries, include them in the audit result, and make exit status respect `--fail-on-drift-participant-updates`.
- Modify: `src/abc/tools/person_drift_history.clj` — expose a small reusable function for person-record hash maps.
- Modify: `test/abc/tools/aozora_history_audit_test.clj` — cover no-sidecar, invalid-sidecar, hash-change, and fail-on-update behavior.
- Modify: `docs/next-steps.md` — add the canonical audit command including `--drift-persons-dir examples/v0/example-persons`.

---

## Task 1: Add Drift Participant Comparison Tests

**Files:**
- Modify: `test/abc/tools/aozora_history_audit_test.clj`

- [ ] **Step 1: Add a fixture helper that writes a minimal drift event + indexes**

Add a helper near the existing test fixture helpers. Reuse `abc.tools.person-drift/materialize-event-id` and `abc.tools.manifest/schema-hash` so the fixture follows the production schemas:

```clojure
(defn- write-drift-sidecars! [persons-dir event]
  (let [event-with-id (drift/materialize-event-id event)
        event-id (get event-with-id "drift_event_id")
        events-dir (io/file persons-dir "_events")
        indexes-dir (io/file persons-dir "_indexes")]
    (.mkdirs events-dir)
    (.mkdirs indexes-dir)
    (json/write-deterministic-json-file!
     (io/file events-dir (str event-id ".json"))
     event-with-id)
    (doseq [participant (get event-with-id "participants")]
      (json/write-deterministic-json-file!
       (io/file indexes-dir (str (get participant "person_id") ".json"))
       {"schema_id" drift/index-schema-id
        "schema_hash" (manifest/schema-hash drift/index-schema-path)
        "person_id" (get participant "person_id")
        "drift_event_ids" [event-id]}))
    event-with-id))
```

Add these namespace aliases:

```clojure
[abc.tools.files :as files]
[abc.tools.json :as json]
[abc.tools.manifest :as manifest]
[abc.tools.person-drift :as drift]
[abc.tools.person-record :as person-record]
```

- [ ] **Step 2: Add small generated-corpus helpers**

Add these helpers near the existing `row`/`csv-text` helpers:

```clojure
(defn- synthetic-person-record [person-id family-name]
  {"person_record_schema_id" "https://w3id.org/abc/schemas/person-record.schema.json"
   "person_record_schema_hash" (manifest/schema-hash "schemas/person-record.schema.json")
   "person_id" person-id
   "family_name" family-name
   "given_name" "人"
   "family_name_reading" "せい"
   "given_name_reading" "ひと"
   "family_name_sort" "せい"
   "given_name_sort" "ひと"
   "family_name_romaji" "Sei"
   "given_name_romaji" "Hito"
   "date_of_birth" "1900-01-01"
   "date_of_death" "1970-01-01"
   "person_copyright_expired" true
   "external_links" []})

(defn- write-corpus! [root persons-by-id]
  (let [persons-dir (io/file root "persons")
        works-dir (io/file root "works")]
    (.mkdirs persons-dir)
    (.mkdirs works-dir)
    (doseq [[person-id record] persons-by-id]
      (json/write-deterministic-json-file!
       (io/file persons-dir (str person-id ".json"))
       record))
    (json/write-deterministic-json-file!
     (io/file works-dir "000100.json")
     {"metadata_record_schema_id" "https://w3id.org/abc/schemas/metadata-record.schema.json"
      "metadata_record_schema_hash" (manifest/schema-hash "schemas/metadata-record.schema.json")
      "work" {"work_id" "000100"
              "title" "テスト作品"
              "title_reading" "てすとさくひん"
              "title_sort" "てすとさくひん"
              "subtitle" nil
              "subtitle_reading" nil
              "original_title" nil
              "first_appearance" nil
              "ndc" "NDC 913"
              "orthography" "新字新仮名"
              "work_copyright_expired" true
              "publication_date" "1997-10-29"
              "last_updated" "2022-07-16"
              "card_url" "https://www.aozora.gr.jp/cards/000001/card100.html"
              "source_editions" [{"edition_name" "テスト作品"
                                  "publisher" "テスト出版社"
                                  "first_edition_year" nil
                                  "input_edition" nil
                                  "proofing_edition" nil
                                  "parent_edition_name" nil
                                  "parent_publisher" nil
                                  "parent_first_edition_year" nil}]}
      "contributors" (vec
                      (for [[person-id record] (sort-by key persons-by-id)]
                        {"person_id" person-id
                         "person_record_hash" (person-record/record-hash record)
                         "relation_to_work" "著者"}))})))
```

- [ ] **Step 3: Add a unit test for no drift artifacts**

```clojure
(deftest drift-participant-updates-empty-without-sidecars-test
  (let [previous-dir (temp-dir "abc-audit-prev")
        current-dir (temp-dir "abc-audit-cur")
        drift-dir (temp-dir "abc-audit-drift")]
    (try
      (write-corpus! previous-dir {"000879" (synthetic-person-record "000879" "芥川")})
      (write-corpus! current-dir {"000879" (synthetic-person-record "000879" "芥川")})
      (is (= []
             (audit/drift-participant-updates
              {:previous-dir (str previous-dir)
               :current-dir (str current-dir)
               :drift-persons-dir (str drift-dir)})))
      (finally
        (delete-recursive previous-dir)
        (delete-recursive current-dir)
        (delete-recursive drift-dir)))))
```

Expected first run: FAIL because `drift-participant-updates` is not defined.

- [ ] **Step 4: Add a unit test for a drift participant hash change**

```clojure
(deftest drift-participant-updates-report-hash-changes-test
  (let [previous-dir (temp-dir "abc-audit-prev")
        current-dir (temp-dir "abc-audit-cur")
        drift-dir (temp-dir "abc-audit-drift")]
    (try
      (write-corpus! previous-dir {"000879" (synthetic-person-record "000879" "芥川")})
      (write-corpus! current-dir {"000879" (synthetic-person-record "000879" "芥川改")})
      (let [previous-person (files/read-json (io/file previous-dir "persons" "000879.json"))
            successor (synthetic-person-record "abc-000000000001" "芥川一")
            event (write-drift-sidecars!
                   drift-dir
                   {"schema_id" drift/event-schema-id
                    "schema_hash" (manifest/schema-hash drift/event-schema-path)
                    "drift_event_type" "split"
                    "date" "2026-04-30"
                    "participants" [{"snapshot_id" "post-abc-000000000001"
                                     "person_id" "abc-000000000001"
                                     "person_record_hash" (person-record/record-hash successor)}
                                    {"snapshot_id" "pre-000879"
                                     "person_id" "000879"
                                     "person_record_hash" (person-record/record-hash previous-person)}]
                    "evidence" ["https://example.org/drift-evidence"]
                    "prov" {"used" ["pre-000879"]
                            "was_generated_by" ["post-abc-000000000001"]
                            "qualified_association" {"agent" "https://w3id.org/abc/agents/test"
                                                     "had_role" "abc:DriftEditor"}}})
            updates (audit/drift-participant-updates
                     {:previous-dir (str previous-dir)
                      :current-dir (str current-dir)
                      :drift-persons-dir (str drift-dir)})]
        (is (= [{"person_id" "000879"
                 "change_type" "hash_changed"
                 "previous_hash" (person-record/record-hash previous-person)
                 "current_hash" (person-record/record-hash
                                 (files/read-json (io/file current-dir "persons" "000879.json")))
                 "drift_event_ids" [(get event "drift_event_id")]}]
               updates)))
      (finally
        (delete-recursive previous-dir)
        (delete-recursive current-dir)
        (delete-recursive drift-dir)))))
```

Expected first run: FAIL because the production helper does not exist.

- [ ] **Step 5: Add an invalid-sidecar test**

```clojure
(deftest drift-participant-updates-reject-invalid-sidecars-test
  (let [previous-dir (temp-dir "abc-audit-prev")
        current-dir (temp-dir "abc-audit-cur")
        drift-dir (temp-dir "abc-audit-drift")
        indexes-dir (io/file drift-dir "_indexes")]
    (try
      (write-corpus! previous-dir {"000879" (synthetic-person-record "000879" "芥川")})
      (write-corpus! current-dir {"000879" (synthetic-person-record "000879" "芥川")})
      (.mkdirs indexes-dir)
      (json/write-deterministic-json-file!
       (io/file indexes-dir "000879.json")
       {"schema_id" drift/index-schema-id
        "schema_hash" (manifest/schema-hash drift/index-schema-path)
        "person_id" "000879"
        "drift_event_ids" ["sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"]})
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"drift sidecars failed validation"
           (audit/drift-participant-updates
            {:previous-dir (str previous-dir)
             :current-dir (str current-dir)
             :drift-persons-dir (str drift-dir)})))
      (finally
        (delete-recursive previous-dir)
        (delete-recursive current-dir)
        (delete-recursive drift-dir)))))
```

Expected first run: FAIL because the production helper does not exist.

- [ ] **Step 6: Run the focused test namespace**

Run:

```bash
clojure -M:test -e "(require 'abc.tools.aozora-history-audit-test) (clojure.test/run-tests 'abc.tools.aozora-history-audit-test)"
```

Expected: FAIL with missing `drift-participant-updates`.

- [ ] **Step 7: Commit the failing tests**

```bash
git add test/abc/tools/aozora_history_audit_test.clj
git commit -m "test: cover drift participant update audit"
```

---

## Task 2: Implement Drift Participant Update Detection

**Files:**
- Modify: `src/abc/tools/aozora_history_audit.clj`
- Modify: `src/abc/tools/person_drift_history.clj`

- [ ] **Step 1: Expose reusable person hash reading**

In `src/abc/tools/person_drift_history.clj`, make a public helper:

```clojure
(defn person-hashes
  "Return a sorted map of person_id to person_record_hash for a generated
  corpus root containing persons/*.json."
  [root]
  (into (sorted-map)
        (map (fn [[person-id record]]
               [person-id (person-record/record-hash record)]))
        (read-persons root)))
```

- [ ] **Step 2: Load valid drift index mappings**

In `src/abc/tools/aozora_history_audit.clj`, require drift support:

```clojure
[abc.tools.person-drift :as drift]
```

Add:

```clojure
(defn- drift-index-map [persons-dir]
  (let [result (drift/validate-drift-events! {:persons-dir persons-dir})]
    (case (:status result)
      :not-present {}
      :ok
      (let [indexes-dir (io/file persons-dir "_indexes")]
        (into (sorted-map)
              (map (fn [file]
                     (let [index (abc-json/read-json-file file)]
                       [(get index "person_id")
                        (vec (sort (get index "drift_event_ids")))])))
              (sort-by #(.getName ^java.io.File %)
                       (filter #(and (.isFile ^java.io.File %)
                                     (clojure.string/ends-with? (.getName ^java.io.File %) ".json"))
                               (or (.listFiles indexes-dir)
                                   (make-array java.io.File 0)))))))
      :error
      (throw (ex-info "drift sidecars failed validation"
                      {:persons-dir persons-dir
                       :failures (:failures result)})))))
```

- [ ] **Step 3: Add the public comparison helper**

```clojure
(defn drift-participant-updates
  "Return review entries for generated person-record changes that touch
  person_ids already mentioned by accepted drift indexes."
  [{:keys [previous-dir current-dir drift-persons-dir]}]
  (if-not drift-persons-dir
    []
    (let [drift-indexes (drift-index-map drift-persons-dir)
          previous-hashes (drift-history/person-hashes previous-dir)
          current-hashes (drift-history/person-hashes current-dir)]
      (vec
       (keep (fn [[person-id event-ids]]
               (let [previous-hash (get previous-hashes person-id)
                     current-hash (get current-hashes person-id)]
                 (cond
                   (= previous-hash current-hash) nil
                   (and previous-hash current-hash)
                   {"person_id" person-id
                    "change_type" "hash_changed"
                    "previous_hash" previous-hash
                    "current_hash" current-hash
                    "drift_event_ids" event-ids}
                   previous-hash
                   {"person_id" person-id
                    "change_type" "removed"
                    "previous_hash" previous-hash
                    "current_hash" nil
                    "drift_event_ids" event-ids}
                   current-hash
                   {"person_id" person-id
                    "change_type" "added"
                    "previous_hash" nil
                    "current_hash" current-hash
                    "drift_event_ids" event-ids})))
             drift-indexes)))))
```

- [ ] **Step 4: Include the list in `audit!`**

After `drift-report` is computed, add:

```clojure
drift-participant-updates
(drift-participant-updates
 {:previous-dir (str previous-corpus)
  :current-dir (str current-corpus)
  :drift-persons-dir drift-persons-dir})
```

Then add the top-level report field:

```clojure
:drift-participant-updates drift-participant-updates
```

- [ ] **Step 5: Run the focused tests**

Run:

```bash
clojure -M:test -e "(require 'abc.tools.aozora-history-audit-test) (clojure.test/run-tests 'abc.tools.aozora-history-audit-test)"
```

Expected: PASS for the tests added in Task 1.

- [ ] **Step 6: Commit**

```bash
git add src/abc/tools/person_drift_history.clj src/abc/tools/aozora_history_audit.clj test/abc/tools/aozora_history_audit_test.clj
git commit -m "feat: flag drift participant updates in upstream audit"
```

---

## Task 3: Add CLI Flags and Exit Behavior

**Files:**
- Modify: `src/abc/tools/aozora_history_audit.clj`
- Modify: `test/abc/tools/aozora_history_audit_test.clj`

- [ ] **Step 1: Add CLI options**

Extend `cli-options`:

```clojure
[nil "--drift-persons-dir DIR"
 "Validated persons directory containing optional _events/ and _indexes/ drift sidecars"]
[nil "--fail-on-drift-participant-updates"
 "Exit 1 when accepted drift participants changed across the audited upstream refs"]
```

Thread `:drift-persons-dir` through `audit!`.

- [ ] **Step 2: Add exit-count logic**

In `-main`, after `candidate-count`, add:

```clojure
drift-participant-update-count
(count (:drift-participant-updates result))
```

Then extend the exit condition:

```clojure
(and (:fail-on-drift-participant-updates options)
     (pos? drift-participant-update-count))
```

- [ ] **Step 3: Extend the existing top-level audit test**

In `audit-history-uses-git-refs-and-flags-real-split-evidence-test`, add a
`drift-dir` temp directory and write a split event before calling `audit/audit!`.
Use the same old/new IDs already in that test: predecessor `000001`, successors
`abc-000000000001` and `abc-000000000002`. Use fixed valid sha256-shaped hashes
because drift validation checks event graph coherence, not the generated corpus
files:

```clojure
(let [event (write-drift-sidecars!
             drift-dir
             {"schema_id" drift/event-schema-id
              "schema_hash" (manifest/schema-hash drift/event-schema-path)
              "drift_event_type" "split"
              "date" "2026-04-30"
              "participants" [{"snapshot_id" "post-abc-000000000001"
                               "person_id" "abc-000000000001"
                               "person_record_hash" "sha256:1111111111111111111111111111111111111111111111111111111111111111"}
                              {"snapshot_id" "post-abc-000000000002"
                               "person_id" "abc-000000000002"
                               "person_record_hash" "sha256:2222222222222222222222222222222222222222222222222222222222222222"}
                              {"snapshot_id" "pre-000001"
                               "person_id" "000001"
                               "person_record_hash" "sha256:0000000000000000000000000000000000000000000000000000000000000000"}]
              "evidence" ["https://example.org/drift-evidence"]
              "prov" {"used" ["pre-000001"]
                      "was_generated_by" ["post-abc-000000000001"
                                          "post-abc-000000000002"]
                      "qualified_association" {"agent" "https://w3id.org/abc/agents/test"
                                               "had_role" "abc:DriftEditor"}}})
      result (audit/audit! {:aozora-repo (str repo-dir)
                            :previous-ref (.getName old-commit)
                            :current-ref (.getName new-commit)
                            :drift-persons-dir (str drift-dir)
                            :work-dir (str work-dir)})
      updates (:drift-participant-updates result)]
  (is (= #{"000001" "abc-000000000001" "abc-000000000002"}
         (set (map #(get % "person_id") updates))))
  (is (= #{"removed" "added"}
         (set (map #(get % "change_type") updates))))
  (is (every? #(= [(get event "drift_event_id")]
                  (get % "drift_event_ids"))
              updates)))
```

- [ ] **Step 4: Run focused tests**

Run:

```bash
clojure -M:test -e "(require 'abc.tools.aozora-history-audit-test) (clojure.test/run-tests 'abc.tools.aozora-history-audit-test)"
```

Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add src/abc/tools/aozora_history_audit.clj test/abc/tools/aozora_history_audit_test.clj
git commit -m "feat: add drift participant audit CLI flags"
```

---

## Task 4: Document the Operational Command

**Files:**
- Modify: `docs/next-steps.md`

- [ ] **Step 1: Update the canonical command**

Change the `aozora-history-audit` command to:

```bash
nix run .#aozora-history-audit -- --aozora-repo /home/bor/Dependencies/aozorabunko --previous-ref <old-ref> --current-ref <new-ref> --drift-persons-dir examples/v0/example-persons --fail-on-candidates --fail-on-drift-participant-updates --output out/aozora-history-audit.json
```

- [ ] **Step 2: Add a short next-steps note**

Add one sentence near the 2026-04-30 Aozora upstream history audit entry:

```markdown
When accepted drift sidecars exist, pass `--drift-persons-dir
examples/v0/example-persons` so ordinary upstream edits to drift participants
surface as `drift_participant_updates[]` instead of being hidden inside a clean
non-split/non-merge audit.
```

- [ ] **Step 3: Commit**

```bash
git add docs/next-steps.md
git commit -m "docs: record drift-aware upstream audit command"
```

---

## Task 5: Full Verification

**Files:**
- Verify only.

- [ ] **Step 1: Run formatter/paren repair**

```bash
clj-paren-repair src/abc/tools/aozora_history_audit.clj src/abc/tools/person_drift_history.clj test/abc/tools/aozora_history_audit_test.clj
```

Expected: exits 0.

- [ ] **Step 2: Run focused tests**

```bash
clojure -M:test -e "(require 'abc.tools.aozora-history-audit-test) (clojure.test/run-tests 'abc.tools.aozora-history-audit-test)"
```

Expected: `0 failures, 0 errors`.

- [ ] **Step 3: Run the real audit with drift sidecars**

```bash
nix run .#aozora-history-audit -- \
  --aozora-repo /home/bor/Dependencies/aozorabunko \
  --previous-ref 36bf8ec832 \
  --current-ref 0e9ea3e586 \
  --drift-persons-dir examples/v0/example-persons \
  --output out/aozora-history-audit.json
```

Expected: `jq '.status, .drift_participant_updates' out/aozora-history-audit.json`
prints `"ok"` and `[]` for the current known-clean upstream pair.

- [ ] **Step 4: Run flake check**

```bash
nix flake check
```

Expected: all checks pass.

- [ ] **Step 5: Check for formatter changes**

Run:

```bash
git status --short
```

If formatter or docs changes occurred, commit them:

```bash
git add src/abc/tools/aozora_history_audit.clj src/abc/tools/person_drift_history.clj test/abc/tools/aozora_history_audit_test.clj docs/next-steps.md
git commit -m "chore: verify drift-aware upstream audit"
```

If no files changed, do not create an empty commit.

---

## Self-Review

- ADR 0022 keeps raw ingest source-faithful and makes drift awareness an audit
  guardrail, matching the operational scenario.
- The plan does not ask the ingester to infer split/merge semantics or rewrite
  contributor IDs.
- The report shape is deterministic and review-oriented.
- `--fail-on-drift-participant-updates` is explicit, so historical rebuilds and
  exploratory audits remain possible.
- The real-audit verification command uses the currently known local Aozora ref
  pair `36bf8ec832..0e9ea3e586`.
