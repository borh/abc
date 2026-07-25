# Publication Slug Collision Integrity Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Stop `soranoha build-publication` from silently destroying publications when two selected sources derive the same work slug, and make the snapshot index refuse to describe a corpus in which that happened.

**Architecture:** The work slug is publication identity. It is derived from `(work_id, primary person_id, zip basename)`, which is **not injective** over selected sources, so two distinct sources occupy one output directory — the second overwrites the first. The production guard is a candidate-level uniqueness assertion that runs *before* any slug-addressed filesystem write. A second, independent guard validates the snapshot index's source-selection object: `source_selection_identity_object.sources` is the only index location that *reliably* retains evidence for **both** classes, including byte-identical Class B collisions. (Class-A artifact references do retain evidence — as hash mismatches — but Class B leaves none there.) Changing the slug *formula* is deliberately **not** in Tasks 1–4: slugs are immutable published identity, so that is a governance decision (Task 5).

**Tech Stack:** Clojure, Kaocha, `abc.tools.soranoha-build-publication`, `abc.tools.snapshot-index`, Nix flake checks.

> **Status (2026-07-25):** Tasks 1–4 complete. Task 5's identity decision was
> made — **Axis 1a, unconditional formula**: the slug carries the card
> directory. The deciding evidence was that no work slug was published or
> committed anywhere (rights publication `:blocked-pending-assessment-migration`;
> zero committed manifests, indexes, or baselines embedding a corpus slug), so
> the immutability constraint below was vacuous for real slugs and the rotation
> was free. Axis 2 was therefore moot: there were no historical identities to
> preserve. Recorded as decision `publication-slug-source-injectivity`, which
> also records that this window is now closed — after a real publication ships,
> the governed-registry alternative becomes the cheaper option permanently.
> The Evidence and Task 1–4 sections below are kept in their original tense as
> the diagnostic record.

## Global Constraints

- Published manifests are immutable. Do **not** rewrite historical slugs, manifests, or snapshot indexes in Tasks 1–4.
- Behavior-preserving and behavior-changing edits are **separate commits**. Task 1 preserves behavior; Tasks 2–3 change it.
- Fail closed. A detected collision must never be resolved by silently picking a winner.
- **Admission rule (decided, 2026-07-25):** a catalog-backed candidate claims identity *before* archive inspection. An unreadable archive does not erase that claim, so `continue_on_failure` must **not** resolve a slug collision. This keeps identity independent of transient archive health and prevents publication membership from changing when corruption is repaired. Task 4 pins this with a test; Task 5 records it in governance.
- Problem/exception payloads use the existing shapes in these namespaces (`problem` helper in `snapshot_index.clj`; `ex-info` with `:code` elsewhere).
- Do not depend on machine-local paths (`/db/...`) in tracked code or tests.
- `just validate-migration` is the final gate.

---

## Evidence: the diagnosed defect

Established empirically on 2026-07-25 against the full corpus (17,602 selected sources, aozorabunko `0e9ea3e586eb0aa34039fabfc85a407d2f98b165`), reproduced across two independent runs with **different** corpus snapshot hashes.

**Root cause —** `abc/src/abc/tools/soranoha_build_publication.clj:102`:

```clojure
(defn- slug [work-id person-id relpath]
  (let [basename (.getName (io/file relpath))          ; <- discards the directory
        stem (subs basename 0 (- (count basename) (count ".zip")))]
    (str work-id "_" person-id "_" stem)))
```

Aozora files the same `work_id` under **multiple contributor card directories** with byte-differing copies. `person_id` comes from primary-person metadata, not the directory (`062694_002402_…` is selected from both `cards/001085/` and `cards/002385/`, neither of which is `002402`). Catalog matching keys on the ZIP **basename** alone, so one catalog row matches every copy. The directory is the only distinguishing element, and it is discarded.

**Consequence 1 — silent publication loss.** 17,602 selected sources produce 17,595 output directories. **7 publications are lost.** The second materialization of a slug overwrites the first's artifacts.

**Consequence 2 — orphaned index references, for four of the seven only.** `artifact-reference` (`:811`) is captured immediately after each manifest write, so both writes append references. But `build-snapshot-index` (`snapshot_index.clj:171`) passes them through `sort-artifact-references` (`:120-124`), which applies **`distinct`**. Byte-identical duplicates therefore collapse before the index is stored. The arithmetic confirms it: 17,595 × 4 = 70,380 expected; the index holds **70,396**; the difference is **16 = 4 Class-A collisions × 4 extra references**.

| Class | Count | `work_content_hash` | Observable symptom |
|---|---|---|---|
| **A** | 4 | **differs** | 36 closure problems (`closure-manifest-content-hash-mismatch` ×16, `closure-reference-field-mismatch` ×20) |
| **B** | 3 | **identical** | **none — silently lost, no diagnostic** |

Precisely: **the existing artifact-reference closure checks cannot see Class B** — narrower than "the verifier cannot see it." `canonical-source-selection` (`:59-64`) only *sorts* `sources`; it does not `distinct` them, so `source_selection_identity_object.sources` retains both slug claims with distinct `text_zip_relpath` values. That is the correct index-level detection site (Task 3).

**Not the initiating cause, but not proven safe:** the failure reproduces bit-identically across two runs, so a race does not *create* the collision. That does not establish the concurrent writes are race-free — colliding writes provably occur inside `ordered-pmap` (Task 2), and `metadata-record.json` is read back after being written.

**Content-identity dedup is ruled out as a fix:** the four Class-A pairs have **different `work_content_hash` / bundle identities**, so collapsing colliding claimants into one publication would discard distinct logical works. (Differing raw ZIP bytes alone would be weak evidence — archives can differ through packaging, timestamps, or compression while carrying identical content. The derived content identities are the load-bearing fact.)

## File Structure

- `abc/src/abc/tools/soranoha_build_publication.clj` — add `candidate-slug-collisions` + `assert-candidate-slugs-unique!` (pure projection + guard) below `slug` (`:105`), and call the guard before `ordered-pmap` (`:565`). No post-selection assertion and no new report field: see the note in Task 2.
- `abc/test/abc/tools/soranoha_build_publication_test.clj` — characterization, the seven-collision table, and the two integration tests. Existing file; append. Requires a local ZIP fixture helper (Task 4 Step 1).
- `abc/src/abc/tools/snapshot_index.clj` — add `source-slug-collisions` below `canonical-source-selection` (`:64`) and enforce it in `validate-snapshot-index!` (`:229`).
- `abc/test/abc/tools/snapshot_index_test.clj` — projection test plus an enforcement test through `build-snapshot-index`. Existing file; append.
- `abc/docs/adr/decisions.edn` — Task 5 only, **by hand**. Tooling never edits decisions.

---

### Task 1: Characterize the collision (behavior-preserving, TRANSITIONAL)

Pins today's behavior so Tasks 2–3 are reviewable. This test asserts the **bug** deliberately.

**This test is transitional and Task 5 Step 1 retires it.** Every viable Task 5 identity policy makes these two coordinates produce *distinct* slugs, so leaving this assertion in place would make Task 5 fail the "full suite, zero failures" gate. It is correct through Tasks 1–4 and must be replaced — not deleted silently — in the identity-policy commit.

**Files:**
- Modify: `abc/test/abc/tools/soranoha_build_publication_test.clj`

**Interfaces:**
- Consumes: the existing private `slug` via `#'`.
- Produces: nothing consumed by later tasks.

- [x] **Step 1: Write the test**

```clojure
(deftest slug-is-not-injective-over-source-directories
  (testing "two sources differing only by card directory collapse to one slug"
    (let [slug-fn #'abc.tools.soranoha-build-publication/slug
          a (slug-fn "047896" "000075" "cards/000075/files/47896_ruby_49619.zip")
          b (slug-fn "047896" "000075" "cards/001030/files/47896_ruby_49619.zip")]
      ;; CHARACTERIZATION of CURRENT (defective) behavior. Retired by Task 5
      ;; Step 1, which replaces it with the governed identity invariant.
      (is (= a b))
      (is (= "047896_000075_47896_ruby_49619" a)))))
```

- [x] **Step 2: Run it and confirm it passes**

Run: `cd abc && clojure -M:test:kaocha -m kaocha.runner --focus abc.tools.soranoha-build-publication-test/slug-is-not-injective-over-source-directories`

Expected: PASS. If it FAILS, the slug formula already changed — stop and re-read `:102`.

- [x] **Step 3: Commit**

```bash
git add abc/test/abc/tools/soranoha_build_publication_test.clj
git commit -m "test(publication): characterize non-injective work slug (transitional)"
```

---

### Task 2: Fail closed on candidate slug collisions, before any write

**Files:**
- Modify: `abc/src/abc/tools/soranoha_build_publication.clj`
- Modify: `abc/test/abc/tools/soranoha_build_publication_test.clj`

**Interfaces:**
- Produces: `(candidate-slug-collisions candidates)` → vector of `{"slug" String "sources" [{"text_zip_relpath" String}]}`, sorted by slug, empty when injective. `(assert-candidate-slugs-unique! candidates)` → `candidates`, or throws `ex-info` with `{:code "publication-slug-collision" :collisions [...]}`. Task 4 consumes both.
- Candidate shape (already produced upstream at `:97-99`): `{:file File :relpath String :row map}`, where `row-work-id` reads `"作品ID"` and `row-person-id` reads `"人物ID"`.

**Why not a post-selection assertion.** An earlier draft also added `slug-collisions`/`assert-slugs-unique!` over the *selected* rows plus a `slug_collisions` report field. Both are omitted deliberately:

- Nothing in production would call the post-selection assertion once the candidate guard exists, so it would be dead code exercised only by its own tests.
- The report field is a tautology. The report is written into the temporary root (`:1117-1118`) and promoted only on success (`:766`, `:768`); the guard throws during selection, long before. So a persisted `slug_collisions` value is observable *only* on collision-free builds, where it is always `[]`. The authoritative diagnostic is the `ex-info` payload, which Task 4 asserts on.

If an operator-facing persisted collision report is ever wanted, it needs a deliberate failure-report contract — not a field that only appears when there is nothing to report.

- [x] **Step 1: Write the failing test**

```clojure
(defn- candidate-stub [work-id person-id relpath]
  {:row {"作品ID" work-id "人物ID" person-id} :relpath relpath :file nil})

(deftest candidate-slug-collisions-detects-duplicate-claims
  (let [collisions #'abc.tools.soranoha-build-publication/candidate-slug-collisions]
    (testing "injective candidates yield no collisions"
      (is (= [] (collisions [(candidate-stub "1" "9" "cards/000009/files/a.zip")
                             (candidate-stub "2" "9" "cards/000009/files/b.zip")]))))
    (testing "same work/person and same basename in two card dirs collide"
      (is (= [{"slug" "047896_000075_47896_ruby_49619"
               "sources" [{"text_zip_relpath" "cards/000075/files/47896_ruby_49619.zip"}
                          {"text_zip_relpath" "cards/001030/files/47896_ruby_49619.zip"}]}]
             (collisions
              [(candidate-stub "047896" "000075" "cards/001030/files/47896_ruby_49619.zip")
               (candidate-stub "047896" "000075" "cards/000075/files/47896_ruby_49619.zip")]))))
    (testing "same basename under a different work id does NOT collide"
      (is (= [] (collisions
                 [(candidate-stub "047896" "000075" "cards/000075/files/x.zip")
                  (candidate-stub "047897" "000075" "cards/001030/files/x.zip")]))))))

(deftest assert-candidate-slugs-unique-fails-closed
  (let [assert-fn #'abc.tools.soranoha-build-publication/assert-candidate-slugs-unique!
        colliding [(candidate-stub "047896" "000075" "cards/000075/files/x.zip")
                   (candidate-stub "047896" "000075" "cards/001030/files/x.zip")]
        thrown (try (assert-fn colliding) nil
                    (catch clojure.lang.ExceptionInfo e e))]
    (is (some? thrown))
    (is (= "publication-slug-collision" (:code (ex-data thrown))))
    (is (= 1 (count (:collisions (ex-data thrown)))))
    (testing "an injective candidate set passes through unchanged"
      (let [ok [(candidate-stub "1" "9" "cards/000009/files/a.zip")]]
        (is (= ok (assert-fn ok)))))))
```

- [x] **Step 2: Run to verify it fails**

Run: `cd abc && clojure -M:test:kaocha -m kaocha.runner --focus abc.tools.soranoha-build-publication-test/candidate-slug-collisions-detects-duplicate-claims`

Expected: FAIL — `Unable to resolve var: … /candidate-slug-collisions`.

- [x] **Step 3: Implement both functions**

Insert immediately **below** `slug` (`abc/src/abc/tools/soranoha_build_publication.clj:105`), so they precede all callers:

```clojure
(defn- candidate-slug-collisions
  "PURE. Candidate slug claims grouped by slug, keeping only slugs claimed more
  than once. Runs BEFORE archive inspection: the slug is a function of
  (work_id, person_id, relpath) alone, so a collision is knowable without
  opening a ZIP — and must be known before any `works/<slug>` write."
  [candidates]
  (->> candidates
       (map (fn [{:keys [row relpath]}]
              {"slug" (slug (row-work-id row) (row-person-id row) relpath)
               "text_zip_relpath" relpath}))
       (group-by #(get % "slug"))
       (filter (fn [[_ claims]] (< 1 (count claims))))
       (sort-by key)
       (mapv (fn [[work-slug claims]]
               {"slug" work-slug
                ;; Project the diagnostic shape explicitly. Returning the raw
                ;; grouped maps would repeat the group's slug inside every
                ;; claimant and break the asserted shape in Task 4.
                "sources" (mapv (fn [claim]
                                  {"text_zip_relpath" (get claim "text_zip_relpath")})
                                (sort-by #(get % "text_zip_relpath") claims))}))))

(defn- assert-candidate-slugs-unique!
  "Return `candidates` when every candidate claims a distinct slug; otherwise
  throw before any slug-addressed filesystem write occurs. A candidate claims
  its identity from the catalog, so an unreadable archive does not withdraw the
  claim: `continue_on_failure` must not resolve a collision."
  [candidates]
  (let [collisions (candidate-slug-collisions candidates)]
    (when (seq collisions)
      (throw (ex-info "selected sources claim duplicate publication slugs"
                      {:code "publication-slug-collision"
                       :collisions collisions})))
    candidates))
```

Note `row-work-id`/`row-person-id` are defined at `:79-83`, above this insertion point.

- [x] **Step 4: Run both tests to verify they pass**

Run: `cd abc && clojure -M:test:kaocha -m kaocha.runner --focus abc.tools.soranoha-build-publication-test/candidate-slug-collisions-detects-duplicate-claims --focus abc.tools.soranoha-build-publication-test/assert-candidate-slugs-unique-fails-closed`

Expected: PASS.

- [x] **Step 5: Wire the guard in BEFORE the parallel writes**

`inspect-selected-work!` derives `work-slug` at `:409` and then writes into `materialized-root/works/<slug>/` at `:414-415` (`create-dirs!`, `write-manifest!`), plus `official-source.json` and `metadata-record.json`. Those run inside `select-candidate` inside `parallel/ordered-pmap` (`:565-568`); `selected` binds at `:569`, i.e. after every colliding write.

Insert immediately **above** the `results (parallel/ordered-pmap …)` binding at `:565`:

```clojure
        _ (assert-candidate-slugs-unique! selected-candidates)
```

That is the only edit in this step. Do not restructure the function.

- [x] **Step 6: Run the full namespace**

Run: `cd abc && clojure -M:test:kaocha -m kaocha.runner --focus abc.tools.soranoha-build-publication-test`

Expected: 0 failures. If an existing test now throws `publication-slug-collision`, its fixture has colliding candidates — fix the **fixture**, not the guard, and say so in the commit message.

- [x] **Step 7: Commit**

```bash
git add abc/src/abc/tools/soranoha_build_publication.clj abc/test/abc/tools/soranoha_build_publication_test.clj
git commit -m "fix(publication): reject duplicate work slugs before any write

Two candidates differing only by card directory collapsed to one slug, so
the second materialization overwrote the first. 7 of 17,602 full-corpus
sources were lost this way; only the 4 with differing content were
detectable downstream. The guard runs before ordered-pmap, because
inspect-selected-work! writes works/<slug> inside it.

Admission rule: a catalog-backed candidate claims identity before archive
inspection, so continue_on_failure does not resolve a collision."
```

---

### Task 3: Enforce source-slug injectivity in the snapshot index

**Do not add a duplicate-*artifact-reference* detector.** `sort-artifact-references` applies `distinct` before the index is stored, so `closure-problems` can never receive a byte-identical duplicate; such a detector would pass its unit test while protecting nothing, and would only re-detect Class A.

Validate the object that retains the evidence: `source_selection_identity_object.sources`.

Scope honestly: once Task 2 lands, a colliding selection never reaches index construction. This task **rejects indexes from other paths when they cross the validation/read boundary** — `validate-snapshot-index!` (`:229`) or `read-valid-snapshot-index` (`:451`, which calls it at `:461`). It does *not* reject at construction time: neither `build-snapshot-index` (`:165`) nor `write-snapshot-index!` (`:200-205`) invokes validation, so a colliding index can still be *built and written* by a path that skips validation. Extending rejection to construction is out of scope here; the boundary above is where every verification path already passes.

**Files:**
- Modify: `abc/src/abc/tools/snapshot_index.clj`
- Modify: `abc/test/abc/tools/snapshot_index_test.clj`

**Interfaces:**
- Produces: `(source-slug-collisions source-selection)` → vector of `{"slug" String "sources" [{"text_zip_relpath" String}]}`, sorted by slug, empty when injective. `validate-snapshot-index!` throws `ex-info` with `{:code "snapshot-source-slug-collision" :collisions [...]}`.

- [x] **Step 1: Write the failing projection test**

```clojure
(deftest source-slug-collisions-detects-duplicate-claims
  (let [collisions #'abc.tools.snapshot-index/source-slug-collisions]
    (testing "injective sources yield no collisions"
      (is (= [] (collisions {"sources" [{"slug" "a" "text_zip_relpath" "cards/1/files/a.zip"}
                                        {"slug" "b" "text_zip_relpath" "cards/2/files/b.zip"}]}))))
    (testing "Class B (identical content) duplicate slug claims are detected"
      (is (= [{"slug" "062694_002402_62694_ruby_78206"
               "sources" [{"text_zip_relpath" "cards/001085/files/62694_ruby_78206.zip"}
                          {"text_zip_relpath" "cards/002385/files/62694_ruby_78206.zip"}]}]
             (collisions
              {"sources" [{"slug" "062694_002402_62694_ruby_78206"
                           "text_zip_relpath" "cards/002385/files/62694_ruby_78206.zip"}
                          {"slug" "062694_002402_62694_ruby_78206"
                           "text_zip_relpath" "cards/001085/files/62694_ruby_78206.zip"}]}))))
    (testing "absent sources is not an error here"
      (is (= [] (collisions {}))))))
```

- [x] **Step 2: Run to verify it fails**

Run: `cd abc && clojure -M:test:kaocha -m kaocha.runner --focus abc.tools.snapshot-index-test/source-slug-collisions-detects-duplicate-claims`

Expected: FAIL — unresolved var.

- [x] **Step 3: Implement the projection**

Add below `canonical-source-selection` (`abc/src/abc/tools/snapshot_index.clj:64`):

```clojure
(defn- source-slug-collisions
  "PURE. Duplicate slug claims within the index's source-selection object. The
  slug names the single output directory `publications/<slug>/`, so two claimants
  mean one publication was overwritten. This object is the only index location
  that RELIABLY retains that evidence for both collision classes: artifact
  references pass through `distinct` in `sort-artifact-references`, so they
  retain evidence only when the colliding works differ in content."
  [source-selection]
  (->> (get source-selection "sources" [])
       (group-by #(get % "slug"))
       (filter (fn [[_ claims]] (< 1 (count claims))))
       (sort-by key)
       (mapv (fn [[work-slug claims]]
               {"slug" work-slug
                "sources" (mapv (fn [claim]
                                  {"text_zip_relpath" (get claim "text_zip_relpath")})
                                (sort-by #(get % "text_zip_relpath") claims))}))))
```

- [x] **Step 4: Run to verify it passes**

Run: `cd abc && clojure -M:test:kaocha -m kaocha.runner --focus abc.tools.snapshot-index-test/source-slug-collisions-detects-duplicate-claims`

Expected: PASS.

- [x] **Step 5: Write the ENFORCEMENT test (the projection test does not prove wiring)**

A correct projection that is never called would leave every other test green. Build a *self-consistent, schema-valid* index through `build-snapshot-index` so all identity hashes recompute, then assert `validate-snapshot-index!` rejects it.

Use the fixtures the namespace already has — do **not** invent field values and do **not** pass a bare `{"sources" [...]}` as the source selection. That map lacks `trust_mode`, `aozora_git_commit`, `catalog_csv_hash`, and `snapshot_date`, and its rows lack their required identity fields (`work_id`, `person_id`, `archive_hash`, `bundle_hash`, `primary_text_member`, `primary_text_hash`, `metadata_record_hash`). `validate-snapshot-index!` runs no JSON Schema validation, so such a fixture would *appear* self-consistent while proving far less than claimed. Derive the colliding rows from the existing complete row instead, and assert schema validity explicitly.

The reusable builder is `build-args` (`abc/test/abc/tools/snapshot_index_test.clj:69`), which merges keyword overrides over a complete argument map; `source-selection` (`:27-37`) is the complete selection whose single row `(first (get source-selection "sources"))` carries every required field.

```clojure
(deftest validate-snapshot-index-rejects-duplicate-source-slugs
  (testing "a schema-valid, self-consistent index with duplicate slugs is rejected"
    (let [collision-slug "062694_002402_62694_ruby_78206"
          source-row (first (get source-selection "sources"))
          colliding-selection
          (assoc source-selection
                 "sources"
                 [(assoc source-row
                         "slug" collision-slug
                         "text_zip_relpath"
                         "cards/001085/files/62694_ruby_78206.zip")
                  (assoc source-row
                         "slug" collision-slug
                         "text_zip_relpath"
                         "cards/002385/files/62694_ruby_78206.zip")])
          index (snapshot-index/build-snapshot-index
                 (build-args :source-selection colliding-selection))
          schema-json (files/read-json "schemas/snapshot-index.schema.json")
          thrown (try
                   (snapshot-index/validate-snapshot-index! index)
                   nil
                   (catch clojure.lang.ExceptionInfo e e))]
      (is (nil? (schema/validation-errors schema-json index))
          "fixture must be schema-valid, or the test proves nothing")
      (is (some? thrown) "expected validate-snapshot-index! to reject the index")
      (is (= "snapshot-source-slug-collision" (:code (ex-data thrown))))
      (testing "both claimants are named"
        (let [relpaths (->> (:collisions (ex-data thrown))
                            first
                            (#(get % "sources"))
                            (map #(get % "text_zip_relpath"))
                            set)]
          (is (= #{"cards/001085/files/62694_ruby_78206.zip"
                   "cards/002385/files/62694_ruby_78206.zip"}
                 relpaths)))))))
```

`files`, `schema`, and `snapshot-index` are already required by this namespace (`:1-9`); no new dependency is needed. Before Step 7 only the expected-exception assertions fail — the schema-validity assertion passes immediately, which is what proves the fixture isolates the new invariant rather than smuggling in an unrelated defect.

- [x] **Step 6: Run to verify it fails**

Run: `cd abc && clojure -M:test:kaocha -m kaocha.runner --focus abc.tools.snapshot-index-test/validate-snapshot-index-rejects-duplicate-source-slugs`

Expected: FAIL on the `(is (some? thrown))` and `:code` assertions only — nothing calls the projection yet. The `schema/validation-errors` assertion must **already pass**; if it fails, the fixture is malformed and the test would prove nothing, so fix the fixture before implementing enforcement.

- [x] **Step 7: Enforce it in `validate-snapshot-index!`**

`validate-snapshot-index!` (`:229`) already reads `source_selection_identity_object` at `:240`. Add the check immediately after the existing `source_selection_hash` comparison (`:238-241`), inside the same `let`, so a tampered index still fails on its hash first:

```clojure
    (let [source-collisions (source-slug-collisions
                             (get snapshot-index "source_selection_identity_object"))]
      (when (seq source-collisions)
        (throw (ex-info "snapshot index source selection claims duplicate slugs"
                        {:code "snapshot-source-slug-collision"
                         :collisions source-collisions}))))
```

- [x] **Step 8: Run the full namespace**

Run: `cd abc && clojure -M:test:kaocha -m kaocha.runner --focus abc.tools.snapshot-index-test`

Expected: 0 failures, including the enforcement test.

- [x] **Step 9: Commit**

```bash
git add abc/src/abc/tools/snapshot_index.clj abc/test/abc/tools/snapshot_index_test.clj
git commit -m "fix(snapshot-index): reject duplicate source slug claims

Validates source_selection_identity_object.sources, not artifact
references: sort-artifact-references applies distinct, so identical-content
duplicates never reach closure verification."
```

---

### Task 4: Prove placement and cover all seven real collisions

**Files:**
- Modify: `abc/test/abc/tools/soranoha_build_publication_test.clj`

**Interfaces:**
- Consumes: `assert-candidate-slugs-unique!` and `candidate-slug-collisions` (Task 2); `materialize-selected-sources!` (`:532`).

The namespace has **no** ZIP or catalog fixture builders today (it requires only `build-publication`, `babashka.fs`, `babashka.process`, `charred.api`, `clojure.test`). Step 1 adds a local helper. Use `java.util.zip` rather than shelling out to `zip`, so the test needs no external binary.

- [x] **Step 1: Add the fixture helper**

Add to the `:require` of the namespace `[clojure.java.io :as io]` and `[clojure.string :as string]`, and add `(:import [java.util.zip ZipEntry ZipOutputStream])`. The namespace currently requires only `build-publication`, `babashka.fs`, `babashka.process`, `charred.api`, and `clojure.test` (`:11-15`), so **both** new requires are mandatory: Task 4 uses `string/replace` and `string/includes?`, and relying on `build-publication` to load `clojure.string` transitively is a latent failure.

```clojure
(defn- write-zip!
  "Write a ZIP at `target` containing `entries` ({name -> content-string}).
  Deterministic entry order; no external `zip` binary required."
  [target entries]
  (fs/create-dirs (fs/parent target))
  (with-open [out (ZipOutputStream. (io/output-stream (fs/file target)))]
    (doseq [[entry-name content] (sort-by key entries)]
      (.putNextEntry out (ZipEntry. ^String entry-name))
      (.write out (.getBytes ^String content "UTF-8"))
      (.closeEntry out))))

(defn- two-card-colliding-aozora-root!
  "Build a minimal aozora root in which ONE catalog row matches the SAME zip
  basename in TWO card directories — the real collision shape. Catalog matching
  keys solely on the basename, so both copies match the single row and derive
  one slug. `second-card-content` differing from the first makes this Class A;
  passing identical content makes it Class B.
  Returns the aozora root path."
  [root basename second-card-content]
  (let [csv-source (fs/path "examples/v0/example-work/aozora-csv"
                            "list_person_all_extended_utf8_127.csv")
        csv-text (-> (slurp (fs/file csv-source))
                     (string/replace "127_ruby_150.zip" basename))]
    (fs/create-dirs (fs/path root "index_pages"))
    (let [csv-staging (fs/path root "csv-staging")]
      (fs/create-dirs csv-staging)
      (spit (fs/file (fs/path csv-staging "list_person_all_extended_utf8.csv")) csv-text)
      (write-zip! (fs/path root "index_pages" "list_person_all_extended_utf8.zip")
                  {"list_person_all_extended_utf8.csv" csv-text})
      (fs/delete-tree csv-staging))
    ;; Person 000879 is the catalog fixture's person; the SECOND card directory
    ;; is deliberately a different person dir, which the slug discards.
    (write-zip! (fs/path root "cards" "000879" "files" basename)
                {"000001.txt" "吾輩《わがはい》は猫である。\n"})
    (write-zip! (fs/path root "cards" "000880" "files" basename)
                {"000001.txt" second-card-content})
    root))
```

Confirm the CSV fixture path resolves from the test working directory (`abc/`) before proceeding: `ls abc/examples/v0/example-work/aozora-csv/list_person_all_extended_utf8_127.csv`. The smoke script uses this same fixture and the same `127_ruby_150.zip` → basename repoint.

- [x] **Step 2: Write the placement test**

```clojure
(deftest colliding-selection-is-rejected-before-any-slug-write
  (testing "two cards sharing a basename fail closed with no works/<slug> written"
    (fs/with-temp-dir [tmp {:prefix "slug-collision-"}]
      (let [aozora-root (two-card-colliding-aozora-root!
                         (fs/path tmp "aozora")
                         "000001_ruby_fixture.zip"
                         "こちらは別の本文である。\n")
            output-root (fs/path tmp "out")
            materialize #'abc.tools.soranoha-build-publication/materialize-selected-sources!
            thrown (try
                     (materialize {:aozora-root (str aozora-root)
                                   :output-root (str output-root)
                                   :snapshot-date "2026-07-25"
                                   :source-trust-mode "fixture"
                                   :aozora-git-commit nil
                                   :continue-on-failure true
                                   :concurrency 2})
                     nil
                     (catch clojure.lang.ExceptionInfo e e))]
        (is (some? thrown) "expected ExceptionInfo for colliding candidates")
        (is (= "publication-slug-collision" (:code (ex-data thrown))))
        (testing "both claimants are named in the failure"
          (let [relpaths (->> (:collisions (ex-data thrown))
                              first
                              (#(get % "sources"))
                              (map #(get % "text_zip_relpath"))
                              set)]
            (is (= 2 (count relpaths)))
            (is (some #(string/includes? % "cards/000879/") relpaths))
            (is (some #(string/includes? % "cards/000880/") relpaths))))
        (testing "NO slug-addressed directory was written"
          ;; This is the regression guard for the placement bug: moving the
          ;; assertion back to the `selected` binding makes this fail.
          (is (not (fs/exists? (fs/path output-root "materialized-root" "works")))))))))
```

- [x] **Step 3: Write the admission-rule test**

Pins the decided rule: an unreadable archive must not resolve a collision.

```clojure
(deftest unreadable-archive-does-not-resolve-a-collision
  (testing "continue_on_failure must not let a corrupt claimant excuse a collision"
    (fs/with-temp-dir [tmp {:prefix "slug-collision-corrupt-"}]
      (let [aozora-root (two-card-colliding-aozora-root!
                         (fs/path tmp "aozora")
                         "000001_ruby_fixture.zip"
                         "irrelevant — overwritten below\n")]
        ;; Corrupt the SECOND claimant so ZIP inspection would fail on it.
        (spit (fs/file (fs/path aozora-root "cards" "000880" "files"
                               "000001_ruby_fixture.zip"))
              "not a zip at all")
        (let [output-root (fs/path tmp "out")
              materialize #'abc.tools.soranoha-build-publication/materialize-selected-sources!
              thrown (try
                       (materialize {:aozora-root (str aozora-root)
                                     :output-root (str output-root)
                                     :snapshot-date "2026-07-25"
                                     :source-trust-mode "fixture"
                                     :aozora-git-commit nil
                                     :continue-on-failure true
                                     :concurrency 2})
                       nil
                       (catch clojure.lang.ExceptionInfo e e))]
          (is (some? thrown))
          (is (= "publication-slug-collision" (:code (ex-data thrown)))
              "must fail on identity, not on the corrupt archive")
          (is (not (fs/exists? (fs/path output-root "materialized-root" "works")))
              "collision must be detected before ZIP inspection"))))))
```

- [x] **Step 4: Write the seven-collision table test**

Full-length hashes are retained as provenance for the observed corpus state even though the candidate guard does not read them; the class split is asserted so the table cannot silently drift.

```clojure
(def ^:private known-2026-07-25-collisions
  "All seven duplicate slug claims from the 2026-07-25 full-corpus run on
  aozorabunko 0e9ea3e586eb0aa34039fabfc85a407d2f98b165. Class A = differing
  work_content_hash (visible to closure verification); Class B = identical
  (invisible to it, because sort-artifact-references applies distinct)."
  [{:class :b :work-id "045183" :person-id "000107" :basename "45183_ruby_23453.zip"
    :cards ["000019" "000107"]
    :hashes ["sha256:81b1b92c912d78fdb85dd46cd04a65cb8dbbad5b01e86b5cc63653f5e621c419"
             "sha256:81b1b92c912d78fdb85dd46cd04a65cb8dbbad5b01e86b5cc63653f5e621c419"]}
   {:class :a :work-id "047896" :person-id "000075" :basename "47896_ruby_49619.zip"
    :cards ["000075" "001030"]
    :hashes ["sha256:91ec677857fe17aa46afae0c0a95886d2d33a41f28b0a4c2359abdf6bdc161d7"
             "sha256:87cba36cfd6793da678e870c1a3c93ea888ac6a90185f1dac00be8887230ebd4"]}
   {:class :a :work-id "047957" :person-id "001030" :basename "47957_ruby_40644.zip"
    :cards ["001030" "001769"]
    :hashes ["sha256:478d4cbebbe7ac2069878d8773feec93a858a50408881ce0e33564fa44ff0955"
             "sha256:7ed77464f724f62cd76c7616530a64483ed350f1f02d3a34455b77a2d8b8f109"]}
   {:class :a :work-id "047959" :person-id "000075" :basename "47959_ruby_40639.zip"
    :cards ["000075" "001030"]
    :hashes ["sha256:0f331c885b929ed915e130052d0bbe169bca33d3bb24d2367be48b484adb1d90"
             "sha256:998a07914ac4f617772c82edb8c9fab0bc0da3297b768f05d9d6291752c16c84"]}
   {:class :a :work-id "047971" :person-id "000075" :basename "47971_txt_40650.zip"
    :cards ["000075" "001030"]
    :hashes ["sha256:9ba20d7e099f6256d5c5534224fbb3fc7407ee472283431d3b1f1f37139a8b1d"
             "sha256:eef28bf7e798e78802612f129cdb44107b0a7419960e3b85aa7cf953d418b0d9"]}
   {:class :b :work-id "050558" :person-id "000975" :basename "50558_ruby_61314.zip"
    :cards ["000150" "000975"]
    :hashes ["sha256:c16514dfb963b0d8c347ab1925d579287e5d4e0a3f21d43e987ae728c743be9e"
             "sha256:c16514dfb963b0d8c347ab1925d579287e5d4e0a3f21d43e987ae728c743be9e"]}
   {:class :b :work-id "062694" :person-id "002402" :basename "62694_ruby_78206.zip"
    :cards ["001085" "002385"]
    :hashes ["sha256:f8ae61ea7efc561ef02c5df483389e605e1ea7a53734821687e9c14a92d964ba"
             "sha256:f8ae61ea7efc561ef02c5df483389e605e1ea7a53734821687e9c14a92d964ba"]}])

(deftest all-seven-known-collisions-are-rejected
  (let [assert-fn #'abc.tools.soranoha-build-publication/assert-candidate-slugs-unique!]
    (testing "the table matches the observed corpus state"
      (is (= 7 (count known-2026-07-25-collisions)))
      (is (= 4 (count (filter #(= :a (:class %)) known-2026-07-25-collisions))))
      (is (= 3 (count (filter #(= :b (:class %)) known-2026-07-25-collisions))))
      (doseq [{:keys [class hashes]} known-2026-07-25-collisions]
        (is (= (= :b class) (apply = hashes))
            "Class B iff both work_content_hashes are equal")))
    (doseq [{:keys [work-id person-id basename cards class]} known-2026-07-25-collisions]
      (testing (str work-id " (class " (name class) ")")
        (let [candidates (mapv (fn [card]
                                 (candidate-stub work-id person-id
                                                 (str "cards/" card "/files/" basename)))
                               cards)
              thrown (try (assert-fn candidates) nil
                          (catch clojure.lang.ExceptionInfo e e))]
          (is (some? thrown) (str "expected rejection for " work-id))
          (is (= "publication-slug-collision" (:code (ex-data thrown)))))))))
```

- [x] **Step 5: Run all of Task 4's tests**

```bash
cd abc && clojure -M:test:kaocha -m kaocha.runner \
  --focus abc.tools.soranoha-build-publication-test/colliding-selection-is-rejected-before-any-slug-write \
  --focus abc.tools.soranoha-build-publication-test/unreadable-archive-does-not-resolve-a-collision \
  --focus abc.tools.soranoha-build-publication-test/all-seven-known-collisions-are-rejected
```

Expected: all PASS. If the two integration tests fail on fixture construction rather than on the assertion, fix the fixture helper before touching production code — a fixture bug must not be "fixed" by weakening the guard.

- [x] **Step 6: Commit**

```bash
git add abc/test/abc/tools/soranoha_build_publication_test.clj
git commit -m "test(publication): prove guard placement, admission rule, and all seven collisions"
```

---

### Task 5: Governance decision on slug identity — DECISION REQUIRED BEFORE STARTING

**Do not begin without an explicit decision from the repository owner.** Tasks 1–4 make the defect loud and safe. Task 5 changes publication identity.

**Files:**
- Modify: `abc/docs/adr/decisions.edn` — **by hand**.
- Create: `abc/docs/adr/<slug>.md` narrative; regenerate `abc/docs/adr/INDEX.md` and `abc/docs/adr/adr-graph.mmd`.
- Modify: `abc/test/abc/tools/soranoha_build_publication_test.clj` (Step 1).

**Why conditional disambiguation is disqualified.** Appending a discriminator "only when a collision is detected" makes a source's identity a function of the *entire selected set*: a work's slug could change because an unrelated source enters or leaves the corpus. That contradicts this plan's immutability constraint. Its blast radius was also understated in an earlier draft — if both claimants take a suffix, **14 source slugs change and 7 historical identities disappear**, not "7 slugs change"; if only one takes it, the rule must name *which*, and choosing by traversal order would make identity operational rather than governed.

**The decision has two independent axes. Choose one value on each.**

*Axis 1 — mechanism (how a slug is derived):*

- **1a. Unconditional formula.** Include the card directory in every slug. Injective by construction, no per-collision governance, stable under corpus change.
- **1b. Governed registry.** A committed list of colliding coordinates with their assigned slugs. Identity is a lookup; stable regardless of what else is selected. Cost: a governance entry whenever upstream adds a collision.

*Axis 2 — migration policy (what happens to existing identities):*

- **2a. Preserve one historical slug per collision.** One coordinate keeps the existing slug *by governed decision* (never by traversal order); other claimants get new slugs. Preserves 17,595 identities; 7 new identities appear.
- **2b. Rotate both claimants.** Neither keeps the historical slug. Cleanest semantics; 7 historical identities are retired and 14 appear.

Combining them makes blast radius explicit: **1a+2b** rotates all 17,602 slugs (full identity rotation against published manifests). **1a+2a** is incoherent unless the preserved slugs are themselves registry entries, which collapses into 1b. **1b+2a** touches 7 identities and adds 7 governed entries. **1b+2b** touches 7 and adds 14.

No recommendation is offered: this sets publication identity policy and trades migration cost against governance overhead. Whichever is chosen, the rule must make a slug derivable from one source's coordinates plus committed governance — **never from the rest of the corpus**.

- [x] **Step 1: Retire the transitional characterization test**

Replace `slug-is-not-injective-over-source-directories` (Task 1) with the chosen invariant. It asserts the defect and *must* fail once identity is fixed:

```clojure
(deftest slug-is-injective-over-source-directories
  (testing "two sources differing only by card directory derive DISTINCT slugs"
    (let [slug-fn #'abc.tools.soranoha-build-publication/slug
          a (slug-fn "047896" "000075" "cards/000075/files/47896_ruby_49619.zip")
          b (slug-fn "047896" "000075" "cards/001030/files/47896_ruby_49619.zip")]
      (is (not= a b)))))
```

Under mechanism **1b** the slug function alone may remain non-injective; in that case assert injectivity at the layer that resolves the registry instead, and delete the Task 1 test rather than inverting it. Either way, do it in this commit — not silently.

- [x] **Step 2: Record the decision** in `decisions.edn`: the chosen axis values, the blast radius from the table above, the Evidence section's class table, and the admission rule from Global Constraints. Run `nix build ./abc#checks.x86_64-linux.adr-governance --no-link`; require exit 0.

  **Also record source-slug injectivity as a durable semantic invariant.** Task 3 tightened `validate-snapshot-index!` so it rejects indexes that are schema-valid under `snapshot-index` 0.2.0 and that an older validator accepted — without a schema version bump. That is defensible as integrity hardening (the rejected indexes describe a corpus in which a publication was overwritten), but until it is written down it exists only in code and tests. Either state the invariant in this decision, or bump the snapshot-index contract; do not leave it implicit. Note the asymmetry that makes it safe in practice: no *correct* index could ever have contained duplicate source slug claims, so nothing legitimate is newly rejected.
- [x] **Step 3: Implement the chosen mechanism**, test-first, asserting injectivity over all seven `known-2026-07-25-collisions` entries.
- [x] **Step 4: Re-target Task 4's tests.** All three change, and two of them change in ways that are easy to get wrong:

  - `colliding-selection-is-rejected-before-any-slug-write` → becomes a *successful* selection. Assert **two distinct** `works/<slug>` directories, and that both slugs appear in the report.
  - `unreadable-archive-does-not-resolve-a-collision` → **must not** become a two-directory success. Its second archive is still corrupt, so after disambiguation the expected behavior is: no slug collision; the valid source materializes; the corrupt source becomes a **derive failure** under `continue_on_failure=true`; **exactly one** `works/<slug>` directory exists. Rewrite it as a post-governance admission test asserting one selected source and one recorded derive failure, and rename it accordingly (e.g. `unreadable-archive-fails-only-its-own-source`). Asserting two directories here would be false.
  - `all-seven-known-collisions-are-rejected` → cannot survive under *any* final policy, because Step 5 also requires 17,602 successful publications; a mechanism that still rejects these seven cannot produce that count. Rename to `all-seven-known-collisions-resolve-injectively` and assert that each entry's two coordinates derive **distinct** slugs (and, across the whole table, that all 14 slugs are distinct). Keep the table's hashes and class-count assertions as provenance.

  "Keep it if the mechanism still rejects" was wrong: rejection and the full-corpus success criterion are mutually exclusive.
- [ ] **Step 5: Re-run the full-corpus build** and require: 17,602 output directories, zero `closure-*` problems, `release-rights-blocked` as the sole release problem.
- [ ] **Step 6: Commit**, and update `abc/docs/superpowers/reports/2026-07-24-publication-surface-disposition.md` with the closeout.

---

## Final gate

- [x] Full suite, 0 failures (baseline: 1,126 tests / 11,601 assertions; observed 1,134 / 11,650 after this plan — exactly the +8 tests / +49 assertions added here).

  The bare `clojure -M:test:kaocha -m kaocha.runner` **fails outside the dev shell**: `abc.tools.tei-header-test` throws `TEI_SCHEMA_PATH must be set`. That is a precondition, not a regression. Run it hermetically:

  ```bash
  cd abc && nix develop --command bash -c \
    'clojure -M:test:kaocha -m kaocha.runner'
  ```

  Any full-suite count quoted in a report must name this command, or the number is not reproducible.
- [x] `nix build ./abc#checks.x86_64-linux.clj-kondo --no-link` — exit 0.
- [x] `nix build ./abc#checks.x86_64-linux.adr-governance --no-link` — exit 0.
- [x] `nix build .#checks.x86_64-linux.publication-build-real-wiring --no-link` — exit 0.
- [x] `just validate-migration` — exit 0.

---

## Deferred to separate plans

Independent subsystems from the same session's design audit. Do not fold them in.

- **F1 — vacuous predicate `pass`.** `diagnostic_completeness` reports `{"vacuous": true, "diagnostic_count": 0}` yet yields `pass`, contributing to `gate_status: release-qualified`. Semantic change to the qualification gate; may reopen an Accepted decision. **Check first whether P5's own verdict was vacuous.** Highest-value evidence-integrity item.
- **F10 — report contract versioned in name only.** The 2026-07-24 and 2026-07-25 `publications-report.json` both declare `soranoha-build-publication-publications-v1` with materially different key sets; `build-plan.json` drifted the same way (`git` → `source_provenance`). This nearly produced a false regression report.
- **F11 — corpus snapshot divergence.** `corpus_snapshot_hash` differs between the Nix-store corpus derivation (`39fb7e0c…`) and a git checkout at the same pinned revision (`74120b1a…`). Decide which is canonical.
- **F4/F5 — campaign site descriptor.** `corpus_root` and `evidence_store_root` are vestigial (preflight passes with an empty `corpus_root`; `capture-core` hardcodes the fixture corpus). `--evidence-tree` is a misnomer for the campaign-values root. F4 blocks full-corpus qualification.
- **F6 — repetition scope.** Three repetitions applied to hermetic and environmental predicates alike. Rotates `predicate_set_hash`; governed.
- **Rights assessment migration.** The only thing between this system and `release_admissible: true`. Existing plan: `abc/docs/superpowers/plans/2026-07-12-rights-assessment-remediation.md`, Tasks 2–5 outstanding, ADR 0035 still `proposed`.
