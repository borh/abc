# Remove XTDB Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use `superpowers:subagent-driven-development` (recommended) or `superpowers:executing-plans` to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking. Use `superpowers:verification-before-completion` before each commit.

**Goal:** Remove the XTDB v1 runtime dependency and every code/test/doc touchpoint that depends on it, leaving the repository with no XTDB dependency, no XTDB-importing namespace, no XTDB-named symbol, and no Clojure dependency drift attributable to XTDB.

**Architecture:** XTDB is isolated to two source namespaces (`abc.xtdb`, the node-lifecycle wrapper, and the persistence half of `abc.load`) plus four test files (`abc.xtdb-test`, `abc.db-test`, `abc.load-test`, `abc.tei-test`) and one dead-but-XTDB-fixture-driven namespace (`abc.db`, required by `abc.core`). The v0 publication surface (`abc.tools.*`) and the Nix sandbox (`nix/clj-nix-deps.edn`, `deps-lock.json`) already exclude XTDB, so removal is a focused delete-and-strip operation, not a port. No migration to XTDB v2 is performed — XTDB is retired, not upgraded.

Query-runtime boundary (2026-07-04): removing XTDB v1 does not select the
replacement query runtime. `docs/handoffs/query-runtime-and-history-index.md`
records the current evidence and keeps the replacement decision as a separate
ADR track: canonical files remain authoritative, generated SQLite indexes are
the first candidate for small coordinate/history lookup, and DuckDB/Parquet
remain the first candidate for analytical fact tables.

A second class of removals is a direct consequence: the text-fetching helpers in `abc.load` (`web-to-git`, `aozora-bunko-text`, `extract-text`, `sha-512`, `local-cache`, `parse-html`) had no callers *other than* the XTDB persistence functions (`work-id->document`, `persist-texts!`). Once those are removed, the chain is dead and is removed in the same task. Likewise `abc.aozora/to-xtdb-id` is a byte-identical duplicate of `abc.aozora/to-id` whose only caller is the now-vestigial `:xt/id` key in `to-subject`; removing it eliminates orphan `:xt/id` keys from every entity map and is required to satisfy the final grep gate.

**Tech Stack:** Clojure 1.12, `deps.edn`, clj-nix (`bin/update-clj-nix-lock` → `deps-lock.json`), Kaocha, Nix flakes, clj-kondo, Antq (`clojure -M:update`).

## Global Constraints

- The v0 design-bundle contract is the authoritative gate: byte-identical publication output. Schema hashes do not rotate.
- `nix/clj-nix-deps.edn` already omits XTDB; it must not acquire XTDB.
- `deps-lock.json` must regenerate cleanly via `bin/update-clj-nix-lock` with no unexplained additions.
- No new dependencies are introduced. This plan only removes.
- Existing skipped (`^:kaocha/skip`) tests are not un-skipped.
- Historical plan/spec documents under `docs/superpowers/plans/` and `docs/superpowers/specs/` are left as-is (they are point-in-time snapshots); only *current-state* notes are reconciled in Task 9.
- A pre-deletion git tag `legacy-archive-pre-xtdb-removal` is pushed before Task 2 so the deleted XTDB code is recoverable.

## File Structure

**Deleted entirely:**
- `src/abc/xtdb.clj` — XTDB node lifecycle + Datalog query wrapper. Owned the runtime dependency.
- `test/abc/xtdb_test.clj` — tested `abc.xtdb` startup memoization.
- `src/abc/db.clj` — dead namespace; only live code is two unused keyword helpers, and its test is XTDB-fixture-driven.
- `test/abc/db_test.clj` — XTDB-fixture-driven; the one DB-backed test is already `^:kaocha/skip`.
- `test/abc/tei_test.clj` — requires `abc.xtdb/work-query` in its `:once` fixture; 2 of 4 tests already skipped. Rebuild of `abc.tei` is tracked separately by the architecture note.

**Modified (XTDB stripped, file kept):**
- `deps.edn` — remove the two `com.xtdb/*` coordinates.
- `src/abc/load.clj` — remove `abc.xtdb`/`xtdb.api` requires, the persistence functions (`persist-db!`, `persist-texts!`, `work-id->document`), the dead text-fetching chain that served only those functions (`web-to-git`, `aozora-bunko-text`, `extract-text`, `sha-512`, `local-cache`, `parse-html`), and the helpers that served only the above (`with-time-duration`, `pretty-demunge`, `!times`, `build-lagging-transducer`, `parallelising-map`, `lazy-pmap`, `PersistentQueue` import). Keep the Aozora metadata-loading API (`aozora-bunko-db`, `remove-empty-vals`, `aozora-bunko-db-coll`) which is exercised by `abc.load-test`.
- `src/abc/aozora.clj` — delete `to-xtdb-id` (duplicate of `to-id`) and drop the `:xt/id` clause from `to-subject`. Removes orphan `:xt/id` keys from every entity map; satisfies the final grep gate.
- `test/abc/load_test.clj` — remove `abc.xtdb` require and the `persist-texts-uses-supplied-node-for-reads-test`. Keep `load-test` and the skipped `extract-texts-test`.
- `src/abc/core.clj` — remove `[db :as db]` require and the commented `db/*` form (consequent on deleting `abc.db`; `abc.core` is required by the empty `abc.core-test`).
- `data/dev/`, `data/test/` — delete local RocksDB stores (gitignored, untracked).
- `.gitignore` — remove the now-unused `/data/dev/` and `/data/test/` entries.

**Reconciled (current-state docs only):**
- `docs/superpowers/notes/2026-07-04-dependency-drift-queue.md` — mark the XTDB row resolved.
- `docs/superpowers/notes/2026-07-04-dependency-probe-results.md` — update the "Legacy Runtime Dependencies → XTDB" section to record removal.
- `docs/repo-cleanup-plan.md` — remove the `data/dev/`, `data/test/` row.
- `docs/high-level-architecture-note.md` — update the "Retire or demote: `abc.xtdb`" line to past tense.

---

## Task 1: Remove XTDB Coordinates and Regenerate the Lock

**Files:**
- Modify: `deps.edn`
- Regenerate: `deps-lock.json`

**Interfaces:**
- Produces: a `deps.edn` with no `com.xtdb/*` entries; a `deps-lock.json` regenerated by clj-nix with no XTDB jars.

- [ ] **Step 1: Tag the pre-removal state**

Run:

```bash
git tag legacy-archive-pre-xtdb-removal
```

Expected: tag created (recoverable reference for the deleted code in later tasks).

- [ ] **Step 2: Remove the two XTDB coordinates from `deps.edn`**

Delete these two lines (and the blank line that follows them) from the `:deps` map:

```clojure
  com.xtdb/xtdb-core                         {:mvn/version "1.24.5"}
  com.xtdb/xtdb-rocksdb                      {:mvn/version "1.24.5"}
```

The surrounding context remains:

```clojure
  uk.org.russet/tawny-owl                    {:mvn/version "2.3.3"
                                              :exclusions [org.slf4j/slf4j-nop]}
  net.sourceforge.owlapi/owlapi-distribution {:mvn/version "5.5.1"
                                              :exclusions [org.slf4j/slf4j-nop]}

  org.graalvm.js/js-language                 {:mvn/version "24.2.2"}
```

(i.e. the XTDB lines sat between the `owlapi-distribution` and `org.graalvm.js/js-language` entries; after removal, `owlapi-distribution`'s closing map is followed by a blank line and then the `graalvm.js` entry.)

- [ ] **Step 3: Regenerate `deps-lock.json`**

Run:

```bash
./bin/update-clj-nix-lock
```

Expected: command exits 0; `deps-lock.json` is rewritten.

- [ ] **Step 4: Review the lock diff for surprises**

Run:

```bash
git diff deps.edn deps-lock.json
```

Expected: `deps.edn` shows only the two-line removal. `deps-lock.json` may show no XTDB-related change (it already excludes XTDB today) or a small correction; confirm there are **no added** jars and **no** string still mentioning `xtdb`:

```bash
git diff deps-lock.json | rg -i 'xtdb|^\+\s*' || true
```

If the diff adds unexpected dependencies, stop and investigate before continuing.

- [ ] **Step 5: Verify the Nix deps cache still builds**

Run:

```bash
nix build .#clj-nix-focused-tests --print-build-logs 2>&1 | tail -5
```

If the `cljDepsCache` derivation name differs, confirm via `nix flake show .# 2>/dev/null | rg -i 'deps|cache|clj'`. Expected: build succeeds (the offline deps cache is consistent with the regenerated lock).

- [ ] **Step 6: Commit**

```bash
git add deps.edn deps-lock.json
git commit -m "chore: remove com.xtdb/xtdb-core and xtdb-rocksdb dependencies"
```

---

## Task 2: Delete the XTDB Wrapper Namespace and Its Test

**Files:**
- Delete: `src/abc/xtdb.clj`
- Delete: `test/abc/xtdb_test.clj`

**Interfaces:**
- Consumes: Task 1 (deps no longer resolve `xtdb.api`).
- Produces: no namespace named `abc.xtdb` exists. Downstream tasks must remove every reference to it.

- [ ] **Step 1: Confirm nothing outside the known removal set requires `abc.xtdb`**

Run:

```bash
rg -nF 'abc.xtdb' src test --glob '*.clj' --glob '*.cljc'
```

Expected matches (these are the only allowed ones and are handled in later tasks):

```
src/abc/load.clj:11:            [abc.xtdb :as xtdb]
src/abc/xtdb.clj:1:(ns abc.xtdb
src/abc/xtdb.clj:51:  (let [a-node (or node (abc.xtdb/node))]
src/abc/xtdb.clj:59:  (let [a-node (or node (abc.xtdb/node))]
src/abc/xtdb.clj:67:  (let [a-node (or node (abc.xtdb/node))]
src/abc/xtdb.clj:82:  (let [a-node (or node (abc.xtdb/node))]
test/abc/load_test.clj:...      [abc.xtdb :as xtdb]
test/abc/db_test.clj:...        [abc.xtdb :as xtdb]
test/abc/tei_test.clj:...       [abc.xtdb :as xtdb]
test/abc/xtdb_test.clj:...
```

If any `abc.tools.*` namespace appears, stop — that contradicts the survey and must be resolved before deletion.

- [ ] **Step 2: Delete the two files**

```bash
git rm src/abc/xtdb.clj test/abc/xtdb_test.clj
```

- [ ] **Step 3: Verify the deletion does not yet break the compile of unrelated namespaces**

The full suite cannot compile yet (`abc.load`, `abc.db`, `abc.tei-test` still reference the deleted namespace). This step only confirms the files are gone:

```bash
test ! -f src/abc/xtdb.clj && test ! -f test/abc/xtdb_test.clj && echo OK
```

Expected: `OK`.

- [ ] **Step 4: Commit (do not push)**

```bash
git commit -m "refactor: delete abc.xtdb namespace and its test

The XTDB v1 node-lifecycle wrapper and Datalog query helpers are removed
as part of XTDB retirement (see plan 2026-07-04-remove-xtdb). Downstream
references in abc.load, abc.db-test, abc.load-test, and abc.tei-test are
stripped in subsequent tasks."
```

This commit intentionally leaves the build red until Tasks 3–7 land; do not run the full gate yet.

---

## Task 3: Strip XTDB Persistence (and Its Dead Helper Chain) from `src/abc/load.clj`

**Files:**
- Modify: `src/abc/load.clj`

**Interfaces:**
- Consumes: Task 2 (`abc.xtdb` no longer exists).
- Produces: `abc.load` exposes only `aozora-bunko-db`, `aozora-bunko-db-coll`, `remove-empty-vals`. Removed: `persist-db!`, `persist-texts!`, `work-id->document`, `with-time-duration`, `pretty-demunge`, `!times`, `build-lagging-transducer`, `parallelising-map`, `lazy-pmap`, `extract-text`, `aozora-bunko-text`, `sha-512`, `local-cache`, `web-to-git`, `parse-html`.

- [ ] **Step 1: Rewrite `src/abc/load.clj` with the XTDB surface and its dead helper chain removed**

Replace the entire contents of `src/abc/load.clj` with:

```clojure
(ns abc.load
  (:require [clojure.java.io :as io]
            [me.raynes.fs :as fs]
            [charred.api :as charred]
            [clojure.string :as string]
            [abc.aozora :as aozora])
  (:import [org.apache.commons.compress.archivers.zip ZipFile]
           [org.apache.commons.io.input BOMInputStream]))

(defn remove-empty-vals [m]
  (into {}
        (remove
         (fn [[_ v]]
           (let [v (string/trim v)]
             (if (or (nil? v)
                     (and (string? v)
                          (empty? v)))
               true
               false)))
         m)))

(defn aozora-bunko-db
  "Loads bibliographic metadata into graph data structure."
  [path]
  (let [normalized-path (fs/expand-home path)
        db-file (fs/file normalized-path "index_pages" "list_person_all_extended_utf8.zip")

        all-records
        (with-open [z (ZipFile. db-file)]
          (let [csv-file (first (enumeration-seq (.getEntries z)))]
            (with-open [r (->> csv-file
                               (.getInputStream z)
                               BOMInputStream.
                               io/reader)]
              (doall (charred/read-csv r)))))

        [header & records] all-records

        entities
        (into []
              (comp (map (partial zipmap header))
                    (map remove-empty-vals)
                    (map aozora/record-to-entities))
              records)]
    (aozora/merge-entities entities)))

(defn aozora-bunko-db-coll [db]
  (let [{:keys [works persons]} db]
    (into (vals works) (vals persons))))
```

Rationale for what was removed and what was kept:

- **Removed (XTDB persistence):** `persist-db!`, `persist-texts!`, `work-id->document`, the commented `xtdb/q` form, `with-time-duration`, `pretty-demunge`, `build-lagging-transducer`, `parallelising-map`, `lazy-pmap`, `!times`, the `(clojure.lang PersistentQueue)` import, the `abc.xtdb` / `xtdb.api` requires, and the trailing `(do (time (persist-db!)) (time (persist-texts!)))` comment. These directly embodied XTDB persistence and are the point of this plan.
- **Removed (dead helper chain, consequence of XTDB removal):** `web-to-git`, `aozora-bunko-text`, `extract-text`, `sha-512`, `local-cache`, `parse-html`. Verified to have zero callers outside `abc.load` itself (the only callers were the now-deleted `work-id->document` and `persist-texts!`, plus a commented form in the `^:kaocha/skip` `extract-texts-test`). These functions are dead *as a consequence* of removing XTDB persistence; keeping them would re-create exactly the "legacy tree" the project is untangling. Also drops the now-unused `abc.config`, `abc.annotation`, `com.climate.claypoole*`, `taoensso.timbre`, and `clojure.repl` requires, and the `URL`, `IOException`, `MessageDigest`, and `ZipArchiveEntry` imports (each was used only by removed functions).
- **Kept:** `aozora-bunko-db`, `aozora-bunko-db-coll`, `remove-empty-vals`. These form the Aozora metadata-loading API and are exercised by `abc.load-test`'s `load-test` (which validates `*db*` against `:abc.aozora/db-entries`). They have no XTDB coupling. (`aozora-bunko-db-coll` currently has no live caller after Task 6 removes the commented `abc.core` form, but it is the documented collection accessor of this namespace's API and is retained deliberately; it can be removed alongside the rest of `abc.load` if that namespace is later retired wholesale.)

- [ ] **Step 2: Confirm no XTDB symbols remain in the file**

Run:

```bash
rg -nF 'xtdb' src/abc/load.clj
rg -nF ':xt/id' src/abc/load.clj
rg -nF 'persist-db!\|persist-texts!\|work-id->document\|web-to-git\|aozora-bunko-text\|sha-512\|local-cache\|extract-text' src/abc/load.clj
```

Expected: no output from all three commands.

- [ ] **Step 3: Confirm the namespace compiles in isolation**

Run:

```bash
clojure -M:test -e "(require 'abc.load) (println :abc.load-required)"
```

Expected: prints `:abc.load-required` and exits 0 (this works because `abc.load` no longer requires `abc.xtdb`).

- [ ] **Step 4: Commit (do not push yet)**

```bash
git add src/abc/load.clj
git commit -m "refactor: strip XTDB persistence and dead helper chain from abc.load

Removes persist-db!, persist-texts!, work-id->document, and the dead
text-fetching chain (web-to-git, aozora-bunko-text, extract-text,
sha-512, local-cache, parse-html) whose only callers were the XTDB
persistence functions. Retains the Aozora metadata-loading API
(aozora-bunko-db, aozora-bunko-db-coll, remove-empty-vals) exercised
by abc.load-test. See plan 2026-07-04-remove-xtdb."
```

---

## Task 4: Remove XTDB Identity Keys from `src/abc/aozora.clj`

**Files:**
- Modify: `src/abc/aozora.clj`

**Interfaces:**
- Consumes: Task 2 (`abc.xtdb` gone — the only consumer of `:xt/id` keys).
- Produces: no `xtdb` substring in `src/abc/aozora.clj`; entity maps no longer carry a vestigial `:xt/id` key.

`abc.aozora/to-xtdb-id` (line 310) is byte-identical to `abc.aozora/to-id` (line 305, which is kept because `:rdf/about` and the `::work-id`/`::person-id` fields use it). `to-xtdb-id`'s only caller is the `:xt/id` clause in `to-subject` (line 318). After Task 2 deletes `abc.xtdb`, no code consumes the `:xt/id` key — every entity map produced by `record-to-entities` would carry a dead `:xt/id`. Removing the clause and the duplicate function eliminates the orphan keys and is required to satisfy Task 10's grep gate (the function name contains the literal string `xtdb`).

- [ ] **Step 1: Delete `to-xtdb-id` and the `:xt/id` clause from `to-subject`**

In `src/abc/aozora.clj`, change:

```clojure
(defn to-xtdb-id [s]
  (if (url? s)
    s
    (keyword "abc.aozora" s)))

(defn to-subject [subj m]
  (remove-nils
   (assoc m :rdf/about (to-id subj)
          :xt/id (to-xtdb-id subj))))
```

to:

```clojure
(defn to-subject [subj m]
  (remove-nils
   (assoc m :rdf/about (to-id subj))))
```

(i.e. delete the `to-xtdb-id` defn entirely — lines 310–313 plus the trailing blank line 314 — and in `to-subject` drop the `:xt/id (to-xtdb-id subj)` clause so the `assoc` sets only `:rdf/about`. The `:rdf/about` key is kept: it is the RDF subject identity consumed by `abc.tools.metadata-record`, `abc.tools.person-record`, and the SHACL fixtures, so it is not vestigial.)

- [ ] **Step 2: Confirm no XTDB references remain in the file**

```bash
rg -ni 'xtdb' src/abc/aozora.clj
rg -nF ':xt/id' src/abc/aozora.clj
rg -nF 'to-xtdb-id' src/abc/aozora.clj
```

Expected: no output from all three.

- [ ] **Step 3: Confirm schemas still validate the trimmed entity maps**

The entity schemas are open malli maps (`::work = [:map ::work-id ...]`, `::db-entry = [:map ::work ::person]`), so dropping the `:xt/id` key does not break validation — the key was never declared. Confirm the namespace compiles and the load-test schema check still parses:

```bash
clojure -M:test -e "(require 'abc.aozora) (require 'abc.load) (println :ok)"
```

Expected: prints `:ok`, exits 0. (The `load-test` schema assertion is exercised end-to-end by Task 10's Kaocha gate.)

- [ ] **Step 4: Commit**

```bash
git add src/abc/aozora.clj
git commit -m "refactor: drop vestigial :xt/id key and to-xtdb-id from abc.aozora

to-xtdb-id was a byte-identical duplicate of to-id whose only caller
was the :xt/id clause in to-subject. After XTDB removal no code consumes
:xt/id, so the clause and the duplicate are deleted to keep entity maps
free of XTDB identity. :rdf/about (RDF subject identity, used by the
SHACL/metadata-record publication path) is retained."
```

---

## Task 5: Strip XTDB from `test/abc/load_test.clj`

**Files:**
- Modify: `test/abc/load_test.clj`

**Interfaces:**
- Consumes: Task 3 (`abc.load` no longer exposes `persist-texts!`, `work-id->document`, or `!times`).
- Produces: `abc.load-test` requires only `abc.load`, `abc.config`, `abc.tools.malli`, `abc.test-utils`. The `persist-texts-uses-supplied-node-for-reads-test` is removed.

- [ ] **Step 1: Rewrite `test/abc/load_test.clj` without the XTDB require or persist test**

Replace the entire contents of `test/abc/load_test.clj` with:

```clojure
(ns abc.load-test
  (:require [clojure.test :as t :refer [deftest testing is use-fixtures]]
            [abc.test-utils :refer :all]
            [abc.tools.malli :as am]
            [abc.load :as load :refer :all]
            [abc.config :as config]))

(def ^:dynamic ^:private *db* nil)

(defn db-fixture [f]
  (let [db (aozora-bunko-db config/aozora-bunko-path)]
    (binding [*db* db]
      (f))))

(use-fixtures :once
  (fn [f] (am/install!) (f))
  db-fixture)

(deftest load-test
  (testing "Loading AB database fixture"
    (is *db*)
    (is (= (set (keys *db*)) #{:works :persons})))
  (testing "Database schema"
    ;; A full database validation is slow
    (is (schema-valid :abc.aozora/db-entries *db*))))

;; Skipped: Aozora text parsing moves out of Clojure; consumed as JSON AST
;; from an external parser per the parser-IR contract.
(deftest ^:kaocha/skip extract-texts-test
  (testing "Extracting texts from database fixture"
    #_(is (schema-validate :document/body :TODO))))
```

Removed: the `[abc.xtdb :as xtdb]` require and the `persist-texts-uses-supplied-node-for-reads-test` (which asserted the `[:xtdb.api/put ...]` tx shape and rebound `xtdb/*` vars). The skipped `extract-texts-test` body is trimmed of the dead `extract-texts` references it no longer makes.

- [ ] **Step 2: Confirm no XTDB symbols remain**

```bash
rg -nF 'xtdb' test/abc/load_test.clj
```

Expected: no output.

- [ ] **Step 3: Commit**

```bash
git add test/abc/load_test.clj
git commit -m "test: remove XTDB require and persist-texts test from abc.load-test

The persist-texts-uses-supplied-node-for-reads-test asserted the old
[:xtdb.api/put ...] tx shape; with XTDB retired there is no tx path to
assert. Retains the load and (skipped) extract-texts tests."
```

---

## Task 6: Delete `abc.db` and `abc.db-test`, Strip `abc.db` from `abc.core`

**Files:**
- Delete: `src/abc/db.clj`
- Delete: `test/abc/db_test.clj`
- Modify: `src/abc/core.clj`

**Interfaces:**
- Consumes: Task 2 (`abc.xtdb` gone — `abc.db-test` cannot compile).
- Produces: `abc.core` no longer requires `abc.db`. `abc.core-test` (an empty 3-line file with no tests, see Step 1) can still load.

Rationale: `abc.db.clj` has no XTDB import but its only live code is two unused keyword helpers (`to-noun`, `to-predicate`); the rest is commented Aristotle/Jena. `abc.db-test` is XTDB-fixture-driven. `abc.core` references `abc.db` only in a commented form but still declares the `:require`, so deleting `abc.db` without editing `abc.core` would break `abc.core-test` at load time (Kaocha loads every `*-test` namespace; this is a compile failure, not a coverage loss — `abc.core-test` defines zero tests).

- [ ] **Step 1: Note `abc.core-test` is an empty test file**

`test/abc/core_test.clj` is exactly:

```clojure
(ns abc.core-test
  (:require [clojure.test :refer :all]
            [abc.core :refer :all]))
```

— three lines, zero `deftest`s. The concern in this task is load-time compilation (`:refer :all` would fail if `abc.core` fails to compile), not loss of test coverage. Keep `abc.core-test` as-is; its continued presence is harmless and removing it is a separate `abc.core`-disposition decision (see the note in Step 3).

- [ ] **Step 2: Delete the two `abc.db` files**

```bash
git rm src/abc/db.clj test/abc/db_test.clj
```

- [ ] **Step 3: Remove the `[db :as db]` clause from `src/abc/core.clj`**

In `src/abc/core.clj`, change the `:require` form from:

```clojure
  (:require [abc
             [aozora :as aozora]
             [tei :as tei]
             [load :as load]
             [db :as db]
             [git :as git]
             [config :as config]
             [owl :as owl]]
            [clojure.string :as string]
            [me.raynes.fs :as fs]
            [clojure.tools.cli :refer [parse-opts]]))
```

to:

```clojure
  (:require [abc
             [aozora :as aozora]
             [tei :as tei]
             [load :as load]
             [git :as git]
             [config :as config]
             [owl :as owl]]
            [clojure.string :as string]
            [me.raynes.fs :as fs]
            [clojure.tools.cli :refer [parse-opts]]))
```

- [ ] **Step 4: Remove the commented `db/*` form from `abc.core`'s `run`**

In `src/abc/core.clj`, change:

```clojure
(defn run [{:keys [input output]}]
  (println "Running...")
  (when-not (fs/directory? output)
    (fs/mkdir output))
  (let [ttl-file-path (fs/file output "aozora-bunko.ttl")]
    #_(db/save-graph! ttl-file-path
                    (->> input
                         load/aozora-bunko-db
                         load/aozora-bunko-db-coll
                         db/to-triples
                         db/to-graph))))
```

to:

```clojure
(defn run [{:keys [input output]}]
  (println "Running...")
  (when-not (fs/directory? output)
    (fs/mkdir output))
  (let [ttl-file-path (fs/file output "aozora-bunko.ttl")]
    ttl-file-path))
```

Note on `run` as a no-op: this edit is behavior-preserving — `run` was *already* a no-op before XTDB removal (the `db/save-graph!` form was commented out, and `-main` discards `run`'s return value). The change here is only to drop the dangling `db/*` comment so no deleted symbol is referenced. It is explicitly out of scope to trim `abc.core` further (e.g. removing `run` or the unused `input`/`output`/`load` requires); `abc.core` is legacy scaffolding whose disposition is tracked by the separate plan `docs/superpowers/plans/2026-04-29-legacy-namespaces-clj-nix.md`. Retaining the `input`/`output`/`load` requires is intentional so that file's pre-existing shape is unchanged beyond the XTDB-driven minimum.

- [ ] **Step 5: Confirm `abc.core` compiles**

```bash
clojure -M:test -e "(require 'abc.core) (println :abc.core-required)"
```

Expected: prints `:abc.core-required`, exits 0. (`abc.core-test` will load at Kaocha time; it performs no assertions.)

- [ ] **Step 6: Confirm nothing else requires `abc.db`**

```bash
rg -nF 'abc.db' src test --glob '*.clj' --glob '*.cljc'
```

Expected: no output.

- [ ] **Step 7: Commit**

```bash
git add src/abc/core.clj
git commit -m "refactor: delete abc.db namespace and test, drop abc.db require from abc.core

abc.db was a dead namespace (two unused helpers plus commented
Aristotle/Jena); abc.db-test was XTDB-fixture-driven. abc.core referenced
abc.db only in a commented form but still required it, so the clause is
dropped to keep abc.core-test loadable. abc.core/run was already a no-op
pre-removal; only the dangling db/* comment is removed here. Further
trimming of abc.core is deferred to the legacy-namespaces plan."
```

---

## Task 7: Delete `abc.tei-test`

**Files:**
- Delete: `test/abc/tei_test.clj`

**Interfaces:**
- Consumes: Task 2 (`abc.xtdb/work-query` gone).

Rationale: `abc.tei-test`'s `:once` fixture calls `xtdb/work-query` against a live node; 2 of its 4 tests are already `^:kaocha/skip`, and the architecture note records `abc.tei` for rebuild around a published TEI ODD. Keeping it would require manufacturing a static fixture matching `metadata-to-tei`'s expected shape — that is rebuild work, not XTDB-removal work, and belongs to the separate TEI track.

- [ ] **Step 1: Delete the file**

```bash
git rm test/abc/tei_test.clj
```

- [ ] **Step 2: Confirm no remaining test references XTDB**

```bash
rg -nF 'xtdb' test --glob '*.clj' --glob '*.cljc'
```

Expected: no output.

- [ ] **Step 3: Commit**

```bash
git commit -m "test: delete abc.tei-test (XTDB-fixture-driven; rebuild tracked separately)

abc.tei-test's :once fixture called abc.xtdb/work-query; 2/4 tests were
already :kaocha/skip. Rebuilt TEI coverage will be added by the separate
abc.tei rebuild track, not by XTDB removal."
```

---

## Task 8: Remove Local XTDB Stores and Stale `.gitignore` Entries

**Files:**
- Delete (untracked): `data/dev/`, `data/test/`
- Modify: `.gitignore`

- [ ] **Step 1: Delete the local RocksDB stores**

```bash
rm -rf data/dev data/test
```

- [ ] **Step 2: Remove the now-unused `.gitignore` entries**

In `.gitignore`, remove these two lines (currently near line 35–36):

```
/data/dev/
/data/test/
```

- [ ] **Step 3: Confirm no source path writes under `data/dev` or `data/test`**

```bash
rg -nF '"data/dev"' src test --glob '*.clj'
rg -nF '"data/test"' src test --glob '*.clj'
```

Expected: no output (the only writers were `abc.xtdb/start!` default `db-path` and `abc.db-test`'s fixture, both deleted).

- [ ] **Step 4: Commit**

```bash
git add .gitignore
git commit -m "chore: remove data/dev and data/test gitignore entries

No code writes under these paths after XTDB retirement. Local RocksDB
stores deleted."
```

---

## Task 9: Reconcile Current-State Docs

**Files:**
- Modify: `docs/superpowers/notes/2026-07-04-dependency-drift-queue.md`
- Modify: `docs/superpowers/notes/2026-07-04-dependency-probe-results.md`
- Modify: `docs/repo-cleanup-plan.md`
- Modify: `docs/high-level-architecture-note.md`

Historical plan/spec documents under `docs/superpowers/plans/` and `docs/superpowers/specs/` are intentionally left unchanged (they are point-in-time snapshots; their XTDB references will be naturally stale).

- [ ] **Step 1: Mark the XTDB drift-queue row resolved**

In `docs/superpowers/notes/2026-07-04-dependency-drift-queue.md`, replace the XTDB table row:

```markdown
| `deps.edn` | `com.xtdb/xtdb-core` | `1.24.5` | `2.1.0` | Defer until XTDB startup-on-require is removed and legacy/runtime disposition is clear. |
```

with:

```markdown
| `deps.edn` | `com.xtdb/xtdb-core` | ~~`1.24.5`~~ | — | **Resolved 2026-07-04.** XTDB removed entirely (retired, not upgraded to v2). See plan `docs/superpowers/plans/2026-07-04-remove-xtdb.md`. |
```

- [ ] **Step 2: Update the probe-results XTDB section**

In `docs/superpowers/notes/2026-07-04-dependency-probe-results.md`, replace the block:

```markdown
XTDB:

- `abc.xtdb` / `abc.load` own the XTDB runtime dependency.
- Task 4 removed startup-on-require for `abc.xtdb`.
- `timeout 10s clojure -M:test -e "(require 'abc.xtdb) (println :xtdb-required)"`
  exits `0`.
- `abc.load` still does not make a standalone child process exit promptly, but
  the narrower probes show that the remaining process hold comes from
  `abc.annotation`, not XTDB.
```

with:

```markdown
XTDB:

- **Removed 2026-07-04.** `abc.xtdb`, `abc.db`, the XTDB-backed half of
  `abc.load`, and the vestigial `:xt/id`/`to-xtdb-id` surface in `abc.aozora`
  were deleted; `com.xtdb/xtdb-core` and `com.xtdb/xtdb-rocksdb` were dropped
  from `deps.edn`. XTDB was retired, not upgraded to v2. See plan
  `docs/superpowers/plans/2026-07-04-remove-xtdb.md`.
- The prior "startup-on-require" finding is moot: the namespace no longer exists.
- The `abc.annotation` process-hold noted in the earlier probe is unrelated
  to XTDB and is unchanged.
```

- [ ] **Step 3: Remove the `data/dev`, `data/test` row from the repo cleanup plan**

In `docs/repo-cleanup-plan.md`, delete the row:

```markdown
| `data/dev/`, `data/test/` | local XTDB/RocksDB-style stores |
```

- [ ] **Step 4: Update the architecture note's "retire or demote" line**

In `docs/high-level-architecture-note.md`, change:

```markdown
- Retire or demote: `abc.xtdb` and `abc.git` as central architecture. They may
  remain useful experiments or query/cache backends, but they conflict with a
  manifest-first model if treated as the source of identity.
```

to:

```markdown
- Retired (2026-07-04): `abc.xtdb` is deleted; XTDB is no longer a dependency.
  `abc.git` is not central architecture. A future query runtime may be chosen
  by ADR when access patterns require it; until then XTDB is not a candidate.
```

Leave the other XTDB mentions in `docs/high-level-architecture-note.md` (the candidate-storage tables) unchanged — they list XTDB as one option among many and are forward-looking design notes, not current-state claims.

- [ ] **Step 5: Commit**

```bash
git add docs/superpowers/notes/2026-07-04-dependency-drift-queue.md \
        docs/superpowers/notes/2026-07-04-dependency-probe-results.md \
        docs/repo-cleanup-plan.md \
        docs/high-level-architecture-note.md
git commit -m "docs: record XTDB removal in current-state notes"
```

---

## Task 10: Final Verification Gate

**Files:** none (verification only).

- [ ] **Step 1: No XTDB references remain anywhere in code or deps**

```bash
rg -ni 'xtdb' src test deps.edn deps-lock.json nix/clj-nix-deps.edn --glob '*.clj' --glob '*.cljc' --glob '*.edn' --glob '*.json'
```

Expected: no output. This deliberately matches on the substring `xtdb` (case-insensitive), so it catches any straggler `to-xtdb-id`, `:xt/id`, `xtdb.api/put`, or stray require. (If `docs/` were included, only design-survey/candidate mentions and historical plans/specs should appear — those are out of scope per Task 9 constraints.)

- [ ] **Step 2: No orphan `:xt/id` keys are produced**

```bash
rg -nF ':xt/id' src test --glob '*.clj' --glob '*.cljc'
```

Expected: no output (Task 4 removed the only producer; Tasks 3 and 5 removed the only test-side occurrences).

- [ ] **Step 3: Antq reports zero XTDB drift**

```bash
clojure -M:update 2>&1 | rg -i 'xtdb' || echo "no xtdb drift"
```

Expected: `no xtdb drift`.

- [ ] **Step 4: Focused Kaocha suite is green**

```bash
ABC_TEI_SCHEMA_SKIP=1 bin/kaocha
```

Expected: all tests pass, 0 failures, 0 errors. (`abc.load-test`'s `load-test` exercises `aozora-bunko-db` end-to-end, which transitively exercises the `to-subject` change from Task 4 via `record-to-entities`; the open-map schemas accept the trimmed entity maps.)

- [ ] **Step 5: Design bundle is byte-identical**

```bash
nix run .#validate-design-bundle
```

Expected: success; no schema hash rotation.

- [ ] **Step 6: Full Nix flake check is green**

```bash
nix flake check --print-build-logs
```

Expected: all checks pass (including `clj-nix-focused-tests` and the design-bundle/provenance checks).

- [ ] **Step 7: Active-surface lint is clean**

```bash
./bin/lint-active
```

Expected: exits 0 (no `--fail-level error` findings on `src/abc/tools` / `test/abc/tools`).

- [ ] **Step 8: Diff hygiene**

```bash
git diff --check
git log --oneline -n 10
```

Expected: no whitespace errors; the ten commits from Tasks 1–9 are present.

---

## Self-Review

**1. Spec coverage.** The request was "remove XTDB; no Clojure dep drift; keep everything clean." Task 1 removes the deps and regenerates the lock (no drift). Tasks 2–7 remove every XTDB-importing/depending namespace and test identified in the survey (`abc.xtdb`, `abc.db`, the persistence half of `abc.load`, `abc.xtdb-test`, `abc.db-test`, `abc.tei-test`, the persist test in `abc.load-test`, the `abc.db` require in `abc.core`) **plus** the XTDB-vestigial surface in `abc.aozora` (`to-xtdb-id` and the `:xt/id` clause in `to-subject`) that a substring grep would otherwise catch (this was flagged in review and verified: `to-xtdb-id` is a byte-identical duplicate of `to-id` whose only caller was the `:xt/id` clause). Task 3 also removes the dead text-fetching chain (`web-to-git`/`aozora-bunko-text`/`extract-text`/`sha-512`/`local-cache`/`parse-html`) whose only callers were the deleted XTDB persistence functions — verified to have zero callers elsewhere, so retaining them would re-create the "legacy tree" the project is untangling. Task 8 removes the local stores and gitignore entries. Task 9 reconciles current-state docs. Task 10 verifies gates, with a substring grep honest enough to catch `to-xtdb-id` and a second grep specifically for orphan `:xt/id` keys. The deferred-drift queue's XTDB row is explicitly resolved. No XTDB v2 migration is performed (retirement, as requested). `abc.core`'s `run` was *already* a no-op pre-removal (the `db/save-graph!` form was commented), so trimming the dangling `db/*` comment is behavior-preserving for the v0 path; further `abc.core` trimming is explicitly deferred to the legacy-namespaces plan.

**2. Placeholder scan.** Every code-bearing step contains the literal target file content or exact before/after blocks. No "TBD", "add validation", or "similar to Task N". The two judgment calls — leaving forward-looking XTDB-as-candidate mentions in the architecture note's storage tables, and retaining `aozora-bunko-db-coll` despite its currently-zero live callers — are stated explicitly with rationale, not left ambiguous.

**3. Type/consistency check.** `abc.load`'s new public surface (Task 3) matches what Task 5's rewritten `abc.load-test` consumes (`aozora-bunko-db`, `aozora-bunko-db-coll`, `schema-valid` via `abc.test-utils`). `abc.aozora` after Task 4 retains `to-id` (used by `:rdf/about`, `::work-id`, `::person-id`) and drops the duplicate `to-xtdb-id`; the open-map malli schemas (`::work`, `::db-entry`) accept entities with or without `:xt/id`, so Task 4 is schema-compatible with Task 5's `load-test`. `abc.core` after Task 6 no longer references `db` anywhere. Task 2's deletion is sequenced before Tasks 3–7 so each downstream edit removes a now-dangling reference; the build is intentionally red between Task 2 and the end of Task 7, then green from Task 7 onward. Task 10 Step 1's substring grep would catch any straggler `xtdb` reference (including `to-xtdb-id`, which Task 4 deletes) introduced by an inconsistent edit; Step 2 catches any orphan `:xt/id` producer that Task 4 might have missed.

One known self-inflicted staleness: historical plans (`2026-07-03-soranoha-naming-migration.md`, `2026-07-03-soranoha-naming-design.md`, `2026-04-29-legacy-namespaces-clj-nix.md`) still mention `xtdb.clj` / `xtdb-*` in future-tense migration steps. These are point-in-time snapshots per the global constraint and are intentionally not rewritten; if the soronoha naming migration or legacy-namespaces disposition is later executed, its plan will need refreshing at that time.
