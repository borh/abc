# Strict Path Traversal Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Replace five duplicated `file-seq`-compatibility traversals with one deterministic traversal that follows a symlinked root, does not descend through descendant directory symlinks, and propagates listing failures.

**Architecture:** `abc.tools.files/sorted-path-seq` owns traversal mechanics and returns a lazy depth-first sequence of `Path` values. Its root-aware worker receives a private lister argument for deterministic failure testing; production consumers receive no injection seam and retain all domain filtering, path formatting, and final sorting.

**Tech Stack:** Clojure 1.12.5, babashka.fs 0.5.34, clojure.test/Kaocha, Nix flakes.

## Global Constraints

- Convert the traversal root to `java.nio.file.Path` exactly once.
- Yield a missing root once and traverse a directory-valued root even when the root itself is a symlink.
- Yield descendant symlinks but never descend through descendant directory symlinks.
- Use `(sort-by str (fs/list-dir path))` for deterministic internal child ordering.
- Preserve laziness, depth-first pre-order, consumer filtering, relative paths, and final output sorting.
- Propagate all listing failures; do not catch `IOException` or `SecurityException` in traversal code.
- Do not add real-path resolution, visited-node tracking, cycle detection, configurable link behavior, or consumer dependency injection.
- File symlink content containment remains out of scope.
- Schema discovery is allowed to fail at namespace load if its schema tree cannot be listed completely.

---

## File map

- `abc/src/abc/tools/files.clj`: owns the public `sorted-path-seq` policy and private lister seam.
- `abc/test/abc/tools/files_test.clj`: directly proves traversal order, root/descendant symlink asymmetry, missing roots, file symlinks, laziness, and exception propagation.
- `abc/src/abc/tools/schema.clj`: discovers schema files through the shared helper.
- `abc/test/abc/tools/schema_test.clj`: proves schemas beneath descendant directory links are excluded.
- `abc/src/abc/tools/soranoha_layout_report.clj`: discovers report files through the shared helper.
- `abc/test/abc/tools/soranoha_layout_report_test.clj`: proves linked descendant files are excluded without changing output order.
- `abc/src/abc/tools/source_snapshot_workset.clj`: discovers work directories through the shared helper.
- `abc/test/abc/tools/source_snapshot_workset_test.clj`: distinguishes a linked work directory itself from work directories nested below a link.
- `abc/src/abc/tools/soranoha.clj`: discovers staged `.tar.zst` archives through the shared helper.
- `abc/src/abc/tools/soranoha_build_publication.clj`: discovers Aozora ZIP inputs through the shared helper.
- `abc/test/abc/tools/soranoha_test.clj`: proves both archive consumers exclude files reachable only below descendant directory links.

---

### Task 1: Add the strict traversal helper

**Files:**
- Modify: `abc/src/abc/tools/files.clj`
- Modify: `abc/test/abc/tools/files_test.clj`

**Interfaces:**
- Consumes: `babashka.fs/path`, `directory?`, `sym-link?`, and `list-dir`.
- Produces: `(sorted-path-seq root) => lazy sequence of java.nio.file.Path`; private `(sorted-path-seq* root list-dir)` used only by direct tests.

- [ ] **Step 1: Add failing contract tests**

Append the following tests. Use a relative link target for the root-link case so it exercises the link itself rather than a pre-resolved target.

```clojure
(deftest sorted-path-seq-is-deterministic-depth-first-preorder-test
  (fs/with-temp-dir [base {}]
    (let [root (fs/path base "root")]
      (fs/create-dirs (fs/path root "b"))
      (fs/create-dirs (fs/path root "a"))
      (spit (fs/file root "b" "z.txt") "z")
      (spit (fs/file root "a" "y.txt") "y")
      (is (= ["" "a" "a/y.txt" "b" "b/z.txt"]
             (mapv #(files/relative-path root %)
                   (files/sorted-path-seq root)))))))

(deftest sorted-path-seq-yields-a-missing-root-once-test
  (fs/with-temp-dir [base {}]
    (let [missing (fs/path base "missing")]
      (is (= [missing] (vec (files/sorted-path-seq missing)))))))

(deftest sorted-path-seq-traverses-a-symlinked-root-test
  (fs/with-temp-dir [base {}]
    (let [target (fs/path base "target")
          root (fs/path base "root-link")]
      (fs/create-dirs target)
      (spit (fs/file target "inside.txt") "inside")
      (fs/create-sym-link root (fs/path "target"))
      (is (= ["root-link" "inside.txt"]
             (mapv #(str (fs/file-name %))
                   (files/sorted-path-seq root)))))))

(deftest sorted-path-seq-yields-but-does-not-descend-descendant-directory-links-test
  (fs/with-temp-dir [base {}]
    (let [root (fs/path base "root")
          external (fs/path base "external")
          link (fs/path root "linked")]
      (fs/create-dirs root)
      (fs/create-dirs external)
      (spit (fs/file external "outside.txt") "outside")
      (fs/create-sym-link link external)
      (is (= [root link] (vec (files/sorted-path-seq root)))))))

(deftest sorted-path-seq-yields-file-links-test
  (fs/with-temp-dir [base {}]
    (let [root (fs/path base "root")
          target (fs/path base "target.txt")
          link (fs/path root "linked.txt")]
      (fs/create-dirs root)
      (spit (fs/file target) "target")
      (fs/create-sym-link link target)
      (is (= [root link] (vec (files/sorted-path-seq root)))))))

(deftest sorted-path-seq-propagates-listing-errors-on-realization-test
  (fs/with-temp-dir [root {}]
    (let [failure (java.io.IOException. "listing failed")
          paths (#'files/sorted-path-seq* root (fn [_] (throw failure)))]
      (is (= root (first paths)))
      (is (identical? failure
                      (try
                        (doall (rest paths))
                        nil
                        (catch java.io.IOException e e)))))))
```

If `files/relative-path` renders the root as an empty string differently on the current JVM, assert the sequence with `(mapv #(str (fs/relativize root %)) ...)` and record the observed root representation explicitly; do not weaken the child-order assertion.

- [ ] **Step 2: Run the focused tests and verify RED**

Run from `abc/`:

```sh
bin/kaocha --focus abc.tools.files-test
```

Expected: compilation failures resolving `files/sorted-path-seq` and `files/sorted-path-seq*`. Existing file utility tests must remain green.

- [ ] **Step 3: Implement the minimal root-aware traversal**

Add this implementation to `abc/src/abc/tools/files.clj`:

```clojure
(defn- sorted-path-seq*
  [root list-dir]
  (let [root (fs/path root)]
    (letfn [(walk [path descend?]
              (lazy-seq
               (cons path
                     (when (and descend? (fs/directory? path))
                       (mapcat #(walk % (not (fs/sym-link? %)))
                               (sort-by str (list-dir path)))))))]
      (walk root true))))

(defn sorted-path-seq
  "Return a deterministic lazy depth-first path sequence rooted at `root`.
  The root may be a directory symlink; descendant symlinks are yielded but
  never traversed. Directory-listing failures propagate."
  [root]
  (sorted-path-seq* root fs/list-dir))
```

Do not move the `list-dir` call outside `lazy-seq`, and do not apply `fs/sym-link?` to the root's initial `descend?` value.

- [ ] **Step 4: Run the focused tests**

Run from `abc/`:

```sh
bin/kaocha --focus abc.tools.files-test
```

Expected: all `abc.tools.files-test` tests pass. The repository cljfmt gate runs through the `clj-kondo` Nix check in Task 3.

- [ ] **Step 5: Commit the helper**

```sh
git add abc/src/abc/tools/files.clj abc/test/abc/tools/files_test.clj
git diff --cached --check
git commit -m "feat(abc): add strict deterministic path traversal"
```

---

### Task 2: Migrate all five traversal consumers

**Files:**
- Modify: `abc/src/abc/tools/schema.clj`
- Modify: `abc/test/abc/tools/schema_test.clj`
- Modify: `abc/src/abc/tools/soranoha_layout_report.clj`
- Modify: `abc/test/abc/tools/soranoha_layout_report_test.clj`
- Modify: `abc/src/abc/tools/source_snapshot_workset.clj`
- Modify: `abc/test/abc/tools/source_snapshot_workset_test.clj`
- Modify: `abc/src/abc/tools/soranoha.clj`
- Modify: `abc/src/abc/tools/soranoha_build_publication.clj`
- Modify: `abc/test/abc/tools/soranoha_test.clj`

**Interfaces:**
- Consumes: `abc.tools.files/sorted-path-seq` from Task 1.
- Produces: unchanged consumer return shapes with strict descendant-link traversal and propagated listing failures.

- [ ] **Step 1: Rewrite the existing symlink characterizations to the approved contract**

Make these exact semantic changes before production edits:

```clojure
;; schema_test.clj and soranoha_layout_report_test.clj:
;; rename the existing "follows-directory-symlinks" tests to
;; "does-not-descend-through-directory-symlinks" and change the expected
;; discovered collection from the linked file to empty.
(is (= [] (vec (#'schema/checked-in-schema-resources root))))
(is (= [] (mapv (comp str fs/file-name) (#'report/regular-files root))))

;; soranoha_test.clj archive expectation:
(is (= ["z.tar.zst"]
       (get (#'soranoha/archive-summary root) "paths")))

;; soranoha_test.clj work ZIP expectation:
(is (= []
       (mapv (comp fs/path :file)
             (#'build-publication/work-zip-files root))))
```

Replace permission-based “unlistable directory is empty” tests; listing propagation is already deterministic at the helper boundary and must not rely on effective UID behavior.

In `source_snapshot_workset_test.clj`, replace the existing link test with these two tests:

```clojure
(deftest workset-includes-a-directory-link-that-is-itself-a-work-dir-test
  (let [base (fixture/temp-dir "abc-source-snapshot-linked-work")
        root (io/file base "root")
        external (io/file base "external")]
    (try
      (.mkdirs root)
      (fixture/materialized-work! external {:slug "linked"
                                            :title "一"
                                            :work-id "000001"
                                            :person-id "000101"
                                            :work-hash (fixture/example-hash "a1")})
      (fs/create-sym-link (fs/path root "linked-work")
                          (fs/path external "works" "linked"))
      (is (= ["000001"]
             (mapv :work_id
                   (:works (workset/workset-from-root
                            {:input-root (str root)
                             :path-base (str base)
                             :snapshot-scope "unit-test"
                             :snapshot-date "2026-07-07"})))))
      (finally
        (fixture/delete-tree! base)))))

(deftest workset-excludes-work-dirs-nested-below-a-directory-link-test
  (let [base (fixture/temp-dir "abc-source-snapshot-linked-parent")
        root (io/file base "root")
        external (io/file base "external")]
    (try
      (.mkdirs root)
      (fixture/materialized-work! external {:slug "nested"
                                            :title "一"
                                            :work-id "000001"
                                            :person-id "000101"
                                            :work-hash (fixture/example-hash "a1")})
      (fs/create-sym-link (fs/path root "linked-works") external)
      (is (= []
             (:works (workset/workset-from-root
                      {:input-root (str root)
                       :path-base (str base)
                       :snapshot-scope "unit-test"
                       :snapshot-date "2026-07-07"}))))
      (finally
        (fixture/delete-tree! base)))))
```

Use the existing `fixture/materialized-work!`, `fixture/example-hash`, and `try/finally fixture/delete-tree!` fixture style already present in that file. The first test's link target must be the materialized work directory itself; the second test's link target must be its parent.

- [ ] **Step 2: Run the five consumer test namespaces and verify RED**

Run from `abc/`:

```sh
bin/kaocha --focus abc.tools.schema-test \
  --focus abc.tools.soranoha-layout-report-test \
  --focus abc.tools.source-snapshot-workset-test \
  --focus abc.tools.soranoha-test
```

Expected: the new descendant-link exclusion assertions fail against the compatibility traversals. The “link itself is a work dir” assertion should already pass and documents the intentional boundary behavior.

- [ ] **Step 3: Replace every duplicated traversal with the helper**

Use these substitutions, leaving each surrounding filter and final sort unchanged:

```clojure
;; schema.clj
(->> (files/sorted-path-seq schema-dir)
     (sort-by #(str (fs/relativize schema-dir %))))

;; soranoha_layout_report.clj
(->> (files/sorted-path-seq root)
     (filter fs/regular-file?)
     (sort-by #(str (fs/relativize root %)))
     (map fs/file))

;; source_snapshot_workset.clj
(defn- directories-below [root]
  (files/sorted-path-seq root))

;; soranoha.clj
(->> (files/sorted-path-seq root)
     (filter fs/regular-file?)
     (filter #(string/ends-with? (str %) ".tar.zst"))
     (sort-by str)
     vec)

;; soranoha_build_publication.clj
(->> (files/sorted-path-seq aozora-root)
     (map fs/file)
     (filter zip-file?)
     (map (fn [file]
            {:file file
             :relpath (aozora-work-zip? aozora-root file)}))
     vec)
```

Delete the `tree-seq` child functions, their `IOException`/`SecurityException` catches, and the `file-seq` compatibility comment. Do not change `zip-file?`, `candidate-work-dir?`, schema parsing, archive summaries, relative-path formatting, or final consumer sorting.

- [ ] **Step 4: Prove all consumers and the schema namespace are green**

Run from `abc/`:

```sh
bin/kaocha --focus abc.tools.files-test \
  --focus abc.tools.schema-test \
  --focus abc.tools.soranoha-layout-report-test \
  --focus abc.tools.source-snapshot-workset-test \
  --focus abc.tools.soranoha-test
```

Expected: all focused tests pass. In particular, requiring `abc.tools.schema-test` must successfully initialize the checked-in schema registry from the repository tree.

- [ ] **Step 5: Verify the duplication is gone**

Run from the repository root:

```sh
rg -n 'tree-seq fs/directory\?|Match java.io/file-seq|catch java.io.IOException|catch SecurityException' \
  abc/src/abc/tools/schema.clj \
  abc/src/abc/tools/soranoha_layout_report.clj \
  abc/src/abc/tools/source_snapshot_workset.clj \
  abc/src/abc/tools/soranoha.clj \
  abc/src/abc/tools/soranoha_build_publication.clj
rg -n 'files/sorted-path-seq' \
  abc/src/abc/tools/schema.clj \
  abc/src/abc/tools/soranoha_layout_report.clj \
  abc/src/abc/tools/source_snapshot_workset.clj \
  abc/src/abc/tools/soranoha.clj \
  abc/src/abc/tools/soranoha_build_publication.clj
```

Expected: the first command returns no matches; the second returns exactly one production use in each of the five files.

- [ ] **Step 6: Commit the consumer migration**

```sh
git add \
  abc/src/abc/tools/schema.clj abc/test/abc/tools/schema_test.clj \
  abc/src/abc/tools/soranoha_layout_report.clj abc/test/abc/tools/soranoha_layout_report_test.clj \
  abc/src/abc/tools/source_snapshot_workset.clj abc/test/abc/tools/source_snapshot_workset_test.clj \
  abc/src/abc/tools/soranoha.clj abc/src/abc/tools/soranoha_build_publication.clj \
  abc/test/abc/tools/soranoha_test.clj
git diff --cached --check
git commit -m "refactor(abc): centralize strict recursive traversal"
```

---

### Task 3: Run repository acceptance gates

**Files:**
- Verify only; no planned source changes.

**Interfaces:**
- Consumes: the completed helper and five migrated consumers.
- Produces: packaged Nix evidence that schema discovery and the full Clojure policy remain valid.

- [ ] **Step 1: Run the Clojure static and focused Nix checks**

Run from the repository root:

```sh
nix build ./abc#checks.x86_64-linux.clj-kondo
nix build ./abc#checks.x86_64-linux.clj-nix-focused-tests
```

Expected: both derivations build successfully. The focused Nix suite must load `abc.tools.schema` and discover the checked-in schemas from the store-source layout.

- [ ] **Step 2: Run the monorepo regression gate**

```sh
just validate-migration
```

Expected: exit status 0. Treat this as broad regression evidence; the focused helper and consumer tests are the acceptance evidence for traversal semantics.

- [ ] **Step 3: Inspect the final diff and repository state**

```sh
git diff --check main...HEAD
git status --short
git log --oneline --decorate -3
```

Expected: no whitespace errors; no uncommitted task changes; the two implementation commits are visible after the design/plan commits. Preserve and report any unrelated pre-existing worktree changes rather than staging them.
