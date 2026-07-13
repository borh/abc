# babashka.fs Consistency Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Standardize safe ABC production filesystem operations on `babashka.fs` 0.5.34 while preserving behavior and explicitly retaining low-level atomicity, permission, containment, archive, byte, and Java-library boundaries.

**Architecture:** Land a syntax-aware production policy first with a fixed shrinking grandfather set, then migrate cohesive namespace groups under old-green/new-green characterization. Use `babashka.fs` directly; keep `abc.tools.files` for existing ABC-specific compound operations and keep exact low-level NIO exceptions visible.

**Tech Stack:** Clojure 1.12, babashka.fs 0.5.34, clojure.test/Kaocha, clj-kondo, Nix flakes.

## Global Constraints

- This is a uniformity refactor: do not change paths, output ordering, overwrite behavior, symlink behavior, errors, formats, or caller-visible return types.
- Characterization tests must pass against the old implementation before production edits and pass unchanged afterward.
- Use `babashka.fs` directly for ordinary operations; do not add a generic filesystem wrapper or runner seam.
- Keep `io/file` where a Java API or preserved public return type requires `java.io.File`.
- Prefer `java.nio.file.Path` internally; convert with `fs/file` or `str` only at the consumer boundary and do not oscillate representations inside a function.
- Use explicit sorting after `fs/list-dir`, `fs/glob`, or traversal whenever output order is observable.
- Preserve missing-directory, missing-path, dangling-symlink, replacement, cleanup, and failure behavior per site.
- Do not replace real-path containment (`toRealPath`), atomic move/temp/cleanup units, POSIX permissions, archive/library File boundaries, or byte-array reads.
- The legacy grandfather set may only shrink. Permanent NIO and non-File/behavior-sensitive instance-interop exceptions are exact namespace/operation pairs with rationales.
- Do not upgrade babashka.fs beyond 0.5.34.

---

### Task 1: Ship syntax-aware drift prevention

**Files:**
- Create: `abc/test/abc/tools/filesystem_policy_test.clj`

**Interfaces:**
- Produces: `legacy-filesystem-namespaces`, the fixed set consumed and reduced by Tasks 2–6; `permanent-files-operations`, the exact NIO exception map; and `permanent-interop-operations`, the exact receiver-blind instance exception map.

- [ ] **Step 1: Add source-form parsing tests**

Create tests for a private `filesystem-interop-calls` analyzer. Parse with `read` rather than searching text so comments and strings do not match. The tests must cover all supported spellings and collisions:

```clojure
(deftest filesystem-interop-call-shapes-test
  (is (= #{:exists :isFile :delete}
         (filesystem-interop-calls
          '[(.exists file)
            (. file isFile)
            (-> file .delete)]))))

(deftest filesystem-policy-ignores-lexical-collisions-test
  (is (empty? (filesystem-interop-calls
               '[".delete" ;; comments disappear at read time
                 (delete transient-map :key)
                 (repository.delete branch)]))))
```

Add a fixture-driven test proving a non-grandfathered temporary production namespace containing `(.exists file)` is reported, while a comment/string and `(.availableProcessors (Runtime/getRuntime))` are not.

- [ ] **Step 2: Run the new focused test and confirm red**

Run from `abc/`:

```sh
bin/kaocha --focus abc.tools.filesystem-policy-test
```

Expected: FAIL because the analyzer and policy data do not exist.

- [ ] **Step 3: Implement the source-form analyzer**

Use `clojure.java.io/reader`, `clojure.walk/postwalk`, and a `LineNumberingPushbackReader`. Read `.clj` with `:read-cond :allow :features #{:clj}` and tolerate EOF with a sentinel. Normalize these syntactic forms:

```clojure
(def forbidden-file-methods
  #{:mkdirs :exists :isFile :isDirectory :listFiles :renameTo :delete
    :getCanonicalFile :getCanonicalPath :getAbsolutePath :relativize})

(defn- dotted-method [x]
  (when (and (symbol? x) (string/starts-with? (name x) "."))
    (keyword (subs (name x) 1))))

(defn- form-methods [form]
  (when (seq? form)
    (let [head (first form)]
      (cond
        (dotted-method head) #{(dotted-method head)}
        (= '. head) (let [method-form (nth form 2 nil)
                          method (if (seq? method-form)
                                   (first method-form)
                                   method-form)]
                      (some-> method name keyword hash-set))
        (#{'-> '->>} head) (into #{} (keep dotted-method) (rest form))
        (= '.. head) (into #{}
                           (keep (fn [step]
                                   (cond
                                     (seq? step) (some-> step first name keyword)
                                     :else (dotted-method step))))
                           (drop 2 form))
        :else #{}))))

(defn- filesystem-interop-calls [forms]
  (let [found (volatile! #{})]
    (walk/postwalk
     (fn [form]
       (vswap! found into (filter forbidden-file-methods (form-methods form)))
       form)
     forms)
    @found))
```

`file-seq` is a separate exact symbol violation. Bind `*default-data-reader-fn*` to return a tagged-literal value so project-specific tags in `.cljc` do not abort the scan. Preserve file/row information in reported violations by reading top-level forms one at a time and attaching the reader line before traversal. Add tests for `(. file (exists))`, reader conditionals, and an unknown tagged literal.

- [ ] **Step 4: Add the initial grandfather and permanent exception data**

Set `legacy-filesystem-namespaces` to the exact production namespaces currently containing ordinary operations:

```clojure
#{abc.tools.adr abc.tools.annotation-join-stats
  abc.tools.annotation-join-stats-run abc.tools.aozora-history-audit
  abc.tools.aozora-ingest abc.tools.aozora-replay
  abc.tools.diagram.adr-graph abc.tools.diagram.core abc.tools.facts
  abc.tools.materialize-annotations abc.tools.materialize-import
  abc.tools.materialize-publication abc.tools.path-containment
  abc.tools.person-drift abc.tools.person-drift-history
  abc.tools.request-set-resolver abc.tools.schema abc.tools.snapshot-index
  abc.tools.soranoha abc.tools.soranoha-build-publication
  abc.tools.soranoha-layout-report abc.tools.soranoha-stage-publication
  abc.tools.source-bundle abc.tools.source-bundle-report
  abc.tools.source-snapshot-workset abc.tools.materialize-source-snapshot
  abc.tools.schematron abc.tools.tei abc.tools.tar abc.tools.validate-corpus
  abc.tools.validate-design-bundle abc.tools.workflow abc.tools.workflow.cache}
```

Set the exact permanent `Files` allowlist and rationale map:

```clojure
{'abc.tools.json
 #{'Files/createTempFile 'Files/setPosixFilePermissions
   'Files/move 'Files/deleteIfExists}
 'abc.tools.diagram.presentation-registry
 #{'Files/createTempFile 'Files/move 'Files/deleteIfExists}
 'abc.tools.source-bundle
 #{'Files/createTempFile 'Files/copy 'Files/deleteIfExists}
 'abc.tools.diagram.presentation-svg
 #{'Files/readAllBytes}
 'abc.tools.soranoha-build-publication
 #{'Files/move}}
```

Store a rationale string beside each namespace in the actual map. Fully-qualified `java.nio.file.Files/createTempDirectory` in `validate-design-bundle` is grandfathered for later migration, not permanent.

Add a static-call normalizer that matches symbols whose namespace is exactly `Files` or ends in `.Files`, then returns a bare `Files/<method>` symbol:

```clojure
(defn- normalized-files-call [x]
  (when (symbol? x)
    (let [owner (namespace x)]
      (when (or (= "Files" owner)
                (and owner (string/ends-with? owner ".Files")))
        (symbol "Files" (name x))))))
```

Test both `Files/move` and `java.nio.file.Files/createTempDirectory`, plus a collision such as `ProfileFiles/read` that must not match.

Add `permanent-interop-operations` with exact methods and rationales:

```clojure
{'abc.tools.source-bundle
 {:operations #{:isDirectory}
  :rationale "ZipArchiveEntry predicate, not java.io.File"}
 'abc.tools.aozora-history-audit
 {:operations #{:renameTo}
  :rationale "Preserve File.renameTo same-filesystem boolean failure contract"}}
```

The instance exception map receives the same anti-staleness tests as the `Files/*` map: removing an entry must expose exactly the named violation, and every rationale is nonblank. Do not infer receiver types from optional type hints.

- [ ] **Step 5: Enforce the baseline without blocking legacy code**

Scan regular `src/**/*.clj` and `src/**/*.cljc` files. Fail for forbidden calls in namespaces outside `legacy-filesystem-namespaces`; fail for any `Files/*` call not in the exact permanent map or grandfathered namespace. Assert that every grandfathered namespace exists and currently has at least one violation, preventing stale entries. Assert the literal initial set in a separate baseline test; later tasks change both the working set and expected smaller set in the same reviewed commit.

- [ ] **Step 6: Run policy and static checks**

```sh
bin/kaocha --focus abc.tools.filesystem-policy-test
nix build ./abc#checks.x86_64-linux.clj-kondo
```

Expected: PASS; the sensitivity fixture fails only while installed and is removed before commit.

- [ ] **Step 7: Commit the enforceable baseline**

```sh
git add abc/test/abc/tools/filesystem_policy_test.clj
git commit -m "test(abc): prevent new legacy filesystem idioms"
```

---

### Task 2: Migrate leaf registries, reports, and shared helpers

**Files:**
- Modify: `abc/src/abc/tools/files.clj`
- Modify: `abc/src/abc/tools/request_set_resolver.clj`
- Modify: `abc/src/abc/tools/schema.clj`
- Modify: `abc/src/abc/tools/facts.clj`
- Modify: `abc/src/abc/tools/soranoha_layout_report.clj`
- Modify: `abc/src/abc/tools/source_bundle_report.clj`
- Modify: `abc/src/abc/tools/diagram/adr_graph.clj`
- Modify: `abc/src/abc/tools/diagram/core.clj`
- Modify: `abc/src/abc/tools/schematron.clj`
- Modify: `abc/src/abc/tools/tei.clj`
- Test: corresponding namespaces under `abc/test/abc/tools/`
- Modify: `abc/test/abc/tools/filesystem_policy_test.clj`

**Interfaces:**
- Consumes: Task 1 policy sets.
- Produces: leaf filesystem code using `fs/exists?`, `fs/regular-file?`, `fs/directory?`, `fs/create-dirs`, `fs/list-dir`, `fs/file-name`, and explicitly sorted results.

- [ ] **Step 1: Add old-code golden characterizations**

Cover: unknown registry paths; absent schema directory; facts/report empty directories; report ZIP ordering; diagram missing files; Schematron canonical cache identity through a symlinked parent; TEI absolute/canonical schema paths; `files/delete-tree!` missing-path no-op and `copy-file!` bare-filename parent behavior. For each listing function assert both the selected filenames and their order. Use `abc.test-fs/with-temp-dir` and create file/directory/dangling-symlink fixtures with `babashka.fs` in tests.

- [ ] **Step 2: Run the focused leaf suites against old code**

```sh
bin/kaocha --focus abc.tools.files-test \
  --focus abc.tools.request-set-resolver-test \
  --focus abc.tools.schema-test \
  --focus abc.tools.facts-test \
  --focus abc.tools.soranoha-layout-report-test \
  --focus abc.tools.source-bundle-report-test \
  --focus abc.tools.diagram.adr-graph-test \
  --focus abc.tools.diagram.core-test \
  --focus abc.tools.schematron-test \
  --focus abc.tools.tei-test
```

Expected: PASS before production changes. Record test/assertion counts in the task report.

- [ ] **Step 3: Replace ordinary leaf operations**

Add `[babashka.fs :as fs]` only where absent. Use these exact transformations while preserving returned File values at Java consumers:

```clojure
(fs/exists? path)
(fs/regular-file? path)
(fs/directory? path)
(fs/create-dirs path)
(->> (fs/list-dir dir)
     (filter fs/regular-file?)
     (sort-by (comp str fs/file-name)))
```

For missing-directory-as-empty sites, retain the guard:

```clojure
(if (fs/directory? dir) (fs/list-dir dir) [])
```

Convert to `(fs/file path)` only for APIs or public results that still require File.

- [ ] **Step 4: Shrink and verify the policy**

Remove the nine migrated grandfathered namespaces (`abc.tools.files` was already compliant) from `legacy-filesystem-namespaces` and its exact baseline assertion. Run the unchanged golden suites and policy test; expected PASS with identical assertions.

- [ ] **Step 5: Commit**

```sh
git add abc/src/abc/tools/files.clj \
  abc/src/abc/tools/request_set_resolver.clj \
  abc/src/abc/tools/schema.clj abc/src/abc/tools/facts.clj \
  abc/src/abc/tools/soranoha_layout_report.clj \
  abc/src/abc/tools/source_bundle_report.clj \
  abc/src/abc/tools/diagram/adr_graph.clj \
  abc/src/abc/tools/diagram/core.clj \
  abc/src/abc/tools/schematron.clj abc/src/abc/tools/tei.clj \
  abc/test/abc/tools
git diff --cached --name-only
git commit -m "refactor(abc): use babashka fs in leaf filesystem code"
```

Before committing, verify `git diff --cached --name-only` contains only the Task 2 files listed above.

---

### Task 3: Migrate Aozora and person-history filesystem code

**Files:**
- Modify: `abc/src/abc/tools/aozora_history_audit.clj`
- Modify: `abc/src/abc/tools/aozora_ingest.clj`
- Modify: `abc/src/abc/tools/aozora_replay.clj`
- Modify: `abc/src/abc/tools/person_drift.clj`
- Modify: `abc/src/abc/tools/person_drift_history.clj`
- Test: corresponding test namespaces
- Modify: `abc/test/abc/tools/filesystem_policy_test.clj`

**Interfaces:**
- Produces: preserved corpus discovery, cache, rename, canonical path, and missing-directory contracts.

- [ ] **Step 1: Characterize old behavior**

Add golden tests for: missing `_indexes` returns empty; JSON/person lists sort by filename; `prepare-owned-path!` removes existing files/trees; ingest creates persons/works parents idempotently; replay distinguishes an existing `.git` directory; canonical default-baseline comparison remains true through a symlinked parent; person-drift directory listings exclude non-JSON and directories; `move-directory!` turns a failed boolean rename into the existing `ex-info`.

- [ ] **Step 2: Run old-code focused suites**

```sh
bin/kaocha --focus abc.tools.aozora-history-audit-test \
  --focus abc.tools.aozora-ingest-test \
  --focus abc.tools.aozora-replay-test \
  --focus abc.tools.person-drift-test \
  --focus abc.tools.person-drift-history-test
```

Expected: PASS before production changes.

- [ ] **Step 3: Migrate operations**

Use `fs/list-dir` behind explicit missing-directory guards and retain existing sort keys. Keep `move-directory!` on `.renameTo`: `fs/move` may copy/delete successfully across filesystems where `File.renameTo` returns false, so replacing it would violate the error contract. Its exact `:renameTo` call is covered by `permanent-interop-operations`. Replace canonical comparisons with `(fs/canonicalize path)` equality. Retain `io/file` for ZIP/JGit/JSON consumers requiring File.

- [ ] **Step 4: Shrink policy, rerun unchanged tests, and commit**

Remove the five namespaces from the grandfather set. Run Task 3 suites plus the policy test. Commit:

```sh
git add abc/src/abc/tools/aozora_history_audit.clj \
  abc/src/abc/tools/aozora_ingest.clj abc/src/abc/tools/aozora_replay.clj \
  abc/src/abc/tools/person_drift.clj \
  abc/src/abc/tools/person_drift_history.clj abc/test/abc/tools
git diff --cached --name-only
git commit -m "refactor(abc): use babashka fs in Aozora history tools"
```

---

### Task 4: Migrate materialization and workflow filesystem code

**Files:**
- Modify: `abc/src/abc/tools/materialize_annotations.clj`
- Modify: `abc/src/abc/tools/materialize_import.clj`
- Modify: `abc/src/abc/tools/materialize_publication.clj`
- Modify: `abc/src/abc/tools/snapshot_index.clj`
- Modify: `abc/src/abc/tools/source_snapshot_workset.clj`
- Modify: `abc/src/abc/tools/materialize_source_snapshot.clj`
- Modify: `abc/src/abc/tools/tar.clj`
- Modify: `abc/src/abc/tools/workflow.clj`
- Modify: `abc/src/abc/tools/workflow/cache.clj`
- Test: corresponding existing test namespaces
- Create: `abc/test/abc/tools/tar_test.clj`
- Modify: `abc/test/abc/tools/filesystem_policy_test.clj`

**Interfaces:**
- Produces: preserved manifest/materialization output paths, cache-state decisions, traversal ordering, and File return shapes.

- [ ] **Step 1: Add old-code golden tests**

Pin: annotations registry listing and order; optional import sidecar precedence; publication optional source manifest; snapshot-index parent creation for a bare filename; source-workset recursive discovery including dangling symlinks and deterministic relative order; materialized source-snapshot relative paths through a symlinked source parent; TAR parent creation; workflow output creation; cache `:missing` versus `:stale` decisions.

- [ ] **Step 2: Run old implementation suites**

```sh
bin/kaocha --focus abc.tools.materialize-annotations-test \
  --focus abc.tools.materialize-import-test \
  --focus abc.tools.materialize-publication-test \
  --focus abc.tools.snapshot-index-test \
  --focus abc.tools.source-snapshot-workset-test \
  --focus abc.tools.materialize-source-snapshot-test \
  --focus abc.tools.tar-test \
  --focus abc.tools.workflow-test \
  --focus abc.tools.workflow.cache-test
```

Expected: PASS before production edits. `tar_test.clj` directly characterizes bare-filename parent creation and archive member ordering.

- [ ] **Step 3: Migrate and preserve boundary types**

Use fs predicates/creation/listing. For source-workset recursive discovery use `fs/walk-file-tree` or `fs/glob` only after confirming it does not follow symlinks under 0.5.34; otherwise keep a small local traversal over `fs/list-dir`. Sort normalized relative path strings before returning. Keep `ZipFile`, TAR, manifest, and parser APIs on `fs/file` at their boundary.

- [ ] **Step 4: Shrink policy and commit green code**

Remove the nine namespaces from the grandfather set. Re-run the unchanged Task 4 suites and policy test, then commit:

```sh
git add abc/src/abc/tools/materialize_annotations.clj \
  abc/src/abc/tools/materialize_import.clj \
  abc/src/abc/tools/materialize_publication.clj \
  abc/src/abc/tools/snapshot_index.clj \
  abc/src/abc/tools/source_snapshot_workset.clj \
  abc/src/abc/tools/materialize_source_snapshot.clj \
  abc/src/abc/tools/tar.clj abc/src/abc/tools/workflow.clj \
  abc/src/abc/tools/workflow/cache.clj \
  abc/test/abc/tools abc/test/abc/tools/filesystem_policy_test.clj
git commit -m "refactor(abc): use babashka fs in materialization workflows"
```

---

### Task 5: Migrate Soranoha orchestration and staging filesystem code

**Files:**
- Modify: `abc/src/abc/tools/soranoha.clj`
- Modify: `abc/src/abc/tools/soranoha_stage_publication.clj`
- Modify: `abc/src/abc/tools/annotation_join_stats.clj`
- Modify: `abc/src/abc/tools/annotation_join_stats_run.clj`
- Modify: `abc/src/abc/tools/adr.clj`
- Modify: `abc/src/abc/tools/validate_corpus.clj`
- Test: corresponding existing test namespaces
- Create: `abc/test/abc/tools/soranoha_stage_publication_test.clj`
- Modify: `abc/test/abc/tools/filesystem_policy_test.clj`

**Interfaces:**
- Produces: preserved CLI orchestration, staging layout, recursive snapshot selection, annotation discovery, ADR validation, and corpus validation behavior.

- [ ] **Step 1: Characterize old code**

Add golden tests for snapshot roots that are missing/files/directories; warnings and run-summary optional files; recursive `soranoha` file selection and sort order; staging replacement and run-summary copy; annotation parser-IR directory selection; ADR missing/not-directory errors and sorted Markdown inputs; validation missing directories and symlink predicates; annotation subprocess fixture discovery.

- [ ] **Step 2: Run focused old-code suites**

```sh
bin/kaocha --focus abc.tools.soranoha-test \
  --focus abc.tools.soranoha-stage-publication-test \
  --focus abc.tools.annotation-join-stats-test \
  --focus abc.tools.annotation-join-stats-run-test \
  --focus abc.tools.adr-test \
  --focus abc.tools.validate-corpus-test
```

Expected: PASS before production edits.

- [ ] **Step 3: Migrate ordinary operations**

Replace predicates, creation, and traversal with fs operations. Preserve CLI/ex-data path strings with `str` only at those boundaries. Keep explicit sorting by existing filename/path keys. Do not convert XML/schema/library File consumers.

- [ ] **Step 4: Shrink policy and commit**

Remove the six namespaces from the grandfather set. Run Task 5 suites and policy test unchanged. Commit:

```sh
git add abc/src/abc/tools/soranoha.clj \
  abc/src/abc/tools/soranoha_stage_publication.clj \
  abc/src/abc/tools/annotation_join_stats.clj \
  abc/src/abc/tools/annotation_join_stats_run.clj \
  abc/src/abc/tools/adr.clj abc/src/abc/tools/validate_corpus.clj \
  abc/test/abc/tools abc/test/abc/tools/filesystem_policy_test.clj
git commit -m "refactor(abc): use babashka fs in Soranoha orchestration"
```

---

### Task 6: Migrate publication build and design validation safely

**Files:**
- Modify: `abc/src/abc/tools/soranoha_build_publication.clj`
- Modify: `abc/src/abc/tools/validate_design_bundle.clj`
- Modify: `abc/src/abc/tools/path_containment.clj`
- Modify: `abc/src/abc/tools/source_bundle.clj`
- Test: corresponding test namespaces
- Modify: `abc/test/abc/tools/filesystem_policy_test.clj`

**Interfaces:**
- Produces: ordinary operations migrated while preserving four permanent boundaries: atomic output promotion, real-path containment, staged archive validation, and Java validation APIs.

- [ ] **Step 1: Golden-test high-risk old behavior**

Pin: output-root exists/replace decisions; parent creation with bare output name; atomic promotion result remains File; publication reuse checks; copy-flat filtering/order; design-bundle temporary directory cleanup on success and thrown validation; missing plaintext behavior; containment states `:missing`, `:inside`, and symlink escape; source-bundle staged-file cleanup after both success and failure.

- [ ] **Step 2: Run old-code suites**

```sh
bin/kaocha --focus abc.tools.soranoha-test \
  --focus abc.tools.validate-design-bundle-test \
  --focus abc.tools.path-containment-test \
  --focus abc.tools.source-bundle-test
```

Expected: direct Kaocha may retain the documented `TEI_SCHEMA_PATH` environmental errors; run the exact new golden vars directly and record them green, then use the Nix focused suite for the complete validation namespace.

- [ ] **Step 3: Migrate ordinary operations but retain exact exceptions**

Keep `Files/move` in `promote-output-root!`, the `toRealPath` calls in containment, and the `Files/createTempFile`/copy/delete unit in source-bundle unchanged. Migrate only surrounding predicates, creation, listing, canonicalization, and flat copy traversal. Convert the two validation-only `Files/createTempDirectory`/manual-delete units to `fs/with-temp-dir` only after the golden cleanup tests prove equivalent dynamic extent; use `(fs/file temp)` at Java consumers.

- [ ] **Step 4: Shrink policy and verify boundaries**

Remove all four namespaces from the grandfather set. `source-bundle` then remains compliant through its exact `permanent-files-operations` and `permanent-interop-operations` entries rather than through grandfathering. Add assertions that deleting any one permanent entry makes the fixture fail and that every rationale string is nonblank.

- [ ] **Step 5: Commit**

```sh
git add abc/src/abc/tools/soranoha_build_publication.clj \
  abc/src/abc/tools/validate_design_bundle.clj \
  abc/src/abc/tools/path_containment.clj \
  abc/src/abc/tools/source_bundle.clj abc/test/abc/tools
git diff --cached --name-only
git commit -m "refactor(abc): standardize publication filesystem operations"
```

---

### Task 7: Audit representation seams and verify the repository

**Files:**
- Read/verify: all production and filesystem characterization namespaces changed by Tasks 1–6

**Interfaces:**
- Produces: final acceptance evidence and a zero-grandfather production policy.

- [ ] **Step 1: Verify the planned grandfather arithmetic reached zero**

Run from `abc/`:

```sh
bin/kaocha --focus abc.tools.filesystem-policy-test
rg -n "file-seq|\.(mkdirs|exists|isFile|isDirectory|listFiles|renameTo|delete|getCanonicalFile|getCanonicalPath|getAbsolutePath|relativize)\b" src --glob '*.clj' --glob '*.cljc'
```

The policy, not the raw search, is acceptance evidence. Assert
`legacy-filesystem-namespaces` is already `#{}` after Tasks 2–6. Each raw-search result
must correspond to a permanent exact instance exception, a Java/library boundary the
policy explicitly permits, or a syntax collision test. Do not reopen migrated
namespaces looking for an invented remainder.

- [ ] **Step 2: Audit Path/File conversion seams**

Search migrated files for adjacent `fs/path`→`fs/file`→`fs/path` or
`io/file`→`.toPath` cycles. Confirm existing golden tests contain type assertions for
functions that historically return File. If the audit finds an issue, return it to the
owning Task 2–6 commit and repeat that task's review; do not create an uncharacterized
catch-all cleanup diff here.

- [ ] **Step 3: Run the policy and all focused filesystem suites**

```sh
bin/kaocha --focus abc.tools.filesystem-policy-test \
  --focus abc.tools.files-test \
  --focus abc.tools.soranoha-test \
  --focus abc.tools.validate-design-bundle-test \
  --focus abc.tools.source-bundle-test \
  --focus abc.tools.source-snapshot-workset-test \
  --focus abc.tools.aozora-history-audit-test
```

Expected: zero failures. The two schema-backed direct-Kaocha tests may require `TEI_SCHEMA_PATH`; distinguish them explicitly and rely on the Nix suite for their complete environment.

- [ ] **Step 4: Run acceptance gates from the monorepo root**

```sh
nix build ./abc#checks.x86_64-linux.clj-kondo
nix build ./abc#checks.x86_64-linux.clj-nix-focused-tests
```

Expected: PASS. These and the source policy are primary acceptance evidence.

- [ ] **Step 5: Run repository regression evidence**

```sh
just validate-migration
```

Expected: PASS; report this as broader regression evidence, not filesystem acceptance evidence.

- [ ] **Step 6: Verify final source state**

```sh
git diff --check
git status --short
```

Confirm the policy reports an empty grandfather set, all permanent exception rationales are nonblank, and no untracked fixtures remain.

- [ ] **Step 7: Confirm no final catch-all diff exists**

`git status --short` must be empty. Task 7 is an audit and verification gate, not a
fallback migration batch. Any required correction returns to its owning task and review
cycle.
