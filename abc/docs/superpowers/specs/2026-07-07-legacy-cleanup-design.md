# Legacy Code Cleanup Design

Date: 2026-07-07
Status: Accepted

## Goal

Delete all dormant legacy Clojure code from `abc/src/` and `abc/test/`, prune
orphaned dependencies from `deps.edn`, and close the malli boundary leak — all
without rotating any v0 schema hash or breaking the design-bundle gate.

## Architecture

Two phases delivered in a single implementation plan:

- **Phase A — Mechanical cleanup.** Delete 10 dead-stub and superseded
  namespaces (plus 3 test files) whose deletion requires zero design decisions.
  Trim `abc.git` of its `abc.config`-dependent functions (6 functions, 2 vars).
  Remove 3 deps that become orphaned.

- **Phase B — Legacy parser deletion.** Delete the 4 dormant parser-era
  namespaces (`abc.aozora`, `abc.tei`, `abc.stats`, `abc.annotation`) + 2
  test files. Close the malli boundary leak. Remove ~10 deps that become
  orphaned. Preserve history via git tag.

After both phases: 14 source files deleted from `abc/src/` (54 → 40), 6 test
files deleted, `deps.edn` shrinks from ~32 `:deps` entries to 21. The v0
contract surface (`abc.tools.*`, `abc.text`, `abc.annotation.schema`,
`abc.ndc`, `abc.git`) is untouched and all gates stay green.

## Principles

1. **Identity stable.** No v0 schema hash rotates. `manifest_identity_object`
   is byte-identical before and after.
2. **Gate green.** `nix flake check`, `nix run .#validate-design-bundle`,
   `bin/kaocha`, `bin/lint-active` all pass at every commit boundary.
3. **Recoverable.** Phase B deletions are preceded by a git tag
   `legacy-archive-pre-legacy-cleanup` so the deleted code remains accessible.
4. **No new features.** This plan only removes dead code and trims deps. It
   does not add, refactor, or reimplement anything.
5. **Single dep lockfile refresh.** `deps-lock.json` is regenerated exactly
   once, after all deps.edn changes are committed.

---

## Phase A — Mechanical Cleanup

### Phase A.1 — Delete dead stubs (6 source files)

These namespaces have zero callers, zero tests referencing them, and zero
implementation beyond a comment or empty `ns` form.

| File | Lines | Content |
|---|---|---|
| `src/abc/wiki.clj` | 3 | Comment about WikiData/DBpedia quality |
| `src/abc/wlsp.clj` | 5 | `ns` form + TODO comment |
| `src/abc/diff.clj` | 2 | Comment about diff utilities |
| `src/abc/web.cljc` | 2 | Comment about static files |
| `src/abc/rdf.clj` | 10 | All code in `comment` blocks; `ns` body is commented out |
| `src/abc/relaxng.clj` | 10 | Stub `validate-tei`, never implemented |

No deps.edn entries are orphaned by this deletion (these namespaces have no
unique deps).

### Phase A.2 — Delete `abc.config` + update `abc.git`

**Delete:** `src/abc/config.clj` — 5 lines, two hardcoded paths:
`../../Dependencies/aozorabunko` and `../aozora-bunko-tei-lod`. These are
obsolete; the monorepo path policy uses env vars (`AB_WORKSPACE_ROOT`, etc.).

**Modify:** `src/abc/git.clj` — remove the `abc.config` require (line 6) and
every function that depends on those hardcoded paths. The functions kept are
those used by `abc.tools.aozora-history-audit` and `abc.git-blob-test`:

**Keep (v0-used):**
- `load-git-repo [path]` — takes explicit path, no config dependency
- `get-commit-date [commit]` — pure Java interop
- `get-file-log [repo path]` — JGit blame
- `resolve-ref [repo ref]` — JGit ref resolution
- `blob-bytes-at [repo ref path]` — JGit blob extraction
- `write-blob-at! [repo ref path output-file]` — blob to file
- `commits-touching-path [repo path & opts]` — path history
- `file-time-span [repo file from-time to-time]` — time-range filter
- `to-conventional-commit [msg]` — no-op stub (kept as-is, no config dependency)
- `diff-formatter` — in `comment` block (kept as-is)

**Remove (config-dependent or dead):**
- `[abc.config :refer [aozora-bunko-path repo-path]]` — the require clause
- `load-aozora-bunko-git` — uses `aozora-bunko-path`
- `load-repo-git` — uses `repo-path`
- `*ab-repo*`, `*repo*` — dynamic vars
- `repo-or-default` — calls `load-repo-git`
- `ab-repo-or-default` — calls `load-aozora-bunko-git`
- `update-aozora-bunko-repo` — uses `ab-repo-or-default`
- `current-tag-version` — only caller was `abc.core`
- `current-commit` — only caller was `abc.core`
- `commit-tei` — only caller was `abc.core`

**Delete:** `test/abc/git_test.clj` — its only test is `^:kaocha/skip` and
depends on `load-aozora-bunko-git`.

**Modify:** `test/abc/git_blob_test.clj` — delete the
`update-aozora-bunko-repo-pulls-aozora-repo-test` (lines 57–93). Keep
`blob-bytes-at-reads-file-content-at-ref-test` and
`commits-touching-path-returns-chronological-path-history-test`.

### Phase A.3 — Delete `abc.core`

**Delete:** `src/abc/core.clj` — 45 lines, no-op CLI. `run` returns an unused
var; `-main` works but is not wired into any v0 alias. Superseded by
`abc.tools.*` CLIs.

**Delete:** `test/abc/core_test.clj` — 3 lines, zero `deftest`s.

No deps are orphaned by this deletion alone (the deps `abc.core` consumed are
still used by other namespaces).

### Phase A.4 — Delete `abc.load`

**Delete:** `src/abc/load.clj` — 45 lines. Already XTDB-stripped. The remaining
API (`aozora-bunko-db`, `aozora-bunko-db-coll`, `remove-empty-vals`) is
superseded by `abc.tools.aozora-csv` + `abc.tools.aozora-ingest`. No v0 caller
exists.

**Delete:** `test/abc/load_test.clj` — requires `abc.load`, `abc.config`,
`abc.aozora`. Has one passing test (`load-test`) and one skipped test
(`extract-texts-test`).

### Phase A.5 — Delete `abc.owl`

**Delete:** `src/abc/owl.clj` — 30 lines, all code in `comment` blocks.
Tawny-OWL ontology was never materialized. Ontology work remains a
design-survey item.

**Delete:** `test/abc/owl_test.clj` — requires `abc.owl`.

### Phase A.6 — Remove orphaned deps from `deps.edn`

After Phase A.1–A.5, six dependencies have zero surviving consumers:

| Dep | Consumers |
|---|---|
| `clj-commons/fs {:mvn/version "1.6.312"}` | `abc.core`, `abc.load` (both deleted) |
| `uk.org.russet/tawny-owl {:mvn/version "2.3.3" …}` | `abc.owl` (deleted) |
| `net.sourceforge.owlapi/owlapi-distribution {:mvn/version "5.5.1" …}` | transitive for tawny-owl |
| `com.climate/claypoole {:mvn/version "1.1.4"}` | Already orphaned (last consumer removed in XTDB cleanup) |
| `parallel/parallel {:mvn/version "0.10"}` | Already orphaned |
| `expound/expound {:mvn/version "0.9.0"}` | `abc.owl-test`, `abc.git-test` (both deleted in Phase A) |

Remove these six entries from the `:deps` map. Regenerate `deps-lock.json`.

### Phase A gate

After all Phase A deletions and modifications:
- `nix flake check` — `clj-nix-focused-tests` passes (Kaocha auto-discovers;
  deleted test namespaces simply don't load)
- `bin/kaocha` — all surviving tests pass
- `bin/lint-active` — clean (lints only `src/abc/tools` + `test/abc/tools`)
- `nix run .#validate-design-bundle` — zero schema hash rotation
- `git grep wiki.clj\|wlsp.clj\|diff.clj\|web.cljc\|rdf.clj\|relaxng.clj\|config.clj\|core.clj\|owl.clj` — no references in tracked files (except docs/plans/specs naming them)
- `abc.git` compiles; `abc.git-blob-test` passes both remaining tests

---

## Phase B — Legacy Parser Deletion

### Phase B.0 — Pre-deletion tag

```bash
git tag -a legacy-archive-pre-legacy-cleanup -m "Last commit containing
pre-v0 parser-era Clojure code (abc.aozora, abc.tei, abc.stats,
abc.annotation). Deleted 2026-07-07 per legacy-cleanup-design."
```

### Phase B.1 — Delete the four legacy namespaces + tests

| Delete | Lines | Description |
|---|---|---|
| `src/abc/aozora.clj` | 330 | Legacy CSV entity parser. Superseded by `abc.tools.aozora-csv`. |
| `src/abc/tei.clj` | 130 | Legacy TEI generation. Superseded by `abc.tools.tei-header` + `abc.tools.parser-ir-tei`. |
| `src/abc/stats.clj` | 75 | Stylometrics. Tokenization/stats moved to Rust `ab-morph-run`. |
| `src/abc/annotation.clj` | 300 | Legacy Aozora annotation parser. Parser is external via `ab-validator`. |
| `test/abc/aozora_test.clj` | — | Requires `abc.aozora` |
| `test/abc/annotation_test.clj` | — | Requires `abc.annotation` |

`test/abc/tei_test.clj` was already deleted in the XTDB removal (2026-07-04).
`abc.stats` has no test file.

### Phase B.2 — Fix the malli boundary leak

**Modify:** `src/abc/tools/malli.clj` (lines 17–44)

Replace the `project-namespaces` list and `ns-loadable?` guard:

```clojure
;; Before:
(def ^:private project-namespaces
  "Registry-owning namespaces, in declared merge order. The Nix
  focused-test sandbox deliberately omits the deps for some of these
  (e.g. java-time for abc.aozora); `install!` skips a namespace whose
  require fails so the foundation still works there."
  '[abc.annotation.schema abc.aozora abc.tei])

(defn- ns-loadable? [ns-sym]
  ;; A transitive :require failure inside the loaded namespace is
  ;; wrapped as Compiler$CompilerException whose cause is the inner
  ;; FileNotFoundException, so checking the cause chain is the
  ;; reliable way to skip Nix-sandbox-omitted namespaces.
  (try (require ns-sym) true
       (catch Throwable t
         (loop [cause t]
           (cond
             (nil? cause) (throw t)
             (instance? java.io.FileNotFoundException cause) false
             :else (recur (.getCause cause)))))))

(defn- compose-project-registry []
  (reduce
   (fn [acc ns-sym]
     (let [v (when (ns-loadable? ns-sym)
               (some-> (resolve (symbol (name ns-sym) "registry")) deref))]
       (cond-> acc (map? v) (merge v))))
   {}
   project-namespaces))
```

```clojure
;; After:
(def ^:private project-namespaces
  "Registry-owning namespaces, in declared merge order."
  '[abc.annotation.schema])

(defn- compose-project-registry []
  (reduce
   (fn [acc ns-sym]
     (let [v (some-> (resolve (symbol (name ns-sym) "registry")) deref)]
       (cond-> acc (map? v) (merge v))))
   {}
   project-namespaces))
```

Rationale: `abc.aozora` and `abc.tei` are deleted in Phase B.1.
`abc.annotation.schema` is the sole remaining registry source and always
loads in the v0 sandbox. The `ns-loadable?` guard is no longer needed.

### Phase B.3 — Remove orphaned deps from `deps.edn`

After Phase B.1, five more dependencies have zero surviving consumers:

| Dep | Consumers (all deleted in Phase B) |
|---|---|
| `lambdaisland/regal {:mvn/version "0.1.175"}` | `abc.aozora` |
| `com.taoensso/timbre {:mvn/version "6.8.0"}` | `abc.annotation` |
| `org.clojure/core.match {:mvn/version "1.1.1"}` | `abc.annotation` |
| `net.cgrand/xforms {:mvn/version "0.19.6"}` | `abc.stats`, `abc.annotation` |
| `clojure.java-time/clojure.java-time {:mvn/version "1.4.3"}` | `abc.aozora` |

Remove these entries from the `:deps` map. Regenerate `deps-lock.json`.

`org.clojure/data.xml` is deliberately retained: the active v0 TEI header and
publication materialization tools still require `clojure.data.xml`.

**Total deps removed (Phase A + B): 11.**

Deps that survive Phase B:
- `clj-jgit`, `org.eclipse.jgit/*` — used by `abc.git` (v0 support)
- `babashka/fs` — used by v0 tools
- `com.ibm.icu/icu4j` — used by `abc.text` (v0 support)
- `charred` — used by v0 tools
- `commons-compress`, `org.tukaani/xz` — used by `abc.ndc` to read the
  checked-in compressed NDC resource
- `malli`, `m3` — used by v0 tools
- `telemere`, `telemere-slf4j` — used by v0 tools
- `jing`, `saxon` — used by v0 TEI validation
- `aristotle`, `jena-shacl` — used by v0 RDF/SHACL
- `titanium-json-ld` — used by v0 Linked Art
- `tools.cli` — used by v0 CLIs
- `camel-snake-kebab` — used by v0 tools
- `babashka/process` — used by v0 tools
- `graalvm.js/js-language` — transitive for `m3`

### Phase B gate

Same as Phase A gate plus:
- `rg 'abc\.(aozora|tei|stats|annotation)[^.]' src/abc/tools/` returns no
  matches (the malli boundary leak is closed)
- `rg 'ns-loadable\?' src/` returns no matches
- `deps.edn` contains no direct `regal`, `timbre`, `xforms`, `core.match`,
  `claypoole`, `parallel`, `expound`, `java-time`, `tawny-owl`,
  `owlapi-distribution`, or `clj-commons/fs` dependency. `deps-lock.json`
  may still contain transitive entries pulled by aliases or active tooling.

---

## Combined Results

### Files deleted (21 total)

| Phase | Source files | Test files |
|---|---|---|
| A.1 | 6 (wiki, wlsp, diff, web, rdf, relaxng) | 0 |
| A.2 | 1 (config) | 1 (git_test.clj) |
| A.3 | 1 (core) | 1 (core_test.clj) |
| A.4 | 1 (load) | 1 (load_test.clj) |
| A.5 | 1 (owl) | 1 (owl_test.clj) |
| B.1 | 4 (aozora, tei, stats, annotation) | 2 (aozora_test, annotation_test) |
| **Total** | **14 source** | **6 test** |

(An additional 11 deps.edn `:deps` entries are removed across both phases.)

### Files modified (8 total)

| Phase | File | Change |
|---|---|---|
| A.2 | `src/abc/git.clj` | Remove config-dependent functions |
| A.2 | `test/abc/git_blob_test.clj` | Remove update-aozora-bunko-repo test |
| A.6 | `deps.edn` | Remove 6 orphaned deps |
| A/B | `deps-lock.json` | Single clj-nix lock refresh after dependency removals |
| B.2 | `src/abc/tools/malli.clj` | Shrink project-namespaces, remove ns-loadable? |
| B.2 | `test/abc/tools/malli_test.clj` | Remove legacy `abc.aozora` registry expectations |
| B.1 | `test/abc/text_test.clj` | Remove legacy `abc.stats` assertion |
| B.3 | `deps.edn` | Remove 5 orphaned deps |

### Deps.edn `:deps` entries

| | Count |
|---|---|
| Before | ~32 |
| After | 21 |
| Removed | 11 (6 in Phase A, 5 in Phase B) |

### Source files remaining in `abc/src/`

| | Count |
|---|---|
| Before | 54 |
| After | 40 (all v0-contracted) |

---

## Risks

1. **`abc.git` function removal.** The config-dependent functions
   (`load-aozora-bunko-git`, etc.) are verified to have zero callers in
   `abc.tools.*`. If a script or external tool calls them, that's a bug in
   the script, not in this cleanup. The git tag preserves them for recovery.

2. **Malli registry merge.** After shrinking to `'[abc.annotation.schema]`,
   the `compose-project-registry` reduces over a single element. If a future
   namespace adds registry schemas, it must be added to the list. This is
   documented in the spec.

3. **Kaocha auto-discovery.** Kaocha discovers tests by `-test$` namespace
   pattern. Deleting test files removes them from discovery automatically. No
   Kaocha configuration change is needed.

4. **`abc.annotation/schema.clj` vestigial keys.** The `:abc.stats/*` and
   `:document/stats` keys in `abc.annotation.schema/registry` are keyword
   definitions, not requires. They reference no deleted namespace and are
   harmless to keep. No v0 tool validates against them, so they are inert
   registry entries. Removing them would be a separate aesthetic cleanup,
   not part of this plan.

---

## References

- Survey: project chat session 2026-07-07
- Legacy namespace plan: `docs/superpowers/plans/2026-04-29-legacy-namespaces-clj-nix.md`
- XTDB removal plan: `docs/superpowers/plans/2026-07-04-remove-xtdb.md`
- Architecture doc: `docs/architecture.md`
- Design survey: `docs/design-survey.md`
