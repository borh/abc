# Dependency and Code Quality Hardening Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use `superpowers:subagent-driven-development` for implementation slices with independent probes, and `superpowers:verification-before-completion` before each commit. Use `architecture-triage` before changing legacy namespace boundaries.

**Goal:** Keep the current green v0 design-bundle baseline while updating safe Nix/Clojure dependencies, classifying risky dependency jumps by measured blast radius, and turning code-quality work into an active-surface gate instead of a broad legacy cleanup sink.

**Architecture:** Three lanes with different trust levels:

1. **Baseline lane:** commit the current conservative dependency refresh and small active-surface code-quality fixes as one bounded maintenance change after re-running the gates.
2. **Probe lane:** run isolated dependency probes for CI actions, Jena/JSON-LD, Saxon, and legacy runtime stacks. A probe can graduate only when it preserves the relevant fixture/gate and records what changed.
3. **Quality lane:** gate the active v0 tool surface first, then disposition or quarantine legacy namespaces before making legacy lint failures blocking.

**Ordering edges:** Task 1 lands before every other task so the current maintenance diff stays reviewable. Task 2 is independent after Task 1. Task 4 blocks the XTDB part of Probe D. Whole-repo lint expansion in Task 6 waits for the active-surface gate in Task 3 and the legacy-boundary decision in Task 6.

**Tech Stack:** Nix flakes, `deps.edn`, `clj-nix`, Kaocha, clj-kondo, Antq, GitHub Actions, ABC v0 design bundle.

---

## Design Findings

The current maintenance slice has already shown the right direction:

- Clojure and several low-risk libraries can move forward while `nix run .#validate-design-bundle`, full `nix flake check`, and Kaocha remain green.
- `src/abc/stats.clj` had a real `string/split` regression that was worth fixing even though the namespace is partly legacy-adjacent.
- `src/abc/xtdb.clj` starts RocksDB on namespace load via `(defonce node (start!))`. That is the clearest code-quality smell found so far because requiring `abc.load` can start stateful storage under `data/dev` and create lock contention.
- `abc.load` braids metadata loading, parser-era text extraction, XTDB persistence, and comments/dormant code. It should not be made more central while parser work is moving to `../ab-validator`.
- Most remaining clj-kondo findings are not equally valuable. Active `abc.tools.*` findings should be fixed or gated first; broad legacy findings need a disposition decision so the repo does not accumulate a large, permanent lint waiver.
- `.github/workflows/validation.yml` still stubs `local-pkgs` as `{ pkgs ? null }: {}` while `flake.nix` now imports it with `craneLib`. The CI stub should be a committed, locally evaluable Nix file that accepts `craneLib ? null, ...` before this maintenance branch is called complete.
- Antq reports major or milestone updates for XTDB, Jena, Saxon, GraalJS, Tawny, and Titanium. Those are not one class of work. Some are active publication-surface dependencies; others are legacy/runtime experiments.

## Dependency Classification

| Dependency | Current target | Classification | Next action |
| --- | --- | --- | --- |
| `actions/checkout` | `v4` -> `v7` | Safe CI maintenance, but verify workflow semantics | Upgrade in its own commit after fixing the `local-pkgs` stub. GitHub says v7 is generally available and changes unsafe fork checkout behavior for `pull_request_target` / `workflow_run`. This repo uses `pull_request`, so the expected impact is low. |
| `org.clojure/clojure`, `babashka/fs`, `charred`, JGit, `m3` | Already refreshed in worktree | Baseline maintenance | Commit with the verified green gates and the small regression fixes. |
| `org.apache.jena/jena-shacl` | `5.3.0` -> `6.1.0` | Active LOD/SHACL surface, major runtime constraint | Probe separately. Jena 6 requires Java 21+, so the probe must record local, Nix, and CI Java runtime policy, then run LOD/SHACL fixture gates. |
| `com.apicatalog/titanium-json-ld` | `1.7.0` -> `2.0.0-M2` | Active LOD surface, milestone release | Do not casually adopt the milestone. Probe only with JSON-LD fixture byte/digest comparison and external-context refusal checks from ADR 0013. |
| `net.sf.saxon/Saxon-HE` | `9.6.0-4` -> reported `13.0` | TEI/Schematron surface, but repo already uses Nix `saxon-he` for ODD generation | Do not jump directly to 13. First confirm whether this Maven dependency is loaded by live Clojure code. If it is live, probe SaxonJ 12.9 before 13; if not, remove or quarantine the unused dependency. |
| `com.xtdb/xtdb-*` | `1.24.5` -> `2.x` | Legacy/runtime experiment, not v0 identity surface | Defer. XTDB 2 changes node configuration and storage/log model; first remove load-time DB startup or decide legacy disposition. |
| `uk.org.russet/tawny-owl` | `2.3.3` -> `3.0.0` | Legacy OWL namespace | Defer until `abc.owl` disposition. Do not let an ontology DSL upgrade drive v0 maintenance. |
| `org.graalvm.js/js-language` | `24.2.2` -> `25.1.3` | Indirect/legacy JavaScript runtime | Defer unless a current v0 command uses it. GraalJS 25.1 has user-observable ECMAScript/runtime changes, so it needs a consumer-specific test. |

## Acceptance Criteria

- The baseline dependency/code-quality maintenance commit preserves:
  - `ABC_TEI_SCHEMA_SKIP=1 bin/kaocha`
  - `nix run .#validate-design-bundle`
  - `nix flake check --print-build-logs`
  - `git diff --check`
- CI validation can evaluate with a committed empty `local-pkgs` override after `flake.nix` started passing `craneLib`.
- Antq remaining drift is documented as a classified queue, not treated as failed maintenance.
- A new active-surface lint command exists or is documented. Green means zero `--fail-level error` findings on the listed active paths; warnings are visible but non-blocking in this first gate.
- No dependency probe is merged only because it evaluates. It must run the gate that exercises the dependency's actual behavior.

---

## Task 1: Commit the Current Green Baseline

**Files already touched:**
- `deps.edn`
- `deps-lock.json`
- `flake.nix`
- `flake.lock`
- `nix/clj-nix-deps.edn`
- `src/abc/load.clj`
- `src/abc/stats.clj`
- `src/abc/tools/aozora_ingest.clj`
- `test/abc/text_test.clj`
- `test/abc/tools/validate_design_bundle_test.clj`

- [ ] **Step 1: Review the diff as one maintenance slice**

Run:

```bash
git diff -- deps.edn deps-lock.json flake.nix flake.lock nix/clj-nix-deps.edn src/abc/load.clj src/abc/stats.clj src/abc/tools/aozora_ingest.clj test/abc/text_test.clj test/abc/tools/validate_design_bundle_test.clj
```

Confirm the diff contains only:

- conservative dependency refreshes,
- the `crane` input and `localPkgsOverlay`,
- `abc.stats` lazy MeCab resolution plus the `string/split` regression fix,
- no-op/unused require cleanup,
- TEI skip assertions that make skipped tests explicit.

- [ ] **Step 2: Re-run the baseline gates**

Run:

```bash
ABC_TEI_SCHEMA_SKIP=1 bin/kaocha
nix run .#validate-design-bundle
nix flake check --print-build-logs
git diff --check
```

- [ ] **Step 3: Capture remaining dependency drift**

Run:

```bash
clojure -M:update
```

Expected: non-zero while Antq still reports the classified major/milestone drift. Record the output in the commit message body or in the next task's notes, but do not mix those upgrades into this baseline commit.

- [ ] **Step 4: Commit only the baseline files**

Do not add unrelated untracked planning docs unless they belong to this maintenance slice.

```bash
git add deps.edn deps-lock.json flake.nix flake.lock nix/clj-nix-deps.edn src/abc/load.clj src/abc/stats.clj src/abc/tools/aozora_ingest.clj test/abc/text_test.clj test/abc/tools/validate_design_bundle_test.clj
git commit -m "chore: refresh baseline deps and tighten active tests"
```

---

## Task 2: Repair CI/Nix Maintenance Surface

**Files:**
- Modify: `.github/workflows/validation.yml`
- Create: `nix/ci-empty-local-pkgs/default.nix`

- [ ] **Step 1: Extract the CI `local-pkgs` stub into a real file**

Create `nix/ci-empty-local-pkgs/default.nix`:

```nix
{ pkgs ? null, craneLib ? null, ... }:
{}
```

Then update `.github/workflows/validation.yml` to stop writing an inline Nix file and instead pass:

```bash
--override-input local-pkgs path:$GITHUB_WORKSPACE/nix/ci-empty-local-pkgs
```

Rationale: `flake.nix` now imports `local-pkgs` with both `pkgs` and `craneLib`. A committed stub makes the CI boundary locally evaluable instead of verifying a different temp file than the one in the workflow.

- [ ] **Step 2: Upgrade checkout in the same CI-maintenance commit**

Change:

```yaml
- uses: actions/checkout@v4
```

to:

```yaml
- uses: actions/checkout@v7
```

This repo's workflow uses `pull_request`, not `pull_request_target`, so the v7 fork-checkout hardening should not change normal PR validation behavior.

- [ ] **Step 3: Verify CI-equivalent validation locally**

Run:

```bash
nix-instantiate --parse nix/ci-empty-local-pkgs/default.nix >/dev/null
nix run --override-input local-pkgs path:$(pwd)/nix/ci-empty-local-pkgs .#validate-design-bundle
```

Then run:

```bash
nix flake check --print-build-logs
```

- [ ] **Step 4: Commit**

```bash
git add .github/workflows/validation.yml nix/ci-empty-local-pkgs/default.nix
git commit -m "ci: refresh checkout and local-pkgs stub"
```

---

## Task 3: Establish an Active-Surface Lint Gate

**Files:**
- Create or modify: `.clj-kondo/config.edn`
- Optional create: `bin/lint-active`
- Optional modify: `flake.nix`

- [ ] **Step 1: Define the first lint target as the active v0 surface**

Start with this contract:

```bash
nix run nixpkgs#clj-kondo -- --lint src/abc/tools test/abc/tools --fail-level error
```

Do not include all `src` and `test` yet. The whole-repo run currently mixes active findings with legacy parser/XTDB/OWL findings and produces too much noise to be a useful gate.

Green means:

- zero error-level findings on exactly `src/abc/tools` and `test/abc/tools`,
- warning-level findings are tracked but non-blocking,
- the path list changes only in a commit that re-runs this lint gate and records the new scope.

- [ ] **Step 1a: Record the initial active-surface lint count**

Run:

```bash
nix run nixpkgs#clj-kondo -- --lint src/abc/tools test/abc/tools --fail-level warning || true
```

Record the initial error/warning counts in the commit body or a short note in this plan if the count is still non-zero before fixes.

- [ ] **Step 2: Fix real active-surface findings**

Prefer code fixes for:

- unresolved namespaces that are real missing requires,
- arity errors,
- unused imports/requires in active `abc.tools.*`,
- redundant bindings in new parser-IR publication modules.

Use `.clj-kondo/config.edn` only for intentional macros or known analyzer limits. The custom `schema-valid` test assertion is a candidate for config or a small test-helper shape change; do not silence unrelated lint categories globally.

- [ ] **Step 3: Add a repeatable command**

Either add a script:

```bash
bin/lint-active
```

that runs the active-surface command above, or add a flake check named `active-clj-kondo` once the command is stable.

- [ ] **Step 4: Verify**

Run:

```bash
bin/lint-active
ABC_TEI_SCHEMA_SKIP=1 bin/kaocha
nix run .#validate-design-bundle
nix flake check --print-build-logs
```

- [ ] **Step 5: Commit**

```bash
git add src/abc/tools test/abc/tools
# Add these only if this task created or modified them:
# git add .clj-kondo/config.edn bin/lint-active flake.nix
git commit -m "chore: add active-surface lint gate"
```

Only add files that actually changed.

---

## Task 4: Remove Load-Time XTDB Startup

**Files:**
- Modify: `src/abc/xtdb.clj`
- Modify: `src/abc/load.clj`
- Modify or create focused tests under `test/abc`

- [ ] **Step 1: Characterize the current side effect**

Write a focused test or small probe proving that requiring `abc.load` currently reaches `abc.xtdb/node`. This may be a documentation/probe step if the current behavior cannot be tested without starting RocksDB.

- [ ] **Step 2: Replace eager `node` with explicit lifecycle or retryable state**

Preferred shape:

- keep `start!` and `stop!` as explicit lifecycle functions,
- make public DB functions accept explicit nodes for active or tested paths,
- remove default-node use from `abc.load` paths that can reasonably receive a node from callers/tests,
- if a default node is retained for legacy REPL convenience, make it retryable rather than a `delay`,
- do not start RocksDB during namespace require.

Legacy-convenience fallback shape:

```clojure
(defonce default-node (atom nil))

(defn node []
  (or @default-node
      (locking default-node
        (or @default-node
            (reset! default-node (start!))))))
```

If `start!` throws, the failure must not be cached for the rest of the JVM process. Do not use `(delay (start!))` for this default.

- [ ] **Step 3: Keep this behavior-preserving**

Do not migrate XTDB 1 to XTDB 2 in this task. The goal is to remove load-time state and lock contention, not to redesign storage.

- [ ] **Step 4: Verify**

Run:

```bash
ABC_TEI_SCHEMA_SKIP=1 bin/kaocha
nix run .#validate-design-bundle
nix flake check --print-build-logs
```

If a focused legacy test needs RocksDB, run it explicitly outside the v0 gate and record that it remains legacy-only.

- [ ] **Step 5: Commit**

```bash
git add src/abc/xtdb.clj src/abc/load.clj test/abc
git commit -m "chore: avoid XTDB startup on require"
```

---

## Task 5: Run Dependency Probes One Lane at a Time

Each probe gets its own branch or commit. Do not combine these.

A probe that fails any required behavior gate is reverted in full. Delete the failed probe branch or reset the probe commit, then record the blocker in the dependency-classification table as the new next action.

### Probe A: Jena 6.1.0

- [ ] Record Java runtime versions and CI Java policy:

```bash
java -version
nix develop -c java -version
rg -n "setup-java|java-version|JAVA_HOME|runs-on" .github/workflows
```

- [ ] If the workflow relies on the runner default Java, either verify that default is Java 21+ in the PR run evidence or add an explicit `actions/setup-java` step with `java-version: '21'` before graduating the Jena 6 probe.
- [ ] Update only `org.apache.jena/jena-shacl`.
- [ ] Regenerate locks.
- [ ] Run the LOD/SHACL-relevant tests and full gates:

```bash
bin/kaocha --focus abc.tools.shacl-test
bin/kaocha --focus abc.tools.manifest-to-rdf-test
nix run .#validate-design-bundle
nix flake check --print-build-logs
```

- [ ] Accept only if fixtures and gates pass under Java 21+; otherwise revert the probe and record the blocker.

### Probe B: Titanium JSON-LD 2.0.0-M2

- [ ] Update only `com.apicatalog/titanium-json-ld`.
- [ ] Run ADR 0013-sensitive validation:

```bash
bin/kaocha --focus abc.tools.linked-art-test
nix run .#validate-design-bundle
```

- [ ] Compare generated Linked Art JSON-LD fixture bytes/digests. Any byte change must be understood as a semantic publication-view change before merge.
- [ ] Confirm the context loader still refuses external network fetches.

### Probe C: Saxon Runtime Harness

- [ ] First determine whether `deps.edn`'s `net.sf.saxon/Saxon-HE` is loaded by any live Clojure code path.

Run:

```bash
rg -n "net.sf.saxon|s9api|TransformerFactory|Saxon|schematron" src test deps.edn
bin/kaocha --focus abc.tools.schematron-test
```

- [ ] If the Clojure Schematron path shells out to the Nix `saxon-he` binary or otherwise does not load `deps.edn`'s Saxon classes, the correct result is to remove or quarantine the unused Maven Saxon dependency rather than bump it.
- [ ] Do not jump straight from `9.6.0-4` to `13.0`.
- [ ] If the Maven Saxon dependency is live, first test SaxonJ 12.9 for the Clojure Schematron path, because the Nix TEI generation path already uses a modern Saxon and Saxonica identifies Saxon 12 as the stable production line. Re-confirm the stable Saxon line at execution time because Saxonica's "latest" page moves.
- [ ] Run:

```bash
bin/kaocha --focus abc.tools.schematron-test
bin/kaocha --focus abc.tools.tei-test
nix run .#validate-design-bundle
nix flake check --print-build-logs
```

- [ ] If behavior changes, record whether the change is in stylesheet execution, Schematron diagnostics, or XML serialization.

### Probe D: Legacy Runtime Dependencies

Do not start with version bumps. Start with disposition:

- `com.xtdb/xtdb-*` belongs to `abc.xtdb`/`abc.load`.
- `uk.org.russet/tawny-owl` belongs to `abc.owl`.
- `org.graalvm.js/js-language` belongs to the M3/GraalJS consumer path.

For each:

- [ ] identify the current command or test that exercises the dependency,
- [ ] decide whether that command is part of the v0 publication path,
- [ ] either quarantine/defer or add a legacy-only probe gate,
- [ ] only then attempt the dependency upgrade.

---

## Task 6: Decide Legacy Namespace Disposition Before Whole-Repo Lint

**Prior art:** `docs/superpowers/plans/2026-04-29-legacy-namespaces-clj-nix.md`

- [ ] **Step 1: Update the legacy namespace discovery with current facts**

At minimum, record:

- `abc.xtdb` starts stateful storage on require unless Task 4 has landed,
- `abc.load` depends on parser-era `abc.aozora` / `abc.annotation` and XTDB,
- `abc.stats` is now less eager about MeCab but still conceptually legacy-adjacent,
- active v0 tooling lives primarily under `abc.tools.*`.

- [ ] **Step 2: Choose a disposition**

Use one of the prior-plan options:

- status quo plus explicit boundary,
- compile-only legacy gate,
- quarantine under a legacy path,
- delete/archive after parser-IR publication replaces old code.

Recommended current default: status quo plus explicit boundary now, with quarantine/delete deferred until `../ab-validator` can produce parser-IR that drives TEI/plaintext publication here.

- [ ] **Step 3: Only then expand lint**

After disposition, run:

```bash
nix run nixpkgs#clj-kondo -- --lint src test --fail-level error
```

Classify findings into:

- active-surface defects,
- legacy defects to fix because they block load/compile,
- legacy defects to ignore because the namespace is quarantined or archived.

- [ ] **Step 4: Commit the disposition or expanded lint config**

Do not make a broad lint-suppression commit without the boundary decision.

---

## Not Next

- Do not upgrade XTDB 1 to XTDB 2 before removing or isolating load-time DB startup.
- Do not upgrade Tawny/OWL as part of v0 parser-IR publication work unless an active publication artifact depends on it.
- Do not make whole-repo clj-kondo blocking while legacy namespace disposition remains open.
- Do not use `nix flake check --no-build` as the final green claim for dependency upgrades. It is useful for evaluation smoke, but the verification claim needs full build execution.
- Do not couple `../ab-validator` parser implementation work to dependency cleanup here except at the parser-IR/AAT contract boundary.

## Source Notes

- GitHub Actions `checkout` v7 is generally available and hardens unsafe fork checkout patterns for `pull_request_target` / `workflow_run`: <https://github.blog/changelog/2026-06-18-safer-pull_request_target-defaults-for-github-actions-checkout/>
- Apache Jena 6 requires Java 21 or later: <https://jena.apache.org/download/>
- XTDB 2 configuration changed around named remotes and multi-database/log-storage layout: <https://docs.xtdb.com/ops/config.html>
- Saxonica identifies SaxonJ 12.x as the stable production line, with 12.9 released on 2025-09-12 at time of plan writing; re-check this moving page before executing Probe C: <https://www.saxonica.com/html/products/latest.html>
- GraalJS 25.1.3 includes user-observable ECMAScript/runtime changes: <https://github.com/oracle/graaljs/blob/master/CHANGELOG.md>
- clj-kondo supports command-line linting, classpath linting, dependency config import, and configurable failure levels: <https://cljdoc.org/d/clj-kondo/clj-kondo/CURRENT/doc/readme>
