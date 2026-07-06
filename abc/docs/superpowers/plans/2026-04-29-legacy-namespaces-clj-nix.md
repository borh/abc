# Legacy Namespaces Disposition Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Decide and document the disposition of the pre-v0 namespaces `abc.aozora`, `abc.tei`, `abc.stats` (and direct dependants like `abc.text`, `abc.ndc`, `abc.annotation*`, `abc.db`, `abc.git`, `abc.load`, `abc.owl`, `abc.rdf`, `abc.relaxng`, `abc.wiki`, `abc.wlsp`, `abc.xtdb`) relative to the v0 contract gate, then execute the chosen disposition so the focused-test alias (`nix/clj-nix-deps.edn`) and the flake checks reflect a single, coherent rule about what is part of v0 and what is not.

**Architecture:** Discovery + Decision (ADR 0019 — *Legacy namespace disposition*) + Execution. The plan deliberately stops at the ADR boundary if Option C (delete-and-archive) or Option B (compile-only gate) is chosen, because the execution shape diverges sharply between options. The execution sub-plan in Phase 3 is written for **all three** candidate dispositions; only the chosen one is run.

**Tech Stack:** Clojure deps.edn, clj-nix lockfile (`nix/clj-nix-deps.edn`, `bin/update-clj-nix-lock`), Nix flake checks (`clj-nix-focused-tests`, `contract-surface`), git history.

---

## Context — Why this needs an ADR before code

The naive read is "bring the legacy namespaces under the v0 contract gate" — i.e. add them to `nix/clj-nix-deps.edn` and the focused-test alias, run their tests in `nix flake check`. **That is not the right answer**, because:

1. **Parsing is moving external** (project memory, 2026-04-29). `abc.annotation/parse-text`, `abc.aozora` text-extraction, and the `abc.tei` document/serialization roundtrip are intentionally dormant. Their failing tests are not regressions; they are paused work waiting for an external parser-IR producer. Pulling them into `clj-nix-focused-tests` would make the gate red on purpose — the opposite of what the gate is for.
2. **Heavy native deps**: `abc.stats` requires `clj-mecab` (native MeCab via JNI), `abc.git` requires JGit, `abc.xtdb` requires XTDB + RocksDB. None of these are in `nix/clj-nix-deps.edn`. Adding them widens the v0 sandbox surface significantly and pulls in optional, non-portable native libraries.
3. **`abc.tei` depends on `abc.aozora`**, which depends on `abc.ndc` and `abc.annotation.schema`. The `abc.ndc` and `abc.text` and `abc.annotation-schema` namespaces are *already* in the focused-test alias — the rest is a tree of dormant code rooted at the parser.
4. **No identity-stability risk** in the legacy code: nothing in `abc.aozora`/`abc.tei`/`abc.stats` participates in `manifest_identity_object`, `metadata_record_hash`, `person_record_hash`, or any of the v0 hash inputs. They are operationally inert from the v0 contract's point of view.

So the real decision is *what to do with this code*, not *how to gate it*. The four candidates:

- **Option A — Status quo**: leave the legacy code in `src/`, leave its deps in `deps.edn` outside `nix/clj-nix-deps.edn`, leave its tests outside the focused-test alias. Document the boundary explicitly in an ADR. Cost: continued ambient drift risk (nothing prevents an `abc.tools.*` namespace from accidentally requiring `abc.aozora` and pulling a `clj-mecab` dep into the v0 surface).
- **Option B — Compile-only gate**: keep the source, add a *sibling* deps spec `nix/legacy-clj-nix-deps.edn` (and the lockfile clj-nix derives from it, `nix/legacy-deps-lock.json`) that carries only the deps the legacy namespaces transitively use, plus a flake check that **loads (requires) every legacy namespace** under that sibling lockfile. Tests stay out. The check fails if a legacy namespace acquires a dependency that isn't in the legacy lockfile (i.e. catches dep drift inside the legacy tree). The "compile-only" name is historical — the gate runs `clojure -M:legacy/compile-only`, an alias that calls `require` for every legacy namespace; it does NOT run `clojure.core/compile` and does not emit `*.class` files. Cost: a second deps spec + lockfile pair to maintain via `bin/update-clj-nix-lock` and a parallel `cljDepsCache` derivation in `flake.nix`. Note: this does NOT load legacy code "under the v0 lockfile" — that is impossible because the v0 lockfile deliberately omits `clj-mecab`, JGit, XTDB, etc. The gate uses a separate sandbox.
- **Option C — Quarantine**: move legacy code to `legacy/abc/` (own paths block, possibly own deps.edn alias). Removes the question entirely from v0's perspective: legacy code lives outside `:paths`. Cost: large file move, blame churn, every existing import path changes.
- **Option D — Delete-and-archive**: delete the legacy namespaces from `master`, preserve them in a tagged commit (`legacy-archive-pre-parser-ir`) for historical reference. Cost: irreversible without git surgery; only acceptable once the external parser is producing parser-IR JSON the v0 surface can consume end-to-end.

The decision matters because each option shapes the next several sprints: Option D unblocks deletion of `clj-mecab`/`clj-jgit`/`tawny-owl`/`xtdb-*` from `deps.edn` (a real simplification); Option B introduces a second flake check (more CI surface); Option C creates a one-off rename PR but leaves the deps tree intact.

## Cross-references

- Project memory: `parsing_external.md` — Aozora parsing moves external; Clojure parser is dormant; failing parser tests are intentional.
- ADR 0002 — Parser-IR contract.
- `docs/next-steps.md` — candidate item (legacy-namespace cleanup).
- `nix/clj-nix-deps.edn` — current v0 sandbox surface (deps + focused-test alias).
- `deps.edn` — full project deps including the legacy stack (`clj-mecab`, `clj-jgit`, `tawny-owl`, `owlapi-distribution`, `xtdb-core`, `xtdb-rocksdb`, `clj-commons/fs`, `babashka/fs`, `timbre`, `core.match`, `expound`, `parallel`, `xforms`, `claypoole`, `regal`).

---

## File Structure (Phase 1 + 2 — Discovery + ADR)

- Create: `docs/adr/0019-legacy-namespace-disposition.md` — the decision document.
- Create: `docs/superpowers/notes/2026-04-29-legacy-namespaces-discovery.md` — discovery scratchpad: namespace dependency tree, deps.edn → namespace mapping, test status table.
- Modify: `docs/next-steps.md` — drop the "legacy namespaces behind clj-nix" candidate; add a milestone entry referencing ADR 0019 and the chosen sub-plan.
- (Phase 3 only — file structure depends on the chosen option; see Phase 3 below.)

---

## Phase 1 — Discovery

### Task 1: Map the legacy namespace dependency tree

**Files:**
- Create: `docs/superpowers/notes/2026-04-29-legacy-namespaces-discovery.md`

- [ ] **Step 1: Enumerate the legacy namespaces and their direct (`:require`) deps**

Run:
```
rg -n '^\(ns abc\.' src/abc | sort
rg -n '^\s+\[abc\.' src/abc/aozora.clj src/abc/tei.clj src/abc/stats.clj src/abc/text.clj src/abc/ndc.clj src/abc/annotation.clj src/abc/load.clj src/abc/db.clj src/abc/git.clj src/abc/owl.clj src/abc/rdf.clj src/abc/relaxng.clj src/abc/wiki.clj src/abc/wlsp.clj src/abc/xtdb.clj
```

Record in the discovery note as a table:

| Namespace | LOC | Direct intra-`abc.*` deps | Direct external deps (mvn/git) | In `nix/clj-nix-deps.edn`? | Test file | Test status (per memory) |
| --- | --- | --- | --- | --- | --- | --- |

Mark each test row as one of: **PASSING-IN-FOCUSED** (already wired into the focused alias), **DORMANT** (matches the failing-on-purpose set in the parser-external memory: `parse-aozora-text`, `aozora2html-test`, `extract-texts-test`, `tei-test/document-test`, `serialization-roundtrip-test`, `db-test/query-test`), or **UNKNOWN** (unverified — flag for Step 2).

- [ ] **Step 2: Verify dormant-test claims with a one-shot run outside the gate**

Run (do **not** add to focused alias):
```
clojure -M:test -e "(require 'clojure.test 'abc.aozora-test 'abc.tei-test) (clojure.test/run-tests 'abc.aozora-test 'abc.tei-test)"
```
Expected: each test categorised in the table is confirmed PASSING/DORMANT. No surprises. If a test the memory called dormant now passes, or vice-versa, update the memory note as part of this step.

- [ ] **Step 3: Build the deps.edn → legacy-namespace attribution**

For each line in `deps.edn` that is **not** in `nix/clj-nix-deps.edn`, identify the namespaces that import it. Record as a table:

| `deps.edn`-only dep | Namespaces using it | Disposition impact |
| --- | --- | --- |

Example seed rows: `clj-mecab` → `abc.stats` only; `clj-jgit` → `abc.git` only; `tawny-owl`/`owlapi-distribution` → `abc.owl` only; `xtdb-core`/`xtdb-rocksdb` → `abc.xtdb`, `abc.db`; `timbre` → many; `core.match`/`regal` → `abc.aozora`; `parallel`/`xforms`/`claypoole` → `abc.stats`, `abc.text`?; `clj-commons/fs`, `babashka/fs` → ?

- [ ] **Step 4: Commit the discovery note**

```
git add docs/superpowers/notes/2026-04-29-legacy-namespaces-discovery.md
git commit -m "docs: discovery notes for legacy-namespace disposition"
```

---

### Task 2: Verify the gate boundary holds today

The current claim is "the legacy code is operationally inert from the v0 contract's point of view". Verify it.

**Files:** none modified; investigation only.

- [ ] **Step 1: Confirm no `abc.tools.*` namespace transitively requires legacy code (static + dynamic)**

Static requires:
```
rg -n '\[abc\.(aozora|tei|stats|owl|xtdb|db|git|wiki|wlsp|relaxng|rdf|load|annotation)' src/abc/tools test/abc/tools
```
Dynamic requires (the symbol-list / `requiring-resolve` / `(require '...)` patterns):
```
rg -n "'abc\.(aozora|tei|stats|owl|xtdb|db|git|wiki|wlsp|relaxng|rdf|load|annotation)" src/abc/tools test/abc/tools
```

Expected hits — these are **known boundary cases**, not accidents:

| Location | What it does | Classification |
| --- | --- | --- |
| `src/abc/tools/malli.clj:22` | `'[abc.annotation.schema abc.aozora abc.tei]` — declared registry-merge list. The accompanying `ns-loadable?` defensively skips a namespace whose `require` fails so the focused-test sandbox still works when `abc.aozora`/`abc.tei` deps are absent. | **Boundary leak by design.** The v0 sandbox tolerates the missing namespaces (the `require` returns false), but the merge list itself names legacy namespaces. ADR 0019 must state explicitly whether this is acceptable (Option A) or whether the legacy registry sources should be split out (call this the **registry-split** sub-decision; see Task 3 Step 3a below). |
| `src/abc/tools/aozora_csv.clj` / `src/abc/tools/aozora_ingest.clj` | Operate on Aozora data without invoking the legacy parser. Verify no `(:require [abc.aozora …])`. | Legitimate v0 reuse if the requires resolve to no legacy code; accidental coupling otherwise. |
| `src/abc/tools/tei.clj` / `src/abc/tools/tei_header.clj` / `src/abc/tools/schematron.clj` | v0 TEI tooling — the "tei" name is shared with `abc.tei` but these namespaces are independent v0 modules. Verify the `(:require)` blocks do not reach `abc.tei`. | Legitimate v0 reuse. |

For every hit not in the table, classify it explicitly in the discovery note: legitimate v0 reuse OR accidental coupling. Accidental couplings are blockers for ADR 0019 Option A and force at least Option B.

- [ ] **Step 2: Confirm the focused-test alias compiles without legacy deps**

Run:
```
nix flake check --no-build-logs |& tee /tmp/flake-check.log
```
Expected: `clj-nix-focused-tests` and `contract-surface` pass. (They already do — this step is a baseline before any disposition lands.)

- [ ] **Step 3: Record the boundary status in the discovery note**

Append a "Gate Boundary Status" section. Either: "boundary holds — legacy is invisible to v0" or "boundary leaks — accidental requires found at L1, L2, ...".

- [ ] **Step 4: Commit**

```
git add docs/superpowers/notes/2026-04-29-legacy-namespaces-discovery.md
git commit -m "docs: confirm legacy/v0 boundary status before disposition decision"
```

---

## Phase 2 — Decision (ADR 0019)

### Task 3: Draft ADR 0019 — Legacy namespace disposition

**Files:**
- Create: `docs/adr/0019-legacy-namespace-disposition.md`

- [ ] **Step 1: Write the ADR shell**

The ADR follows the same template as ADRs 0017/0018: `Status` (Draft), `Date` (2026-04-29), `Context`, `Decision`, `Hard Rule`, `Acceptance Criteria`, `Consequences`, `References`.

- [ ] **Step 2: Fill in `Context`**

Summarise the four-option landscape (Status quo / Compile-only gate / Quarantine / Delete-and-archive) verbatim from this plan's "Context" section above. Anchor the discussion in:
- the parser-external project memory (paste the rule + Why),
- the boundary-status finding from Task 2 (boundary holds vs. leaks — pasted from the discovery note),
- the deps.edn → namespace attribution table from Task 1.

The Context must be self-contained: a future reader who has not seen this plan should be able to evaluate the four options from the ADR alone.

- [ ] **Step 3: Fill in `Decision`**

This is the human-judgement step. Pick one of the four options. Default recommendation if no other input: **Option A (Status quo) + an explicit ADR boundary statement**, because it is the cheapest and lowest-risk move that closes the open question. Promote to Option B (compile-only gate) only if Task 2 found accidental coupling; promote to Option C (quarantine) only if a near-term sprint expects to ingest parser-IR end-to-end and wants legacy out of `src/` before that work; promote to Option D (delete) only with explicit user sign-off, because it is destructive.

For whichever option is chosen, write the Decision body in the same shape as ADR 0018: a short prose statement, then a bulleted list of concrete artifacts that change vs. stay byte-identical.

- [ ] **Step 3a: Sub-decision — registry-split for `abc.tools.malli`**

`src/abc/tools/malli.clj:22` names `abc.annotation.schema`, `abc.aozora`, `abc.tei` in its `project-namespaces` list and uses `ns-loadable?` to skip the ones whose deps the focused-test sandbox does not provide. This is a known boundary leak — the v0 namespace `abc.tools.malli` references legacy namespaces by name, even though it tolerates their absence at runtime. The ADR must answer:

- **Sub-A (accept the soft reference)**: keep the list as-is. Rationale: the `ns-loadable?` defence already isolates the sandbox; renaming would churn an unrelated file. Cost: an `abc.tools.malli` reader sees legacy names and may reasonably wonder if they are part of v0.
- **Sub-B (split the registry)**: introduce `abc.tools.malli-v0` (registry sources = `abc.annotation.schema` only) and `abc.tools.malli-legacy` (registry sources = `abc.aozora`, `abc.tei`). The focused-test alias loads only `-v0`; legacy loaders use `-legacy`. Cost: one new file, one rename, every test that calls `install!` updates its require. Benefit: the v0 surface no longer textually references legacy namespaces.

Recommended default: **Sub-A** for Option A or B (the soft reference is harmless when the boundary is otherwise clean), **Sub-B** for Option C or D (because relocating/deleting `abc.aozora` and `abc.tei` would require updating `abc.tools.malli` anyway, and a clean split is cheaper than guarding the require list with conditionals).

Whichever sub-decision is taken, record it explicitly in the ADR Decision section and reflect it in the Phase 3 task list (an additional sub-task for Sub-B).

- [ ] **Step 4: Fill in `Hard Rule`**

For Option A: "JSON contracts unchanged. `nix/clj-nix-deps.edn` unchanged. No flake check added or removed. The ADR is the boundary statement: `abc.aozora`, `abc.tei`, `abc.stats` and their dependants are pre-v0 code retained for historical and out-of-band-tooling use; their tests are not part of the v0 contract gate."

For Option B: "JSON contracts unchanged. A new flake check `legacy-namespace-compile` is introduced; it consumes a sibling lockfile `nix/legacy-clj-nix-deps.edn` and verifies the legacy namespaces compile but does not run their tests. No schema hashes rotate."

For Option C: "JSON contracts unchanged. Dormant source files relocate from `src/abc/<name>.clj` to `legacy/src/abc/<name>.clj` (and matching test moves to `legacy/test/abc/`); `deps.edn` `:paths` adds `\"legacy/src\"` and the `:test` alias's `:extra-paths` adds `\"legacy/test\"`. PASSING-IN-FOCUSED namespaces (per the Task 1 discovery table — `abc.text`, `abc.ndc`, `abc.annotation.schema`, anything in `nix/clj-nix-deps.edn`'s focused-test alias) **stay in `src/`** and are NOT moved. No schema hashes rotate. The git move preserves blame via `git mv` (verify with `git log --follow legacy/src/abc/aozora.clj`)."

For Option D: "JSON contracts unchanged. A pre-deletion tag `legacy-archive-pre-parser-ir` is pushed before the delete commit. Affected `deps.edn` entries (`clj-mecab`, `clj-jgit`, `tawny-owl`, `owlapi-distribution`, `xtdb-*`, `timbre`, `core.match`, `regal`, `parallel`, `xforms`, `claypoole`, `clj-commons/fs`, `babashka/fs`, `expound`) are removed in the same commit. Schema hashes do not rotate."

- [ ] **Step 5: Fill in `Acceptance Criteria`**

For all options: `nix flake check` passes; `manifest.json` is byte-identical to the pre-disposition commit; `metadata_record_hash`, `person_record_hash`, `manifest_identity_object` are unchanged.

For Option B specifically: the new check FAILS when an artificial test-only commit makes a legacy namespace require a dep that is NOT in `nix/legacy-clj-nix-deps.edn` (compile-time `FileNotFoundException`); the check PASSES when an artificial test-only commit removes a previously-required dep from a legacy namespace while leaving it in the lockfile (unused-but-present is acceptable). I.e. the gate catches *missing-dep drift*, not unused-dep drift, and that is sufficient for the goal — preventing a legacy namespace from acquiring an undeclared dependency.

- [ ] **Step 6: Fill in `Consequences`**

Per option, what becomes harder/easier next sprint. Cite the next-sprint impact for each (Option D unlocks dep removal; Option C unlocks `src/` clarity; Option B adds a CI check; Option A defers the question).

- [ ] **Step 7: Mark Status: Accepted, set Accepted date, commit**

```
git add docs/adr/0019-legacy-namespace-disposition.md
git commit -m "feat: ADR 0019 — legacy namespace disposition (chosen: <option>)"
```

The commit message MUST name the chosen option.

---

## Phase 3 — Execution

The execution path branches on the option chosen in ADR 0019. **Execute only the section matching the chosen option.** Each section is self-contained.

### Phase 3-A — If Option A (Status quo) is chosen

**Files:**
- Modify: `docs/next-steps.md` — replace the candidate item with a milestone entry citing ADR 0019.
- Optionally: a one-line comment near the top of `nix/clj-nix-deps.edn` pointing at ADR 0019 ("v0 surface; legacy namespaces are intentionally excluded — see ADR 0019").

#### Task A1: Update next-steps + add the lockfile pointer

- [ ] **Step 1: Drop the candidate, add the milestone, bump the count**

Open `docs/next-steps.md`. Find the "legacy namespaces behind clj-nix" candidate (currently candidate item; the exact wording is what was carried forward from the previous next-steps refresh — verify against `git log -p -- docs/next-steps.md | head -60`). Remove that bullet. Renumber the remaining candidates. In the milestone log section, add:

```markdown
- **2026-04-29 — Legacy-namespace disposition resolved (ADR 0019).** The
  pre-v0 `abc.aozora`, `abc.tei`, `abc.stats` (+ dependants) are
  explicitly excluded from the v0 contract gate. They remain in `src/`
  for historical and out-of-band-tooling reference. Failing tests in
  those namespaces are not regressions (per parser-external memory).
```

Bump the recent-milestones count word by exactly one. Read the current count word at the top of `docs/next-steps.md` first — the TEI rule expansion plan also bumps this same line, so the starting value depends on landing order. Read, increment by one, write. Do not hard-code a target.

- [ ] **Step 2: Add the boundary-pointer comment to `nix/clj-nix-deps.edn`**

```clojure
;; v0 contract surface. Legacy namespaces (abc.aozora, abc.tei, abc.stats,
;; abc.owl, abc.xtdb, abc.db, abc.git, abc.load, abc.wiki, abc.wlsp,
;; abc.relaxng, abc.rdf, abc.annotation*) are intentionally excluded —
;; see ADR 0019.
{:paths ...}
```

- [ ] **Step 3: Verify**

Run:
```
nix flake check --no-build-logs
```
Expected: all checks pass; nothing changed semantically.

- [ ] **Step 4: Commit**

```
git add docs/next-steps.md nix/clj-nix-deps.edn
git commit -m "docs: drop legacy-namespace candidate, add ADR 0019 milestone"
```

---

### Phase 3-B — If Option B (Compile-only gate) is chosen

**Files:**
- Create: `nix/legacy-clj-nix-deps.edn` — sibling **deps spec** (parallel to `nix/clj-nix-deps.edn`) carrying the legacy-namespace deps + a `:legacy/compile-only` alias that **loads (requires) every legacy namespace** so that `clj-mecab.parse` JNI binding, `clj-jgit` git loaders, etc. resolve at require-time. The alias does NOT run `clojure.core/compile` (that would emit `*.class` files for AOT) — it runs `(require '[abc.aozora …])` only, which exercises the dep graph and surfaces missing deps as `FileNotFoundException` / `ClassNotFoundException`. The earlier "AOT-compiles" wording was misleading; the gate is a require-time smoke check, not Clojure AOT.
- Create: `nix/legacy-deps-lock.json` — the lockfile clj-nix's `deps-lock` tool derives from the deps spec above. Path is consumed by the `legacyCljDepsCache = mk-deps-cache { lockfile = ./nix/legacy-deps-lock.json; }` binding in the new flake derivation (Phase 3-B Task B1 Step 3). Generated by `bin/update-clj-nix-lock` — never hand-edit.
- Modify: `flake.nix` — add a new derivation `legacy-namespace-compile` and wire it into `checks.<system>.legacy-namespace-compile`.
- Modify: `bin/update-clj-nix-lock` — extend to update both lockfiles (or document the second invocation).
- Modify: `docs/next-steps.md` — milestone entry citing ADR 0019.

#### Task B1: Create the sibling lockfile

- [ ] **Step 1: Author `nix/legacy-clj-nix-deps.edn`**

```clojure
{:paths ["resources" "src"]
 :deps {org.clojure/clojure {:mvn/version "1.12.4"}
        ;; ... copy the deps.edn entries needed by the legacy namespaces
        ;; (per the discovery table in Task 1, Step 3) ...
        clj-mecab/clj-mecab {:mvn/version "1.0.102"}
        clj-jgit/clj-jgit {:mvn/version "1.1.0"
                           :exclusions [org.eclipse.jgit/org.eclipse.jgit.gpg.bc]}
        ;; tawny-owl, owlapi-distribution, xtdb-core, xtdb-rocksdb,
        ;; timbre, core.match, regal, parallel, xforms, claypoole,
        ;; clj-commons/fs, babashka/fs, expound — copy from deps.edn
        }
 :aliases
 {:legacy/compile-only
  {:main-opts
   ["-e"
    "(require 'abc.aozora 'abc.tei 'abc.stats 'abc.text 'abc.ndc 'abc.annotation 'abc.annotation.schema 'abc.db 'abc.git 'abc.load 'abc.owl 'abc.rdf 'abc.relaxng 'abc.wiki 'abc.wlsp 'abc.xtdb)"]}}}
```

- [ ] **Step 2: Run the lockfile generator for the sibling file**

The clj-nix `deps-lock` tool writes its output to `deps-lock.json` in the current working directory by default; it does not honour the input filename. Two ways to land the output at the path Step 3 consumes (`nix/legacy-deps-lock.json`):

```bash
## Option A: redirect via stdout (preferred — no temp file).
nix run github:jlesquembre/clj-nix#deps-lock -- \
  --deps-include nix/legacy-clj-nix-deps.edn \
  --lockfile nix/legacy-deps-lock.json

## Option B: run the generator and rename if --lockfile is not supported
## by the pinned clj-nix version. Verify in the deps-lock --help output
## first; if the flag is absent, fall back to:
nix run github:jlesquembre/clj-nix#deps-lock -- \
  --deps-include nix/legacy-clj-nix-deps.edn
mv deps-lock.json nix/legacy-deps-lock.json
```

Expected: `nix/legacy-deps-lock.json` exists at the path Step 3's `legacyCljDepsCache` consumes (`./nix/legacy-deps-lock.json`) and Step 7's `git add` stages.

The matching update to `bin/update-clj-nix-lock` (Step 5) must use the same flag/rename pattern so the script and the manual recipe stay aligned.

- [ ] **Step 3: Add a flake derivation `legacy-namespace-compile`**

The existing `clj-nix-focused-tests` (see `flake.nix` around line 238) is built with `pkgs.runCommand`, not `mkCljApp`. It composes a Maven dep cache via `mk-deps-cache` (the helper from the `clj-nix` overlay that this flake already uses) and runs `clojure` from inside the sandbox. The new derivation must follow the same shape — using `mkCljApp` in this codebase would diverge from the established pattern and would not have access to the same `cljDepsCache` plumbing.

Sketch (alongside `clj-nix-focused-tests` in `flake.nix`):

```nix
legacyCljDepsCache = pkgs.mk-deps-cache {
  lockfile = ./nix/legacy-deps-lock.json;
};

legacy-namespace-compile = pkgs.runCommand "abc-legacy-namespace-compile"
  {
    nativeBuildInputs = [
      pkgs.clojure
      pkgs.libxml2
      mecab            # provides libmecab.so for clj-mecab JNI bindings
      unidic           # the dictionary clj-mecab.parse loads at require-time
    ];
    LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath [ mecab ];
    MECABRC = "${mecab}/etc/mecabrc";
    MECAB_DICDIR = mecabDicDir;
  }
  ''
    cp -R ${./.} source
    chmod -R u+w source
    cd source
    cp ${./nix/legacy-clj-nix-deps.edn} deps.edn

    export HOME="${legacyCljDepsCache}"
    export JAVA_TOOL_OPTIONS="-Duser.home=${legacyCljDepsCache}"
    export CLJ_CONFIG="$HOME/.clojure"
    export CLJ_CACHE="$TMPDIR/cp-cache"
    export GITLIBS="$HOME/.gitlibs"

    ## Compile (load) the legacy namespaces; do not run their tests.
    ## A failure here means a legacy namespace requires a dep that is
    ## not in nix/legacy-clj-nix-deps.edn / nix/legacy-deps-lock.json.
    clojure -M:legacy/compile-only

    mkdir -p "$out"
    echo "ABC legacy namespaces compiled under legacy clj-nix lockfile." > "$out/result.txt"
  '';
```

Note on MeCab: `abc.stats` `(:require [clj-mecab.parse …])`, and `clj-mecab.parse` loads `libmecab.so` via JNI at require-time. The dev shell at `flake.nix:362` provides `mecab`, `unidic`, `MECABRC`, `MECAB_DICDIR`, and `LD_LIBRARY_PATH` so `clojure -M…` can require the namespace; the sandbox derivation must mirror that env, otherwise `clojure -M:legacy/compile-only` will throw `UnsatisfiedLinkError` on the very first require. The `mecab`, `unidic`, and `mecabDicDir` symbols above are the same `let`-bindings as `flake.nix:352`–`360` — the sketch assumes the let scope is shared. If the new derivation lives in a different scope, hoist or rebind those three names inside it.

Wire `legacy-namespace-compile` into `checks.<system>` next to `clj-nix-focused-tests` and `contract-surface`. **Verify the MeCab plumbing actually works** by running the sandboxed build once and confirming the compile succeeds; if it fails with `UnsatisfiedLinkError`, the MeCab env is incomplete (commonly `LD_LIBRARY_PATH` or `MECABRC` is wrong inside the sandbox vs. the dev shell).

Open question to confirm against the actual `clj-nix` overlay: the helper used in this flake to build a deps cache is named `mk-deps-cache`. Verify against the existing `cljDepsCache` binding in `flake.nix` (search the file for the `mk-deps-cache` or `mkCljDeps` symbol — whichever name `clj-nix.overlays.default` exposes here). If the name differs, adjust the snippet to match; do not invent a new name.

- [ ] **Step 4: Verify the check actually catches dep drift**

Make a synthetic commit on a throwaway branch that **removes the `(:require [clj-mecab.parse …])` from `abc.stats`** but leaves `clj-mecab` listed in `nix/legacy-clj-nix-deps.edn`. Re-run `bin/update-clj-nix-lock` (the single extended script per Step 5 below) then `nix flake check`. Expected: `legacy-namespace-compile` **passes** (the dep is unused but present — that is fine). Discard the throwaway commit.

Then make a *second* throwaway commit that **adds a fresh `(:require [some-new-fake/missing-dep …])` to `abc.stats`** without adding the dep to the legacy lockfile. Run `nix flake check`. Expected: `legacy-namespace-compile` **fails** (compile-time `FileNotFoundException` for the missing dep). Discard the throwaway commit.

The first sub-step proves the check accepts unused-but-present deps; the second proves it rejects missing-but-required deps. The original phrasing of this step in an earlier draft of this plan inverted these expectations — the corrected logic above is what the gate actually exercises.

- [ ] **Step 5: Extend `bin/update-clj-nix-lock` to update both lockfiles in one invocation**

Commit to a single script, not a sibling. The script must regenerate the existing v0 lockfile **and** the new legacy sibling on every invocation, so callers don't need to remember a second command. The actual paths in the repo (verified at the time of this plan):

- v0 lockfile: `./deps-lock.json` at repo root (consumed by `flake.nix:187` via `lockfile = ./deps-lock.json` and again at `flake.nix:274`). The current `bin/update-clj-nix-lock` (`bin/update-clj-nix-lock:5`) is a one-liner that runs `nix run github:jlesquembre/clj-nix#deps-lock -- --deps-include nix/clj-nix-deps.edn "$@"`, which writes `deps-lock.json` in the working directory (i.e. repo root when invoked from there).
- Legacy lockfile: `./nix/legacy-deps-lock.json` (the new sibling).

Replace the one-liner with:

```bash
## bin/update-clj-nix-lock — refresh both lockfiles
set -euo pipefail
cd "$(git rev-parse --show-toplevel)"

## v0 lockfile — writes deps-lock.json in CWD (repo root). Preserve the
## existing path because flake.nix:187 + flake.nix:274 reference it.
nix run github:jlesquembre/clj-nix#deps-lock -- \
  --deps-include nix/clj-nix-deps.edn

## Legacy sibling — write to nix/legacy-deps-lock.json explicitly.
nix run github:jlesquembre/clj-nix#deps-lock -- \
  --deps-include nix/legacy-clj-nix-deps.edn \
  --lockfile nix/legacy-deps-lock.json
```

(If `--lockfile` is unsupported by the pinned clj-nix version, fall back to the rename pattern from Phase 3-B Task B1 Step 2: run the second command without `--lockfile`, then `mv deps-lock.json nix/legacy-deps-lock.json` — but ONLY after the v0 step has finished, otherwise the v0 lockfile is overwritten before being consumed.)

The single-script choice means: there is exactly one `bin/update-clj-nix-lock` invocation in the README, the developer workflow, and the CI script — and Step 4 verification and Step 7 staging both reference that single script. **A sibling `bin/update-clj-nix-legacy-lock` is rejected** to avoid the failure mode of refreshing one lockfile and forgetting the other.

- [ ] **Step 6: Update `docs/next-steps.md`**

```markdown
- **2026-04-29 — Legacy-namespace compile-only gate (ADR 0019).** A new
  flake check `legacy-namespace-compile` consumes
  `nix/legacy-clj-nix-deps.edn` and verifies the legacy namespaces
  compile under the lockfile. Their tests remain out of the v0 contract
  gate.
```

- [ ] **Step 7: Verify and commit**

```
nix flake check --no-build-logs
git add deps-lock.json nix/legacy-clj-nix-deps.edn nix/legacy-deps-lock.json flake.nix bin/update-clj-nix-lock docs/next-steps.md
git commit -m "feat: legacy-namespace compile-only gate (ADR 0019)"
```

`deps-lock.json` (the v0 lockfile at repo root, per `flake.nix:187`) is staged because the extended `bin/update-clj-nix-lock` regenerates it alongside the new legacy lockfile. If the regeneration is a no-op (no input changed), git will simply not show it as modified — the `git add` is a safety net, not a guarantee of churn.

---

### Phase 3-C — If Option C (Quarantine) is chosen

**Files:**

- Move (via `git mv`): every legacy `src/abc/*.clj` → `legacy/src/abc/*.clj`. The exact list comes from Task 1's discovery table — files marked DORMANT plus their direct legacy-only dependants. Files marked PASSING-IN-FOCUSED (`abc.text`, `abc.ndc`, `abc.annotation.schema`, anything else listed in `nix/clj-nix-deps.edn`'s focused-test alias and `flake.nix:271`'s `contract-surface` block) **stay in `src/`** and are NOT moved.
- Move: every dormant test file → `legacy/test/abc/...`. Tests already in the focused-test alias (`ndc_test`, `text_test`, `annotation_schema_test`, etc.) **stay in `test/abc/`**.
- Modify: `deps.edn` — `:paths` gains `"legacy/src"`; the `:test` alias's `:extra-paths` gains `"legacy/test"`.
- Modify: `nix/clj-nix-deps.edn` — unchanged (the focused-test alias enumerates by namespace name, not file path; the relocation is invisible to it).
- Modify: every `(:require [abc.aozora ...] ...)` import — **none**, because Clojure path-resolves namespaces from each `:paths` root: with `"legacy/src"` in `:paths`, the file `legacy/src/abc/aozora.clj` still provides the namespace `abc.aozora`.
- Modify: `docs/next-steps.md` — milestone entry.

**Path/namespace mapping (worked example):**

| Before | After | Namespace |
| --- | --- | --- |
| `src/abc/aozora.clj` | `legacy/src/abc/aozora.clj` | `abc.aozora` (unchanged) |
| `src/abc/tei.clj` | `legacy/src/abc/tei.clj` | `abc.tei` (unchanged) |
| `test/abc/aozora_test.clj` | `legacy/test/abc/aozora_test.clj` | `abc.aozora-test` (unchanged) |

The `:paths` root for the namespace-to-file resolver is `legacy/src`, NOT `legacy`. Hence the directory layout is `legacy/src/abc/...`, NOT `legacy/abc/...`. An earlier draft of this plan was inconsistent on this; the layout above is the canonical one.

#### Task C1: Plan the move

- [ ] **Step 1: Build the exact move list**

From the Task 1 discovery note, list every `src/abc/*.clj` file flagged as legacy (not in the focused alias). For each, produce:

| Source path | Target path | Test moves with it? |
| --- | --- | --- |

This list goes into the discovery note as an appendix and is the input for Step 2.

#### Task C2: Execute the move

- [ ] **Step 2: `git mv` each source file**

```
mkdir -p legacy/src/abc legacy/test/abc
```

Then for each row in the table from Step 1 marked DORMANT (not PASSING-IN-FOCUSED):
```
git mv src/abc/<name>.clj legacy/src/abc/<name>.clj
```
And for each dormant test file (skip tests in the focused alias):
```
git mv test/abc/<name>_test.clj legacy/test/abc/<name>_test.clj
```

- [ ] **Step 3: Update `deps.edn`**

`deps.edn` currently has `:paths ["resources" "src"]` and a `:test` alias with `:extra-paths ["test"]` (verify against `deps.edn:1` and the `:test` alias around line 73). Update:

```clojure
{:paths ["resources" "src" "legacy/src"]
 ...
 :aliases
 {:test {:extra-paths ["test" "legacy/test"]
         ...}}}
```

The `legacy/src` path joins `:paths` so `(require 'abc.aozora)` continues to resolve during ad-hoc `clojure -M:test` runs from the project root. The flake's focused-test alias remains unchanged: it enumerates test namespaces by name (`'abc.tools.metadata-record-test`, etc.), not by file path, so it does not see `legacy/test/`.

- [ ] **Step 4: Verify blame is preserved**

For two sample files:
```
git log --follow legacy/src/abc/aozora.clj | head -20
git log --follow legacy/src/abc/tei.clj | head -20
```
Expected: history goes back to the original `src/abc/...` location.

- [ ] **Step 5: Verify the focused-test gate is unchanged**

```
nix flake check --no-build-logs
```
Expected: all checks pass. The focused-test alias enumerates by namespace name, not by file path, so the relocation is invisible to it.

- [ ] **Step 6: Update `docs/next-steps.md`**

```markdown
- **2026-04-29 — Legacy namespaces quarantined to `legacy/` (ADR 0019).**
  Pre-v0 namespaces relocated from `src/abc/` to `legacy/src/abc/` and from
  `test/abc/` to `legacy/test/abc/`. Blame is preserved via `git mv`.
  The v0 focused-test alias is unchanged.
```

- [ ] **Step 7: Commit**

```
git add deps.edn legacy/ docs/next-steps.md
git commit -m "refactor: quarantine legacy namespaces to legacy/ (ADR 0019)"
```

---

### Phase 3-D — If Option D (Delete-and-archive) is chosen

**This is destructive. Do not execute without explicit user sign-off captured in the ADR's Decision section.**

**Files:**
- Delete: every `src/abc/*.clj` marked DORMANT in the Task 1 discovery table — and ONLY those. `abc.text`, `abc.ndc`, `abc.annotation.schema`, and any other namespace listed in `nix/clj-nix-deps.edn`'s focused-test alias or in `flake.nix`'s `contract-surface` block are PASSING-IN-FOCUSED and **must not be deleted**. The deletion list is derived from the discovery table, not hand-typed.
- Delete: every corresponding `test/abc/*_test.clj` marked DORMANT in the Task 1 discovery table.
- Modify: `deps.edn` — remove the deps whose Task 1 attribution table lists ONLY deleted namespaces as consumers. A dep with at least one surviving consumer (whether v0 or legacy) **stays**. The remove list comes from the attribution table; it is not hand-typed.
- Modify: `nix/clj-nix-deps.edn` — unchanged.
- Modify: `docs/next-steps.md` — milestone entry citing ADR 0019.
- Tag (before deleting): `legacy-archive-pre-parser-ir` on the commit prior to deletion.

**Critical safety note:** `abc.text`, `abc.ndc`, and `abc.annotation/schema.clj` ALL appear in the v0 contract surface (`flake.nix:276-278` `contract-surface` derivation; `nix/clj-nix-deps.edn` focused-test alias). Any deletion list that includes them is wrong. The first sub-step below builds a *derived* deletion list against this exclusion set.

#### Task D1: Pre-delete archive tag

- [ ] **Step 1: Tag the current HEAD**

```
git tag -a legacy-archive-pre-parser-ir -m "Last commit containing pre-v0 parser/TEI/stats Clojure code (ADR 0019)"
git push origin legacy-archive-pre-parser-ir
```

(Skip the push if the user has not authorised it — the local tag is sufficient as long as it survives via the remote later.)

#### Task D2: Delete the legacy code

- [ ] **Step 2: Generate the exact deletion list**

Do NOT type the deletion list by hand. Derive it from the Task 1 discovery table:

```bash
## Sketch (adapt to the actual columns in the discovery note):
##   col 1 = namespace, col 2 = file, col 7 = test status
## Keep only rows where col 7 == DORMANT.
awk -F'|' '$7 ~ /DORMANT/ { gsub(/^ +| +$/, "", $2); print $2 }' \
    docs/superpowers/notes/2026-04-29-legacy-namespaces-discovery.md \
    > /tmp/legacy-deletion-list.txt
cat /tmp/legacy-deletion-list.txt
```

Then sanity-check the list against the v0 contract-surface exclusion set (these MUST NOT appear in the list — if any do, the discovery table is wrong and must be corrected before proceeding):

```bash
EXCLUSIONS='src/abc/text.clj
src/abc/ndc.clj
src/abc/annotation/schema.clj
src/abc/tools/'

while read -r f; do
  if grep -qF "$f" <<<"$EXCLUSIONS"; then
    echo "ERROR: deletion list includes contract-surface file: $f"
    exit 1
  fi
done < /tmp/legacy-deletion-list.txt
echo "deletion list safety-check passed"
```

- [ ] **Step 3: Apply the deletion list**

```bash
xargs -a /tmp/legacy-deletion-list.txt git rm
```

Then for each surviving deleted file's test (only if marked DORMANT — same exclusion set applies):

```bash
## Build the test deletion list the same way (col 6 = test file):
awk -F'|' '$7 ~ /DORMANT/ { gsub(/^ +| +$/, "", $6); print $6 }' \
    docs/superpowers/notes/2026-04-29-legacy-namespaces-discovery.md \
    > /tmp/legacy-test-deletion-list.txt
xargs -a /tmp/legacy-test-deletion-list.txt git rm
```

- [ ] **Step 4: Trim `deps.edn` from the attribution table**

Same derivation pattern: from the Task 1 deps.edn → namespace attribution table, list every row where ALL consuming namespaces are in the deleted set. Those deps are now orphaned and removed from `deps.edn`. Deps with at least one surviving consumer (v0 or legacy) stay.

```bash
## Capture the orphan list, manually inspect, then remove:
git diff deps.edn  # before removal
$EDITOR deps.edn   # remove only the rows the discovery table identified as orphaned
git diff deps.edn  # after — confirm no v0-shared dep was touched
```

- [ ] **Step 5: Verify**

```
nix flake check --no-build-logs
```
Expected: focused-test alias still passes (no v0 namespace required the deleted code); contract-surface still passes (no schema rotated).

- [ ] **Step 6: Update `docs/next-steps.md`**

```markdown
- **2026-04-29 — Legacy namespaces deleted (ADR 0019).** Pre-v0
  parser/TEI/stats Clojure code removed from `master`; archived under
  tag `legacy-archive-pre-parser-ir`. `deps.edn` shrinks by N entries.
  Focused-test gate unchanged.
```

- [ ] **Step 7: Commit**

```
git add -A src/ test/ deps.edn docs/next-steps.md
git commit -m "refactor: delete legacy namespaces (ADR 0019); archived as legacy-archive-pre-parser-ir"
```

---

## Self-Review

**1. Spec coverage.** The four options from `Context` each map to exactly one Phase 3 sub-section (A, B, C, D). The ADR draft step covers writing all four context paragraphs; the Decision step picks one; the Hard Rule step writes the option-specific invariants. The discovery tasks (Task 1, Task 2) produce the inputs the Decision step needs. Coverage is complete; the gap is human judgement at Task 3 Step 3, which is by design.

**2. Placeholder scan.** Two soft spots:
- Phase 3-B Step 3 references "the existing `clj-nix-focused-tests` derivation" pattern but does not paste it verbatim — the engineer running this task must read `flake.nix` to see the current shape. That is acceptable here because the flake is large; pasting the derivation in the plan would rot.
- Phase 3-C Step 1 names "the dormant tests" without enumerating them. The Task 1 discovery note is the source of truth for that list; the plan deliberately does not duplicate it. If executing Phase 3-C without Task 1 having been completed, this step is under-specified.
Both are acceptable: the plan is a Discovery + Decision + Execution sequence and Phase 3 explicitly depends on Phase 1 outputs.

**3. Type/name consistency.** `nix/legacy-clj-nix-deps.edn` is referenced in Phase 3-B Steps 1, 2, 5, 6, 7 — same name throughout. The tag name `legacy-archive-pre-parser-ir` appears in Phase 3-D Steps 1 and 6 — same. ADR number `0019` appears in every milestone entry — same.

**4. Cross-option contamination.** Each Phase 3 sub-section can be run independently of the others. Re-checked: Phase 3-A does not touch `legacy/`; Phase 3-B does not move source files; Phase 3-C does not delete deps; Phase 3-D pre-conditions on the archive tag.

**5. Boundary leak from `abc.tools.malli`.** Task 2 Step 1 explicitly flags `src/abc/tools/malli.clj:22`'s dynamic require list as a known boundary case (not an accident). Task 3 Step 3a forces the ADR to take an explicit position (Sub-A: keep the soft reference; Sub-B: split the malli registry into v0 and legacy halves). Phase 3 sub-sections inherit the chosen sub-decision: Sub-B adds an `abc.tools.malli-v0` rename that the migration tasks must execute alongside the option-specific work.

**6. Phase 3-C path/namespace mapping.** `legacy/src/abc/<name>.clj` (NOT `legacy/abc/<name>.clj`) is the canonical layout, because the path-to-namespace resolver needs `legacy/src` as a `:paths` root. The ADR Hard Rule, the Phase 3-C File Structure, the worked-example table, and the `git mv` commands all use the `legacy/src/abc/` form consistently.

**7. Phase 3-D safety on contract-surface files.** The deletion list is *derived* from the discovery table, not hand-typed. An explicit safety-check shell snippet rejects any deletion list containing `src/abc/text.clj`, `src/abc/ndc.clj`, `src/abc/annotation/schema.clj`, or anything under `src/abc/tools/` — i.e. the v0 contract-surface files. A wrong discovery table fails the check before `git rm` runs. The earlier draft of this section had a hard-coded `git rm` line that included these files; that draft has been removed.

**8. Phase 3-B compile-only flake check.** The flake snippet uses `pkgs.runCommand` + `mk-deps-cache`, mirroring the existing `clj-nix-focused-tests` derivation in `flake.nix:238` — NOT `mkCljApp` (which is unused in this codebase). The dep-drift verification step exercises both directions: a missing-but-required dep must FAIL the check; an unused-but-present dep must PASS. The earlier draft inverted these; the corrected logic is in Phase 3-B Task B1 Step 4.

**9. `docs/next-steps.md` count conflict with TEI plan.** Both plans bump the same milestone-count word. Each plan now reads-then-bumps by exactly one (Phase 3-A Step 1 in this plan; Task 14 Step 1 in the TEI plan). Order of landing is invariant; the count advances correctly whether this plan or the TEI plan lands first.
