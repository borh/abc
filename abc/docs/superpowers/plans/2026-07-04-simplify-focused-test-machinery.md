# Eliminate the focused-test machinery and the vestigial clj-mecab binding Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Delete the `:abc/focused-test` machinery (hand-duplicated namespace list, regex-Clojure-from-EDN-string coverage guard, curated `nix/clj-nix-deps.edn`, legacy allowlist) AND remove the `clj-mecab` runtime binding from the Clojure app (the dep, the require, the two call sites, the devShell mecab env), keeping the `:mecab.features/*` schema definitions as the contract surface the sibling Rust `ab-validator` repo conforms to. CI's Clojure tests switch to clj-nix's offline classpath against the full `deps.edn` + kaocha auto-discovery.

**Architecture:** Two complected concerns unwind together. (1) The `:abc/focused-test` machinery exists to run a curated subset of tests against a deliberately-trimmed offline classpath (`deps-lock.json` built from `nix/clj-nix-deps.edn`), because several test namespaces require libs absent from that trim (`clj-mecab`, `java-time`, `tawny-owl`, `clj-commons/fs`). (2) The reason `clj-mecab` was on the classpath at all is two vestigial in-Clojure parsing paths — `abc.annotation/parse-with-tags` (calls `mecab/parse-sentence`) and `abc.stats/compute-text` (lazy `requiring-resolve 'clj-mecab.parse/parse-sentence`) — which the parser-IR contract and the Rust `ab-validator` repo have superseded. Reachability confirms it: `parse-with-tags` is called only by `add-tags`, reachable only from `parse-text`, exercised only by `annotation_test`'s `parse-aozora-text` deftest (already `^:kaocha/skip`, commented "Aozora text parsing moves out of Clojure"); `compute-text` is exercised only by `text_test`'s `compute-text-splits-lines-before-tokenizing-test`, which stubs `parse-sentence` via `with-redefs`. No tool `-main` reaches either path.

So: removing `clj-mecab` first collapses one of the focused-test machinery's raisons-d'être (mecab's external binary + missing-from-trim-classpath was the reason `abc.annotation-test` was allowlisted). With clj-mecab gone, the remaining allowlist entries are `java-time` (on root `deps.edn`, just absent from the trim) and `tawny-owl`/`timbre` (likewise) — all solvable by regenerating `deps-lock.json` from the full `deps.edn` and running kaocha auto-discovery. clj-nix's native [`mk-deps-cache`](https://jlesquembre.github.io/clj-nix/api/#mk-deps-cache) already powers `cljDepsCache` (flake.nix:370); the lockfile regenerated from the full `deps.edn` makes the trimmed `nix/clj-nix-deps.edn` redundant.

**Tech Stack:** Nix flakes; [clj-nix](https://jlesquembre.github.io/clj-nix/) (`mk-deps-cache`, `deps-lock`); Clojure CLI (`deps.edn`); [kaocha](https://github.com/lambdaisland/kaocha); clojure.test; malli (schemas kept).

## Global Constraints

- **Behavior-preserving ordering.** The clj-mecab removal (Phase 1) must not change which tests pass — both mecab call sites are already quarantined to `^:kaocha/skip`-marked or `with-redefs`-stubbed tests. The lockfile regen (Task 4) and skip annotations (Task 3) characterize *current* test behavior. The runner swap (Task 5) and machinery deletion (Task 6) verify against the Task 2 baseline. **One kind of change per commit.**
- **Nix sandbox has no network.** Every Clojure dep must be in `deps-lock.json`. clj-nix `mk-deps-cache` (flake.nix:370) provides the offline maven+gitlibs cache.
- **`deps-lock.json` is generated from `deps.edn` files clj-nix discovers.** Per the [lock-file docs](https://jlesquembre.github.io/clj-nix/lock-file/), `deps-lock` reads all `deps.edn` by default; `--deps-include`/`--deps-exclude` restrict. After this change there is exactly one `deps.edn` (root) feeding the lockfile.
- **Files tracked by git are the only ones Nix sees.** (flake constraint).
- **Schema keys stay.** The `:mecab.features/*` keys in `src/abc/annotation/schema.clj` are the morpheme-token contract the Rust `ab-validator` parser emits. Remove only the *runtime binding* (`clj-mecab` dep, the require, the parse calls), not the schema.
- clj-nix version pinned via the `clj-nix` flake input (unchanged). `mk-deps-cache` signature: `mk-deps-cache { lockfile = ./deps-lock.json; }`.
- kaocha skip semantics (verified): `:kaocha.filter/skip-meta` defaults to `[:kaocha/skip]`. Metadata on a `deftest` skips that test. **The namespace must still load** for kaocha to discover deftests — loadability (full classpath) is the prerequisite; `^:kaocha/skip` is for deftests whose *body* needs resources the sandbox lacks (live DB zip, real git repo, hermit reasoner).

---

## File Structure

Files touched, with responsibility:

**Phase 1 — remove clj-mecab:**
- **`deps.edn`** (MODIFY): remove `clj-mecab/clj-mecab {:mvn/version "1.0.102"}` from `:deps`.
- **`src/abc/annotation.clj`** (MODIFY): remove `[clj-mecab.parse :as mecab]` from `(:require …)`. Change `parse-with-tags`'s `:sentence/tokens` from `(mapv … (mecab/parse-sentence plaintext) …)` to `[]` (empty — tokens are produced by the external Rust parser; this ns no longer tokenizes). Keep the fn; it still produces `:sentence/annotated-text` and `:sentence/text`.
- **`src/abc/stats.clj`** (MODIFY): remove the `parse-sentence` private fn (the `requiring-resolve`). `compute-text` no longer tokenizes; it returns `:sentence-lengths` only (the `:tokens` reduction over `:mecab.features/orth` is gone). Keep `doc-to-token-map`'s shape but stop dereferencing `:mecab.features/orth` from parse output — it reads `:sentence/tokens` produced elsewhere (the parser-IR path), so it stays. `text_test`'s stub continues to satisfy the contract.
- **`test/abc/text_test.clj`** (MODIFY): the `with-redefs` stub redefined `abc.stats/parse-sentence`, which no longer exists. Change the test to stub the token source `compute-text` actually uses, or simplify the test to assert `:sentence-lengths` from a stubbed-token input. See Task 1 Step 4.
- **`flake.nix`** (MODIFY, devShell): remove the mecab override, `unidic`, `mecabDicDir`, `LD_LIBRARY_PATH` mecab/systemd entries, `MECABRC`, `MECAB_DICDIR`, the `shellHook` mecab exports. DevShell no longer needs mecab.
- **`test/abc/annotation_test.clj`** (no edit — `parse-aozora-text` already `^:kaocha/skip`; the other deftests don't touch mecab).
- Keep: `src/abc/annotation/schema.clj` (the `:mecab.features/*` keys are the contract surface — unchanged).

**Phase 2 — eliminate the focused-test machinery:**
- **`deps-lock.json`** (regenerate): from full `deps.edn` (now without clj-mecab).
- **`nix/clj-nix-deps.edn`** (DELETE).
- **`nix/.focused-test-legacy-allowlist`** (DELETE).
- **`nix/check-focused-test-coverage.sh`** (DELETE).
- **`flake.nix`** (MODIFY): `checks.clj-nix-focused-tests` swaps `clojure -M:abc/focused-test` → `clojure -M:test:kaocha`, drops the deps.edn swap-in; `checks.focused-test-coverage` deleted; `contract-surface`'s `test -f ${./nix/clj-nix-deps.edn}` line removed.
- **`bin/update-clj-nix-lock`** (MODIFY): drop `--deps-include nix/clj-nix-deps.edn`.
- **`test/abc/load_test.clj`**, **`test/abc/git_test.clj`** (MODIFY): add `^:kaocha/skip` to deftests whose bodies need absent external resources (live DB zip, real git repo).
- **`README.md`** + any stale **`docs/adr/*.md`** (MODIFY, final task).

No new files. Net deletion: ~140 lines bash+Python regex + one duplicated ~36-symbol list + one trimmed deps.edn + one allowlist + the mecab devShell env + the clj-mecab dep + two vestigial parse calls.

---

## Phase 0: Characterize the CI test surface (PROTECT)

Characterization first — the skill's PROTECT step. Pin the *current* "these tests pass in `clj-nix-focused-tests`" set so Phase 2's deletion can be verified to preserve observable behavior. **No edits in this phase** — only capture.

**Files:**
- Read: `nix/clj-nix-deps.edn` (the `:abc/focused-test` `-e` string — authoritative list of what runs today)
- Read: `flake.nix:423-456` (the `clj-nix-focused-tests` derivation)
- Read: the mecab call sites (`src/abc/annotation.clj:429-442`, `src/abc/stats.clj:39-40`)
- Produce: `docs/superpowers/notes/2026-07-04-focused-test-baseline.txt` (tracked)

**Interfaces:**
- Consumes: the `:abc/focused-test` `(test/run-tests ...)` namespace list
- Produces: a saved text record of (a) the namespace list, (b) the `clojure -M:abc/focused-test` summary line ("Ran N tests containing M assertions."), (c) the mecab-call-site reachability table proving removal is behavior-preserving for the runnable set.

### Task 0: Capture baseline

- [ ] **Step 1: Extract the authoritative namespace list from `:abc/focused-test`**

```bash
cd /home/bor/Projects/abc/.worktrees/simplify-focused-test
mkdir -p docs/superpowers/notes
python - <<'PY' > docs/superpowers/notes/2026-07-04-focused-test-baseline.txt
import re, pathlib
deps = pathlib.Path("nix/clj-nix-deps.edn").read_text()
m = re.search(r"\(test/run-tests (.*?)\)\]", deps)
syms = re.findall(r"'([A-Za-z][A-Za-z0-9.\-*_+!?]*)", m.group(1))
print(f"# {len(syms)} namespaces currently run by :abc/focused-test")
for s in sorted(syms): print(s)
PY
```

Expected: ~36 namespaces. Append to the notes file.

- [ ] **Step 2: Build the current `clj-nix-focused-tests` check and capture its summary**

```bash
nix build .#clj-nix-focused-tests -L 2>&1 | tee tmp/focused-build-before.log
grep -E "Ran [0-9]+ tests|[0-9]+ assertions" tmp/focused-build-before.log >> docs/superpowers/notes/2026-07-04-focused-test-baseline.txt
```

Record N (test count) and M (assertions). This N is the **PROTECT baseline**. Phase 2's kaocha run (minus `^:kaocha/skip`'d deftests) must report the same N (or N+2 if `abc.aozora-test`'s deftests become runnable — see Phase 2 Task 3).

- [ ] **Step 3: Reconcile each allowlist namespace against its file**

```bash
cd /home/bor/Projects/abc/.worktrees/simplify-focused-test
for ns in core owl html aozora annotation tei xtdb db load git; do
  f="test/abc/${ns}_test.clj"
  if [ -f "$f" ]; then
    deftests=$(grep -cE '^\(deftest|^[[:space:]]*\(deftest' "$f")
    skips=$(grep -c 'kaocha/skip' "$f")
    printf "%-12s deftests=%-3s kaocha/skip=%-2s\n" "$ns" "$deftests" "$skips"
  else
    printf "%-12s FILE_MISSING (stale allowlist)\n" "$ns"
  fi
done >> docs/superpowers/notes/2026-07-04-focused-test-baseline.txt
```

Expected (verified in pre-plan investigation):
- `core` — 0 deftests (stub). Loads with full classpath. No skip.
- `owl` — deftests inside `(comment …)` (not real). Loads with `tawny-owl` on classpath. No skip, 0 runnable.
- `html` — 1 deftest, already `^:kaocha/skip`. Loads fine. No edit.
- `aozora` — 2 deftests, NOT skipped. `abc.aozora` requires `java-time` (present in full lockfile). Deftests use `mg/generate` (no DB). Loads + runs. Previously excluded only for the trimmed classpath. N increases by 2. No skip edit.
- `annotation` — `parse-aozora-text` already `^:kaocha/skip`; `parse-aozora-plaintext` + `parse-annotations-tests` don't call mecab (they test `aozora-annotation->tags` / `doc->plaintext`). After Phase 1 removes clj-mecab, `abc.annotation` loads without the native binary. **No skip edits needed in annotation_test.**
- `tei`, `xtdb`, `db` — **FILE_MISSING.** Stale (XTDB removed). Deleted with the allowlist in Phase 2 Task 6.
- `load` — `load-test` (not marked) + `extract-texts-test` (`^:kaocha/skip`). `abc.load` requires `me.raynes/fs` (via `clj-commons/fs`, present in full lockfile). `load-test` body needs the aozora-bunko DB zip → **add `^:kaocha/skip`** (Phase 2 Task 3).
- `git` — `git-log` (not marked) calls `load-aozora-bunko-git` (real repo). **Add `^:kaocha/skip`** (Phase 2 Task 3).

- [ ] **Step 4: Record the mecab reachability proof**

Append to the notes file:
```
# mecab reachability (justifies Phase 1 removal as behavior-preserving)
- src/abc/annotation.clj:440 (mecab/parse-sentence in parse-with-tags)
  callers: add-tags (:446) -> parse-text (:486) -> test/abc/annotation_test.clj only
  the test deftest (parse-aozora-text) is already ^:kaocha/skip
- src/abc/stats.clj:40 (requiring-resolve 'clj-mecab.parse/parse-sentence)
  callers: compute-text -> test/abc/text_test.clj only, which stubs via with-redefs
- no tool -main (aozora_ingest, validate_corpus, materialize_*, validate_design_bundle,
  aat_parser_ir_compat, parser_ir_*) requires abc.annotation's parse path or abc.stats's mecab path
- the :mecab.features/* schema keys in src/abc/annotation/schema.clj are KEPT
  (contract surface for the Rust ab-validator parser)
```

- [ ] **Step 5: Commit the baseline note**

```bash
git add docs/superpowers/notes/2026-07-04-focused-test-baseline.txt
git commit -m "docs: characterize focused-test + mecab baseline before simplification"
```

---

## Phase 1: Remove the clj-mecab runtime binding

Cut the dep, the two require/call sites, and the devShell mecab env. Schema keys stay. This is behavior-preserving for the runnable test set (Phase 0 Step 4 proved reachability is confined to already-skipped/stubbed tests).

### Task 1: Remove clj-mecab from src

**Files:**
- Modify: `deps.edn` (remove the `clj-mecab/clj-mecab` line)
- Modify: `src/abc/annotation.clj` (drop the require; neutralize `parse-with-tags`'s tokenization)
- Modify: `src/abc/stats.clj` (drop `parse-sentence`; neutralize `compute-text`'s tokenization)
- Modify: `test/abc/text_test.clj` (the `with-redefs` target is gone)

**Interfaces:**
- Consumes: Phase 0 Step 4's reachability proof
- Produces: `abc.annotation` and `abc.stats` load without `clj-mecab` on the classpath. The `:mecab.features/*` schema in `abc.annotation.schema` is unchanged.

- [ ] **Step 1: Remove the `clj-mecab` dep from `deps.edn`**

Find:
```clojure
  clj-mecab/clj-mecab                        {:mvn/version "1.0.102"}
```
Replace with: (remove the line; close up the blank line if it leaves a double-blank).

- [ ] **Step 2: Drop the require in `src/abc/annotation.clj`**

Find:
```clojure
            [clj-mecab.parse :as mecab]
```
Replace with: (remove the line).

- [ ] **Step 3: Neutralize `parse-with-tags`'s tokenization in `src/abc/annotation.clj`**

The fn at ~429 currently produces `:sentence/tokens` by calling `mecab/parse-sentence`. Tokens are now the Rust parser's job. Find:
```clojure
(defn parse-with-tags [sentence]
  (let [annotated-text (if-let [tagged-sentence (try (aozora-annotation->tags sentence)
                                                     (catch Exception e (do (timbre/error sentence e)
                                                                            (throw e))))]
                         tagged-sentence
                         sentence)
        plaintext (sentence->plaintext annotated-text)]
    {:sentence/tags           #{}
     :sentence/annotated-text annotated-text
     :sentence/text           plaintext
     :sentence/tokens         (mapv (fn [m position] (assoc m :mecab.features/position position))
                                    (mecab/parse-sentence plaintext)
                                    (range))}))
```
Replace `:sentence/tokens` line with an empty vector (tokens are produced downstream by the Rust parser; this fn annotates only):
```clojure
(defn parse-with-tags [sentence]
  (let [annotated-text (if-let [tagged-sentence (try (aozora-annotation->tags sentence)
                                                     (catch Exception e (do (timbre/error sentence e)
                                                                            (throw e))))]
                         tagged-sentence
                         sentence)
        plaintext (sentence->plaintext annotated-text)]
    {:sentence/tags           #{}
     :sentence/annotated-text annotated-text
     :sentence/text           plaintext
     ;; Tokens (mecab morphemes) are produced by the Rust ab-validator parser,
     ;; not in Clojure. Kept empty here; see :mecab.features/* schema in
     ;; abc.annotation.schema for the contract the external parser conforms to.
     :sentence/tokens         []}))
```

- [ ] **Step 4: Drop `parse-sentence` in `src/abc/stats.clj` and neutralize `compute-text`'s tokenization**

Find:
```clojure
(defn- parse-sentence [s]
  ((requiring-resolve 'clj-mecab.parse/parse-sentence) s))

(defn compute-text
  "Tokenizes input text `s` and returns a map containing words "
  [s]
  (into {}
        (comp
          (map parse-sentence)
          (x/transjuxt {:tokens           (x/reduce (fn ([] []) ([a] a) ([a x] (x/into a (:mecab.features/orth x)))))
                        :sentence-lengths (x/reduce (fn ([] []) ([a] a) ([a x] (conj a (count x)))))}))
        (string/split s #"\n+")))
```
Replace with (tokenization moves to the Rust parser; `compute-text` keeps `:sentence-lengths` only, taking a tokenizable input line directly — the test stubs the input as token sequences):
```clojure
;; Tokenization (mecab morpheme parsing) is done by the Rust ab-validator
;; parser, not in Clojure. compute-text now takes per-line token sequences
;; and computes sentence-length statistics over them; :tokens is the caller's
;; responsibility. See :mecab.features/* schema in abc.annotation.schema.
(defn compute-text
  "Given a map of line -> token sequence, returns sentence-length statistics."
  [lines->tokens]
  (into {}
        (comp
          (map (fn [[line tokens]]
                 [line {:sentence-lengths (count tokens)}]))))
        lines->tokens))
```
Note: this changes `compute-text`'s signature (it took a string, now takes a map). Its only caller is `text_test` (Step 5). `doc-to-token-map` (which reads `:sentence/tokens` produced elsewhere — the parser-IR path) is **unchanged**: it already consumes `:mecab.features/orth` from token maps produced downstream, not from `clj-mecab`. Leave it.

- [ ] **Step 5: Update `test/abc/text_test.clj` to the new `compute-text` signature**

Find:
```clojure
(deftest compute-text-splits-lines-before-tokenizing-test
  (testing "compute-text tokenizes each newline-delimited input line"
    (with-redefs [abc.stats/parse-sentence (fn [_line] [#:mecab.features{:orth "token"}])]
      (let [{:keys [sentence-lengths]} (stats/compute-text "猫\n犬")]
        (is (= [1 1] sentence-lengths))))))
```
Replace with:
```clojure
(deftest compute-text-splits-lines-into-sentence-lengths-test
  (testing "compute-text returns per-line token counts as sentence-lengths"
    (let [result (stats/compute-text {"猫" [#:mecab.features{:orth "token"}]
                                      "犬" [#:mecab.features{:orth "token"}]})]
      (is (= 1 (get-in result ["猫" :sentence-lengths])))
      (is (= 1 (get-in result ["犬" :sentence-lengths]))))))
```

- [ ] **Step 6: Verify clj-mecab is gone and the namespaces load**

```bash
cd /home/bor/Projects/abc/.worktrees/simplify-focused-test
grep -rn "clj-mecab\|mecab/parse-sentence\|requiring-resolve.*mecab" src/ test/
# Expected: only :mecab.features/* schema keys + the empty :sentence/tokens []
clojure -e "(require 'abc.annotation 'abc.stats 'abc.annotation.schema) (println :ok)" 2>&1 | tail -5
```

Expected: no `clj-mecab` references in src/test except schema keys. The require succeeds without mecab on the classpath (the schema ns has no mecab dep; `abc.annotation` no longer requires `clj-mecab.parse`).

- [ ] **Step 7: Run the affected tests (online classpath)**

```bash
clojure -M:test:kaocha abc.annotation-test abc.text-test 2>&1 | tee tmp/mecab-removal-kaocha.log
```

Expected: `abc.text-test` green with the new signature. `abc.annotation-test`'s `parse-aozora-text` still `^:kaocha/skip`. `parse-aozora-plaintext` and `parse-annotations-tests` green (they don't touch mecab). 0 failures.

- [ ] **Step 8: Commit**

```bash
git add deps.edn src/abc/annotation.clj src/abc/stats.clj test/abc/text_test.clj
git commit -m "parse: remove clj-mecab runtime binding from the Clojure app

Aozora text tokenization (mecab morpheme parsing) is done by the sibling
Rust ab-validator repo per the parser-IR contract; the two in-Clojure
call sites (abc.annotation/parse-with-tags, abc.stats/compute-text) were
reachable only from already-^:kaocha/skip'd or with-redefs-stubbed tests.

Removes:
- clj-mecab/clj-mecab dep from deps.edn
- [clj-mecab.parse :as mecab] require in abc.annotation
- the mecab/parse-sentence call in parse-with-tags (:sentence/tokens -> [])
- the parse-sentence requiring-resolve in abc.stats; compute-text takes
  per-line token sequences instead of a raw string

Keeps the :mecab.features/* schema in abc.annotation.schema unchanged —
it's the contract surface the Rust parser conforms to."
```

### Task 2: Remove the mecab devShell env from `flake.nix`

**Files:**
- Modify: `flake.nix` (devShell block, ~716-760)

**Interfaces:**
- Consumes: Task 1 (clj-mecab no longer a dep)
- Produces: `nix develop` no longer installs mecab/unidic or sets MECAB*/LD_LIBRARY_PATH for mecab. DevShell still has clojure, git, git-cliff, jdk21, jq, libxml2.

- [ ] **Step 1: Read the current devShell**

```bash
cd /home/bor/Projects/abc/.worktrees/simplify-focused-test
sed -n '712,765p' flake.nix
```

- [ ] **Step 2: Strip the mecab env**

Find the `default = pkgs.mkShell { … }` block containing `mecab`, `unidic`, `mecabDicDir`, `LD_LIBRARY_PATH` (mecab+systemd), `MECABRC`, `MECAB_DICDIR`, and the `shellHook` mecab exports. Replace the whole `default` shell with:
```nix
          default = pkgs.mkShell {
            packages = with pkgs; [
              clojure
              git
              git-cliff
              jdk21
              jq
              libxml2
            ];
          };
```
Delete the `mecab` override `let` bindings (`mecab`, `unidic`, `mecabDicDir`) above the shell — they're now unused. The `validation` shell (below `default`) is unchanged.

- [ ] **Step 3: Verify the devShell evaluates**

```bash
nix flake show --allow-import-from-derivation 2>&1 | grep -i "devShell\|default" | head
nix develop --command bash -c "echo ok; which clojure" 2>&1 | tail -5
```

Expected: devShell builds. (mecab/unidic removal may take a moment if the local nix store had them; no network needed for removal.)

- [ ] **Step 4: Commit**

```bash
git add flake.nix
git commit -m "devshell: drop mecab + unidic env

clj-mecab is no longer a dependency (previous commit); the devShell's mecab
override, unidic-cwj dictionary, MECABRC/MECAB_DICDIR env, and the
mecab/systemd LD_LIBRARY_PATH are vestigial. DevShell keeps clojure, git,
git-cliff, jdk21, jq, libxml2."
```

---

## Phase 2: Eliminate the focused-test machinery

With clj-mecab gone, the trimmed `nix/clj-nix-deps.edn`'s classpath divergence is reduced to `java-time` / `tawny-owl` / `timbre` / `clj-commons/fs` / `kaocha` — all solvable by a lockfile regenerated from the full `deps.edn`.

### Task 3: Add `^:kaocha/skip` to resource-dependent deftests

Mark deftests whose bodies need resources absent from the Nix sandbox check derivation (live DB zip, real git repo). Namespaces themselves now load with the full classpath (Task 4); kaocha discovers deftests and skips the marked ones.

**Files:**
- Modify: `test/abc/load_test.clj` (`load-test` deftest, ~line 19)
- Modify: `test/abc/git_test.clj` (`git-log` deftest, ~line 23)
- No edit: `test/abc/annotation_test.clj` (no deftest needs a skip after Phase 1 — `parse-aozora-text` already skipped; others don't need mecab)
- No edit: `test/abc/html_test.clj` (already marked), `test/abc/aozora_test.clj` (runs cleanly), `test/abc/owl_test.clj` (0 real deftests), `test/abc/core_test.clj` (0 deftests)

**Interfaces:**
- Consumes: Phase 0 Step 3's reconcile table
- Produces: every deftest that would fail at *runtime* in the sandbox carries `^:kaocha/skip`.

- [ ] **Step 1: Mark `load-test` in `test/abc/load_test.clj`**

Find:
```clojure
(deftest load-test
```
Replace with:
```clojure
;; Skipped in CI: body needs the aozora-bunko DB zip on disk (sandbox lacks it).
(deftest ^:kaocha/skip load-test
```

- [ ] **Step 2: Mark `git-log` in `test/abc/git_test.clj`**

Find:
```clojure
(deftest git-log
```
Replace with:
```clojure
;; Skipped in CI: body calls load-aozora-bunko-git (needs the real aozora-bunko git repo on disk).
(deftest ^:kaocha/skip git-log
```

- [ ] **Step 3: Verify the skip annotations load (online classpath)**

```bash
cd /home/bor/Projects/abc/.worktrees/simplify-focused-test
clojure -M:test:kaocha --print-summary 2>&1 | tee tmp/kaocha-pre-lockfile.log
```

Expected: all namespaces load. Skipped deftests appear as skipped. `abc.aozora-test`'s 2 deftests run green. 0 failures. If a namespace fails to load here, **stop** — Phase 1 missed a dep that namespace needs; investigate before Task 4.

- [ ] **Step 4: Commit**

```bash
git add test/abc/load_test.clj test/abc/git_test.clj
git commit -m "test: mark resource-dependent deftests ^:kaocha/skip for offline CI

These deftests need runtime resources absent from the Nix sandbox check
derivation (live aozora-bunko DB zip, real git repo). The namespaces
themselves still load and run their non-resource deftests. Prepares for
replacing :abc/focused-test with kaocha auto-discovery."
```

### Task 4: Regenerate `deps-lock.json` from the full `deps.edn`

**Files:**
- Modify: `bin/update-clj-nix-lock` (drop `--deps-include`)
- Regenerate: `deps-lock.json`

**Interfaces:**
- Consumes: root `deps.edn` (now clj-mecab-free; `:test` + `:kaocha` alias deps included)
- Produces: `deps-lock.json` with `clojure.java-time`, `clj-commons/fs`, `tawny-owl`, `timbre`, `owlapi`, `kaocha`, `test.check`, `orchestra`, `specviz` present (verified by `mvn-path` grep). No `clj-mecab` entries.

- [ ] **Step 1: Edit `bin/update-clj-nix-lock`**

Current:
```bash
exec nix run github:jlesquembre/clj-nix#deps-lock -- --deps-include nix/clj-nix-deps.edn "$@"
```
New:
```bash
exec nix run github:jlesquembre/clj-nix#deps-lock -- "$@"
```

- [ ] **Step 2: Regenerate `deps-lock.json`**

```bash
cd /home/bor/Projects/abc/.worktrees/simplify-focused-test
./bin/update-clj-nix-lock
```

Expected: `deps-lock.json` rewritten (network required at lockfile-gen time — not the sandbox).

- [ ] **Step 3: Verify the previously-absent libs are present; clj-mecab absent**

```bash
python - <<'PY'
import json
d = json.load(open("deps-lock.json"))
paths = [x["mvn-path"] for x in d["mvn-deps"]]
for probe in ["java-time","clj-commons","tawny","timbre","owlapi","kaocha","test.check","orchestra","specviz","clj-mecab","mecab"]:
    hits = [p for p in paths if probe.lower() in p.lower()]
    print(f"{probe:15s} -> {len(hits)} {hits[:1]}")
PY
```

Expected: every non-mecab probe has ≥1 hit; `clj-mecab`/`mecab` has 0.

- [ ] **Step 4: Verify `mk-deps-cache` still builds the current derivation**

```bash
nix build .#clj-nix-focused-tests -L 2>&1 | tee tmp/full-lockfile-build.log
```

Expected: build SUCCEEDS here — `cljDepsCache` rebuilds with the full deps; the derivation still runs `:abc/focused-test` against the swapped-in `nix/clj-nix-deps.edn` (deleted in Task 6, still present now). Same "Ran N tests" as Phase 0 Step 2's baseline. If it fails with a missing-dep error, `deps-lock.json` is incomplete — re-run Step 2.

- [ ] **Step 5: Commit**

```bash
git add bin/update-clj-nix-lock deps-lock.json
git commit -m "build: regenerate deps-lock.json from full deps.edn

Drops --deps-include nix/clj-nix-deps.edn so deps-lock.json captures the
complete classpath (java-time, clj-commons/fs, tawny-owl, timbre, kaocha,
test.check, orchestra, specviz). clj-mecab entries are gone (Phase 1).
The trimmed lockfile's purpose—offline subset for :abc/focused-test—is
obsolete now that CI will use kaocha auto-discovery (next commits)."
```

### Task 5: Switch the `clj-nix-focused-tests` derivation to kaocha

**Files:**
- Modify: `flake.nix:423-456` (the `clj-nix-focused-tests` derivation)

**Interfaces:**
- Consumes: Task 4's full `deps-lock.json` + Task 3's skip annotations + Phase 1's mecab removal
- Produces: a Nix check that runs kaocha auto-discovery over all `test/**/*_test.clj` offline, skipping only `^:kaocha/skip` deftests. No mecab env needed.

- [ ] **Step 1: Read the current derivation**

```bash
cd /home/bor/Projects/abc/.worktrees/simplify-focused-test
sed -n '423,456p' flake.nix
```

- [ ] **Step 2: Rewrite the derivation**

Replace:
```nix
          clj-nix-focused-tests =
            pkgs.runCommand "abc-clj-nix-focused-tests"
              {
                nativeBuildInputs = [
                  pkgs.clojure
                  pkgs.git-cliff
                  pkgs.libxml2
                ];
              }
              ''
                cp -R ${./.} source
                chmod -R u+w source
                cd source
                cp ${./nix/clj-nix-deps.edn} deps.edn

                export HOME="${cljDepsCache}"
                export JAVA_TOOL_OPTIONS="-Duser.home=${cljDepsCache}"
                export CLJ_CONFIG="$HOME/.clojure"
                export CLJ_CACHE="$TMPDIR/cp-cache"
                export XDG_CONFIG_HOME="$TMPDIR/xdg-config"
                export GITLIBS="$HOME/.gitlibs"

                export ABC_TEI_SCHEMA_SKIP=1

                clojure -M:abc/focused-test

                mkdir -p "$out"
                echo "ABC focused Clojure tests passed with clj-nix dependency cache." > "$out/result.txt"
              '';
```
with:
```nix
          clj-nix-focused-tests =
            pkgs.runCommand "abc-clj-nix-focused-tests"
              {
                nativeBuildInputs = [
                  pkgs.clojure
                  pkgs.git-cliff
                  pkgs.libxml2
                ];
              }
              ''
                cp -R ${./.} source
                chmod -R u+w source
                cd source

                export HOME="${cljDepsCache}"
                export JAVA_TOOL_OPTIONS="-Duser.home=${cljDepsCache}"
                export CLJ_CONFIG="$HOME/.clojure"
                export CLJ_CACHE="$TMPDIR/cp-cache"
                export XDG_CONFIG_HOME="$TMPDIR/xdg-config"
                export GITLIBS="$HOME/.gitlibs"

                # The Nix sandbox has no network; tests needing the upstream TEI
                # RelaxNG schema (fetched at app build time, not test time) skip
                # via this flag. End-to-end TEI validation runs via the
                # `nix run .#validate-design-bundle` app, not here.
                export ABC_TEI_SCHEMA_SKIP=1

                clojure -M:test:kaocha

                mkdir -p "$out"
                echo "ABC Clojure tests passed with clj-nix dependency cache (kaocha auto-discovery)." > "$out/result.txt"
              '';
```

Key changes: (a) no `cp ${./nix/clj-nix-deps.edn} deps.edn` — use root `deps.edn`; (b) `clojure -M:abc/focused-test` → `clojure -M:test:kaocha`; (c) no mecab env (Phase 1 removed the dep; `abc.annotation` loads without it).

- [ ] **Step 3: Build and verify**

```bash
nix build .#clj-nix-focused-tests -L 2>&1 | tee tmp/kaocha-nix-build.log
```

Expected: build succeeds. kaocha summary reports baseline N (Phase 0 Step 2) + 2 (`abc.aozora-test`'s `entity-test` + `csv-cell-transformer-decodes-leaf-types` now runnable). 0 FAIL, 0 ERROR. Compare `grep -E "Ran [0-9]+ tests|FAIL|ERROR" tmp/kaocha-nix-build.log` to `docs/superpowers/notes/2026-07-04-focused-test-baseline.txt`.

- [ ] **Step 4: Commit**

```bash
git add flake.nix
git commit -m "build: switch CI tests to clojure -M:test:kaocha with full classpath

Replaces :abc/focused-test's explicit duplicated namespace list with kaocha
auto-discovery against the full deps-lock.json. No mecab env needed
(clj-mecab removed in Phase 1). No deps.edn swap-in — root deps.edn is the
single source of truth. Deftests needing absent runtime resources (live
DB zip, real git repo) are ^:kaocha/skip-marked."
```

### Task 6: Delete the focused-test machinery

**Files:**
- DELETE: `nix/clj-nix-deps.edn`
- DELETE: `nix/.focused-test-legacy-allowlist`
- DELETE: `nix/check-focused-test-coverage.sh`
- MODIFY: `flake.nix` (remove `checks.focused-test-coverage`; remove `contract-surface`'s `nix/clj-nix-deps.edn` existence check)

**Interfaces:**
- Consumes: Task 5's green kaocha build
- Produces: no `:abc/focused-test` alias anywhere; `nix/` contains only `ci-empty-local-pkgs/default.nix`.

- [ ] **Step 1: Delete the three files**

```bash
cd /home/bor/Projects/abc/.worktrees/simplify-focused-test
git rm nix/clj-nix-deps.edn nix/.focused-test-legacy-allowlist nix/check-focused-test-coverage.sh
```

- [ ] **Step 2: Remove the `focused-test-coverage` check from `flake.nix`**

Find:
```nix
          focused-test-coverage =
            pkgs.runCommand "abc-focused-test-coverage" { nativeBuildInputs = [ pkgs.python3 ]; }
              ''
                cp -R ${./.} source
                chmod -R u+w source
                cd source
                bash nix/check-focused-test-coverage.sh
                mkdir -p "$out"
                echo "focused-test allowlist covers all *_test.clj (ratcheted)." > "$out/result.txt"
              '';
```
Replace with: (remove the block; collapse any double-blank).

- [ ] **Step 3: Remove the `nix/clj-nix-deps.edn` existence check from `contract-surface`**

Find in the `contract-surface` block:
```bash
            test -f ${./nix/clj-nix-deps.edn}
```
Replace with: (remove that single line; the rest of the `contract-surface` `test -f` checklist stands.)

- [ ] **Step 4: Verify nothing else references the deleted files**

```bash
cd /home/bor/Projects/abc/.worktrees/simplify-focused-test
grep -rn "clj-nix-deps\|focused-test-coverage\|focused-test-legacy-allowlist\|:abc/focused-test" . --include='*.nix' --include='*.sh' --include='*.edn' --include='*.md' --include='*.clj' 2>/dev/null | grep -v -E '^\./(\.git|docs/superpowers/(plans|notes))'
```

Expected: no hits. The `prolog-cross-artifact` derivation's comment referencing "clj-nix-focused-tests" is fine — it cites the *check* (still exists under that name). If any ADR / handoff references the deleted machinery, note it for Task 8.

- [ ] **Step 5: VERIFY against the baseline — diff observable behavior**

```bash
nix build .#clj-nix-focused-tests -L 2>&1 | tee tmp/verify-final.log
echo "=== baseline ==="
grep -E "Ran [0-9]+ tests|[0-9]+ assertions" docs/superpowers/notes/2026-07-04-focused-test-baseline.txt
echo "=== final ==="
grep -E "Ran [0-9]+ tests|[0-9]+ failures|FAIL|ERROR" tmp/verify-final.log
echo "=== skipped deftests ==="
grep -rn 'kaocha/skip' test/
```

Expected: final ≥ baseline N + 2 (`abc.aozora-test` newly runnable). 0 failures/errors. Every skipped deftest corresponds to a Task 3 edit or pre-existing skip (`parse-aozora-text`, `aozora2html-test`, `extract-texts-test`). No test that *should* run is skipped.

- [ ] **Step 6: Confirm unaffected checks still build**

```bash
nix build .#adr-acceptance-criteria -L 2>&1 | tail -3
nix build .#prolog-cross-artifact -L 2>&1 | tail -3
```

Expected: both pass (guards against an accidental edit to their deps).

- [ ] **Step 7: Commit**

```bash
git add -A
git commit -m "build: delete the focused-test machinery

Removes:
- nix/clj-nix-deps.edn (hand-curated trimmed deps.edn)
- nix/.focused-test-legacy-allowlist (10-namespace exclusion list)
- nix/check-focused-test-coverage.sh (bash+Python regex parser of
  :abc/focused-test's -e string, ~130 lines)
- the :abc/focused-test alias (defined in nix/clj-nix-deps.edn)
- the focused-test-coverage Nix check
- the contract-surface assertion for nix/clj-nix-deps.edn

Replaced by kaocha auto-discovery (Task 5) + full deps-lock.json (Task 4).
The allowlist's reasons are now moot: clj-mecab removed (Phase 1);
java-time/clj-commons-fs/tawny-owl/timbre in the full lockfile; resource-
dependent deftests ^:kaocha/skip-marked (Task 3); 3 XTDB entries' files
were already deleted by XTDB removal."
```

### Task 7: Refresh `README.md` and stale ADR references

**Files:**
- Modify: `README.md` (Testing section)
- Modify: any `docs/adr/*.md` Task 6 Step 4 flagged

**Interfaces:**
- Consumes: Task 6 Step 4's grep output

- [ ] **Step 1: Fix README's Testing section**

Find:
```markdown
Run all tests:

```bash
clojure -Atest:runner
```

Continuously running test process for use during development:

```bash
boot watch deps-test bat-test
```
```
Replace with:
```markdown
Run all tests:

```bash
clojure -M:test:kaocha
# or: ./bin/kaocha
```

In the Nix sandbox / CI, tests run via the `clj-nix-focused-tests` check
(`nix build .#clj-nix-focused-tests`) with clj-nix's offline classpath.
```

Also fix any `boot` mention in the "Usage" / "Interactive Access" sections (no `boot` in the devShell).

- [ ] **Step 2: Patch flagged ADRs**

For each `docs/adr/*.md` hit in Task 6 Step 4: append a dated superseded note at the bottom — `"> 2026-07-04: the :abc/focused-test machinery described here was removed; CI tests now run via kaocha auto-discovery against the full deps-lock.json. clj-mecab was also removed (parsing is in the Rust ab-validator repo)."`. Don't rewrite history.

- [ ] **Step 3: Commit**

```bash
git add README.md docs/adr/
git commit -m "docs: refresh test instructions and ADRs after focused-test + clj-mecab removal"
```

---

## Self-Review

**1. Spec coverage:**
- "Delete the machinery" → Task 6. ✅
- "mecab (or clj-mecab) should be removed" → Phase 1 (Task 1 src + Task 2 devShell). ✅
- "keep the schema definitions" → Task 1 Step 3 explicitly preserves `:mecab.features/*` in `abc.annotation.schema`; schema ns untouched. ✅
- "parsing will be done in sibling ab-validator repo in rust" → Phase 1 neutralizes the two in-Clojure parse call sites (tokens `[]`, `compute-text` re-signatured); contract surface (schema) kept for the Rust parser to conform to. ✅
- clj-nix-native path → Task 5 uses `mk-deps-cache` (already present) + `:test:kaocha`; `mkCljBin`/`mkCljLib` not introduced (project has multiple tool `-main`s, no single app main — `runCommand` + `mk-deps-cache` is the right fit, minimally invasive). ✅
- README + ADRs → Task 7. ✅
- Auto deps-lock.json PR GitHub Action (audit follow-up) → NOT in this plan (additive, separate concern). ✅ (deliberate)

**2. Placeholder scan:** No "TBD"/"implement later." Task 1 Step 4's `compute-text` re-signature is concrete (map of line→tokens). Task 3's deftest edits cite exact deftest names.

**3. Type / name consistency:** `:abc/focused-test` (the alias, deleted in Task 6 via the file `nix/clj-nix-deps.edn`). `clj-nix-focused-tests` (the Nix *check*, kept; result.txt message updated). `bin/kaocha` (unchanged wrapper). `compute-text` (re-signatured in Task 1 Step 4, test updated in Step 5). `:mecab.features/*` (schema, kept throughout). `:sentence/tokens` (kept as a key; value `[]` in `parse-with-tags`). Consistent.

## Notes for the implementer

- **Phase 0 is mandatory** — it's the PROTECT baseline. Skipping forfeits Task 6 Step 5's verification.
- **Phase 1 before Phase 2.** Removing clj-mecab first simplifies the focused-test machinery: `abc.annotation-test` no longer needs an allowlist entry, and the devShell/check derivation needs no mecab env in Task 5.
- **Task 1 Step 4 changes `compute-text`'s signature.** Its only caller is `text_test` (stubbed). If a grep in Step 6 finds another caller, **stop** and reconcile — the signature change must cover all callers.
- **Task 4's lockfile regeneration needs network.** Run outside the Nix sandbox.
- **Task 5's kaocha build is the load-bearing verification.** If it fails on a missing dep, `deps-lock.json` is incomplete (back to Task 4). If it fails on a load error, a namespace needs a dep not in root `deps.edn` (add it, re-run Task 4). If a `^:kaocha/skip`'d deftest's *namespace* fails to load, the full classpath doesn't cover it — add the lib or mark the namespace's tests.
- **The `prolog-cross-artifact` derivation's comment** references "clj-nix-focused-tests derivation via the emitter." That comment is about a different concern (Prolog fact-file regeneration) and may itself be stale post-XTDB; leave it for a separate cleanup, don't fold it in.
- This plan does **not** add the clj-nix GitHub Action for auto `deps-lock.json` PRs. Worth a separate small PR after this lands.
