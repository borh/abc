# Nix Flake Quality Remediation Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Remove duplication and non-idiomatic constructs from the three repo flakes (`flake.nix`, `abc/flake.nix`, `ab-validator/flake.nix`) and their helper `.nix` files, without changing what any flake output builds.

**Architecture:** These are behavior-preserving refactors. The correctness oracle is **`drvPath` invariance**: a pure factoring that yields the same derivation attrs produces a byte-identical `drvPath`. Task 1 builds a snapshot harness that records the `drvPath` (or, for apps, the `program` path) of every output of all three flakes on the current system. Every later refactor task re-runs the harness and asserts an empty diff. Three findings intentionally change the builder text (routing checks through `mkSmokeCheck`, fixing broken heredoc indentation, dropping no-op `runHook` calls); those tasks swap the `drvPath`-diff oracle for a `nix build` success + output-identity oracle, which is called out explicitly in each.

**Tech Stack:** Nix flakes (`nixpkgs` unstable), `nixfmt`, `rustPlatform.buildRustPackage`, `clj-nix`, `flake-utils`.

## Global Constraints

- **Behavior preservation is the top constraint.** No output may change what it builds. Default oracle: `drvPath` unchanged (Task 1 harness). Where a task intentionally changes `drvPath`, that task states its alternate oracle inline.
- **`path:` input re-locking.** The root flake consumes `abc` and `ab-validator` via `path:./abc` / `path:../abc` inputs recorded in `flake.lock`. After editing a sub-flake, the root flake sees stale content until re-locked. Every task that edits `abc/` or `ab-validator/` ends by running `nix flake lock` at the repo root before snapshotting root outputs. Snapshot each flake **at its own directory** (`nix eval ./abc#…`, `nix eval ./ab-validator#…`, `nix eval .#…`).
- **Formatting.** Run the repo formatter on every touched file: `nixfmt <file>`. The monorepo `monorepo-nix-format` check enforces `nixfmt --check` over all `*.nix`; a mis-formatted file fails CI.
- **Never change** pinned input revs, `fetchurl`/`fetchzip`/`fetchgit` hashes, `cargoLock.outputHashes` values, or the set of systems a flake targets — except Task 9, which is explicitly about the system-iteration idiom and changes no output.
- **Commit granularity.** One commit per task (or per sub-step where noted). Every commit message ends with the repo's required trailer block (see any recent `git log` entry).
- **Worktree.** Execute in a fresh git worktree on a feature branch under `.worktrees/` (project convention — see memory `work-in-worktrees`), created via `superpowers:using-git-worktrees` at execution start. Merge to `main` only after all gates pass. The prior `worktree-clojure-quality-remediation` worktree has already been removed; do not reuse that name.
- **Verification tooling floor.** All `nix eval`/`nix build` invocations assume the flake inputs are already fetched/cached in the local store (they are pinned and were built previously). No input revs or hashes change, so no network fetch of new inputs is expected.

---

## File Structure

Files touched, and each one's responsibility after this plan:

- **`flake.nix`** (root) — monorepo integration. Refactored: `lib` hoisted, `pkgsFor` helper, `prefixAttrs`/`optionalOutputAttrs` replaced with `lib` idioms, `monorepoScripts` wrapper helper. (Tasks 2.)
- **`abc/flake.nix`** — ABC dev/build flake. Refactored: `mkCljApp` app helper (Task 3), shared clj sandbox-env + copy-source prelude strings for checks (Task 4).
- **`ab-validator/flake.nix`** — Rust workspace flake (the largest). Refactored: `gaijiEnv`/`gaijiBuildInputs`/`mkUnscaffoldedStub`/`mkRustBin` helpers collapsing 6 Rust-bin derivations + 3 simple bins (Task 5); hand-rolled checks routed through `mkSmokeCheck` and the parity shell/check body deduped (Task 6); broken indentation fixed and no-op `runHook`s removed (Task 7); `vibrato-rkyv` hash-divergence comment added (folded into Task 5).
- **`nix/tei.nix`, `abc/nix/tei-profile-artifacts.nix`** — TEI artifact builders. Cross-referencing comment on the duplicated `teiP5Version` (Task 8).
- **`flake-drvpath-snapshot.sh`** (worktree root, **untracked**, never committed) — the verification harness (Task 1).

The plan is intentionally ordered low-risk → higher-risk: pure idiom cleanups first (Tasks 2–5, all `drvPath`-preserving), then the intentional-change tasks (6–7), then documentation reconciliation (8–9).

---

## Task 1: Build the drvPath-invariance harness

**Files:**
- Create: `flake-drvpath-snapshot.sh` (worktree root, untracked)

**Interfaces:**
- Produces: a script `./flake-drvpath-snapshot.sh <label>` that writes `snapshot-<label>.txt` — a sorted `attrpath => hash-or-program` manifest across `packages`, `checks`, `apps`, `devShells` for all three flakes on the current system. Later tasks call `./flake-drvpath-snapshot.sh before` / `after` and `diff` the two files.

- [ ] **Step 1: Write the harness script**

Create `flake-drvpath-snapshot.sh` at the worktree root:

```bash
#!/usr/bin/env bash
# Snapshot flake output identities (drvPath for derivations, program for apps)
# so a behavior-preserving refactor can be proven by an empty before/after diff.
# UNTRACKED dev tool — do not `git add`.
set -euo pipefail

label="${1:?usage: flake-drvpath-snapshot.sh <label>}"
out="snapshot-${label}.txt"
sys="$(nix eval --impure --raw --expr builtins.currentSystem)"

# flake-dir  output-attr  field-expression
# apps expose {type,program,meta}; everything else is a derivation with drvPath.
emit() {
  local dir="$1" output="$2" field="$3"
  nix eval --json "${dir}#${output}.${sys}" \
    --apply "set: builtins.mapAttrs (_: v: v.${field} or null) set" 2>/dev/null \
    | nix run nixpkgs#jq -- -r \
        --arg d "$dir" --arg o "$output" \
        'to_entries[] | "\($d) \($o).\(.key) => \(.value)"' \
    || echo "${dir} ${output} => <eval-error>"
}

{
  for dir in . ./abc ./ab-validator; do
    for output in packages checks devShells; do
      emit "$dir" "$output" drvPath
    done
    emit "$dir" apps program
  done
} | LC_ALL=C sort > "$out"

echo "wrote $out ($(wc -l < "$out") lines) for system $sys"
```

- [ ] **Step 2: Make it executable and produce the baseline**

Run:
```bash
chmod +x flake-drvpath-snapshot.sh
./flake-drvpath-snapshot.sh baseline
```
Expected: prints `wrote snapshot-baseline.txt (N lines) for system x86_64-linux` with N in the low hundreds. If any line shows `<eval-error>` or `=> null`, investigate before proceeding — the harness must fully evaluate every output for the oracle to be trustworthy. (A legitimate `null` only occurs if an app lacks `program`; none should.)

- [ ] **Step 3: Sanity-check the baseline is stable (idempotent)**

Run:
```bash
./flake-drvpath-snapshot.sh baseline2
diff snapshot-baseline.txt snapshot-baseline2.txt && echo "STABLE"
```
Expected: `STABLE` (empty diff). This proves the harness itself is deterministic, so any later non-empty diff is caused by the edit, not eval noise.

- [ ] **Step 4: Exclude the harness and snapshots from git**

Run:
```bash
printf '%s\n' 'flake-drvpath-snapshot.sh' 'snapshot-*.txt' >> .git/info/exclude
git status --porcelain | grep -E 'flake-drvpath|snapshot-' && echo "STILL VISIBLE (fix exclude)" || echo "properly ignored"
```
Expected: `properly ignored`. The harness is a scaffold, not a deliverable.

- [ ] **Step 5: No commit for this task** — the harness stays untracked. Proceed to Task 2.

---

## Task 2: Root `flake.nix` idiom cleanup (findings 10–13)

Replace hand-rolled attr helpers with `lib` idioms, hoist `lib`, add a `pkgsFor` helper, and DRY the `monorepoScripts` wrappers. All changes are pure attribute-level factoring → **`drvPath`-preserving**.

**Files:**
- Modify: `flake.nix` (root)

**Interfaces:**
- Produces (internal): `lib` bound at top-level `let`; `pkgsFor = system: import nixpkgs { inherit system; }`; `prefixAttrs = prefix: lib.mapAttrs' (n: v: lib.nameValuePair "${prefix}${n}" v)`; `optionalOutputAttrs = flake: outputName: system: lib.attrByPath [ outputName system ] { } flake`; `mkWrappedScript` inside `monorepoScripts`.

- [ ] **Step 1: Snapshot before**

Run: `./flake-drvpath-snapshot.sh t2-before`

- [ ] **Step 2: Hoist `lib` and add `pkgsFor`**

In the top-level `let` (currently begins at `systems = [ … ]`), add these two bindings immediately after `forAllSystems`:

```nix
      lib = nixpkgs.lib;

      pkgsFor = system: import nixpkgs { inherit system; };
```

Then, in each of `formatter`, `apps`, `checks`, `packages`, `devShells`, replace the line
```nix
          pkgs = import nixpkgs { inherit system; };
```
with
```nix
          pkgs = pkgsFor system;
```
and in `devShells` delete the now-redundant `inherit (nixpkgs) lib;` line (line ~402) since `lib` is now in scope from the outer `let`.

- [ ] **Step 3: Replace `prefixAttrs` with `lib.mapAttrs'`**

Replace the whole `prefixAttrs = …;` binding (currently the `builtins.listToAttrs (map …)` block) with:

```nix
      prefixAttrs = prefix: lib.mapAttrs' (name: value: lib.nameValuePair "${prefix}${name}" value);
```

- [ ] **Step 4: Replace `optionalOutputAttrs` with `lib.attrByPath`**

Replace the whole `optionalOutputAttrs = …;` binding with:

```nix
      optionalOutputAttrs = flake: outputName: system: lib.attrByPath [ outputName system ] { } flake;
```

- [ ] **Step 5: DRY the `monorepoScripts` wrappers**

Inside `monorepoScripts pkgs`, in its `let`, add a helper after `runtimePath`:

```nix
          mkWrappedScript =
            name: body:
            pkgs.writeShellScript name ''
              set -euo pipefail
              export PATH="${runtimePath}:$PATH"
              ${body}
            '';
```

Then rewrite the returned attrset so the four simple wrappers use it (keeping `validate-migration` explicit because its body is multi-line):

```nix
        {
          schema-drift = mkWrappedScript "soranoha-schema-drift" ''exec bash scripts/monorepo-schema-drift.sh "$@"'';
          tei-version-coherence = mkWrappedScript "soranoha-tei-version-coherence" ''exec bash scripts/monorepo-tei-version-coherence.sh "$@"'';
          flake-input-policy = mkWrappedScript "soranoha-flake-input-policy" ''exec python scripts/monorepo-flake-input-policy.py "$@"'';
          python-quality = mkWrappedScript "soranoha-python-quality" ''exec bash scripts/python-quality.sh "$@"'';

          validate-migration = mkWrappedScript "soranoha-validate-migration" ''
            bash tests/monorepo-active-path-hygiene-smoke.sh
            bash scripts/monorepo-schema-drift.sh
            bash scripts/monorepo-tei-version-coherence.sh
            python scripts/monorepo-flake-input-policy.py
            nix flake check --no-build "$@"
          '';
        };
```

> ⚠️ **`drvPath` note for Step 5:** `writeShellScript` hashes the exact script text. The original wrappers put `exec …` on its own line after the `export PATH` line; `mkWrappedScript` must reproduce that whitespace/newline layout so the generated script text is byte-identical. If Step 7's diff shows only the four wrapper apps changing, the body string differs by whitespace — adjust `mkWrappedScript`'s `${body}` placement until the diff is empty. This is exactly what the harness is for.

- [ ] **Step 6: Format**

Run: `nixfmt flake.nix`
Expected: exits 0, no error.

- [ ] **Step 7: Snapshot after and prove invariance**

Run:
```bash
nix flake lock   # root only edited; harmless, keeps lock coherent
./flake-drvpath-snapshot.sh t2-after
diff snapshot-t2-before.txt snapshot-t2-after.txt && echo "INVARIANT — behavior preserved"
```
Expected: `INVARIANT — behavior preserved` (empty diff). A non-empty diff means the refactor changed a derivation — reconcile per the Step 5 note before committing.

- [ ] **Step 8: Commit**

```bash
git add flake.nix
git commit -m "refactor(flake): use lib idioms for attr helpers and DRY monorepo script wrappers"
# (append the repo's required commit trailer block)
```

---

## Task 3: `abc/flake.nix` app DRY — `mkCljApp` helper (finding 8)

Collapse ~10 copy-pasted Clojure-launcher `apps` blocks into one helper. The `program` each produces is a `writeShellScript` whose text is unchanged → **`drvPath`/`program`-preserving**.

**Files:**
- Modify: `abc/flake.nix`

**Interfaces:**
- Consumes: existing `mkCljLauncher { name, alias, env ? "" }` (unchanged).
- Produces: `mkCljApp = { name, alias, description, env ? "" }: { type = "app"; program = toString (mkCljLauncher { inherit name alias env; }); meta.description = description; }`.

- [ ] **Step 1: Snapshot before**

Run: `./flake-drvpath-snapshot.sh t3-before`

- [ ] **Step 2: Add the `mkCljApp` helper**

In the `apps` `let` block, immediately after the `mkCljLauncher = …;` binding, add:

```nix
          mkCljApp =
            {
              name,
              alias,
              description,
              env ? "",
            }:
            {
              type = "app";
              program = toString (mkCljLauncher { inherit name alias env; });
              meta.description = description;
            };
```

- [ ] **Step 3: Rewrite the launcher-backed apps using the helper**

Replace each of these app attributes with the `mkCljApp` form. The `name`, `alias`, `env`, and `meta.description` values are copied verbatim from the current blocks — do not alter any string:

```nix
          validate-design-bundle = mkCljApp {
            name = "abc-validate-design-bundle";
            alias = "abc/validate-design-bundle";
            description = "Validate ABC v0 design-bundle schemas and fixtures";
            env = ''
              export PATH="${
                pkgs.lib.makeBinPath [
                  pkgs.git-cliff
                  pkgs.libxml2
                ]
              }:''${PATH:-}"
              export TEI_SCHEMA_PATH="${tei.teiAllSchema}"
            '';
          };

          soranoha = mkCljApp {
            name = "soranoha";
            alias = "abc/soranoha";
            description = "Soranoha snapshot publication command dispatcher";
          };

          materialize-import = mkCljApp {
            name = "abc-materialize-import";
            alias = "abc/materialize-import";
            description = "Materialize imported ab-validator output as ABC manifests";
          };

          materialize-publication = mkCljApp {
            name = "abc-materialize-publication";
            alias = "abc/materialize-publication";
            description = "Materialize parser-IR publication plaintext and TEI artifacts";
          };

          materialize-publications-batch = mkCljApp {
            name = "abc-materialize-publications-batch";
            alias = "abc/materialize-publication";
            description = "Materialize parser-IR publication plaintext and TEI artifacts from a batch JSON";
          };

          materialize-source-snapshot = mkCljApp {
            name = "abc-materialize-source-snapshot";
            alias = "abc/materialize-source-snapshot";
            description = "Materialize a source corpus snapshot and source manifests";
          };

          manifest-to-rdf = mkCljApp {
            name = "abc-manifest-to-rdf";
            alias = "abc/manifest-to-rdf";
            description = "Generate deterministic RDF/Turtle view from an ABC manifest";
          };

          aozora-ingest = mkCljApp {
            name = "abc-aozora-ingest";
            alias = "abc/aozora-ingest";
            description = "Build a metadata-record JSON from the canonical (pinned) Aozora catalog, or a --zip slice";
            env = ''
              export ABC_AOZORA_CATALOG_ZIP="${aozorabunko-src}/index_pages/list_person_all_extended_utf8.zip"
              export ABC_AOZORA_CATALOG_URL="github:aozorabunko/aozorabunko/0e9ea3e586eb0aa34039fabfc85a407d2f98b165"
            '';
          };

          validate-corpus = mkCljApp {
            name = "abc-validate-corpus";
            alias = "abc/validate-corpus";
            description = "Validate an ingested corpus directory through SHACL";
          };

          person-drift-history = mkCljApp {
            name = "abc-person-drift-history";
            alias = "abc/person-drift-history";
            description = "Audit generated corpus snapshots for conservative person split/merge candidates";
          };

          aozora-history-audit = mkCljApp {
            name = "abc-aozora-history-audit";
            alias = "abc/aozora-history-audit";
            description = "Extract two Aozora git refs, ingest them, validate current corpus, and report person drift candidates";
          };
```

**Leave unchanged** the apps whose `program` is a bespoke `writeShellScript` rather than `mkCljLauncher`: `aozora-upstream-audit`, `regenerate-tei-profile`, `tei-eaj-aozora-tei-source`, and all the `mkTeiEajAozoraReportApp`-based apps. They are not launcher-backed and are out of scope for this helper.

- [ ] **Step 4: Format**

Run: `nixfmt abc/flake.nix`

- [ ] **Step 5: Snapshot after and prove invariance**

Run:
```bash
nix flake lock            # re-lock root against edited abc
./flake-drvpath-snapshot.sh t3-after
diff snapshot-t3-before.txt snapshot-t3-after.txt && echo "INVARIANT — behavior preserved"
```
Expected: `INVARIANT`. This checks both `abc#apps.*` and the root's re-exported `abc-*` apps. Any diff → the `env`/`name`/`alias`/`description` moved into `mkCljApp` differs from the original; reconcile until empty.

- [ ] **Step 6: Commit**

```bash
git add abc/flake.nix
git commit -m "refactor(abc/flake): collapse Clojure-launcher apps into mkCljApp helper"
# (append the repo's required commit trailer block)
```

---

## Task 4: `abc/flake.nix` check DRY — shared prelude strings (finding 9)

The `checks` derivations repeat two boilerplate blocks: (a) `cp -R ${./.} source; chmod -R u+w source; cd source`, and (b) the 6-line Clojure sandbox-env export block. Factor both into `let`-bound strings. Because the builder text must stay byte-identical, this is **`drvPath`-preserving only if the interpolated strings reproduce the originals exactly** — the harness is the gate.

**Files:**
- Modify: `abc/flake.nix`

**Interfaces:**
- Produces (internal): `copyWritableSource` (string) and `cljSandboxEnv` (string) in the `checks` `let`.

- [ ] **Step 1: Snapshot before**

Run: `./flake-drvpath-snapshot.sh t4-before`

- [ ] **Step 2: Add the shared prelude strings**

In the `checks` `let` block (after `contractSurfacePaths`), add:

```nix
          copyWritableSource = ''
            cp -R ${./.} source
            chmod -R u+w source
            cd source
          '';

          cljSandboxEnv = ''
            export HOME="${cljDepsCache}"
            export JAVA_TOOL_OPTIONS="-Duser.home=${cljDepsCache}"
            export CLJ_CONFIG="$HOME/.clojure"
            export CLJ_CACHE="$TMPDIR/cp-cache"
            export XDG_CONFIG_HOME="$TMPDIR/xdg-config"
            export GITLIBS="$HOME/.gitlibs"
          '';
```

> ⚠️ **Byte-identity requirement:** in the current `clj-nix-focused-tests` and `diagram-drift` derivations, `CLJ_CONFIG` is written as `"$HOME/.clojure"` (not `"${cljDepsCache}/.clojure"`). Reproduce it exactly as above. Indentation inside a Nix `''`-string is stripped to the common minimum, so the relative indentation of these lines against the surrounding builder text must match the originals for the produced script to be identical. Verify with Step 5's diff, not by eye.

- [ ] **Step 3: Use the strings in the two clj checks**

In `clj-nix-focused-tests`, replace the literal
```nix
                cp -R ${./.} source
                chmod -R u+w source
                cd source

                export HOME="${cljDepsCache}"
                export JAVA_TOOL_OPTIONS="-Duser.home=${cljDepsCache}"
                export CLJ_CONFIG="$HOME/.clojure"
                export CLJ_CACHE="$TMPDIR/cp-cache"
                export XDG_CONFIG_HOME="$TMPDIR/xdg-config"
                export GITLIBS="$HOME/.gitlibs"
```
with
```nix
                ${copyWritableSource}
                ${cljSandboxEnv}
```
Apply the identical substitution in `diagram-drift`.

- [ ] **Step 4: Use `copyWritableSource` in the remaining copy-source checks**

For each of `clj-kondo`, `aat-parser-ir-probe-tests`, `schema-contract-drift`, `adr-acceptance-criteria`, and `prolog-cross-artifact`, replace their leading
```nix
                cp -R ${./.} source
                chmod -R u+w source
                cd source
```
(note: some use `+w`, some `u+w` — **preserve each one's exact flag**; only replace the blocks that are literally `chmod -R u+w`) with `${copyWritableSource}`.

> ⚠️ Check each site's `chmod` flags first (`u+w` vs `+w`) and its indentation. `copyWritableSource` uses `chmod -R u+w`. If a site uses `chmod -R +w`, **skip it** (leave inline) — folding it would change the builder text and thus `drvPath`. Only fold exact matches. Record any skipped sites in the commit message.

- [ ] **Step 5: Format, re-lock, snapshot, prove invariance**

Run:
```bash
nixfmt abc/flake.nix
nix flake lock
./flake-drvpath-snapshot.sh t4-after
diff snapshot-t4-before.txt snapshot-t4-after.txt && echo "INVARIANT — behavior preserved"
```
Expected: `INVARIANT`. Any changed check `drvPath` means the interpolated prelude diverged from the original text — reconcile until empty (this is the whole point of choosing a byte-exact oracle here).

- [ ] **Step 6: Commit**

```bash
git add abc/flake.nix
git commit -m "refactor(abc/flake): share clj sandbox-env and copy-source preludes across checks"
# (append the repo's required commit trailer block)
```

---

## Task 5: `ab-validator/flake.nix` — Rust-bin helpers (findings 1, 2, 3, 7)

The largest reduction. Extract `gaijiEnv`, `gaijiBuildInputs`, `mkUnscaffoldedStub`, and a `mkRustBin` skeleton; re-express the 6 workspace-gated derivations (`abValidator`, `workspaceCheck`, `abAatToParserIr`, `abMorphRun`, `abIndex`, `abCheck`) and the 3 simple bins (`taxonomyGenerator`, `sourceInventoryBin`, `abOracleBin`) through them. Add the `vibrato-rkyv` hash-divergence comment (finding 7). All **`drvPath`-preserving** — the helper must reproduce each derivation's exact attr set.

**Files:**
- Modify: `ab-validator/flake.nix`

**Interfaces:**
- Produces (internal): `gaijiEnv` (attrset of 3 `AB_AOZORA_RS_GAIJI_*`), `gaijiBuildInputs` (list: `pdfium-binaries` ++ darwin optionals), `mkUnscaffoldedStub` (name → writeShellApplication), and:
  ```nix
  mkRustBin = { pname, cargoBuildFlags ? null, nativeBuildInputs ? [ pkgs.pkg-config ],
                buildInputs ? [ ], env ? { }, doCheck ? false, gated ? true,
                stub ? (mkUnscaffoldedStub pname), extra ? { } }: <derivation-or-stub>
  ```

**Per-site parameter table** (the exact inputs each call site passes — this is the complete spec; the helper + this table fully determine every derivation):

| binding | pname | cargoBuildFlags | nativeBuildInputs | buildInputs | env | doCheck | gated | extra / stub |
|---|---|---|---|---|---|---|---|---|
| `abValidator` | `ab-validator` | (none) | `[pkg-config python3 zstd]` | `gaijiBuildInputs` | `gaijiEnv // { AB_ABC_ROOT }` | `true` | yes | `extra = { preCheck = vibratoDictionaryPreCheck; }` |
| `workspaceCheck` | `ab-validator-check` | `["--workspace"]` | `[pkg-config python3 zstd]` | `gaijiBuildInputs` | `gaijiEnv // { AB_ABC_ROOT }` | `true` | yes | `extra = { cargoTestFlags = ["--workspace" "--features" "ab-morph-run/test-analyzer"]; }`; `stub = pkgs.runCommand "ab-validator-workspace-not-yet-scaffolded" { } ''touch "$out"''` |
| `abAatToParserIr` | `ab-aat-to-parser-ir` | `["--package" "ab-aat-to-parser-ir"]` | `[pkg-config zstd]` | `gaijiBuildInputs` | `gaijiEnv // { AB_ABC_ROOT }` | `false` | yes | (default stub) |
| `abMorphRun` | `ab-morph-run` | `["--package" "ab-morph-run"]` | `[pkg-config zstd]` | `gaijiBuildInputs` | `gaijiEnv // { AB_ABC_ROOT }` | `false` | yes | (default stub) |
| `abIndex` | `ab-index` | `["--package" "ab-index"]` | `[pkg-config]` (default) | `[]` (default) | `{ AB_ABC_ROOT }` | `false` | yes | (default stub) |
| `abCheck` | `ab-check` | `["--package" "ab-check"]` | `[pkg-config]` (default) | `[]` (default) | `{ AB_ABC_ROOT }` | `false` | yes | (default stub) |
| `taxonomyGenerator` | `ab-taxonomy-generator` | `["--package" "ab-coverage" "--bin" "generate_taxonomy"]` | `[]` | `[]` (default) | `{ }` (default) | `false` | `false` | — |
| `sourceInventoryBin` | `ab-source-inventory` | `["--package" "ab-coverage" "--bin" "ab-source-inventory"]` | `[]` | `[]` (default) | `{ }` (default) | `false` | `false` | — |
| `abOracleBin` | `ab-oracle` | `["--package" "ab-oracle"]` | `[]` | `[]` (default) | `{ }` (default) | `false` | `false` | — |

where `AB_ABC_ROOT` denotes `AB_ABC_ROOT = "${abcSchemaRootForNix}";`.

- [ ] **Step 1: Snapshot before**

Run: `./flake-drvpath-snapshot.sh t5-before`

- [ ] **Step 2: Add the helpers**

In the big `let` (place after `abCargoDeps`, before the first `rustPlatform.buildRustPackage` call site — e.g. just above `buildRustUpstreamParser` or near `abCargoDeps`; location does not affect `drvPath`), add:

```nix
        # Shared gaiji provisioning for the CLIs/adapters whose build.rs (via
        # third_party/aozora-rs-gaiji) needs the pinned JIS X 0213 menkuten
        # table, the Aozora gaiji_chuki PDF, and a pdfium binary. See
        # aozoraRsAdapter for the full rationale.
        gaijiEnv = {
          AB_AOZORA_RS_GAIJI_MENKUTEN_PATH = "${aozoraRsGaijiMenkuten}";
          AB_AOZORA_RS_GAIJI_CHUKI_PDF = "${aozoraRsGaijiChukiPdf}";
          AB_AOZORA_RS_GAIJI_PDFIUM_DIR = "${pkgs.pdfium-binaries}/lib";
        };

        gaijiBuildInputs = [
          pkgs.pdfium-binaries
        ]
        ++ lib.optionals pkgs.stdenv.isDarwin [
          pkgs.libiconv
          pkgs.darwin.apple_sdk.frameworks.Security
          pkgs.darwin.apple_sdk.frameworks.SystemConfiguration
        ];

        # Stub for a workspace CLI when the Rust workspace is not scaffolded
        # (no Cargo.toml/Cargo.lock). Defined once instead of re-inlined per CLI.
        mkUnscaffoldedStub =
          name:
          pkgs.writeShellApplication {
            inherit name;
            text = ''
              echo 'Rust workspace not scaffolded' >&2
              exit 1
            '';
          };

        # Common skeleton for a workspace Rust binary built from `source` against
        # the shared abCargoDeps vendor dir. Each call site passes only its real
        # differences (package flags, extra deps, gaiji opt-in, doCheck).
        mkRustBin =
          {
            pname,
            cargoBuildFlags ? null,
            nativeBuildInputs ? [ pkgs.pkg-config ],
            buildInputs ? [ ],
            env ? { },
            doCheck ? false,
            gated ? true,
            stub ? (mkUnscaffoldedStub pname),
            extra ? { },
          }:
          let
            drv = rustPlatform.buildRustPackage (
              {
                inherit
                  pname
                  nativeBuildInputs
                  buildInputs
                  doCheck
                  ;
                version = "0.1.0";
                src = source;
                cargoDeps = abCargoDeps;
              }
              // lib.optionalAttrs (cargoBuildFlags != null) { inherit cargoBuildFlags; }
              // env
              // extra
            );
          in
          if gated then (if hasCargoManifest && hasCargoLock then drv else stub) else drv;
```

> ⚠️ **Why `nativeBuildInputs`/`buildInputs` default to explicit `[…]`:** `abIndex`/`abCheck` omit `buildInputs` and the 3 simple bins omit both. `buildRustPackage` treats an omitted list and an explicit `[]` identically, so passing `[]` reproduces the original `drvPath`. Do not "optimize" by conditionally omitting them.

- [ ] **Step 3: Rewrite the 9 call sites per the table**

Replace each existing binding with its `mkRustBin` form. Worked examples for the three shapes (apply the analogous transform to the rest using the table):

`abValidator` (no cargoBuildFlags, has preCheck, doCheck=true, gated):
```nix
        abValidator = mkRustBin {
          pname = "ab-validator";
          nativeBuildInputs = [
            pkgs.pkg-config
            pkgs.python3
            pkgs.zstd
          ];
          buildInputs = gaijiBuildInputs;
          env = gaijiEnv // { AB_ABC_ROOT = "${abcSchemaRootForNix}"; };
          doCheck = true;
          extra = { preCheck = vibratoDictionaryPreCheck; };
        };
```

`abIndex` (lean, gated):
```nix
        abIndex = mkRustBin {
          pname = "ab-index";
          cargoBuildFlags = [
            "--package"
            "ab-index"
          ];
          env = { AB_ABC_ROOT = "${abcSchemaRootForNix}"; };
        };
```

`taxonomyGenerator` (ungated, no env):
```nix
        taxonomyGenerator = mkRustBin {
          pname = "ab-taxonomy-generator";
          nativeBuildInputs = [ ];
          cargoBuildFlags = [
            "--package"
            "ab-coverage"
            "--bin"
            "generate_taxonomy"
          ];
          gated = false;
        };
```

`workspaceCheck` (custom stub + cargoTestFlags):
```nix
        workspaceCheck = mkRustBin {
          pname = "ab-validator-check";
          nativeBuildInputs = [
            pkgs.pkg-config
            pkgs.python3
            pkgs.zstd
          ];
          buildInputs = gaijiBuildInputs;
          env = gaijiEnv // { AB_ABC_ROOT = "${abcSchemaRootForNix}"; };
          cargoBuildFlags = [ "--workspace" ];
          doCheck = true;
          stub = pkgs.runCommand "ab-validator-workspace-not-yet-scaffolded" { } ''touch "$out"'';
          extra = {
            cargoTestFlags = [
              "--workspace"
              "--features"
              "ab-morph-run/test-analyzer"
            ];
          };
        };
```

Apply the same pattern to `abAatToParserIr`, `abMorphRun`, `abCheck`, `sourceInventoryBin`, `abOracleBin` using their table rows. Delete the now-unused inline `if hasCargoManifest && hasCargoLock then … else …` blocks and the per-site darwin/gaiji repetition they contained.

- [ ] **Step 4: Add the `vibrato-rkyv` hash-divergence comment (finding 7)**

On the line in `mecabDicConverterCargoLock.outputHashes` reading `"vibrato-rkyv-0.7.7" = "sha256-M6ALF…";`, add an explanatory comment directly above it:

```nix
          outputHashes = {
            # NOTE: this vibrato-rkyv-0.7.7 hash intentionally differs from the
            # like-named key in `cargoGitOutputHashes` below — mecab-dic-converter
            # pins a different rev/tree of the fork than the ab-validator
            # workspace does, so the vendored source hashes are not the same key
            # by coincidence. Do not "deduplicate" these two values.
            "vibrato-rkyv-0.7.7" = "sha256-M6ALFpSjs9M+6tvCmn2ZTevUS7NBL6RmnE5GB/qVMEo=";
            "crawdad-rkyv-0.4.0-rkyv.2" = "sha256-FlSXUYHNFUIuEK4sLhbCKJsgRm/EKnHDu7VPpdpvu10=";
          };
```

Comments do not affect `drvPath`.

- [ ] **Step 5: Format**

Run: `nixfmt ab-validator/flake.nix`

- [ ] **Step 6: Snapshot after and prove invariance**

Run:
```bash
nix flake lock
./flake-drvpath-snapshot.sh t5-after
diff snapshot-t5-before.txt snapshot-t5-after.txt && echo "INVARIANT — behavior preserved"
```
Expected: `INVARIANT` — covering `ab-validator#packages.*` (all 9 rewritten bins), `ab-validator#checks.*` (`cargo-test`/`default`/`ab-validator` all alias `workspaceCheck`), and the root's `ab-validator-*` re-exports. **This is the highest-value correctness gate in the plan.** Any single changed `drvPath` names exactly which call site's attrs diverged from the table — fix that row until the diff is empty.

- [ ] **Step 7: Commit**

```bash
git add ab-validator/flake.nix
git commit -m "refactor(ab-validator/flake): extract gaijiEnv/mkRustBin, collapse 9 Rust-bin derivations"
# (append the repo's required commit trailer block)
```

---

## Task 6: `ab-validator/flake.nix` — route hand-rolled checks through `mkSmokeCheck` + dedupe parity body (finding 4)

`mkSmokeCheck` already abstracts the `mktemp -d; cp -R "${source}"; chmod -R +w; cd` boilerplate, but four checks hand-roll it. Routing them through `mkSmokeCheck` (and moving their inline logic into the smoke test scripts or `extraPreScript`) **changes the builder text → `drvPath` changes intentionally.** Oracle switches from `drvPath`-diff to **`nix build` success**.

> **Scope decision:** `aozora2htmlRustParityShell` (an app) and `aozora2htmlRustParityCheck` share a cargo-build + pytest body; those are the cleanest wins and are addressed here. `aozoraEpub3SmokeCheck` has an embedded Python heredoc that `mkSmokeCheck`'s single `testScript` shape does not cleanly fit; **do not force it into `mkSmokeCheck`** — its indentation is fixed separately in Task 7. `taxonomyDriftCheck` and `abcSchemaContractDriftCheck` use `diff`/`cmp` against `${source}` paths and a bespoke abc-root staging; they are left as-is (folding them would not reduce complexity meaningfully and risks behavior change). This task therefore does exactly two things: (a) dedupe the aozora2html parity body, (b) leave the other three hand-rolled checks for their dedicated tasks/as-is. Record this scoping in the commit message.

**Files:**
- Modify: `ab-validator/flake.nix`

**Interfaces:**
- Produces (internal): `aozora2htmlParityText` — a shared shell-body string parameterized by repo root, consumed by both `aozora2htmlRustParityShell` and `aozora2htmlRustParityCheck`.

- [ ] **Step 1: Record the current build identity of the affected checks**

Run:
```bash
nix eval --raw .#checks.x86_64-linux.aozora2html-rust-parity.drvPath
nix eval --raw .#apps.x86_64-linux.aozora2html-rust-parity.program
```
Note both values (they will change; that is expected).

- [ ] **Step 2: Extract the shared parity body**

In the big `let`, add a helper that produces the body given a repo-root expression. The two current sites differ only in which root they `cargo build` against (`$PWD`-or-`${source}` for the shell app, `$work_dir/source` for the check):

```nix
        # Shared body for the aozora2html Rust-mapper parity smoke: build the
        # mapper offline from the vendored deps, then run the pytest oracle.
        # `root` is the shell expression naming the checked-out repo root.
        aozora2htmlParityText =
          root:
          ''
            export AB_AOZORA2HTML_BIN="${aozora2htmlParser}/bin/aozora2html"
            cargo \
              --config "source.crates-io.replace-with='vendored-sources'" \
              --config "source.vendored-sources.directory='${aozora2htmlCargoDeps}'" \
              build --manifest-path "${root}/adapters/aozora2html/Cargo.toml" --release --offline
            python -m pytest "${root}/adapters/aozora2html/tests/test_mapper.py" -vv
          '';
```

- [ ] **Step 3: Rewrite the shell app to use it**

Replace `aozora2htmlRustParityShell`'s `text` body (keeping the `repo_root` discovery preamble) so the build/pytest tail comes from the helper:

```nix
        aozora2htmlRustParityShell = pkgs.writeShellApplication {
          name = "aozora2html-rust-parity";
          runtimeInputs = [
            pkgs.perl
            rustToolchain
            pythonWithAatSchemaDeps
          ];
          text = ''
            repo_root="$PWD"
            if [ ! -d "$repo_root/adapters/aozora2html" ]; then
              repo_root="${source}"
            fi
          ''
          + aozora2htmlParityText "$repo_root";
        };
```

> Note: `aozora2htmlParityText "$repo_root"` interpolates the **shell** variable `$repo_root` into the Nix string (it appears literally as `$repo_root` in the produced script). Confirm the produced script still references `"$repo_root/adapters/…"` after the change.

- [ ] **Step 4: Rewrite the check to use it**

Replace `aozora2htmlRustParityCheck`'s inline build/pytest lines with the helper applied to `$work_dir/source`:

```nix
        aozora2htmlRustParityCheck =
          pkgs.runCommand "aozora2html-rust-parity-check"
            {
              nativeBuildInputs = [
                pkgs.perl
                rustToolchain
                pythonWithAatSchemaDeps
              ];
            }
            ''
              work_dir="$(mktemp -d)"
              cp -R "${source}" "$work_dir/source"
              chmod -R +w "$work_dir/source"
              cd "$work_dir/source"
            ''
            + aozora2htmlParityText "$work_dir/source"
            + ''
              touch "$out"
            '';
```

- [ ] **Step 5: Format**

Run: `nixfmt ab-validator/flake.nix`

- [ ] **Step 6: Prove behavior with a real build (oracle: build success)**

Because `drvPath` changes here by design, build both the app and the check and confirm they still work:
```bash
nix build .#checks.x86_64-linux.aozora2html-rust-parity -L
nix build .#apps.x86_64-linux.aozora2html-rust-parity 2>/dev/null || true   # apps aren't buildable; eval instead:
nix eval --raw .#apps.x86_64-linux.aozora2html-rust-parity.program
${_program:-}   # optional: run the produced app binary --help if it supports it
```
Expected: the check builds green (`cp -Lr`/pytest all pass). If the parity pytest fails, the extracted body diverged from the original invocation — compare against the values recorded in Step 1's git history.

- [ ] **Step 7: Confirm no *other* output changed**

Run:
```bash
./flake-drvpath-snapshot.sh t6-after
diff snapshot-t5-after.txt snapshot-t6-after.txt
```
Expected: the **only** differing lines are `./ab-validator checks.aozora2html-rust-parity` and `./ab-validator apps.aozora2html-rust-parity` (and their root `ab-validator-*` re-exports). Any other changed line is collateral damage — revert and narrow the edit.

- [ ] **Step 8: Commit**

```bash
git add ab-validator/flake.nix
git commit -m "refactor(ab-validator/flake): share aozora2html parity body between app and check"
# (append the repo's required commit trailer block)
```

---

## Task 7: `ab-validator/flake.nix` — fix broken indentation and drop no-op `runHook`s (findings 5, 6)

`aozoraEpub3SmokeCheck` (~28-space over-indent with a column-0 Python heredoc) and `sudachiDictionaryFull` (mixed indentation + meaningless `runHook preInstall`/`postInstall` inside a `runCommand`) are the two least-readable blocks. Reindenting a `''`-string and removing `runHook` lines **changes the builder text → `drvPath` changes intentionally.** Bash is whitespace-insensitive at line starts and `runHook` with no phase defined is a no-op, so behavior is preserved. Oracle: **`nix build` success + output identity** (for `sudachiDictionaryFull`, whose output is a dictionary file that must be byte-identical).

**Files:**
- Modify: `ab-validator/flake.nix`

- [ ] **Step 1: Capture `sudachiDictionaryFull`'s current output for the identity oracle**

Run:
```bash
nix build .#packages.x86_64-linux.sudachi-dictionary-full --out-link result-sudachi-before
sha256sum result-sudachi-before/share/sudachi/system_full.dic
readlink -f result-sudachi-before/share/sudachi/system.dic
```
Record the sha256 and the symlink target.

- [ ] **Step 2: Rewrite `sudachiDictionaryFull` cleanly**

Replace the derivation's build script, removing the `runHook preInstall`/`runHook postInstall` no-ops and normalizing indentation:

```nix
        sudachiDictionaryFull =
          pkgs.runCommand "sudachi-dictionary-20260116-full"
            {
              nativeBuildInputs = [ pkgs.unzip ];
            }
            ''
              mkdir -p "$out/share/sudachi"
              unzip -j ${sudachiDictionaryFullZip} '*.dic' -d "$out/share/sudachi"
              dic="$(find "$out/share/sudachi" -maxdepth 1 -type f -name '*.dic' | head -n 1)"
              test -n "$dic"
              if [ "$dic" != "$out/share/sudachi/system_full.dic" ]; then
                mv "$dic" "$out/share/sudachi/system_full.dic"
              fi
              ln -s system_full.dic "$out/share/sudachi/system.dic"
            '';
```

- [ ] **Step 3: Rebuild and prove output identity**

Run:
```bash
nix build .#packages.x86_64-linux.sudachi-dictionary-full --out-link result-sudachi-after
sha256sum result-sudachi-after/share/sudachi/system_full.dic
readlink -f result-sudachi-after/share/sudachi/system.dic
```
Expected: the sha256 matches Step 1 and the `system.dic` symlink still resolves to `system_full.dic`. (The store path itself changes because the builder text changed — that is fine; the *content* is what must match.)

- [ ] **Step 4: Reindent `aozoraEpub3SmokeCheck`**

Normalize the `runCommand` body so the shell lines use consistent indentation and the `<<'PY'` heredoc content sits at a readable (but still column-0-relative, since Python is whitespace-sensitive and the heredoc is quoted) position. Do **not** change any command, path, argument, or the Python source — only leading whitespace of the shell (non-heredoc) lines. Keep the `python - "$bin" … <<'PY' … PY` block's interior exactly as-is (quoted heredoc = literal). After editing, visually confirm every shell statement (`work_dir=…`, `cp -R`, `chmod`, `cd`, `cargo … build`, `bin=…`, `printf … > …`, the second `printf … | bash … | jq …`, `touch "$out"`) is present and unaltered except for indentation.

- [ ] **Step 5: Rebuild the epub3 smoke check (oracle: build success)**

Run:
```bash
nix build .#checks.x86_64-linux.aozora-epub3-smoke -L
```
Expected: green — the fixtures validate and the wrapper+JAR smoke passes, identical to before. If it fails, a shell line was altered beyond whitespace during reindentation; compare to the pre-edit version in git.

- [ ] **Step 6: Format**

Run: `nixfmt ab-validator/flake.nix`

- [ ] **Step 7: Confirm no other output changed**

Run:
```bash
./flake-drvpath-snapshot.sh t7-after
diff snapshot-t6-after.txt snapshot-t7-after.txt
```
Expected: the only differing lines are `sudachi-dictionary-full` and `aozora-epub3-smoke` (plus root re-exports). Anything else → revert and narrow.

- [ ] **Step 8: Commit and clean up the temp out-links**

```bash
rm -f result-sudachi-before result-sudachi-after
git add ab-validator/flake.nix
git commit -m "style(ab-validator/flake): fix heredoc indentation and drop no-op runHook calls"
# (append the repo's required commit trailer block)
```

---

## Task 8: Document the duplicated `teiP5Version` (finding 15)

`teiP5Version = "4.11.0"` lives in both `nix/tei.nix` and `abc/nix/tei-profile-artifacts.nix`. They are in **different flakes** and cannot cleanly share a constant across the `path:` boundary; the duplication is already guarded by the `monorepo-tei-version-coherence` check. The minimal, honest fix is a cross-referencing comment at both sites naming the coherence check as the single-source-of-truth mechanism. Comments do not affect `drvPath`.

**Files:**
- Modify: `nix/tei.nix`
- Modify: `abc/nix/tei-profile-artifacts.nix`

- [ ] **Step 1: Add the comment in `nix/tei.nix`**

Above `teiP5Version = "4.11.0";` add:
```nix
  # TEI P5 version. Deliberately duplicated in abc/nix/tei-profile-artifacts.nix
  # (separate flake — no shared constant across the path: boundary). Drift between
  # the two is caught by the `monorepo-tei-version-coherence` check, which is the
  # single source of truth for coherence. Bump both together.
```

- [ ] **Step 2: Add the mirror comment in `abc/nix/tei-profile-artifacts.nix`**

Above its `teiP5Version = "4.11.0";` add:
```nix
  # TEI P5 version. Deliberately duplicated in <repo-root>/nix/tei.nix (separate
  # flake). Drift is caught by the `monorepo-tei-version-coherence` check. Bump
  # both together.
```

- [ ] **Step 3: Format and verify parse**

Run:
```bash
nixfmt nix/tei.nix abc/nix/tei-profile-artifacts.nix
nix eval --raw .#packages.x86_64-linux.tei-p5-reference.drvPath
nix eval --raw ./abc#checks.x86_64-linux.tei-profile-drift.drvPath
```
Expected: both `nix eval`s succeed and print store paths (proving the files still parse and evaluate). Comments changed nothing.

- [ ] **Step 4: Snapshot after (belt-and-suspenders) and commit**

```bash
nix flake lock
./flake-drvpath-snapshot.sh t8-after
diff snapshot-t7-after.txt snapshot-t8-after.txt && echo "INVARIANT"
git add nix/tei.nix abc/nix/tei-profile-artifacts.nix
git commit -m "docs(nix): cross-reference the guarded teiP5Version duplication"
# (append the repo's required commit trailer block)
```
Expected: `INVARIANT`.

---

## Task 9: Reconcile the system-iteration idiom (finding 14) — decision + documentation only

The three flakes iterate systems two ways: `ab-validator` uses `flake-utils.lib.eachDefaultSystem`; root and `abc` hand-roll `forAllSystems = genAttrs [ "x86_64-linux" "aarch64-linux" ]`. This is the weakest finding — a consistency nit, not a defect. **Converting root/abc to `flake-utils` would add an input and change no output; converting `ab-validator` off `flake-utils` would drop its darwin coverage.** Neither is worth output-churn. The resolution is to record the divergence as intentional.

> **Decision point for the executor/reviewer:** if the team prefers active unification over documentation, stop and raise it — that is a design change beyond this behavior-preserving plan and belongs in its own spec. Absent that, do Steps 1–2.

**Files:**
- Modify: `flake.nix` (root)

- [ ] **Step 1: Add an explanatory comment at the root `forAllSystems` definition**

Above `forAllSystems = nixpkgs.lib.genAttrs systems;` add:
```nix
      # Root and abc deliberately target Linux only (x86_64 + aarch64) via a
      # hand-rolled genAttrs, while ab-validator uses flake-utils.eachDefaultSystem
      # for its Rust builds (which include darwin). The root wraps only ab-validator's
      # Linux outputs. This split is intentional; do not unify without widening the
      # supported-system contract.
```

- [ ] **Step 2: Format, verify, snapshot, commit**

```bash
nixfmt flake.nix
./flake-drvpath-snapshot.sh t9-after
diff snapshot-t8-after.txt snapshot-t9-after.txt && echo "INVARIANT"
git add flake.nix
git commit -m "docs(flake): document the intentional system-iteration idiom split"
# (append the repo's required commit trailer block)
```
Expected: `INVARIANT`.

---

## Final verification (run after all tasks)

- [ ] **Full flake check on each flake** (the real CI gate — heavier than eval):
```bash
nix flake check ./ab-validator --no-build
nix flake check ./abc --no-build
nix flake check . --no-build
```
Expected: all succeed. `--no-build` keeps it fast; drop it for a full (slow) build if time allows before merge.

- [ ] **The monorepo nix-format gate:**
```bash
nix build .#checks.x86_64-linux.monorepo-nix-format -L
```
Expected: green (every touched `.nix` passes `nixfmt --check`).

- [ ] **End-to-end invariance:** confirm the union of all intentional `drvPath` changes is exactly the Task 6 + Task 7 outputs:
```bash
diff snapshot-baseline.txt snapshot-t9-after.txt
```
Expected: the only differing lines are `aozora2html-rust-parity` (check + app), `sudachi-dictionary-full`, and `aozora-epub3-smoke`, each with their root `ab-validator-*` re-exports. **Every other output is byte-identical to the baseline** — the whole refactor moved zero behavior except the three intended-change sites. If any unexpected line differs, bisect by task snapshot (`snapshot-tN-after.txt`) to find the culprit.

- [ ] **Remove the harness scaffold** (it was never committed; just delete the files):
```bash
rm -f flake-drvpath-snapshot.sh snapshot-*.txt
```

- [ ] **Finish the branch** via `superpowers:finishing-a-development-branch` (merge to `main` after gates pass, per project convention).

---

## Self-Review (author checklist — completed)

**Spec coverage** — every survey finding maps to a task:

| Finding | Task |
|---|---|
| 1 (5 near-identical Rust bins) | 5 |
| 2 (gaiji env copy-pasted 9×) | 5 |
| 3 (dead scaffolding fallbacks) | 5 (centralized into `mkUnscaffoldedStub`; fallback kept but defined once) |
| 4 (checks bypass `mkSmokeCheck`; parity body dup) | 6 (parity dedupe done; other three scoped out with rationale) |
| 5 (broken indentation) | 7 |
| 6 (no-op `runHook`) | 7 |
| 7 (two `vibrato-rkyv` hashes) | 5 (comment) |
| 8 (10 clj app blocks) | 3 |
| 9 (clj env / copy-source dup) | 4 |
| 10 (`prefixAttrs` → `mapAttrs'`) | 2 |
| 11 (`optionalOutputAttrs` → `attrByPath`) | 2 |
| 12 (`lib` binding + `pkgsFor`) | 2 |
| 13 (`monorepoScripts` wrappers) | 2 |
| 14 (system-iteration idiom) | 9 (documented; active unification flagged as out-of-scope) |
| 15 (`teiP5Version` duplication) | 8 (documented; cross-flake constant not feasible) |

**Note on findings 3, 4-partial, 14, 15:** the survey framed these as "reconcile/document/could." This plan resolves 3 structurally (single stub definition), does the high-value half of 4 (parity dedupe) and explicitly scopes out the low-value half with reasons, and resolves 14/15 as documentation because the structural alternatives change outputs or cross flake boundaries — which would violate the behavior-preservation constraint. If the team wants the structural versions of 14/15, they are separate specs.

**Placeholder scan:** no TBD/TODO/"handle edge cases"/"similar to Task N" — the one cross-reference (Task 5's per-site table) is a complete data table, not a "see above."

**Type/name consistency:** helper names are stable across tasks — `mkCljApp`, `copyWritableSource`/`cljSandboxEnv`, `gaijiEnv`/`gaijiBuildInputs`/`mkUnscaffoldedStub`/`mkRustBin`, `aozora2htmlParityText`, `pkgsFor`, `prefixAttrs`/`optionalOutputAttrs`. The `env` parameter of `mkRustBin` is merged via `//` at every call site consistently.
