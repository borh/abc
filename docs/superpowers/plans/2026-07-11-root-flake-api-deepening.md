# Root Flake API Deepening Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Replace the root flake's blanket component mirrors with the approved explicit operator facade while preserving component capabilities, hermetic `soranoha` wiring, and validation coverage.

**Architecture:** The root flake exposes five apps, one package, one integrated development shell, the formatter, and fourteen root-owned checks per supported system. Specialist outputs remain in `abc` and `ab-validator`; repository-maintainer cross-component probe workflows move behind two root `just` recipes backed by one tested runner script.

**Tech Stack:** Nix flakes, Bash, `just`, inline Python 3 contract assertions, Markdown documentation.

## Global Constraints

- Treat the root flake and root `justfile` as the primary development entry points.
- Preserve the root supported systems exactly: `x86_64-linux` and `aarch64-linux`.
- Do not add `flake-schemas`, generic output allowlists, or component-output traversal helpers.
- Do not add compatibility aliases for removed `abc-*` or `ab-validator-*` root outputs.
- Preserve `mkAdapterAwareSoranohaApp`, its runtime `PATH`, six `AB_*` assignments, validator-owned paths, and dispatch text byte-for-byte.
- `validate-migration` is the only retained app whose wrapper text changes intentionally.
- Set `AB_WORKSPACE_ROOT` to the monorepo root whenever directly checking `ab-validator`.
- Do not move validator dependencies into the ABC flake or change component ownership.
- Do not change runtime configuration, corpus location policy, source/schema identity, TEI version, or publication behavior.
- Preserve historical reports, completed plans, and evidence records; migrate active callers and regeneration instructions only.
- After changing files beneath a `path:` flake input, refresh the root lock with `nix flake lock` from the monorepo root.
- Keep generated snapshots and corpus-scale artifacts untracked.
- Preserve `optionalOutputAttrs` for private root consumption; delete only its blanket-export call sites.

## File Map

- `flake.nix` — explicit root output facade, retained private component consumption, and expanded `validate-migration` wrapper.
- `justfile` — complete no-build validation orchestration and two maintainer probe recipes.
- `tests/root-flake-output-contract-smoke.sh` — durable exact root-output contract for both supported systems.
- `scripts/run-tei-eaj-probe-workflow.sh` — hermetic shared implementation of the two maintainer probe recipes.
- `tests/tei-eaj-probe-workflows-smoke.sh` — isolated command/env contract test for the probe runner.
- `AGENTS.md`, `README.md`, `docs/migration-status.md` — active root API and validation documentation.
- `docs/presentations/jadh-2026-wip-slides.md`, `abc/docs/architecture.md`, `abc/docs/handoffs/tei-eaj-aozora-comparison.md` — active regeneration commands.
- `ab-validator/tests/parser-ir-level3-tei-eaj-compare-smoke.sh` — active operator guidance in a failure message.
- `flake.lock` — refreshed `abc`/`ab-validator` path-input identities after component-tree documentation/test edits.

---

### Task 1: Capture the pre-change output and identity baseline

**Files:**
- Create: `flake-output-snapshot.sh` (untracked worktree helper)
- Create: `snapshot-root-api-before.json` (untracked evidence)
- Create: `snapshot-identities-before.txt` (untracked evidence)

**Interfaces:**
- Produces: the authoritative pre-change counts/structure and host-system identity manifest used by Tasks 2 and 5.
- Consumes: all three flakes in their current, committed state.

- [ ] **Step 1: Create the untracked snapshot helper**

Use `apply_patch` to create this worktree-root helper, then keep it untracked:

```bash
#!/usr/bin/env bash
set -euo pipefail

label="${1:?usage: flake-output-snapshot.sh <label>}"
system="$(nix eval --impure --raw --expr builtins.currentSystem)"

nix flake show --json . > "snapshot-root-api-${label}.json"

emit() {
  local dir="$1" output="$2" field="$3"
  nix eval --json "${dir}#${output}.${system}" \
    --apply "set: builtins.mapAttrs (_: value: value.${field} or null) set" \
    | jq -r --arg dir "$dir" --arg output "$output" \
      'to_entries[] | "\($dir) \($output).\(.key) => \(.value)"'
}

{
  for dir in . ./abc ./ab-validator; do
    emit "$dir" apps program
    for output in packages checks devShells; do
      emit "$dir" "$output" drvPath
    done
  done
} | LC_ALL=C sort > "snapshot-identities-${label}.txt"

python - "snapshot-root-api-${label}.json" <<'PY'
import json
import sys

with open(sys.argv[1], encoding="utf-8") as handle:
    outputs = json.load(handle)

system = "x86_64-linux"
counts = {
    name: len(outputs[name][system])
    for name in ("apps", "packages", "checks", "devShells")
}
counts["formatter"] = 1 if system in outputs["formatter"] else 0
print(counts, "total=", sum(counts.values()))
PY
```

- [ ] **Step 2: Make the helper executable and exclude its artifacts**

Run:

```sh
chmod +x flake-output-snapshot.sh
printf '%s\n' 'flake-output-snapshot.sh' 'snapshot-root-api-*.json' 'snapshot-identities-*.txt' >> .git/info/exclude
```

Expected: `git status --short` does not list the helper or snapshots.

- [ ] **Step 3: Capture and validate the baseline**

Run:

```sh
./flake-output-snapshot.sh before
rg '=> null$' snapshot-identities-before.txt && exit 1 || true
jq '{apps:(.apps."x86_64-linux"|keys|length),packages:(.packages."x86_64-linux"|keys|length),checks:(.checks."x86_64-linux"|keys|length),devShells:(.devShells."x86_64-linux"|keys|length),formatter:(.formatter|has("x86_64-linux"))}' snapshot-root-api-before.json
```

Expected: no null identities; counts are `35`, `40`, `72`, `7`, and formatter `true`. Treat the snapshot, not these prose values, as authoritative if they differ.

- [ ] **Step 4: Record the flagship retained app identity separately**

Run:

```sh
nix eval --raw .#apps.x86_64-linux.soranoha.program > snapshot-soranoha-program-before.txt
printf '\n' >> snapshot-soranoha-program-before.txt
before_program="$(cat snapshot-soranoha-program-before.txt)"
sed -E 's#/nix/store/[a-z0-9]{32}-#/nix/store/HASH-#g' "$before_program" \
  > snapshot-soranoha-script-normalized-before.txt
printf '%s\n' 'snapshot-soranoha-program-*.txt' 'snapshot-soranoha-script-normalized-*.txt' >> .git/info/exclude
```

Expected: the file contains one `/nix/store/...-soranoha-with-adapters` program path.

- [ ] **Step 5: Do not commit this task**

The helper and snapshots are disposable characterization artifacts.

---

### Task 2: Implement and enforce the explicit root facade

**Files:**
- Create: `tests/root-flake-output-contract-smoke.sh`
- Modify: `flake.nix`
- Modify: `justfile`

**Interfaces:**
- Consumes: the approved exact output names and Task 1 baseline.
- Produces: five root apps, one root package, one root shell, formatter, fourteen checks, and explicit root/component validation orchestration.

- [ ] **Step 1: Write the failing exact-contract smoke test**

Create `tests/root-flake-output-contract-smoke.sh` with:

```bash
#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

nix flake show --json "$repo_root" | python -c '
import json
import sys

outputs = json.load(sys.stdin)
systems = {"aarch64-linux", "x86_64-linux"}
expected = {
    "apps": {
        "flake-input-policy",
        "schema-drift",
        "soranoha",
        "tei-version-coherence",
        "validate-migration",
    },
    "packages": {"tei-p5-reference"},
    "devShells": {"default"},
    "checks": {
        "monorepo-aat-materialization-workflow",
        "monorepo-aat-run-set",
        "monorepo-active-path-hygiene",
        "monorepo-batch-run-staleness",
        "monorepo-fidelity-lock-idempotency",
        "monorepo-flake-input-policy",
        "monorepo-nix-format",
        "monorepo-python-quality",
        "monorepo-runtime-config",
        "monorepo-schema-drift",
        "monorepo-tei-p5-reference",
        "monorepo-tei-version-coherence",
        "monorepo-workflow-run-lib",
        "tei-eaj-aozora-alignment-probe-generation",
    },
}

errors = []
for output_name, expected_names in expected.items():
    actual_systems = set(outputs.get(output_name, {}))
    if actual_systems != systems:
        errors.append(
            f"{output_name} systems: expected {sorted(systems)}, got {sorted(actual_systems)}"
        )
    for system in systems:
        actual_names = set(outputs.get(output_name, {}).get(system, {}))
        if actual_names != expected_names:
            errors.append(
                f"{output_name}.{system}: expected {sorted(expected_names)}, got {sorted(actual_names)}"
            )

formatter_systems = set(outputs.get("formatter", {}))
if formatter_systems != systems:
    errors.append(
        f"formatter systems: expected {sorted(systems)}, got {sorted(formatter_systems)}"
    )

if errors:
    raise SystemExit("\n".join(errors))
'

echo "root flake output contract smoke ok"
```

- [ ] **Step 2: Run the test and verify the old mirror fails**

Run:

```sh
bash tests/root-flake-output-contract-smoke.sh
```

Expected: FAIL listing the current extra prefixed apps, packages, checks, and shells.

- [ ] **Step 3: Remove mechanical export machinery without rewriting retained wrappers**

In `flake.nix`:

1. Delete the `prefixAttrs` binding.
2. Delete `mkProbeAwareAbcApp` and its two root app definitions.
3. Delete `python-quality` from `monorepoScripts` and root apps.
4. Leave `mkAdapterAwareSoranohaApp` and its `soranoha` construction text unchanged.
5. Replace the apps result expression with the explicit facade:

```nix
        in
        (
          if builtins.hasAttr "soranoha" abcApps then
            {
              soranoha = mkScriptApp (mkAdapterAwareSoranohaApp abcApps.soranoha) (
                abcApps.soranoha.meta.description or "Soranoha snapshot publication command dispatcher"
              );
            }
          else
            { }
        )
        // {
          schema-drift = mkScriptApp scripts.schema-drift "Check monorepo ABC schema contract drift";
          tei-version-coherence = mkScriptApp scripts.tei-version-coherence "Check TEI P5 source/profile version coherence";
          flake-input-policy = mkScriptApp scripts.flake-input-policy "Check release-critical flake inputs are explicitly pinned";
          validate-migration = mkScriptApp scripts.validate-migration "Run Soranoha monorepo migration validation gates";
        }
```

6. Delete the two prefixed merges and `parser-ir-ortho-publication-smoke` alias from `checks`, leaving the existing fourteen root-owned entries unchanged.
7. Replace `packages` with only:

```nix
        in
        {
          tei-p5-reference = tei.reference;
        }
```

8. Replace the `devShells` result expression with only the existing `default` shell body; keep `abcShells` and `abValidatorShells` as private inputs to `inputsFrom`.

- [ ] **Step 4: Expand the explicit `validate-migration` wrapper**

Keep its existing formatting and append the direct checks exactly as follows:

```nix
          validate-migration = pkgs.writeShellScript "soranoha-validate-migration" ''
            set -euo pipefail
            export PATH="${runtimePath}:$PATH"
            workspace_root="$PWD"
            bash tests/monorepo-active-path-hygiene-smoke.sh
            bash tests/root-flake-output-contract-smoke.sh
            bash scripts/monorepo-schema-drift.sh
            bash scripts/monorepo-tei-version-coherence.sh
            python scripts/monorepo-flake-input-policy.py
            nix flake check --no-build "$@"
            (cd "$workspace_root/abc" && nix flake check --no-build "$@")
            (
              cd "$workspace_root/ab-validator"
              AB_WORKSPACE_ROOT="$workspace_root" nix flake check --no-build "$@"
            )
          '';
```

Expected intentional identity change: only the retained `validate-migration` app wrapper changes; do not route it through `mkWrappedScript`.

- [ ] **Step 5: Make root `just` validation explicit**

Add this recipe and make `check-no-build` own all three flake evaluations:

```make
root-flake-output-contract:
	@bash tests/root-flake-output-contract-smoke.sh

check-no-build: runtime-config-smoke active-path-hygiene root-flake-output-contract schema-drift tei-version-coherence flake-input-policy python-quality nix-format-check root-flake-check-no-build
	@(cd abc && nix flake check --no-build)
	@(cd ab-validator && AB_WORKSPACE_ROOT="$(pwd)/.." nix flake check --no-build)

validate-migration: check-no-build
```

Remove the old duplicated `validate-migration` prerequisite list.

- [ ] **Step 6: Run focused evaluation and contract tests**

Run:

```sh
nixfmt flake.nix
bash tests/root-flake-output-contract-smoke.sh
nix flake check --no-build
nix eval --raw .#apps.x86_64-linux.soranoha.program > snapshot-soranoha-program-after.txt
printf '\n' >> snapshot-soranoha-program-after.txt
diff -u snapshot-soranoha-program-before.txt snapshot-soranoha-program-after.txt
```

Expected: contract smoke passes; root check evaluation passes; `soranoha` program diff is empty.

- [ ] **Step 7: Verify removed aliases still resolve from component flakes before editing component trees**

Run representative identity comparisons:

```sh
before_abc="$(awk -F ' => ' '$1 == ". apps.abc-materialize-publication" {print $2}' snapshot-identities-before.txt)"
after_abc="$(nix eval --raw ./abc#apps.x86_64-linux.materialize-publication.program)"
test "$before_abc" = "$after_abc"

before_validator="$(awk -F ' => ' '$1 == ". packages.ab-validator-ab-aat-to-parser-ir" {print $2}' snapshot-identities-before.txt)"
after_validator="$(nix eval --raw ./ab-validator#packages.x86_64-linux.ab-aat-to-parser-ir.drvPath)"
test "$before_validator" = "$after_validator"

if nix eval .#apps.x86_64-linux.abc-materialize-publication >/dev/null 2>&1; then
  echo "removed root alias still resolves" >&2
  exit 1
fi
```

Expected: component identities match their old aliases; removed root alias fails.

- [ ] **Step 8: Commit the explicit facade**

```sh
git add flake.nix justfile tests/root-flake-output-contract-smoke.sh
git commit -m "refactor(nix): curate root flake API"
```

---

### Task 3: Preserve probe composition behind maintainer recipes

**Files:**
- Create: `scripts/run-tei-eaj-probe-workflow.sh`
- Create: `tests/tei-eaj-probe-workflows-smoke.sh`
- Modify: `justfile`

**Interfaces:**
- Produces: `just tei-eaj-alignment-probe` and `just tei-eaj-reports-with-probes`.
- Consumes: `./ab-validator#ab-aat-to-parser-ir`, `./abc#tei-eaj-aozora-alignment-probe`, and `./abc#tei-eaj-aozora-reports-with-probes`.

- [ ] **Step 1: Write the failing isolated runner test**

Create `tests/tei-eaj-probe-workflows-smoke.sh`:

```bash
#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
tmp="$(mktemp -d)"
trap 'rm -rf "$tmp"' EXIT
mkdir -p "$tmp/bin"

cat > "$tmp/bin/nix" <<'SH'
#!/usr/bin/env bash
set -euo pipefail
: "${TRACE:?}"
printf '%s\n' "$*" >> "$TRACE"
case "$1" in
  build)
    test "$*" = "build ./ab-validator#ab-aat-to-parser-ir --no-link --print-out-paths"
    printf '%s\n' /nix/store/fake-ab-aat-to-parser-ir
    ;;
  run)
    test "${ABC_TEI_EAJ_ALIGNMENT_PROBE_BIN:-}" = "/nix/store/fake-ab-aat-to-parser-ir/bin/ab-aat-to-parser-ir"
    ;;
  *) exit 64 ;;
esac
SH
chmod +x "$tmp/bin/nix"

TRACE="$tmp/trace" PATH="$tmp/bin:$PATH" \
  bash "$repo_root/scripts/run-tei-eaj-probe-workflow.sh" alignment-probe --max-rows 4
TRACE="$tmp/trace" PATH="$tmp/bin:$PATH" \
  bash "$repo_root/scripts/run-tei-eaj-probe-workflow.sh" reports-with-probes

grep -Fxq "run ./abc#tei-eaj-aozora-alignment-probe -- --max-rows 4" "$tmp/trace"
grep -Fxq "run ./abc#tei-eaj-aozora-reports-with-probes --" "$tmp/trace"

if TRACE="$tmp/trace" PATH="$tmp/bin:$PATH" \
  bash "$repo_root/scripts/run-tei-eaj-probe-workflow.sh" unknown 2>/dev/null; then
  echo "unknown workflow unexpectedly succeeded" >&2
  exit 1
fi

echo "TEI-EAJ probe workflow smoke ok"
```

- [ ] **Step 2: Run it and verify the runner is missing**

Run:

```sh
bash tests/tei-eaj-probe-workflows-smoke.sh
```

Expected: FAIL because `scripts/run-tei-eaj-probe-workflow.sh` does not exist.

- [ ] **Step 3: Implement the minimal hermetic runner**

Create `scripts/run-tei-eaj-probe-workflow.sh`:

```bash
#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
workflow="${1:?usage: run-tei-eaj-probe-workflow.sh WORKFLOW [ARGS...]}"
shift

case "$workflow" in
  alignment-probe) app="tei-eaj-aozora-alignment-probe" ;;
  reports-with-probes) app="tei-eaj-aozora-reports-with-probes" ;;
  *)
    echo "unknown TEI-EAJ probe workflow: $workflow" >&2
    exit 64
    ;;
esac

cd "$repo_root"
probe_root="$(nix build ./ab-validator#ab-aat-to-parser-ir --no-link --print-out-paths)"
export ABC_TEI_EAJ_ALIGNMENT_PROBE_BIN="$probe_root/bin/ab-aat-to-parser-ir"
exec nix run "./abc#$app" -- "$@"
```

- [ ] **Step 4: Add the two public maintainer recipes**

Add to `justfile`:

```make
tei-eaj-alignment-probe *args:
	@scripts/run-tei-eaj-probe-workflow.sh alignment-probe {{args}}

tei-eaj-reports-with-probes *args:
	@scripts/run-tei-eaj-probe-workflow.sh reports-with-probes {{args}}
```

- [ ] **Step 5: Run focused tests**

Run:

```sh
bash tests/tei-eaj-probe-workflows-smoke.sh
just --dry-run tei-eaj-alignment-probe -- --max-rows 4
just --dry-run tei-eaj-reports-with-probes
```

Expected: smoke passes; dry runs invoke the runner with the correct workflow selector.

- [ ] **Step 6: Commit the maintainer workflows**

```sh
git add justfile scripts/run-tei-eaj-probe-workflow.sh tests/tei-eaj-probe-workflows-smoke.sh
git commit -m "feat(tooling): preserve TEI-EAJ probe workflows"
```

---

### Task 4: Migrate active callers and document the new ownership boundary

**Files:**
- Modify: `AGENTS.md`
- Modify: `README.md`
- Modify: `docs/migration-status.md`
- Modify: `docs/presentations/jadh-2026-wip-slides.md`
- Modify: `abc/docs/architecture.md`
- Modify: `abc/docs/handoffs/tei-eaj-aozora-comparison.md`
- Modify: `ab-validator/tests/parser-ir-level3-tei-eaj-compare-smoke.sh`
- Modify: `flake.lock`

**Interfaces:**
- Consumes: the component-qualified paths and maintainer recipes from Tasks 2–3.
- Produces: no active repository instruction that depends on a removed root alias.

- [ ] **Step 1: Update root developer guidance**

In `AGENTS.md` and `README.md`, replace root mirrored check commands with:

```sh
nix build ./abc#checks.x86_64-linux.clj-kondo
nix build ./abc#checks.x86_64-linux.clj-nix-focused-tests
nix build ./ab-validator#checks.x86_64-linux.cargo-check
nix build ./ab-validator#checks.x86_64-linux.cargo-clippy
nix build ./ab-validator#checks.x86_64-linux.cargo-fmt
```

Update the README validation description to say that `just validate-migration` evaluates root and both component flakes directly.

- [ ] **Step 2: Replace the migration-era prefix policy**

In `docs/migration-status.md`:

- Change `root-flake-check-no-build` documentation to say it evaluates only root-owned integration checks.
- Change `check-no-build` and `validate-migration` documentation to say they evaluate root, ABC, and validator flakes explicitly.
- Replace the prefix-policy paragraph with the approved ownership rule and examples:

```markdown
The root flake is an explicit operator-facing integration facade. Component
tools remain available from their owning flakes:

- use `./abc#...` for publication, schema, TEI, manifest, and registry tools;
- use `./ab-validator#...` for parser, adapter, evidence, corpus, dictionary,
  and analyzer tools;
- use root outputs only for supported end-to-end workflows, root-owned
  cross-component contracts, cross-component artifacts, and the integrated
  development shell.

Convenience alone is not sufficient for promotion to the root.
```

- [ ] **Step 3: Migrate active presentation and component documentation**

Apply these exact command-shape migrations:

```text
nix run .#abc-<app> -> nix run ./abc#<app>
nix build .#checks.x86_64-linux.abc-<check> -> nix build ./abc#checks.x86_64-linux.<check>
nix run .#ab-validator-<app> -> nix run ./ab-validator#<app>
nix build .#ab-validator-<package> -> nix build ./ab-validator#<package>
```

In `abc/docs/handoffs/tei-eaj-aozora-comparison.md`, use:

```sh
just tei-eaj-alignment-probe
just tei-eaj-reports-with-probes
```

In `ab-validator/tests/parser-ir-level3-tei-eaj-compare-smoke.sh`, change the failure guidance to:

```text
run nix run ./abc#tei-eaj-aozora-workset-json or set AB_TEI_EAJ_WORKSET=/path/to/workset.json
```

Do not edit dated files under `docs/superpowers/`, `abc/docs/superpowers/`, `ab-validator/docs/superpowers/`, checked-in reports, or presentation evidence records.

- [ ] **Step 4: Assert that active removed aliases are gone**

Run:

```sh
if rg -n '\.\#(abc-|ab-validator-)' \
  AGENTS.md README.md justfile scripts tests docs/migration-status.md \
  docs/presentations/jadh-2026-wip-slides.md abc/docs/architecture.md \
  abc/docs/handoffs ab-validator/tests; then
  echo "active removed root alias remains" >&2
  exit 1
fi
```

Expected: no matches.

- [ ] **Step 5: Refresh path-input locks and run focused checks**

Run from the monorepo root:

```sh
nix flake lock
bash tests/root-flake-output-contract-smoke.sh
bash tests/tei-eaj-probe-workflows-smoke.sh
just nix-format-check
```

Expected: root lock records current component paths; both smoke tests and formatting pass.

- [ ] **Step 6: Commit the caller migration**

```sh
git add AGENTS.md README.md docs/migration-status.md \
  docs/presentations/jadh-2026-wip-slides.md abc/docs/architecture.md \
  abc/docs/handoffs/tei-eaj-aozora-comparison.md \
  ab-validator/tests/parser-ir-level3-tei-eaj-compare-smoke.sh flake.lock
git commit -m "docs(nix): migrate component output callers"
```

---

### Task 5: Run final contract, normalized-wrapper, and validation gates

**Files:**
- Modify only if a verification failure reveals a scoped defect in a Task 2–4 file.
- Remove: disposable snapshot helper and snapshot artifacts from the worktree after comparison.

**Interfaces:**
- Consumes: all preceding task outputs and Task 1 snapshots.
- Produces: evidence that the facade is exact, retained hermetic wiring survives, component access remains, and validation coverage is complete.

- [ ] **Step 1: Capture the post-change snapshots**

Run:

```sh
./flake-output-snapshot.sh after
nix eval --raw .#apps.x86_64-linux.soranoha.program > snapshot-soranoha-program-final.txt
printf '\n' >> snapshot-soranoha-program-final.txt
final_program="$(cat snapshot-soranoha-program-final.txt)"
sed -E 's#/nix/store/[a-z0-9]{32}-#/nix/store/HASH-#g' "$final_program" \
  > snapshot-soranoha-script-normalized-final.txt
```

Expected: snapshot helper completes without null identities.

- [ ] **Step 2: Verify both-system structure and exact counts**

Run:

```sh
bash tests/root-flake-output-contract-smoke.sh
jq '{apps:(.apps."x86_64-linux"|keys|length),packages:(.packages."x86_64-linux"|keys|length),checks:(.checks."x86_64-linux"|keys|length),devShells:(.devShells."x86_64-linux"|keys|length),formatter:(.formatter|has("x86_64-linux"))}' snapshot-root-api-after.json
```

Expected: `5` apps, `1` package, `14` checks, `1` shell, formatter `true`; the smoke test proves the same names exist on `aarch64-linux`.

- [ ] **Step 3: Verify retained `soranoha` wrapper structure after path-input migration**

Run:

```sh
diff -u snapshot-soranoha-script-normalized-before.txt snapshot-soranoha-script-normalized-final.txt
```

Expected: empty normalized diff. The raw program path may change because Task 4 changes files inside both path inputs. Any normalized change means the adapter-aware wrapper structure or dependency shape changed; stop and reconcile before continuing.

- [ ] **Step 4: Verify private dependencies and removed aliases**

Run:

```sh
nix eval --raw ./abc#apps.x86_64-linux.soranoha.program >/dev/null
nix eval --raw ./ab-validator#packages.x86_64-linux.upstream-parser-aozora2html.drvPath >/dev/null
nix eval --raw ./ab-validator#packages.x86_64-linux.aozora2html-adapter.drvPath >/dev/null
nix eval --raw ./ab-validator#packages.x86_64-linux.ab-aat-to-parser-ir.drvPath >/dev/null

for attr in \
  apps.x86_64-linux.abc-materialize-publication \
  apps.x86_64-linux.ab-validator-sudachi \
  packages.x86_64-linux.ab-validator-ab-aat-to-parser-ir \
  devShells.x86_64-linux.abc-validation \
  checks.x86_64-linux.abc-clj-kondo; do
  if nix eval ".#$attr" >/dev/null 2>&1; then
    echo "removed root alias still resolves: $attr" >&2
    exit 1
  fi
done
```

Expected: private dependencies resolve; every removed root alias fails.

- [ ] **Step 5: Run focused and full validation**

Run:

```sh
bash tests/tei-eaj-probe-workflows-smoke.sh
just python-quality
just nix-format-check
nix flake check --no-build
(cd abc && nix flake check --no-build)
(cd ab-validator && AB_WORKSPACE_ROOT="$(pwd)/.." nix flake check --no-build)
just validate-migration
```

Expected: every command exits zero. If the full migration gate is too slow to complete in one invocation, preserve its output and resume it; do not report completion from focused checks alone.

- [ ] **Step 6: Inspect the final diff and worktree**

Run:

```sh
git diff --check
git status --short
git log -5 --oneline
```

Expected: no uncommitted implementation changes, no snapshot artifacts, and the three implementation commits from Tasks 2–4 follow the approved spec/plan commits.

- [ ] **Step 7: Remove disposable characterization artifacts**

Run:

```sh
rm -f flake-output-snapshot.sh snapshot-root-api-*.json snapshot-identities-*.txt \
  snapshot-soranoha-program-*.txt snapshot-soranoha-script-normalized-*.txt
```

Expected: `git status --short` remains clean because all removed files were untracked.
