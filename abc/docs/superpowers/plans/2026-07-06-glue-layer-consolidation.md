# Glue-Layer Consolidation (Pre-Monorepo)

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Eliminate the three largest sources of duplication and staleness in the bash/python/nix glue layer across abc and ab-validator, making the imminent monorepo merge trivial instead of dangerous.

**Architecture:** Three independent workstreams targeting different repos and tech stacks: (A) schema contract versioning in abc's Clojure repo, (B) Nix smoke-test factory extraction in ab-validator's flake, (C) Python report library extraction in ab-validator's reports directory. Each produces a self-contained, testable deliverable with no cross-task coupling.

**Tech Stack:** JSON Schema, Nix (flake + runCommand), Python 3, Clojure (kaocha tests), Bash

## Global Constraints

- Nix checks must remain fully offline-capable (no network in sandbox)
- Python scripts remain runnable from repo root with `python reports/.../script.py --args`
- Both repos stay buildable independently until the monorepo merge (no cross-repo flake inputs, no sibling-directory assumptions)
- Commit granularity: one commit per completed task, referencing this plan
- Schemas already have `$id` URIs and `$schema` draft-2020-12; this plan only adds `version`

## Implementation Review Notes

- Phase A rotates ABC schema hashes. The implementation keeps ab-validator's vendored schema snapshot and mapping artifact on their previous hash contract until a dedicated protocol-rotation slice updates `data/abc-schemas/`, `data/aat-to-parser-ir-mapping-v1.json`, generated mapping reports, and ABC compatibility-registry evidence together.
- `src/abc/tools/validate_design_bundle.clj` already accepts the live parser-IR schema hash dynamically. After adding `version`, add the previous live hash to `legacy-parser-ir-schema-hashes`; do not add the new live hash there unless it later becomes legacy.
- Nix verification must not use `| head -1`; that hides failures. Use strict `nix eval ...drvPath` for evaluation and build representative checks with `nix build ... --print-build-logs --no-link`.
- `reports/lib/paths.py` must return `_LIB_DIR.parents[1]`: for `.../repo/reports/lib`, `parents[0]` is `reports/` and `parents[1]` is the repo root.
- Shared Python JSON helpers must use `encoding="utf-8"` for reads and writes.

---

## Phase A: Schema Contract Versioning (abc repo)

### Task A1: Add `version` field to the 6 cross-project schemas and accept rotated hashes

**Repo:** abc

**Files:**
- Modify: `schemas/parser-ir.schema.json` (add `version`)
- Modify: `schemas/aat-parser-ir-divergence.schema.json` (add `version`)
- Modify: `schemas/aat-parser-ir-mapping.schema.json` (add `version`)
- Modify: `schemas/manifest.schema.json` (add `version`)
- Modify: `schemas/parser-ir-publication-preservation.schema.json` (add `version`)
- Modify: `schemas/source-region-coverage.schema.json` (add `version`)
- Modify: `src/abc/tools/validate_design_bundle.clj` (accept rotated hashes)

**Interfaces:**
- Produces: each schema gains a `version` field (semver string). All 6 schemas already have `$id` URIs and `$schema`. Adding `version` rotates the JCS-canonicalized SHA-256 hash that `src/abc/tools/manifest.clj` produces via `schema-hash` → `hash/sha256-json-jcs` → `hash/format-sha256`. The `validate_design_bundle.clj` legacy hash set must expand to accept the new hashes alongside the old ones.

- [ ] **Step 1: Add `version` to parser-ir.schema.json**

All 6 schemas already have `$id` and `$schema` (confirmed: checked `head -5` on each file). Only `version` is missing. Insert `"version": "<semver>"` on the line after `"title"`.

For `schemas/parser-ir.schema.json`, insert after line 4 (`"title": "ABC Parser IR",`):
```json
  "version": "0.5.0",
```

- [ ] **Step 2: Add `version` to the other 5 schemas**

Same insertion pattern — after the `"title"` line, add `"version": "<semver>",`:

`aat-parser-ir-divergence.schema.json` → `"version": "0.3.0"`
`aat-parser-ir-mapping.schema.json` → `"version": "0.2.3"`
`manifest.schema.json` → `"version": "0.4.0"`
`parser-ir-publication-preservation.schema.json` → `"version": "0.2.0"`
`source-region-coverage.schema.json` → `"version": "0.2.0"`

- [ ] **Step 3: Compute the new JCS-canonicalized schema hashes**

The `version` field addition changes the SHA-256 hash that `manifest/schema-hash` computes via JCS canonicalization. Run a one-off command to get the new hashes:

```bash
clojure -M -e '
(require (quote [abc.tools.manifest :as m]))
(doseq [f ["schemas/parser-ir.schema.json"
           "schemas/aat-parser-ir-divergence.schema.json"
           "schemas/aat-parser-ir-mapping.schema.json"
           "schemas/manifest.schema.json"
           "schemas/parser-ir-publication-preservation.schema.json"
           "schemas/source-region-coverage.schema.json"]]
  (println (str f ": " (m/schema-hash f))))
'
```

Record the output — you'll need the `parser-ir.schema.json` hash for the next step.

- [ ] **Step 4: Preserve the previous live parser-ir schema hash as legacy**

In `src/abc/tools/validate_design_bundle.clj`, `accepted-parser-ir-schema-hashes` already adds the live hash from `schemas/parser-ir.schema.json`. Add the previous live hash to `legacy-parser-ir-schema-hashes` so artifacts produced immediately before the `version` addition remain accepted:

```clojure
(def legacy-parser-ir-schema-hashes
  #{"sha256:41c43f0c88a66c31ae4fbf9b9eeb04de92756082acaaaa1c2e21f1a5bf74a396"
    "sha256:8e56871965e647e40ade08fd9dd580a3516d33905be17957cc79750bd42ea64d"
    "sha256:da916a3a92f64d985cb98f9b2ddc7f562e660fd0c3dbe0c902392d3764b0158a"
    "sha256:c081f2365e2159e6e608733c4eb4e6fdf1fa80203ccd3d5e1f2afc533da8d411"})
```

- [ ] **Step 5: Run schema tests**

```bash
clojure -M:test:kaocha -m kaocha.runner --focus abc.tools.schema-test
clojure -M:test:kaocha -m kaocha.runner --focus abc.tools.validate-design-bundle-test/schema-hash-errors-test
clojure -M:test:kaocha -m kaocha.runner --focus abc.tools.validate-design-bundle-test/parser-ir-schema-hash-errors-test
```

Expected: all focused tests pass. The broader `abc.tools.validate-design-bundle-test` namespace currently has pre-existing failures unrelated to this slice (TEI schema env, source-region fixture drift, and compatibility-registry drift).

- [ ] **Step 6: Commit**

```bash
git add schemas/parser-ir.schema.json \
        schemas/aat-parser-ir-divergence.schema.json \
        schemas/aat-parser-ir-mapping.schema.json \
        schemas/manifest.schema.json \
        schemas/parser-ir-publication-preservation.schema.json \
        schemas/source-region-coverage.schema.json \
        src/abc/tools/validate_design_bundle.clj
git commit -m "feat: add version field to cross-project schemas

Adds explicit semver version to the 6 schemas consumed by
ab-validator. All schemas already had \$id URIs. The version
field rotates JCS-canonicalized hashes; validate_design_bundle
legacy set expanded to accept both old and new hashes."
```

### Task A2 (DEFERRED): Cross-repo schema drift check

**Status: deferred to monorepo.** A Nix check that compares abc schemas against ab-validator's snapshot requires a sibling-directory assumption (`../ab-validator/data/abc-schemas/schemas`) or a flake input pointing at the other repo. Both violate the constraint that each repo stays independently buildable.

After the monorepo merge, `data/abc-schemas/schemas/` and `schemas/` become the same directory. At that point, add a CI check that validates `data/abc-schemas/schemas/` is a symlink to `schemas/` (or that no stale copy exists). The `version` fields added in Task A1 will serve as the single source of truth for drift detection.

---

## Phase B: Nix Smoke-Test Factory (ab-validator repo)

### Task B1: Extract the smoke-test check pattern into a Nix helper

**Repo:** ab-validator

**Files:**
- Modify: `flake.nix` (add `mkSmokeCheck` function, refactor 2 checks to use it, then all remaining checks)

**Interfaces:**
- Produces: `mkSmokeCheck` — a function in the flake's `let` block that takes `{ name, testScript, nativeBuildInputs ? [], extraEnv ? {} }` and returns a `pkgs.runCommand` derivation

- [ ] **Step 1: Read the current repeated pattern to confirm it matches**

The pattern to extract (occurs 14+ times in flake.nix, e.g., `level3AdmissionSmokeCheck`, `plainProseSourceDeltaSmokeCheck`, `publicationBundleSmokeCheck`):

```nix
pkgs.runCommand "check-name"
  {
    nativeBuildInputs = [ pkgs.bash pkgs.jq ... ];
  }
  ''
    work_dir="$(mktemp -d)"
    cp -R "${source}" "$work_dir/source"
    chmod -R +w "$work_dir/source"
    cd "$work_dir/source"
    export TMPDIR="$work_dir/tmp"
    mkdir -p "$TMPDIR"
    export HOME="$work_dir/home"
    mkdir -p "$HOME"
    bash tests/the-smoke.sh
    touch "$out"
  '';
```

Some checks also set extra environment variables before the `bash` call (e.g., `AB_SOURCE_INVENTORY_BIN`).

- [ ] **Step 2: Add `mkSmokeCheck` to the flake's `let` block**

In `flake.nix`, after the existing `let` bindings (after `pythonWithAatSchemaDeps` around line 570), add:

```nix
mkSmokeCheck =
  {
    name,
    testScript,
    nativeBuildInputs ? [ ],
    extraEnv ? { },
    extraPreScript ? "",
  }:
  let
    envExports = pkgs.lib.concatStringsSep "\n" (
      pkgs.lib.mapAttrsToList (k: v: "export ${k}=\"${v}\"") extraEnv
    );
  in
  pkgs.runCommand name
    {
      nativeBuildInputs = nativeBuildInputs ++ [ pkgs.bash pkgs.coreutils ];
    }
    ''
      work_dir="$(mktemp -d)"
      cp -R "${source}" "$work_dir/source"
      chmod -R +w "$work_dir/source"
      cd "$work_dir/source"

      export TMPDIR="$work_dir/tmp"
      mkdir -p "$TMPDIR"
      export HOME="$work_dir/home"
      mkdir -p "$HOME"
      ${envExports}
      ${extraPreScript}

      bash "${testScript}"
      touch "$out"
    '';
```

Note: `envExports` is computed in a `let` binding because `concatStringsSep` produces a string, not a list. `mapAttrsToList` alone produces a Nix list which cannot be interpolated directly into a shell string.

- [ ] **Step 3: Refactor `sourceInventorySmokeCheck` as the first consumer**

Before (current code around line 895 of flake.nix):
```nix
sourceInventorySmokeCheck =
  pkgs.runCommand "source-inventory-smoke-check"
    {
      nativeBuildInputs = [
        sourceInventoryBin
        pkgs.jq
        pkgs.ripgrep
      ];
    }
    ''
      work_dir="$(mktemp -d)"
      cp -R "${source}" "$work_dir/source"
      chmod -R +w "$work_dir/source"
      cd "$work_dir/source"

      export TMPDIR="$work_dir/tmp"
      mkdir -p "$TMPDIR"
      export HOME="$work_dir/home"
      mkdir -p "$HOME"
      export AB_SOURCE_INVENTORY_BIN="${sourceInventoryBin}/bin/ab-source-inventory"

      bash tests/source-inventory-smoke.sh
      touch "$out"
    '';
```

After:
```nix
sourceInventorySmokeCheck = mkSmokeCheck {
  name = "source-inventory-smoke-check";
  testScript = "tests/source-inventory-smoke.sh";
  nativeBuildInputs = [
    sourceInventoryBin
    pkgs.jq
    pkgs.ripgrep
  ];
  extraEnv = {
    AB_SOURCE_INVENTORY_BIN = "${sourceInventoryBin}/bin/ab-source-inventory";
  };
};
```

- [ ] **Step 4: Verify the refactored check builds**

```bash
system=$(nix eval --impure --raw --expr builtins.currentSystem)
nix build ".#checks.$system.source-inventory-smoke" --print-build-logs --no-link
```

Expected: identical behavior — passes if the original passed, fails identically if the original fails.

- [ ] **Step 5: Commit the factory + first consumer**

```bash
git add flake.nix
git commit -m "refactor(nix): extract mkSmokeCheck factory, migrate source-inventory

Extracts the 6-line 'copy source to tmp + run bash test' pattern
that repeats 14+ times into a mkSmokeCheck function. Migrates
source-inventory-smoke as the proof-of-concept consumer."
```

### Task B2: Migrate remaining smoke checks to mkSmokeCheck

**Files:**
- Modify: `flake.nix` (refactor the remaining 13 check derivations)

- [ ] **Step 1: Refactor sourceRepresentabilityGateCheck**

Before pattern matches the one from B1. After:
```nix
sourceRepresentabilityGateCheck = mkSmokeCheck {
  name = "source-representability-gate-check";
  testScript = "tests/source-representability-gate-smoke.sh";
  nativeBuildInputs = [ sourceInventoryBin pkgs.jq pkgs.ripgrep ];
  extraEnv = {
    AB_SOURCE_INVENTORY_BIN = "${sourceInventoryBin}/bin/ab-source-inventory";
  };
};
```

- [ ] **Step 2: Refactor level3AdmissionSmokeCheck**

```nix
level3AdmissionSmokeCheck = mkSmokeCheck {
  name = "parser-ir-level3-admission-smoke-check";
  testScript = "tests/parser-ir-level3-admission-smoke.sh";
  nativeBuildInputs = [ pkgs.jq pkgs.python3 pkgs.ripgrep ];
};
```

- [ ] **Step 3: Refactor plainProseSourceDeltaSmokeCheck**

```nix
plainProseSourceDeltaSmokeCheck = mkSmokeCheck {
  name = "parser-ir-plain-prose-source-delta-smoke-check";
  testScript = "tests/parser-ir-plain-prose-source-delta-smoke.sh";
  nativeBuildInputs = [ pkgs.jq pkgs.python3 pkgs.ripgrep ];
};
```

- [ ] **Step 4: Refactor publicationBundleSmokeCheck**

```nix
publicationBundleSmokeCheck = mkSmokeCheck {
  name = "parser-ir-publication-bundle-smoke-check";
  testScript = "tests/parser-ir-publication-bundle-smoke.sh";
  nativeBuildInputs = [ pkgs.jq pkgs.python3 pkgs.ripgrep ];
};
```

- [ ] **Step 5: Refactor publicationBundleBatchSmokeCheck**

```nix
publicationBundleBatchSmokeCheck = mkSmokeCheck {
  name = "parser-ir-publication-bundle-batch-smoke-check";
  testScript = "tests/parser-ir-publication-bundle-batch-smoke.sh";
  nativeBuildInputs = [ pkgs.jq pkgs.python3 pkgs.ripgrep ];
};
```

- [ ] **Step 6: Refactor aatOracleDataSchemaSmokeCheck**

```nix
aatOracleDataSchemaSmokeCheck = mkSmokeCheck {
  name = "aat-oracle-data-schema-smoke-check";
  testScript = "tests/aat-oracle-data-schema-smoke.sh";
  nativeBuildInputs = [ pythonWithAatSchemaDeps ];
  extraEnv = {
    AB_VALIDATOR_DIRECT_PYTHON = "1";
    AB_DB_ROOT = "$TMPDIR/ab-validator";
  };
};
```

Note: `AB_DB_ROOT` uses shell variable expansion — this will need `extraPreScript` instead of `extraEnv` since `extraEnv` values go through Nix string interpolation. Use `extraPreScript`:

```nix
extraPreScript = ''
  export AB_DB_ROOT="$TMPDIR/ab-validator"
'';
```

- [ ] **Step 7: Refactor adapterFidelityNotesSchemaSmokeCheck**

```nix
adapterFidelityNotesSchemaSmokeCheck = mkSmokeCheck {
  name = "adapter-fidelity-notes-schema-smoke-check";
  testScript = "tests/adapter-fidelity-notes-schema-smoke.sh";
  nativeBuildInputs = [ pythonWithAatSchemaDeps ];
  extraEnv = { AB_VALIDATOR_DIRECT_PYTHON = "1"; };
  extraPreScript = ''
    export AB_DB_ROOT="$TMPDIR/ab-validator"
  '';
};
```

- [ ] **Step 8: Refactor abAatToParserIrCheck — uses different pattern**

This check differs: it uses `cp -R` from `${source}` but also sets `AB_AAT_TO_PARSER_IR_BIN` and `AB_ABC_ROOT`. It fits the factory:

```nix
abAatToParserIrCheck = mkSmokeCheck {
  name = "ab-aat-to-parser-ir-smoke-check";
  testScript = "tests/aat-to-parser-ir-cli-smoke.sh";
  nativeBuildInputs = [ pkgs.babashka pkgs.clojure pkgs.jq pythonWithAatSchemaDeps ];
  extraEnv = {
    AB_ABC_ROOT = "${source}/data/abc-schemas";
    AB_AAT_TO_PARSER_IR_BIN = "${abAatToParserIr}/bin/ab-aat-to-parser-ir";
  };
};
```

- [ ] **Step 9: Refactor aozora2htmlRustParityCheck — uses different source path pattern**

This check uses a local `work_dir` copy but with a custom `cargo` config pointing at vendored deps. It doesn't run a bash smoke test — it runs `cargo build` + `python -m pytest`. This one does NOT fit the factory. Leave it as-is.

- [ ] **Step 10: Refactor aozoraAdapterSmokeCheck — uses vendored cargo deps**

This check uses `${source}` but with `cargo --config` pointing at `aozoraCargoDeps`. It needs `extraPreScript` for the cargo config and a custom `nativeBuildInputs`:

```nix
aozoraAdapterSmokeCheck = mkSmokeCheck {
  name = "aozora-adapter-smoke-check";
  testScript = "tests/aozora-adapter-smoke.sh";
  nativeBuildInputs = [
    rustToolchain
    pkgs.jq
    pkgs.python3
    pkgs.ripgrep
    pkgs.python3Packages.jsonschema
  ];
  extraEnv = {
    AB_AOZORA_BIN = "${referenceAozora}/bin/aozora";
  };
  extraPreScript = ''
    cargo --config "source.crates-io.replace-with='vendored-sources'" \
      --config "source.vendored-sources.directory='${aozoraCargoDeps}'" \
      build --manifest-path "$work_dir/source/adapters/aozora/Cargo.toml" --release --offline
  '';
};
```

Wait — the `cargo build` runs in `$work_dir/source`, but `mkSmokeCheck` already does `cd "$work_dir/source"`. The `extraPreScript` runs after the `cd`, so this works.

- [ ] **Step 11: Refactor aozoraNotationSpecComparatorSmokeCheck**

This check uses `substituteInPlace` before running the bash script. It doesn't fit the factory — the substituteInPlace is a Nix-level build step, not a runtime step. Leave as-is.

- [ ] **Step 12: Refactor aozoraEpub3SmokeCheck — uses inline Python**

This check runs inline Python, not a bash smoke test script. Not a fit for the factory. Leave as-is.

- [ ] **Step 13: Refactor taxonomyDriftCheck — runs a binary, not a smoke test**

Not a fit. Leave as-is.

- [ ] **Step 14: Verify all refactored checks evaluate and build**

```bash
system=$(nix eval --impure --raw --expr builtins.currentSystem)

# Verify Nix evaluation (no build):
for check in \
  source-inventory-smoke \
  source-representability-gate \
  parser-ir-level3-admission-smoke \
  parser-ir-plain-prose-source-delta-smoke \
  parser-ir-publication-bundle-smoke \
  parser-ir-publication-bundle-batch-smoke \
  aat-oracle-data-schema-smoke \
  adapter-fidelity-notes-schema-smoke \
  aat-to-parser-ir-smoke \
  aozora-smoke
do
  nix eval --raw ".#checks.$system.$check.drvPath" >/dev/null
done
```

All should evaluate without Nix errors. Build representative checks to verify full execution:

```bash
nix build ".#checks.$system.parser-ir-level3-admission-smoke" --print-build-logs --no-link
nix build ".#checks.$system.source-inventory-smoke" --print-build-logs --no-link
```

- [ ] **Step 15: Commit**

```bash
git add flake.nix
git commit -m "refactor(nix): migrate all fitting smoke checks to mkSmokeCheck

Converts 10 of the 14 smoke-test derivations to use the factory.
Leaves 4 checks as-is that have incompatible patterns
(aozora2html-rust-parity, aozora-notation-spec-comparator,
aozora-epub3 with inline Python, taxonomy-drift with binary run).

Reduces ~120 lines of repeated Nix boilerplate to ~60 lines of
declarative attribute sets."
```

---

## Phase C: Python Report Library (ab-validator repo)

### Task C1: Create reports/lib/ with shared path and schema utilities

**Repo:** ab-validator

**Files:**
- Create: `reports/lib/__init__.py`
- Create: `reports/lib/paths.py`
- Create: `reports/lib/schemas.py`
- Create: `reports/lib/hashing.py`
- Create: `reports/lib/io.py`

**Interfaces:**
- `paths.repo_root()` → `Path` — resolves to the ab-validator repository root. Index: `reports/lib/paths.py` → `.parent` = `reports/lib/` → `.parents[1]` = repo root.
- `paths.schemas_dir()` → `Path` — `repo_root() / "data" / "abc-schemas" / "schemas"`
- `paths.policy_dir()` → `Path` — `repo_root() / "data" / "abc-schemas" / "data"`
- `schemas.load_schema(path: Path)` → `dict` — `json.loads(path.read_text(encoding="utf-8"))`, loads a JSON Schema from a file path
- `schemas.validate_instance(instance, schema)` — `jsonschema.validate(instance, schema)`, raises on failure
- `hashing.sha256_hex(data: str | bytes)` → `str` — returns `"sha256:<hex>"` format, matching existing report conventions
- `hashing.file_sha256(path: Path)` → `str` — returns `"sha256:<hex>"` format
- `io.read_json(path: Path)` → `Any` — `json.loads(path.read_text(encoding="utf-8"))`
- `io.write_json(path: Path, value, *, indent=2)` — creates parent dirs, writes JSON with trailing newline

- [ ] **Step 1: Create reports/lib/__init__.py**

```python
"""Shared utilities for ab-validator report scripts."""
```

- [ ] **Step 2: Create reports/lib/paths.py**

```python
"""Resolve canonical paths within the ab-validator repository."""

from __future__ import annotations

from pathlib import Path

_LIB_DIR = Path(__file__).resolve().parent  # reports/lib/


def repo_root() -> Path:
    """Return the ab-validator repository root.

    reports/lib/paths.py → reports/lib/ (parent) → reports/ (parents[0]) → repo root (parents[1]).
    """
    return _LIB_DIR.parents[1]


def schemas_dir() -> Path:
    """Return data/abc-schemas/schemas/ with the shared abc JSON schemas."""
    return repo_root() / "data" / "abc-schemas" / "schemas"


def policy_dir() -> Path:
    """Return data/abc-schemas/data/ with publication policies."""
    return repo_root() / "data" / "abc-schemas" / "data"
```

- [ ] **Step 3: Create reports/lib/schemas.py**

```python
"""Load and validate JSON schemas and instances."""

from __future__ import annotations

import json
from pathlib import Path
from typing import Any

import jsonschema


def load_schema(path: Path) -> dict[str, Any]:
    """Load a JSON Schema from a file path."""
    return json.loads(path.read_text(encoding="utf-8"))


def validate_instance(instance: Any, schema: dict[str, Any]) -> None:
    """Validate an instance against a JSON Schema. Raises on failure."""
    jsonschema.validate(instance, schema)


def load_and_validate(path: Path, schema: dict[str, Any]) -> Any:
    """Load a JSON file and validate it against a schema."""
    instance = json.loads(path.read_text(encoding="utf-8"))
    jsonschema.validate(instance, schema)
    return instance
```

- [ ] **Step 4: Create reports/lib/hashing.py**

```python
"""Content-addressable hashing utilities.

All hash functions return the `sha256:<hex>` format used throughout
the report scripts (e.g. reports/parser-ir/publication-coverage.py:1919,
reports/parser-ir/publication-bundle-validate.py:99).
"""

from __future__ import annotations

import hashlib
from pathlib import Path


def sha256_hex(data: str | bytes) -> str:
    """Return `sha256:<hex>` digest of a string or bytes."""
    if isinstance(data, str):
        data = data.encode("utf-8")
    return "sha256:" + hashlib.sha256(data).hexdigest()


def file_sha256(path: Path) -> str:
    """Return `sha256:<hex>` digest of a file's contents."""
    return "sha256:" + hashlib.sha256(path.read_bytes()).hexdigest()
```

- [ ] **Step 5: Create reports/lib/io.py**

```python
"""JSON and file I/O with parent-directory creation."""

from __future__ import annotations

import json
from pathlib import Path
from typing import Any


def read_json(path: Path) -> Any:
    """Read and parse a JSON file."""
    return json.loads(path.read_text(encoding="utf-8"))


def write_json(path: Path, value: Any, *, indent: int = 2) -> None:
    """Write a JSON-serializable value, creating parent directories."""
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(
        json.dumps(value, indent=indent, ensure_ascii=False) + "\n",
        encoding="utf-8",
    )
```

- [ ] **Step 6: Add a test that imports the library from repo root**

Scripts in `reports/` are invoked as `python reports/parser-ir/script.py`. Python puts the script's directory on `sys.path`, not the repo root. The import smoke must run the same way:

```bash
cd "$(git rev-parse --show-toplevel)"
python -c "
import sys
sys.path.insert(0, '.')
from reports.lib.paths import repo_root, schemas_dir, policy_dir
from reports.lib.schemas import load_schema, validate_instance
from reports.lib.hashing import sha256_hex, file_sha256
from reports.lib.io import read_json, write_json
print('reports.lib imports OK')
print(f'repo_root: {repo_root()}')
print(f'schemas_dir: {schemas_dir()}')
"
```

Expected: prints resolved paths without import errors.

- [ ] **Step 7: Commit**

```bash
git add reports/lib/
git commit -m "feat: add reports/lib/ with shared path, schema, hashing, and I/O utilities

Extracts the 4 most duplicated patterns across the 25 Python report
scripts: repo-root resolution, JSON schema loading/validation, SHA-256
hashing, and JSON file I/O with parent-directory creation."
```

### Task C2: Migrate 4 report scripts as proof-of-concept consumers

**Files:**
- Modify: `reports/parser-ir/level3-admission.py`
- Modify: `reports/parser-ir/plain-prose-source-delta.py`
- Modify: `reports/parser-ir/publication-bundle-validate.py`
- Modify: `reports/parser-ir/publication-coverage.py`

- [ ] **Step 1: Migrate level3-admission.py**

This script is invoked as `python reports/parser-ir/level3-admission.py`. To make `from reports.lib import ...` resolve, insert `sys.path` manipulation at the top (after the `from __future__` line):

```python
#!/usr/bin/env python
"""Classify profile-aware Level 3 TEI admission from measured reports."""

from __future__ import annotations

import sys
from pathlib import Path

# Ensure repo root is on sys.path so `from reports.lib import ...` resolves
# when this script is invoked as `python reports/parser-ir/level3-admission.py`.
_REPO_ROOT = Path(__file__).resolve().parents[2]
if str(_REPO_ROOT) not in sys.path:
    sys.path.insert(0, str(_REPO_ROOT))

import argparse
from collections import Counter
from typing import Any

from reports.lib.io import read_json, write_json
from reports.lib.paths import schemas_dir, policy_dir
from reports.lib.hashing import sha256_hex
```

Then replace module-level path constructions:
- `pathlib.Path(__file__).resolve().parents[2]` → delete (replaced by `_REPO_ROOT` above, or use `from reports.lib.paths import repo_root`)
- `REPO_ROOT / "data/abc-schemas/schemas/parser-ir.schema.json"` → `schemas_dir() / "parser-ir.schema.json"`
- `hashlib.sha256(data.encode()).hexdigest()` → `sha256_hex(data)` (note: lib returns `sha256:<hex>`, remove manual `"sha256:" + ...` prefixes)
- `json.loads(path.read_text())` → `read_json(path)`

- [ ] **Step 2: Migrate plain-prose-source-delta.py**

Same sys.path pattern as Step 1. Add the `sys.path.insert` block, replace `pathlib.Path(...).resolve().parents[2]` with `_REPO_ROOT` / `repo_root()`, replace schema paths with `schemas_dir() / "..."`, replace `json.loads(path.read_text())` with `read_json(path)`, replace manual `"sha256:" + hashlib...` with `sha256_hex()`. The lib's `sha256_hex` already includes the `sha256:` prefix.

- [ ] **Step 3: Migrate publication-bundle-validate.py**

Same sys.path pattern. This script has `"sha256:" + hashlib.sha256(path.read_bytes()).hexdigest()` at line 99. Replace with `file_sha256(path)` from reports.lib (which already returns the `sha256:<hex>` format). Also replace schema path constructions, JSON I/O.

- [ ] **Step 4: Migrate publication-coverage.py**

Same sys.path pattern. This is the largest script (1,942 lines). It has extensive schema validation logic. Replace the 6 `REPO_ROOT / "data/abc-schemas/..."` path constructions with `schemas_dir() / "..."` and `policy_dir() / "..."`. Replace `json.loads(path.read_text())` with `read_json(path)`. Replace `"sha256:" + hashlib.sha256(...)` at line 1919 with `sha256_hex(...)` from reports.lib. The `SHA256_PATTERN` regex at line 107 stays (it validates the format).

- [ ] **Step 5: Run each migrated script's smoke test**

```bash
just parser-ir-level3-admission-smoke
just parser-ir-plain-prose-source-delta-smoke
just parser-ir-publication-bundle-smoke
just parser-ir-publication-coverage-smoke
```

Expected: all smoke tests pass with identical output to before the migration.

- [ ] **Step 6: Commit**

```bash
git add reports/lib/ \
        reports/parser-ir/level3-admission.py \
        reports/parser-ir/plain-prose-source-delta.py \
        reports/parser-ir/publication-bundle-validate.py \
        reports/parser-ir/publication-coverage.py
git commit -m "refactor(python): migrate 4 report scripts to reports.lib

Replaces duplicated repo-root discovery, schema path construction,
JSON I/O, and hashing with shared utilities from reports.lib.
Behavior-preserving; all smoke tests pass unchanged."
```

---

## Self-Review

**1. Spec coverage:** The original analysis identified 6 actions. This plan covers the 3 highest-value pre-merge actions:
- Schema contract versioning (Tasks A1–A2)
- Nix smoke-test factory (Tasks B1–B2)
- Python report library (Tasks C1–C2)

The remaining 3 (smoke test runner, Nix structure unification, schema contract CI) are deferred to during/post-monorepo-merge since they depend on the unified repo layout.

**2. Placeholder scan:** No TBDs, TODOs, or "implement later" markers. Every step has concrete code.

**3. Type consistency:** `mkSmokeCheck` signature is used consistently across B1 and B2. `reports.lib` interface is defined in C1 and consumed identically in C2.

**4. Additionally verified:**
- The `source-region-coverage.schema.json` hash mismatch between repos is the exact bug the drift check (Task A2) will catch
- The `reports/lib/` path computation is: `reports/lib/paths.py` → `.parent` = `reports/lib/` → `.parents[1]` = repo root. (`.parents[0]` is `reports/`.)
- The 10 smoke checks migrated in B2 are all the ones that follow the exact copy-to-tmp pattern; the 4 left as-is genuinely differ (cargo vendoring, substituteInPlace, inline Python, binary run)

## Open Design Decisions

### Schema version semantics

The plan adds `version` fields to schemas but does not define what version bumps mean. After Task A1, document in a `schemas/README.md`:
- **Major:** breaking change (hash rotation expected, old hashes eventually removed from legacy set in `validate_design_bundle.clj`)
- **Minor:** additive change (new optional fields, old hashes stay in legacy set)
- **Patch:** docs/clarifications only (no hash change)

Add a CI check that when a schema's file hash changes (caught by a `tei-profile-drift`-style check), its `version` field was also incremented.

### Pre-merge schema drift mitigation

Task A2 (cross-repo drift check) was deferred to monorepo because it requires sibling-directory assumptions. Until the monorepo merge, add a lightweight version-assertion check in ab-validator's `tests/lib/aat-fidelity-env.sh` or a dedicated smoke test that validates the `$id` and `version` of each schema loaded from `data/abc-schemas/schemas/` matches expected values. This catches the case where a schema was edited in abc and the ab-validator copy was forgotten.

### Python invocation path design

The `sys.path.insert(0, str(_REPO_ROOT))` approach in migrated scripts works for all three invocation modes:
- Direct: `python reports/parser-ir/script.py` from repo root
- Smoke test: `python $repo_root/reports/parser-ir/script.py` (absolute path, Nix sandbox or local)
- Arbitrary cwd: `python /absolute/path/to/repo/reports/parser-ir/script.py`

Verified by tracing `tests/*.sh` — all smoke tests use `$repo_root` for Python invocation, not relative paths.

### mkSmokeCheck extensibility

`extraPreScript` runs after source copy and `cd`, before `bash "${testScript}"`. It can be used for pre-processing (sed, file generation, environment-dependent path setup). Two checks currently use `substituteInPlace` as a Nix build step; these should be refactored to use `extraPreScript` when they're migrated to the factory.
