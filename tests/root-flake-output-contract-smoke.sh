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
