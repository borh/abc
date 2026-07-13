#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

nix flake show --json --all-systems "$repo_root" | python -c '
import json
import sys

raw_outputs = json.load(sys.stdin)
if "inventory" in raw_outputs:
    outputs = {
        output_name: {
            system: system_value.get("children", {})
            for system, system_value in output_value.get("output", {})
            .get("children", {})
            .items()
        }
        for output_name, output_value in raw_outputs["inventory"].items()
    }
else:
    outputs = raw_outputs
systems = {"aarch64-linux", "x86_64-linux"}
expected_output_names = {"apps", "checks", "devShells", "formatter", "packages"}
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
        "monorepo-adr-governance",
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
actual_output_names = set(outputs)
if actual_output_names != expected_output_names:
    errors.append(
        f"top-level outputs: expected {sorted(expected_output_names)}, got {sorted(actual_output_names)}"
    )

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
