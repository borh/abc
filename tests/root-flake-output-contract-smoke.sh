#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

# eval-cache disabled to match the validate cache-independence
# policy: this check exists to detect an output surface that drifted from the
# contract, which a stale eval cache would hide.
nix --option eval-cache false flake show --json --all-systems "$repo_root" | python -c '
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
expected_output_names = {
    "apps",
    "checks",
    "devShells",
    "formatter",
    "nixosConfigurations",
    "nixosModules",
    "packages",
}
# Outputs keyed by name rather than by system.
expected_flat = {
    "nixosConfigurations": {"snh-ceremony"},
    "nixosModules": {"snh-ceremony-iso"},
}
expected = {
    "apps": {
        "flake-input-policy",
        "soranoha-kernel",
        "soranoha-replay",
        "soranoha-compare-serving",
        "soranoha-publication-replay",
        "regenerate-tei-profile",
        "tei-version-coherence",
        "validate",
    },
    "packages": {"tei-p5-reference", "tei-profile-artifacts"},
    "devShells": {"default"},
    "checks": {
        "monorepo-active-path-hygiene",
        "monorepo-ceremony-image",
        "monorepo-figure-quotes",
        "monorepo-flake-input-policy",
        "monorepo-nix-format",
        "monorepo-python-quality",
        "monorepo-runtime-config",
        "monorepo-schema-hash-coherence",
        "monorepo-tei-p5-reference",
        "monorepo-tei-version-coherence",
        "monorepo-workflow-run-lib",
        "soranoha-tests",
        "soranoha-typecheck",
        "tei-profile-drift",
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

for output_name, expected_names in expected_flat.items():
    actual_names = set(outputs.get(output_name, {}))
    if actual_names != expected_names:
        errors.append(
            f"{output_name}: expected {sorted(expected_names)}, got {sorted(actual_names)}"
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
