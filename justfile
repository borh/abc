schema-drift:
	@bash scripts/monorepo-schema-drift.sh

split-import-parity-audit:
	@python3 scripts/monorepo-parity-audit.py

tei-version-coherence:
	@bash scripts/monorepo-tei-version-coherence.sh

flake-input-policy:
	@python3 scripts/monorepo-flake-input-policy.py

root-flake-check-no-build:
	@nix flake check --no-build

check-no-build: schema-drift tei-version-coherence flake-input-policy
	@(cd abc && nix flake check --no-build)
	@(cd ab-validator && AB_WORKSPACE_ROOT="$(pwd)/.." nix flake check --no-build)

validate-migration: schema-drift tei-version-coherence flake-input-policy root-flake-check-no-build
