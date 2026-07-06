schema-drift:
	@bash scripts/monorepo-schema-drift.sh

parity-audit:
	@python3 scripts/monorepo-parity-audit.py

root-flake-check-no-build:
	@nix flake check --no-build

check-no-build: schema-drift
	@(cd abc && nix flake check --no-build)
	@(cd ab-validator && AB_WORKSPACE_ROOT="$(pwd)/.." nix flake check --no-build)

validate-migration: parity-audit root-flake-check-no-build
