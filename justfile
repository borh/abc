schema-drift:
	@bash scripts/monorepo-schema-drift.sh

split-import-parity-audit:
	@python3 scripts/monorepo-parity-audit.py

root-flake-check-no-build:
	@nix flake check --no-build

check-no-build: schema-drift
	@(cd abc && nix flake check --no-build)
	@(cd ab-validator && AB_WORKSPACE_ROOT="$(pwd)/.." nix flake check --no-build)

validate-migration: schema-drift root-flake-check-no-build
