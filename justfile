schema-drift:
	@bash scripts/monorepo-schema-drift.sh

check-no-build: schema-drift
	@(cd abc && nix flake check --no-build)
	@(cd ab-validator && AB_WORKSPACE_ROOT="$(pwd)/.." nix flake check --no-build)
