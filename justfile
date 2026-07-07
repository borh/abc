schema-drift:
	@bash scripts/monorepo-schema-drift.sh

split-import-parity-audit:
	@python scripts/monorepo-parity-audit.py

tei-version-coherence:
	@bash scripts/monorepo-tei-version-coherence.sh

flake-input-policy:
	@python scripts/monorepo-flake-input-policy.py

runtime-config:
	@bash -lc 'source scripts/soranoha-runtime-env.sh; soranoha_runtime_summary'

runtime-config-smoke:
	@bash tests/runtime-config-smoke.sh

python-quality:
	@bash scripts/python-quality.sh

nix-format-check:
	@find . \
		-path './.git' -prune -o \
		-path './.direnv' -prune -o \
		-path './result*' -prune -o \
		-name '*.nix' -print0 \
		| xargs -0 nixfmt --check

root-flake-check-no-build:
	@nix flake check --no-build

check-no-build: runtime-config-smoke schema-drift tei-version-coherence flake-input-policy python-quality nix-format-check
	@(cd abc && nix flake check --no-build)
	@(cd ab-validator && AB_WORKSPACE_ROOT="$(pwd)/.." nix flake check --no-build)

validate-migration: runtime-config-smoke schema-drift tei-version-coherence flake-input-policy python-quality nix-format-check root-flake-check-no-build
