schema-drift:
	@bash scripts/monorepo-schema-drift.sh

sync-schema-mirror:
	@bash scripts/sync-schema-mirror.sh

tei-version-coherence:
	@bash scripts/monorepo-tei-version-coherence.sh

flake-input-policy:
	@python scripts/monorepo-flake-input-policy.py

runtime-config:
	@bash -lc 'source scripts/soranoha-runtime-env.sh; soranoha_runtime_summary'

runtime-config-smoke:
	@bash tests/runtime-config-smoke.sh

active-path-hygiene:
	@bash tests/monorepo-active-path-hygiene-smoke.sh

python-quality:
	@bash scripts/python-quality.sh

nix-format-check:
	@find . \
		-path './.git' -prune -o \
		-path './.claude' -prune -o \
		-path './.worktrees' -prune -o \
		-path './worktrees' -prune -o \
		-path './.direnv' -prune -o \
		-path './result*' -prune -o \
		-name '*.nix' -print0 \
		| xargs -0 nixfmt --check

root-flake-check-no-build:
	@nix flake check --no-build

root-flake-output-contract:
	@bash tests/root-flake-output-contract-smoke.sh

tei-eaj-alignment-probe *args:
	@scripts/run-tei-eaj-probe-workflow.sh alignment-probe {{args}}

tei-eaj-reports-with-probes *args:
	@scripts/run-tei-eaj-probe-workflow.sh reports-with-probes {{args}}

check-no-build: runtime-config-smoke active-path-hygiene root-flake-output-contract schema-drift tei-version-coherence flake-input-policy python-quality nix-format-check root-flake-check-no-build
	@(cd abc && nix flake check --no-build)
	@(cd ab-validator && AB_WORKSPACE_ROOT="$(pwd)/.." nix flake check --no-build)

phase5-checkpoint:
	@system="$(nix eval --impure --raw --expr builtins.currentSystem)"; \
	nix build "./ab-validator#checks.$system.phase5-checkpoint" --print-build-logs

validate-migration: check-no-build phase5-checkpoint

# Unseeded simulation soak (15x counts); failures print the seed to replay.
sim-soak:
	cd abc && ABC_SIM_SOAK=1 clojure -M:test:kaocha -m kaocha.runner --focus :simulation

# Replay the pinned aozorabunko history through the audit machinery and
# diff against the committed baseline (needs network on a cold cache).
replay-aozora:
	cd abc && clojure -M:abc/aozora-replay -- --check

# Re-run the replay and rewrite the committed baseline (adjudicate the
# diff in the same PR as whatever caused it).
replay-aozora-update:
	cd abc && clojure -M:abc/aozora-replay -- --update

source-bundle-corpus-check:
	nix build ./abc#checks.x86_64-linux.source-bundle-corpus --print-build-logs
