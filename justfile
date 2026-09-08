nix_eval := "nix --option eval-cache false"

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

comment-hygiene:
	@bash scripts/comment-hygiene-check.sh

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
	@{{nix_eval}} flake check --no-build

root-flake-output-contract:
	@bash tests/root-flake-output-contract-smoke.sh

tei-eaj-alignment-probe *args:
	@scripts/run-tei-eaj-probe-workflow.sh alignment-probe {{args}}

tei-eaj-reports-with-probes *args:
	@scripts/run-tei-eaj-probe-workflow.sh reports-with-probes {{args}}

validate-eval-cache-smoke:
	@bash tests/validate-eval-cache-smoke.sh

check-no-build: runtime-config-smoke active-path-hygiene root-flake-output-contract tei-version-coherence flake-input-policy python-quality nix-format-check validate-eval-cache-smoke root-flake-check-no-build
	@(cd ab-validator && AB_WORKSPACE_ROOT="$(pwd)/.." {{nix_eval}} flake check --no-build)

# Publication tests and generated TEI profile must match their checked-in sources.
evidence-gate: soranoha-tests typecheck
	@system="$({{nix_eval}} eval --impure --raw --expr builtins.currentSystem)"; \
	{{nix_eval}} build ".#checks.$system.tei-profile-drift" \
		"./ab-validator#checks.$system.research-clojure-tests" \
		"./ab-validator#checks.$system.research-python-tests" --print-build-logs

# Soranoha kernel + snh conformance suite plus clj-kondo/cljfmt, hermetic
# against the root wrapper's shared Clojure/dependency-cache context.
soranoha-tests:
	@system="$({{nix_eval}} eval --impure --raw --expr builtins.currentSystem)"; \
	{{nix_eval}} build ".#checks.$system.soranoha-tests" --print-build-logs

# release-parser-reproducible: the two release binaries must rebuild
# byte-identically. `nix build --rebuild` re-realizes each derivation and fails
# if the freshly built output differs byte-for-byte from the cached path, so it
# establishes reproducibility against the baseline built first. This needs the
# Nix daemon (a sandboxed runCommand builder cannot invoke it), so it is a just
# recipe rather than a checks.<system> derivation.
release-parser-reproducible:
	{{nix_eval}} build ./ab-validator#ab-aozora ./ab-validator#ab-aat-to-parser-ir --no-link --print-build-logs
	{{nix_eval}} build ./ab-validator#ab-aozora --rebuild --no-link --print-build-logs
	{{nix_eval}} build ./ab-validator#ab-aat-to-parser-ir --rebuild --no-link --print-build-logs
	@echo "release parser binaries rebuild reproducibly"

# Authenticate instrument policies against their declared source closure, and
# establish that capture generation is deterministic, which is what makes a
# capture worth retaining as evidence. Both require building their suites;
# check-no-build's `flake check --no-build` evaluates them without running them.
parser-rq-instrument-identity:
	@system="$({{nix_eval}} eval --impure --raw --expr builtins.currentSystem)"; \
	{{nix_eval}} build "./ab-validator#checks.$system.parser-rq-publication-pytest" \
		"./ab-validator#checks.$system.parser-rq-predicate-hardening-capture-smoke" \
		--no-link --print-build-logs

# Build the flake checks no other gate recipe covers. Every check here is
# hermetic and needs no corpus data, so the only reason any of them sat outside
# the gate was that `flake check --no-build` evaluates without running.
# Deliberately absent: cargo-test, the workspace suite, which runs several
# minutes and stays a focused check; and four ab-validator checks that are
# currently red. The ab-validator set is enumerated rather than run through
# `flake check` so those four cannot hide the rest.
flake-checks:
	@{{nix_eval}} flake check --print-build-logs
	@system="$({{nix_eval}} eval --impure --raw --expr builtins.currentSystem)"; \
	{{nix_eval}} build --no-link --print-build-logs \
		"./ab-validator#checks.$system.cargo-check" \
		"./ab-validator#checks.$system.cargo-clippy" \
		"./ab-validator#checks.$system.cargo-fmt" \
		"./ab-validator#checks.$system.cargo-deny" \
		"./ab-validator#checks.$system.monorepo-path-hygiene-smoke" \
		"./ab-validator#checks.$system.monorepo-workspace-layout-smoke" \
		"./ab-validator#checks.$system.taxonomy-drift" \
		"./ab-validator#checks.$system.source-inventory-smoke" \
		"./ab-validator#checks.$system.source-representability-gate" \
		"./ab-validator#checks.$system.aat-fidelity-duckdb-smoke" \
		"./ab-validator#checks.$system.reports-pytest" \
		"./ab-validator#checks.$system.parser-rq-core-attempt-python-tests" \
		"./ab-validator#checks.$system.parser-rq-campaign-provenance-python-tests" \
		"./ab-validator#checks.$system.parser-rq-predicate-hardening-capture-python-tests" \
		"./ab-validator#checks.$system.parser-rq-resource-capture-smoke" \
		"./ab-validator#checks.$system.upstream-aozora-notation-spec"

validate: check-no-build evidence-gate parser-rq-instrument-identity flake-checks release-parser-reproducible

typecheck:
	@system="$({{nix_eval}} eval --impure --raw --expr builtins.currentSystem)"; \
	{{nix_eval}} build ".#checks.$system.soranoha-typecheck" --print-build-logs
