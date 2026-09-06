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
evidence-gate: soranoha-tests
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

# parser-rq-instrument-identity: build the predicate-hardening identity suite,
# which authenticates each instrument policy against the reviewed source closure
# it claims to bind. `check-no-build` only EVALUATES flake checks, so this
# derivation had never been built by any recipe; the suite meanwhile errored at
# import for want of a monorepo-shaped staging root, and two committed identity
# faults went unobserved. Building it here is what makes that repair stick.
parser-rq-instrument-identity:
	@system="$({{nix_eval}} eval --impure --raw --expr builtins.currentSystem)"; \
	{{nix_eval}} build "./ab-validator#checks.$system.parser-rq-publication-pytest" \
		--no-link --print-build-logs

validate: check-no-build evidence-gate parser-rq-instrument-identity release-parser-reproducible
