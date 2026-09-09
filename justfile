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

docs-links:
	@python scripts/docs-link-check.py

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

check-no-build: runtime-config-smoke active-path-hygiene root-flake-output-contract tei-version-coherence flake-input-policy python-quality docs-links nix-format-check validate-eval-cache-smoke root-flake-check-no-build
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
# byte-identically. `nix build --rebuild` re-realizes a derivation and fails if
# the freshly built output differs byte-for-byte from the cached path, so it
# establishes reproducibility against the baseline built first. This needs the
# Nix daemon (a sandboxed runCommand builder cannot invoke it), so it is a just
# recipe rather than a checks.<system> derivation.
#
# Each derivation path is resolved once and the rebuild names it directly rather
# than re-evaluating the flake attribute. A flake source in a dirty git tree
# carries the working tree into the derivation hash, so an edit landing between
# the two steps makes the second one name a derivation that was never built, and
# nix then reports that it cannot check rather than that the bytes differ. That
# red says nothing about reproducibility, and this gate is release evidence.
# Pinning removes the failure outright: both steps address one derivation
# whatever the tree does meanwhile, so no clean-tree precondition is needed.
release-parser-reproducible:
	@aozora="$({{nix_eval}} eval --raw ./ab-validator#ab-aozora.drvPath)" \
	&& parser_ir="$({{nix_eval}} eval --raw ./ab-validator#ab-aat-to-parser-ir.drvPath)" \
	&& {{nix_eval}} build "$aozora^*" "$parser_ir^*" --no-link --print-build-logs \
	&& {{nix_eval}} build "$aozora^*" --rebuild --no-link --print-build-logs \
	&& {{nix_eval}} build "$parser_ir^*" --rebuild --no-link --print-build-logs \
	&& echo "release parser binaries rebuild reproducibly"

# Authenticate instrument policies against their declared source closure, and
# establish that capture generation is deterministic, which is what makes a
# capture worth retaining as evidence. Both require building their suites;
# check-no-build's `flake check --no-build` evaluates them without running them.
parser-rq-instrument-identity:
	@system="$({{nix_eval}} eval --impure --raw --expr builtins.currentSystem)"; \
	{{nix_eval}} build "./ab-validator#checks.$system.parser-rq-publication-pytest" \
		"./ab-validator#checks.$system.parser-rq-predicate-hardening-capture-smoke" \
		--no-link --print-build-logs

# Build every flake check. `flake check --no-build` evaluates without running,
# which is how thirty checks stayed silently red while appearing gated, so the
# ab-validator set is built here by name. The names come from the flake rather
# than a hand-kept list: a check added later is gated without touching this
# recipe, which is the failure mode being closed.
#
# `cargo-test`, `default` and `ab-validator` are the same multi-minute workspace
# derivation under three names, excluded here and run as a focused check.
flake-checks:
	@{{nix_eval}} flake check --print-build-logs
	@system="$({{nix_eval}} eval --impure --raw --expr builtins.currentSystem)"; \
	names="$({{nix_eval}} eval --raw "./ab-validator#checks.$system" --apply \
		'cs: builtins.concatStringsSep " " (builtins.filter (n: !(builtins.elem n [ "default" "ab-validator" "cargo-test" ])) (builtins.attrNames cs))')"; \
	targets=""; \
	for name in $names; do targets="$targets ./ab-validator#checks.$system.$name"; done; \
	{{nix_eval}} build --no-link --print-build-logs $targets

validate: check-no-build evidence-gate parser-rq-instrument-identity flake-checks release-parser-reproducible

typecheck:
	@system="$({{nix_eval}} eval --impure --raw --expr builtins.currentSystem)"; \
	{{nix_eval}} build ".#checks.$system.soranoha-typecheck" --print-build-logs
