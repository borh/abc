nix_eval := "nix --option eval-cache false"

tei-version-coherence:
	@bash scripts/monorepo-tei-version-coherence.sh

flake-input-policy:
	@python scripts/monorepo-flake-input-policy.py

# Recorded schema hashes are assertions about which schema a record or an
# importer was written against. Recompute them, so a schema edit fails here
# rather than at the first import of real data.
schema-hash-coherence:
	@python scripts/monorepo-schema-hash-coherence.py

runtime-config:
	@bash -lc 'source scripts/soranoha-runtime-env.sh; soranoha_runtime_summary'

runtime-config-smoke:
	@bash tests/runtime-config-smoke.sh

active-path-hygiene:
	@bash tests/monorepo-active-path-hygiene-smoke.sh

python-quality:
	@bash scripts/python-quality.sh

# Scans the files git tracks, so it runs here rather than as a flake check:
# the check source is a store path with no git directory, and falling back to a
# tree walk would scan build outputs and any local corpus checkout.
comment-hygiene:
	@bash scripts/comment-hygiene-check.sh

docs-links:
	@python scripts/docs-link-check.py

# The two JIS X 0213 gaiji tables must still match the licensed source table
# they are generated from, so neither can be hand-edited away from its terms.
gaiji-table-drift:
	@python ab-validator/crates/ab-aozora-encoding/data/generate_jisx0213_tables.py --check

# Re-derive every corpus figure quoted in prose from the pinned Aozora catalog.
# Not part of any bundled gate: it needs a corpus checkout, which the sandboxed
# checks do not have. Set CORPUS_CHECKOUT, or pass --aozora-root. `--stems`
# adds the figures that need a pass over every work archive.
catalog-figures *args:
	@python scripts/catalog-figures-check.py {{args}}

# The half of that check which needs no corpus, so it can be gated: every file
# recorded as quoting a figure still quotes the same number.
figure-quotes:
	@python scripts/catalog-figures-check.py --quotes-only

# Build a browsable preview of the public serving tree and serve it on
# 127.0.0.1:PORT. Not a release: the chain is local and throwaway, signed with
# the checked-in conformance fixture keys, and the assessment snapshot is
# asserted rather than evaluated. Everything a reader would see comes from the
# real publication and serving code. Needs a corpus checkout: set
# CORPUS_CHECKOUT. SORANOHA_PREVIEW_DIR moves the work directory, which
# defaults under XDG_CACHE_HOME so a preview never touches the tree.
site-preview works="8" port="8787":
	@bash scripts/site-preview.sh {{works}} {{port}}

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

check-no-build: runtime-config-smoke active-path-hygiene root-flake-output-contract tei-version-coherence flake-input-policy schema-hash-coherence figure-quotes python-quality comment-hygiene docs-links gaiji-table-drift nix-format-check validate-eval-cache-smoke root-flake-check-no-build
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
