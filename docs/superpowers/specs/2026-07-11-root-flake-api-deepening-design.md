# Root Flake API Deepening Design

**Date:** 2026-07-11
**Status:** Approved

## Purpose

Restructure the Soranoha root flake from a near-complete mirror of the `abc`
and `ab-validator` flakes into a small, explicit operator-facing integration
API.

The restructuring must reduce root output sprawl without removing component
capabilities, weakening validation coverage, changing component ownership, or
introducing machine-local runtime dependencies.

## Current Problem

The pre-change machine-readable output snapshot is the source of truth for
output counts. At design time it reports, per supported system, 35 apps, 40
packages, 72 checks, and 7 development shells. Most of these outputs are
produced by mechanically prefixing and merging the component output sets:

- `abc` outputs become `abc-*` root outputs.
- `ab-validator` outputs become `ab-validator-*` root outputs.

This combines three distinct concerns in the root API:

1. supported Soranoha operator workflows;
2. component-specific developer tools and build artifacts;
3. repository validation and CI orchestration.

The prefixed exports were useful during the monorepo migration, but the
migration fallback now obscures ownership and makes every component output look
like a supported root contract.

## Design Decisions

### Intended consumer

The root flake primarily serves Soranoha operators and downstream consumers.
Developers use the component flakes directly for specialist work:

```sh
nix run ./abc#<app>
nix build ./ab-validator#<package>
nix develop ./abc#<shell>
```

### Compatibility policy

The change is an intentional clean break. Prefixed root aliases are removed
without a deprecation period or compatibility redirects.

All active, repository-controlled callers are migrated in the same change.
Dated reports, completed plans, and checked-in evidence records retain their
historical commands unless they are also active regeneration instructions.

The possibility of unknown external consumers is accepted as a release-note
risk. Removed attributes fail explicitly during flake attribute resolution.

### Root ownership rule

An output belongs at the root only when at least one of the following is true:

- It is a supported end-to-end operator workflow.
- It enforces a contract between components or between a component and a
  root-owned input.
- It is a cross-component artifact whose version or identity is owned by the
  monorepo.
- It provides the integrated monorepo development environment.

Convenience alone is not sufficient for promotion to the root.

### Component ownership

- ABC owns publication schemas, TEI profile policy, manifest identity,
  registry admission, and their associated tools.
- `ab-validator` owns parser and adaptor measurement, AAT evidence, parser-IR
  conversion evidence, corpus reports, dictionaries, analyzers, and their
  associated tools.
- The root may consume private component outputs to assemble integration
  workflows. Consumption does not imply re-export.
- Neither component gains a new dependency on the other as part of this work.

## Root Output Contract

### Apps

The root exposes exactly these five apps per supported system:

- `soranoha`
- `validate-migration`
- `schema-drift`
- `tei-version-coherence`
- `flake-input-policy`

`soranoha` remains the primary publication workflow dispatcher. It qualifies
as an end-to-end operator workflow. `validate-migration` qualifies separately
because it enforces root-owned repository and cross-component release
contracts; it is intentionally retained even though it orchestrates developer
quality checks internally. The other three apps are direct,
automation-friendly release diagnostics with distinct contracts.

`python-quality` is removed from root apps because it is a developer quality
gate rather than an operator workflow. It remains available through the root
`justfile`, the `monorepo-python-quality` check, and `validate-migration`.

All prefixed component apps are removed from the root.

### Packages

The root exposes exactly one package per supported system:

- `tei-p5-reference`

The root owns the pinned TEI version and its cross-component coherence
contract. Parser implementations, adapters, corpora, dictionaries, analyzers,
and other component build artifacts remain available only from their owning
component flake.

### Development shells

The root exposes only `devShells.<system>.default`, representing the integrated
monorepo environment.

Specialist shells remain component-owned and are entered with commands such as:

```sh
nix develop ./abc#validation
nix develop ./ab-validator#aozora2html
```

### Formatter

The existing per-system `nixfmt` formatter remains unchanged.

### Checks

The root exposes only root-owned policy and integration checks. It retains the
existing checks whose behavior crosses component, repository, or root-input
boundaries:

- `monorepo-aat-materialization-workflow`
- `monorepo-aat-run-set`
- `monorepo-active-path-hygiene`
- `monorepo-batch-run-staleness`
- `monorepo-fidelity-lock-idempotency`
- `monorepo-flake-input-policy`
- `monorepo-nix-format`
- `monorepo-python-quality`
- `monorepo-runtime-config`
- `monorepo-schema-drift`
- `monorepo-tei-p5-reference`
- `monorepo-tei-version-coherence`
- `monorepo-workflow-run-lib`
- `tei-eaj-aozora-alignment-probe-generation`

The root removes:

- all prefixed `abc-*` checks;
- all prefixed `ab-validator-*` checks;
- the unprefixed `parser-ir-ortho-publication-smoke` component-check alias.

Component correctness remains owned and exposed by the component flakes. The
root `justfile` coordinates complete repository validation by running root and
component checks directly. Both `just validate-migration` and the retained
`validate-migration` app must explicitly run `nix flake check --no-build` for
the root, `abc`, and `ab-validator`. The validator invocation must follow the
existing workspace-aware form:

```sh
(
  cd ab-validator
  AB_WORKSPACE_ROOT="$(pwd)/.." nix flake check --no-build
)
```

This is a deliberate orchestration expansion in two places. It replaces the
component evaluation previously obtained indirectly through mirrored root
checks while preserving the effective validation coverage.

## Retained Soranoha Integration Wrapper

The retained `soranoha` app is itself a substantial cross-component wrapper,
not a direct alias of `abc#soranoha`. The existing
`mkAdapterAwareSoranohaApp` construction privately consumes these
`ab-validator` packages:

- `upstream-parser-aozora2html`
- `aozora2html-adapter`
- `ab-aat-to-parser-ir`

It also consumes the validator-owned aozora2html adapter script and AAT mapping
data. The wrapper provides its ABC dispatcher with a pinned runtime `PATH` and
these six environment variables:

- `AB_SEVENZIP_BIN`
- `AB_AOZORA2HTML_ADAPTER`
- `AB_AOZORA2HTML_BIN`
- `AB_AOZORA2HTML_MAPPER_BIN`
- `AB_AAT_TO_PARSER_IR_BIN`
- `AB_AAT_TO_PARSER_IR_MAPPING`

This wiring is part of the retained root app contract. It keeps publication
materialization hermetic and prevents fallback to a stub parser. The
implementation must preserve `mkAdapterAwareSoranohaApp`, its dependency set,
runtime `PATH`, six environment assignments, and final dispatch byte-for-byte.
These component outputs are private dependencies and are not re-exported.

## Cross-Component Evidence Workflows

The current root apps `abc-tei-eaj-aozora-alignment-probe` and
`abc-tei-eaj-aozora-reports-with-probes` are not plain aliases. They inject the
`ab-validator` package `ab-aat-to-parser-ir` into ABC report tooling through
`ABC_TEI_EAJ_ALIGNMENT_PROBE_BIN`.

These are repository-maintainer evidence workflows rather than supported
operator workflows, so they do not remain root apps. Their integration behavior
is preserved as follows:

- ABC retains dependency-injected component-local report apps.
- The root `justfile` gains `tei-eaj-alignment-probe` and
  `tei-eaj-reports-with-probes` recipes. Each obtains the package path with
  `nix build ./ab-validator#ab-aat-to-parser-ir --no-link --print-out-paths`,
  exports
  `ABC_TEI_EAJ_ALIGNMENT_PROBE_BIN=<path>/bin/ab-aat-to-parser-ir`, and invokes
  the corresponding app with `nix run ./abc#<app>`.
- Active report and presentation regeneration instructions use those recipes.
- The existing root integration check continues to construct the same
  cross-component dependency internally.
- The wiring is not moved into the ABC flake, because doing so would reverse
  the component dependency boundary.

The recipes must use flake outputs and pinned repository inputs. They must not
fall back to binaries from ambient `PATH`, machine-local output directories, or
untracked `references/` paths.

## Migration Rules

Simple component aliases migrate mechanically:

```text
.#abc-<name>          -> ./abc#<name>
.#ab-validator-<name> -> ./ab-validator#<name>
```

Root-owned names such as `.#soranoha` and `.#tei-p5-reference` remain
unchanged.

The implementation must inventory references before editing and classify each
one as:

- active caller to migrate;
- active cross-component workflow to route through a `just` recipe;
- historical evidence to preserve;
- obsolete documentation to remove.

No compatibility aliases are retained after the active migration is complete.

## Implementation Shape

The root uses an explicit facade rather than allowlist or traversal helpers:

- Delete `prefixAttrs` and all blanket component-output merges.
- Construct the five approved apps explicitly. Preserve the existing
  `mkAdapterAwareSoranohaApp` definition and its use for `soranoha` verbatim.
- Construct `tei-p5-reference` explicitly.
- Construct only the integrated default shell.
- Retain root-owned checks explicitly.
- Keep `optionalOutputAttrs` for genuine private consumption of component
  outputs, including `abc#soranoha`, `upstream-parser-aozora2html`,
  `aozora2html-adapter`, and `ab-aat-to-parser-ir`; remove or narrow other uses
  if the final usage audit permits.
- Preserve the existing Linux-only root supported-system contract.

This change does not introduce generic export helpers or split every output
class into a separate Nix file. Explicit repetition is intentional API review
friction. Reorganizing the large root check implementation is a separate
concern and is outside this change unless required to preserve behavior.

"Construct explicitly" does not authorize equivalent rewrites of retained
wrapper scripts. Nix derivation identity includes generated script text, and
the existing `validate-migration` comment documents that helper interpolation
can re-indent a multi-line body and change its derivation hash. Retained
wrappers stay textually identical unless this spec explicitly requires a
behavior change. The new direct component checks make
`validate-migration` the one intentional retained-app wrapper change.

## Failure Behavior

- Removed aliases produce a normal missing-flake-attribute failure.
- Component commands retain their existing arguments, environment variables,
  output formats, and exit behavior.
- Root apps retain their existing arguments, output formats, and exit behavior.
- Cross-component `just` recipes fail if the required flake output cannot be
  evaluated or built; they do not silently select another binary.
- The `validate-migration` app wrapper changes intentionally so that it checks
  the root and both component flakes directly after mirrored checks are
  removed. Its validator invocation supplies `AB_WORKSPACE_ROOT` exactly as
  the established `just check-no-build` path does. Its validation coverage must
  not shrink.
- No runtime configuration, corpus location policy, source identity, schema
  identity, or TEI version changes as part of this restructuring.

## Verification

Before modifying outputs, record a machine-readable snapshot for all three
flakes. Derive all pre-change output counts from this snapshot rather than from
the illustrative counts in this document. On the build host, record derivation
paths for derivations and program paths for apps. For both supported systems,
record attribute names and output structure through evaluation; cross-system
derivation-path identity is not required when the corresponding platform cannot
be built on the host.

Verification distinguishes three cases:

1. **Retained identity:** every retained non-`self`-dependent root output and
   every unchanged component output has the same derivation or program path,
   except where a documented wrapper change makes identity preservation
   impossible. Root checks that embed `self` necessarily receive new derivation
   paths when repository content changes; verify their attribute structure and
   focused builds instead.
2. **Moved accessibility:** every removed component alias still resolves from
   its owning component flake with the same derivation or program identity.
3. **Intentional removal:** removed root aliases fail attribute resolution and
   are absent from `nix flake show`.

Additional verification must prove:

- The root exposes 5 apps, 1 package, 1 development shell, the formatter, and
  14 root-owned checks per supported system. Derive and compare both the before
  and after counts from the snapshots.
- No root output is generated by prefixing or traversing a component output
  set.
- All active repository references use retained root outputs, qualified
  component outputs, or the approved maintainer recipes.
- The two cross-component evidence recipes provide the Nix-built
  `ab-aat-to-parser-ir` binary and successfully evaluate their ABC app targets.
- Immediately after the root-only output refactor, compare the realized
  `soranoha` wrapper text with store hashes removed. Raw program-path identity
  is not expected: any tracked root edit changes the parent Git source snapshot
  and therefore churns the nested `path:./ab-validator` source hash embedded in
  the wrapper. Normalized realized-script equality, together with stable direct
  ABC app and validator package identities, proves that the runtime `PATH`, six
  `AB_*` assignments, validator-owned adapter and mapping path shapes, and final
  ABC dispatch were preserved apart from expected source identities. Repeat the
  normalized comparison after later caller migration; do not require raw
  wrapper program-path equality at either point.
- The `validate-migration` app and `just validate-migration` each evaluate the
  root, `abc`, and `ab-validator` checks with `--no-build`; both validator
  invocations set `AB_WORKSPACE_ROOT` to the monorepo root.
- Root `nix flake check --no-build` succeeds.
- Direct `abc` and `ab-validator` no-build checks succeed.
- `just validate-migration` succeeds.
- Focused Nix formatting checks succeed.

Large corpus builds are not required merely to prove the namespace change, but
any wrapper whose environment injection changes must receive a focused
behavioral check proportional to that change.

## API Review Policy

Future root-output additions require a review note stating:

- the intended operator or downstream consumer;
- why the output cannot remain component-local;
- which cross-component or root-owned contract it represents;
- its stability and compatibility expectations;
- how it is verified.

An output that lacks a concrete current consumer and root-owned contract stays
in its component flake.

## Design Review

**Primary classification: Deepen.** The root becomes a small integration
interface over substantial component and workflow machinery.

**Secondary classification: Decomplect.** Operator API, developer convenience,
and CI aggregation are separated instead of being braided through blanket
exports.

**Evidence:** The design-time snapshot reports 155 named per-system outputs,
while repository callers use only a small subset. The implementation snapshot
must re-derive that number. The root currently creates most of the surface
through `prefixAttrs`. Two apparent probe aliases contain genuine
cross-component injection and receive explicit migration treatment; the
retained `soranoha` app contains a larger private cross-component injection that
must remain byte-identical.

**Assumption:** Unknown external consumers do not outweigh the selected clean
cutover policy. Evidence of important external reliance would change release
communication or timing, but not the target ownership boundary.

**Hazard check:** No trust boundary, shared mutable state, identity format, or
data lifecycle changes. The material risks are output-name compatibility,
wrapper environment injection, and incomplete caller migration. Snapshot and
focused behavior checks characterize those risks before deletion.

## Out of Scope

- Adding `flake-schemas`.
- Renaming ABC namespaces, schemas, vocabularies, or artifact identities.
- Redesigning the `soranoha` command-line interface.
- Changing parser, publication, TEI, manifest, or validation behavior.
- Changing supported systems.
- Consolidating the component flakes.
- Refactoring the internal construction of every root check.
- Removing specialist outputs from their owning component flakes.
