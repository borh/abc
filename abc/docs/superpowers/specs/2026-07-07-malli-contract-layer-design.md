# Malli Contract Layer Design

Date: 2026-07-07
Status: Draft

## Goal

Use Malli as ABC's Clojure-owned contract layer for in-process data,
EDN registries, policy documents, option maps, and cross-field invariants,
without replacing the existing protocol authorities:

- JSON Schema remains the cross-language wire contract for Rust, Python, and
  ABC JSON artifacts.
- TEI ODD/RelaxNG/Schematron remain the TEI publication contract.
- SHACL remains the RDF graph contract.
- Rust serde/domain types remain the Rust in-process contract.

The purpose is not to make Malli the only schema technology. The purpose is to
stop hand-writing Clojure validators where the same facts can be expressed once
as schema data, then reused for validation, generation, documentation, and
optional JSON Schema export.

## Current State

Malli is already present and loaded through `abc.tools.malli`, but its active
surface is narrow:

- `abc.annotation.schema/registry` is the only registry-owning namespace.
- `abc.tools.malli/install!` composes that registry with a few
  design-bundle cross-event schemas.
- `abc.tools.schema` still validates on-disk JSON artifacts through
  `m3.json-schema`.
- Several EDN and policy contracts are still hand-written as predicates and
  string-building validators.

The clearest duplication is in:

- `abc.tools.aat-parser-ir-compat`
- `abc.tools.parser-evidence`

Both define local copies of common contract ideas:

- `sha256:` hash strings
- non-blank strings
- semver strings
- positive and non-negative integers
- required map keys
- enum membership
- nested evidence-scope shape
- duplicate identity keys

That code is correct enough today, but it is not a good long-term contract
surface: the shape, errors, generators, and documentation are all separate
from the validation logic.

## Design Principles

1. **One authority per boundary.** Malli may author Clojure-owned contracts,
   but it must not silently supersede JSON Schema, TEI, SHACL, or Rust type
   authority.
2. **Schema definitions as data.** When ABC owns a data shape, prefer a Malli
   schema value over bespoke predicate code.
3. **Generated artifacts are checked.** Any JSON Schema exported from Malli is
   committed only with a drift gate proving the export matches source schemas.
4. **Function schemas at boundaries only.** Use `m/=>` or `mx/defn` for CLI
   option maps, loaded document values, registry admission functions, and
   materializer boundaries. Avoid instrumenting every local helper.
5. **No runtime Malli dependency for Python/Rust.** Python and Rust consume
   generated JSON Schema or stable data artifacts, not Clojure runtime calls.
6. **Keep semantic checks explicit.** Malli handles structural contracts and
   local cross-field invariants. Higher-level domain checks that depend on
   external files, hashes, graph semantics, or TEI validation stay in the
   owning tool namespace.

## Authority Matrix

| Surface | Primary authority | Malli role |
|---|---|---|
| `schemas/parser-ir.schema.json` | JSON Schema | Do not regenerate in v0; Malli may mirror small test fixtures only |
| `schemas/manifest.schema.json` | JSON Schema | Do not regenerate in v0 |
| `schemas/source-region-coverage.schema.json` | JSON Schema | Possible future Malli source after explicit schema rotation |
| TEI profile | ODD, RelaxNG, Schematron | None, except option/result maps around validators |
| RDF graph validity | SHACL | None, except option/result maps around validators |
| `data/aat-parser-ir-compatibility.edn` | ABC EDN registry | Malli source of structural contract |
| `data/parser-evidence-citations.edn` | ABC EDN registry | Malli source of structural contract |
| `data/source-region-publication-policy-v0.json` | ABC policy data | Malli source candidate; export JSON Schema if Python/Rust need it |
| `data/parser-ir-publication-policy-v0.json` | ABC policy data | Malli source candidate; export JSON Schema if Python/Rust need it |
| ABC Clojure CLI option maps | Clojure function boundary | Malli boundary schemas |
| ABC Clojure materializer result maps | Clojure function boundary | Malli boundary schemas and generated examples |

## Proposed Architecture

Extend `abc.tools.malli` into a small contract foundation with three layers.

### Layer 1 - Shared Scalar Schemas

Add reusable names to the default registry:

- `::nonblank-string`
- `::sha256-hash`
- `::semver`
- `::positive-int`
- `::nonnegative-int`
- `::workspace-logical-path`
- `::schema-id`
- `::schema-hash`

These names replace local duplicated predicates in EDN validators and can be
used by future policy schemas.

### Layer 2 - Clojure-Owned Document Schemas

Add Malli schemas for ABC-owned EDN/JSON documents whose current validators are
mostly hand-written:

- `::aat-parser-ir-compat-registry`
- `::aat-parser-ir-compat-entry`
- `::aat-parser-ir-evidence-scope`
- `::parser-evidence-index`
- `::parser-evidence-entry`
- `::source-region-publication-policy`
- `::parser-ir-publication-policy`

These schemas express local shape and basic cross-field invariants. Examples:

- compatibility registry entries require all match keys
- `:mapping_version` is semver
- hash fields match `sha256:[0-9a-f]{64}`
- conversion-audit evidence requires `files_scanned`, `files_succeeded`, and
  `files_failed`
- `files_scanned == files_succeeded + files_failed`
- `rules_total == rules_emitted + rules_missing`
- `:evidence_scope :adapter == :aat_adapter`
- parser evidence logical paths are workspace-relative and must not start with
  `/` or `../`

Duplicate-key checks can remain as small explicit functions because they are
collection-level identity checks rather than one-entry shape checks. The
important simplification is that entry shape and entry-local invariants become
schema data.

### Layer 3 - Export and Test Utilities

Add a small CLI or function set for contract artifacts:

- print a Malli schema description for a registry key
- generate example valid values for selected schemas
- optionally export JSON Schema for ABC-owned policy shapes
- validate committed EDN/JSON files using Malli and return humanized errors

Exported JSON Schema is allowed only where ABC owns the shape and where a
consumer outside Clojure needs the contract. It is not a backdoor to rotate
existing v0 schemas.

## First Migration Slice

Start with the two EDN registries:

1. `abc.tools.aat-parser-ir-compat`
2. `abc.tools.parser-evidence`

The slice should be behavior-preserving:

- keep public functions and return shapes:
  - `registry-errors`
  - `validate-registry!`
  - `index-errors`
  - `validate-index!`
- keep current error tests passing, or update them only to assert equivalent
  human-readable content rather than exact bespoke punctuation
- keep duplicate-key logic explicit
- remove local scalar predicates once shared Malli schemas cover them
- add generator-backed tests for valid entries and targeted invalid entries

This slice gives immediate value because it removes duplicated validation
logic without touching JSON Schema, TEI generation, or cross-language
contracts.

## Later Migration Slices

### Policy Documents

Move these policy shapes to Malli:

- `source-region-publication-policy-v0.json`
- `parser-ir-publication-policy-v0.json`

Keep the files as JSON because they are easy to inspect and can be consumed by
non-Clojure tools. Malli validates their shape inside ABC. If Python/Rust tools
need schema validation for them, export JSON Schema from Malli and add a drift
gate.

### CLI and Materializer Boundaries

Add function schemas around higher-risk Clojure boundaries:

- materialize-publication request maps
- materialize-publications-batch row/result maps
- materialize-source-snapshot options
- validate-design-bundle option/result maps
- TEI validation result maps before they are serialized

The rule is boundary-first: instrument values crossing a tool boundary, not
every private helper.

### Generated Examples

Use Malli generators to create small valid examples for:

- compatibility registry entries
- parser evidence entries
- source-region policy dispositions
- publication policy renderer rows

These should supplement existing hand-built fixtures, not replace measured
corpus evidence.

### Documentation

Use Malli descriptions and schema-to-English output where it helps explain ABC
policy documents. Generated descriptions must remain secondary; the normative
contract is still the schema value plus tests.

## Non-Goals

- Do not make Python or Rust invoke Clojure/Malli at runtime.
- Do not replace TEI RelaxNG/Schematron with Malli.
- Do not replace SHACL with Malli.
- Do not rotate `parser-ir.schema.json`, `manifest.schema.json`, or other v0
  wire schemas merely to make Malli the source.
- Do not convert every helper function to `mx/defn`.
- Do not hide measured policy decisions inside schema predicates.

## Risks And Controls

| Risk | Control |
|---|---|
| Malli becomes a second source of truth for JSON wire schemas | Keep JSON Schema authoritative unless a spec explicitly rotates ownership |
| Generated JSON Schema drifts from Malli source | Add a drift check for every generated schema |
| Function instrumentation makes tests noisy or slow | Instrument only public boundaries and keep local helper schemas out of runtime hot paths |
| Humanized Malli errors break existing exact-string tests | Tests should assert stable content and paths, not punctuation |
| Duplicate-key and cross-document checks become awkward in Malli | Keep them as explicit functions; Malli handles entry-local contracts |
| Policies hide product decisions inside schema validation | Schemas validate shape and allowed values; policy admission remains in policy documents and tests |

## Acceptance Criteria

The Malli contract-layer migration is successful when:

1. `abc.tools.aat-parser-ir-compat` and `abc.tools.parser-evidence` use shared
   Malli schemas for entry shape and scalar contracts.
2. Their public validation APIs remain stable.
3. Existing compatibility and parser-evidence tests still cover the same
   invalid cases.
4. At least one generator-backed test creates valid registry entries that pass
   the validators.
5. No JSON Schema hash rotates.
6. `nix flake check`, `bin/kaocha`, `bin/lint-active`, `just python-quality`,
   and monorepo no-build checks remain green.

## Self-Review

- **Placeholder scan:** No unresolved placeholder tokens.
- **Internal consistency:** Malli owns Clojure/ABC-local data contracts; JSON
  Schema, TEI, SHACL, and Rust remain authoritative at their boundaries.
- **Scope check:** First implementation slice is limited to two EDN validators.
  Policy documents and CLI boundary schemas are explicit later slices.
- **Ambiguity check:** Duplicate-key checks stay as explicit functions; Malli
  handles entry-local shape and scalar/cross-field contracts.
