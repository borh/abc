# Monorepo Migration Status

Date: 2026-07-07

Soranoha is the integration monorepo for the former sibling repositories:

- `abc/`
- `ab-validator/`

The split repositories are archived. Soranoha is now the canonical repository;
active development, validation, and source identity are tracked from this
repository.

## Imported Baseline

| Component | Split-repo revision imported | Monorepo path |
| --- | --- | --- |
| ABC | `9d45a3f` | `abc/` |
| ab-validator | `fda09f1` | `ab-validator/` |

These revisions are historical migration provenance only. They are not active
parity targets after the cutoff.

## Intentional Deltas

The monorepo intentionally replaces split-repo compatibility details with shared
monorepo sources:

- removed copied schema files under `ab-validator/data/abc-schemas/schemas/*.schema.json`
- added `ab-validator/data/abc-schemas/schemas` as a symlink to the monorepo
  `abc/schemas`
- replaced `ab-validator/data/abc-schemas/data/source-region-publication-policy-v0.json`
  with a symlink to the monorepo ABC source-region policy
- updated `ab-validator/data/abc-schemas/README.md` to document the monorepo
  source of truth

This makes `abc/schemas` the only schema byte source and `abc/data` the
source-region policy source of truth in the monorepo while preserving
`ab-validator/data/abc-schemas/schema-contracts.json` as the consumer-side
contract snapshot.

Additional delta (2026-07-07): `ab-validator/dictionary` is a real local
directory (`compiled/`, `optimized/`) instead of the split repo's symlink to
`../vibrato-pipe/dictionary`, which dangles in the monorepo layout. Analyzer
dictionaries are nix-only flake outputs; `just dictionary-build-all` populates
`dictionary/compiled/` with symlinks into the nix store, so the monorepo needs
no reference to `vibrato-pipe`.

Additional delta (2026-07-07): TEI P5 references are pinned by the root flake
as `.#tei-p5-reference` at TEI P5 4.11.0 (`P5_Release_4.11.0`), matching ABC's
TEI profile validation basis. The `ab-validator` publication next-work report
uses this flake output by default; `AB_TEI_P5_ROOT` is reserved for an explicit
operator override.

Additional delta (2026-07-07): split-repo parity auditing was retired. It was a
cutover-only guard and is no longer meaningful once Soranoha is canonical.

Additional delta (2026-07-07): ABC Clojure lint now checks all `abc/src` and
`abc/test`; the old active-surface-only lint baseline was removed after legacy
Clojure namespaces were deleted.

## Validation Commands

Run from the monorepo root:

```sh
just active-path-hygiene
just schema-drift
just python-quality
just nix-format-check
just root-flake-check-no-build
just tei-version-coherence
just flake-input-policy
just check-no-build
just validate-migration
nix build .#tei-p5-reference --no-link
nix run .#tei-version-coherence
nix run .#flake-input-policy
nix run .#schema-drift
```

What they prove:

- `active-path-hygiene`: active source/tooling does not depend on sibling
  checkout paths or untracked `references/` parser paths.
- `schema-drift`: `ab-validator` schema contracts still match `abc/schemas` and
  its source-region policy symlink still points at `abc/data`.
- `python-quality`: tracked Python passes ruff format/check and mypy.
- `nix-format-check`: tracked Nix files are formatted with `nixfmt`.
- `root-flake-check-no-build`: the root flake evaluates only root-owned
  integration checks without building large outputs.
- `tei-version-coherence`: the root TEI P5 source reference and ABC TEI profile
  inputs all target TEI P5 4.11.0.
- `flake-input-policy`: release-critical source-evidence inputs carry explicit
  rev/tag pins in flake input URLs, not only in lockfiles, and component
  compatibility locks remain coherent with the root lock for shared inputs.
- `check-no-build`: the root, ABC, and validator flakes evaluate explicitly
  without building large outputs.
- `validate-migration`: runs the cheap monorepo validation gate: runtime config,
  active path hygiene, schema/policy drift, TEI version coherence, flake input
  policy, Python quality, Nix formatting, and explicit no-build evaluation of
  the root, ABC, and validator flakes.
- `nix build .#tei-p5-reference --no-link`: verifies the pinned TEI P5
  reference tree exposes the files cited by the publication mapping dossiers.

The root flake is an explicit operator-facing integration facade. Component
tools remain available from their owning flakes:

- use `./abc#...` for publication, schema, TEI, manifest, and registry tools;
- use `./ab-validator#...` for parser, adapter, evidence, corpus, dictionary,
  and analyzer tools;
- use root outputs only for supported end-to-end workflows, root-owned
  cross-component contracts, cross-component artifacts, and the integrated
  development shell.

Convenience alone is not sufficient for promotion to the root.

## Path Policy

Live transition code should resolve cross-component paths through explicit roots:

- `AB_WORKSPACE_ROOT` for the monorepo root
- `AB_ABC_ROOT` when an operator intentionally uses a non-default ABC checkout
- `AB_TEI_P5_ROOT` when an operator intentionally uses a non-default TEI P5
  reference checkout
- `AB_DB_ROOT` for local large corpus data under `/db`

Historical reports and dated plans may still mention `/home/bor/Projects/...`,
`../abc`, or `../ab-validator`; those are evidence records, not active defaults.

The active-code path hygiene gate enforces this for runnable source and tooling.

## Naming Policy

Soranoha is the physical monorepo and integration project name. It is not yet a
replacement for the accepted ABC contract surface.

Do not rename these surfaces as part of monorepo setup:

- `abc.*` Clojure namespaces
- `abc.tools.*` command namespaces and app semantics
- `https://w3id.org/abc/...` vocabulary and artifact identity URIs
- `abc-...` Schematron rule IDs and validation-result references
- historical manifest, parser-evidence, TEI, RDF, SHACL, and fixture paths

Any future Soranoha rename needs a dedicated ADR that decides whether Soranoha
is a public project name, code namespace, vocabulary base, component taxonomy,
or a phased combination. Until then, root flake output prefixes are operational
labels, not a public vocabulary migration.

## Heavy Data Policy

Large corpus material stays outside Git. The monorepo uses cheap, local
validation gates for ordinary development and operator-driven full-corpus runs
for measurement:

- cheap gates: schema drift, smoke tests, no-build flake checks
- heavy gates: AAT full-corpus conversion, parser performance, source inventory,
  TEI/publication coverage, and morph warehouse materialization

GitHub-style hosted CI is not assumed to be capable of the heavy gates. Those
belong on the local/custom Forgejo runner path with access to `/db` and parser
reference data.
