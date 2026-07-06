# Monorepo Migration Status

Date: 2026-07-06

Soranoha is the integration monorepo for the former sibling repositories:

- `abc/`
- `ab-validator/`

The split repositories still exist during the cutover, but the migration state is
tracked and validated from this repository.

## Imported Revisions

| Component | Split-repo revision imported | Monorepo path |
| --- | --- | --- |
| ABC | `9d45a3f` | `abc/` |
| ab-validator | `fda09f1` | `ab-validator/` |

## Intentional Deltas

The `abc/` component is a tracked-file exact import from the split ABC repo.

The `ab-validator/` component is an exact tracked-file import except for the ABC
schema contract layout:

- removed copied schema files under `ab-validator/data/abc-schemas/schemas/*.schema.json`
- added `ab-validator/data/abc-schemas/schemas` as a symlink to `../../../abc/schemas`
- replaced `ab-validator/data/abc-schemas/data/source-region-publication-policy-v0.json`
  with a symlink to `../../../../abc/data/source-region-publication-policy-v0.json`
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

## Validation Commands

Run from the monorepo root:

```sh
just split-import-parity-audit
just schema-drift
just root-flake-check-no-build
just check-no-build
just validate-migration
nix run .#schema-drift
nix run .#split-import-parity-audit
```

What they prove:

- `split-import-parity-audit`: optional tracked source parity against the split
  repos, retained only as a cutover audit.
- `schema-drift`: `ab-validator` schema contracts still match `abc/schemas` and
  its source-region policy symlink still points at `abc/data`.
- `root-flake-check-no-build`: the root flake evaluates monorepo checks and
  prefixed component checks without building large outputs.
- `check-no-build`: both component flakes evaluate through their no-build
  checks from the monorepo layout. This is retained as a direct component
  fallback while the root flake settles.
- `validate-migration`: runs monorepo schema/policy drift and root no-build
  validation together. Split-repo parity is intentionally not part of this
  default gate after the monorepo becomes the working source tree.

The root flake prefixes component outputs instead of renaming them:

- `abc` flake outputs are exposed as `abc-*`.
- `ab-validator` flake outputs are exposed as `ab-validator-*`.
- Monorepo-local checks/apps use unprefixed names such as `schema-drift`,
  `split-import-parity-audit`, and `validate-migration`.

## Path Policy

Live transition code should resolve cross-component paths through explicit roots:

- `AB_WORKSPACE_ROOT` for the monorepo root
- `AB_ABC_ROOT` when an operator intentionally uses a non-default ABC checkout
- `AB_DB_ROOT` for local large corpus data under `/db`

Historical reports and dated plans may still mention `/home/bor/Projects/...`,
`../abc`, or `../ab-validator`; those are evidence records, not active defaults.

Known active exceptions:

- `ab-validator/reports/aat-fidelity/aat_parser_ir_mapping/generate.py` still
  defaults `--abc-root` to `../abc` for split-repo operation. It is an operator
  script and accepts an explicit `--abc-root`.
- `ab-validator/crates/ab-morph-run` tests keep `../abc/out/corpus` as an
  explicit CLI argument fixture. The command itself requires `--from`.

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
