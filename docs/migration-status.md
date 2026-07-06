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
- updated `ab-validator/data/abc-schemas/README.md` to document the monorepo
  source of truth

This makes `abc/schemas` the only schema byte source in the monorepo while
preserving `ab-validator/data/abc-schemas/schema-contracts.json` as the
consumer-side contract snapshot.

## Validation Commands

Run from the monorepo root:

```sh
just parity-audit
just schema-drift
just check-no-build
just validate-migration
```

What they prove:

- `parity-audit`: tracked source parity against the split repos, allowing only
  the documented schema symlink delta.
- `schema-drift`: `ab-validator` schema contracts still match `abc/schemas`.
- `check-no-build`: both component flakes evaluate through their no-build
  checks from the monorepo layout.
- `validate-migration`: runs the parity audit and no-build validation together.

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
