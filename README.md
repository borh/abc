# Soranoha

Soranoha is the canonical monorepo for the Aozora Bunko conversion and
validation system. It contains:

- `abc/`: Clojure/Nix publication, schema, TEI, manifest, and validation tools.
- `ab-validator/`: Rust adapters, parser/IR validation, corpus measurement, and
  report tooling.
- `soranoha/`: the publication build kernel and snh protocol, including
  [assessment evaluation and experimental RDF](soranoha/docs/assessment-evaluation.md).

The old split repositories are archived. Use this repository for active
development, source identity, validation, and release work.

## Development

Enter the root development shell:

```sh
nix develop
```

Inspect local runtime paths:

```sh
just runtime-config
```

Run the standard cheap validation gate:

```sh
just validate-migration
```

This checks runtime configuration, active path hygiene, schema/policy drift, TEI
version coherence, release-critical flake input pins, Python quality, and Nix
formatting. It also evaluates the root and both component flakes directly.

## Focused Checks

```sh
just python-quality
just nix-format-check
nix build ./abc#checks.x86_64-linux.clj-kondo
nix build ./abc#checks.x86_64-linux.clj-nix-focused-tests
nix build ./ab-validator#checks.x86_64-linux.cargo-check
nix build ./ab-validator#checks.x86_64-linux.cargo-clippy
nix build ./ab-validator#checks.x86_64-linux.cargo-fmt
```

Heavy corpus measurements are operator-driven and require local data under the
configured `AB_DB_ROOT`; they are not part of the ordinary validation gate.

## Source Identity

The root `flake.lock` is the canonical lock. Component locks under `abc/` and
`ab-validator/` are compatibility locks for direct component workflows and must
remain coherent with the root lock for shared non-path inputs.
