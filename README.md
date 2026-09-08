# Soranoha

Soranoha is the canonical monorepo for the Aozora Bunko conversion and
validation system. It contains:

- `ab-validator/`: Rust adapters, parser/IR validation, corpus measurement, and
  report tooling. Its parser core, the `ab-aozora-*` crates, is an independent
  fork of [P4suta/aozora](https://github.com/P4suta/aozora) at revision
  `1a4f864603970983719655aa4af4525958ac2d38`, dual-licensed MIT or Apache-2.0,
  and it determines the transcription of every work in the corpus.
- `soranoha/`: publication, TEI/plaintext conversion, schemas, validation, and the snh protocol, including
  [assessment evaluation and experimental RDF](soranoha/docs/assessment-evaluation.md).

## Development

Enter the root development shell:

```sh
nix develop
```

Inspect local runtime paths:

```sh
just runtime-config
```

Run the standard validation gate:

```sh
just validate
```

This checks runtime configuration, active path hygiene, TEI profile generation and
version coherence, release-critical flake input pins, Python quality, and Nix
formatting. It runs publication and parser identity tests, checks parser binary
reproducibility, and evaluates the root and validator flakes directly.

## Focused Checks

```sh
just python-quality
just nix-format-check
just soranoha-tests
just typecheck
nix build ./ab-validator#checks.x86_64-linux.cargo-check
nix build ./ab-validator#checks.x86_64-linux.cargo-clippy
nix build ./ab-validator#checks.x86_64-linux.cargo-fmt
```

Heavy corpus measurements are operator-driven and require local data under the
configured `AB_DB_ROOT`; they are not part of the ordinary validation gate.

## Source Identity

The root `flake.lock` is the canonical lock. The `ab-validator/flake.lock` supports direct research workflows and must
remain coherent with the root lock for shared non-path inputs.

`just typecheck` checks the five publication-whitespace functions with Typed Clojure.
Its annotations and locked checker dependencies live in `soranoha/dev/typecheck`,
separate from runtime dependencies and publication toolchain identities.

`nix run .#soranoha-replay` measures cold-cache builds and consecutive source
revisions with the production stages and delta oracle. See
[performance measurements](docs/performance.md) for the command and timing scope.

See [publication architecture](docs/design/publication-architecture.md) for the
cache, assessment, publication and serving boundaries, and the
[snh specification](docs/design/snh-protocol-v1.md) for the wire contract.

The [key ceremony](docs/key-ceremony.md) establishes the two pinned signing roles
and the offline governance signing path used to withdraw or amend a published work.

Published TEI carries a small extension vocabulary in the namespace
`https://w3id.org/soranoha/ns/tei`, conventionally bound to `snh:`. See the
[TEI extension vocabulary](soranoha/docs/tei-vocabulary.md) for its attributes
and validation rule identifiers.

Each published work is identified by its Aozora work id and contributor card
directory, as in `000092_000879`. See
[work identifiers](soranoha/docs/work-identifiers.md) for the form, what it
promises across releases, and where exact edition identity lives instead.

## Rights and citation

| Scope | Licence |
|---|---|
| Underlying Aozora Bunko works | Public domain — not Soranoha's to license |
| Published corpus artifacts: TEI, plaintext, Markdown, validation reports, catalog, manifests | [CC0-1.0](LICENSE-CC0) |
| TEI customisation and protocol JSON Schemas | [CC0-1.0](LICENSE-CC0) |
| All source code | [Apache-2.0](LICENSE) |

Attribution is requested, not required, mirroring Aozora Bunko's own posture.
[docs/rights.md](docs/rights.md) is the full public statement — the two rights
layers, the toolchain's licence position, where the grant appears in published
bytes, and how a rights holder requests withdrawal. [docs/citation.md](docs/citation.md)
gives the citation forms for a release, a single work, and an exact byte
sequence; `CITATION.cff` carries the corpus record in machine-readable form.
