# Development

This is the canonical monorepo for the conversion and validation system. Readers
of the corpus want [start here](start-here.md), [a worked example](worked-example.md)
and the [glossary](user-glossary.md) instead; this document is for people who
build or change the system.

## Layout

- `ab-validator/`: the Rust Aozora parser and the AAT contract and parser-IR
  conversion around it. The parser core (`ab-aozora-*` crates and
  `ab-notation-strategies`) is forked from
  [P4suta/aozora](https://github.com/P4suta/aozora)
  (`1a4f864603970983719655aa4af4525958ac2d38`, MIT or Apache-2.0).
- `soranoha/`: publication kernel, TEI/plaintext conversion, record schemas,
  validation, and the snh protocol, including
  [assessment evaluation](../soranoha/docs/assessment-evaluation.md).

## Shell and gates

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
formatting. It runs the publication tests, builds every check of the validator
flake, checks parser binary reproducibility, and evaluates the root and validator
flakes directly.

Focused checks:

```sh
just python-quality
just nix-format-check
just soranoha-tests
just typecheck
nix build ./ab-validator#checks.x86_64-linux.cargo-check
nix build ./ab-validator#checks.x86_64-linux.cargo-clippy
nix build ./ab-validator#checks.x86_64-linux.cargo-fmt
```

`just typecheck` checks the five publication-whitespace functions with Typed Clojure.
Its annotations and locked checker dependencies live in `soranoha/dev/typecheck`,
separate from runtime dependencies and publication toolchain identities.

`nix run .#soranoha-replay` measures cold-cache builds and consecutive source
revisions with the production stages and delta oracle. See
[performance measurements](performance.md) for the command and timing scope.

## Source identity

Nix owns source identity and the toolchain; domain behaviour stays in Clojure,
Rust and Python. Every evidence-bearing input (the TEI P5 source, the TEI-EAJ
reference, the corpus snapshot, dictionary and converter sources) is pinned by
revision or release tag in its flake URL, not only in the lock, so a routine
lock update cannot move it. Infrastructure inputs such as nixpkgs and clj-nix
are lock-only pins.

The root `flake.lock` is the canonical lock. The `ab-validator/flake.lock`
supports working in that component directly and must agree with the root lock
for shared non-path inputs, so a shared input moves in both locks in one change.
`just flake-input-policy` enforces both rules, and `just tei-version-coherence`
holds every TEI reference in the repository at P5 4.11.0.

## Where the rest is written

- [Publication architecture](design/publication-architecture.md): the cache,
  assessment, publication and serving boundaries.
- [snh specification](design/snh-protocol-v1.md): the wire contract.
- [Key ceremony](key-ceremony.md): the two pinned signing roles and the offline
  governance signing path used to withdraw or amend a published work.
- [Zenodo deposits and ORCID anchoring](zenodo-deposits.md): the trust anchor
  that publishes the role assignment and the quarterly snapshot deposit that
  gives a release a DOI.
- [Archival](archival.md): the Software Heritage observation a release has to
  pass before it is citable, and how a citation resolves through the archive.
- [TEI extension vocabulary](../soranoha/docs/tei-vocabulary.md): the
  `https://w3id.org/soranoha/ns/tei` namespace, bound to `snh:`, with its
  attributes and validation rule identifiers.
- [Work identifiers](../soranoha/docs/work-identifiers.md): the
  `000092_000879` form, what it promises across releases, and where exact
  edition identity lives instead.
- [Parser and rendering invariants](parser-invariants.md): the behaviour the
  converter must preserve, for use in code comments and commit messages.
- [Rights](rights.md) and [citation](citation.md): the two rights layers,
  toolchain licensing, and the citation forms; `CITATION.cff` carries the
  machine-readable corpus citation.
