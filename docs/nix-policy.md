# Nix Policy

Soranoha treats Nix as the repository's identity, toolchain, and bounded
materialization layer. Domain behavior stays in Clojure, Rust, and Python code.

## Roles

Nix owns:

- source identity for TEI P5, TEI-EAJ, parser candidates, corpus/reference
  snapshots, schemas, and dictionary/converter sources
- toolchain exposure through `apps`, `checks`, `packages`, and `devShells`
- bounded materialization recipes that can be named, cached, and reproduced

Nix does not own:

- corpus-delta logic
- parser comparison semantics
- Parser-IR, TEI, RDF, or sidecar policy
- report interpretation
- full-corpus Cartesian derivation matrices

## Lock Authority

The root `flake.lock` is the canonical lock for Soranoha. The old split
repositories are archived and no longer define active source identity.

The `ab-validator/flake.lock` supports direct research flake workflows. When a
component flake input changes, update the root lock and the component
lock in the same change so root-prefixed outputs and direct component workflows
see the same source identities.

The `flake-input-policy` gate enforces this requirement for shared non-path inputs.
If a component lock and the root lock share an input name, their `original`
and `locked` records must match.

## Release-Critical Inputs

Release-critical inputs must be explicitly pinned in the flake URL by `rev` or
release tag, not only by the lockfile:

- TEI P5 source
- TEI-EAJ `aozora_tei`
- Aozora Bunko corpus/reference source snapshots
- parser reference repositories
- dictionary/converter sources

The lockfile still records the NAR hash and exact revision. The explicit URL
pin prevents routine lock updates from changing evidence-bearing inputs by
accident.

Infrastructure inputs such as `nixpkgs`, `clj-nix`, `rust-overlay`,
`flake-utils`, `crane`, and `flake-parts` may be lock-only pins, with update
review handled by normal Nix lock diffs.

## TEI P5 Identity

Soranoha profile validation is pinned to TEI P5 4.11.0. The monorepo TEI source
reference must therefore also use TEI P5 4.11.0:

- root input: `github:TEIC/TEI/P5_Release_4.11.0`
- Soranoha full TEI Relax NG: TEI Vault P5 4.11.0 `tei_all.rng`
- Soranoha ODD expansion source: TEI Vault P5 4.11.0 `p5subset.xml`

The `monorepo-tei-version-coherence` check enforces this alignment.

## Validation

Run these cheap gates from the monorepo root after Nix input or source-policy
changes:

```sh
just tei-version-coherence
just flake-input-policy
just root-flake-check-no-build
just validate
```

Heavy corpus measurements remain operator-driven and are not part of the
ordinary Nix flake check.
