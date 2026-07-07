# TEI P5 Mapping Dossiers

These dossiers track Aozora source feature families through Parser-IR, TEI P5,
ABC extension or sidecar policy, plaintext projection, current evidence, and
open decisions.

TEI P5 references are supplied by the monorepo root flake:

```sh
TEI_P5_ROOT="$(nix build --no-link --print-out-paths .#tei-p5-reference)"
```

Use `$TEI_P5_ROOT` as the citation base for TEI P5 claims. The reference is
pinned to TEI P5 4.11.0 to match ABC's TEI profile validation basis. Relevant
directories verified by the root flake include:

- `$TEI_P5_ROOT/Source/Guidelines`
- `$TEI_P5_ROOT/Source/Specs`
- `$TEI_P5_ROOT/Test`

Every dossier uses these sections:

- `Source Inventory`
- `Parser-IR Representation`
- `TEI P5 Target`
- `ABC Extension Or Sidecar`
- `Plaintext Projection`
- `Current Evidence`
- `Open Decisions`

Statuses:

- `admitted`
- `policy-needed`
- `adapter-fidelity-needed`
- `schema-needed`
- `diagnostic-only`
