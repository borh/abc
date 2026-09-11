# Soranoha

Soranoha is a TEI edition of Aozora Bunko. It converts the Japanese texts
Aozora Bunko distributes as annotated plain text into TEI P5, and publishes
each work as TEI, plaintext, Markdown, and a validation report, with a signed
record of the published bytes.

**Reading or using the corpus?** Begin with [start here](docs/start-here.md),
which requires no Nix, Clojure, or TEI background, followed by
[a worked example](docs/worked-example.md) and the
[glossary](docs/user-glossary.md). The rest of this file covers development.

This is the canonical monorepo for the conversion and validation system. It
contains:

- `ab-validator/`: the Rust Aozora parser, validation, corpus measurement, and
  report tooling. The parser core (`ab-aozora-*` crates and
  `ab-notation-strategies`) is forked from
  [P4suta/aozora](https://github.com/P4suta/aozora) (`1a4f864603970983719655aa4af4525958ac2d38`,
  MIT or Apache-2.0).
- `soranoha/`: publication kernel, TEI/plaintext conversion, record schemas,
  validation, and the snh protocol, including
  [assessment evaluation](soranoha/docs/assessment-evaluation.md).

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
[Zenodo deposits and ORCID anchoring](docs/zenodo-deposits.md) covers the trust
anchor that publishes the role assignment and the quarterly snapshot deposit that
gives a release a DOI. [Archival](docs/archival.md) covers the Software Heritage
observation a release has to pass before it is citable, and the recipe for
resolving a citation through the archive.

Published TEI carries a small extension vocabulary in the namespace
`https://w3id.org/soranoha/ns/tei`, conventionally bound to `snh:`. See the
[TEI extension vocabulary](soranoha/docs/tei-vocabulary.md) for its attributes
and validation rule identifiers.

Each published work is identified by its Aozora work id and contributor card
directory, as in `000092_000879`. See
[work identifiers](soranoha/docs/work-identifiers.md) for the form, what it
promises across releases, and where exact edition identity lives instead.

[Parser and rendering invariants](docs/parser-invariants.md) names the
behaviour the converter must preserve, for use in code comments and commit
messages; the [glossary](docs/user-glossary.md) covers the vocabulary of the
published corpus.

## The published site

An exported serving tree holds two kinds of file. Chain content (manifests,
signatures, blobs, governance events) is copied byte for byte, and the
work-facing routes are names over it. The browse layer is generated from that release and provides a landing page
at `/`, author, title, and NDC indexes, individual work pages at
`/works/<identifier>/`, a reading view at `/works/<identifier>/read`, an
explanation for each withdrawn work, and pre-built bulk archives under `/bulk/`
for the whole corpus, an author, or an NDC class. Each artifact has two names
for one underlying blob: `/works/<identifier>/<type>` for citation links, and a
descriptive filename for browser downloads. Each work also serves its citation
record at `/works/<identifier>/citation.json` and `.bib`, and embeds COinS
metadata for reference managers.
Nothing generated is named by a manifest or checked by a verifier; the signed
discovery record is `/catalog.json`. Site text is bilingual, Japanese first.

Serving is a static tree with no application runtime, configured entirely by
[the checked-in Caddyfile](soranoha/config/caddy/Caddyfile). Changing it
requires bumping the revision and hash pin in the estate's `soranoha-serve.nix`,
which is what keeps serving policy immutable system state rather than release
data.

## Rights and citation

| Scope | Licence |
|---|---|
| Underlying Aozora Bunko works | Public domain, or CC BY where the rightsholder licensed it; recorded per work (not Soranoha's to license) |
| Published corpus artifacts (TEI, plaintext, Markdown, validation reports, catalog, manifests) | [CC0-1.0](LICENSE-CC0) |
| TEI customisation and protocol JSON Schemas | [CC0-1.0](LICENSE-CC0) |
| All source code | [Apache-2.0](LICENSE) |

Soranoha's own encoding is under CC0, so attribution for it is requested
rather than required, mirroring Aozora Bunko's own practice. Where the
underlying work is under CC BY, attribution to that work is a condition of that
licence; each work's page, catalog entry and TEI header state which of the two
standings applies to it. [docs/rights.md](docs/rights.md) provides the complete public
statement covering the two rights layers, toolchain licensing, header grants in
published bytes, and the withdrawal process. [docs/citation.md](docs/citation.md)
gives citation forms for releases, individual works, and exact byte sequences,
in Japanese and English, with the role of each component; `CITATION.cff` carries
the machine-readable corpus citation.
