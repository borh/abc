# Rights and licensing

Every signed release manifest names this statement in `rights.statement_url`,
and every published TEI file names it in `publicationStmt/availability`, at
<https://w3id.org/soranoha/rights>. Signed bytes cannot be corrected later, so
they name that identifier rather than whichever host serves the corpus.
Registering the `w3id.org/soranoha/` redirects is a precondition of public
genesis; the identifiers are written here in the form the signed bytes name,
whether or not they resolve at the moment you try them.

## The short version

You may copy, redistribute, adapt, translate, mine, and republish everything
Soranoha publishes, for any purpose, commercial or not, without asking and
without payment. For almost every work, attribution is requested, not required.
For the works whose rightsholder publishes them under a Creative Commons
Attribution licence, attribution is a condition of that licence. Each work
states which of the two it is, on its page and in its TEI header.

If you use the corpus in research, please cite it (see
[citation](citation.md)). That is a scholarly norm here, not a licence
condition.

## Two distinct rights layers

A published work involves two distinct rights layers.

**The underlying works** are texts from [Aozora
Bunko](https://www.aozora.gr.jp/), under one of two standings. Almost all of
them are works whose copyright term has expired and whose rights have lapsed.
Soranoha neither holds nor claims any right in those. Aozora Bunko's [handling rules](https://www.aozora.gr.jp/guide/kijyunn.html)
permit free copying, redistribution, performance, and adaptation of
copyright-expired files, with nothing owed in permission or fee. Altering the
text itself is permitted within the scope of Article 20(2)(iv) of the Copyright
Act, which is what covers format conversion, notation changes, and modernising
旧字旧仮名.
The rules request preserving contributor credits and recording textual
modifications; Soranoha preserves contributor credits in each TEI `<back>`
and publishes every work alongside its validation report and recorded divergences.

The rest are works whose copyright subsists and whose rightsholder publishes
them on Aozora Bunko under a Creative Commons Attribution licence. Aozora
Bunko's catalog records that copyright subsists but not under what terms, so
the licence is read from the notice the rightsholder wrote into the colophon.
Soranoha publishes such a work only where attribution is the sole condition:
a licence adding NonCommercial, NoDerivatives, or ShareAlike is refused at
selection, and the work does not enter the corpus. The standing each work was
admitted under travels with it, in `works[].rights` in the manifest, in the
catalog, and in the work's own TEI header.

Soranoha publishes only works its assessment finds free to publish on these
terms; the reasoning is recorded in
[`soranoha/docs/evidence/aozora-rights-source-contract.md`](../soranoha/docs/evidence/aozora-rights-source-contract.md).

**Soranoha's encoding** (TEI markup, plaintext and Markdown
projections, validation reports, catalog, and release manifests)
is dedicated to the public domain under [CC0
1.0 Universal](https://creativecommons.org/publicdomain/zero/1.0/). Where that
encoding attracts copyright or a database right, those rights are waived.

CC0 was chosen because TEI transcription is largely a mechanical conversion
of Aozora Bunko markup. Dedicated public domain avoids imposing attribution
obligations downstream. Scholarly attribution is requested through `CITATION.cff`
and standard citation practice (see [citation](citation.md)).

## What covers what

| Scope | Licence | Where |
|---|---|---|
| Underlying works | Public domain (copyright expired), or CC BY where the rightsholder licensed it | Not Soranoha's to license; recorded per work as `works[].rights` |
| TEI, plaintext, Markdown, validation reports, catalog, manifests | [CC0-1.0](../LICENSE-CC0) | Published bytes |
| TEI customisation: `soranoha/schemas/tei-profile.{odd,rng,sch}` | [CC0-1.0](../LICENSE-CC0) | Repository |
| Protocol JSON Schemas: `soranoha/resources/snh/schemas/` | [CC0-1.0](../LICENSE-CC0) | Repository |
| Locally authored source code | [Apache-2.0](../LICENSE) | Repository |
| Forked parser core, eight crates | MIT OR Apache-2.0 | `ab-validator/crates/`, listed below |
| Data tables | Provenance, and terms where the source states them, in each file's header | `ab-validator/crates/*/data/` |

The encoding vocabulary and protocol schemas are dedicated under CC0. Locally authored source
code is licensed under Apache-2.0 to provide an explicit patent grant. The forked
parser crates retain upstream's dual MIT/Apache-2.0 licence.

Data tables in `ab-validator/crates/*/data/` document their provenance and terms
in each file header. The gaiji character dictionary (`外字注記辞書`) mapping is CC0.
The JIS X 0213 code tables follow Project X0213's permissive terms. Gaiji alias mappings
are locally authored.

## The toolchain

The parser core under `ab-validator/crates/` is forked from
[P4suta/aozora](https://github.com/P4suta/aozora) (revision
`1a4f864603970983719655aa4af4525958ac2d38`), dual-licensed **MIT OR Apache-2.0**.
Upstream licence files, notices, and source headers are preserved.

Eight crates are forked: `ab-aozora-corpus`, `ab-aozora-encoding`,
`ab-aozora-facade`, `ab-aozora-pipeline`, `ab-aozora-render`, `ab-aozora-spec`,
`ab-aozora-syntax`, and `ab-notation-strategies` (forked from `aozora-proptest`).
The CLI binary `ab-aozora` is locally authored.

Redistributing published corpus artifacts carries only the CC0 encoding
dedication and public domain terms. Redistributing the toolchain carries the
toolchain's licenses.

The Aozora Bunko catalog CSV used for bibliographic metadata is published by
Aozora Bunko under CC BY 4.0.

## Machine-readable form

Each published artifact carries its own terms, so a file stays interpretable
once detached from this site.

In every signed release manifest, the grant Soranoha makes over its own layer,
which is the same for the whole release:

```json
"rights": {
  "encoding": "CC0-1.0",
  "statement_url": "https://w3id.org/soranoha/rights"
}
```

and, beside each work, the standing that work was admitted under:

```json
"works": [
  { "slug": "000092_000879", "rights": "public-domain" },
  { "slug": "054333_001657", "rights": "CC-BY-2.1-JP" }
]
```

In every published TEI file, one `licence` element covers each rights layer.
Below, the underlying work is out of copyright, so its status is represented by
the Creative Commons Public Domain Mark; for a work under CC BY the first
`licence` names that licence instead and states attribution as its condition.
Soranoha's encoding is dedicated under CC0 either way:

```xml
<publicationStmt>
  <!-- publisher, idno and date omitted -->
  <availability status="free">
    <licence target="https://creativecommons.org/publicdomain/mark/1.0/">The
      underlying work is in the public domain; Soranoha asserts no rights over
      it.</licence>
    <licence target="https://creativecommons.org/publicdomain/zero/1.0/">Soranoha's
      encoding of this work, and the artifacts derived from it, are dedicated to
      the public domain under CC0-1.0. Attribution is requested, not required.
      Full rights statement: <ptr target="https://w3id.org/soranoha/rights"/></licence>
  </availability>
</publicationStmt>
```

Both definitions originate in `soranoha/data/publication-policy.edn`, hashed into each
manifest as `admission.policy_hash`. The signed manifest is authoritative. The
TEI Schematron rule `snh-publication-licence` in `soranoha/schemas/tei-profile.sch`
rejects files missing `publicationStmt/availability/licence` with a `@target`.

## If you hold rights in a published work

To report a copyright concern or request removal of a published work,
contact the maintainer via <https://orcid.org/0000-0003-2246-8774> with the
work identifier (e.g. `000092_000879`) or Aozora Bunko card number and the
basis of the claim. A claim does not need to be a formal legal notice to be
acted on.

Withdrawal is recorded through signed governance events. Each event carries a
public `statement` and one of four `reason_code` values: `rights` for a
subsisting-rights claim, `takedown-request` for a removal asked for on other
grounds, and `data-defect` or `other` for withdrawals that are not claims.
Appending the signed event is itself a release, so it does not wait for the
next corpus release.

- The work is removed from `works[]`, so its artifacts are no longer served at
  `/works/<identifier>/`, and the release publishes a new catalog that no
  longer describes it: a manifest may not name a catalog describing a work that
  release withdrew.
- The work is recorded in `withdrawn[]`, referencing the signed governance event served at `/withdrawn/<identifier>.json`.
- **Append-only history**: Earlier releases containing the work remain in the signed chain; withdrawal ceases current distribution without rewriting historical releases.
- **Irreversible withdrawal**: A withdrawal statement may be amended via `event-amendment`, but amendments cannot reverse the withdrawal itself.

See [`docs/design/snh-protocol-v1.md`](design/snh-protocol-v1.md) and
`soranoha/resources/snh/schemas/snh-governance-event-1.schema.json`.

## No warranty

Soranoha publishes transcriptions alongside validation reports and recorded
divergences, not a guarantee of fidelity. The corpus is provided as-is. Do not
rely on it for a legal, medical, or safety purpose without independent
verification against the source edition each work names in its `sourceDesc`.
