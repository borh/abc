# Rights and licensing

This is the full statement referenced by every signed release manifest as
`rights.statement_url` and by every published TEI file as
`publicationStmt/availability`. It is served at
<https://soranoha.org/rights>.

## The short version

You may copy, redistribute, adapt, translate, mine, and republish everything
Soranoha publishes, for any purpose, commercial or not, without asking and
without payment. Attribution is requested, not required.

If you use the corpus in research, please cite it — see
[citation](citation.md). That is a scholarly norm here, not a licence
condition.

## Two layers, stated separately

A published work has two rights layers, and they are not the same thing.

**The underlying works** are texts from [Aozora
Bunko](https://www.aozora.gr.jp/) whose copyright has expired. Soranoha
neither holds nor claims any right in them. Aozora's own handling rules
([取り扱い規準](https://www.aozora.gr.jp/guide/kijyunn.html)) state that files
for copyright-expired works may be freely copied, redistributed, shared —
有償・無償 alike — and modified, and that attribution is 希望, requested rather
than required. Soranoha publishes only works its assessment finds free to
publish; the reasoning is recorded in
[`soranoha/docs/evidence/aozora-rights-source-contract.md`](../soranoha/docs/evidence/aozora-rights-source-contract.md).

**Soranoha's encoding** — the TEI markup, the plaintext and Markdown
projections, the validation reports, the catalog, and the release manifests —
is dedicated to the public domain under [CC0
1.0 Universal](https://creativecommons.org/publicdomain/zero/1.0/). Where that
encoding attracts copyright or a database right at all, those rights are
waived.

CC0 rather than CC BY was a deliberate choice. The TEI is largely a mechanical
conversion of Aozora's own markup, so an attribution requirement would rest on
a thin claim while stacking obligations on anyone combining this corpus with
others. Credit is secured through `CITATION.cff` and a stated citation form
instead of through a licence condition. The accepted consequence is that
Soranoha has no legal recourse if a reuser strips credit.

## What covers what

| Scope | Licence | Where |
|---|---|---|
| Underlying works | Public domain (copyright expired) | Not Soranoha's to license |
| TEI, plaintext, Markdown, validation reports, catalog, manifests | [CC0-1.0](../LICENSE-CC0) | Published bytes |
| TEI customisation: `soranoha/schemas/tei-profile.{odd,rng,sch}` | [CC0-1.0](../LICENSE-CC0) | Repository |
| Protocol JSON Schemas: `soranoha/resources/snh/schemas/` | [CC0-1.0](../LICENSE-CC0) | Repository |
| All source code | [Apache-2.0](../LICENSE) | Repository |

The encoding vocabulary and the protocol schemas are CC0 so that an
independent implementation can adopt them without inheriting an attribution
obligation. The code is Apache-2.0 for its explicit patent grant: the snh
protocol is specified for independent reimplementation, and a patent grant is
what makes that invitation credible.

## The toolchain

The Rust parser core under `ab-validator/crates/ab-aozora-*` is an independent
fork of [P4suta/aozora](https://github.com/P4suta/aozora) at revision
`1a4f864603970983719655aa4af4525958ac2d38`, dual-licensed **MIT OR
Apache-2.0**. Its licence files, `NOTICE`, and per-file upstream headers are
retained in the fork. This matters to two kinds of reader: anyone assessing
licence obligations before redistributing the tooling, and anyone assessing
how the transcriptions were produced, for whom the parser's lineage is a
methodological fact.

Redistributing the *published corpus* does not carry the toolchain's
obligations. Redistributing the *tooling* does.

The Aozora catalog CSV that Soranoha reads bibliographic metadata from is
published by Aozora Bunko under CC BY 4.0. That licence covers catalog reuse;
it grants nothing over the works themselves.

## Machine-readable form

Each published artifact carries its own terms, so a file stays interpretable
once detached from this site.

In every signed release manifest:

```json
"rights": {
  "works": "public-domain",
  "encoding": "CC0-1.0",
  "statement_url": "https://soranoha.org/rights"
}
```

In every published TEI file, one `licence` element per rights layer — the
underlying work's standing carried by the Creative Commons Public Domain Mark,
Soranoha's own encoding by CC0:

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
      Full rights statement: <ptr target="https://soranoha.org/rights"/></licence>
  </availability>
</publicationStmt>
```

Both come from one document, `soranoha/data/publication-policy.edn`, whose
bytes each manifest already hashes as `admission.policy_hash`. The terms
signed off, the terms in the release record, and the terms inside a detached
TEI file therefore cannot disagree. Where a copy nonetheless differs, the
manifest field is authoritative — it is the signed one.

The TEI profile enforces the grant's presence: `snh-publication-licence` in
`soranoha/schemas/tei-profile.sch` rejects any Soranoha TEI file whose header
lacks `publicationStmt/availability/licence` with a non-empty `@target`. A
published work without stated terms is a validation failure, not an omission.

## If you hold rights in a published work

Soranoha publishes only works its assessment finds to be free of subsisting
rights, but a corpus of this size will eventually be wrong about one.

Write to the address on <https://orcid.org/0000-0003-2246-8774>, naming the
work identifier (the `000092_000879` form, visible in the URL) or the Aozora
card, and the basis of the claim. A claim does not need to be a formal legal
notice to be acted on.

What happens next is part of the published record rather than a quiet edit.
Withdrawal is a signed governance event carrying a `reason_code` — `rights`
for a subsisting-rights claim, `takedown-request` for a request made on other
grounds — and a public `statement`. The next release then:

- removes the work from `works[]`, so its artifacts are no longer served at
  `/works/<identifier>/`;
- publishes a new catalog that no longer describes it, since a manifest may
  not name a catalog describing a work that release withdrew;
- adds it to `withdrawn[]` pointing at the governing event, which is served at
  `/withdrawn/<identifier>.json`.

Two consequences are worth stating plainly. First, a withdrawal is
append-only: the earlier releases that did contain the work remain in the
chain, because a published, signed, archived record cannot be rewritten
without destroying the guarantee that makes it citable. Withdrawal stops
current distribution; it does not unpublish history. Second, a withdrawal
statement can be corrected by a later `event-amendment`, but an amendment
never reverses the withdrawal itself.

The mechanism is specified in
[`docs/design/snh-protocol-v1.md`](design/snh-protocol-v1.md) and its schema in
`soranoha/resources/snh/schemas/snh-governance-event-1.schema.json`.

## No warranty

Soranoha publishes transcriptions with their validation reports and their
divergences from the source recorded, not a guarantee of fidelity. The corpus
is provided as-is. Do not rely on it for a legal, medical, or safety purpose
without independent verification against the source edition each work names in
its `sourceDesc`.
