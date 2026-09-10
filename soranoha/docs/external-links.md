# Attributed external links

External-link files record claims about explicitly named local entities. Entity
kinds are `document` (a document resource), `work` (the intellectual work),
`edition` (a particular source edition), and `person`. A kind and caller-owned
identifier together identify a local entity; changing a link or its attribution
does not change that identity. Declarations do not establish that an external
catalog has the same conceptual boundaries or that the local entity exists.

An input file has this shape; the example uses fictional identifiers:

```json
{
  "schema": "soranoha-external-links/1",
  "assertions": [{
    "target": {"kind": "person", "id": "example-person"},
    "relation": "http://www.w3.org/2004/02/skos/core#closeMatch",
    "external": {"kind": "person", "iri": "https://example.org/people/1"},
    "attribution": {
      "agent": "https://example.org/reviewers/1",
      "method": "Manual comparison of the cited source",
      "evidence": ["https://example.org/source/1"]
    }
  }]
}
```

Relations and external identifiers must be absolute IRIs. Attribution requires an
agent IRI, a nonempty method description, and evidence IRIs. The caller supplies
these values; the exporter performs no network lookup or identity reconciliation.
Document, work, edition, and person identifiers are never substituted for one
another. The relation remains explicit, including any stronger identity claim.

The internal N-Quads export describes each claim using RDF reification and PROV
attribution in its own content-addressed named graph, so each statement is
described rather than asserted; see
[RDF reification](https://www.w3.org/TR/rdf-schema/#ch_reification). Acceptance
and identity stay the consumer's explicit step, and `owl:sameAs` is never
inferred.

Assertion identity hashes canonical JSON of its typed targets, relation and
attribution. Evidence ordering and duplicates do not change identity. A local
entity resolves to `<base>entities/<kind>/<percent-escaped id>` and its named
graph to `<base>assertions/<assertion id>`, so the base has to end in a slash
and carry no query or fragment, and a local identifier cannot be `.` or `..`.
The base is a deployment choice; assertion content IDs do not depend on it.
Exports sort and deduplicate N-Quads lines, so input-file order cannot change bytes.
The export command refuses to overwrite an existing file.

From the monorepo root, write an experimental dataset:

```sh
nix run .#soranoha-kernel -- links-export \
  --links links.json --rdf-base https://example.org/soranoha/ --out links.nq
```

Repeat `--links` to combine files. The command writes a JSON summary to stdout
with the assertion content IDs and output path.
