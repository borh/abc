# Assessment evaluation and internal RDF

Assessment source records describe reviewed findings and the precise premises they consume. The evaluator regenerates the existing publication snapshot using the kernel's CAS and constructive traces. The committed empty source is a quarantine baseline: it authorizes no works. Publication remains blocked pending the first assessed batch.

## Evaluate a reviewed source

From the repository root, using a clean Git checkout of the corpus:

```sh
nix run .#soranoha-kernel -- assessment-evaluate \
  --root "$ASSESSMENT_STORE" \
  --aozora-root "$CORPUS_CHECKOUT" \
  --assessment-source soranoha/data/assessment-source.json \
  --as-of 2026-09-05 \
  --out "$SNAPSHOT_OUTPUT"
```

The Nix entry point supplies the toolchain identity. Direct library or Clojure CLI callers must supply it themselves. `assessment-evaluate` accepts draft inputs and never publishes. Retained-evidence observations additionally require `--evidence-root`, an explicit directory containing the retained bytes at their declared relative paths; digests are checked before evaluation. Evidence retention is independent of the disposable computation store.

Author source records against [the closed schema](../resources/assessment/source-1.schema.json). `soranoha.assessment.records/encode` returns canonical bytes; decoding refuses unknown fields, duplicate keys and noncanonical encoding. [Synthetic evaluator fixtures](../test/soranoha/assessment/evaluate_test.clj) illustrate records and premise fingerprints. [The real-edition dossier](../data/dossiers/README.md) records observations only and makes no legal findings.

## Dependency and authority boundaries

A fact is scoped by subject, predicate and jurisdiction. Person death years can be shared; contribution attribution and wartime-addition findings belong to the specific contribution. Catalog rows supply candidates, never the complete rights-relevant set. A reviewed contribution-set finding may discharge listed roles and add omitted identities. Minted identities use a disjoint namespace and their evidence must be consumed explicitly.

Premises default to `evidence-version`. A `value` or `set-membership` projection requires an explicit rationale and retains only the dependency it declares. Evaluation separates semantic identity from basis identity: a corrected citation updates provenance while unchanged conclusions can reuse downstream semantic computations. Source capture reads current checkout observations; changed or removed inputs make their actual dependent findings unavailable. Independent facts retain their identities and traces.

Effective dates belong to findings. Derived dates are at least the latest consumed premise date; `--as-of` validates applicability without becoming every fact's identity. The implemented sufficient expiry rule is deliberately limited to the adopted Japanese assessment model and dates from 2018-12-29 onward. Failure to establish that rule does not establish copyright protection. Unsupported historical or rights cases require reviewed findings. Conflicting applicable justifications, cycles and dangling controls are structural errors; unavailable support produces not-evaluated snapshot facts. This evaluator neither implements an OWL reasoner nor uses open-world inference to authorize publication.

The release CLI requires source and snapshot bytes committed in the same Git checkout revision. The operator's owner-controlled Git workflow supplies acceptance authority; the assessor string is attribution, not an authenticated signature. After validating all file inputs, preflight captures observations, evaluates and byte-compares the regenerated snapshot. Before publication it checks the consumed corpus content and rechecks committed assessment bytes. Unrelated owner-file changes do not invalidate the assessment. Review and commit regenerated snapshot changes before release.

The build/render graph and frozen snh protocol remain separate from assessment rules. No assessment source or RDF graph is added to the signed wire format.

## Experimental RDF dataset

Add these flags to the evaluation command:

```sh
--rdf-out "$RDF_OUTPUT" --rdf-base https://example.invalid/soranoha/
```

The result is deterministic N-Quads. Each historical finding has its own named graph and PROV review metadata. Only the graph `<https://example.invalid/soranoha/accepted>` contains currently available conclusions. Do not treat a union of the historical graphs as accepted knowledge. ScopedFact resources preserve jurisdiction and effective date rather than asserting timeless properties of people or works.

For example, query the accepted graph explicitly:

```sparql
PREFIX a: <urn:soranoha:assessment:>
SELECT ?subject ?predicate ?value ?date WHERE {
  GRAPH <https://example.invalid/soranoha/accepted> {
    ?fact a:subject ?subject ; a:predicate ?predicate ;
          a:value ?value ; a:effectiveDate ?date .
  }
}
```

Finding and conclusion fragments reuse their own trace entries; assembly depends on fragment hashes. Base-IRI or vocabulary-profile changes affect projection, not assessment or rendering. JSON record identities remain authoritative; RDF serialization is an internal view and does not define signed identities. Future RDF/OWL mappings can evolve at this boundary without changing the evaluator's closed acceptance rules.
