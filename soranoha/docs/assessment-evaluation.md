# Assessment evaluation and internal RDF

Assessment source records describe owner-reviewed Aozora reliance declarations and independent findings with their precise premises. The evaluator regenerates the publication snapshot using the kernel's CAS and constructive traces. The committed source accepts reliance for reviewed corpus editions recorded in `soranoha/data/assessment-source.json`. The publication policy requires applicable assessment evidence; other candidates remain quarantined.

The research outputs are TEI and visible-body plaintext. The rights snapshot describes admission evidence only; it does not certify transcription or markup fidelity. See [source accountability and review exports](source-accountability.md) for the separate build outputs and checks.

## Prepare Aozora reliance evidence

The default admission basis is reliance on Aozora Bunko’s published, work-level copyright-expired classification for the exact edition, scoped to Japan. It is an attributed upstream assertion, not Soranoha’s independent public-domain finding about every contributor. Concrete exceptions or conflicting applicable reviewed findings prevent reliance.

Set these paths to absolute paths, then capture a draft for 蜘蛛の糸:

```sh
nix run .#soranoha-kernel -- aozora-reliance-prepare \
  --aozora-root "$CORPUS_CHECKOUT" \
  --evidence-root "$EVIDENCE_ROOT" \
  --slug 000092_000879 \
  --out "$DRAFT_SOURCE"
```

To preserve an existing source’s other records, also pass `--assessment-source "$EXISTING_SOURCE"`. Preparation refreshes only the selected edition’s declaration and preserves any recorded exception; clearing an exception requires an explicit source edit. Its observation date is the capture’s UTC date; `--as-of` optionally supplies the owner decision date, which cannot precede observation. Preparation writes a draft and retained evidence; it neither commits acceptance nor publishes.

Capture retains the official catalog, card, ZIP and applicable rules as exact response bytes at `$EVIDENCE_ROOT/<sha256>`. Evaluation verifies those bytes and checks the live official catalog’s **作品著作権フラグ**, the card’s edition link, the canonical file bundle and the approved rules digest. Being present in the Git archive is insufficient. Missing, protected, changed or unreachable official evidence makes that declaration unavailable; archived evidence cannot substitute for a current check. ZIP repacking and unrelated catalog or card edits preserve applicability when the relevant assertion, link and canonical bundle remain the same.

Review the generated declaration’s basis and dates, then evaluate it using the command below with `--assessment-source "$DRAFT_SOURCE" --evidence-root "$EVIDENCE_ROOT"`. To record a concrete unresolved exception, set that declaration’s `exception` to a nonempty explanation. Remove or revise declarations through the owner-controlled source workflow.

Evidence supporting publication is retained indefinitely, separately from disposable computation caches. Soranoha accepts an explicit evidence directory and checks digests. The overall NixOS configuration chooses its storage path and owns backup and recovery. Raw retained evidence stays internal; public snapshot digest references do not promise a public evidence download service.

## Evaluate a reviewed source

From the repository root, using a clean Git checkout of the corpus, set `ASSESSMENT_STORE`, `CORPUS_CHECKOUT`, and `SNAPSHOT_OUTPUT` to absolute paths. The Nix wrapper changes directory to its store copy before launching Clojure, so input and output paths must not depend on the caller’s working directory:

```sh
nix run .#soranoha-kernel -- assessment-evaluate \
  --root "$ASSESSMENT_STORE" \
  --aozora-root "$CORPUS_CHECKOUT" \
  --assessment-source "$PWD/soranoha/data/assessment-source.json" \
  --evidence-root "$EVIDENCE_ROOT" \
  --as-of 2026-09-06 \
  --out "$SNAPSHOT_OUTPUT"
```

The Nix entry point supplies the toolchain identity. Direct library or Clojure CLI callers must supply it themselves. `assessment-evaluate` accepts draft inputs and never publishes. Retained-evidence observations also require `--evidence-root`, an explicit directory containing the retained bytes at their declared relative paths; digests are checked before evaluation. Evidence retention is independent of the disposable computation store.

Author source records against [the closed schema](../resources/assessment/source-1.schema.json). `soranoha.assessment.records/encode` returns canonical bytes; decoding refuses unknown fields, duplicate keys and noncanonical encoding. Authored finding IDs must not use the reserved `jp-conservative-term/` rule namespace. [Synthetic evaluator fixtures](../test/soranoha/assessment/evaluate_test.clj) illustrate records and premise fingerprints. [The real-edition dossier](../data/dossiers/README.md) records observations only and makes no legal findings.

## Dependency and authority boundaries

A fact is scoped by subject, predicate and jurisdiction. Person death years can be shared; contribution attribution and wartime-addition findings belong to the specific contribution. Catalog rows supply candidates, never the complete rights-relevant set. A reviewed contribution-set finding may discharge listed roles and add omitted identities. Minted identities use a disjoint namespace and their evidence must be consumed explicitly.

Premises default to `evidence-version`. A `value` or `set-membership` projection requires an explicit rationale and retains only the dependency it declares. Evaluation separates semantic identity from basis identity. A corrected citation updates provenance while unchanged conclusions can reuse downstream semantic computations. Source capture reads current checkout observations; changed or removed inputs make their actual dependent findings unavailable. Independent facts retain their identities and traces.

Effective dates belong to findings. Derived dates are at least the latest consumed premise date; `--as-of` validates applicability without becoming every fact's identity. The implemented sufficient expiry rule covers the adopted Japanese assessment model for dates from 2018-12-29 onward. Failure to establish that rule does not establish copyright protection. Unsupported historical or rights cases remain not-evaluated; explicit reviewed status findings may record in-copyright or undetermined, but public-domain is derived only from the established rule. Conflicting applicable justifications, cycles and dangling controls are structural errors; unavailable support produces not-evaluated snapshot facts. This evaluator neither implements an OWL reasoner nor uses open-world inference to authorize publication.

Review and commit the accepted source and regenerated snapshot before release. The release CLI requires source and snapshot bytes committed in the same Git checkout revision. The operator's owner-controlled Git workflow supplies acceptance authority; the assessor string is attribution, not an authenticated signature. After validating all file inputs, preflight captures local source identities. Before publication, the release evaluates current observations, byte-compares the regenerated snapshot, checks the consumed corpus content, performs current official applicability checks for reliance declarations, and rechecks committed assessment bytes. Any changed evaluation refuses the captured release. Unrelated owner-file changes do not invalidate the assessment. Review and commit regenerated snapshot changes before release.

The scheduled release workflow supplies the runner’s UTC date with `--as-of "$(date -u +%F)"`. The date validates applicability; it does not stamp fact values or cause daily snapshot churn. A finding dated later than that UTC date fails with `future-assessment-finding`; a new JST calendar date can lead UTC by up to nine hours. Reproduce a particular evaluation with its original `--as-of`, not just its input revision. The workflow passes `--evidence-root` when `SORANOHA_EVIDENCE_ROOT` is present in the runner environment. Configure that environment and its mounted directory through the server configuration before using retained evidence or reliance declarations in scheduled releases; otherwise release fails with `missing-evidence-root`.

Assessment-owned source records, dossiers, publication snapshot, and policy live in `soranoha/data/` and are consumed through explicit paths. The same-revision check binds the assessment source and snapshot within the monorepo; the policy is validated separately.

The toolchain identity covers the runtime and dependency environment, not Soranoha source code. Change the assessment `rule-version` whenever its legal rule or computation changes; that version identifies both the rule stage and its recorded basis. Change `reliance-version` when reliance eligibility or payload computation changes. Change `assessment-fact-version` when fact serialization or meaning changes, and the applicable RDF `fragment-versions` entry or `assembly-version` when that stage’s serialization or meaning changes. Keep these versions local to the affected stage so unrelated program changes preserve reusable results.

The build/render graph remains separate from assessment rules. All sources emit `snh-assessment-snapshot/2`, whose candidates distinguish independent assessments from edition-level reliance. The snapshot carries the attributed assertion and retained-evidence digests; it does not invent contributor findings. The manifest and signature formats remain unchanged. Assessment source files and RDF graphs remain internal inputs and projections.

## Experimental RDF dataset

Set `RDF_OUTPUT` to an absolute path and add these flags to the evaluation command:

```sh
--rdf-out "$RDF_OUTPUT" --rdf-base https://example.invalid/soranoha/
```

The result is deterministic N-Quads. Each historical finding has its own named graph and PROV review metadata. Reviewed premises are recorded as activity usage; deterministic conclusion derivations also use `prov:wasDerivedFrom`. Only the graph `<https://example.invalid/soranoha/accepted>` contains currently available conclusions. Do not treat a union of the historical graphs as accepted knowledge. ScopedFact resources preserve jurisdiction and effective date rather than asserting timeless properties of people or works.

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

Reliance resources separately attribute the upstream assertion and the owner’s reliance decision, including whether it currently applies. They do not assert individual public-domain facts. Finding, conclusion and reliance fragments reuse their own trace entries; assembly depends on fragment hashes. Base-IRI or vocabulary-profile changes affect projection, not assessment or rendering. JSON record identities remain authoritative; RDF serialization is an internal view and does not define signed identities. Future RDF/OWL mappings can evolve at this boundary without changing the evaluator's closed acceptance rules.
