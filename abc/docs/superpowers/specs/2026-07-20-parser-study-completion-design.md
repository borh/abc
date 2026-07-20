# Parser Study Completion and Third-Party Retirement

**Date:** 2026-07-20

**Status:** Proposed

## Purpose

Close the preregistered neutral comparison as a durable research publication,
then remove third-party parsers from Soranoha's active parser surface. Closure
means every planned observation is measured or carries a specific, quantified
terminal limitation; it does not mean manufacturing a measurement whose
independent authority does not exist. The study must remain reproducible as a
derivation from frozen evidence without requiring third-party parsers to remain
production dependencies.

This is the remaining Track S work from the parser release-qualification
campaign. Track R is complete: the custom parser is admitted, release-qualified,
and governed by Accepted ADR 0039. Track S cannot reopen, qualify, admit, rank, or
replace that decision.

## Problem

The current neutral report honestly publishes one measured corpus-robustness
arm and explicit missing rows. Four preregistered, higher-information axes still
lack complete evidence:

- fidelity;
- diagnostic code, severity, and span scoring;
- source-span accuracy; and
- censored performance statistics.

The comparison machinery also remains an active dependency surface. That is
appropriate while the study still needs executions, but not after publication.
Keeping third-party packages, adapters, and runners alive indefinitely would
turn a bounded research exercise into a permanent alternate-parser subsystem.

The desired end state is therefore two values, separated in time:

1. a closed research publication whose claims can be regenerated and audited;
2. a smaller active system in which the custom parser is the only parser.

## Evidence and Constraints

The following decisions and observations are already settled and are not
reopened here:

- `aozora-parser-comparison-preregistration.{md,json,sha256}` freezes the study
  questions, candidates, revisions, corpora, missing-data rules, and performance
  procedure.
- ADR 0038 assigns development ownership to the custom parser while structurally
  separating comparison evidence from admission and release authority.
- ADR 0039 records successful release qualification of the custom parser.
- The current report generator emits explicit missing rows rather than zeros.
- The current robustness rows cover corpus parse completion only; the frozen
  malformed-input fixture arm is still unmeasured and requires third-party
  execution before retirement.
- The diagnostics fixture and scorer exist. The fidelity/oracle and span tooling
  have useful components but do not yet emit study-consumable axis evidence.
- The existing XHTML comparison is not a full-corpus fidelity oracle: it contains
  17,601 observations for the 17,886-work corpus, and only 15,231 observations
  are currently eligible as rendered-body proxies.
- Third-party native and adapter-normalized lanes remain separate. Adapter-added
  behavior is never credited as native behavior.
- Corpus-scale artifacts stay out of source control. Paths, hostnames, mount
  details, and copy topology are runtime concerns, not research identity.

## Decision

Close Track S as one bounded research publication with four independently
testable axis resolvers and one existing report generator. A resolver may emit
measurements, defensible non-comparability, or quantified unavailability. Its
job is to settle the evidence state, not force a number.

Each resolver emits a small, immutable axis-evidence value. A closed evidence
index names exactly the values used by the publication. The report generator
validates that index and derives the machine result and narrative report. A
single verification command re-hashes the referenced evidence and regenerates
the publication byte-identically from any caller-supplied research-bundle root.

After the publication closure verifies, a separate retirement change removes
third-party parser packages, adapters, runners, and active checks. It preserves:

- the preregistration;
- the compact evidence index and axis summaries;
- the generated report;
- the Git revision containing the final research runner; and
- a copyable, content-verified research bundle containing the raw run artifacts
  and source snapshots needed to audit or re-derive the published results.

No active application or release path may depend on that bundle after
retirement. It is a research record, not a dormant parser backend.

This refines the earlier campaign acceptance wording. "Complete" means no
unexamined missingness and no omitted execution that retirement would make
impossible. It does not convert a missing independent oracle into
`non_comparable`, or require a second full parser solely to make an axis green.

```text
frozen preregistration + markup + captured parser outputs
                           │
                    S2 / S1 / S3 / S4
                           │
                 immutable axis evidence
                           │
                 closed publication index
                           │
              machine result + narrative report
                           │
              verify from an arbitrary copied root
                           │
                  retire active alternatives
```

## Why One Closure, Not One New Framework

The four axes differ scientifically, but share only three useful mechanics:

1. closed lane membership from the frozen preregistration;
2. content identity and streaming verification; and
3. projection into the existing report.

Those mechanics belong to the existing study evidence and report tooling. The
axis-specific meaning stays in four small resolvers. There is no generic
measurement framework, plugin protocol, site policy, storage topology, or new
orchestrator.

The implementation order is S2, S1, S3, S4:

- S2 begins with the all-axis closure census, then establishes the axis-evidence
  contract with the lowest-uncertainty missing axis.
- S1 settles oracle adequacy without displacing the source markup's authority.
- S3 may legitimately terminate individual lanes as `non_comparable`.
- S4 performs the only controlled repeated execution and is therefore last.

## Alternatives

### Keep the current report and retire now

This is fastest, and the current missing rows are honest. It does not satisfy the
campaign's Track S acceptance criterion or the goal of a comprehensive research
publication. Rejected.

### Build four independent study campaigns

This gives every axis complete autonomy, but duplicates candidate membership,
identity, evidence verification, and report integration four times. It also
permits axes to drift onto different candidate sets. Rejected.

### Preserve third-party parsers as permanent optional backends

This makes future reruns easy but imposes indefinite dependency, security,
packaging, and maintenance cost for code that has no production authority.
Rejected. The final study revision and research bundle preserve the evidence
without preserving an active alternate-parser architecture.

## Research Identity and Runtime Place

Study identity is the immutable value formed from:

- the preregistration content hash;
- the frozen candidate and adapter revisions;
- the corpus, fixture, and performance-workset hashes;
- the axis policy/version; and
- the logical hashes of the axis evidence consumed by the report.

It does not include a hostname, filesystem path, mount source, device, backup
provider, or copy count. A `research_bundle_root` is a runtime argument. Moving
or copying the same closed bytes does not create a new study.

The performance host capture remains an observation required by the frozen
preregistration. It describes whether two runs are comparable; it is neither a
machine identity nor a publication-storage policy. Exact observations are
published as attempt context and evaluated only by the frozen comparability
rule.

## Axis Evidence Contract

Add one strict, versioned axis-evidence schema with a discriminated union by
axis. Every record carries:

- study identity and axis;
- candidate, measurement mode, parser revision, and adapter revision;
- corpus or fixture identity;
- instrument/policy identity;
- closed raw-input references;
- metric observations; and
- one terminal disposition per required metric.

Metric dispositions are:

- `measured` — derived from authenticated raw evidence;
- `non_comparable` — the lane lacks a semantic analogue required by the metric;
- `unavailable` — evidence or a required run is absent or invalid; and
- `failed` — execution produced an authenticated failure outcome.

An axis is resolved for publication only when every preregistered metric has an
authenticated disposition. `unavailable` is permitted only with a concrete
missing authority/input and a retained denominator; it is never converted to
`non_comparable` merely to finish. An axis may be summarized as measured when at
least one applicable metric is measured and all other required metrics are
defensibly non-comparable. The report must disclose that metric-level
disposition matrix and may not hide an unavailable metric behind a measured
summary.

The current result schema's single `numerator` and `denominator` cannot express
these multi-metric axes without choosing a misleading scalar. Add a version-2
result schema under a new identity and retain version 1 for the historical
report. Each v2 row carries a closed `metrics` array of named typed observations
and an axis summary derived from those observations. Do not add an overall
score.

## Publication Completeness Census

Before adding an axis measurement, derive a closed matrix of every frozen
candidate/mode and all nine preregistered axes. Each metric is classified as:

- already backed by authenticated evidence;
- derivable from existing authenticated values;
- requiring one final third-party execution;
- structurally non-comparable; or
- unavailable because an independent authority/input does not exist.

This census is a projection of the preregistration and evidence index, not a new
workflow engine. It prevents the four Track S headings from hiding unfinished
work elsewhere in the study. In particular, the malformed-input robustness
fixture must run before retirement. Construct coverage, maintenance, packaging,
and license rows must consume their existing evidence or retain an exact named
limitation; no row becomes measured merely because the new axes are resolved.

Every item classified as requiring third-party execution joins the bounded
capture set. Retirement is forbidden while that set is non-empty.

## S2 — Diagnostic Scoring

Run every executable native and adapter-normalized lane against the frozen four
case diagnostic fixture. Capture raw diagnostic output before normalization.
Project only the preregistered facts: presence, stable code, severity, and UTF-8
span.

The existing conformance scorer remains the scoring authority. A thin
lane-specific projection may expose a native field, but it must not invent a
code, severity, or span that the parser did not report. A structurally absent
field is `non_comparable` for that metric; a missing capture is `unavailable`.

The producer emits false-positive and false-negative counts with the exact
four-case denominator and per-case witnesses. This first producer pins the
shared axis-evidence/index contract.

## S1 — Fidelity Authority and Scoring

Aozora markup is the source authority. It owns corpus membership, syntax,
construct occurrences, and source coordinates. XHTML is downstream rendered
output and may never override or redefine those facts.

The custom parser cannot be its own reference. S1 therefore begins with an
adequacy audit over the exact 17,886-work membership. For each fidelity metric,
the audit records which independent evidence exists:

- independently derived markup inventories for ruby, gaiji, notes, and
  structural constructs, never the custom parser's claim ledger;
- reviewed `ab-oracle` cases for the narrow constructs they actually cover; and
- official Aozora XHTML only as a visible-rendering cross-check for works whose
  captured XHTML satisfies the existing eligibility policy.

The current XHTML run proves why this boundary matters: its observation count is
285 short of the corpus count, and a further 2,370 observed works are not
eligible rendered-body proxies. The adequacy audit must reconcile membership,
not infer missing work IDs from counts alone. XHTML cannot define the full-corpus
denominator or serve as a blanket structure oracle.

If independent evidence closes a metric's full denominator, the scorer emits
exact byte/occurrence agreement with mismatch witnesses. Otherwise that metric
remains `unavailable` with exact covered and missing counts. The denominator
never shrinks to the easy subset.

Do not build another full Aozora parser merely to manufacture a reference. A
markup-to-visible-text interpretation is authoritative only if its rules and
implementation are independent of the custom parser and independently reviewed.
The curated oracle cases may be published as a bounded supporting appendix, but
they do not satisfy the preregistered full-corpus fidelity denominator.

## S3 — Span Accuracy

The raw Aozora markup bytes own the UTF-8 coordinate space and eligible-byte
denominator. An exact/overlap score additionally requires an independent
reference inventory of semantic occurrence intervals. The custom parser's own
claim ledger is evidence about its output, not a neutral reference, and cannot
grade that output.

The resolver first audits which reference intervals exist independently. It may
use reviewed golden spans or a source inventory whose implementation and policy
do not depend on the custom parser. It may not reconstruct spans from parser
text, adapter control flow, or the release-qualification claim ledger.

A lane that does not emit source spans is `non_comparable`, not zero and not
failed. A lane that emits spans can always be measured for bounds validity and
covered source bytes. Exact and overlap metrics remain `unavailable` wherever
the independent semantic reference inventory is incomplete. Malformed or
out-of-bounds claimed coordinates are measured invalid spans.

The custom baseline is measured at its frozen study revision; its later release
qualification result is not substituted into this historical comparison.

## S4 — Controlled Performance

Execute the frozen six-work performance set in the registered candidate and work
order, with one warm-up and five measured repetitions, concurrency one, network
disabled, and the frozen 300-second censoring rule.

Capture elapsed time, completion/censoring, and peak process-tree memory from
the same repetition. Reuse the existing cgroup-v2 resource measurement
mechanism for memory rather than introduce a second RSS definition. Throughput
is derived from the frozen input-byte count and elapsed time.

The analyzer owns the Kaplan–Meier estimator and the frozen two-stage bootstrap:
10,000 resamples, seed 20260714, works first and repetitions second. It emits
median and p95 only under the preregistered survival thresholds; otherwise it
emits the declared lower bound.

Runs occur serially so the study does not perturb itself. Host observations are
captured immediately before each candidate run and evaluated under the frozen
comparability predicate. Non-comparable host strata remain published separately;
they are never pooled or ranked.

## Publication Closure

The final publication index is a closed set containing:

- the preregistration;
- existing run manifests and appendix manifests;
- S1-S4 axis-evidence records;
- any existing evidence records used for the other published axes; and
- the generated machine result and narrative report.

The publication contains all nine preregistered axes in their registered order.
Completeness means every row is grounded in the index with its real disposition,
not that every row is forced to `measured`.

Generation rejects missing members, duplicate lane/axis identities, extra
members, identity drift, and any axis row not derived from the index. The report
continues to publish per-axis results only. It names missing and non-comparable
metrics and prohibits an overall winner.

One command verifies a research bundle by streaming the manifest-referenced
bytes, then regenerates the result and report byte-identically. It accepts the
bundle root as an argument and works after the bundle has been copied to another
directory. This proves containment and replicability without encoding the
location or prescribing how copies are made.

The research bundle is not automatically published or uploaded. Copying it is
an explicit local operation into a caller-controlled root. Raw sources and
third-party artifacts remain subject to their licenses; the public publication
may contain hashes and derived results without redistributing restricted bytes.

## Retirement Boundary

Retirement is a separate change after publication closure, not the final step of
the measurement command. Before retirement, every capture that requires a
third-party executable must have completed or produced an authenticated
build/run failure. A remaining `unavailable` result may not mean "we forgot to
run the parser." It may describe an independent-reference gap such as S1,
because preserving parser output cannot create missing authority.

The retirement change:

- removes third-party parser flake inputs/packages from active outputs;
- removes their adapters, runners, and active tests;
- removes application/configuration choices that select them;
- retains shared libraries only when the custom parser still owns and uses them;
- keeps historical study documents, schemas, manifests, report artifacts, and
  citations truthful;
- records the final study commit in the publication manifest; and
- adds a guard that active parser inventories contain only the custom parser.

Historical documents may still name retired parsers. Active code may not import,
build, invoke, or advertise them. No compatibility shim returns fake results;
the parsers are simply unavailable outside the frozen research revision.

## Failure Semantics

- Missing or hash-mismatched raw evidence: the affected metric is
  `unavailable`; publication closure does not claim completion.
- Oracle/reference gap: quantified `unavailable`; do not shrink the corpus and
  do not invent a replacement oracle.
- No semantic field or span analogue: `non_comparable`, with the absent
  capability named.
- Parser build/run failure: retain the candidate and denominator and publish the
  authenticated failure.
- Performance host mismatch: publish separate strata; no pooled statistic.
- Report/index drift: fail generation; never patch the generated result by hand.
- Research-bundle verification failure: retirement is blocked until the closed
  bundle verifies from a copied root.

## Verification

Tests must prove:

1. every frozen candidate/mode/axis metric appears exactly once;
2. missing evidence cannot shrink a denominator or become zero;
3. absent native diagnostic fields and absent span capability become
   `non_comparable`, while absent captures remain `unavailable`;
4. the all-axis census exposes the unmeasured robustness fixture and every other
   execution-dependent gap before retirement;
5. the fidelity adequacy audit rejects custom-parser-derived authority, pins the
   current XHTML observation-count and eligibility gaps, reconciles actual
   membership, and never shrinks the corpus;
6. the KM and bootstrap implementation reproduces fixed hand-checkable examples,
   preserves censoring, and is deterministic under seed 20260714;
7. performance strata are pooled only when the frozen predicate holds;
8. the report is a pure projection of the closed evidence index;
9. a copied research bundle verifies and regenerates byte-identically from an
   arbitrary root; and
10. after retirement, active build and runtime graphs contain no third-party
   parser packages or selectors, while the historical publication still
   validates.

## Scope Fence

This design does not:

- alter ADR 0038, ADR 0039, the qualification predicate set, admission, or the
  production parser choice;
- rerun or reinterpret Track R evidence;
- change the frozen candidate set, revisions, corpora, fixture labels,
  denominators, bootstrap, or missing-data rules;
- add a generic experiment framework or alternate parser API;
- encode a host, path, filesystem, copy count, or storage provider in study
  identity; or
- delete historical evidence or rewrite prior reports to pretend retired parsers
  never existed.

## Falsifiers

Reopen the design if:

- an `unavailable` metric is found to conceal a third-party execution that was
  never attempted before retirement;
- an axis resolver needs to import release-qualification verdicts rather than
  raw/source-authority evidence;
- the frozen result contract cannot be versioned without changing the
  preregistration's scientific meaning;
- a third-party lane cannot be executed from the frozen study revision before
  retirement; or
- the publication cannot regenerate from a copied closed research bundle after
  active third-party dependencies are removed.

## Dev Handoff

The first implementation slice is the completeness census plus S2. It
establishes the axis-evidence schema, closed index, diagnostic capture, scorer
projection, report-schema v2 shape, and one end-to-end report row without
touching S1, S3, or S4 semantics. The census also enumerates the bounded capture
debt, including the malformed-input robustness fixture.

S1, S3, and S4 then land as separately testable resolvers. Publication closure
and third-party retirement are separate final plans and commits. Retirement may
begin only after all third-party-dependent captures are terminal, every
remaining unavailable metric names a non-execution blocker, the report
regenerates byte-identically, and the research bundle verifies from a copied
root.

## Decision Log

| Decision | Status | Revisit trigger |
| --- | --- | --- |
| One closed study with four axis resolvers | Proposed | Shared mechanics become axis-specific or candidates drift |
| Report schema v2 uses typed metric arrays | Proposed | A lossless representation exists without schema rotation |
| Runtime location is not study identity | Proposed | A scientific claim is shown to depend on storage place |
| Publish before retiring third-party execution | Proposed | Frozen lanes cannot be executed or evidence cannot be contained |
| Preserve research values, delete active alternatives | Proposed | A production requirement for a third-party parser emerges |
