# Parser Qualification Remediation Design

Date: 2026-07-11
Status: Proposed design for review
Parent: `2026-07-11-adr-logical-remediation-program-design.md`
Depends on: `2026-07-11-adr-evidence-and-lifecycle-remediation-design.md`

## Purpose

Record the project-owned custom parser as the practical production direction,
separate that ownership decision from compatibility admission and release
qualification, preserve a scientifically neutral comparison of existing
parsers, and make maintenance claims falsifiable.

## Current Faults

ADR 0002 states must-pass parser viability gates. ADR 0030 frames measurements
of existing parsers as a selection contest, chooses a consolidated-parser base
while reporting three failed `must` vectors and 0.969 coverage, then moves ADR
0002 to Accepted. The project nevertheless requires a maintained custom parser:
no existing parser supplies the owned AAT, source-region, diagnostic, and
publication structures required by the architecture. Comparative evidence can
characterize reusable implementations and establish baselines, but cannot make
an existing parser satisfy those ownership requirements or satisfy all release
predicates.

ADR 0032 claims selective porting will remain cheaper than merging. Its revisit
accounting excludes planned roadmap work and does not define a session or a
counterfactual merge measurement.

## Ownership Decision

The production parser is a project-owned custom parser maintained within the
Soranoha monorepo. Its current implementation may inherit code from the pinned
hard fork recorded by ADR 0032 and may selectively port techniques or fixes
from other parsers. That provenance does not make an upstream parser the owner
of Soranoha's production contract.

This is a feasibility and ownership decision, not the conclusion of a parser
horse race. Reversing it requires evidence that an externally owned parser can
satisfy the complete owned contract without transferring critical publication
policy or release control outside the project.

## Parser State Machine

```text
measured implementation
    -> reusable source or comparator role

project-owned custom parser
    -> admitted parser/adapter/mapping tuple
    -> release-qualified parser
```

- **Measured implementation:** comparison report is complete and reproducible.
- **Reusable source or comparator:** measured behavior justifies a bounded
  role such as inherited code, selective port, regression oracle, or throughput
  reference; it does not confer production ownership.
- **Admitted tuple:** exact output tuple passes ADR 0023 compatibility gates.
- **Release-qualified parser:** all release predicates pass on the accepted
  qualification corpus.

The two branches are deliberately not one promotion chain. Measurement of an
existing parser does not promote it toward production ownership. Existing
admitted historical adapters remain independent evidence producers.

## Neutral Existing-Parser Comparison

The comparison exists to describe the available parser landscape and provide
falsifiable baselines for the custom parser. It MUST NOT be designed to
retroactively justify the custom-parser decision.

Before executing or refreshing the comparison, check in a preregistered study
contract containing:

- the research questions and intended uses of each metric;
- the complete candidate inclusion and exclusion rules;
- pinned source revisions, adapters, corpora, fixtures, and environment;
- native-output measurements separated from adapter-normalized measurements;
- construct-level and frequency-weighted metrics with exact denominators;
- failure, timeout, unsupported, and silent-drop definitions;
- diagnostic, span, fidelity, robustness, performance, maintenance, packaging,
  and license observations;
- treatment of missing data and non-comparable capabilities;
- declared comparator/oracle roles; and
- the analysis and reporting procedure fixed before results are inspected.

The report publishes all candidate results, including results inconvenient to
the custom-parser direction. It distinguishes observation from interpretation,
reports uncertainty and limitations, and does not collapse heterogeneous axes
into one winner score. Weighting is permitted only for a named downstream use
case and must include sensitivity analysis showing whether reasonable weights
change the conclusion.

The custom parser is evaluated with the same applicable instruments and is not
granted a pass for project ownership. Where its owned contract has no analogue
in existing parsers, the report marks the axis non-comparable instead of
assigning competitors zero.

Primary uses of the comparison are:

- baseline and regression measurement for the custom parser;
- discovery of constructs or algorithms worth porting;
- independent comparator/oracle evidence;
- documented coverage of the parser ecosystem; and
- evidence that can falsify performance or fidelity claims about the custom
  parser.

## Qualification Corpus and Predicates

Promote the smoke-corpus list from prose to a pinned artifact recording source
snapshot, work IDs, selection reasons, expected outcomes, and list hash.

The qualification report records exact numerators and denominators for:

- fatal failures and the predeclared threshold;
- parsed-source span coverage and ignored-region taxonomy;
- silent drops;
- stable diagnostic code, severity, and available span;
- parser-IR schema validation;
- required publication structures;
- wall time, memory, and timeout policy; and
- parser, adapter, mapping, schema, and corpus identities.

Rounded composite coverage is descriptive, never a substitute for a must-pass
predicate. A report with an observed value below its threshold has verdict
`fail` even when the candidate remains the best available base.

## ADR Repair

A corrective ADR preserves ADR 0030's measurements but narrows its normative
conclusion: the inherited hard-fork pin is provenance and an implementation
starting point for the project-owned parser, not proof that an existing parser
won production ownership. ADR 0002's comparative-study scope records neutral
characterization evidence. Its release-viability scope remains Proposed until
the custom parser passes qualification. Comparison, admission, and release
gates cite distinct typed evidence entries.

## Fork Economics

Record quarterly:

- upstream-review time;
- selective-port time;
- security-review and missed-fix incidents;
- fork-only defect time;
- planned divergence time;
- remeasurement time; and
- a disposable trial-merge estimate at review points.

The trial merge produces a documented, re-derivable estimate with explicit
assumptions; it is not described as a counterfactual measurement. It is typed
as `:expert-assessment`, with observed conflict counts or timings separately
typed as `:benchmark` when reproducible.

Planned work is labelled separately but included in total ownership cost. A
focused session has a fixed duration declared in the report. Revisit predicates
use observed rolling-quarter totals, port-cost distribution, missed-fix
severity, and maintainer availability.

## Acceptance Criteria

- The custom-parser ownership decision is stated independently of comparative
  rankings.
- A preregistered study contract fixes candidate scope, metrics, denominators,
  adapter treatment, and reporting rules before comparison results are used.
- The report publishes per-axis observations and limitations for every included
  parser without an unqualified aggregate winner score.
- Native behavior and adapter-induced behavior are reported separately.
- The custom parser is measured by the same applicable instruments and may
  fail comparison axes without changing the ownership decision.
- State-transition tests reject comparison evidence as admission or
  qualification evidence.
- The pinned corpus contract declares every threshold before measurement.
- A 0.969 observation fails a 1.0 predicate mechanically.
- Qualification reports contain exact identities, counts, caveats, and result
  hashes.
- The consolidated fork passes every release predicate before release authority
  becomes publication.
- Fork-cost reports include all categories and a documented, re-derivable
  estimate with stated assumptions.
- A change in inherited code provenance or implementation substrate requires a
  new ownership/provenance ADR and evidence, but does not alter manifest
  identity semantics.

## Safe Fallback

Continue maintaining the custom parser without publication authority. Preserve
the neutral comparison as evidence. Do not weaken a failing predicate merely
to qualify the parser; changing a predicate requires a separately evidenced
ADR.
