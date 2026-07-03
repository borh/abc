# Next Best Steps Deep-Dive

> Date: 2026-07-03. Skills used: `hammock-driven-design` and
> `architecture-triage`. Scope: post-ADR-0024-cascade investigation of the
> next highest-leverage work in ABC after the design-bundle gate was restored.

## Current Verified State

- Working tree was clean at the start of this investigation.
- Recent commits:
  - `d9d24f8 docs: preserve parser mapping handoff evidence`
  - `2a09630 fix(parser-ir): complete ADR 0024 fixture cascade`
- `nix run .#validate-design-bundle` passes.
- `nix flake check` passes. The command reports all checks passed, omitting
  incompatible `aarch64-linux` checks.

## Triage Result

The remaining work splits into four buckets:

1. **Design-corpus drift that is implementation-ready.**
   The repo has implemented behavior recorded in Draft ADRs. This is not a
   runtime bug, but it erodes the promotion contract: readers cannot tell which
   decisions are live obligations.

2. **Validation-harness hardening that is implementation-ready.**
   The Schematron half of the validate-bundle simplification has landed, but
   person-drift failure codes remain scattered in `person_drift.clj` with no
   authoritative registry.

3. **Protocol/identity work that is high-value but larger.**
   The owned AAT -> parser-IR mapping is now better specified, but it crosses
   ABC and ab-validator ownership. Execute it in ABC-side slices first.

4. **Measurement-blocked provisional work.**
   The marker registry and bounded-workset designs explicitly say not to
   promote to production until their remaining probes are run.

## Recommended Order

### 1. Reconcile implemented ADR statuses and ADR 0006 stale text

Route: documentation/status correction.

Why first:

- It is low-risk and removes live design-corpus drift.
- `docs/handoffs/rfc-restructure-spec.md` already verified the evidence and
  gives exact target ADRs.
- The current repo state satisfies the requested verification gates
  (`validate-design-bundle` and `flake check`).

Do:

- Accept ADRs 0007, 0009, 0010, and 0015 with `Accepted: 2026-07-03`.
- Add concise `Implementation Status` sections citing the live code/gates.
- Update ADR 0006 Implementation Status so it no longer says TEI validation is
  upstream-only or that the project ODD/Schematron path is still a stub.
- Optionally add one-sentence implementation-missing notes to ADRs 0002-0005
  rather than accepting them.

Done when:

- The four implemented ADRs read `Status: Accepted`.
- `nix run .#validate-design-bundle` and `nix flake check` still pass.

### 2. Finish the person-drift half of validate-bundle simplification

Route: codebase simplification / gate hardening.

Why second:

- `validate-bundle-spec-verification.md` says `EXECUTE-AS-IS`.
- The Schematron exact-equality/root-cause fix has landed, but the drift code
  universe remains implicit and scattered.
- This is a behavior-preserving strengthening if limited to an authoritative
  registry plus "actual codes must be known" checks. Do not require every code
  to have a negative fixture in the same patch.

Do:

- Add `person-drift/failure-codes` containing the 20 known emitted codes.
- Add a small helper/invariant so emitted failures are members of that set.
- Teach `validate-drift-fixtures!` to reject unknown actual codes.
- Keep the existing exact per-fixture expected-set behavior.
- File or defer the separate semantic work: negative fixtures for uncovered
  codes such as `:used-not-sorted`, `:generated-not-sorted`,
  `:duplicate-snapshot-id`, `:participant-in-both-used-and-generated`,
  `:snapshot-prefix-usage-mismatch`, `:unresolved-curie-prefix`,
  `:unexpected-rdf-type`, `:schema-hash-mismatch`, and `:exception`.

Done when:

- Focused validate-design-bundle tests pass.
- `nix run .#validate-design-bundle` passes.

### 3. Start owned AAT -> parser-IR mapping on the ABC side only

Route: protocol/identity boundary work.

Why third:

- It is the highest-value protocol work, but it changes schema identity,
  manifests, fixtures, materialization, and eventually ab-validator.
- ABC can safely establish its accepting contract before the producer CLI
  exists.

Do as small slices:

- Add mapping and divergence schemas to ABC and validate them in the bundle.
- Add optional `derived_from` to parser-IR.
- Add `mapping_hash` to manifest inputs and `aat_parser_ir_mapping_hash` to
  manifest identity.
- Add `mapping-divergence` sidecar role.
- Add the compatibility registry with both `mapping_hash` (document identity)
  and `mapping_schema_hash` (mapping-document contract provenance).

Guardrails:

- Do not reintroduce `aat_parser_ir_mapping_schema_hash` as an identity
  dimension.
- Treat `ruby.direction` as direct projection after ADR 0024, not as L-02 loss.
- Keep release strictness mode-bound: dev records critical divergence and
  continues; release-smoke fails on critical divergence.

Done when:

- ABC boundary fixtures materialize with the mapping identity dimension.
- `nix run .#validate-design-bundle` passes after each slice.

### 4. Run measurement probes before productionizing provisional designs

Route: hammock/prototype, not production implementation.

Do not implement the authoritative marker registry or bounded-workset index
as production code yet.

Measurement candidates:

- Bounded workset: build the disposable A-vs-C Nix benchmark from
  `bounded-workset-index-design.md` and measure eval wall time, peak memory,
  cold build time, incremental single-work rebuild behavior, derivation count,
  and per-work failure attribution for 100/1,000/5,000 works.
- Aozora marker registry: generate a draft registry from all 13 annotation
  pages with `description = NEEDS_REVIEW`, then count how much human curation
  remains.
- Mapping realism: run the aozora2html full-corpus AAT probe before making
  warigaki policy claims for that adapter path.

Done when:

- Each provisional design has a measurement note that either kills an approach,
  keeps it viable, or identifies the next narrow unknown.

## Not Next

- Full production marker registry implementation: explicitly provisional.
- Production bounded-workset index: explicitly pre-decision.
- ADR 0004 release-security implementation: important before public release,
  but it is not the next local gate unless a release cut is imminent.
- Full `docs/architecture.md` / `docs/design-survey.md` split before ADR
  status reconciliation: the status drift should be resolved first so the new
  architecture surface can cite stable decisions.

## Incubation Note

The ordering above is provisional but evidence-backed. The only item that
looks immediately executable without new measurement is ADR status
reconciliation. The person-drift registry is the next code change with the
best risk/reward ratio. Owned mapping should follow in ABC-only slices, and
the registry/workset efforts should stay as measured prototypes until their
own handoffs' open questions are closed.
