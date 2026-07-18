# Parser-RQ Bounded Production Preflight Design

**Date:** 2026-07-19  
**Status:** Proposed  
**Scope:** Pre-freeze integration assurance for the parser release-qualification campaign

## Purpose

The release-qualification campaign has used authoritative, one-shot captures to
discover integration defects that bounded verification should have found first.
Three successive candidates exposed three different production seams:

1. the core capture did not receive the authenticated adapter executable;
2. the core capture assumed flat report filenames while `ab-check` emits nested,
   path-hashed reports; and
3. source accountability received the committed ignored-region taxonomy in
   pretty-printed JSON while its Rust boundary requires the taxonomy's exact JCS
   bytes.

Each failure was honestly terminal for its candidate. The problem is not the
fail-closed behavior. The problem is that the authoritative run is serving as
the first composed integration test.

This design moves portable integration discovery before candidate freeze. It
does not weaken authorization, create retries, or reinterpret the three failed
candidates.

## Decision

Extend the existing root `parser-rq-production-wiring` check into one bounded
production-chain preflight. The check uses:

- the actual `parser-rq-candidate` package;
- the actual repository drivers;
- the committed production graph and production policy files; and
- the existing three-work bounded Aozora corpus.

It executes the portable production dataflow in dependency order:

1. core attempt with real `ab-check` and `ab-aozora`;
2. predicate hardening with real `ab-aozora` and
   `ab-aat-to-parser-ir`;
3. source accountability with the exact committed taxonomy;
4. diagnostic-gap derivation from the real bounded upstream outputs; and
5. publication capture and validation from the real bounded Parser-IR outputs.

The check then passes those outputs through the same production projection and
closed-membership boundaries used by the orchestrator. It succeeds only when
the expected members are present and every producer/consumer boundary accepts
the bytes emitted by its predecessor.

The check is preflight evidence, not qualification evidence. It uses synthetic
identity values, writes only into the Nix build sandbox, publishes no campaign
generation, and never contributes predicate observations.

Candidate freeze is prohibited unless `just validate-migration`, including this
check, passes at the exact commit to be frozen.

## Resource-Lane Boundary

The cgroup resource lane is not portable into a Nix build sandbox. This design
does not fake that property or add a second resource implementation.

The bounded production-chain check authenticates the resource executable and
command mapping as it does today. The existing resource capture tests continue
to own record construction and analyzer behavior. Hinoki's existing
`preflight-site` and live cgroup smoke own the host-controlled cgroup contract
before freeze.

Therefore the pre-freeze boundary is deliberately two-part:

- one portable, composed dataflow check for the five portable operations; and
- one explicit live capability check for the resource operation.

Neither result enters qualification identity. Both are prerequisites for
authorizing a candidate.

## Canonical Taxonomy Correction

`abc/data/parser-rq-ignored-regions-v1.json` is an identity-bearing policy
value. The Rust producer already owns its byte contract and requires exact JCS
bytes. Keep that authority in Rust; do not duplicate JCS validation in Python or
Clojure.

Rewrite the committed taxonomy as the exact JCS byte sequence accepted by the
producer. Add a regression test that invokes the real source-accountability CLI
with the production taxonomy path and bounded inputs. The existing negative
tests for noncanonical documents remain unchanged.

The representation correction changes the taxonomy's content hash and therefore
the next candidate identity. It does not change the ignored-region rules, the
predicate set, or any historical candidate. Regenerate only artifacts and ADR
evidence that directly bind the taxonomy or the strengthened production check.

## Component Boundaries

### Root production-wiring check

Owns the composed portable preflight. It supplies the real candidate package and
repository source to a focused integration test and fails on any rejected
producer output, policy input, membership set, or projection.

### Existing producers

Remain the sole authorities for their input contracts and measurements. The
preflight invokes their public CLIs; it does not reimplement validation or
arithmetic.

### Campaign orchestrator

Remains the sole production executor. No `--dry-run`, `--preflight`, alternate
graph, or compatibility path is added. The preflight verifies the pieces that
the orchestrator composes without becoming a second orchestrator.

### Site preflight

Continues to own volatile runtime capabilities and explicit runtime paths. Host,
filesystem topology, storage location, and backup arrangement remain outside
qualification meaning.

## Closed Inputs and Outputs

The bounded chain uses the same three work IDs already pinned by the production
policies. Membership comes from those policies and the bounded corpus index,
not from filesystem discovery.

For every operation, the check rejects:

- a missing or extra work;
- duplicate authenticated work identity;
- malformed or noncanonical policy input where the producer requires canonical
  bytes;
- an upstream output that the downstream public CLI rejects;
- a projection that cannot authenticate the producer output; and
- an unexpected filesystem member, symlink, or escaping locator at a closed
  output boundary.

The check may inspect temporary paths to run the pipeline, but paths are not
persisted, hashed into qualification identity, or asserted as reproducibility
facts.

## Failure Semantics

A bounded-chain failure blocks candidate freeze. It does not create a candidate,
authorization, capture attempt, unavailable observation, registry row, or ADR
status change.

Once a candidate is authorized, the existing one-shot semantics remain:
failure is terminal and no retry is permitted. The failed authorizations already
committed for the previous candidates remain immutable historical facts.

If the bounded chain cannot exercise a portable operation without reproducing
the orchestrator, that is a falsifier for this design. Stop and narrow the
operation's public boundary rather than adding orchestration logic to the test.

## Alternatives Considered

### Correct only the taxonomy file

Rejected. It repairs the observed symptom but leaves the next uncomposed seam
to the next authoritative run.

### Run an ungoverned full rehearsal on hinoki

Rejected. It previews volatile results, may warm the candidate's runtime state,
and creates a second near-production execution mode adjacent to the one-shot
authority.

### Add a production-orchestrator dry-run mode

Rejected. It adds state and branching to the release authority, requires fake
authorization/resource semantics, and risks drift between dry-run and
production paths.

### Create a new preflight framework

Rejected. The root Nix check, bounded corpus, real candidate package, and public
producer CLIs already provide the required pieces. The missing property is
composition, not another abstraction.

## Verification Strategy

1. First demonstrate the current root production-wiring check passes while the
   real source-accountability CLI rejects the committed taxonomy.
2. Add a failing integration assertion for the portable chain using the exact
   production taxonomy and policies.
3. Commit the taxonomy's exact JCS bytes and make the smallest wiring-test
   changes needed to run the five-operation chain.
4. Prove negative membership and policy mutations fail at their owning public
   boundaries.
5. Run the focused root production-wiring check, the affected Rust/Python tests,
   `just python-quality`, and `just validate-migration`.
6. On hinoki, run the live cgroup capability check and the repository runtime
   preflight before freezing another candidate.

## Non-Goals

- Changing any predicate, comparator, threshold, or predicate-set identity.
- Changing registry admission, evaluation, or ADR promotion semantics.
- Creating or publishing qualification observations during preflight.
- Encoding hostname, mount, filesystem, storage, or backup facts in candidate
  or qualification identity.
- Adding retries or selecting among multiple captures.
- Replacing producer-owned validation with a shared generic validator.
- Expanding the bounded corpus or tuning it to expected predicate verdicts.

## Acceptance Criteria

1. The exact production taxonomy is accepted by the real source-accountability
   CLI and its content is exact JCS.
2. One root check executes the five portable operations with real candidate
   binaries, repository drivers, production policies, and the bounded corpus.
3. Real output from each upstream operation is consumed by its real downstream
   boundary; help/version probes alone are insufficient.
4. The same production projection and closed-membership code authenticates the
   bounded outputs.
5. The resource lane remains explicitly host-controlled and is not simulated as
   part of the portable chain.
6. `just validate-migration` includes and passes the strengthened root check.
7. No new orchestrator mode, generic preflight framework, qualification field,
   or machine-specific identity is introduced.
8. All three failed candidate authorizations remain unchanged.
9. No fourth candidate is frozen until both the portable chain and live resource
   capability checks pass at its exact commit.

## Falsifiers

Reopen this decision if:

- a portable production operation cannot be exercised through its public CLI
  without duplicating campaign orchestration;
- the bounded chain passes while an unchanged static producer/consumer contract
  fails in the next authoritative run;
- running the bounded chain alters persistent or host-global state; or
- the resource lane can be made genuinely portable without simulating cgroup
  semantics.

## Decision Record

This design deliberately responds to three different authoritative integration
failures with one compositional boundary rather than a fourth local patch. The
extra pre-freeze work is justified by a narrower release process afterward: one
portable chain, one live resource capability check, and then the single governed
capture.
