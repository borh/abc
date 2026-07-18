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

The check does not reconstruct this chain. It calls the orchestrator's shared
composition functions by reference. The implementation must extract one
`_execute_operation` helper from `execute_graph`; both production and preflight
call that helper. It owns operation-specific preparation, `operation_argv`,
command execution, and `_project_operation`. A second extracted
`_assert_member_set` helper owns the final closed member-set check for the
caller-supplied expected set.

The preflight calls `authenticate_inputs` with a synthetic campaign record whose
provenance executable rows are derived from the actual `parser-rq-candidate`
package and the committed production graph. It does not replace the returned
executable map afterward. Synthetic values are limited to qualification,
authorization, and readiness identities that have no measurement authority.

The check succeeds only when the expected members are present and every
producer/consumer boundary accepts the bytes emitted by its predecessor.

The check is preflight evidence, not qualification evidence. It writes only
into the Nix build sandbox, publishes no campaign generation, and never
contributes predicate observations.

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
authorizing a candidate. Task 10 must run both checks from one detached
candidate tree, record its Git revision before the first check, assert that the
tree still has that revision immediately before candidate construction, and
abort otherwise. This is an operational shell guard, not a persisted freeze
token. A person can still bypass the runbook; that residual human authority is
explicit and is not misrepresented as a qualification invariant.

## Canonical Taxonomy Correction

`abc/data/parser-rq-ignored-regions-v1.json` is an identity-bearing source-
accountability policy value. The Rust producer already owns its byte contract
and requires exact JCS bytes. Keep that authority in Rust; do not duplicate JCS
validation in Python or Clojure.

Rewrite the committed taxonomy as the exact JCS byte sequence accepted by the
producer. Add a regression test that invokes the real source-accountability CLI
with the production taxonomy path and bounded inputs. The existing negative
tests for noncanonical documents remain unchanged.

Exact canonical bytes are the taxonomy value: a pretty-printed document is
invalid input, not an alternate representation of the same admitted value. Its
evidence identity therefore intentionally hashes the canonical bytes emitted by
the producer.

The taxonomy hash is not a field of `qualification_identity`. The next candidate
identity changes because the correction is committed at a new `parser_git_rev`,
as every code or policy correction does. It does not change the ignored-region
rules, the predicate set, or any historical candidate. Regenerate only artifacts
and ADR evidence that directly bind the taxonomy or the strengthened production
check.

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
graph, or compatibility path is added.

The shared internal contract is explicit:

- `authenticate_inputs` owns graph/provenance equality, executable-byte
  authentication, and construction of `AuthenticatedCampaign.executables`;
- `operation_argv` owns the exact producer command for each operation;
- `_execute_operation` owns dependency preparation, invocation, and projection;
- `_project_operation` owns the Clojure member projection and its closed output
  keys; and
- `_assert_member_set` owns final canonical-member closure.

Production `execute_graph` remains the only owner of authorization verification,
the real campaign lock, time-window enforcement, capture-start state, terminal
recording, composition into a generation, and publication. The preflight reuses
the composition helpers above but none of those release-authority concerns.

### Site preflight

Continues to own volatile runtime capabilities and explicit runtime paths. Host,
filesystem topology, storage location, and backup arrangement remain outside
qualification meaning.

## Closed Inputs and Outputs

The bounded chain uses the same three work IDs already pinned by the production
policies. Membership comes from those policies and the bounded corpus index,
not from filesystem discovery.

The corpus is adequate only if the real core operation emits at least one report
below an adapter subdirectory with a path-hashed filename. The preflight asserts
that topology before projection and separately proves that an injected flat-
named JSON member bearing an unexpected `work_id` is rejected. Filename shape
is not identity; authenticated work membership remains the authority. If the
three works do not produce the required topology, the corpus is inadequate and
candidate freeze stays blocked; the Non-Goal against expansion does not override
this adequacy condition.

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

If the bounded chain cannot exercise a portable operation through the shared
composition helpers without copying their logic, that is a falsifier for this
design. Stop and narrow the operation's public boundary rather than adding an
independent orchestration path to the test.

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
2. Add a failing integration assertion that authenticates the real candidate
   package and runs the portable chain through the shared composition helpers,
   exact production taxonomy, and production policies.
3. Regression-pin all three historical seams before fixing the chain:
   - removing the adapter executable row makes `authenticate_inputs` fail, and
     the authenticated adapter path appears in the real core `operation_argv`;
   - the bounded core output contains a nested, path-hashed report, while a flat
     intruder with an unexpected `work_id` fails closed-membership validation;
     and
   - pretty-printed production taxonomy bytes fail the Rust CLI while exact JCS
     bytes pass.
4. Extract `_execute_operation` and `_assert_member_set` from `execute_graph`
   without changing production behavior, then make the preflight call them.
5. Commit the taxonomy's exact JCS bytes and prove negative membership and policy
   mutations fail at their owning public boundaries.
6. Run the focused root production-wiring check, the affected Rust/Python tests,
   `just python-quality`, and `just validate-migration`.
7. On hinoki, bind one `freeze_rev` to the detached candidate tree, run the live
   cgroup capability check and repository runtime preflight from that tree, and
   assert the revision is unchanged immediately before candidate construction.

## Non-Goals

- Changing any predicate, comparator, threshold, or predicate-set identity.
- Changing registry admission, evaluation, or ADR promotion semantics.
- Creating or publishing qualification observations during preflight.
- Encoding hostname, mount, filesystem, storage, or backup facts in candidate
  or qualification identity.
- Adding retries or selecting among multiple captures.
- Replacing producer-owned validation with a shared generic validator.
- Expanding the bounded corpus or tuning it to expected predicate verdicts,
  unless the existing corpus fails the explicit report-topology adequacy check.

## Acceptance Criteria

1. The exact production taxonomy is accepted by the real source-accountability
   CLI and its content is exact JCS.
2. One root check executes the five portable operations with real candidate
   binaries, repository drivers, production policies, and the bounded corpus.
3. Real output from each upstream operation is consumed by its real downstream
   boundary; help/version probes alone are insufficient.
4. `authenticate_inputs`, `operation_argv`, `_execute_operation`,
   `_project_operation`, and `_assert_member_set` are called by both production
   and preflight; their logic is not copied into the integration test.
5. The resource lane remains explicitly host-controlled and is not simulated as
   part of the portable chain.
6. `just validate-migration` includes and passes the strengthened root check.
7. No new orchestrator mode, generic preflight framework, qualification field,
   or machine-specific identity is introduced.
8. All three failed candidate authorizations remain unchanged.
9. The Task 10 shell guard runs the portable chain and live resource capability
   check from one detached tree, records its `freeze_rev`, and refuses candidate
   construction if the tree revision changes. The remaining ability to bypass
   the runbook is documented human operational authority, not a machine-enforced
   qualification property.
10. The new chain demonstrably catches all three historical seams before a
    fourth candidate is frozen.

## Falsifiers

Reopen this decision if:

- a portable production operation cannot be exercised through the shared
  composition helpers without duplicating their logic;
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
