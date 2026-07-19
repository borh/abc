# Parser-RQ Bounded Production Preflight Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use `superpowers:executing-plans` or `superpowers:subagent-driven-development` to execute this plan task by task. Keep the checkboxes current.

**Goal:** Prove the portable parser-RQ production chain with the real candidate package, policies, bounded corpus, and orchestrator composition code before candidate freeze.

**Architecture:** Extract two behavior-preserving helpers from the production orchestrator, then call those helpers from the existing root production-wiring check. Authenticate the real candidate package rather than replacing its executable map. Keep release authority, real locks, authorization, terminal state, publication, resource measurement, and qualification evidence out of the portable preflight.

**Tech Stack:** Python 3, pytest, Clojure 1.12, Rust, Nix flakes, Bash, JSON/JCS.

## Global Constraints

- Work on `main`, as explicitly requested. Do not push until the entire plan is green.
- Use TDD for every behavior change: demonstrate the focused failure, make the minimum change, and rerun the focused check.
- Do not mint an authorization, freeze a candidate, start a volatile lane, write a capture generation, append the registry, or change ADR status.
- Do not add an orchestrator dry-run/preflight mode, a second graph, a freeze-token protocol, or duplicate producer validation.
- Do not replace `AuthenticatedCampaign.executables` or `drivers` after real authentication. The Nix test may replace only `clojure_prefix` with its hermetic launcher.
- Do not add hostname, filesystem, storage, or backup facts to candidate or qualification identity.
- Preserve the three existing failed authorizations as immutable history.
- Use `apply_patch` for source edits and formatters only for mechanical formatting.

---

### Task 1: Extract the production composition seam without changing behavior

**Files:**
- Modify: `abc/tools/parser_rq_campaign_orchestrator.py`
- Modify: `abc/tools/test_parser_rq_campaign_orchestrator.py`

**Interfaces:**
- `_execute_operation(operation, campaign, paths, lock, runner, cwd)` owns operation-specific preparation, `operation_argv`, execution, and `_project_operation`.
- `_assert_member_set(paths, expected_members)` owns exact final JSON-member closure.
- `execute_graph` remains the sole owner of authorization, the production lock, time, capture state, terminal recording, composition, and publication.

- [ ] **Step 1: Add a recording runner and failing helper tests**

Add a test runner that records `argv`, `cwd`, and `pass_fds` while creating the bounded output expected by the selected operation. Add tests named:

```python
def test_execute_operation_passes_lock_fd_only_to_core(...) -> None: ...
def test_assert_member_set_rejects_missing_and_extra_members(...) -> None: ...
```

The first test must prove that `capture-core` receives `(lock.fd,)`, while a non-core operation receives `()`. The second must prove exact equality, not subset membership.

- [ ] **Step 2: Run the focused tests and confirm RED**

```bash
nix build ./abc#checks.x86_64-linux.parser-rq-campaign-orchestrator -L
```

Expected failure: the two helpers cannot be imported or called because the logic still lives inline in `execute_graph`.

- [ ] **Step 3: Extract the minimum helpers**

Move the existing per-operation body into this shape without changing its ordering or errors:

```python
def _execute_operation(
    operation: str,
    campaign: AuthenticatedCampaign,
    paths: RuntimePaths,
    lock: LockCapability,
    runner: Runner,
    cwd: Path,
) -> None:
    if operation == "derive-diagnostic-gap":
        _prepare_diagnostic_input(campaign, paths)
    elif operation == "capture-publication":
        _prepare_publication(campaign, paths, runner, cwd)
    elif operation == "capture-resource":
        _prepare_resource(campaign, paths)

    argv = operation_argv(operation, campaign, paths, lock)
    pass_fds = (lock.fd,) if operation == "capture-core" else ()
    _run_checked(runner, argv, cwd=cwd, pass_fds=pass_fds)
    _project_operation(operation, campaign, paths, runner, cwd)
```

Extract the final member comparison into:

```python
def _assert_member_set(paths: RuntimePaths, expected_members: Collection[str]) -> None:
    expected = {f"{member}.json" for member in expected_members}
    installed = {f"{member}.json" for member in EXPECTED_INSTALLED_MEMBERS}
    actual = {
        path.name
        for path in paths.root.glob("*.json")
        if path.name in installed
    }
    if actual != expected:
        raise ProtocolError(
            f"canonical member mismatch: expected={sorted(expected)} actual={sorted(actual)}"
        )
```

Call both helpers from `execute_graph`. Do not move or copy authentication, locking, authorization, terminal-state, composition, or publication code.

- [ ] **Step 4: Prove the refactor is behavior-preserving**

```bash
nix build ./abc#checks.x86_64-linux.parser-rq-campaign-orchestrator -L
just python-quality
```

- [ ] **Step 5: Commit the extraction**

```bash
git add abc/tools/parser_rq_campaign_orchestrator.py \
  abc/tools/test_parser_rq_campaign_orchestrator.py
git commit -m 'refactor(parser-rq): share operation composition seam'
```

---

### Task 2: Authenticate the real candidate package in the root wiring test

**Files:**
- Modify: `abc/tools/test_parser_rq_campaign_orchestrator.py`
- Modify: `flake.nix`

**Interfaces:**
- The test receives the candidate package path from the existing
  `PARSER_RQ_CANDIDATE_ROOT` environment variable.
- Provenance executable rows are derived by streaming the actual package files and matching the committed production graph.
- `authenticate_inputs` constructs the authoritative executable and driver maps.

- [ ] **Step 1: Make the existing real-candidate test demand real authentication**

Keep the existing test name selected by the root Nix check, but remove its
post-authentication replacement of `executables` and `drivers`. Change its
fixture construction to request authentication of the actual candidate package.

- [ ] **Step 2: Run the root check and confirm RED at authentication**

```bash
nix build .#checks.x86_64-linux.parser-rq-production-wiring -L
```

Expected RED: the old synthetic provenance rows do not authenticate the real
candidate executable paths and bytes. No projection or Clojure failure is
expected in this task.

- [ ] **Step 3: Derive authentic provenance rows in the test fixture**

Add `real_authenticated_campaign(tmp_path, repository_root, candidate_root)`. It must:

1. load `abc/data/parser-rq-production-graph-v1.json`;
2. derive every executable row named by the graph from `candidate_root`, including byte count and SHA-256;
3. write a synthetic provenance core and receipt whose identities agree with the graph and candidate fixture;
4. call `authenticate_inputs` with `candidate_tree=repository_root`; and
5. return the authenticated result unchanged.

Do not use `dataclasses.replace` to substitute `executables` or `drivers`.

- [ ] **Step 4: Regression-pin the adapter seam**

Add:

```python
def test_real_candidate_authentication_requires_adapter_row(...) -> None: ...
def test_real_core_argv_uses_authenticated_adapter(...) -> None: ...
```

The first removes the adapter provenance row and expects `authenticate_inputs` to fail closed. The second asserts the `--adapter` value in `operation_argv("capture-core", ...)` equals the authenticated adapter executable path.

Change the root check selection from the single legacy test name to
`-k real_candidate` so all real-package authentication regressions run inside
the candidate-bearing Nix check. This is only test selection; the Clojure
runtime closure remains deferred until Task 3 exercises projection.

- [ ] **Step 5: Verify and commit**

```bash
nix build .#checks.x86_64-linux.parser-rq-production-wiring -L
just python-quality
nixfmt flake.nix
just nix-format-check
git add abc/tools/test_parser_rq_campaign_orchestrator.py flake.nix
git commit -m 'test(parser-rq): authenticate production candidate wiring'
```

---

### Task 3: Compose the five portable operations and correct the taxonomy bytes

**Files:**
- Modify: `abc/tools/test_parser_rq_campaign_orchestrator.py`
- Modify: `abc/data/parser-rq-ignored-regions-v1.json`
- Modify: `ab-validator/reports/parser-ir/test_parser_rq_core_attempt_capture.py`
- Modify: `flake.nix`

**Interfaces:**
- The preflight calls `authenticate_inputs`, `_materialize_runtime`, `_execute_operation`, and `_assert_member_set` by reference.
- It executes `capture-core`, `capture-predicate-pair`, `capture-source`,
  `derive-diagnostic-gap`, and `capture-publication` in production graph order.
- It expects exactly six portable members because predicate hardening projects both `diagnostic_completeness` and `parser_ir_conformance`.

- [ ] **Step 1: Add the bounded runtime fixture**

Create test helpers that write a full runtime JSON value for the three work IDs already named by the committed policies. Source paths must resolve below the immutable repository root, source hashes must be streamed from those files, the authorization window must contain the test's explicit current time, and all synthetic identity references must be internally coherent.

The helper may synthesize qualification, authorization, readiness, and attempt identities. It must not synthesize executable provenance, policy membership, source membership, or producer output.

- [ ] **Step 2: Add the real bounded-chain test**

Add `test_bounded_production_chain_uses_shared_composition(...)` with this structure:

```python
campaign = real_authenticated_campaign(
    tmp_path, repository_root, Path(os.environ["PARSER_RQ_CANDIDATE_ROOT"])
)
paths = RuntimePaths.below(campaign.config.staging_root)
write_bounded_runtime(paths.runtime, ...)
_materialize_runtime(paths)
cwd = paths.root / "detached-cwd"
cwd.mkdir(parents=True, exist_ok=True)
lock = _acquire_lock(tmp_path / "campaign.lock")
try:
    for operation in campaign.operations:
        if operation == "capture-resource":
            break
        _execute_operation(
            operation, campaign, paths, lock, SubprocessRunner(), cwd
        )
finally:
    os.close(lock.fd)

_assert_member_set(
    paths,
    {
        "core_attempt",
        "diagnostic_completeness",
        "parser_ir_conformance",
        "source_recognition",
        "diagnostic_gap",
        "publication_structure",
    },
)
```

Use the module's existing runner abstraction rather than passing a callable if its concrete signature differs. The load-bearing property is that production and preflight call the same `_execute_operation`, not the spelling of the runner value.

- [ ] **Step 3: Pin the historical report-topology failure**

After the shared core operation returns, assert that its retained raw output includes at least one report below an adapter subdirectory and that its basename is path-hashed. In the core matcher test, inject `flat-intruder.json` containing an unexpected `work_id` and assert the owning matcher rejects it. Do not make filename shape an identity source.

Update the root check's pytest selection so it runs both the real authentication
regressions and `test_bounded_production_chain_uses_shared_composition`; do not
leave the new chain outside the Nix gate.

- [ ] **Step 4: Demonstrate that real projection needs a declared Clojure runtime**

```bash
nix build .#checks.x86_64-linux.parser-rq-production-wiring -L
```

Expected RED: the bounded chain reaches `_project_operation`, but the root
check does not yet contain the Clojure runtime and dependency closure needed by
the authenticated campaign's projection commands.

- [ ] **Step 5: Supply the existing hermetic runtime, not a new wrapper**

Update `parser-rq-production-wiring` to include `cljPkgs.clojure`, `bash`,
`coreutils`, the existing Clojure dependency cache/environment, and the
existing Python test environment. In the bounded test, replace only the
authenticated campaign's `clojure_prefix` with this launcher from the immutable
repository source:

```python
(
    "bash",
    "-c",
    'cd "$0" && exec clojure "$@"',
    str(repository_root / "abc"),
)
```

Do not replace `executables` or `drivers`. Do not depend on the operation
process working directory or ambient `PATH` beyond the Nix check's declared
inputs.

- [ ] **Step 6: Demonstrate the production taxonomy failure**

Run:

```bash
nix build .#checks.x86_64-linux.parser-rq-production-wiring -L
```

Expected RED from the real source-accountability CLI:

```text
taxonomy is not canonical JSON
```

This is the regression for the third failed candidate; do not bypass it in Python or Clojure.

- [ ] **Step 7: Produce candidate canonical bytes, with Rust as the authority**

```bash
tmp="$(mktemp)"
jq -j -cS . abc/data/parser-rq-ignored-regions-v1.json > "$tmp"
mv "$tmp" abc/data/parser-rq-ignored-regions-v1.json
```

For the current string-and-empty-array document, this produces the candidate
compact, sorted, newline-free bytes. `jq` is not a general RFC 8785
canonicalizer. The real source-accountability CLI in the next step is the sole
authority: if it rejects these bytes, obtain the exact canonical bytes through
the Rust producer rather than weakening or duplicating its validation. Do not
change the rules.

- [ ] **Step 8: Prove all three historical seams and the full portable chain**

```bash
nix build .#checks.x86_64-linux.parser-rq-production-wiring -L
nix build ./ab-validator#checks.x86_64-linux.parser-rq-core-attempt-python-tests -L
cd ab-validator
cargo test -p ab-parser-rq-source-accountability \
  taxonomy_parser_rejects_non_v1_and_noncanonical_documents
cd ..
just python-quality
```

Confirm the Rust test actually runs one test; do not use `--exact` with a partial name.

- [ ] **Step 9: Commit the bounded chain**

```bash
git add abc/tools/test_parser_rq_campaign_orchestrator.py \
  abc/data/parser-rq-ignored-regions-v1.json \
  ab-validator/reports/parser-ir/test_parser_rq_core_attempt_capture.py \
  flake.nix
git commit -m 'test(parser-rq): compose bounded production preflight'
```

---

### Task 4: Bind every pre-freeze prerequisite to one detached revision

**Files:**
- Modify: `abc/docs/superpowers/plans/2026-07-17-parser-rq-admission-promotion.md`

**Interfaces:**
- Task 10 owns the operational pre-freeze sequence.
- `freeze_rev` is a shell-local guard only; it is not evidence, a receipt, or an identity field.

- [ ] **Step 1: Strengthen Task 10's pre-freeze commands**

Replace separate, unbound instructions with one block run from the detached candidate tree:

```bash
freeze_rev="$(git -C "$candidate_tree" rev-parse HEAD)"
test -z "$(git -C "$candidate_tree" status --short)"

nix build "$candidate_tree#checks.x86_64-linux.parser-rq-production-wiring" -L
nix run "$candidate_tree/ab-validator#parser-rq-resource-cgroup-smoke"
: "${PARSER_RQ_SITE_DESCRIPTOR:?set the reviewed site-descriptor JSON path}"
graph="$candidate_tree/abc/data/parser-rq-production-graph-v1.json"
python "$candidate_tree/abc/tools/parser_rq_campaign_site.py" preflight-site \
  --site-descriptor "$PARSER_RQ_SITE_DESCRIPTOR" --graph "$graph" \
  --evidence-tree-clean true

test "$(git -C "$candidate_tree" rev-parse HEAD)" = "$freeze_rev"
test -z "$(git -C "$candidate_tree" status --short)"
```

Immediately before candidate construction, repeat both revision and cleanliness assertions. State explicitly that changing either check or the tree requires rerunning the whole block.

- [ ] **Step 2: Name the residual authority honestly**

Add one sentence: the runbook mechanically guards an honest invocation, but a person can bypass it; no persisted token or qualification invariant is claimed. Do not add such a protocol.

- [ ] **Step 3: Confirm the edit did not widen campaign authority**

```bash
rg -n 'freeze_rev|parser-rq-production-wiring|resource-cgroup-smoke|preflight-site' \
  abc/docs/superpowers/plans/2026-07-17-parser-rq-admission-promotion.md
rg -n 'freeze token|freeze_token|preflight mode|--preflight|--dry-run' \
  abc/docs/superpowers/plans/2026-07-17-parser-rq-admission-promotion.md
```

The first command must show one coherent sequence. The second must show no new mechanism; prose that explicitly rejects such a mechanism is acceptable.

- [ ] **Step 4: Commit the runbook correction**

```bash
git add abc/docs/superpowers/plans/2026-07-17-parser-rq-admission-promotion.md
git commit -m 'docs(parser-rq): bind pre-freeze checks to one revision'
```

---

### Task 5: Verify the complete change, push it, and run the live boundary

**Files:**
- Verify only; modify no protocol or qualification artifact unless a named check proves direct drift.

- [ ] **Step 1: Review the entire range before claiming completion**

```bash
git diff --check HEAD~4..HEAD
git diff --stat HEAD~4..HEAD
git diff HEAD~4..HEAD -- \
  abc/tools/parser_rq_campaign_orchestrator.py \
  abc/tools/test_parser_rq_campaign_orchestrator.py \
  flake.nix \
  abc/data/parser-rq-ignored-regions-v1.json \
  ab-validator/reports/parser-ir/test_parser_rq_core_attempt_capture.py \
  abc/docs/superpowers/plans/2026-07-17-parser-rq-admission-promotion.md
```

Check specifically that `execute_graph` calls the extracted helper, the test does not replace authenticated executable/driver maps, and no production entry point gained a preflight branch.

- [ ] **Step 2: Run formatting, lint, and comment checks**

```bash
just python-quality
just nix-format-check
nix build ./abc#checks.x86_64-linux.clj-kondo -L
nix build ./abc#checks.x86_64-linux.clj-nix-focused-tests -L
scripts/comment-hygiene-check.sh
```

- [ ] **Step 3: Run the focused production-boundary checks**

```bash
nix build .#checks.x86_64-linux.parser-rq-production-wiring -L
nix build ./abc#checks.x86_64-linux.parser-rq-campaign-orchestrator -L
nix build ./ab-validator#checks.x86_64-linux.parser-rq-core-attempt-python-tests -L
nix build ./ab-validator#checks.x86_64-linux.parser-rq-predicate-hardening-capture-smoke -L
nix build ./ab-validator#checks.x86_64-linux.parser-rq-publication-pytest -L
```

- [ ] **Step 4: Run the full migration/governance gate**

```bash
just validate-migration
```

If this reports a descriptor whose content directly binds the taxonomy or root check, recapture only that named descriptor using its existing command. Do not regenerate unrelated evidence speculatively.

- [ ] **Step 5: Confirm historical and worktree integrity**

```bash
test "$(find abc/docs/reports/parser-rq/runs -type f -path '*/authorizations/*' | wc -l)" -ge 3
test -z "$(git status --short)"
git log --oneline --decorate -8
```

Do not alter the three historical failed authorizations.

- [ ] **Step 6: Rebase safely, rerun the decisive check, and push**

```bash
git fetch origin
git rebase origin/main
nix build .#checks.x86_64-linux.parser-rq-production-wiring -L
just validate-migration
git push origin main
```

- [ ] **Step 7: Run the host-controlled boundary at the exact pushed commit**

On `hinoki.hyakutake-barbel.ts.net`, create or update the detached candidate tree to the pushed commit, record `freeze_rev`, then run:

```bash
nix build "$candidate_tree#checks.x86_64-linux.parser-rq-production-wiring" -L
nix run "$candidate_tree/ab-validator#parser-rq-resource-cgroup-smoke"
: "${PARSER_RQ_SITE_DESCRIPTOR:?set the reviewed site-descriptor JSON path}"
graph="$candidate_tree/abc/data/parser-rq-production-graph-v1.json"
python "$candidate_tree/abc/tools/parser_rq_campaign_site.py" preflight-site \
  --site-descriptor "$PARSER_RQ_SITE_DESCRIPTOR" --graph "$graph" \
  --evidence-tree-clean true
test "$(git -C "$candidate_tree" rev-parse HEAD)" = "$freeze_rev"
test -z "$(git -C "$candidate_tree" status --short)"
```

Stop after these prerequisites. Candidate construction and authorization resume only through the original P5 Task 10 sequence.

## Self-Review Checklist

- [ ] Production and preflight call the same `_execute_operation` and `_assert_member_set` implementations.
- [ ] Both call `_execute_operation` with `paths.root / "detached-cwd"` as the operation working directory.
- [ ] The real candidate package is authenticated, and the returned executable/driver maps are not replaced.
- [ ] Missing adapter provenance fails before execution, and core argv contains the authenticated adapter.
- [ ] The bounded chain produces nested path-hashed reports and rejects an unexpected flat intruder by work identity.
- [ ] The production taxonomy is exact canonical JSON with unchanged rules and no duplicated JCS validator.
- [ ] Six portable members are closed exactly; resource measurement remains a separate live cgroup check.
- [ ] No qualification, admission, predicate, authorization, one-shot, or ADR-promotion semantics changed.
- [ ] No machine, filesystem, storage, or backup fact entered candidate or qualification identity.
- [ ] Portable composition, live cgroup capability, and runtime/site preflight are guarded against one detached revision, with human bypass acknowledged.
- [ ] All focused checks and `just validate-migration` pass after the final rebase.
