# Parser-RQ Portable Evidence Integrity Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Remove machine identity and backup topology from parser release qualification while retaining fail-closed readiness and single-store content authentication.

**Architecture:** Keep one untracked four-path runtime descriptor, one portable readiness receipt, one closed evidence-integrity receipt, and one separate operational retention declaration. Delete the committed site policy and two-domain replication protocol; measurement-specific capabilities remain owned by their instruments, while the production orchestrator fails closed until the retention declaration names an owner, backup procedure, and current restore test.

**Tech Stack:** Clojure 1.12, Python 3, JSON Schema 2020-12, EDN/JCS SHA-256 identities, pytest, Kaocha, Nix flakes, ADR evidence governance.

## Global Constraints

- Execute Tasks 1–7 on an isolated `parser-rq/portable-evidence-integrity`
  worktree branch created from the current local `main`. Preserve unrelated user
  changes, keep each task focused-green, and do not advance or push `main` until
  the complete sequence passes the final gates. Then fast-forward `main` once.
- Do not mint a real capture authorization, start a volatile lane, create an authoritative capture generation, append a compatibility row, or change ADR 0039/0040 status.
- Candidate identity, qualification identity, predicate identity, executable provenance, corpus membership, observation semantics, admission, and canonical-generation selection remain unchanged.
- Runtime paths never enter candidate, qualification, readiness, authorization, capture, evaluation, admission, or promotion identity.
- Keep no compatibility alias for `site-policy`, `replica`, `replication`, `host_policy_ref`, or `verify-replicas` in active parser-RQ code.
- Historical reports and superseded designs remain truthful history and are not rewritten merely because they mention hinoki or replication.
- Backup and restore are operational responsibilities. Parser-RQ code must not call or attest a backup system.
- The authoritative campaign remains operationally blocked until
  `abc/docs/reports/parser-rq-evidence-retention.json` names the owner,
  applicable backup procedure, and current restore-test record. This is
  deliberately an operator-owned stop. The production orchestrator reads the closed
  record and refuses a blocked or incomplete declaration; that declaration is
  not evidence that the external backup or restore itself occurred.
- Use TDD: every semantic deletion begins with a failing assertion against the current contract.
- Use `apply_patch` for source edits; use formatters only for mechanical formatting.

## Execution bootstrap

Before Task 1, use `superpowers:using-git-worktrees` and create the isolated
execution branch from the current local `main` (which contains this approved
plan):

```bash
main_worktree="$(pwd)"
execution_worktree="$(dirname "$main_worktree")/soranoha-parser-rq-portable"
git worktree add -b parser-rq/portable-evidence-integrity \
  "$execution_worktree" main
cd "$execution_worktree"
```

Tasks 2–6 rotate one multi-language contract and therefore are not promised to
pass the complete monorepo gate individually. Each must pass its named focused
checks. The branch is the atomic integration unit: do not push it and do not
move `main` until Task 7 proves the entire sequence.

---

### Task 1: Record the governance and operational handoff

**Files:**
- Create: `abc/docs/adr/0042-portable-parser-rq-evidence-integrity.md`
- Create: `abc/docs/reports/parser-rq-evidence-retention.md`
- Create: `abc/docs/reports/parser-rq-evidence-retention.json`
- Create: `abc/docs/superpowers/notes/2026-07-18-adr-evidence-component-root-followup.md`
- Modify: `abc/docs/superpowers/specs/2026-07-18-parser-rq-execution-readiness-design.md`
- Modify: `abc/docs/superpowers/plans/2026-07-18-parser-rq-execution-readiness.md`

**Interfaces:**
- Consumes: approved portable-evidence design and current ADR 0039/0040/0041 authority boundaries.
- Produces: ADR 0042 in `Proposed` state, one explicit operator-owned stop, and
  one bounded follow-up for the pre-existing ADR-capture root-resolution defect.

- [ ] **Step 1: Write ADR 0042 as a proposed protocol amendment**

Create the ADR with this exact header and decision boundary:

```markdown
# ADR 0042: Separate Parser-RQ Evidence Integrity from Storage Retention

Status: Proposed
Date: 2026-07-18
Depends on: ADR 0039 [scope: release qualification evidence integrity], ADR 0041 [scope: fixed parser release instruments]
Validation scope: structural
Release authority: development

## Implementation Status

The target contract is approved but not yet implemented. ADR 0039 and ADR 0040
remain Proposed, and no authoritative parser-RQ capture exists.

## Context

The execution-readiness protocol braided parser evidence authentication with a
named host, mount topology, and two-path replication. Reliable storage and
external backup already own retention; parser qualification must authenticate
referenced bytes without defining backup infrastructure.

## Decision

The target contract deletes committed site identity and application-level
replication. Runtime configuration supplies four paths. Readiness binds the
candidate, provenance, graph, corpus, and clean revisions without binding those
paths. Promotion requires exact closed membership and a streaming SHA-256 and
byte-count re-hash from one configured evidence store.

Evidence retention is owned by the repository operator through the operational
record at `docs/reports/parser-rq-evidence-retention.json` and its adjacent
runbook. Missing ownership, procedure, or restore evidence blocks production
execution but is not a parser qualification verdict or identity input.

This stop is enforced by the production orchestrator through a separate closed
operational record. The record does not prove an external restore; it makes the
named operator's authorization explicit and fail-closed without entering any
qualification identity. The orchestrator authenticates stored content, while
the named operator owns the external retention procedure and restore evidence.

## Consequences

The unprovisioned replica ceases to be a qualification blocker as a consequence,
not as the motivation. Predicate, provenance, corpus, admission, and canonical
generation semantics remain unchanged. Historical hinoki observations remain
historical facts.

## Acceptance Criteria

- **ADR-0042-C1 — structural-invariant:** Active parser-RQ contracts contain no
  machine identity, filesystem topology, site policy, or replication protocol.
- **ADR-0042-C2 — fixture-behavior:** One-store evidence verification rejects
  missing, escaping, truncated, extra, or hash-mismatched closed members.
- **ADR-0042-C3 — structural-invariant:** Authorization binds portable readiness,
  and promotion requires the self-authenticating closed evidence-integrity
  receipt while predicate, provenance, and admission contracts remain unchanged.
```

- [ ] **Step 2: Write the honest retention record**

Create `abc/docs/reports/parser-rq-evidence-retention.json` as the sole
operational status value, initially blocked and with no invented procedure or
successful restore claim:

```json
{"owner":"Repository operator","schema_version":"abc/parser-rq-evidence-retention/v1","status":"blocked"}
```

Create the adjacent `abc/docs/reports/parser-rq-evidence-retention.md` runbook:

```markdown
# Parser-RQ Evidence Retention Readiness

The portable parser-RQ implementation may merge, but the authoritative campaign
must not mint its sole authorization until all three records below are supplied:

- the reliable evidence-store service or operating procedure covering the
  configured `evidence_store_root`;
- the external backup procedure and accountable operator;
- the date and result of a successful restore test applicable to that store.

These records are operational prerequisites, not parser qualification evidence.
They must never be copied into candidate, readiness, authorization, capture,
evaluation, admission, or promotion identity.

The adjacent JSON record is the closed operational stop consumed by the
production orchestrator. It does not prove that a backup or restore occurred;
it makes the named operator's authorization explicit and fail-closed. No field
from it enters qualification identity or evidence.
```

- [ ] **Step 3: Record the component-root workaround as a bounded follow-up**

Create `abc/docs/superpowers/notes/2026-07-18-adr-evidence-component-root-followup.md`:

```markdown
# ADR evidence component-root resolution follow-up

The current all-descriptor recapture requires an excluded local `abc/abc -> .`
symlink because component-root profiles resolve one path relative to the
component and another relative to the workspace. The link is a temporary,
pre-existing workaround and is not part of parser-RQ evidence identity.

A later focused change must characterize the existing input-key projection,
make the capture tool resolve component-root and workspace-root without a
self-referential symlink, migrate affected descriptors atomically, and delete
the workaround from runbooks. This portability slice does not combine that
capture-tool correction with the storage-contract rotation.
```

- [ ] **Step 4: Mark the former site/replica design as superseded in scope**

Add a short notice immediately below each older document's title:

```markdown
> **Superseded in scope:** Host identity, filesystem topology, and two-domain
> replication are replaced by ADR 0042 and the portable-evidence-integrity
> design. Candidate provenance, one-shot authorization, and the fixed capture
> graph remain active.
```

Do not rewrite the historical rationale or commands in those documents.

- [ ] **Step 5: Run documentation and governance syntax checks**

Run:

```bash
git diff --check
nix develop ./abc --command abc/bin/kaocha --focus abc.tools.adr-test
```

Expected: no whitespace errors; ADR parsing tests pass; ADR 0042 remains Proposed and therefore needs no registered evidence yet.

- [ ] **Step 6: Commit**

```bash
git add abc/docs/adr/0042-portable-parser-rq-evidence-integrity.md \
  abc/docs/reports/parser-rq-evidence-retention.md \
  abc/docs/superpowers/notes/2026-07-18-adr-evidence-component-root-followup.md \
  abc/docs/superpowers/specs/2026-07-18-parser-rq-execution-readiness-design.md \
  abc/docs/superpowers/plans/2026-07-18-parser-rq-execution-readiness.md
git commit -m "docs(parser-rq): assign evidence retention ownership"
```

---

### Task 2: Rotate the closed JSON contracts atomically

**Files:**
- Delete: `abc/data/parser-rq-site-policy-v1.json`
- Delete: `abc/schemas/parser-rq-site-policy.schema.json`
- Delete: `abc/schemas/parser-rq-site-preflight.schema.json`
- Delete: `abc/schemas/parser-rq-replication-receipt.schema.json`
- Modify: `abc/schemas/parser-rq-site-descriptor.schema.json`
- Modify: `abc/schemas/parser-rq-readiness-receipt.schema.json`
- Modify: `abc/schemas/parser-rq-capture-authorization.schema.json`
- Create: `abc/schemas/parser-rq-evidence-integrity-receipt.schema.json`
- Modify: `abc/src/abc/tools/validate_design_bundle.clj`
- Modify: `abc/test/abc/tools/validate_design_bundle_test.clj`

**Interfaces:**
- Consumes: existing JSON Schema loader and `validate-json!` registry.
- Produces: descriptor v2, readiness v2, authorization v3, and evidence-integrity receipt v1. Preflight becomes check-only and has no persisted schema.

- [ ] **Step 1: Write failing closed-contract tests**

In `validate_design_bundle_test.clj`, replace site-policy/replication fixtures with these exact shapes and assert each schema rejects one removed field:

```clojure
(def portable-descriptor
  {"schema_id" "https://w3id.org/abc/schemas/parser-rq-site-descriptor.schema.json"
   "schema_version" "2.0.0"
   "corpus_root" "/runtime/corpus"
   "evidence_store_root" "/runtime/evidence"
   "scratch_root" "/runtime/scratch"
   "campaign_lock_path" "/runtime/parser-rq.lock"})

(def portable-readiness
  {"schema_id" "https://w3id.org/abc/schemas/parser-rq-readiness-receipt.schema.json"
   "schema_version" "2.0.0"
   "readiness_receipt_ref" hash-value
   "candidate_ref" hash-value
   "qualification_identity_ref" hash-value
   "provenance_core_ref" hash-value
   "production_graph_hash" hash-value
   "production_graph_version" "abc/parser-rq-production-graph/v1"
   "candidate_git_rev" git-value
   "candidate_tree_clean" true
   "evidence_base_git_rev" git-value
   "evidence_tree_clean" true
   "corpus_snapshot_hash" hash-value
   "corpus_list_hash" hash-value})

(testing "portable contracts reject removed places"
  (is (seq (schema/validation-errors descriptor-schema
                                     (assoc portable-descriptor
                                            "kernel_hostname" "hinoki"))))
  (is (seq (schema/validation-errors readiness-schema
                                     (assoc portable-readiness
                                            "site_facts" {})))))
```

Add an authorization-v3 assertion that `host_policy_ref` is rejected, and an integrity-receipt assertion with `status: verified`, one closed blob row, `rehash`, and `observed_bytes`.

- [ ] **Step 2: Run the focused test and verify red**

Run:

```bash
nix develop ./abc --command abc/bin/kaocha \
  --focus abc.tools.validate-design-bundle-test/parser-rq-portable-contracts
```

Expected: FAIL because v2/v3 schemas and the integrity schema do not exist and removed fields remain accepted or required.

- [ ] **Step 3: Replace the contracts without aliases**

Use these exact key sets:

```text
site descriptor v2:
  schema_id schema_version corpus_root evidence_store_root scratch_root campaign_lock_path

readiness receipt v2:
  schema_id schema_version readiness_receipt_ref candidate_ref
  qualification_identity_ref provenance_core_ref production_graph_hash
  production_graph_version candidate_git_rev candidate_tree_clean
  evidence_base_git_rev evidence_tree_clean corpus_snapshot_hash corpus_list_hash

capture authorization v3:
  schema_id schema_version authorization_ref authorization_ordinal candidate_ref
  qualification_identity_ref readiness_receipt_ref not_before_utc not_after_utc
  repetitions reduction

evidence-integrity receipt v1:
  schema_id schema_version receipt_ref candidate_ref capture_generation_ref status
  blobs | reason
```

For verified integrity rows require exactly `blob`, `rehash`, and `observed_bytes`. Keep the existing logical blob definition and locator traversal pattern. `verified` requires non-empty unique `blobs` and forbids `reason`; `unavailable` requires `reason` and forbids `blobs`.

Delete the old policy and replication files in the same patch. Do not leave deprecated schema IDs in `validate_design_bundle.clj`.

- [ ] **Step 4: Update the design-bundle schema registry**

Remove all reads and registrations for:

```text
schemas/parser-rq-site-policy.schema.json
schemas/parser-rq-site-preflight.schema.json
schemas/parser-rq-replication-receipt.schema.json
data/parser-rq-site-policy-v1.json
```

Register `schemas/parser-rq-evidence-integrity-receipt.schema.json`. Keep descriptor, readiness, and authorization under their existing paths with their rotated versions. No preflight document remains to register.

- [ ] **Step 5: Run focused and schema tests**

Run:

```bash
nix develop ./abc --command abc/bin/kaocha \
  --focus abc.tools.validate-design-bundle-test
nix build ./abc#checks.x86_64-linux.schema-contract-drift
```

Expected: all parser-RQ schema fixtures pass; design-bundle validation contains no deleted path.

- [ ] **Step 6: Commit**

```bash
git add abc/data abc/schemas abc/src/abc/tools/validate_design_bundle.clj \
  abc/test/abc/tools/validate_design_bundle_test.clj
git commit -m "refactor(parser-rq): remove site and replication contracts"
```

---

### Task 3: Reduce runtime readiness to four places and three volatile checks

**Files:**
- Modify: `abc/config/parser-rq-site.example.json`
- Modify: `abc/tools/parser_rq_campaign_site.py`
- Modify: `abc/tools/test_parser_rq_campaign_site.py`
- Modify: `abc/flake.nix`

**Interfaces:**
- Consumes: descriptor v2 and preflight/readiness v2 from Task 2.
- Produces:
  - `RuntimeFacts(clock_synchronized: bool, lock_available: bool)`
  - `authenticate_runtime(descriptor: dict[str, object], facts: RuntimeFacts, probe: Probe) -> None`
  - `preflight_runtime(graph, descriptor, facts, probe, *, evidence_tree_clean, independent_build_capability) -> None`
  - `seal_readiness(graph, descriptor, facts, probe, *, candidate_ref, qualification_identity_ref, provenance_core_ref, candidate_git_rev, candidate_tree_clean, evidence_base_git_rev, evidence_tree_clean, corpus_snapshot_hash, corpus_list_hash) -> dict[str, object]`
  - `recheck_readiness(receipt, descriptor, facts, probe) -> None`

- [ ] **Step 1: Replace topology tests with portability characterizations**

Delete `configured_policy`, `MountFact`, hostname/DNS mutations, and replica tests. Add:

```python
def descriptor(tmp_path: Path) -> dict[str, object]:
    return {
        "schema_id": "https://w3id.org/abc/schemas/parser-rq-site-descriptor.schema.json",
        "schema_version": "2.0.0",
        "corpus_root": str(tmp_path / "corpus"),
        "evidence_store_root": str(tmp_path / "evidence"),
        "scratch_root": str(tmp_path / "scratch"),
        "campaign_lock_path": str(tmp_path / "campaign.lock"),
    }


def test_runtime_authentication_is_place_portable(tmp_path: Path) -> None:
    for name in ("first", "second"):
        value = descriptor(tmp_path / name)
        for key in ("corpus_root", "evidence_store_root", "scratch_root"):
            Path(str(value[key])).mkdir(parents=True)
        site.authenticate_runtime(
            value,
            site.RuntimeFacts(clock_synchronized=True, lock_available=True),
            FakeProbe(),
        )


@pytest.mark.parametrize(
    ("facts", "message"),
    [
        (site.RuntimeFacts(False, True), "clock"),
        (site.RuntimeFacts(True, False), "lock"),
    ],
)
def test_runtime_authentication_rejects_failed_volatile_fact(
    tmp_path: Path, facts: object, message: str
) -> None:
    value = descriptor(tmp_path)
    for key in ("corpus_root", "evidence_store_root", "scratch_root"):
        Path(str(value[key])).mkdir(parents=True)
    with pytest.raises(site.SiteUnavailable, match=message):
        site.authenticate_runtime(value, facts, FakeProbe())
```

Add a probe-failure test and assert readiness contains neither descriptor hash, preflight ref, paths, hostname, nor `site_facts`.

- [ ] **Step 2: Run tests and verify red**

Run:

```bash
nix develop ./abc --command pytest -q abc/tools/test_parser_rq_campaign_site.py
```

Expected: FAIL because `RuntimeFacts`, `authenticate_runtime`, and the portable signatures do not exist.

- [ ] **Step 3: Delete mount and DNS discovery**

Remove `MountFact`, hostname resolution, interface enumeration, mount parsing, remote filesystem allowlists, policy hashing, `authenticate_site`, and all `--policy` CLI arguments. Keep the existing bounded `RealProbe.verify_writable` implementation.

Implement the narrow check:

```python
@dataclass(frozen=True)
class RuntimeFacts:
    clock_synchronized: bool
    lock_available: bool


def authenticate_runtime(
    descriptor: dict[str, object], facts: RuntimeFacts, probe: Probe
) -> None:
    if set(descriptor) != {
        "schema_id", "schema_version", "corpus_root", "evidence_store_root",
        "scratch_root", "campaign_lock_path",
    }:
        raise SiteUnavailable("runtime descriptor violates its closed contract")
    if (
        descriptor["schema_id"]
        != "https://w3id.org/abc/schemas/parser-rq-site-descriptor.schema.json"
        or descriptor["schema_version"] != "2.0.0"
    ):
        raise SiteUnavailable("runtime descriptor identity is unsupported")
    roots = [Path(str(descriptor[key])) for key in
             ("corpus_root", "evidence_store_root", "scratch_root")]
    lock_path = Path(str(descriptor["campaign_lock_path"]))
    if not all(path.is_absolute() for path in [*roots, lock_path]):
        raise SiteUnavailable("runtime descriptor paths must be absolute")
    try:
        corpus = roots[0].resolve(strict=True)
    except OSError as error:
        raise SiteUnavailable("corpus root is unavailable") from error
    if not corpus.is_dir():
        raise SiteUnavailable("corpus root is not a directory")
    if not facts.clock_synchronized:
        raise SiteUnavailable("local clock is not synchronized")
    if not facts.lock_available:
        raise SiteUnavailable("campaign lock is unavailable")
    probe.verify_writable(Path(str(descriptor["evidence_store_root"])).resolve())
    probe.verify_writable(Path(str(descriptor["scratch_root"])).resolve())
```

`preflight_runtime` authenticates the graph, requires a clean evidence tree and
passed independent-build capability, calls `authenticate_runtime`, and returns
`None`; it writes no report. `seal_readiness` independently calls
`authenticate_runtime` and emits only readiness-v2 keys. `recheck_readiness`
authenticates the receipt self-reference, calls `authenticate_runtime`, and
compares no runtime places.

Keep the public CLI subcommand names `preflight-site`, `seal-readiness`, and
`recheck-readiness` to avoid inventing a second naming migration. They dispatch
to `preflight_runtime`, `seal_readiness`, and `recheck_readiness` respectively;
none accepts `--policy`, and `preflight-site` has no `--out` option.

The production CLI constructs `RuntimeFacts` from the synchronized-clock query
and a real nonblocking lock probe. Only Python unit tests may inject facts; no
CLI option or environment variable accepts readiness booleans.

- [ ] **Step 4: Reduce the example configuration**

Replace it with:

```json
{
  "schema_id": "https://w3id.org/abc/schemas/parser-rq-site-descriptor.schema.json",
  "schema_version": "2.0.0",
  "corpus_root": "/runtime/parser-rq/corpus",
  "evidence_store_root": "/runtime/parser-rq/evidence",
  "scratch_root": "/runtime/parser-rq/scratch",
  "campaign_lock_path": "/runtime/parser-rq/campaign.lock"
}
```

- [ ] **Step 5: Run Python and Nix checks**

Run:

```bash
nix develop ./abc --command pytest -q abc/tools/test_parser_rq_campaign_site.py
just python-quality
nix build ./abc#checks.x86_64-linux.parser-rq-campaign-site
```

Expected: all tests pass and the tracked Python quality set remains complete.

- [ ] **Step 6: Commit**

```bash
git add abc/config/parser-rq-site.example.json abc/tools/parser_rq_campaign_site.py \
  abc/tools/test_parser_rq_campaign_site.py abc/flake.nix
git commit -m "refactor(parser-rq): make runtime readiness site independent"
```

---

### Task 4: Replace two-store replication with closed one-store evidence verification

**Files:**
- Modify: `ab-validator/reports/parser-ir/parser-rq-campaign-provenance.py`
- Modify: `ab-validator/reports/parser-ir/test_parser_rq_campaign_provenance.py`
- Modify: `ab-validator/flake.nix`

**Interfaces:**
- Consumes: `LogicalBlob`, `_safe_path`, `_stream_identity`, and the integrity schema from Task 2.
- Produces:
  - `verify_evidence(blobs: list[LogicalBlob], root: Path) -> dict[str, object]`
  - CLI `verify-evidence --blobs PATH --evidence-root PATH --candidate-ref HASH --capture-generation-ref HASH --out PATH`

- [ ] **Step 1: Write failing one-store properties**

Replace `test_replication_rehashes_both_failure_domains` with:

```python
def test_verify_evidence_authenticates_one_closed_store(tmp_path: Path) -> None:
    payload = b"immutable evidence"
    digest = module.sha256_bytes(payload)
    blob = module.LogicalBlob(digest, len(payload), "application/json", "aa/blob")
    (tmp_path / "aa").mkdir()
    (tmp_path / "aa/blob").write_bytes(payload)
    value = module.verify_evidence([blob], tmp_path)
    assert value == {
        "status": "verified",
        "blobs": [
            {"blob": blob._asdict(), "rehash": digest, "observed_bytes": len(payload)}
        ],
    }


def test_verify_evidence_fails_closed(tmp_path: Path) -> None:
    payload = b"immutable evidence"
    digest = module.sha256_bytes(payload)
    cases = [
        module.LogicalBlob(digest, len(payload), "application/json", "missing"),
        module.LogicalBlob(digest, len(payload), "application/json", "../escape"),
    ]
    for blob in cases:
        assert module.verify_evidence([blob], tmp_path)["status"] == "unavailable"
```

Retain and adapt the existing duplicate-membership and byte-mismatch assertions. Add a CLI test asserting `verify-replicas` is rejected and `verify-evidence` writes a self-authenticating receipt with candidate and capture references.

- [ ] **Step 2: Run the focused tests and verify red**

Run:

```bash
nix develop ./ab-validator --command pytest -q \
  ab-validator/reports/parser-ir/test_parser_rq_campaign_provenance.py \
  -k 'verify_evidence or replication'
```

Expected: FAIL because `verify_evidence` and its CLI do not exist.

- [ ] **Step 3: Implement one-store verification**

Replace `verify_replicas` with:

```python
def verify_evidence(blobs: list[LogicalBlob], root: Path) -> dict[str, object]:
    try:
        resolved_root = root.resolve(strict=True)
    except OSError:
        return {"status": "unavailable", "reason": "evidence root is offline"}
    if not blobs or len({blob.sha256 for blob in blobs}) != len(blobs):
        return {"status": "unavailable", "reason": "blob membership is empty or duplicated"}
    records: list[dict[str, object]] = []
    for blob in sorted(blobs, key=lambda value: value.sha256):
        if not blob.media_type or blob.bytes < 0:
            return {"status": "unavailable", "reason": "blob metadata is invalid"}
        path = _safe_path(resolved_root, blob.locator)
        if path is None:
            return {"status": "unavailable", "reason": "blob locator is absent or unsafe"}
        observed_hash, observed_bytes = _stream_identity(path)
        if observed_hash != blob.sha256 or observed_bytes != blob.bytes:
            return {"status": "unavailable", "reason": "evidence bytes do not authenticate"}
        records.append(
            {"blob": blob._asdict(), "rehash": observed_hash, "observed_bytes": observed_bytes}
        )
    return {"status": "verified", "blobs": records}
```

The CLI wraps that value with schema/version/candidate/capture fields, computes `receipt_ref` over all fields except itself, writes unavailable receipts before returning 2, and writes verified receipts before returning 0. Remove the `verify-replicas` parser entirely.

- [ ] **Step 4: Run focused and package checks**

Run:

```bash
nix develop ./ab-validator --command pytest -q \
  ab-validator/reports/parser-ir/test_parser_rq_campaign_provenance.py
nix build ./ab-validator#checks.x86_64-linux.parser-rq-campaign-provenance-python-tests
just python-quality
```

Expected: all provenance and repository Python checks pass; no two-store behavior remains.

- [ ] **Step 5: Commit**

```bash
git add ab-validator/reports/parser-ir/parser-rq-campaign-provenance.py \
  ab-validator/reports/parser-ir/test_parser_rq_campaign_provenance.py \
  ab-validator/flake.nix
git commit -m "refactor(parser-rq): verify evidence in one content store"
```

---

### Task 5: Rotate Clojure readiness, authorization, and promotion atomically

**Files:**
- Modify: `abc/src/abc/tools/parser_rq_campaign.clj`
- Modify: `abc/test/abc/tools/parser_rq_campaign_test.clj`
- Modify: `abc/test/abc/tools/parser_rq_admission_promotion_drift_test.clj`
- Modify: `abc/test/fixtures/parser-rq/admission-promotion/summary.edn`

**Interfaces:**
- Consumes: readiness v2, authorization v3, and integrity receipt v1.
- Produces:
  - `build-authorization [candidate receipt ordinal not-before not-after]`
  - `evidence-integrity-errors [receipt manifest-blobs] -> vector<string>`
  - `evidence-integrity-receipt-errors [receipt candidate-ref capture-ref manifest-blobs] -> vector<string>` authenticates schema/version, JCS self-reference, campaign bindings, and closed members in the same helper used by promotion.
  - promotion reads `evidence-integrity-receipt.json` through `read-json-value` from the capture generation.

- [ ] **Step 1: Write failing portable identity tests**

Change the shared readiness and authorization fixtures to the new closed keys and add:

```clojure
(deftest readiness-and-authorization-bind-no-runtime-place
  (is (empty? (campaign/verify-authorization-record
               candidate provenance graph receipt authorization)))
  (doseq [removed [:site_preflight_report_ref :site_facts :host_policy_ref]]
    (is (not (contains? receipt removed)))
    (is (not (contains? authorization removed)))))

(deftest evidence-integrity-authenticates-closed-manifest-membership
  (let [blob {:sha256 sha :bytes 10 :media_type "application/json"
              :locator "aa/blob"}
        verified {:status :verified
                  :blobs [{:blob blob :rehash sha :observed_bytes 10}]}]
    (is (= [] (campaign/evidence-integrity-errors verified [blob])))
    (is (seq (campaign/evidence-integrity-errors
              verified [(assoc blob :bytes 11)])))
    (is (seq (campaign/evidence-integrity-errors
              (assoc-in verified [:blobs 0 :rehash] sha-b) [blob])))))
```

Change the drift summary from `:replica_count 2` to `:evidence_integrity_receipt_count 1`; mutate it to `0` and `2` in the adversarial test.

Add a cross-language test that creates one real blob and closed blob-list JSON
under a temporary evidence root, invokes the real Python
`parser-rq-campaign-provenance.py verify-evidence` CLI, reads the emitted JSON
with the same Clojure JSON reader used by promotion, and passes that value to
`evidence-integrity-receipt-errors`. Assert the error vector is empty. Then
mutate `:receipt_ref` and assert the helper reports an invalid self-reference.
Resolve the workspace root by walking parents for the root `justfile`, `abc/`,
and `ab-validator/`, so the test works when Kaocha starts at either the
monorepo or component root.

- [ ] **Step 2: Run focused tests and verify red**

Run:

```bash
nix develop ./abc --command abc/bin/kaocha \
  --focus abc.tools.parser-rq-campaign-test \
  --focus abc.tools.parser-rq-admission-promotion-drift-test
```

Expected: FAIL because the old key sets, builder arity, replication function, and fixture are still active.

- [ ] **Step 3: Rotate readiness and authorization**

Set `readiness-receipt-keys` and `authorization-keys` to the Task 2 contracts. Require readiness schema version `2.0.0` and authorization version `3.0.0`. Delete the site-preflight SHA check. Change the builder to:

```clojure
(defn build-authorization [candidate receipt ordinal not-before not-after]
  (let [authorization
        {:schema_id "https://w3id.org/abc/schemas/parser-rq-capture-authorization.schema.json"
         :schema_version "3.0.0"
         :authorization_ordinal ordinal
         :candidate_ref (:candidate_ref candidate)
         :qualification_identity_ref (:qualification_identity_ref candidate)
         :readiness_receipt_ref (:readiness_receipt_ref receipt)
         :not_before_utc not-before
         :not_after_utc not-after
         :repetitions 3
         :reduction "maximum"}]
    (assoc authorization :authorization_ref (authorization-ref authorization))))
```

Do not add an optional sixth argument.

- [ ] **Step 4: Replace promotion's replication authority**

Rename `replication-errors` to `evidence-integrity-errors` and use:

```clojure
(defn evidence-integrity-errors [{:keys [status blobs] :as receipt} manifest-blobs]
  (let [receipt-blobs (mapv :blob blobs)
        identity #(select-keys % [:sha256 :bytes :media_type :locator])]
    (cond-> []
      (not= #{:schema_id :schema_version :receipt_ref :candidate_ref
              :capture_generation_ref :status :blobs}
            (set (keys receipt)))
      (conj "evidence integrity receipt violates its closed key contract")
      (not (named= :verified status))
      (conj "evidence integrity status is not verified")
      (or (empty? receipt-blobs)
          (not= (set (map identity manifest-blobs))
                (set (map identity receipt-blobs)))
          (not= (count manifest-blobs) (count receipt-blobs)))
      (conj "evidence integrity membership differs from capture manifests")
      (some (fn [{:keys [blob rehash observed_bytes] :as row}]
              (or (not= #{:blob :rehash :observed_bytes}
                         (set (keys row)))
                  (not= (:sha256 blob) rehash)
                  (not= (:bytes blob) observed_bytes)
                  (not (sha256? (:sha256 blob)))
                  (not (nat-int? (:bytes blob)))
                  (string/blank? (:media_type blob))))
            blobs)
      (conj "evidence integrity receipt does not authenticate stored bytes"))))
```

In `promotion-errors`, read `evidence-integrity-receipt.json` with
`read-json-value`, authenticate its schema identity/version, `receipt_ref`,
`candidate_ref`, and `capture_generation_ref`, and pass it to the renamed
function. Put those receipt-level checks in
`evidence-integrity-receipt-errors` and call that same helper from both the
cross-language test and promotion; do not create a second test-only verifier.
Delete every replication binding and error string.

- [ ] **Step 5: Run focused, Clojure, and drift checks**

Run:

```bash
nix develop ./abc --command abc/bin/kaocha \
  --focus abc.tools.parser-rq-campaign-test \
  --focus abc.tools.parser-rq-admission-promotion-drift-test
nix build ./abc#checks.x86_64-linux.clj-kondo
nix build ./abc#checks.x86_64-linux.parser-rq-admission-promotion-smoke
```

Expected: all pass; promotion still rejects stale registry, sibling capture, canonical drift, provenance drift, and non-qualified reports.
The focused campaign test must include the real Python-writes/Clojure-verifies
receipt path; adjacent monolingual suites are not sufficient.

- [ ] **Step 6: Commit**

```bash
git add abc/src/abc/tools/parser_rq_campaign.clj \
  abc/test/abc/tools/parser_rq_campaign_test.clj \
  abc/test/abc/tools/parser_rq_admission_promotion_drift_test.clj \
  abc/test/fixtures/parser-rq/admission-promotion/summary.edn
git commit -m "refactor(parser-rq): gate promotion on evidence integrity"
```

---

### Task 6: Simplify the orchestrator and make the deletion executable

**Files:**
- Create: `abc/docs/reports/parser-rq-evidence-retention.json`
- Modify: `abc/tools/parser_rq_campaign_orchestrator.py`
- Modify: `abc/tools/test_parser_rq_campaign_orchestrator.py`
- Modify: `abc/bin/parser-rq-campaign-capture.sh`
- Modify: `abc/docs/superpowers/plans/2026-07-17-parser-rq-admission-promotion.md`
- Create: `abc/test/abc/tools/parser_rq_portability_test.clj`
- Modify: `abc/flake.nix`
- Modify: `flake.nix`

**Interfaces:**
- Consumes: portable site CLI, authorization v3, `verify-evidence` CLI, and a
  separate closed operational retention declaration.
- Produces: one production command with no site-policy or replica argument, a
  fail-closed operational stop outside qualification identity, and a permanent
  active-surface regression guard.

- [ ] **Step 1: Write failing orchestrator tests**

Update `CampaignConfig` fixtures to contain `site_descriptor` but no `site_policy`. Add:

```python
def test_production_parser_has_no_site_policy_or_replica_option() -> None:
    parser = orchestrator._parser()
    option_strings = {
        option for action in parser._actions for option in action.option_strings
    }
    assert "--site-policy" not in option_strings
    assert not any("replica" in option for option in option_strings)


def test_prepare_commands_recheck_only_runtime_descriptor(campaign, paths) -> None:
    commands = orchestrator._prepare_commands(campaign, paths)
    recheck = next(command for command in commands if "recheck-readiness" in command)
    assert "--site-descriptor" in recheck
    assert "--policy" not in recheck


def test_production_fails_closed_on_blocked_retention(tmp_path: Path) -> None:
    config = fixture(tmp_path)
    write_json(config.retention_record, {
        "schema_version": "abc/parser-rq-evidence-retention/v1",
        "status": "blocked",
        "owner": "operator",
    })
    with pytest.raises(PreparationFailed, match="retention is blocked"):
        authenticate_inputs(config)
```

Extend the cwd-independent production-wiring test to invoke the real site CLI with descriptor v2.

- [ ] **Step 2: Run orchestrator tests and verify red**

Run:

```bash
nix build ./abc#checks.x86_64-linux.parser-rq-campaign-orchestrator \
  --print-build-logs
```

Expected: FAIL because `CampaignConfig`, authentication, CLI parsing, and prepared commands still require site policy.

- [ ] **Step 3: Delete the policy from orchestration**

Remove `site_policy` from `CampaignConfig` and `AuthenticatedCampaign`. Remove
the candidate-tree policy read, hash check, configured-replica gate, and
`--site-policy`. Keep descriptor validation as untracked runtime configuration.
Add `retention_record` as a production-only operational input. Require the
closed v1 ready shape with a nonblank owner and procedure, a `passed` restore
result, and a UTC restore-test instant. This record must never enter a
qualification identity or evidence receipt.

The readiness recheck command must be exactly:

```python
(
    sys.executable,
    _driver(campaign, "abc/tools/parser_rq_campaign_site.py"),
    "recheck-readiness",
    "--site-descriptor",
    str(config.site_descriptor),
    "--receipt",
    str(config.readiness_receipt),
)
```

Do not add a storage provider or injectable site-policy object.

- [ ] **Step 4: Rewrite the P5 runbook commands**

In the P5 plan:

- remove site-policy generation/configuration and all replica provisioning steps;
- keep `$PARSER_RQ_SITE_DESCRIPTOR` and require descriptor v2;
- replace `primary_store_root`/`replica_store_root` with `evidence_store_root`;
- build authorization without `--host-policy`;
- invoke `verify-evidence` with the closed blob list, one evidence root, candidate ref, and capture-generation ref;
- install `evidence-integrity-receipt.json` in the capture generation;
- require the evidence-retention record to be unblocked before candidate freeze;
- keep independent builds, provenance, fixed graph, one-shot authorization, capture, evaluation, admission, and ADR transition commands unchanged.

- [ ] **Step 5: Add the permanent active-surface guard**

Create a focused Clojure test that fails closed over active parser-RQ source
trees. Resolve the repository root by walking parents until the directory
contains the root `justfile`, `abc/`, and `ab-validator/`. Recursively scan
regular text files under these reviewed active roots:

```clojure
["abc/src"
 "abc/tools"
 "abc/bin"
 "abc/schemas"
 "abc/data"
 "abc/config"
 "abc/test"
 "ab-validator/reports/parser-ir"]
```

Also scan the individual active files `flake.nix`, `abc/flake.nix`,
`ab-validator/flake.nix`, and
`abc/docs/superpowers/plans/2026-07-17-parser-rq-admission-promotion.md`.
Select known text extensions plus extensionless scripts; reject unreadable
selected files rather than skipping them. Exclude exactly this guard's own file
because it constructs the forbidden strings. Historical reports, ADRs, and
superseded designs are outside the active roots and remain truthful history.

The test shape is:

```clojure
(ns abc.tools.parser-rq-portability-test
  (:require [babashka.fs :as fs]
            [clojure.string :as string]
            [clojure.test :refer [deftest is]]))

(def active-roots
  ["abc/src" "abc/tools" "abc/bin" "abc/schemas" "abc/data"
   "abc/config" "abc/test" "ab-validator/reports/parser-ir"])

(def active-files
  ["flake.nix" "abc/flake.nix" "ab-validator/flake.nix"
   "abc/docs/superpowers/plans/2026-07-17-parser-rq-admission-promotion.md"])

(def forbidden
  [(str "host" "_policy_ref")
   (str "site" "-policy")
   (str "site" "_policy")
   (str "rep" "lica")
   (str "verify" "-replicas")
   (str "replication" "-receipt")
   (str "replica" "_failure_domain")
   (str "stable" "_host_label")
   (str "kernel" "_hostname")
   (str "remote" "_authority")
   (str "mount" "_class")])

(deftest active-parser-rq-surface-has-no-site-or-backup-policy
  (doseq [path (scanned-active-files (repo-root))
          token forbidden]
    (is (not (string/includes? (slurp path) token))
        (str path " contains removed token " token))))
```

Implement `repo-root` and `scanned-active-files` as small deterministic helpers,
sort the returned canonical paths, and add assertions that the set contains at
least these formerly dense surfaces:

```clojure
#{"ab-validator/reports/parser-ir/parser-rq-campaign-provenance.py"
  "ab-validator/reports/parser-ir/test_parser_rq_campaign_provenance.py"
  "abc/tools/test_parser_rq_campaign_site.py"
  "abc/tools/test_parser_rq_campaign_orchestrator.py"
  "abc/config/parser-rq-site.example.json"
  "abc/flake.nix"
  "ab-validator/flake.nix"}
```

This is a denylist sweep over what exists, not an allowlist of nine remembered
files. Do not scan historical reports, prior designs, ADR 0042, or the guard
file itself.

- [ ] **Step 6: Register and run integration checks**

Add the portability namespace to `clj-nix-focused-tests` and keep both parser-RQ Python files in `python-quality`. Update the root production-wiring check to the new real CLI arguments.

Run:

```bash
nix develop ./abc --command pytest -q \
  abc/tools/test_parser_rq_campaign_site.py \
  abc/tools/test_parser_rq_campaign_orchestrator.py
nix develop ./abc --command abc/bin/kaocha \
  --focus abc.tools.parser-rq-portability-test
nix build .#checks.x86_64-linux.parser-rq-production-wiring
nix build ./abc#checks.x86_64-linux.parser-rq-campaign-orchestrator
nix build ./abc#checks.x86_64-linux.parser-rq-admission-promotion-smoke
just python-quality
just nix-format-check
scripts/comment-hygiene-check.sh
```

Expected: all pass; the production-wiring check retains its unrelated-working-
directory assertion, the portability guard works from monorepo or `abc/` root,
and no active site or replica token remains.

- [ ] **Step 7: Commit**

```bash
git add abc/tools abc/bin/parser-rq-campaign-capture.sh \
  abc/test/abc/tools/parser_rq_portability_test.clj abc/flake.nix flake.nix \
  abc/docs/superpowers/plans/2026-07-17-parser-rq-admission-promotion.md
git commit -m "refactor(parser-rq): remove site policy from execution"
```

---

### Task 7: Prove the governed transition and accept ADR 0042

**Files:**
- Create: `abc/docs/evidence/parser-rq-portable-integrity/capture/parser-rq-portable-integrity.edn`
- Create: `abc/docs/evidence/adr-entries/parser-rq-portable-integrity.edn`
- Create: `abc/docs/evidence/adr-runs/parser-rq-portable-integrity.json` through the capture tool
- Modify: `abc/docs/adr/0042-portable-parser-rq-evidence-integrity.md`
- Modify: `abc/docs/adr/adr-evidence.edn` through the registrar
- Modify: governed bundles under `abc/docs/evidence/adr-runs/` through clean-tree recapture

**Interfaces:**
- Consumes: all focused checks from Tasks 2–6 and the ADR evidence capture/register protocol.
- Produces: accepted ADR 0042 with registered bounded evidence and a green monorepo governance closure.

- [ ] **Step 1: Add the focused evidence descriptor and registration template**

Use one closed capture-v1 command so the registered observation executes both
the Clojure authority checks and the real Python verifier check:

```clojure
{:schema-version "abc-adr-evidence-capture-v1"
 :tool "bash"
 :argv ["bash" "--noprofile" "--norc" "-c"
        "bin/kaocha --focus abc.tools.parser-rq-portability-test --focus abc.tools.parser-rq-campaign-test --focus abc.tools.parser-rq-admission-promotion-drift-test && nix build ../ab-validator#checks.x86_64-linux.parser-rq-campaign-provenance-python-tests"]
 :input-profile
 {:kind "component-clojure-test-v1"
  :component-root "abc"
  :roots ["abc.tools.parser-rq-portability-test"
          "abc.tools.parser-rq-campaign-test"
          "abc.tools.parser-rq-admission-promotion-drift-test"]
  :explicit ["abc/bin/kaocha"
             "abc/docs/evidence/parser-rq-portable-integrity/capture/parser-rq-portable-integrity.edn"
             "ab-validator/flake.lock"
             "ab-validator/flake.nix"
             "ab-validator/reports/parser-ir/parser-rq-campaign-provenance.py"
             "ab-validator/reports/parser-ir/test_parser_rq_campaign_provenance.py"]}
 :observation-key "parser-rq-portable-integrity-pass"}
```

The registration template contains ADR-0042-C1, C2, and C3, all pointing to `docs/evidence/adr-runs/parser-rq-portable-integrity.json` and observation key `parser-rq-portable-integrity-pass`, with the claim/evidence kinds from ADR 0042.

- [ ] **Step 2: Commit the complete implementation checkpoint while ADR 0042 is Proposed**

Run:

```bash
just python-quality
just nix-format-check
nix build ./abc#checks.x86_64-linux.clj-kondo
nix build ./abc#checks.x86_64-linux.clj-nix-focused-tests
nix build ./ab-validator#checks.x86_64-linux.parser-rq-campaign-provenance-python-tests
nix build .#checks.x86_64-linux.parser-rq-production-wiring
git diff --check
```

Expected: all pass. Then commit the descriptor/template before capture so the capture reads a clean revision:

```bash
git add abc/docs/evidence/parser-rq-portable-integrity \
  abc/docs/evidence/adr-entries/parser-rq-portable-integrity.edn
git commit -m "test(parser-rq): define portable integrity evidence"
```

- [ ] **Step 3: Capture the focused ADR 0042 bundle from a clean tree**

Run from `abc/`:

```bash
stage="$(mktemp -d -t parser-rq-portable-evidence.XXXXXXXX)"
mkdir -p "$stage/capture"
nix develop . --command clojure -M:abc/adr-evidence-capture \
  --descriptor docs/evidence/parser-rq-portable-integrity/capture/parser-rq-portable-integrity.edn \
  --output "$stage/capture/parser-rq-portable-integrity.json" \
  --staging-root "$stage/capture" \
  --repo-root . \
  --workspace-root ..
jq -e '.observations["parser-rq-portable-integrity-pass"].value == true' \
  "$stage/capture/parser-rq-portable-integrity.json"
```

Expected: capture exits 0 and jq prints `true`.

- [ ] **Step 4: Install the focused run and accept ADR 0042**

Install the generated file:

```bash
cp "$stage/capture/parser-rq-portable-integrity.json" \
  docs/evidence/adr-runs/parser-rq-portable-integrity.json
```

Change ADR 0042 to:

```text
Status: Accepted
Date: 2026-07-18
Accepted: 2026-07-18
```

Update Implementation Status to state that portable contracts, one-store verification, promotion integration, the active-surface guard, and bounded evidence are implemented. State separately that the authoritative campaign is still blocked by the unfilled evidence-retention record.

Do not run the registrar yet: Proposed ADR criteria are intentionally absent
from the registrable accepted-ADR inventory, and the changed governed tree has
made existing bundles stale. Commit the accepted ADR and focused run so the
complete recapture in Step 5 reads one clean revision.

- [ ] **Step 5: Recapture governance evidence after the accepted ADR changes the governed tree**

Commit the ADR 0042 run, registry, and status first, then use the repository's complete descriptor set from a clean tree:

```bash
git add docs/adr/0042-portable-parser-rq-evidence-integrity.md \
  docs/evidence/adr-runs/parser-rq-portable-integrity.json
git commit -m "docs(adr): accept portable parser rq evidence integrity"
```

Run from `abc/`. The component-root capture profiles currently require an
excluded local `abc -> .` link; create it only for recapture and remove it in the
trap:

```bash
stage="$(mktemp -d -t parser-rq-governance.XXXXXXXX)"
mkdir -p "$stage/runs"
grep -qxF 'abc/abc' ../.git/info/exclude || echo 'abc/abc' >> ../.git/info/exclude
ln -s . abc
trap 'rm -f abc' EXIT

{
  find docs/evidence/adr-capture -maxdepth 1 -type f -name '*.edn'
  find docs/evidence -mindepth 3 -type f -path '*/capture/*.edn'
} | sort -u > "$stage/descriptors.txt"

while IFS= read -r descriptor; do
  name="$(basename "$descriptor" .edn)"
  nix develop . --command clojure -M:abc/adr-evidence-capture \
    --descriptor "$descriptor" \
    --output "$stage/runs/$name.json" \
    --staging-root "$stage/runs" \
    --repo-root . \
    --workspace-root ..
done < "$stage/descriptors.txt"

jq -s -e 'all(.[]; all(.observations[]; .value == true))' "$stage"/runs/*.json
cp "$stage"/runs/*.json docs/evidence/adr-runs/

nix develop . --command clojure -M -e '
(require (quote [abc.tools.files :as files])
         (quote [babashka.fs :as fs]))
(let [paths (sort (map str (fs/glob "docs/evidence/adr-entries" "*.edn")))
      templates (map files/read-edn paths)
      entries (vec (mapcat :entries templates))]
  (spit "docs/evidence/adr-entries/.all-generated.edn"
        (str (pr-str {:schema-version :abc-adr-evidence-registration-v1
                      :entries entries}) "\n")))'

nix develop . --command clojure -M:abc/adr-evidence-register \
  --entries docs/evidence/adr-entries/.all-generated.edn \
  --registry docs/adr/adr-evidence.edn \
  --workspace-root ..
rm docs/evidence/adr-entries/.all-generated.edn
rm abc
trap - EXIT
```

Expected: every descriptor exits 0, every observation value is true, and the
registrar updates all owned claims in one candidate registry rather than
sequentially replacing shared claim IDs.

- [ ] **Step 6: Run final gates**

Run from the monorepo root:

```bash
git diff --check
just python-quality
just nix-format-check
scripts/comment-hygiene-check.sh
nix build .#checks.x86_64-linux.parser-rq-production-wiring
nix build .#checks.x86_64-linux.monorepo-adr-governance --print-build-logs
just validate-migration
```

Expected: every command exits 0. Confirm the operational JSON record remains
`"status":"blocked"`; passing qualification checks must not imply backup readiness.

- [ ] **Step 7: Commit recaptured governance evidence, fast-forward main, and push**

```bash
git add abc/docs/adr/adr-evidence.edn abc/docs/evidence/adr-runs
git commit -m "fix(abc): recapture portable parser rq governance evidence"
git status --short
test -z "$(git status --short)"

execution_head="$(git rev-parse HEAD)"
git -C "$main_worktree" merge --ff-only parser-rq/portable-evidence-integrity
test "$(git -C "$main_worktree" rev-parse HEAD)" = "$execution_head"
git -C "$main_worktree" push origin main
test "$(git -C "$main_worktree" rev-parse HEAD)" = \
  "$(git -C "$main_worktree" rev-parse origin/main)"
```

Expected: the execution worktree is clean, `main` fast-forwards once across the
fully verified sequence, and local `main` equals `origin/main`. Intermediate
contract-rotation commits were never exposed on `main` or the remote.

---

## Final Self-Review Checklist

- [ ] The active-surface guard finds no host policy, replication protocol, hostname requirement, remote authority, or mount class.
- [ ] Predicate, provenance, corpus, admission, and generation-selection semantic diffs are empty.
- [ ] Candidate and qualification identity tests pass with different runtime descriptors.
- [ ] Evidence integrity authenticates exact closed membership and one streamed copy; it claims no durability or backup result.
- [ ] The P5 runbook contains no real authorization value and no executed authoritative capture.
- [ ] The retention record remains honestly blocked until the operator supplies real procedure and restore-test facts.
- [ ] Historical hinoki reports and superseded design rationale remain unchanged.
- [ ] ADR 0039 and ADR 0040 remain Proposed; ADR 0041 remains Accepted; ADR 0042 is accepted only after registered evidence passes.
