# Parser Release Qualification Execution Readiness Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make the P5 parser-release campaign an executable, fail-closed production transaction without authorizing or running the authoritative capture.

**Architecture:** Deepen three boundaries before candidate freeze: a Python provenance tool independently realizes and authenticates candidate executables; a Python site tool authenticates runtime places against committed policy and seals readiness values; and one Python orchestrator owns the lock and fixed lane graph while Clojure remains the authority for campaign identities, authorization, composition, and promotion. The checked-in policy starts with the external replica explicitly unconfigured, so implementation can finish and validate without accidentally declaring hinoki campaign-ready.

**Tech Stack:** Python 3.12/pytest/ruff/mypy, Clojure/Malli/Kaocha, JSON Schema 2020-12, Bash, Nix 2.34 fresh local stores, JCS SHA-256, Git.

## Global Constraints

- Follow `abc/docs/superpowers/specs/2026-07-18-parser-rq-execution-readiness-design.md` exactly.
- Work on `main`; make reviewable commits and preserve unrelated user changes.
- Do not mint an authorization, execute a volatile predicate lane, create a real capture generation, append a registry row, or change ADR 0039/0040 status while implementing this plan.
- Do not change the nine predicates, their thresholds or observed keys, the qualification corpus, the admission relation, or registry content.
- One production orchestrator owns lane membership, order, lock lifetime, and capture-start time. Production callers cannot supply commands through arguments or environment variables.
- The core producer accepts an inherited locked descriptor and must not reacquire the campaign lock.
- Independent realization means two initially empty, distinct local stores, target output absent before each build, dependency seeding without the target output, and an offline local build whose log names the target derivation.
- The committed site policy remains `replica_status: unconfigured` until a real remote failure domain is provisioned and separately reviewed.
- A local disk, loop device, bind mount, local filesystem, or mount authority resolving to a hinoki address cannot satisfy replica independence.
- Site-preflight staging reports are disposable. Only a final readiness receipt is committed with a candidate, and authorization v2 binds that receipt.
- Structural authorization verification never consults the clock. Temporal verification requires an explicit captured kernel-realtime instant and synchronized-clock state.
- All candidate and policy inputs resolve from the detached candidate tree. Evidence Git operations use an explicit evidence-tree root; neither depends on the current directory.
- Run comment hygiene and every language check required by `/home/bor/Projects/soranoha/AGENTS.md`.
- Run `just validate-migration` without caller-local `NIX_CONFIG`; the root recipe owns Nix evaluation policy.

## File Structure

- `ab-validator/reports/parser-ir/parser-rq-campaign-provenance.py` owns fresh-store realization, build-record capture, reproducibility comparison, binding, executable resolution, and replica byte re-hashing. It does not decide qualification or promotion.
- `abc/tools/parser_rq_campaign_site.py` owns host/mount/clock/lock probes and canonical site-preflight/readiness values. Policy supplies identities; the untracked descriptor supplies places.
- `abc/tools/parser_rq_campaign_orchestrator.py` owns the serial production graph, inherited lock capability, pre-start volatile rechecks, and terminal lifecycle.
- `abc/bin/parser-rq-campaign-capture.sh` becomes a location-independent thin launcher and contains no lane selection.
- `abc/src/abc/tools/parser_rq_campaign.clj` remains the sole owner of candidate, authorization, capture, evaluation, and promotion identity rules.
- `abc/schemas/parser-rq-{build-record,site-policy,site-descriptor,site-preflight,readiness-receipt}.schema.json` close the new value protocols.
- `abc/schemas/parser-rq-{executable-provenance,capture-authorization}.schema.json` rotate atomically with their verifiers and bounded fixtures.
- `abc/data/parser-rq-production-graph-v1.json` is the closed, hashed lane graph and argv-template source of truth.
- `abc/data/parser-rq-site-policy-v1.json` initially records the approved host and an unconfigured replica.
- `abc/config/parser-rq-site.example.json` documents runtime places without hardcoding machine-local paths in active code.
- `abc/test/fixtures/parser-rq/execution-readiness/` contains bounded build, site, receipt, authorization, and graph values; no corpus-scale or volatile evidence belongs there.

---

### Task 1: Close the Execution-Readiness Value Contracts

**Files:**
- Create: `abc/schemas/parser-rq-build-record.schema.json`
- Create: `abc/schemas/parser-rq-site-policy.schema.json`
- Create: `abc/schemas/parser-rq-site-descriptor.schema.json`
- Create: `abc/schemas/parser-rq-site-preflight.schema.json`
- Create: `abc/schemas/parser-rq-readiness-receipt.schema.json`
- Modify: `abc/schemas/parser-rq-executable-provenance.schema.json`
- Modify: `abc/schemas/parser-rq-capture-authorization.schema.json`
- Create: `abc/data/parser-rq-site-policy-v1.json`
- Create: `abc/data/parser-rq-production-graph-v1.json`
- Create: `abc/config/parser-rq-site.example.json`
- Modify: `abc/src/abc/tools/validate_design_bundle.clj`
- Modify: `abc/test/abc/tools/validate_design_bundle_test.clj`

**Interfaces:**
- Produces schema IDs ending in `parser-rq-*/...schema.json`, with all objects closed by `additionalProperties: false`.
- Production graph version: `abc/parser-rq-production-graph/v1`.
- Site policy version: `abc/parser-rq-site-policy/v1`, initially `replica_status: "unconfigured"`.
- Authorization schema version: `2.0.0`, with required `readiness_receipt_ref`.
- Executable provenance version: `2.0.0`, with exactly `build-a` and `build-b`, a `provenance_core_ref`, and final candidate bindings.

- [ ] **Step 1: Add failing schema-contract tests**

Add minimal valid values and mutation cases to `validate_design_bundle_test.clj`. Pin these load-bearing cases explicitly:

```clojure
(testing "authorization v2 requires the sealed receipt"
  (is (nil? (schema/validation-errors authorization-schema authorization-v2)))
  (is (seq (schema/validation-errors authorization-schema
                                     (dissoc authorization-v2 "readiness_receipt_ref")))))

(testing "unconfigured replica policy cannot masquerade as configured"
  (is (nil? (schema/validation-errors site-policy-schema unconfigured-policy)))
  (is (seq (schema/validation-errors
            site-policy-schema
            (assoc unconfigured-policy "replica_status" "configured")))))

(testing "runtime places cannot introduce policy identity"
  (is (seq (schema/validation-errors site-descriptor-schema
                                     (assoc descriptor "replica_mount_class" "nfs")))))
```

Also reject unknown fields, absolute-path fields in committed policy, missing graph hashes, duplicate executable names, fewer or more than two build rows, candidate fields in an unbound proof, absent site-fact projections, and authorization receipt hashes with the wrong shape.

- [ ] **Step 2: Run the focused test and confirm red**

```bash
nix develop ./abc --command abc/bin/kaocha --focus abc.tools.validate-design-bundle-test
```

Expected: FAIL because the five new schemas and registered policy values do not exist, and authorization v1 does not require a receipt.

- [ ] **Step 3: Implement the schemas and initial policies**

Use tagged `if/then` branches in the site-policy schema:

```json
{
  "if": {"properties": {"replica_status": {"const": "configured"}}},
  "then": {"required": ["replica_failure_domain", "replica_mount_class", "replica_authority"]},
  "else": {
    "not": {"anyOf": [
      {"required": ["replica_failure_domain"]},
      {"required": ["replica_mount_class"]},
      {"required": ["replica_authority"]}
    ]}
  }
}
```

The committed initial policy must contain:

```json
{
  "schema_id": "https://w3id.org/abc/schemas/parser-rq-site-policy.schema.json",
  "schema_version": "1.0.0",
  "stable_host_label": "hinoki.hyakutake-barbel.ts.net",
  "kernel_hostname": "hinoki",
  "production_graph_version": "abc/parser-rq-production-graph/v1",
  "primary_failure_domain": "hinoki-primary",
  "replica_status": "unconfigured",
  "remote_filesystem_allowlist": ["nfs", "nfs4", "cifs", "ceph", "fuse.sshfs"]
}
```

The graph policy names exactly seven installed members in order and keeps executable names and argv templates separate from runtime places. Generate its `policy_hash` from the object without that field; do not type the digest.

- [ ] **Step 4: Register all new schemas and policy values**

Add all five schema paths and both committed JSON values to `validate_design_bundle.clj`. Add coherence checks that graph member names equal the existing campaign member set and that unconfigured policy has no remote identity fields.

- [ ] **Step 5: Verify and commit**

```bash
nix develop ./abc --command abc/bin/kaocha --focus abc.tools.validate-design-bundle-test
just nix-format-check
git add abc/schemas/parser-rq-*.schema.json abc/data/parser-rq-site-policy-v1.json \
  abc/data/parser-rq-production-graph-v1.json abc/config/parser-rq-site.example.json \
  abc/src/abc/tools/validate_design_bundle.clj abc/test/abc/tools/validate_design_bundle_test.clj
git commit -m "feat(parser-rq): define execution readiness contracts"
```

Expected: focused tests and Nix formatting pass; no predicate or registry file appears in the commit.

### Task 2: Make Independent Nix Realization a Production Provenance Command

**Files:**
- Modify: `ab-validator/reports/parser-ir/parser-rq-campaign-provenance.py`
- Modify: `ab-validator/reports/parser-ir/test_parser_rq_campaign_provenance.py`
- Modify: `abc/flake.nix`

**Interfaces:**

```python
def direct_seed_paths(derivation: dict[str, object]) -> tuple[str, ...]: ...
def realize_target(request: RealizeRequest, runner: Runner) -> dict[str, object]: ...
def capture_build(store_uri: str, build_json: Path, graph: dict[str, object]) -> dict[str, object]: ...
def compare_builds(first: dict[str, object], second: dict[str, object]) -> dict[str, object]: ...
def bind_provenance(proof: dict[str, object], candidate_ref: str,
                    qualification_identity_ref: str) -> dict[str, object]: ...
def main(argv: list[str] | None = None) -> int: ...
```

CLI subcommands: `realize-build`, `capture-build`, `compare-builds`, `bind-provenance`, `resolve-executable`, and `verify-replicas`. Every writing command requires `--out` and uses same-directory temporary file + `os.replace`.

- [ ] **Step 1: Add failing CLI and derivation-projection tests**

Tests must use a fake command runner and temporary store roots. Assert the exact load-bearing command:

```python
assert build_call == [
    "nix", "build", "--store", store_uri,
    "--offline", "--no-link", "--json", f"{drv_path}^out",
]
```

Assert the sequence rejects a pre-existing store root, target present after seeding, missing direct input output, an unavailable fixed-output/network input, build logs lacking `building '<drv>'`, raw `nix build --json` passed directly to `compare-builds`, duplicate executable names, graph-template disagreement, and two records naming the same store URI or build ID. Test `--help` for every subcommand and verify failed output is never installed at `--out`.

- [ ] **Step 2: Confirm red**

```bash
nix develop ./abc --command pytest -q \
  ab-validator/reports/parser-ir/test_parser_rq_campaign_provenance.py
```

Expected: FAIL because the module has no CLI, store-aware build record, atomic writer, or binder.

- [ ] **Step 3: Implement derivation seeding and offline realization**

Parse `nix derivation show` JSON rather than grepping it. Seed exactly the target `.drv`, direct `inputSrcs`, and each named output closure from direct `inputDrvs`. Before the build, require:

```python
if runner.path_info(store_uri, out_path):
    raise ProvenanceUnavailable("target output was present before local realization")
```

After `nix copy --no-check-sigs --to STORE ...`, repeat the absence check. Run the offline command with `LC_ALL=C`, authenticate target presence afterward, and require the build log to contain the target derivation build line. Record the initially-empty check, absent-target checks, sorted seed paths, store URI, build ID, build-log logical identity, target output, and NAR identity.

The concrete `drv^out` build has no evaluation phase, so do not pass
`--eval-store`. Require all direct source inputs and named direct-input output
closures to exist in the daemon store before seeding. A missing fixed-output or
network-fetched input is a preflight stop, never permission for the offline
fresh store to fetch it.

- [ ] **Step 4: Implement build capture, comparison, binding, and executable resolution**

`capture-build` must read executable bytes through the explicitly named store using `nix store cat --store STORE OUTPUT/bin/NAME`; it must not concatenate `/nix/store` onto a host path. Cross-check the graph policy's closed executable set and argv templates. `compare-builds` emits an unbound proof; `bind-provenance` adds only schema/candidate/qualification fields and verifies the core hash remains equal.

- [ ] **Step 5: Add the Nix check and verify**

Add a `parser-rq-campaign-provenance` check beside `parser-rq-core-attempt` in `abc/flake.nix`, with Python, pytest, and required Nix CLI test dependencies.

```bash
nix build ./abc#checks.x86_64-linux.parser-rq-campaign-provenance
just python-quality
git add ab-validator/reports/parser-ir/parser-rq-campaign-provenance.py \
  ab-validator/reports/parser-ir/test_parser_rq_campaign_provenance.py abc/flake.nix
git commit -m "feat(parser-rq): realize reproducible candidate builds"
```

### Task 3: Authenticate Site Policy, Places, and Volatile Readiness

**Files:**
- Create: `abc/tools/parser_rq_campaign_site.py`
- Create: `abc/tools/test_parser_rq_campaign_site.py`
- Modify: `abc/flake.nix`

**Interfaces:**

```python
@dataclass(frozen=True)
class SiteFacts:
    kernel_hostname: str
    stable_addresses: tuple[str, ...]
    local_addresses: tuple[str, ...]
    primary_mount: MountFact
    replica_mount: MountFact
    clock_synchronized: bool
    lock_available: bool

def authenticate_site(policy: dict[str, object], descriptor: dict[str, object],
                      facts: SiteFacts, probe: Probe) -> dict[str, object]: ...
def preflight_site(...) -> dict[str, object]: ...
def seal_readiness(...) -> dict[str, object]: ...
def recheck_readiness(...) -> None: ...
```

CLI subcommands: `preflight-site`, `seal-readiness`, and `recheck-readiness`.

- [ ] **Step 1: Add failing pure host and mount-policy tests**

Use injected facts; never consult live DNS or mounts in unit tests. Pin passing short-hostname plus local stable-address behavior. Reject wrong short hostname, stable DNS with no local address, same resolved root, same mount source, same fsid, local block source, loop source, bind mount, local filesystem type, remote authority mismatch, remote authority resolving locally, and `replica_status: unconfigured` before any I/O probe runs.

```python
with pytest.raises(SiteUnavailable, match="external replica is unconfigured"):
    authenticate_site(unconfigured_policy, descriptor, facts, probe)
assert probe.calls == []
```

- [ ] **Step 2: Add failing probe-lifecycle tests**

Assert disposable probe order is `create -> fsync-file -> fsync-dir -> stream-read -> remove -> fsync-dir`; content read must equal content written. Assert lock probing uses a nonblocking exclusive lock and releases it. Assert synchronized-clock false fails. Assert neither report nor receipt contains authorization intervals or observation values.

Call `recheck_readiness` after a successful seal, replace the replica mount or
make its stream-read disagree, and assert it reruns the same full lifecycle and
fails before returning. A re-stat-only implementation must fail this test.

- [ ] **Step 3: Confirm red**

```bash
nix develop ./abc --command pytest -q abc/tools/test_parser_rq_campaign_site.py
```

- [ ] **Step 4: Implement platform adapters and canonical values**

Read runtime paths only from `--site-descriptor`; do not support ambient environment fallbacks. Use `/proc/self/mountinfo` plus `stat` for mount facts, `getaddrinfo` plus `ip -j address` for address relation, and `timedatectl show -p NTPSynchronized --value` for the explicit local-clock assumption. Hash canonical JSON with sorted keys and compact separators.

The preflight build-capability probe uses a tiny derivation created under the explicit scratch root and calls Task 2's fresh-store routine. It records no candidate revision and its output is deleted. `seal-readiness` reauthenticates current site facts and binds the candidate, qualification identity, provenance core, site-report hash, corpus hashes, graph hash/version, evidence base revision/cleanliness, and embedded site-fact projection.

- [ ] **Step 5: Add the Nix check, run quality, and commit**

```bash
nix build ./abc#checks.x86_64-linux.parser-rq-campaign-site
just python-quality
git add abc/tools/parser_rq_campaign_site.py abc/tools/test_parser_rq_campaign_site.py abc/flake.nix
git commit -m "feat(parser-rq): authenticate campaign site readiness"
```

### Task 4: Bind Readiness into Authorization Version 2

**Files:**
- Modify: `abc/src/abc/tools/parser_rq_campaign.clj`
- Modify: `abc/test/abc/tools/parser_rq_campaign_test.clj`
- Modify: `abc/test/abc/tools/validate_design_bundle_test.clj`

**Interfaces:**

```clojure
(readiness-receipt-ref receipt) ; => "sha256:..."
(verify-readiness-receipt candidate provenance graph receipt) ; => vector of errors
(verify-authorization-record candidate provenance graph receipt authorization) ; => vector
(verify-authorization candidate provenance graph receipt authorization utc clock-synchronized?) ; => vector
(build-authorization candidate receipt ordinal not-before not-after host-policy-ref) ; => map
```

CLI changes:

```text
parser-rq-campaign verify-authorization-record --candidate PATH --provenance PATH --graph PATH --receipt PATH --authorization PATH
parser-rq-campaign verify-authorization        --candidate PATH --provenance PATH --graph PATH --receipt PATH --authorization PATH --utc TIME --clock-synchronized true
parser-rq-campaign authorize --candidate PATH --receipt PATH ...
```

- [ ] **Step 1: Migrate the shared fixture builder and write failing relation tests**

Replace hand-written authorization maps in `parser_rq_campaign_test.clj` with one builder that constructs a receipt and authorization v2. Add structural success for a future interval while temporal verification fails before the window. Add separate negative tests for a receipt binding a different candidate, qualification identity, provenance core, site-report hash, or graph version.

- [ ] **Step 2: Pin clock separation and self-reference failures**

```clojure
(is (empty? (campaign/verify-authorization-record candidate provenance graph receipt future-auth)))
(is (some #(re-find #"outside" %)
          (campaign/verify-authorization candidate provenance graph receipt future-auth
                                         "2026-07-18T00:00:00Z" true)))
(is (some #(re-find #"clock is not synchronized" %)
          (campaign/verify-authorization candidate provenance graph receipt open-auth
                                         "2026-07-18T01:00:00Z" false)))
```

Mutating `readiness_receipt_ref` without resealing must fail the authorization self-reference check.

- [ ] **Step 3: Confirm red**

```bash
nix develop ./abc --command abc/bin/kaocha --focus abc.tools.parser-rq-campaign-test
```

- [ ] **Step 4: Implement minimal v2 verification and CLI dispatch**

Split the existing `verify-authorization` body: closed schema, references, ordinal, identity, interval ordering, repetition, and reduction belong to `verify-authorization-record`; only explicit `utc`, synchronized state, and interval membership belong to the temporal function. Remove the `(Instant/now)` CLI fallback. Missing `--utc` or `--clock-synchronized` is a usage error.

- [ ] **Step 5: Run focused tests and commit**

```bash
nix develop ./abc --command abc/bin/kaocha \
  --focus abc.tools.parser-rq-campaign-test \
  --focus abc.tools.validate-design-bundle-test
nix build ./abc#checks.x86_64-linux.clj-kondo
git add abc/src/abc/tools/parser_rq_campaign.clj \
  abc/test/abc/tools/parser_rq_campaign_test.clj \
  abc/test/abc/tools/validate_design_bundle_test.clj
git commit -m "feat(parser-rq): bind readiness to capture authorization"
```

### Task 5: Transfer Lock Ownership to the Campaign Controller

**Files:**
- Modify: `ab-validator/reports/parser-ir/parser-rq-core-attempt-capture.py`
- Modify: `ab-validator/reports/parser-ir/test_parser_rq_core_attempt_capture.py`

**Interfaces:**

```python
@dataclass(frozen=True)
class LockCapability:
    fd: int
    device: int
    inode: int

def capture_repetitions(config: CaptureConfig, lock: LockCapability) -> dict[str, object]: ...
```

Production CLI requires `--inherited-lock-fd`; `--lock-path` remains test-only and is rejected by production mode.

- [ ] **Step 1: Add the failing inherited-lock characterization**

Have the test parent acquire `LOCK_EX|LOCK_NB`, pass the inheritable descriptor, and prove a separately opened descriptor cannot lock the same inode while all three repetitions run. Assert core capture does not call `open(lock_path)` or `flock(LOCK_EX)`.

- [ ] **Step 2: Add lock replacement and loss tests**

Replace the path during repetition two and close the inherited descriptor during repetition two. Both must terminate with `exclusive campaign lock was not retained`; no repetition three command runs.

- [ ] **Step 3: Confirm red**

```bash
nix develop ./abc --command pytest -q \
  ab-validator/reports/parser-ir/test_parser_rq_core_attempt_capture.py
```

- [ ] **Step 4: Implement capability validation**

Validate `fstat(fd)` against the controller-provided device/inode before and after every candidate process. Do not reacquire. Mark the descriptor inheritable only around the child spawn and pass it through `pass_fds=(fd,)`; restore non-inheritable state afterward.

Add an `argparse` `main()` that reads candidate, authorization, corpus, policy,
time executable, staging root, and inherited-lock coordinates from explicit
options. It emits the raw core index atomically and never derives an envelope.
`python .../parser-rq-core-attempt-capture.py --help` must exit zero.

- [ ] **Step 5: Verify and commit**

```bash
nix develop ./abc --command pytest -q \
  ab-validator/reports/parser-ir/test_parser_rq_core_attempt_capture.py
just python-quality
git add ab-validator/reports/parser-ir/parser-rq-core-attempt-capture.py \
  ab-validator/reports/parser-ir/test_parser_rq_core_attempt_capture.py
git commit -m "refactor(parser-rq): inherit the campaign lock"
```

### Task 6: Expose Existing Pure Analyzers Through One Member Projector

**Files:**
- Create: `abc/src/abc/tools/parser_rq_member.clj`
- Create: `abc/test/abc/tools/parser_rq_member_test.clj`
- Modify: `abc/deps.edn`

**Interfaces:**

```clojure
(project-source-recognition inputs) ; => {:source_span_coverage envelope}
(project-diagnostic-gap inputs)     ; => {:silent_drops envelope}
(project-predicate-pair inputs)     ; => {:diagnostic_completeness envelope
                                      ;     :parser_ir_schema_validation envelope}
(project-publication inputs)        ; => {:publication_structure envelope}
(project-resource inputs)           ; => {:peak_cgroup_memory_bytes envelope}
```

CLI alias: `:abc/parser-rq-member`. Closed subcommands: `source-recognition`,
`diagnostic-gap`, `predicate-pair`, `publication`, and `resource`. Each reads
explicit authenticated artifacts and writes one closed member value atomically.
It owns no execution, lane membership, authorization, lifecycle, installation,
composition, or promotion authority.

- [ ] **Step 1: Add failing adapter tests**

Prove each adapter delegates to the existing pure analyzer, preserves numeric
and sentinel observation values, checks the candidate identity, and refuses an
output key outside its assigned contract. The predicate-pair command must emit
exactly two envelopes; every other command emits exactly one.

- [ ] **Step 2: Confirm red**

```bash
nix develop ./abc --command abc/bin/kaocha --focus abc.tools.parser-rq-member-test
```

- [ ] **Step 3: Implement only explicit I/O adaptation**

Reuse `abc.tools.parser-rq-source-accountability`,
`parser-rq-diagnostic-completeness`, `parser-rq-parser-ir-conformance`,
`parser-rq-publication`, and `parser-rq-resource`. Do not duplicate their
validation or formulas. Write beneath a caller-supplied staging path; the
orchestrator in Task 7 performs canonical installation.

- [ ] **Step 4: Verify and commit**

```bash
nix develop ./abc --command abc/bin/kaocha --focus abc.tools.parser-rq-member-test
nix build ./abc#checks.x86_64-linux.clj-kondo
git add abc/src/abc/tools/parser_rq_member.clj \
  abc/test/abc/tools/parser_rq_member_test.clj abc/deps.edn
git commit -m "feat(parser-rq): expose member projections"
```

### Task 7: Implement the Fixed Production Orchestrator

**Files:**
- Create: `abc/tools/parser_rq_campaign_orchestrator.py`
- Create: `abc/tools/test_parser_rq_campaign_orchestrator.py`
- Modify: `ab-validator/reports/parser-ir/parser-rq-resource-capture.py`
- Create: `ab-validator/reports/parser-ir/test_parser_rq_resource_capture_cli.py`
- Modify: `abc/bin/parser-rq-campaign-capture.sh`
- Modify: `abc/flake.nix`

**Interfaces:**

```python
def authenticate_inputs(config: CampaignConfig) -> AuthenticatedCampaign: ...
def execute_graph(campaign: AuthenticatedCampaign, runner: Runner) -> Terminal: ...
def main(argv: list[str] | None = None) -> int: ...
```

The authoritative operation mapping is:

| Graph operation | Candidate executable(s) from provenance | Repository driver from detached tree | Projected/installed member(s) |
|---|---|---|---|
| `capture-core` | `ab-check` | `parser-rq-core-attempt-capture.py` | `core_attempt` |
| `capture-source` | `ab-parser-rq-source-accountability capture-corpus` | none | `source_recognition` |
| `capture-predicate-pair` | `ab-aozora`, `ab-aat-to-parser-ir` | `parser-rq-predicate-hardening-capture.py` | `diagnostic_completeness`, `parser_ir_conformance` |
| `derive-diagnostic-gap` | `ab-parser-rq-diagnostic-authorization capture-corpus` | none | `diagnostic_gap` |
| `capture-publication` | `ab-aozora`, `ab-aat-to-parser-ir` | `clojure -M:abc/materialize-publication`, `publication-bundle-validate.py`, `publication-rq-capture.py` | `publication_structure` |
| `capture-resource` | `ab-check` and the policy-named parser executable | `parser-rq-resource-capture.py`, `parser-rq-resource-wrapper.py` | `resource` |

The graph holds operation IDs and installed-member contracts, never caller
commands. The two-output predicate operation is the only cardinality greater
than one.

The graph contains closed operation identifiers, not shell strings:

```json
{
  "members": [
    {"name": "core_attempt", "operation": "capture-core"},
    {"name": "source_recognition", "operation": "capture-source"},
    {"name": "predicate_hardening", "operation": "capture-predicate-pair"},
    {"name": "diagnostic_gap", "operation": "derive-diagnostic-gap"},
    {"name": "publication_structure", "operation": "capture-publication"},
    {"name": "resource", "operation": "capture-resource"}
  ],
  "installed_members": [
    "core_attempt", "source_recognition", "diagnostic_gap",
    "diagnostic_completeness", "parser_ir_conformance",
    "publication_structure", "resource"
  ]
}
```

- [ ] **Step 1: Add failing closed-graph tests**

Load the committed graph policy and assert exact order. The public CLI accepts only `--candidate`, `--authorization`, `--provenance`, `--readiness-receipt`, `--site-policy`, `--site-descriptor`, `--candidate-tree`, `--evidence-tree`, `--staging-root`, and `--production`. Assert every `PARSER_RQ_*_CAPTURE` environment variable is ignored and then rejected if present, and that no `--lane-command` option exists.

- [ ] **Step 2: Add pre-start versus post-start terminal tests**

Inject a runner and clock. Host/replica/lock/clock failure before capture-start must leave no capture index and report `preparation_failed`, with ordinal unconsumed. Once capture-start is atomically recorded, interruption, unknown producer status, missing member, or lock loss must install one `unavailable` terminal record and must never call a second attempt.

- [ ] **Step 3: Add executable and cwd-independence tests**

Each candidate executable path must come from authenticated provenance. Repository Python/Clojure paths and policy files must resolve beneath `candidate_tree`; evidence output and Git checks must resolve beneath `evidence_tree`. Run the bounded graph with process cwd set to an unrelated empty directory.

- [ ] **Step 4: Add lock and exact-membership tests**

Assert the controller acquires one descriptor, passes that same capability to core, keeps it through composition/authentication, and releases it last. Reject omitted, extra, repeated, or reordered lanes and exactly-seven-member violations. Assert pure analyzer/composer calls never execute a candidate binary.

Add a resource CLI test proving `parser-rq-resource-capture.py` runs work IDs
serially and writes its index atomically.

- [ ] **Step 5: Confirm red**

```bash
nix develop ./abc --command pytest -q abc/tools/test_parser_rq_campaign_orchestrator.py
```

- [ ] **Step 6: Implement the graph as data interpreted by one controller**

Production mode must compare the graph file's hash to both provenance and readiness receipt before resolving any command. Construct argv from the reviewed templates plus authenticated runtime values; never invoke a shell. Immediately before capture start call `recheck-readiness`, acquire and validate the lock, obtain one UTC realtime value, then call Clojure temporal authorization verification with that same value. Store that exact timestamp in the capture index.

`recheck-readiness` must rerun the complete replica lifecycle:
create, fsync file, fsync directory, stream-read and compare, remove, and fsync
directory again. A mount-only recheck is insufficient. It also rechecks host
identity, remote authority, mount class/source/fsid, clock synchronization, and
lock identity before capture start.

Map operation identifiers in code with an exhaustive `match`; unknown values
are protocol errors. `capture-source` resolves
`ab-parser-rq-source-accountability` from provenance; diagnostic authorization
resolves `ab-parser-rq-diagnostic-authorization`; core, predicate-pair,
publication, and resource invoke their repository scripts from the detached
candidate tree. All raw outputs then pass through `parser-rq-member` before the
controller installs canonical members. No executable or argv is copied from
the runtime descriptor.

Invoke Clojure from any working directory with this exact prefix:

```python
clojure_prefix = [
    "nix", "develop", "--no-write-lock-file", str(candidate_tree / "abc"),
    "--command", "clojure",
]
```

Temporal verification appends `-M:abc/parser-rq-campaign
verify-authorization ...`; projection appends `-M:abc/parser-rq-member
SUBCOMMAND ...`. Unit tests assert these full argv values. Task 8 drives these
real subprocesses from an unrelated empty cwd.

- [ ] **Step 7: Replace the shell with a thin launcher**

The shell computes its repository root from its own path and executes only:

```bash
exec python "$repo_root/abc/tools/parser_rq_campaign_orchestrator.py" "$@"
```

It contains no `flock`, `date`, Clojure invocation, environment-variable command, or lane order.

- [ ] **Step 8: Add the Nix check and commit**

```bash
nix build ./abc#checks.x86_64-linux.parser-rq-campaign-orchestrator
just python-quality
git add abc/tools/parser_rq_campaign_orchestrator.py \
  abc/tools/test_parser_rq_campaign_orchestrator.py \
  ab-validator/reports/parser-ir/parser-rq-resource-capture.py \
  ab-validator/reports/parser-ir/test_parser_rq_resource_capture_cli.py \
  abc/bin/parser-rq-campaign-capture.sh abc/flake.nix
git commit -m "feat(parser-rq): own the fixed campaign graph"
```

### Task 8: Prove Real Wiring and Migrate Deterministic Drift

**Files:**
- Create: `abc/test/fixtures/parser-rq/execution-readiness/`
- Modify: `abc/test/abc/tools/parser_rq_admission_promotion_drift_test.clj`
- Modify: `abc/test/abc/tools/parser_rq_campaign_test.clj`
- Modify: `abc/bin/parser-rq-admission-promotion-smoke.sh`
- Create: `abc/bin/parser-rq-execution-readiness-smoke.sh`
- Modify: `abc/flake.nix`

**Interfaces:**
- One fixture generator owns candidate, unbound proof, bound provenance, site facts, final receipt, authorization v2, seven lane members, capture index, and expected summary.
- Drift means byte-identical regeneration of deterministic fixture values; live mount/build probes are separate integration smokes.
- The integration smoke invokes the real orchestrator runner, real default-package Rust executables, real repository capture scripts, and real Clojure projector over the bounded corpus. Only site facts, authorization time, stores, and corpus size are synthetic.

- [ ] **Step 1: Add the failing fixture regeneration test**

Generate all deterministic values under a temporary root, then compare recursively with the committed fixture. The test must fail initially because the fixture still uses authorization v1 and has no readiness receipt.

- [ ] **Step 2: Add adversarial binding mutations**

Mutate one field at a time: graph hash, candidate ref, qualification identity, provenance core, site-report hash, receipt hash, capture-start timestamp, executable bytes, and member order. Require the appropriate boundary to reject each mutation before promotion verification.

- [ ] **Step 3: Add repository search guards**

```bash
! rg -n '"schema_version"[[:space:]]*:[[:space:]]*"1\.0\.0"' \
  abc/test abc/test/fixtures/parser-rq/admission-promotion \
  -g '*authorization*' -g '*.json' -g '*.edn'
! rg -n 'PARSER_RQ_(CORE|SOURCE|PREDICATE|DIAGNOSTIC|PUBLICATION|RESOURCE)_CAPTURE' \
  abc/bin abc/src abc/tools ab-validator/reports
```

Intentional historical documentation is outside this active-code guard.

- [ ] **Step 4: Regenerate deterministic drift with injected boundaries**

Use fake executables, injected site facts, and temporary stores; do not contact hinoki or run the qualification corpus. Commit only canonical small values and the summary.

- [ ] **Step 5: Add the real-executable bounded orchestrator smoke**

`parser-rq-execution-readiness-smoke.sh` receives the built default package
path from its Nix check and starts in an unrelated empty directory. Its Python
harness imports the orchestrator, supplies a sealed bounded site adapter and
clock, but uses the production `SubprocessRunner`, production operation-to-argv
resolver, and production member installer. It must execute the two
provenance-resolved Rust CLIs, the repository capture drivers, the real Clojure
member projector through the pinned `nix develop` prefix, composition, and
capture authentication. Assert the expected seven canonical members and nine
envelopes, and assert every producer invocation count is one except the core's
fixed three repetitions. Do not replace any producer or subprocess with a fake
runner in this smoke. Separately invoke `parser-rq-campaign-capture.sh --help`
from that empty cwd to prove the thin production launcher resolves itself.

- [ ] **Step 6: Verify and commit**

```bash
nix develop ./abc --command abc/bin/kaocha \
  --focus abc.tools.parser-rq-admission-promotion-drift-test \
  --focus abc.tools.parser-rq-campaign-test
nix build ./abc#checks.x86_64-linux.parser-rq-admission-promotion-smoke
nix build ./abc#checks.x86_64-linux.parser-rq-execution-readiness-smoke
git add abc/test/fixtures/parser-rq/execution-readiness \
  abc/test/abc/tools/parser_rq_admission_promotion_drift_test.clj \
  abc/test/abc/tools/parser_rq_campaign_test.clj \
  abc/bin/parser-rq-admission-promotion-smoke.sh \
  abc/bin/parser-rq-execution-readiness-smoke.sh abc/flake.nix
git commit -m "test(parser-rq): prove production graph wiring"
```

### Task 9: Rewrite P5 Operational Steps Around Preflight and Sealing

**Files:**
- Modify: `abc/docs/superpowers/plans/2026-07-17-parser-rq-admission-promotion.md`
- Modify: `abc/docs/superpowers/plans/2026-07-15-parser-release-qualification-campaign.md`

**Interfaces:**
- P5 Task 10 becomes preparation: preflight, two independent candidate builds, compare, bind, candidate derivation, readiness seal, structural authorization verification, commit/push.
- P5 Task 11 remains the sole volatile execution, now invoked only through the fixed orchestrator and the untracked site descriptor.

- [ ] **Step 1: Replace the invalid raw `nix build --json` sequence**

Pin two `realize-build` calls with distinct absent roots and IDs, two `capture-build` calls, `compare-builds`, candidate derivation, and `bind-provenance`. Explicitly require real target build logs and target absence before each build.

- [ ] **Step 2: Add the no-authorization site-preflight checkpoint**

The plan must first run with the committed unconfigured policy and expect an operational stop. After separately provisioning and reviewing the real remote-domain policy, rerun `preflight-site`; only a green report may feed `seal-readiness`.

- [ ] **Step 3: Replace future-window temporal verification with structural verification**

Before committing an authorization, invoke only `verify-authorization-record`. State that the orchestrator supplies the one real capture-start instant to temporal verification. Add receipt and provenance paths to every authorization/capture command.

- [ ] **Step 4: Replace caller-supplied configuration and commands**

Task 11 passes one `--site-descriptor`, explicit candidate/evidence trees, and staging root. Delete the six command environment variables and `hostname -f` assertion. Retain the rule that an unavailable result after capture start is the candidate's terminal result.

- [ ] **Step 5: Document the staging-value cleanup and operational blocker**

State exactly when fresh stores, build records, and site-preflight report are deleted, and that the final receipt embeds the authenticated site-fact projection. State that implementation completion is not campaign completion while replica policy is unconfigured.

Add an explicit status ledger to the existing P5 plan: implemented foundation
tasks remain historically unchecked checklist instructions, while Tasks 10+
are marked “not started.” Do not mechanically check steps without matching
commits/evidence, and do not leave the document implying that unchecked
foundation code is absent.

- [ ] **Step 6: Run documentation consistency checks and commit**

```bash
rg -n 'nix build --store|verify-authorization|PARSER_RQ_.*_CAPTURE|hostname -f|readiness' \
  abc/docs/superpowers/plans/2026-07-17-parser-rq-admission-promotion.md \
  abc/docs/superpowers/plans/2026-07-15-parser-release-qualification-campaign.md
scripts/comment-hygiene-check.sh
git add abc/docs/superpowers/plans/2026-07-17-parser-rq-admission-promotion.md \
  abc/docs/superpowers/plans/2026-07-15-parser-release-qualification-campaign.md
git commit -m "docs(parser-rq): make P5 transaction executable"
```

Expected: every operational command named by P5 exists; no bare future-window temporal check or caller-selected lane remains.

### Task 10: Run the Whole-Range Integrity Review

**Files:**
- Modify only if the review finds a defect: files introduced or changed by Tasks 1-9.

**Interfaces:**
- This task adds no feature. It proves the plan's boundaries compose and fixes discovered defects in their owning task files.

- [ ] **Step 1: Prove non-goals with path-existence assertions and diffs**

```bash
for path in \
  abc/data/parser-release-qualification-predicates.edn \
  abc/data/parser-release-qualification-corpus.edn \
  abc/data/aat-parser-ir-compatibility.edn \
  abc/docs/adr/0039-custom-parser-release-qualification.md \
  abc/docs/adr/0040-process-tree-memory-qualification.md; do
  test -e "$path"
  implementation_base=$(git log -1 --format=%H -- \
    abc/docs/superpowers/plans/2026-07-18-parser-rq-execution-readiness.md)
  git diff --exit-code "$implementation_base" -- "$path"
done
```

The plan commit is the immutable implementation baseline. Do not replace it
with a relative commit count, weaken the paths, or omit the existence checks.

- [ ] **Step 2: Exercise every CLI usage path**

```bash
python ab-validator/reports/parser-ir/parser-rq-campaign-provenance.py --help
python abc/tools/parser_rq_campaign_site.py --help
python abc/tools/parser_rq_campaign_orchestrator.py --help
nix develop ./abc --command clojure -M:abc/parser-rq-campaign verify-authorization-record --help
```

Expected: exit 0 for top-level help; missing required operands exit 2 without writing outputs.

- [ ] **Step 3: Run focused suites together**

```bash
nix develop ./abc --command pytest -q \
  ab-validator/reports/parser-ir/test_parser_rq_campaign_provenance.py \
  ab-validator/reports/parser-ir/test_parser_rq_core_attempt_capture.py \
  abc/tools/test_parser_rq_campaign_site.py \
  abc/tools/test_parser_rq_campaign_orchestrator.py \
  ab-validator/reports/parser-ir/test_parser_rq_resource_capture_cli.py
nix develop ./abc --command abc/bin/kaocha \
  --focus abc.tools.validate-design-bundle-test \
  --focus abc.tools.parser-rq-campaign-test \
  --focus abc.tools.parser-rq-member-test \
  --focus abc.tools.parser-rq-admission-promotion-drift-test
```

- [ ] **Step 4: Run repository quality gates**

```bash
just python-quality
just nix-format-check
nix build ./abc#checks.x86_64-linux.clj-kondo
scripts/comment-hygiene-check.sh
just validate-migration
```

`scripts/python-quality.sh` discovers all tracked `*.py` with `git ls-files`;
assert both new tool paths appear in that list before accepting this gate.

- [ ] **Step 5: Commit only genuine corrections**

If review changes code, rerun the owning focused test plus Step 4, then commit:

```bash
git diff --name-only -- abc ab-validator
git diff --name-only -z -- abc ab-validator | xargs -0 --no-run-if-empty git add --
git commit -m "fix(parser-rq): close execution readiness review gaps"
```

If no defect is found, make no empty commit.

### Task 11: Push the Implementation Checkpoint Without Starting P5

**Files:**
- No source changes expected.

**Interfaces:**
- Produces a pushed `main` implementation checkpoint.
- Does not produce a candidate, site-preflight report, readiness receipt, authorization, capture, evaluation, registry row, or ADR transition.

- [ ] **Step 1: Verify clean synchronization state**

```bash
test "$(git branch --show-current)" = main
test -z "$(git status --porcelain)"
git fetch origin
test "$(git rev-parse HEAD)" = "$(git rev-parse origin/main)" || \
  git log --oneline --left-right origin/main...HEAD
```

Expected before push: local `main` is ahead only by the reviewed implementation commits and is not behind.

- [ ] **Step 2: Push and verify the exact revision**

```bash
implementation_rev=$(git rev-parse HEAD)
git push origin main
git fetch origin
test "$implementation_rev" = "$(git rev-parse origin/main)"
```

- [ ] **Step 3: Record the honest handoff state**

Report:

```text
Execution-readiness implementation: complete and pushed.
External replica policy: unconfigured; authoritative P5 execution remains blocked.
Authorization minted: no.
Candidate capture started: no.
Next authorized action: provision/review the remote failure domain, then run the no-authorization preflight from P5.
```

Do not describe the parser release campaign itself as complete.

## Self-Review Checklist

- [ ] Every acceptance criterion in the execution-readiness design maps to a task and an executable test.
- [ ] The real Nix target is absent before each offline build; dependency substitution cannot substitute the target.
- [ ] Concrete derivation builds omit the inert evaluation-store flag and fail if a required seeded dependency is absent.
- [ ] Remote-domain independence comes from reviewed policy plus authority checks, not merely distinct paths/devices or successful I/O.
- [ ] Readiness receipt semantics say “ready at seal”; host, full replica I/O lifecycle, lock, and clock are rechecked before capture start.
- [ ] The disposable site report is not a runtime input; its authenticated projection and hash are in the final receipt.
- [ ] The local synchronized clock is an explicit trust assumption.
- [ ] Authorization v2 receipt mismatch tests cover candidate, identity, provenance, report, and graph independently.
- [ ] Production graph membership has one source of truth and no caller-selected commands.
- [ ] The member projector is only a Clojure I/O adapter; the orchestrator installs and the composer remains membership authority.
- [ ] Core receives the controller's lock capability and never reacquires the path.
- [ ] The bounded fixture regenerates byte-identically without contacting hinoki.
- [ ] A separate bounded smoke executes the production argv resolver and every real producer through the orchestrator.
- [ ] Predicate, corpus, registry, and ADR non-goal guard paths all exist before their diffs run.
- [ ] Implementation can be green while site policy remains safely unconfigured; no authorization or capture is created.
