# Workflow Orchestration Design

Status: Proposed design
Date: 2026-07-08
Owner: Soranoha architecture track

This spec deepens the idea of using DAG/workflow concepts to simplify
Soranoha orchestration. It is not a proposal to immediately adopt Nodely,
Pathom, or `core.async.flow` as the build engine. It defines a smaller,
repo-native workflow value and run-report contract first, so existing Clojure,
Python, Bash, and Nix orchestration can become more inspectable before any
runtime engine choice becomes load-bearing.

## Problem

Soranoha has several orchestration-shaped programs:

- `abc.tools.soranoha/publication-rehearsal!` runs source snapshot generation,
  request-set resolution, snapshot-root materialization, reports, staging, and
  validation in one long sequential `let`.
- `abc.tools.soranoha-build-publication/build-publication!` validates a local
  Aozora checkout, selects official source ZIPs, materializes source work
  roots, writes build reports, and delegates to publication rehearsal.
- `abc.tools.materialize-publication/materialize-publications-batch!` owns
  per-job concurrency, try/catch, status aggregation, and summary output.
- `abc.tools.validate-design-bundle/validate-design-bundle!` is a long
  validation script with implicit stages and logging.
- Bash scripts such as
  `ab-validator/reports/morph-warehouse/build-report.sh` and
  `ab-validator/reports/aat-fidelity/run-cross-adapter-report.sh` perform
  multi-step report builds with temporary directories, command execution,
  evidence paths, and summaries.
- Python scripts such as
  `ab-validator/reports/parser-ir/tei-eaj-generated-compare.py` coordinate
  subprocess conversion, batch materialization, threadpool concurrency, and
  failure rows.
- Nix flakes wrap many smoke checks as shell fragments in `runCommand`.

These programs repeat the same operational concerns:

- step order and dependencies;
- output-root preparation and promotion;
- start/end timestamps and durations;
- fatal errors versus failure-as-value;
- per-step status summaries;
- deterministic report writing;
- command/path/hash provenance;
- validation gates;
- concurrency controls.

The repetition makes the code harder to inspect. A reviewer must read control
flow to answer simple questions: what steps exist, what inputs each step needs,
what outputs each step produces, which step failed, and which outputs were
written.

## Design Goal

Make orchestration explicit as data and make runs inspectable without changing
artifact identity.

The first useful target is behavior-preserving extraction:

```text
publication-rehearsal!
  source-snapshot
  resolve-request-set
  materialize-snapshot-root
  validate-snapshot-root
  publication-report
  layout-report
  stage-publication
  validate-staged-publication
  write-rehearsal-report
```

After extraction, the command should still print the same important paths and
produce the same snapshot/publication artifacts, but it should also write a
machine-readable workflow plan and workflow run report.

## Non-Goals

- Do not change manifest identity, request-set identity, snapshot-index
  identity, or artifact content hashes.
- Do not make workflow IDs citable publication identity.
- Do not replace Nix derivations or make Nix enumerate the corpus.
- Do not port Python or Bash orchestration to Clojure in the first slice.
- Do not add Nodely, Pathom, or `core.async.flow` before the internal workflow
  value proves useful.
- Do not introduce resumable builds in the first slice. A workflow run report
  can record enough state to support a future resume design.

## Prior Art

### core.async.flow

`core.async.flow` separates application logic from topology, execution,
communication, lifecycle, monitoring, and error handling. Its process functions
remain ordinary data-to-data functions, while a flow data structure describes
connections, channels, and lifecycle. This is a strong later fit for
long-running, concurrent per-work corpus materialization.

It is not the first slice because Soranoha's immediate need is finite
artifact-DAG observability, not a channel network.

### Nodely

Nodely provides declarative data dependency graphs with conditional, async, and
lazy resolution. It is a plausible later fit for "realize this target value
from dependency declarations" workflows.

It is not the first slice because we do not yet need lazy target selection
inside ABC orchestration; the current workflows are explicit command pipelines.

### Pathom 3

Pathom models attribute relationships with resolvers and planners. It is a
good fit for query and inspection surfaces such as `explain-snapshot`:

```text
given work_id -> artifact refs -> manifests -> hashes -> local paths -> reports
```

It is not a good fit for side-effectful build execution. Pathom should remain a
candidate for generated views and explanation APIs, not the publication build
engine.

## Decision

Adopt a two-layer design:

1. **Language-neutral workflow run schema.** Every orchestrator can emit the
   same `workflow-run.json` shape, regardless of whether the work was executed
   by Clojure, Bash, Python, Rust, or Nix.
2. **Small Clojure serial workflow runner.** ABC-owned Clojure commands can
   define workflow plans as data and execute them through one small runner.

External engines remain deferred:

- Evaluate `core.async.flow` only after the per-work materialization stage has
  a stable step protocol and a measured need for long-running flow lifecycle.
- Evaluate Nodely only if target-driven lazy realization becomes a real need.
- Evaluate Pathom only for inspection/query surfaces.

## Workflow Plan

A workflow plan is an operational value. It is not an artifact identity object.

Example shape:

```clojure
{:workflow/id "soranoha.publication-rehearsal.v1"
 :workflow/schema-version "soranoha-workflow-plan-v1"
 :workflow/steps
 [{:step/id :source-snapshot
   :step/requires [:input-root :source-snapshot-root :snapshot-scope
                   :snapshot-date]
   :step/produces [:source-snapshot-result]}
  {:step/id :resolve-request-set
   :step/requires [:request-set-label :source-snapshot-result :request-set-file]
   :step/produces [:request-set]}
  {:step/id :materialize-snapshot-root
   :step/requires [:request-set :request-set-file :snapshot-root]
   :step/produces [:snapshot]}
  {:step/id :stage-publication
   :step/requires [:snapshot-root :snapshot :staged-root]
   :step/produces [:staged-result]}]}
```

Rules:

- Step IDs are stable strings or keywords.
- `requires` and `produces` are operational keys in the workflow state map.
- A workflow plan may be written as EDN for Clojure internals, but
  `workflow-plan.json` must use JSON strings.
- A step cannot produce a key already produced by an earlier step in the first
  runner. Replacement semantics are a future extension, not part of v1.
- The first Clojure runner executes topologically but rejects ambiguous plans:
  missing inputs, duplicate step IDs, duplicate produced keys, and dependency
  cycles fail before any step runs.

## Workflow Run Report

`workflow-run.json` is the cross-language inspection artifact.

Minimum JSON shape:

```json
{
  "schema_id": "https://w3id.org/abc/schemas/workflow-run.schema.json",
  "schema_version": "soranoha-workflow-run-v1",
  "workflow_id": "soranoha.publication-rehearsal.v1",
  "run_id": "20260708T120000Z-00000000",
  "status": "passed",
  "started_at": "2026-07-08T12:00:00Z",
  "ended_at": "2026-07-08T12:00:01Z",
  "duration_ms": 1000,
  "step_count": 3,
  "steps_passed": 3,
  "steps_failed": 0,
  "steps": [
    {
      "id": "source-snapshot",
      "status": "passed",
      "started_at": "2026-07-08T12:00:00Z",
      "ended_at": "2026-07-08T12:00:00Z",
      "duration_ms": 100,
      "requires": ["input-root", "snapshot-scope", "snapshot-date"],
      "produces": ["source-snapshot-result"],
      "inputs": [
        {"role": "input-root", "path": "materialized-root"}
      ],
      "outputs": [
        {
          "role": "source-snapshot",
          "path": "source-snapshot/source-snapshot.json",
          "content_hash": "sha256:0000000000000000000000000000000000000000000000000000000000000000"
        }
      ],
      "messages": []
    }
  ]
}
```

Rules:

- `status` is one of `passed`, `failed`, `partial`, or `skipped`.
- `run_id` is operational provenance. It does not participate in artifact
  identity.
- `inputs` and `outputs` are bounded inspection records. They may carry paths,
  content hashes, artifact IDs, request-set IDs, and snapshot identity hashes.
- A step error is recorded as data with `error_class`, `message`, and optional
  bounded `data`. Stack traces are local logs, not required in the JSON report.
- For failure-as-value workflows, a step can have `status = "passed"` while
  producing failure manifests. The step's messages must record the count of
  failed coordinate values.
- Top-level `status = "partial"` means the workflow completed its configured
  run contract but produced one or more failure-as-value outputs.
- Paths are relative to the workflow output root whenever possible.

## Clojure Runner Contract

Namespace:

```clojure
abc.tools.workflow
```

Primary function:

```clojure
(run-workflow!
 {:workflow-id "soranoha.publication-rehearsal.v1"
  :output-root output-root
  :initial-state {:input-root input-root
                  :snapshot-date "2026-07-08"}
  :steps [{:id :source-snapshot
           :requires [:input-root :snapshot-date]
           :produces [:source-snapshot-result]
           :run source-snapshot-step}]})
  :clock clock-fn
  :write-json! json-writer})
```

Step function contract:

```clojure
(fn [state]
  {:state-updates {:request-set request-set-value}
   :outputs [{:role "request-set"
              :path "request-sets/full-corpus-publication-basic-ja.json"
              :content_hash "sha256:0000000000000000000000000000000000000000000000000000000000000000"}]
   :messages [{:level "info"
               :message "resolved request set"
               :data {"request_set_id"
                      "sha256:1111111111111111111111111111111111111111111111111111111111111111"}}]})
```

Runner behavior:

- Validate the plan before running any step.
- Execute steps in topological order.
- Merge `:state-updates` into the state after each successful step.
- Record step timing, messages, inputs, and outputs.
- On thrown exceptions, record a failed step and rethrow by default.
- A step may return `:status :partial` or `:status :passed` with failure
  outputs instead of throwing when failure-as-value is part of that step's
  domain contract. The runner records the returned status; it does not
  interpret domain failure policy in v1.
- Write `workflow-plan.json` before execution and `workflow-run.json` after
  each step, so an interrupted run remains inspectable.

The runner should not know about manifests, TEI, Aozora, tokenizers, or Nix.
Domain-specific step functions own those details.

## Bash and Python Integration

The same run-report schema should be reusable without forcing Clojure execution.

### Bash

Add a tiny shell helper after the Clojure runner proves the schema:

```bash
workflow_init "$run_file" "$workflow_id" "$run_id"
workflow_step_pass "$step_id" "$output_path"
workflow_step_fail "$step_id" "$message"
workflow_finish "$status"
```

The first helper records coarse step timing: a pass/fail call creates one
complete step event. If later scripts need accurate per-step durations, add a
separate start/end API then. The helper should avoid complex JSON manipulation
in shell. It can append JSONL step events and call a small Clojure or Python
finalizer to produce `workflow-run.json`.

First Bash target:

```text
ab-validator/reports/morph-warehouse/build-report.sh
```

This script has a clean loop over SQL templates and already writes an index.
Recording each template as a workflow step would make report generation
inspectable without changing its behavior.

### Python

Add `ab-validator/reports/lib/workflow.py` after the Bash helper or in the same
phase if a Python script is the first non-Clojure adopter.

First Python target:

```text
ab-validator/reports/parser-ir/tei-eaj-generated-compare.py
```

This script is a better stress test because it has concurrent tasks and
subprocess failures. It should record task groups, not every low-level helper
call.

## Nix Integration

Nix should not depend on workflow-run identity. Nix remains the build and check
isolation layer.

Recommended rule:

- `runCommand` checks may emit `workflow-run.json` into `$out` or the build log
  for debugging, but the workflow report is not a separate Nix output identity.
- Keep `mkMonorepoCheck` and `mkSmokeCheck` as Nix-level wrappers. Do not move
  Nix evaluation decisions into the workflow runner.
- If a Nix check wraps a script that already emits `workflow-run.json`, the Nix
  check should preserve that file under `$out/workflow-run.json` when the check
  output needs inspection.

## Identity and Trust

Workflow reports are operational provenance.

They can cite:

- artifact IDs;
- content hashes;
- request-set IDs;
- snapshot identity hashes;
- manifest paths;
- source checkout commits;
- command arguments.

They do not define:

- artifact IDs;
- content hashes;
- request-set IDs;
- snapshot identity hashes.

If a workflow report disagrees with a manifest or snapshot index, the manifest
or snapshot index wins for publication identity. The workflow report is useful
for debugging how the output was produced.

## Error Model

Three failure classes remain distinct:

1. **Plan failure:** invalid workflow graph, missing initial input, duplicate
   produced key, or dependency cycle. No step runs.
2. **Step failure:** a step throws or exits non-zero and the workflow policy is
   fail-fast. The run status is `failed`.
3. **Failure as value:** the step succeeds operationally but emits one or more
   failure manifests or failure rows. The run status is `partial` unless the
   workflow policy declares those failures acceptable for the run's scope.

This keeps the existing Soranoha distinction between command failure and
artifact-level failure.

## Candidate Migration Targets

| Target | Fit | First useful extraction |
|---|---|---|
| `publication-rehearsal!` | High | Serial Clojure workflow, same outputs, `workflow-run.json`. |
| `build-publication!` | High | Wrapper workflow around source materialization and rehearsal delegation. |
| `materialize-publications-batch!` | High | Reuse step/task status and concurrency summary shape. |
| `validate-design-bundle!` | Medium | Later; broad blast radius but high report value. |
| `morph-warehouse/build-report.sh` | High | Bash workflow events for SQL template loop. |
| `run-cross-adapter-report.sh` | Medium-high | Bash workflow around oracle run, markdown render, DuckDB load, summary. |
| `tei-eaj-generated-compare.py` | High | Python workflow around conversion/materialization task groups. |
| Nix `mkMonorepoCheck` | Medium | Preserve workflow reports from wrapped scripts, do not replace Nix. |
| Pure report builders | Low | Use shared report helpers, not workflow DAGs. |

## Acceptance Criteria

The first implementation slice is accepted when:

- `abc/schemas/workflow-run.schema.json` validates a passed run fixture and
  rejects a run with an invalid status.
- `abc.tools.workflow/run-workflow!` can run a three-step workflow, write
  `workflow-plan.json` and `workflow-run.json`, and reject a missing dependency
  before executing any step.
- `publication-rehearsal!` produces the same publication artifacts as before
  and additionally writes `workflow-plan.json` and `workflow-run.json`.
- Existing focused Clojure/Nix tests pass.
- `nix flake check` passes after the slice is merged.

The second implementation slice is accepted when:

- `build-publication!` uses the Clojure workflow runner for its top-level
  stages.
- `materialize-publications-batch!` emits a schema-conformant workflow run
  sidecar and, when it writes a batch summary, a summary field that points to
  that sidecar.
- At least one Bash report script emits a workflow run report without changing
  its existing output files.

The third implementation slice is accepted when:

- one Python orchestration script emits workflow-compatible run data;
- a small summary command or report can list workflow steps, durations, and
  failed outputs across Clojure and non-Clojure runs.

## Risks

### Risk: Fake seam

If the runner only wraps one command and never carries shared behavior, it is a
fake seam. Mitigation: first convert `publication-rehearsal!`, then immediately
convert `build-publication!` or `materialize-publications-batch!` before
expanding the abstraction.

### Risk: Engine capture

Adopting `core.async.flow`, Nodely, or Pathom too early could force the
workflow model to match an engine rather than the project. Mitigation: keep the
first schema and Clojure runner dependency-free.

### Risk: Identity confusion

Workflow IDs and run IDs may be mistaken for citable publication identity.
Mitigation: every schema and report says workflow data is operational
provenance, while manifests/request sets/snapshot indexes remain canonical.

### Risk: JSON noise

Full per-file step records for 17k works could become too large. Mitigation:
record bounded per-step summaries by default and allow opt-in detailed task
records for debug runs.

## Open Follow-Ups

- Define a resume/reuse policy after workflow reports expose enough state to
  reason about partial runs.
- Decide whether `core.async.flow` is useful for the full-corpus adapter/parser
  pipeline after real subprocess/concurrency behavior is measured.
- Decide whether Pathom should power `explain-snapshot` and workflow browsing.
- Decide whether workflow-run reports should be linked from snapshot indexes as
  optional operational provenance sidecars.
