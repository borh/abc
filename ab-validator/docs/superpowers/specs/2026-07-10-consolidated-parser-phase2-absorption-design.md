# Consolidated Parser Phase 2 — Absorption Design

Date: 2026-07-10
Status: Approved for planning
Parent design: `2026-07-10-consolidated-parser-design.md` (Phase 2 — absorption)
Governance: ADR 0030 (selection), ADR 0032 (hard detach at `1a4f864`)
Prerequisite evidence: Phase 0+1 landed on main (merge `dbc7658c`); Gate A
`FORK_PARITY_CONFIRMED` (17,886/0/0), Gate B conformance parity, perf gate
PASS (−0.07%). Provenance and the feature-unification hazard are recorded in
`docs/handoffs/2026-07-10-parser-fork-provenance.md`.

## Goal

`ab-aozora-aat` emits AAT natively in-process from the lifted fork crates;
the permanent `ab-aozora` stdin→AAT binary fronts it at the harness edge; the
throwaway `ab-aozora-cli` shim is deleted; the measurement harnesses repoint
to `ab-aozora`; adapter identity rotates to `ab-aozora` under an unadmitted
registry identity. The legacy lane (pinned upstream binary + `adapters/aozora`
subprocess path) is demoted to comparison-only, byte-untouched.

What disappears is the *internal* subprocess hop (`aozora inspect` JSON
between parser and AAT assembly). The stdin→AAT process boundary the harness
owns survives, per the parent design's Executable Boundary contract.

## Decisions (settled in brainstorming, 2026-07-10)

1. **Hazard fix: facade feature split + converter hardening** (defense in
   depth). `ab-aozora-aat` never enables the `preserve_order`-carrying
   feature; `ab-aat-to-parser-ir` additionally stops relying on serde_json
   map ordering for canonical output.
2. **Absorption by copy; the original stays frozen.** Adapter AAT-assembly
   logic is ported into `ab-aozora-aat` as its owning home;
   `adapters/aozora` remains byte-untouched as the frozen legacy-lane
   comparator until Phase 4 retirement. Temporary duplication is accepted:
   the corpus-wide parity gate verifies the transcription, and both copies'
   divergence risk ends when the legacy lane is retired.
3. **Folded Minors** (from the Phase 1 final-review triage), each landing on
   a surface Phase 2 already touches: justfile `AOZORA_BIN` existence guard;
   `reports/**` pytest wired into the check gate; the
   `measure-parser-performance.py` schemaVersion-1 drift fixed during its
   repoint. Shim-scoped Minors (container-pairs dispatch, SJIS shim golden,
   shim env hermeticity) become moot when the shim is deleted.

## Hazard resolution (first work item — it unblocks the workspace)

Recorded defect: `ab-aozora-facade`'s `json` feature enables
`serde_json/preserve_order`; cargo feature unification turned it on for every
workspace crate, silently flipping `ab-aat-to-parser-ir`'s canonical
sorted-key JSON to insertion order (50/57 integration failures in
`--workspace` builds only). The Phase 1 fix excluded the shim from the
workspace. Phase 2 needs a real resolution because `ab-aozora-aat` must be a
normal workspace member.

### Facade feature split

Split `ab-aozora-facade`'s `json` module along the already-existing seam
between typed constructors and string serialization:

- New feature `entries`: the projection types (`Node`, `Diagnostic`, `Pair`,
  `ContainerPair`, `GaijiResolution`, `Slug`) and the `*_entries()`
  constructor functions. These build structs from the parse tree and do not
  serialize; the feature must not enable `serde_json/preserve_order`
  (plain `serde` for derives is acceptable).
- Feature `json` (unchanged surface): the string/envelope serialization
  functions and schema emitters. Keeps
  `["dep:serde", "dep:serde_json", "serde_json/preserve_order"]` — upstream
  byte-stable envelopes still depend on it — and now also depends on
  `entries`.

This is the first semantic edit to a lifted crate. It is a fork-owned
divergence under ADR 0032 and is recorded as such in the provenance handoff
(LIFT_SET annotation: `ab-aozora-facade` no longer rename-only). The shim
keeps using `json` inside its excluded workspace until it is deleted;
`ab-aozora-aat` uses only `entries`.

Acceptance: with `ab-aozora-aat` and `ab-aozora` as workspace members,
`cargo tree -e features --workspace` shows no `preserve_order` activation
anywhere in the root workspace, and the full workspace test suite passes.

### Converter hardening

`ab-aat-to-parser-ir`'s canonical JSON output must not depend on
`serde_json`'s map type ordering. Canonicalization sorts keys explicitly at
the serialization call site (recursively, over an intermediate representation
whose ordering the code controls), so output bytes are identical whether or
not `preserve_order` is active in the build graph. A unit test proves sorting
on deliberately unsorted input constructed without relying on map iteration
order (i.e., the test input must be built through a path that preserves
insertion order regardless of serde_json features — not by round-tripping
through `serde_json::Value`).

This hardening must be a no-op on current output bytes: the existing 57-test
integration suite (which compares against canonical golden fixtures) is the
regression net and must pass unchanged.

### Residual rule

The Phase 2 hard constraint from the provenance handoff is narrowed, not
dropped: any *future* crate enabling `serde_json/preserve_order` must live
outside the root workspace unless every workspace consumer of canonical JSON
is order-independent by construction. The handoff's hazard section is updated
to state the narrowed rule and point at the feature split.

## New components

### `crates/ab-aozora-aat` (library, workspace member)

The owning home of AAT emission for the fork lane. Ports from
`adapters/aozora/src/lib.rs` (946 LOC), replacing the subprocess `inspect`
hop with direct facade calls:

- Shift_JIS/CP932 decode shims and the decoded-span bookkeeping
  (`decoded.span_text`), unchanged semantics.
- Envelope/warning handling: where the adapter deserialized
  `Envelope<AozoraNode>` etc. from subprocess JSON, `ab-aozora-aat` consumes
  the typed entry constructors (`node_entries`, `diagnostic_entries`,
  `gaiji_entries` — exposed under the new `entries` feature; whether they
  keep their current module path is an implementation detail of the split)
  and maps facade types to the ported internal types. The schemaVersion wire check disappears with the wire; the facade
  crate version recorded in `--version` replaces it as the compatibility
  coordinate.
- AAT assembly: the `json!`-based document construction
  (`aozora_body_text` body selection, `blocks_from_inline_content`, gaiji,
  warnings, meta) ported verbatim. Because `serde_json::Value` maps are
  BTreeMap-backed without `preserve_order`, verbatim porting yields
  byte-identical AAT except `/meta/adapter_version` — the parity gate's
  expectation.
- Public seam mirrors the adapter:
  `pub fn aat_json_from_bytes(bytes: &[u8]) -> Result<Vec<u8>>` plus an
  outcome type that distinguishes success / success-with-warnings / fatal so
  the binary can map to exit codes without re-parsing its own output.
- Depends on: `ab-aozora-facade` (features: `entries` only; **never**
  `json`), `ab-aozora-encoding` (via facade re-export or directly, matching
  what the port needs), `serde_json` (default features).

Unit tests: the adapter's existing in-crate tests (decode vectors, body
selection, block synthesis) are ported alongside the logic they cover — the
frozen original keeps its copies; duplication here is deliberate and dies
with the legacy lane.

### `crates/ab-aozora` (thin binary, workspace member)

The permanent harness-edge executable, per the parent design's Executable
Boundary contract:

- stdin: raw source bytes (Shift_JIS/CP932 as in the corpus); stdout: one
  AAT JSON document; nothing else on stdout.
- Exit codes: `0` success, `2` success-with-warnings, `1` fatal — the
  existing adapter wire contract
  (`docs/handoffs/adding-aozora2-aozoraepub3-parser-support.md`).
- Errors structured on stderr; no partial AAT on stdout after a fatal (build
  the full output in memory before writing).
- `--version` emits: adapter id (`ab-aozora`), adapter version (the
  `ab-aozora-aat` crate version), AAT schema version, and build identity
  (constituent `ab-aozora-*` crate versions; git rev when available at build
  time, `unknown` otherwise — never a build failure).
- No other flags or subcommands. Argument parsing stays std-only; CLI
  dependencies must not enter the library.

Binary name and crate name are both `ab-aozora`; a separate crate (not a
`[[bin]]` target inside `ab-aozora-aat`) keeps the library's dependency
surface free of CLI concerns and gives nix a clean package target.

### Deleted at end of phase

`crates/ab-aozora-cli` (the shim): crate directory, its standalone
`[workspace]` + `Cargo.lock`, its goldens and `AB_UPSTREAM_AOZORA_BIN` test
wiring, and the root `Cargo.toml` `exclude` entry with its hazard comment
(the narrowed rule moves to the provenance handoff). A repo-wide grep for
`ab-aozora-cli` (docs excluded where historical: frozen reports, ledger,
handoff history) gates the removal — no dangling references in build files,
nix packaging, justfile, or harness code.

## Harness lane for the candidate (prerequisite for every gate)

`run-aat-full.sh --aozora-bin` does **not** select the adapter executable: it
exports `AB_AOZORA_BIN` — the inner inspect-protocol parser the nix-built
`aozora-adapter` spawns (`aozora inspect {nodes,diagnostics,gaiji} -`) —
while `ab-check` continues to invoke `aozora-adapter`. `ab-aozora` speaks no
inspect protocol, so it cannot ride that option. The candidate needs its own
adapter lane; this is a real harness-interface change:

- `run-aat-full.sh` gains adapter id `ab-aozora` (ADAPTER enum becomes
  `aozora | ab-aozora | aozora2html | aozora-epub3`). `ab-check --adapter`
  invokes the `ab-aozora` binary directly — it already implements the
  adapter wire contract (stdin bytes → AAT on stdout, exit 0/2/1).
- Default resolution is the flake package (`$repo_root#ab-aozora`); a new
  `--adapter-bin PATH` override (valid only with `--adapter ab-aozora`)
  carries the same discipline as `--aozora-bin`: absolutized, `-x` checked,
  `--version` and sha256 recorded verbatim in `metadata.json`, hashed into
  `input_set_hash`, exit 2 on failure, and the binary's containing directory
  standing in for the tree-hash staleness plumbing so a differing override
  is never served a stale dump.
- This lane has **no renderer identity**: the adapter binary is the complete
  generator identity (`adapter_hash_target` is the `ab-aozora` binary;
  renderer fields are absent for this adapter id). Metadata identifies the
  directly invoked adapter.
- `--aozora-bin` remains exclusively the legacy adapter's inner-parser
  override; the two options are mutually exclusive by construction (each is
  valid only with its own adapter id).
- Tests: an `ab-aozora`-lane smoke in the pattern of
  `tests/aozora-bin-override-smoke.sh` proving the override binary is what
  actually executes and that a differing binary changes recorded identity
  (staleness not served).

`reports/aat-fidelity/run-perf-workset.py` has the same conflation: it
hardcodes `<bin> inspect nodes -`. It gains per-lane invocation forms — each
lane is an explicit argv (run with the work's bytes on stdin) plus optional
env (e.g. `AB_AOZORA_BIN` for the adapter lane) — with its unit tests
extended accordingly.

## Gates

Gate order: absorption parity → perf → conformance echo → only then harness
repoint, shim deletion, and identity rotation.

### Absorption parity gate (corpus-wide, hinoki)

- Reference: a fresh full-corpus AAT dump via the frozen `adapters/aozora`
  lane, resolved fail-closed through `run-sets/current.json` with content
  hash verified — the same discipline as Phase 1 Gate A. (If the current
  run-set already points at a verified adapter-lane dump, reuse it; never an
  unpinned directory.)
- Candidate: full-corpus AAT via `--adapter ab-aozora --adapter-bin PATH`
  (the branch-built binary), on hinoki, with the binary's sha256 and
  `--version` recorded in the dump metadata.
- Comparator — **byte parity is the gate**. The existing
  `compare-aat-dumps.py` is semantic: it parses JSON and ignores key order
  and numeric formatting, so it cannot enforce the port-fidelity claim on
  its own. It gains a byte-parity mode (or a sibling comparator, decided at
  plan time) that, per work: parses each dump only to extract the
  `/meta/adapter_version` value, requires the exact serialized occurrence
  `"adapter_version":"<escaped value>"` to appear exactly once in the raw
  bytes (fail closed otherwise), substitutes a fixed placeholder in both
  documents, then compares the remaining bytes. Byte and semantic results
  are reported separately: byte parity across all works is the blocking
  requirement; the semantic comparator runs as the localization diagnostic
  when byte parity fails. Unit tests cover the substitution edge cases
  (value occurring in text content, escaping) — these join the `reports/**`
  pytest surface being wired into CI.
- Required result: 0 missing, 0 byte-divergent, 0 fatal-error works across
  all 17,886.
- Legacy span semantics are deliberately preserved for this gate (sanitized
  byte offsets, `line_start`/`line_end` = 1); the span fix is Phase 3's
  identity-rotated step.

### Performance gate

Per the parent design's protocol: `perf-workset-v1` (hash-pinned, fail-closed
sha256), `--release` with sccache disabled, 1 warm-up + ≥5 measured runs per
lane, both lanes measured fresh in the same session on the same recorded
machine identity (hinoki). Lanes, via the perf runner's new argv forms:

- baseline: the frozen adapter end-to-end (`aozora-adapter` argv, stdin→AAT,
  `AB_AOZORA_BIN` = pinned upstream) — the full legacy cost including its
  three inspect subprocess round-trips;
- candidate: `ab-aozora` argv, stdin→AAT.

This is an end-to-end AAT-production comparison, deliberately different from
the Phase 1 perf report's inner-binary `inspect nodes` measurement; the
report records the mode change, and comparisons stay within this session.
Blockers: any new timeout (unconditional), or >10% median wall-time
regression. `ab-aozora` should be *faster* (no subprocess hops); a slowdown
indicates a port defect even under threshold — record and investigate before
proceeding.

### Conformance echo check (local, cheap)

The conformance harness (`reports/parser-conformance/run-aozora-notation-spec.py`)
already supports `aat`-mode adapters. Score both vector suites (127 P4suta +
30 official-docs seed) in `aat` mode against the frozen adapter lane and
against `ab-aozora`: scores must be identical vector-for-vector. This adds a
second, independent instrument over the same equivalence the corpus gate
measures, at negligible cost. The pinned upstream binary remains the
`inspect`-mode comparator via the legacy lane; `inspect`-mode scoring of the
fork ends with the shim.

### Gate checkpoint contract (evidence before deletion)

Prose ordering is not enforcement: a single branch could contain the shim
deletion whether or not the hinoki evidence ever passed. The boundary is
therefore an evidence-bearing checkpoint:

- Each of the three gates freezes a report under
  `docs/superpowers/reports/` recording: the candidate source commit, the
  candidate binary's sha256 and verbatim `--version` output, the reference
  identity (run-set id + content hash for parity; baseline argv + binary
  identity for perf), and the verdict.
- The three reports must be **committed** before any deletion, harness
  default, or identity-rotation change is made, and all three must cite the
  same candidate commit and `--version` identity — a mixed-candidate
  evidence set is invalid.
- The deletion/rotation work starts with an explicit verification step
  (checked in the implementation plan, recorded in the progress ledger):
  the three reports exist in the tree, each states its passing verdict, and
  the cited candidate identities agree. The deletion/rotation commit
  messages cite the three report paths.
- Frozen reports are never rewritten (standing evidence rule); a re-run
  after a fix produces a new report superseding the old one by reference.

## Harness repoint (after gates pass)

- `reports/aat-fidelity/run-aat-full.sh`: the `ab-aozora` adapter id (added
  as the gate prerequisite above) becomes a first-class measurement lane,
  nix-resolved by default. `--aozora-bin` keeps its existing meaning (legacy
  adapter's inner-parser override) untouched. `--adapter aozora` remains the
  default comparison lane until Phase 4 activation — flipping which adapter
  id publication measurement *defaults to* is part of the Phase 4 cutover,
  not Phase 2.
- `reports/aat-fidelity/measure-parser-performance.py`: gains the
  `ab-aozora` lane; its schemaVersion-1 drift (advertised handling that no
  longer matches any adapter) is fixed in the same change (folded Minor).
- `justfile` `aozora-notation-spec-comparison`: `AOZORA_BIN` gains an
  existence/executability guard with a clear error before any run starts
  (folded Minor).
- `reports/**` Python unit tests (`test_compare_aat_dumps.py`, the perf
  runner's tests, and any added by this phase) are wired into the repository
  check gate so harness regressions fail pre-merge (folded Minor). Scope is
  unit tests only — corpus-scale runs stay manual/hinoki.
- Evidence discipline is unchanged and binding: explicit binary parameters
  everywhere (`AOZORA_BIN`/`--aozora-bin`/`--adapter-bin`; ambient env never
  trusted),
  run-set fail-closed reference resolution, frozen evidence JSONs never
  rewritten (Phase 1 artifacts keep their historical version strings).

## Identity rotation (last work item)

New exact adapter identity: id `ab-aozora`, version = the `ab-aozora-aat`
crate version at rotation. Under ADR 0023:

- Fresh conversion-audit evidence generated with the new identity over the
  parity-gated AAT (compatibility evidence, not parser-selection evidence —
  the ADR 0030 boundary).
- Exact-match registry row(s) in `abc/data/aat-parser-ir-compatibility.edn`
  citing that evidence. No wildcard entries, no carried-over rows from the
  `aozora` (upstream-pinned) identity.
- The identity lands **unadmitted** for publication: per the parent design's
  Legacy Lane Retention, registry activation of the fork's identity is the
  atomic Phase 4 step. Phase 2 changes no publication lane behavior.
- Manifest identity rules untouched (ADR 0001/0023): parser identity lives
  entirely in adapter/mapping coordinates.

The legacy lane (upstream-aozora-src flake input + `AB_AOZORA_BIN` + the
`adapters/aozora` subprocess path) is demoted to comparison-only: still
building, still runnable, referenced by comparison tooling only. Nothing is
deleted from it.

## Out of scope

- Span semantics stay legacy — the parity gate depends on it; the
  `decoded_utf8`/real-line fix is Phase 3's own identity-rotated step.
- No capability changes (the 3 diagnostics `must` fixes and the
  `jizume`/`yokogumi`/`keigakomi` classifiers are Phase 3).
- No AAT schema changes, no `ab-aat-to-parser-ir` mapping changes (the
  hardening above changes serialization mechanics, not mapping semantics —
  output bytes are proven unchanged).
- No changes to `ab-source-syntax`, other adapters, or publication lanes.
- No default-lane repoint: harnesses *can* run `ab-aozora` explicitly; the
  default lane flips at Phase 4 activation.

## Testing summary

- Facade split: existing facade tests green under both feature sets;
  workspace feature-graph assertion (`no preserve_order in root workspace`).
- Converter hardening: sorting unit test on order-preserving input; existing
  57-test integration suite unchanged (byte-level no-op proof).
- `ab-aozora-aat`: ported adapter unit tests; facade-consumption mapping
  tests where adapter deserialization tests don't transfer directly.
- `ab-aozora`: exit-code contract tests (success / warnings / fatal), no
  partial-output-on-fatal test, `--version` field presence test.
- Harness: `ab-aozora`-lane smoke test (override executes + identity
  changes), byte-parity comparator unit tests (substitution edge cases),
  perf-runner lane-argv unit tests — all on the `reports/**` pytest / test
  surface wired into CI this phase.
- Gates: corpus byte parity (hinoki), perf protocol (hinoki), conformance
  echo (local), each with a frozen evidence report under
  `docs/superpowers/reports/` satisfying the checkpoint contract.
- Post-deletion: full workspace suite + shim-reference grep gate.

## Risks and open points

- **Port fidelity is the phase's central risk.** Mitigated by copy-not-move
  (the frozen original is always diffable), verbatim `json!` porting, and
  the byte-parity comparator: serialization-order or formatting drift fails
  the gate rather than hiding behind a semantic comparison.
- **Facade type mapping.** The adapter's `AozoraNode`/`AozoraDiagnostic`/
  `AozoraGaiji` types were shaped by the wire JSON; the facade's entry types
  are shaped by upstream internals. Field-level mismatches (naming, optional
  fields, enum spellings) surface as parity diffs; the mapping layer is where
  port defects concentrate — review focus goes there.
- **`entries` feature split may not be clean** if upstream's entry
  constructors reach into serialization helpers. Discovered at
  implementation time; the fallback is moving the typed layer into a new
  fork-owned module rather than re-gating upstream code in place. Either
  way the acceptance criterion (no `preserve_order` in the root workspace
  feature graph) is unchanged.
- **Perf surprise.** Removing subprocess hops should help; if the fresh
  measurement shows regression, the phase blocks pending investigation even
  under threshold (stated in the perf gate).

## Rollback

- Before shim deletion lands: revert the branch; nothing outside it changed.
- After landing: the legacy lane is intact and all harness bindings are
  explicit parameters, so pointing measurement back at the pinned upstream
  binary is a parameter change, not a code revert. Registry rows for the
  unadmitted `ab-aozora` identity can sit inert indefinitely; removing them
  is a data change under ADR 0023 with no publication impact.
- Reversal of the absorption itself (abandoning `ab-aozora-aat`) would be a
  new decision against ADR 0030/0032 — out of scope here; the frozen legacy
  lane is what makes it cheap if ever taken.
