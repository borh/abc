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

## Gates

Gate order: absorption parity → perf → conformance echo → only then harness
repoint, shim deletion, and identity rotation.

### Absorption parity gate (corpus-wide, hinoki)

- Reference: a fresh full-corpus AAT dump via the frozen `adapters/aozora`
  lane, resolved fail-closed through `run-sets/current.json` with content
  hash verified — the same discipline as Phase 1 Gate A. (If the current
  run-set already points at a verified adapter-lane dump, reuse it; never an
  unpinned directory.)
- Candidate: full-corpus AAT via the `ab-aozora` binary under explicit
  `--aozora-bin`, on hinoki, with the binary's sha256 and `--version`
  recorded in the dump metadata.
- Comparator: the existing `reports/aat-fidelity/compare-aat-dumps.py`
  semantic comparator with its single allowlisted pointer
  `/meta/adapter_version`. Expectation is byte-equality modulo that pointer;
  any other divergence is a port defect. Required result: 0 missing,
  0 diverged, 0 fatal-error works across all 17,886.
- Legacy span semantics are deliberately preserved for this gate (sanitized
  byte offsets, `line_start`/`line_end` = 1); the span fix is Phase 3's
  identity-rotated step.

### Performance gate

Per the parent design's protocol: `perf-workset-v1` (hash-pinned, fail-closed
sha256), `--release` with sccache disabled, 1 warm-up + ≥5 measured runs per
binary, both lanes measured fresh in the same session on the same recorded
machine identity (hinoki). Lanes: pinned upstream binary via the frozen
adapter path vs `ab-aozora`. Blockers: any new timeout (unconditional), or
>10% median wall-time regression on the workset. `ab-aozora` should be
*faster* (one process, no inspect subprocess round-trips); a slowdown
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

## Harness repoint (after gates pass)

- `reports/aat-fidelity/run-aat-full.sh`: `--aozora-bin` now names the
  `ab-aozora` binary for the fork lane; identity recording (sha256 +
  `--version`) unchanged. The default (no override) remains the legacy
  pinned-upstream lane until Phase 4 activation — repointing the *default*
  is part of the Phase 4 cutover, not Phase 2.
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
- Evidence discipline is unchanged and binding: explicit
  `AOZORA_BIN`/`--aozora-bin` everywhere (ambient env never trusted),
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
- Gates: corpus parity (hinoki), perf protocol (hinoki), conformance echo
  (local), each with a frozen evidence report under
  `docs/superpowers/reports/`.
- Post-deletion: full workspace suite + shim-reference grep gate.

## Risks and open points

- **Port fidelity is the phase's central risk.** Mitigated by copy-not-move
  (the frozen original is always diffable), verbatim `json!` porting, and
  the corpus-wide byte-level expectation — semantic-only divergences cannot
  hide behind the comparator because the expectation is byte-equality modulo
  one pointer.
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
