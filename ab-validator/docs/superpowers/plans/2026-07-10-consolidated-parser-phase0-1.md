# Consolidated Parser Phase 0+1 (ADR 0031 + Lift + Shim + Parity) Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Land ADR 0031 (hard-detach governance), lift the aozora parser
core into `ab-validator/crates/` as `ab-*` crates, ship the throwaway
`ab-aozora-cli` inspect shim, and prove full parity (conformance + perf +
full corpus) against the pinned upstream binary.

**Architecture:** Strangler parity-first per
`docs/superpowers/specs/2026-07-10-consolidated-parser-design.md`. Phase 1
changes no parser semantics: verbatim crate lift (rename-only), a shim that
reproduces the upstream `aozora inspect` protocol byte-for-byte, explicit
`--aozora-bin`/`AOZORA_BIN` harness parameters so gates provably run the
shim, and three measured gates (Gate B conformance, perf gate, Gate A
corpus). Phases 2–4 get their own plans afterwards.

**Tech Stack:** Rust (edition 2024, `ab-validator` workspace), Nix flake
(pinned `upstream-aozora-src` = `P4suta/aozora@1a4f864`), Clojure tooling on
the abc side (ADR diagrams), Python report harnesses.

## Global Constraints

- Hard-detach rev (verbatim everywhere): `1a4f864603970983719655aa4af4525958ac2d38`.
- Lifted code is rename-only in this plan: **zero semantic edits** to lifted
  logic. Any behavior diff found by a gate is a lift defect to fix by
  re-copying, never by "improving" code.
- Crate naming: upstream `aozora-<x>` → `ab-aozora-<x>`; the umbrella crate
  `aozora` → `ab-aozora-facade`. Package + lib names renamed, module
  structure otherwise untouched.
- **No inherited test is deleted.** Test-only workspace deps are lifted too
  (`aozora-proptest` → `ab-aozora-proptest`); `proptest`/`insta` stay as
  crates.io dev-deps at the upstream workspace versions. Only `criterion`
  `[[bench]]` targets are dropped, and the drop is recorded in the test
  inventory (Task 2).
- License/attribution: upstream is `MIT OR Apache-2.0`; copy upstream
  `NOTICE` + both LICENSE files into each lifted crate; every lifted crate's
  `src/lib.rs` starts with the provenance header (Task 3 Step 3).
- All cargo commands: `export RUSTC_WRAPPER= SCCACHE_DISABLE=1` first.
- Never pipe a gate command into `tail`/`grep` without `set -o pipefail` —
  gates must fail on the command's own exit status.
- Harness runs must pass the binary under test **explicitly**
  (`AOZORA_BIN=` justfile arg / `--aozora-bin` flag from Task 6); ambient
  `AB_AOZORA_BIN` alone is not trusted evidence because the harnesses
  re-resolve from the nix store.
- Reference data resolves fail-closed through
  `reports/aat-fidelity/run-sets/current.json` + `AB_DB_ROOT` (never
  hardcode `/db/...`).
- Full-corpus and perf runs happen on the heavy-compute host (hinoki);
  baseline and candidate perf runs must run on the same host.
- Tasks state their working directory in Step 1 and keep every path in the
  task relative to it; commits run from that same directory.
- Commit message style: conventional commits as in recent history.

---

### Task 1: ADR 0031 — hard-detach amendment (abc side)

**Files:**
- Create: `abc/docs/adr/0031-parser-fork-hard-detach.md`
- Modify: `abc/docs/adr/0030-aozora-parser-selection.md` (header: add `Amended by: ADR 0031`)
- Modify (generated): `abc/docs/adr/adr-graph.mmd` (via `clojure -M:abc/diagrams`)

**Interfaces:**
- Consumes: ADR 0030 (Accepted, 2026-07-10), ADR README status vocabulary
  and edit policy (`abc/docs/adr/README.md`).
- Produces: ADR 0031 (Accepted) that later tasks and the fork-provenance
  handoff cite by number.

- [ ] **Step 1: Write ADR 0031** (working directory: `abc/`)

Create `docs/adr/0031-parser-fork-hard-detach.md`:

```markdown
# ADR 0031: Parser Fork Hard Detach

Status: Accepted
Date: 2026-07-10
Accepted: 2026-07-10
Supersedes: none
Amends: ADR 0030
Depends on: ADR 0030
Source: `ab-validator/docs/superpowers/specs/2026-07-10-consolidated-parser-design.md`

## Context

ADR 0030 selected `aozora-pipeline` (`P4suta/aozora`) as the consolidated
parser base with an upstream-first, fork-fallback engagement model. Design
brainstorming for the fork (2026-07-10) chose a different ownership model:
a library fork living in the `ab-validator` workspace, hard-detached at the
measured pin, with no future upstream merges.

The decision is a maintenance-economics tradeoff, not a measurement
problem (a pinned revision does not move). The two candidate models:

- **Merge relationship**: track upstream releases; every merge re-imports
  an actively developed ~90k-LOC workspace under our gates and forces a
  full re-measure cycle (the 2026-07-08 study's §7 controlled re-pin cost
  a session even for a measurement-equivalent bump). Our Level-3
  publication structures modify parse output invasively, so merges would
  conflict in exactly the code we change most.
- **Hard detach with selective porting**: no standing merge burden; an
  upstream change we want is a deliberate, reviewed patch with its own
  re-measure. Cost is forfeiting upstream fixes by default and owning all
  future parser work with current staffing (a single maintainer, working
  in bounded sessions).

Expected divergence favors detaching: the fork's roadmap (native AAT
emission, source-region model, Level-3 structures) rewrites the projection
surface upstream has no reason to accept wholesale, while the parsing core
we inherit is already corpus-clean at the pin (coverage 0.969, zero
timeouts). The porting lane stays open and cheap relative to merging.

## Decision

- The consolidated parser is a hard fork of `P4suta/aozora` at rev
  `1a4f864603970983719655aa4af4525958ac2d38` (the measured pin of the
  ADR 0030 evidence). No future merges from upstream.
- ADR 0030's upstream-first engagement model is replaced: upstream
  contributions are no longer part of the parser plan. If a later upstream
  change is wanted, it is ported as a reviewed patch with re-measurement,
  not a merge.
- Lifted crates live in `ab-validator/crates/` under `ab-aozora-*` names
  (`aozora` umbrella → `ab-aozora-facade`), inside the workspace gates.
  Attribution is preserved: the upstream `NOTICE` file travels with the
  lifted code and each lifted crate records the upstream repository and
  rev in a provenance header. Upstream license is `MIT OR Apache-2.0`,
  matching the workspace.
- **Revisit trigger:** re-open this decision by superseding ADR if either
  (a) upstream ships a capability the acceptance criteria need whose port
  is estimated at more than two focused sessions, or (b) fork-only
  maintenance (excluding planned roadmap work) exceeds roughly one session
  per month over a quarter.
- All other ADR 0030 decisions (selection evidence, candidate dispositions,
  selection-is-not-admission, reversibility via adapter/mapping
  coordinates) are unchanged.

## Consequences

- Upstream fixes and features after the pin are forfeited by default;
  porting one is a deliberate, measured act.
- The fork's identity chain starts at the pin: parser identity is carried
  by the `ab-aozora` adapter/version coordinates under ADR 0023, exactly as
  for any adapter.
- The `upstream-aozora-src` flake input remains until the legacy comparison
  lane is retired after Phase 4 admission (design: Legacy Lane Retention).

## Acceptance Criteria

- ADR 0030 carries the reciprocal `Amended by: ADR 0031` header link,
  enforced by the header-hygiene lint proven in
  `test/abc/tools/diagram/adr_graph_test.clj`.
- The fork-provenance handoff
  (`ab-validator/docs/handoffs/2026-07-10-parser-fork-provenance.md`)
  records the detach rev, lifted-crate provenance, and test inventory.

## Rollback

Superseding ADR re-establishing an upstream relationship (or a different
base per ADR 0030's rollback path); no registry, manifest, or identity
change is implied by this ADR itself.
```

- [ ] **Step 2: Add the reciprocal link to ADR 0030**

In `docs/adr/0030-aozora-parser-selection.md`, change the header block

```markdown
Supersedes: none
Amends: ADR 0002
```

to

```markdown
Supersedes: none
Amended by: ADR 0031
Amends: ADR 0002
```

- [ ] **Step 3: Regenerate diagrams and run the ADR gates**

```bash
clojure -M:abc/diagrams
bash nix/check-acceptance-criteria.sh
clojure -M:test:kaocha -m kaocha.runner --focus abc.tools.diagram.adr-graph-test
```

Expected: `wrote docs/adr/adr-graph.mmd`; gate exits 0; kaocha reports
0 failures.

- [ ] **Step 4: Commit** (still in `abc/`)

```bash
git add docs/adr/0031-parser-fork-hard-detach.md docs/adr/0030-aozora-parser-selection.md docs/adr/adr-graph.mmd
git commit -m "docs(adr): ADR 0031 hard-detach amendment to parser selection"
```

---

### Task 2: Dependency-closure discovery + fork-provenance handoff

**Files:**
- Create: `ab-validator/docs/handoffs/2026-07-10-parser-fork-provenance.md`

**Interfaces:**
- Consumes: pinned upstream source via
  `nix eval --raw .#upstream-parser-aozora.src`.
- Produces: the confirmed crate list `LIFT_SET` that Task 3 lifts, plus the
  test inventory, recorded in the handoff. Expected LIFT_SET (verify,
  don't assume): `aozora-spec, aozora-veb, aozora-encoding, aozora-scan,
  aozora-syntax, aozora-pipeline, aozora-render, aozora` (umbrella), plus
  test-support `aozora-proptest`.

- [ ] **Step 1: Resolve the pinned source and compute the closure**
  (working directory: `ab-validator/`)

```bash
SRC=$(nix eval --raw .#upstream-parser-aozora.src)
echo "$SRC"
for c in aozora aozora-render aozora-pipeline aozora-syntax aozora-scan aozora-spec aozora-veb aozora-encoding aozora-proptest; do
  echo "== $c";
  sed -n '/^\[dependencies\]/,/^\[dev-dependencies\]\|^\[build-dependencies\]\|^\[features\]\|^\[\[/p' \
    "$SRC/crates/$c/Cargo.toml" | grep -E '^aozora'
done
```

Expected edges: `aozora (umbrella) → render, pipeline, syntax, spec,
encoding` (+ optional serde/serde_json behind the `json` feature);
`pipeline → spec, syntax, encoding, scan`; `syntax → spec, veb, encoding`;
`scan → spec`; `render → <record>`; `spec`/`encoding` → none (record
`encoding` build-deps). If `aozora-render` pulls further `aozora-*` crates,
they join LIFT_SET — record each addition.

- [ ] **Step 2: Confirm the umbrella's `cst`/`query` re-exports are feature-gated off**

`$SRC/crates/aozora/src/lib.rs` has `pub mod cst` / `pub mod query`
re-export blocks. Verify they are behind optional features that default
off:

```bash
sed -n '/^\[features\]/,/^\[/p' "$SRC/crates/aozora/Cargo.toml"
grep -n 'feature = "cst"\|feature = "query"\|cfg(feature' "$SRC/crates/aozora/src/lib.rs" | head
```

Expected: `cst`/`query` are optional and not in `default`. If they are NOT
optional, STOP: LIFT_SET must grow (`aozora-cst`, `aozora-query`) — record
the growth and its own dependency edges before proceeding.

- [ ] **Step 3: Record external (crates.io) dependencies and the upstream version table**

```bash
for c in aozora aozora-render aozora-pipeline aozora-syntax aozora-scan aozora-spec aozora-veb aozora-encoding aozora-proptest; do
  echo "== $c";
  sed -n '/^\[dependencies\]/,/^\[dev-dependencies\]\|^\[\[/p' "$SRC/crates/$c/Cargo.toml" | grep -v '^aozora' | grep '='
done
sed -n '/^\[workspace.dependencies\]/,/^\[/p' "$SRC/Cargo.toml"
```

Record both outputs — Task 3 copies exact versions from the workspace
table; Task 4 audits the surface.

- [ ] **Step 4: Build the inherited test inventory**

```bash
for c in aozora aozora-render aozora-pipeline aozora-syntax aozora-scan aozora-spec aozora-veb aozora-encoding; do
  echo "== $c";
  ls "$SRC/crates/$c/tests" 2>/dev/null;
  grep -rl "#\[test\]\|proptest!" "$SRC/crates/$c/src" | head;
  grep -n "^\[\[bench\]\]" -A1 "$SRC/crates/$c/Cargo.toml"
done
```

Record per crate: integration test files, source files containing unit/
property tests, and bench targets (the only thing dropped).

- [ ] **Step 5: Write the fork-provenance handoff**

Create `docs/handoffs/2026-07-10-parser-fork-provenance.md`:

```markdown
# Parser fork provenance (hard detach)

Date: 2026-07-10
Authority: ADR 0030 (selection), ADR 0031 (hard detach),
`docs/superpowers/specs/2026-07-10-consolidated-parser-design.md`.

## Detach point

- Upstream: `github.com/P4suta/aozora`
- Rev: `1a4f864603970983719655aa4af4525958ac2d38` (flake input
  `upstream-aozora-src`, locked 2026-07-08)
- License: MIT OR Apache-2.0 (upstream NOTICE + LICENSE files copied into
  each lifted crate)

## Lifted crates (LIFT_SET)

| upstream crate | fork crate | workspace-internal deps |
| --- | --- | --- |
| aozora-spec | ab-aozora-spec | (none) |
| aozora-veb | ab-aozora-veb | <from Step 1> |
| aozora-encoding | ab-aozora-encoding | (none; build-deps: <from Step 1>) |
| aozora-scan | ab-aozora-scan | spec |
| aozora-syntax | ab-aozora-syntax | spec, veb, encoding |
| aozora-pipeline | ab-aozora-pipeline | spec, syntax, encoding, scan |
| aozora-render | ab-aozora-render | <from Step 1> |
| aozora (umbrella) | ab-aozora-facade | render, pipeline, syntax, spec, encoding |
| aozora-proptest (test-support) | ab-aozora-proptest | <from Step 1> |

Not lifted: aozora-cst / aozora-query (feature-gated off in the umbrella —
Step 2 evidence), aozora-cli (the shim in crates/ab-aozora-cli reimplements
only the 3-kind inspect dispatch over `ab_aozora_facade::json`), and all
bindings/tooling crates per the design's minimal-core decision.

## External dependency surface added

<paste Step 3 output + Task 4 resolved tree>

## Test inventory

| crate | inherited tests (files) | retained | dropped |
| --- | --- | --- | --- |
| <one row per LIFT_SET crate from Step 4> | | all | benches only: <list> |

Rule: no inherited test deleted; `[[bench]]` targets dropped (criterion is
not lifted). Any deviation must be recorded here with a reason before it
lands.

## Verbatim-lift statement

Phase 1 lifts are rename-only (package/lib names, intra-workspace dep
paths, provenance headers). No semantic edits. Gate evidence:
- Gate B: <link conformance comparison report when Task 7 lands>
- Perf gate: <link perf parity report when Task 8 lands>
- Gate A: <link corpus parity report when Task 9 lands>
```

Cells marked `<from Step N>` are filled from this task's command output
before committing; the three gate links are filled by Tasks 7–9.

- [ ] **Step 6: Commit**

```bash
git add docs/handoffs/2026-07-10-parser-fork-provenance.md
git commit -m "docs(handoffs): parser fork provenance, lift closure, test inventory"
```

---

### Task 3: Lift LIFT_SET into `ab-validator/crates/` (rename-only)

**Files:**
- Create: `ab-validator/crates/ab-aozora-{spec,veb,encoding,scan,syntax,pipeline,render,facade,proptest}/` (full crate trees)
- Modify: `ab-validator/Cargo.toml` (workspace members)

**Interfaces:**
- Consumes: LIFT_SET and `$SRC` from Task 2.
- Produces: building crates with all inherited tests green;
  `ab-aozora-facade` (with `json` feature) is the library Task 5 links
  against.

- [ ] **Step 1: Copy the crates** (working directory: `ab-validator/`)

```bash
SRC=$(nix eval --raw .#upstream-parser-aozora.src)
declare -A NAME=( [aozora]=facade [aozora-render]=render [aozora-pipeline]=pipeline \
  [aozora-syntax]=syntax [aozora-scan]=scan [aozora-spec]=spec [aozora-veb]=veb \
  [aozora-encoding]=encoding [aozora-proptest]=proptest )
for up in "${!NAME[@]}"; do
  dst="crates/ab-aozora-${NAME[$up]}"
  cp -r --no-preserve=mode "$SRC/crates/$up" "$dst"
  cp "$SRC/NOTICE" "$SRC/LICENSE-MIT" "$SRC/LICENSE-APACHE" "$dst/"
done
```

- [ ] **Step 2: Rename packages and rewrite dep tables**

For each lifted crate's `Cargo.toml`:
- `name = "ab-aozora-<new>"`, keep `edition`, set
  `license = "MIT OR Apache-2.0"`, `version = "0.1.0"`,
  `publish = false`.
- Replace `aozora-<x> = { workspace = true }` (and `path = "../aozora-<x>"`)
  entries with `ab-aozora-<new> = { path = "../ab-aozora-<new>" }`.
- Replace external `workspace = true` deps with the exact versions from the
  upstream `[workspace.dependencies]` table recorded in Task 2 Step 3
  (verbatim version strings + features — no guessed majors).
- Keep `proptest`/`insta` dev-deps (upstream versions);
  `aozora-proptest = { path = ... }` → `ab-aozora-proptest = { path = "../ab-aozora-proptest" }`.
- Delete `[[bench]]` sections, their `criterion` dev-dep, and `benches/`
  directories only.

Then rename source identifiers:

```bash
for pair in "aozora_render:ab_aozora_render" "aozora_pipeline:ab_aozora_pipeline" \
  "aozora_syntax:ab_aozora_syntax" "aozora_scan:ab_aozora_scan" "aozora_spec:ab_aozora_spec" \
  "aozora_veb:ab_aozora_veb" "aozora_encoding:ab_aozora_encoding" "aozora_proptest:ab_aozora_proptest"; do
  old="${pair%%:*}"; new="${pair##*:}"
  grep -rl "\b$old\b" crates/ab-aozora-*/src crates/ab-aozora-*/tests 2>/dev/null \
    | xargs -r sed -i "s/\b$old\b/$new/g"
done
# The umbrella crate is used as `aozora::…` only by external consumers; the
# shim will import it as `ab_aozora_facade`. Inside the lifted crates the
# umbrella is not a dependency, so no `aozora::` rewrite is needed — verify:
grep -rn "^use aozora::\|[^_]aozora::" crates/ab-aozora-*/src | grep -v "ab_aozora" | head
```

Expected for the final grep: no hits (if any appear, rewrite those
`aozora::` paths to `ab_aozora_facade::` — they can only occur in lifted
tests).

- [ ] **Step 3: Add the provenance header to each lifted crate**

Prepend to each `crates/ab-aozora-<new>/src/lib.rs`:

```rust
//! Forked from https://github.com/P4suta/aozora
//! at rev 1a4f864603970983719655aa4af4525958ac2d38 (hard detach; ADR 0031).
//! Upstream crate: <upstream name>. License: MIT OR Apache-2.0 (see NOTICE).
```

- [ ] **Step 4: Wire workspace membership**

Add the nine crates to `[workspace] members` in `Cargo.toml`, matching the
existing list style.

- [ ] **Step 5: Build and run every inherited test**

```bash
export RUSTC_WRAPPER= SCCACHE_DISABLE=1
cargo build -p ab-aozora-facade --features json --release
cargo test -p ab-aozora-spec -p ab-aozora-veb -p ab-aozora-encoding \
  -p ab-aozora-scan -p ab-aozora-syntax -p ab-aozora-pipeline \
  -p ab-aozora-render -p ab-aozora-facade -p ab-aozora-proptest
```

Expected: build exits 0; all inherited tests pass, including property
tests. Any failure is a rename defect — fix the rename. **Deleting a test
is not an available fix**; if a test cannot run at all (e.g. depends on a
non-lifted crate), STOP and record it in the test inventory with a reason,
then surface for review before proceeding.

- [ ] **Step 6: Workspace gates**

```bash
cargo fmt --check || cargo fmt
cargo clippy --workspace --all-targets -- -D warnings
```

Expected: clean. Formatting normalization is whitespace-only. Clippy lints
in lifted code are silenced with targeted `#[allow(...)]` +
`// lifted-code allowance` comments, not by rewriting logic.

- [ ] **Step 7: Commit**

```bash
git add crates/ab-aozora-* Cargo.toml Cargo.lock
git commit -m "feat(parser): lift aozora parser core at 1a4f864 as ab-aozora-* crates"
```

---

### Task 4: cargo-deny audit of the inherited dependency surface

**Files:**
- Modify: `ab-validator/deny.toml` (only if a new license/advisory entry is needed)
- Modify: `ab-validator/docs/handoffs/2026-07-10-parser-fork-provenance.md` (record results)

**Interfaces:**
- Consumes: workspace with lifted crates (Task 3).
- Produces: clean `cargo deny check` and the recorded new transitive
  surface.

- [ ] **Step 1: Run the audit directly (no pipes)** (working directory: `ab-validator/`)

```bash
export RUSTC_WRAPPER= SCCACHE_DISABLE=1
cargo deny check
```

Expected: exit 0 (read the full output; do not filter it through a pipe).
If the repository exposes a nix-backed deny check (look for it:
`nix flake show 2>/dev/null | grep -i deny`), run that form instead and
record which was used. If a lifted dependency trips a license or advisory
rule, STOP and surface it for a scoping decision — do not silently
allowlist.

- [ ] **Step 2: Record the added surface in the provenance handoff**

```bash
cargo tree -p ab-aozora-facade --features json -e normal --prefix none | sort -u > /tmp/fork-tree.txt
```

Append the resolved third-party crates (minus pre-existing workspace deps)
under "External dependency surface added".

- [ ] **Step 3: Commit**

```bash
git add deny.toml docs/handoffs/2026-07-10-parser-fork-provenance.md
git commit -m "chore(parser): audit lifted dependency surface"
```

---

### Task 5: `ab-aozora-cli` shim (inspect protocol + `--version`, byte-compatible)

**Files:**
- Create: `ab-validator/crates/ab-aozora-cli/Cargo.toml`
- Create: `ab-validator/crates/ab-aozora-cli/src/main.rs`
- Create: `ab-validator/crates/ab-aozora-cli/tests/golden.rs`
- Modify: `ab-validator/Cargo.toml` (workspace member)

**Interfaces:**
- Consumes: `ab-aozora-facade` with the `json` feature (Task 3) — the
  `aozora::json` envelope functions and `Document` parse entry.
- Produces: binary `ab-aozora-cli` supporting exactly
  `ab-aozora-cli inspect {nodes,diagnostics,gaiji} -` (stdin → stdout JSON
  `{"schemaVersion":2,"data":…}`, byte-identical to the pinned upstream
  `aozora inspect <kind> -`) and `ab-aozora-cli --version` (shim identity,
  consumed by `adapters/aozora`'s `adapter_version()` which shells out to
  `$AB_AOZORA_BIN --version`). Tasks 7 and 9 run the existing
  `adapters/aozora` against this binary.

- [ ] **Step 1: Map the upstream inspect call path** (working directory: `ab-validator/`)

```bash
SRC=$(nix eval --raw .#upstream-parser-aozora.src)
grep -n "inspect\|json::" "$SRC/crates/aozora-cli/src/main.rs" | sed -n '1,40p'
grep -n "pub fn" "$SRC/crates/aozora/src/json.rs"
"$(nix build .#upstream-parser-aozora --no-link --print-out-paths)/bin/aozora" --version
```

Record: (a) which `aozora::json` functions the CLI's inspect subcommand
calls for `nodes` / `diagnostics` / `gaiji`, (b) how the CLI reads `-`
(stdin) and constructs `Document` (parse options, diagnostic policy), and
(c) the upstream `--version` output shape. The wire structs/serializers
live in `aozora::json` (now `ab_aozora_facade::json`) — the shim calls
them; it copies nothing from the excluded CLI beyond the thin dispatch
pattern.

- [ ] **Step 2: Create the crate**

`crates/ab-aozora-cli/Cargo.toml`:

```toml
[package]
name = "ab-aozora-cli"
version = "0.1.0"
edition = "2024"
license = "MIT OR Apache-2.0"
publish = false
description = "Throwaway parity shim: upstream `aozora inspect` protocol over the lifted ab-aozora-facade. Deleted at Phase 2."

[dependencies]
ab-aozora-facade = { path = "../ab-aozora-facade", features = ["json"] }
```

(Add further deps only if Step 1 shows the dispatch needs them.)

- [ ] **Step 3: Write `src/main.rs`**

```rust
//! Parity shim (Phase 1 only): reproduces `aozora inspect
//! {nodes,diagnostics,gaiji} -` byte-for-byte over the lifted
//! ab-aozora-facade (fork of P4suta/aozora at
//! 1a4f864603970983719655aa4af4525958ac2d38; ADR 0031).
use std::io::{Read, Write};

const VERSION_LINE: &str = concat!(
    "ab-aozora-cli ",
    env!("CARGO_PKG_VERSION"),
    " (fork of P4suta/aozora @ 1a4f864, ADR 0031)"
);

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.iter().any(|a| a == "--version") {
        println!("{VERSION_LINE}");
        return;
    }
    let kind = match args.as_slice() {
        [_, cmd, kind, dash]
            if cmd == "inspect"
                && dash == "-"
                && matches!(kind.as_str(), "nodes" | "diagnostics" | "gaiji") =>
        {
            kind.clone()
        }
        _ => {
            eprintln!("usage: ab-aozora-cli inspect {{nodes|diagnostics|gaiji}} -  |  --version");
            std::process::exit(64);
        }
    };
    let mut source = String::new();
    if let Err(err) = std::io::stdin().read_to_string(&mut source) {
        eprintln!("ab-aozora-cli: read stdin: {err}");
        std::process::exit(1);
    }
    match emit(&kind, &source) {
        Ok(bytes) => {
            if std::io::stdout().lock().write_all(&bytes).is_err() {
                std::process::exit(1);
            }
        }
        Err(err) => {
            eprintln!("ab-aozora-cli: {err}");
            std::process::exit(1);
        }
    }
}

/// Calls the same `ab_aozora_facade::json` envelope functions the upstream
/// CLI's inspect subcommand calls (mapped in Task 5 Step 1), with the same
/// Document construction (parse options / diagnostic policy) and the same
/// trailing-newline behavior. Fill the three arms from the Step 1 mapping;
/// the golden test defines correctness.
fn emit(kind: &str, source: &str) -> Result<Vec<u8>, String> {
    match kind {
        "nodes" => todo_replace_with_mapped_call(source, "nodes"),
        "diagnostics" => todo_replace_with_mapped_call(source, "diagnostics"),
        "gaiji" => todo_replace_with_mapped_call(source, "gaiji"),
        _ => unreachable!("kind validated in main"),
    }
}
```

The `todo_replace_with_mapped_call` placeholder MUST be gone by Step 5 —
each arm becomes the direct `ab_aozora_facade::json::<fn>` call recorded in
Step 1 (`Document` built identically to upstream). The compiler enforces
removal (the helper is never defined).

- [ ] **Step 4: Write the golden byte-equality test**

`crates/ab-aozora-cli/tests/golden.rs`:

```rust
use std::io::Write;
use std::process::{Command, Stdio};

/// Runs `bin args… < input`, captures (exit_code, stdout_bytes).
fn run(bin: &str, args: &[&str], input: &[u8]) -> (i32, Vec<u8>) {
    let mut child = Command::new(bin)
        .args(args)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::null())
        .spawn()
        .unwrap_or_else(|e| panic!("spawn {bin}: {e}"));
    child.stdin.as_mut().unwrap().write_all(input).unwrap();
    let out = child.wait_with_output().unwrap();
    (out.status.code().unwrap_or(-1), out.stdout)
}

#[test]
fn inspect_matches_pinned_upstream_on_samples() {
    let upstream = std::env::var("AB_UPSTREAM_AOZORA_BIN")
        .expect("set AB_UPSTREAM_AOZORA_BIN to the pinned upstream aozora binary");
    let shim = env!("CARGO_BIN_EXE_ab-aozora-cli");
    let samples: &[&[u8]] = &[
        "表題\r\n著者\r\n\r\n-------------------------------------------------------\r\n【テキスト中に現れる記号について】\r\n《》：ルビ\r\n-------------------------------------------------------\r\n吾輩《わがはい》は猫である。※［＃「けものへん＋苗」、第3水準1-87-64］\r\n［＃５字下げ］一［＃「一」は中見出し］\r\n底本：「テスト」\r\n".as_bytes(),
        b"plain ascii only\n",
        "壊れた《ルビ\r\n".as_bytes(),
    ];
    for kind in ["nodes", "diagnostics", "gaiji"] {
        for (i, sample) in samples.iter().enumerate() {
            let (up_code, up_out) = run(&upstream, &["inspect", kind, "-"], sample);
            let (sh_code, sh_out) = run(shim, &["inspect", kind, "-"], sample);
            assert_eq!(up_code, sh_code, "exit code diverged: kind={kind} sample={i}");
            assert_eq!(
                up_out, sh_out,
                "stdout diverged: kind={kind} sample={i}"
            );
        }
    }
}

#[test]
fn version_flag_identifies_the_shim() {
    let shim = env!("CARGO_BIN_EXE_ab-aozora-cli");
    let (code, out) = run(shim, &["--version"], b"");
    assert_eq!(code, 0);
    let text = String::from_utf8(out).unwrap();
    assert!(text.contains("ab-aozora-cli"), "got: {text}");
    assert!(text.contains("1a4f864"), "got: {text}");
}
```

(The env var is `AB_UPSTREAM_AOZORA_BIN`, deliberately NOT `AB_AOZORA_BIN`,
so the test can never accidentally compare the shim to itself when the
adapter env var points at the shim.)

- [ ] **Step 5: Implement `emit` and run the golden test to green**

```bash
export RUSTC_WRAPPER= SCCACHE_DISABLE=1
export AB_UPSTREAM_AOZORA_BIN="$(nix build .#upstream-parser-aozora --no-link --print-out-paths)/bin/aozora"
cargo test -p ab-aozora-cli --test golden
```

Expected: first run fails to compile (`todo_replace_with_mapped_call`
undefined) — replace the three arms with the mapped
`ab_aozora_facade::json` calls, then re-run until PASS (byte-identical for
all 9 kind×sample cells + the version test). Iterate on Document options /
serializer settings / trailing newline; never edit lifted crates.

- [ ] **Step 6: Commit**

```bash
git add crates/ab-aozora-cli Cargo.toml Cargo.lock
git commit -m "feat(parser): ab-aozora-cli inspect shim with golden parity tests"
```

---

### Task 6: Explicit binary parameters for both harnesses

**Files:**
- Modify: `ab-validator/justfile` (recipe `aozora-notation-spec-comparison`: add `AOZORA_BIN=""` parameter)
- Modify: `ab-validator/reports/aat-fidelity/run-aozora-aat-full.sh` and/or `run-aat-full.sh` (add `--aozora-bin PATH`)
- Create: `ab-validator/tests/aozora-bin-override-smoke.sh`

**Interfaces:**
- Consumes: shim binary (Task 5).
- Produces: harness invocations where the binary under test is an explicit
  argument that controls BOTH execution and recorded generator identity;
  Tasks 7–9 use these parameters. Ambient `AB_AOZORA_BIN` is never the
  evidence path.

- [ ] **Step 1: Parameterize the justfile recipe** (working directory: `ab-validator/`)

In the `aozora-notation-spec-comparison` recipe, add an `AOZORA_BIN=""`
parameter (alongside `VECTORS`/`REPORT_MD`/`SUMMARY_JSON`) and replace the
unconditional store resolution

```make
aozora_pkg="$(nix build --no-link --print-out-paths '{{repo_root}}#upstream-parser-aozora')"; \
```

usage so the bin honors the override:

```make
aozora_bin="{{AOZORA_BIN}}"; \
if [ -z "$aozora_bin" ]; then \
  aozora_pkg="$(nix build --no-link --print-out-paths '{{repo_root}}#upstream-parser-aozora')"; \
  aozora_bin="$aozora_pkg/bin/aozora"; \
fi; \
```

and thread `$aozora_bin` into the existing `aozora_bin_q` quoting line in
place of `$aozora_pkg/bin/aozora`. Keep all other adapters' resolution
untouched.

- [ ] **Step 2: Add `--aozora-bin` to the full-corpus runner**

Locate the aozora-binary resolution in the runner scripts:

```bash
grep -n "AB_AOZORA_BIN\|upstream-parser-aozora" reports/aat-fidelity/run-aozora-aat-full.sh reports/aat-fidelity/run-aat-full.sh
```

Add a `--aozora-bin PATH` flag (parsed like the scripts' existing flags).
Behavior when set:
- skip the nix-store resolution for the aozora parser and
  `export AB_AOZORA_BIN="$PATH_ARG"`;
- record in the run's `metadata.json`: the override path, its sha256
  (`sha256sum`), and the output of `"$PATH_ARG" --version`;
- fail (exit 2) if the path does not exist or `--version` fails.
When not set: current pinned-store behavior, unchanged.

- [ ] **Step 3: Write the override smoke test**

`tests/aozora-bin-override-smoke.sh`:

```bash
#!/usr/bin/env bash
# Proves the harness override actually controls execution: a stub binary
# with a distinctive --version must appear in the run metadata, and the
# run must use it (the stub emits one fixed envelope).
set -euo pipefail
cd "$(dirname "$0")/.."
tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT
cat > "$tmp/stub-aozora" <<'EOF'
#!/usr/bin/env bash
if [ "${1:-}" = "--version" ]; then echo "stub-aozora 9.9.9-OVERRIDE-PROOF"; exit 0; fi
echo '{"schemaVersion":2,"data":{"nodes":[]}}'
EOF
chmod +x "$tmp/stub-aozora"
# Smallest possible corpus slice: run the aat runner in whatever single-work
# / smoke mode it supports (check its --help; use the existing smoke flags),
# pointed at the stub.
out="$tmp/out"
reports/aat-fidelity/run-aozora-aat-full.sh --aozora-bin "$tmp/stub-aozora" \
  --out-dir "$out" --jobs 1 $(reports/aat-fidelity/run-aozora-aat-full.sh --help 2>&1 | grep -q -- --limit && echo "--limit 1")
grep -q "OVERRIDE-PROOF" "$out"/metadata.json
echo "override smoke: OK"
```

(Adapt the single-work limiting flag to what the runner actually supports —
read its `--help`; if it has none, add `--limit N` while adding
`--aozora-bin` in Step 2.)

- [ ] **Step 4: Run the smoke test**

```bash
bash tests/aozora-bin-override-smoke.sh
```

Expected: `override smoke: OK`.

- [ ] **Step 5: Commit**

```bash
git add justfile reports/aat-fidelity/run-aozora-aat-full.sh reports/aat-fidelity/run-aat-full.sh tests/aozora-bin-override-smoke.sh
git commit -m "feat(harness): explicit aozora binary override with identity recording"
```

---

### Task 7: Gate B — conformance equivalence (127 vectors + 30-seed)

**Files:**
- Create: `ab-validator/docs/superpowers/reports/2026-07-10-fork-parity-conformance.md`
- Create (evidence): `ab-validator/docs/superpowers/reports/2026-07-10-fork-parity-conformance.{baseline,shim}.summary.json`
- Modify: `ab-validator/docs/handoffs/2026-07-10-parser-fork-provenance.md` (Gate B link)

**Interfaces:**
- Consumes: shim (Task 5), parameterized justfile recipe (Task 6).
- Produces: Gate B evidence; parity confirmed on both instruments.

- [ ] **Step 1: Baseline run with explicit output paths** (working directory: `ab-validator/`)

```bash
export RUSTC_WRAPPER= SCCACHE_DISABLE=1
just aozora-notation-spec-comparison \
  REPORT_MD=docs/superpowers/reports/2026-07-10-fork-parity-conformance.baseline.md \
  SUMMARY_JSON=docs/superpowers/reports/2026-07-10-fork-parity-conformance.baseline.summary.json
```

(No `AOZORA_BIN` → pinned upstream store binary; explicit paths avoid the
dated-default files entirely.)

- [ ] **Step 2: Shim run and diff**

```bash
cargo build -p ab-aozora-cli --release
just aozora-notation-spec-comparison \
  AOZORA_BIN="$PWD/target/release/ab-aozora-cli" \
  REPORT_MD=docs/superpowers/reports/2026-07-10-fork-parity-conformance.shim.md \
  SUMMARY_JSON=docs/superpowers/reports/2026-07-10-fork-parity-conformance.shim.summary.json
diff <(python3 -m json.tool docs/superpowers/reports/2026-07-10-fork-parity-conformance.baseline.summary.json) \
     <(python3 -m json.tool docs/superpowers/reports/2026-07-10-fork-parity-conformance.shim.summary.json)
```

Expected: no diff in aozora-adapter rows (if the summary embeds binary
paths/versions, those keys may differ — list any such key explicitly in
the Gate B report as identity-only). Any scoring diff = lift/shim defect;
return to Task 3/5, fix, re-run.

- [ ] **Step 3: Repeat for the official-docs seed**

Use the invocation recorded in
`docs/superpowers/reports/2026-07-09-official-docs-seed-expansion.md`, run
twice (pinned default vs `AOZORA_BIN=` shim via the same recipe/harness
parameter), diff the scored outputs the same way. Expected: identical
scores.

- [ ] **Step 4: Write the Gate B report and link it**

`docs/superpowers/reports/2026-07-10-fork-parity-conformance.md` records:
shim git rev + sha256, pinned binary store path, the two summary file
hashes per instrument, identity-only differing keys (if any), verdict
`FORK_CONFORMANCE_PARITY_CONFIRMED`. Add the link to the provenance
handoff's Gate B line. Delete the two intermediate `.baseline.md`/
`.shim.md` narrative files if redundant with the report (keep the
summary.json evidence files).

- [ ] **Step 5: Commit**

```bash
git add docs/superpowers/reports/2026-07-10-fork-parity-conformance* docs/handoffs/2026-07-10-parser-fork-provenance.md
git commit -m "test(parser): Gate B conformance parity for the lifted fork"
```

---

### Task 8: Perf workset pinning + Phase 1 performance gate

**Files:**
- Create: `ab-validator/data/perf-workset.json`
- Create: `ab-validator/reports/aat-fidelity/run-perf-workset.py`
- Create: `ab-validator/docs/superpowers/reports/2026-07-10-fork-parity-perf.md`
- Modify: `ab-validator/docs/handoffs/2026-07-10-parser-fork-provenance.md` (perf gate link)

**Interfaces:**
- Consumes: shim (Task 5); work ids from
  `docs/superpowers/reports/2026-07-08-parser-performance-sample.json`;
  raw corpus via `resolve-aozorabunko-corpus.sh` / `AB_CORPUS`.
- Produces: `data/perf-workset.json` (hash-pinned, verified by the runner)
  and the baseline-vs-shim perf report implementing the design's protocol
  (1 warm-up + ≥5 measured runs, median + spread, machine identity, 10%
  threshold, timeouts unconditional).

- [ ] **Step 1: Pin the workset** (working directory: `ab-validator/`; run on hinoki with Task 9)

Extract the 6 work ids from
`docs/superpowers/reports/2026-07-08-parser-performance-sample.json`,
resolve each raw source file in the corpus, and write
`data/perf-workset.json`:

```json
{
  "workset_id": "perf-workset-v1",
  "date": "2026-07-10",
  "source_corpus": "aozorabunko@0e9ea3e586eb0aa34039fabfc85a407d2f98b165",
  "protocol": {
    "build_profile": "release",
    "sccache": "disabled",
    "warmup_runs": 1,
    "measured_runs": 5,
    "median_regression_block_threshold_pct": 10,
    "new_timeout_policy": "unconditional blocker",
    "per_work_timeout_seconds": 90
  },
  "works": [
    { "work_id": "<id>", "corpus_relpath": "<relative path in corpus>", "source_sha256": "<sha256sum of the raw file>" }
  ]
}
```

(one `works[]` row per sample work; all values from measurement, no
placeholders committed).

- [ ] **Step 2: Write the runner**

`reports/aat-fidelity/run-perf-workset.py`:

```python
#!/usr/bin/env python3
"""Phase perf gate: baseline-vs-candidate wall-time on the pinned workset.

Reads data/perf-workset.json, VERIFIES each work's source_sha256 (fail
closed on mismatch), runs each binary as `<bin> inspect nodes -` with
1 warm-up + N measured runs per work, and emits a JSON report with per-work
medians/spread, machine identity, and a blocking verdict:
- BLOCK if any candidate run times out where baseline did not
- BLOCK if candidate workset median wall-time regresses > threshold_pct
- PASS otherwise (regressions under threshold are recorded, not blocking)

Usage:
  run-perf-workset.py --workset data/perf-workset.json \
      --baseline-bin PATH --candidate-bin PATH --corpus DIR --out report.json
"""
import argparse
import hashlib
import json
import pathlib
import platform
import statistics
import subprocess
import sys
import time


def sha256(path: pathlib.Path) -> str:
    return hashlib.sha256(path.read_bytes()).hexdigest()


def timed_runs(bin_path: str, source: bytes, warmup: int, measured: int,
               timeout_s: float) -> dict:
    times, timeouts = [], 0
    for i in range(warmup + measured):
        start = time.monotonic()
        try:
            subprocess.run([bin_path, "inspect", "nodes", "-"], input=source,
                           stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL,
                           timeout=timeout_s, check=False)
            elapsed = time.monotonic() - start
        except subprocess.TimeoutExpired:
            timeouts += 1
            elapsed = timeout_s
        if i >= warmup:
            times.append(elapsed)
    return {
        "median_s": round(statistics.median(times), 4),
        "stdev_s": round(statistics.pstdev(times), 4),
        "runs_s": [round(t, 4) for t in times],
        "timeouts": timeouts,
    }


def main() -> int:
    ap = argparse.ArgumentParser()
    ap.add_argument("--workset", required=True)
    ap.add_argument("--baseline-bin", required=True)
    ap.add_argument("--candidate-bin", required=True)
    ap.add_argument("--corpus", required=True)
    ap.add_argument("--out", required=True)
    args = ap.parse_args()

    ws = json.loads(pathlib.Path(args.workset).read_text())
    proto = ws["protocol"]
    corpus = pathlib.Path(args.corpus)
    report = {
        "workset_id": ws["workset_id"],
        "machine": {"node": platform.node(), "machine": platform.machine(),
                    "processor": platform.processor(),
                    "cpu_count": __import__("os").cpu_count()},
        "bins": {
            "baseline": {"path": args.baseline_bin,
                         "version": subprocess.run([args.baseline_bin, "--version"],
                                                   capture_output=True, text=True).stdout.strip()},
            "candidate": {"path": args.candidate_bin,
                          "version": subprocess.run([args.candidate_bin, "--version"],
                                                    capture_output=True, text=True).stdout.strip()},
        },
        "works": [],
    }
    for work in ws["works"]:
        src_path = corpus / work["corpus_relpath"]
        actual = sha256(src_path)
        if actual != work["source_sha256"]:
            print(f"FAIL-CLOSED: {work['work_id']} sha256 {actual} != pinned", file=sys.stderr)
            return 2
        source = src_path.read_bytes()
        row = {"work_id": work["work_id"]}
        for label, bin_path in (("baseline", args.baseline_bin), ("candidate", args.candidate_bin)):
            row[label] = timed_runs(bin_path, source, proto["warmup_runs"],
                                    proto["measured_runs"], proto["per_work_timeout_seconds"])
        report["works"].append(row)

    base_med = statistics.median(w["baseline"]["median_s"] for w in report["works"])
    cand_med = statistics.median(w["candidate"]["median_s"] for w in report["works"])
    regression_pct = 100.0 * (cand_med - base_med) / base_med if base_med else 0.0
    new_timeouts = any(w["candidate"]["timeouts"] > w["baseline"]["timeouts"]
                       for w in report["works"])
    report["summary"] = {
        "baseline_workset_median_s": round(base_med, 4),
        "candidate_workset_median_s": round(cand_med, 4),
        "regression_pct": round(regression_pct, 2),
        "threshold_pct": proto["median_regression_block_threshold_pct"],
        "new_timeouts": new_timeouts,
        "verdict": "BLOCK" if new_timeouts or
                   regression_pct > proto["median_regression_block_threshold_pct"]
                   else "PASS",
    }
    pathlib.Path(args.out).write_text(json.dumps(report, indent=2) + "\n")
    print(json.dumps(report["summary"], indent=2))
    return 0 if report["summary"]["verdict"] == "PASS" else 1


if __name__ == "__main__":
    raise SystemExit(main())
```

- [ ] **Step 3: Run the gate (baseline = pinned upstream, candidate = shim; same host as Task 9)**

```bash
export RUSTC_WRAPPER= SCCACHE_DISABLE=1
cargo build -p ab-aozora-cli --release
UP="$(nix build .#upstream-parser-aozora --no-link --print-out-paths)/bin/aozora"
CORPUS="$(bash reports/aat-fidelity/resolve-aozorabunko-corpus.sh 2>/dev/null || echo "$AB_CORPUS")"
python3 reports/aat-fidelity/run-perf-workset.py \
  --workset data/perf-workset.json \
  --baseline-bin "$UP" --candidate-bin "$PWD/target/release/ab-aozora-cli" \
  --corpus "$CORPUS" \
  --out docs/superpowers/reports/2026-07-10-fork-parity-perf.json
```

Expected: `"verdict": "PASS"`, exit 0 (same code ⇒ regression ≈ 0%). A
BLOCK verdict is a lift defect (e.g. debug build, missing feature flag) —
investigate before proceeding.

- [ ] **Step 4: Write the perf report and link it**

`docs/superpowers/reports/2026-07-10-fork-parity-perf.md`: workset id,
machine identity block, summary table (per-work medians ± stdev, both
bins), verdict. Link from the provenance handoff's perf-gate line.

- [ ] **Step 5: Commit**

```bash
git add data/perf-workset.json reports/aat-fidelity/run-perf-workset.py docs/superpowers/reports/2026-07-10-fork-parity-perf.* docs/handoffs/2026-07-10-parser-fork-provenance.md
git commit -m "test(parser): Phase 1 perf gate on the pinned workset"
```

---

### Task 9: Gate A — full-corpus AAT parity (hinoki, run-set-resolved)

**Files:**
- Create: `ab-validator/reports/aat-fidelity/compare-aat-dumps.py`
- Create: `ab-validator/reports/aat-fidelity/tests/test_compare_aat_dumps.py`
- Create: `ab-validator/docs/superpowers/reports/2026-07-10-fork-parity-corpus.md`
- Modify: `ab-validator/docs/handoffs/2026-07-10-parser-fork-provenance.md` (Gate A link)

**Interfaces:**
- Consumes: shim (Task 5), `--aozora-bin` runner flag (Task 6), reference
  dump resolved via `reports/aat-fidelity/run-sets/current.json` +
  `AB_DB_ROOT` (adapter `aozora` entry: `aat_dir`, `expected.content_hash`).
- Produces: Gate A evidence — fork AAT semantically identical to the
  reference for all 17,886 works under the single-pointer allowlist.

- [ ] **Step 1: Write the comparator** (working directory: `ab-validator/`)

`reports/aat-fidelity/compare-aat-dumps.py`:

```python
#!/usr/bin/env python3
"""Semantic-JSON parity between two AAT dumps.

Gate definition (Phase 1 Gate A): equality of parsed JSON documents after
normalizing EXACTLY one pointer — /meta/adapter_version — which embeds the
producing binary's identity (upstream store path vs fork shim) and is the
only sanctioned difference. This is semantic JSON equality, not byte
equality: key order and float formatting differences would be invisible,
which is acceptable because both sides are serde_json output.

Exit 0 = parity; 1 = divergence; 2 = usage/reference error.
"""
import json
import pathlib
import sys


def normalize(doc):
    meta = doc.get("meta")
    if isinstance(meta, dict):
        meta = dict(meta)
        meta.pop("adapter_version", None)
        doc = dict(doc)
        doc["meta"] = meta
    return doc


def load_dir(d: pathlib.Path) -> dict:
    files = {p.name: p for p in sorted(d.glob("*.json"))}
    if not files:
        print(f"ERROR: no *.json under {d}", file=sys.stderr)
        raise SystemExit(2)
    return files


def main() -> int:
    if len(sys.argv) != 3:
        print(__doc__, file=sys.stderr)
        return 2
    a_files = load_dir(pathlib.Path(sys.argv[1]))
    b_files = load_dir(pathlib.Path(sys.argv[2]))
    missing = sorted(set(a_files) ^ set(b_files))
    diverged = []
    for name in sorted(set(a_files) & set(b_files)):
        a = normalize(json.loads(a_files[name].read_text()))
        b = normalize(json.loads(b_files[name].read_text()))
        if a != b:
            diverged.append(name)
    print(json.dumps({
        "compared": len(set(a_files) & set(b_files)),
        "missing_count": len(missing),
        "missing_sample": missing[:20],
        "diverged_count": len(diverged),
        "diverged_sample": diverged[:20],
    }, indent=2))
    return 0 if not missing and not diverged else 1


if __name__ == "__main__":
    raise SystemExit(main())
```

- [ ] **Step 2: Write the comparator unit test**

`reports/aat-fidelity/tests/test_compare_aat_dumps.py`:

```python
import json
import pathlib
import subprocess
import sys

SCRIPT = pathlib.Path(__file__).resolve().parents[1] / "compare-aat-dumps.py"


def write(d: pathlib.Path, name: str, doc: dict) -> None:
    d.mkdir(parents=True, exist_ok=True)
    (d / name).write_text(json.dumps(doc))


def run(a, b):
    proc = subprocess.run([sys.executable, str(SCRIPT), str(a), str(b)],
                          capture_output=True, text=True)
    return proc.returncode, json.loads(proc.stdout)


def test_adapter_version_under_meta_is_normalized(tmp_path):
    a, b = tmp_path / "a", tmp_path / "b"
    doc = {"meta": {"adapter": "aozora", "adapter_version": "upstream x.y"}, "blocks": []}
    write(a, "w.json", doc)
    write(b, "w.json", {**doc, "meta": {"adapter": "aozora", "adapter_version": "shim z"}})
    code, out = run(a, b)
    assert code == 0 and out["diverged_count"] == 0


def test_any_other_difference_diverges(tmp_path):
    a, b = tmp_path / "a", tmp_path / "b"
    write(a, "w.json", {"meta": {"adapter": "aozora", "adapter_version": "v"}, "blocks": []})
    write(b, "w.json", {"meta": {"adapter": "AOZORA2", "adapter_version": "v"}, "blocks": []})
    code, out = run(a, b)
    assert code == 1 and out["diverged_count"] == 1


def test_missing_files_diverge(tmp_path):
    a, b = tmp_path / "a", tmp_path / "b"
    write(a, "w1.json", {"meta": {}})
    write(a, "w2.json", {"meta": {}})
    write(b, "w1.json", {"meta": {}})
    code, out = run(a, b)
    assert code == 1 and out["missing_count"] == 1
```

Run: `python3 -m pytest reports/aat-fidelity/tests/test_compare_aat_dumps.py -q`
Expected: 3 passed.

- [ ] **Step 3: Resolve the reference dump fail-closed (hinoki)**

```bash
python3 - <<'EOF'
import hashlib, json, os, pathlib, sys
rs = json.loads(pathlib.Path("reports/aat-fidelity/run-sets/current.json").read_text())
entry = rs["adapters"]["aozora"]
db_root = os.environ["AB_DB_ROOT"]  # fail loudly if unset
aat_dir = pathlib.Path(entry["aat_dir"].replace("${AB_DB_ROOT}", db_root))
files = sorted(aat_dir.glob("*.json"))
h = hashlib.sha256()
for p in files:
    h.update(p.name.encode())
    h.update(p.read_bytes())
digest = f"sha256:{h.hexdigest()}"
expected = entry["expected"]["content_hash"]
print(json.dumps({"aat_dir": str(aat_dir), "files": len(files),
                  "content_hash": digest, "expected": expected,
                  "match": digest == expected}))
sys.exit(0 if digest == expected else 2)
EOF
```

Expected: `"match": true` with 17,886 files. If the run-set's content-hash
scheme differs from name+bytes concatenation, use the repository's own
verifier instead (find it: `grep -rn "content_hash" reports/aat-fidelity/*.py | head`)
— the requirement is fail-closed verification through the run-set, not this
exact hashing recipe. If it does not match, STOP: the reference is not what
the run-set pins.

- [ ] **Step 4: Run the fork full-corpus dump on hinoki (explicit override)**

```bash
nix develop --command bash -c '
  export RUSTC_WRAPPER= SCCACHE_DISABLE=1
  cargo build -p ab-aozora-cli --release
  reports/aat-fidelity/run-aozora-aat-full.sh \
    --aozora-bin "$PWD/target/release/ab-aozora-cli" \
    --out-dir "$AB_DB_ROOT/aat-corpus/aozora-fork-parity-$(git rev-parse --short HEAD)" \
    --jobs 32 --force'
```

Expected: 17,886 AAT files; 0 `fatal_error` in check-reports; the run
`metadata.json` records the override path, sha256, and the shim's
`--version` line (Task 6 behavior).

- [ ] **Step 5: Compare and record**

```bash
python3 reports/aat-fidelity/compare-aat-dumps.py \
  "<resolved reference aat_dir from Step 3>" \
  "$AB_DB_ROOT/aat-corpus/aozora-fork-parity-<rev>/aat/aozora-adapter"
```

Expected: `"missing_count": 0, "diverged_count": 0`, exit 0. On
divergence: minimize one work via
`target/release/ab-aozora-cli inspect nodes - < work.txt` vs the pinned
binary, fix in Task 3/5 scope, re-run from Step 4.

Then write `docs/superpowers/reports/2026-07-10-fork-parity-corpus.md`
recording: run-set id + reference `content_hash` (verified true), fork dump
path + its computed content hash (same recipe as Step 3), comparator output
JSON, the gate definition ("semantic JSON equality, single allowlisted
pointer `/meta/adapter_version`"), verdict `FORK_PARITY_CONFIRMED`. Fill
the Gate A line in the provenance handoff. The scratch fork dump may be
deleted afterwards (the report records its identity).

- [ ] **Step 6: Commit**

```bash
git add reports/aat-fidelity/compare-aat-dumps.py reports/aat-fidelity/tests/test_compare_aat_dumps.py docs/superpowers/reports/2026-07-10-fork-parity-corpus.md docs/handoffs/2026-07-10-parser-fork-provenance.md
git commit -m "test(parser): Gate A full-corpus AAT parity for the lifted fork"
```

---

## Phase-exit checklist (Phase 1 done means)

- ADR 0031 accepted with reciprocal link and the maintenance-economics
  rationale + revisit trigger; ADR gates green (Task 1).
- Nine `ab-aozora-*` crates build with **all** inherited tests green
  (benches dropped and inventoried; nothing else) inside workspace gates
  (Task 3), deny-audited without pipes (Task 4).
- Shim passes golden byte-equality vs the pinned binary and serves
  `--version` (Task 5).
- Both harnesses take the binary under test as an explicit parameter that
  controls execution AND recorded identity, proven by the override smoke
  test (Task 6).
- Gate B: both conformance instruments identical under explicit
  `AOZORA_BIN` (Task 7).
- Perf gate: PASS verdict from `run-perf-workset.py` (hash-verified
  workset, 1+5 runs, machine identity recorded, ≤10% median, no new
  timeouts) on the same host as Gate A (Task 8).
- Gate A: reference resolved fail-closed via the run-set, fork dump
  generated under `--aozora-bin`, semantic parity with the single
  `/meta/adapter_version` allowlist, `FORK_PARITY_CONFIRMED` (Task 9).
- The pinned upstream lane is untouched and remains the comparison lane
  (design: Legacy Lane Retention) — nothing is deleted in this phase.

Phase 2 (absorption + permanent `ab-aozora` binary) gets its own plan,
written against the post-Phase-1 tree.
