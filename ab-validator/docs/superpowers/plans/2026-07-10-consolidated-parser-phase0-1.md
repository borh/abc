# Consolidated Parser Phase 0+1 (ADR 0031 + Lift + Shim + Parity) Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Land ADR 0031 (hard-detach governance), lift the aozora-pipeline
parser core into `ab-validator/crates/` as `ab-*` crates, ship the throwaway
`ab-aozora-cli` inspect shim, and prove full parity (conformance + full
corpus) against the pinned upstream binary.

**Architecture:** Strangler parity-first per
`docs/superpowers/specs/2026-07-10-consolidated-parser-design.md`. Phase 1
changes no parser semantics: verbatim crate lift (rename-only), a shim that
reproduces the upstream `aozora inspect` protocol byte-for-byte, and two
measured gates (Gate B conformance, Gate A corpus) proving the lift is
inert. Phases 2–4 get their own plans afterwards.

**Tech Stack:** Rust (edition 2024, `ab-validator` workspace), Nix flake
(pinned `upstream-aozora-src` = `P4suta/aozora@1a4f864`), Clojure tooling on
the abc side (ADR diagrams), Python report harnesses.

## Global Constraints

- Hard-detach rev (verbatim everywhere): `1a4f864603970983719655aa4af4525958ac2d38`.
- Lifted code is rename-only in this plan: **zero semantic edits** to parser
  logic. Any behavior diff found by a gate is a lift defect to fix by
  re-copying, never by "improving" code.
- Crate naming: upstream `aozora-<x>` → `ab-aozora-<x>`; package + lib names
  renamed, module structure otherwise untouched.
- License/attribution: upstream is `MIT OR Apache-2.0`; copy upstream
  `NOTICE` into each lifted crate; every lifted crate's `src/lib.rs` starts
  with the provenance header (Task 3 Step 2).
- All cargo commands: `export RUSTC_WRAPPER= SCCACHE_DISABLE=1` first
  (sccache socket-path failure in long session paths).
- `AB_AOZORA_BIN` is not exported by `nix develop`; set it explicitly where
  a task needs the pinned binary:
  `export AB_AOZORA_BIN="$(nix build .#upstream-parser-aozora --no-link --print-out-paths)/bin/aozora"`.
- Full-corpus runs happen on the heavy-compute host (hinoki); expect
  ~5–10 min at `--jobs 32`. Reference dump:
  `/db/ab-validator/aat-corpus/aozora-full-repin-1a4f864` (17,886 works).
- Working directory for ab-validator tasks: `ab-validator/` inside the
  soranoha monorepo; abc tasks run in `abc/`.
- Commit message style: conventional commits as in recent history
  (`docs(adr): …`, `feat(parser): …`, `test(parser): …`).

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

- [ ] **Step 1: Write ADR 0031**

Create `abc/docs/adr/0031-parser-fork-hard-detach.md`:

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
measured pin, with no future upstream merges. Upstream is under heavy
active development; carrying a merge relationship against an actively
diverging base would make every re-measure a moving target, and the
comparison study already proved measurement equivalence only at the pin.

## Decision

- The consolidated parser is a hard fork of `P4suta/aozora` at rev
  `1a4f864603970983719655aa4af4525958ac2d38` (the measured pin of the
  ADR 0030 evidence). No future merges from upstream.
- ADR 0030's upstream-first engagement model is replaced: upstream
  contributions are no longer part of the parser plan. If a later upstream
  change is wanted, it is ported as a reviewed patch with re-measurement,
  not a merge.
- Lifted crates live in `ab-validator/crates/` under `ab-aozora-*` names,
  inside the workspace gates. Attribution is preserved: the upstream
  `NOTICE` file travels with the lifted code and each lifted crate records
  the upstream repository and rev in a provenance header. Upstream license
  is `MIT OR Apache-2.0`, matching the workspace.
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
  records the detach rev and lifted-crate provenance.

## Rollback

Superseding ADR re-establishing an upstream relationship (or a different
base per ADR 0030's rollback path); no registry, manifest, or identity
change is implied by this ADR itself.
```

- [ ] **Step 2: Add the reciprocal link to ADR 0030**

In `abc/docs/adr/0030-aozora-parser-selection.md`, change the header block

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
cd abc
clojure -M:abc/diagrams
bash nix/check-acceptance-criteria.sh
clojure -M:test:kaocha -m kaocha.runner --focus abc.tools.diagram.adr-graph-test
```

Expected: `wrote docs/adr/adr-graph.mmd`; gate exits 0; kaocha reports
0 failures.

- [ ] **Step 4: Commit**

```bash
git add abc/docs/adr/0031-parser-fork-hard-detach.md abc/docs/adr/0030-aozora-parser-selection.md abc/docs/adr/adr-graph.mmd
git commit -m "docs(adr): ADR 0031 hard-detach amendment to parser selection"
```

---

### Task 2: Dependency-closure discovery + fork-provenance handoff

**Files:**
- Create: `ab-validator/docs/handoffs/2026-07-10-parser-fork-provenance.md`

**Interfaces:**
- Consumes: pinned upstream source via
  `nix eval --raw .#upstream-parser-aozora.src` (run in `ab-validator/`).
- Produces: the confirmed crate list `LIFT_SET` that Task 3 lifts, recorded
  in the handoff. Expected (verify, don't assume):
  `aozora-spec, aozora-veb, aozora-encoding, aozora-scan, aozora-syntax,
  aozora-pipeline`.

- [ ] **Step 1: Resolve the pinned source and compute the closure**

```bash
cd ab-validator
SRC=$(nix eval --raw .#upstream-parser-aozora.src)
echo "$SRC"   # a /nix/store/...-source path
# Workspace-internal dependency edges of the parse path:
for c in aozora-pipeline aozora-syntax aozora-scan aozora-spec aozora-veb aozora-encoding aozora-cst; do
  echo "== $c";
  sed -n '/^\[dependencies\]/,/^\[dev-dependencies\]\|^\[build-dependencies\]\|^\[\[/p' \
    "$SRC/crates/$c/Cargo.toml" | grep -E '^aozora'
done
```

Expected edges: `pipeline → spec, syntax, encoding, scan`;
`syntax → spec, veb, encoding`; `scan → spec`; `spec →` (none);
`veb →` (verify); `encoding →` (none; has `[build-dependencies]` — record
them). `cst → pipeline` (cst is a *consumer* of pipeline; it is NOT lifted
unless Step 2 shows the inspect path needs it).

- [ ] **Step 2: Confirm the inspect path does not require aozora-cst**

```bash
grep -rn "aozora_cst\|aozora-cst" "$SRC/crates/aozora-cli/" | head
```

Expected: no hits in the document-inspect path (if there are hits, add
`aozora-cst` to LIFT_SET and record why).

- [ ] **Step 3: Record external (crates.io) dependencies of LIFT_SET**

```bash
for c in aozora-spec aozora-veb aozora-encoding aozora-scan aozora-syntax aozora-pipeline; do
  echo "== $c";
  sed -n '/^\[dependencies\]/,/^\[dev-dependencies\]\|^\[\[/p' "$SRC/crates/$c/Cargo.toml" | grep -v '^aozora' | grep '='
done
```

Record the list (expected small: `aho-corasick`, `memchr`, `smallvec`,
serde-feature deps) — Task 4 audits it.

- [ ] **Step 4: Write the fork-provenance handoff**

Create `ab-validator/docs/handoffs/2026-07-10-parser-fork-provenance.md`:

```markdown
# Parser fork provenance (hard detach)

Date: 2026-07-10
Authority: ADR 0030 (selection), ADR 0031 (hard detach),
`docs/superpowers/specs/2026-07-10-consolidated-parser-design.md`.

## Detach point

- Upstream: `github.com/P4suta/aozora`
- Rev: `1a4f864603970983719655aa4af4525958ac2d38` (flake input
  `upstream-aozora-src`, locked 2026-07-08)
- License: MIT OR Apache-2.0 (upstream NOTICE copied into each lifted crate)

## Lifted crates (LIFT_SET)

| upstream crate | fork crate | workspace-internal deps |
| --- | --- | --- |
| aozora-spec | ab-aozora-spec | (none) |
| aozora-veb | ab-aozora-veb | <fill from Step 1> |
| aozora-encoding | ab-aozora-encoding | (none; build-deps: <fill>) |
| aozora-scan | ab-aozora-scan | spec |
| aozora-syntax | ab-aozora-syntax | spec, veb, encoding |
| aozora-pipeline | ab-aozora-pipeline | spec, syntax, encoding, scan |

Not lifted: aozora-cst (consumer of pipeline; not needed by the inspect
path), aozora-cli (only its document-inspect emission module is copied into
crates/ab-aozora-cli — see that crate's provenance header), and all
bindings/tooling crates per the design's minimal-core decision.

## External dependency surface added

<paste Step 3 output>

## Verbatim-lift statement

Phase 1 lifts are rename-only (package/lib names, intra-workspace dep
paths, provenance headers). No semantic edits. Gate evidence:
- Gate B: <link conformance comparison report when Task 6 lands>
- Gate A: <link corpus parity report when Task 8 lands>
```

The two `<fill…>` cells are completed from Step 1/Step 3 output in this
same task; the two Gate links are completed by Tasks 6 and 8 (each task
updates this file — that is part of those tasks).

- [ ] **Step 5: Commit**

```bash
git add ab-validator/docs/handoffs/2026-07-10-parser-fork-provenance.md
git commit -m "docs(handoffs): parser fork provenance and lift closure"
```

---

### Task 3: Lift LIFT_SET into `ab-validator/crates/` (rename-only)

**Files:**
- Create: `ab-validator/crates/ab-aozora-{spec,veb,encoding,scan,syntax,pipeline}/` (full crate trees)
- Modify: `ab-validator/Cargo.toml` (workspace members + workspace deps)

**Interfaces:**
- Consumes: LIFT_SET and `$SRC` from Task 2.
- Produces: building crates `ab-aozora-spec` … `ab-aozora-pipeline` with
  upstream unit tests green; `ab_aozora_pipeline` is the library Task 5
  links against.

- [ ] **Step 1: Copy the crates**

```bash
cd ab-validator
SRC=$(nix eval --raw .#upstream-parser-aozora.src)
for c in spec veb encoding scan syntax pipeline; do
  cp -r --no-preserve=mode "$SRC/crates/aozora-$c" "crates/ab-aozora-$c"
  cp "$SRC/NOTICE" "crates/ab-aozora-$c/NOTICE"
  cp "$SRC/LICENSE-MIT" "$SRC/LICENSE-APACHE" "crates/ab-aozora-$c/"
done
```

- [ ] **Step 2: Rename packages and rewrite intra-workspace deps**

For each `crates/ab-aozora-$c/Cargo.toml`: set
`name = "ab-aozora-$c"`, replace `workspace = true` dep entries for lifted
siblings with path deps, e.g. in `ab-aozora-pipeline/Cargo.toml`:

```toml
[package]
name = "ab-aozora-pipeline"
version = "0.1.0"
edition = "2024"
license = "MIT OR Apache-2.0"

[dependencies]
ab-aozora-spec = { path = "../ab-aozora-spec" }
ab-aozora-syntax = { path = "../ab-aozora-syntax" }
ab-aozora-encoding = { path = "../ab-aozora-encoding" }
ab-aozora-scan = { path = "../ab-aozora-scan" }
aho-corasick = { version = "1", features = ["std"] }
memchr = "1"
smallvec = "1"
```

(Exact external versions: copy the version numbers from the upstream root
`Cargo.toml` `[workspace.dependencies]` table — do not guess; `memchr`/
`smallvec` majors above are placeholders to be replaced by the upstream
table's values.) Drop upstream `[dev-dependencies]` on non-lifted crates
(`aozora-proptest`, `aozora-render`, `criterion`, `insta` benches): delete
the `[[bench]]` sections and the bench files; keep `#[cfg(test)]` unit
tests. Rename crate references in source:

```bash
for c in spec veb encoding scan syntax pipeline; do
  grep -rl "aozora_$c" crates/ab-aozora-*/src | xargs -r sed -i "s/\baozora_$c\b/ab_aozora_$c/g"
done
```

- [ ] **Step 3: Add the provenance header to each lifted crate**

Prepend to each `crates/ab-aozora-$c/src/lib.rs`:

```rust
//! Forked from https://github.com/P4suta/aozora
//! at rev 1a4f864603970983719655aa4af4525958ac2d38 (hard detach; ADR 0031).
//! Upstream crate: aozora-<c>. License: MIT OR Apache-2.0 (see NOTICE).
```

- [ ] **Step 4: Wire workspace membership**

In `ab-validator/Cargo.toml` `[workspace] members`, add the six crates
alongside the existing `crates/*` entries (match the file's existing list
style).

- [ ] **Step 5: Build and run the inherited tests**

```bash
export RUSTC_WRAPPER= SCCACHE_DISABLE=1
cargo build -p ab-aozora-pipeline --release
cargo test -p ab-aozora-spec -p ab-aozora-veb -p ab-aozora-encoding -p ab-aozora-scan -p ab-aozora-syntax -p ab-aozora-pipeline
```

Expected: build exits 0; all inherited unit tests pass. Any failure is a
rename defect (missed identifier, dropped feature flag) — fix the rename,
never the logic. If a unit test depends on a dropped dev-dependency, delete
that test file and record the deletion in the fork-provenance handoff
(these are usually proptest/insta suites; core `#[test]` fns stay).

- [ ] **Step 6: Workspace gates**

```bash
cargo fmt --check
cargo clippy --workspace --all-targets -- -D warnings
```

Expected: clean. Formatting diffs from upstream style are allowed to be
fixed by `cargo fmt` (whitespace-only; not a semantic edit). Clippy lints
in lifted code are silenced with targeted `#[allow(...)]` + a
`// lifted-code allowance` comment, not by rewriting logic.

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

- [ ] **Step 1: Run the audit**

```bash
cd ab-validator
export RUSTC_WRAPPER= SCCACHE_DISABLE=1
cargo deny check 2>&1 | tail -20
```

Expected: pass. If a lifted dependency trips a license or advisory rule,
STOP and surface it for a scoping decision (design risk: "anything large
or unwanted in the closure triggers a scoping decision") — do not silently
allowlist.

- [ ] **Step 2: Record the added surface in the provenance handoff**

Append the resolved new third-party crates (from `cargo tree -p
ab-aozora-pipeline -e normal --prefix none | sort -u` minus pre-existing
workspace deps) under "External dependency surface added".

- [ ] **Step 3: Commit**

```bash
git add deny.toml docs/handoffs/2026-07-10-parser-fork-provenance.md
git commit -m "chore(parser): audit lifted dependency surface"
```

---

### Task 5: `ab-aozora-cli` shim (inspect protocol, byte-compatible)

**Files:**
- Create: `ab-validator/crates/ab-aozora-cli/Cargo.toml`
- Create: `ab-validator/crates/ab-aozora-cli/src/main.rs`
- Create: `ab-validator/crates/ab-aozora-cli/src/emit.rs` (copied upstream emission code)
- Create: `ab-validator/crates/ab-aozora-cli/tests/golden.rs`
- Modify: `ab-validator/Cargo.toml` (workspace member)

**Interfaces:**
- Consumes: `ab_aozora_pipeline` (Task 3).
- Produces: binary `ab-aozora-cli` supporting exactly
  `ab-aozora-cli inspect {nodes,diagnostics,gaiji} -` (stdin → stdout JSON
  `{"schemaVersion":2,"data":…}`), byte-identical to the pinned upstream
  `aozora inspect <kind> -`. Tasks 6 and 8 run the existing
  `adapters/aozora` with `AB_AOZORA_BIN` pointed at this binary.

- [ ] **Step 1: Locate the upstream document-inspect emission path**

```bash
SRC=$(nix eval --raw .#upstream-parser-aozora.src)
grep -n "inspect" "$SRC/crates/aozora-cli/src/main.rs" | head -30
grep -rn "schemaVersion" "$SRC/crates/aozora-cli/src/" | grep -v introspect
```

Identify the function(s) that: read input, run the pipeline parse, and
serialize the `nodes` / `diagnostics` / `gaiji` envelopes. Note their names
and the exact serialization calls (serde settings, key order, trailing
newline). These are what `emit.rs` copies.

- [ ] **Step 2: Create the crate**

`crates/ab-aozora-cli/Cargo.toml`:

```toml
[package]
name = "ab-aozora-cli"
version = "0.1.0"
edition = "2024"
license = "MIT OR Apache-2.0"
description = "Throwaway parity shim: upstream `aozora inspect` protocol over the lifted ab-aozora crates. Deleted at Phase 2."

[dependencies]
ab-aozora-pipeline = { path = "../ab-aozora-pipeline" }
serde = { version = "1.0", features = ["derive"] }
serde_json = "1.0"
anyhow = "1.0"
```

(Add `ab-aozora-encoding`/`ab-aozora-syntax` path deps only if the copied
emission code imports them.)

- [ ] **Step 3: Copy the emission code into `src/emit.rs`**

Copy the functions identified in Step 1 verbatim from
`$SRC/crates/aozora-cli/src/` into `src/emit.rs`, rename `aozora_*` crate
paths to `ab_aozora_*`, and add the provenance header:

```rust
//! Copied from P4suta/aozora crates/aozora-cli (document-inspect emission)
//! at rev 1a4f864603970983719655aa4af4525958ac2d38 (ADR 0031). Byte-for-byte
//! output compatibility with `aozora inspect {nodes,diagnostics,gaiji}` is
//! the contract; tests/golden.rs enforces it.
pub fn run(kind: &str, source: &str) -> anyhow::Result<String> {
    // body: the copied upstream emission for the three kinds, dispatching
    // on `kind` exactly as upstream's inspect subcommand does.
}
```

- [ ] **Step 4: Write `src/main.rs`**

```rust
use std::io::{Read, Write};

mod emit;

fn main() {
    let args: Vec<String> = std::env::args().collect();
    // Accept exactly: <bin> inspect <kind> -   where kind ∈ nodes|diagnostics|gaiji
    let kind = match args.as_slice() {
        [_, cmd, kind, dash]
            if cmd == "inspect"
                && dash == "-"
                && matches!(kind.as_str(), "nodes" | "diagnostics" | "gaiji") =>
        {
            kind.clone()
        }
        _ => {
            eprintln!("usage: ab-aozora-cli inspect {{nodes|diagnostics|gaiji}} -");
            std::process::exit(64);
        }
    };
    let mut source = String::new();
    if let Err(err) = std::io::stdin().read_to_string(&mut source) {
        eprintln!("ab-aozora-cli: read stdin: {err}");
        std::process::exit(1);
    }
    match emit::run(&kind, &source) {
        Ok(json) => {
            let mut stdout = std::io::stdout().lock();
            // Upstream terminates the envelope with a newline; golden tests
            // will catch any mismatch either way.
            if stdout.write_all(json.as_bytes()).is_err() {
                std::process::exit(1);
            }
        }
        Err(err) => {
            eprintln!("ab-aozora-cli: {err}");
            std::process::exit(1);
        }
    }
}
```

(If Step 1 shows upstream exits non-zero on parse failure for `inspect`,
mirror that exit code exactly; the golden test in Step 5 compares exit
codes too.)

- [ ] **Step 5: Write the golden byte-equality test**

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
    let upstream = std::env::var("AB_AOZORA_BIN")
        .expect("set AB_AOZORA_BIN to the pinned upstream aozora binary");
    let shim = env!("CARGO_BIN_EXE_ab-aozora-cli");
    // Aozora-shaped sample with ruby, gaiji, a command line, CRLF + header,
    // plus an edge-case sample (empty body, malformed marker).
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
                "stdout diverged: kind={kind} sample={i}\nupstream: {}\nshim:     {}",
                String::from_utf8_lossy(&up_out),
                String::from_utf8_lossy(&sh_out)
            );
        }
    }
}
```

- [ ] **Step 6: Run the golden test — verify it fails before emit.rs is complete, passes after**

```bash
export RUSTC_WRAPPER= SCCACHE_DISABLE=1
export AB_AOZORA_BIN="$(nix build .#upstream-parser-aozora --no-link --print-out-paths)/bin/aozora"
cargo test -p ab-aozora-cli --test golden
```

Expected: PASS (byte-identical output for all 9 kind×sample cells). Iterate
on `emit.rs` copying fidelity (serializer settings, newline) until green —
without editing lifted parser crates.

- [ ] **Step 7: Commit**

```bash
git add crates/ab-aozora-cli Cargo.toml Cargo.lock
git commit -m "feat(parser): ab-aozora-cli inspect shim with golden parity test"
```

---

### Task 6: Gate B — conformance equivalence (127 vectors + 30-seed)

**Files:**
- Create: `ab-validator/docs/superpowers/reports/2026-07-10-fork-parity-conformance.md`
- Modify: `ab-validator/docs/handoffs/2026-07-10-parser-fork-provenance.md` (Gate B link)

**Interfaces:**
- Consumes: shim binary (Task 5); existing harness
  `reports/parser-conformance/run-aozora-notation-spec.py` (via
  `just aozora-notation-spec-comparison`) and
  `reports/parser-conformance/author-official-seed.py` outputs.
- Produces: Gate B evidence report; parity confirmed on both instruments.

- [ ] **Step 1: Score the pinned upstream (baseline)**

```bash
cd ab-validator
export RUSTC_WRAPPER= SCCACHE_DISABLE=1
export AB_AOZORA_BIN="$(nix build .#upstream-parser-aozora --no-link --print-out-paths)/bin/aozora"
VECTORS="$(nix build --no-link --print-out-paths .#upstream-aozora-notation-spec)/conformance/vectors"
just aozora-notation-spec-comparison
cp docs/superpowers/reports/2026-07-08-aozora-notation-spec-comparison.summary.json /tmp/baseline-conformance.json 2>/dev/null || true
```

(If the justfile writes a dated summary elsewhere, note the actual output
path from the recipe and copy that; the point is a baseline snapshot file.)

- [ ] **Step 2: Score the shim and diff**

```bash
export AB_AOZORA_BIN="$(cargo build -p ab-aozora-cli --release 2>/dev/null; echo $PWD/target/release/ab-aozora-cli)"
just aozora-notation-spec-comparison
diff <(python3 -m json.tool /tmp/baseline-conformance.json) \
     <(python3 -m json.tool docs/superpowers/reports/*aozora-notation-spec-comparison.summary.json)
```

Expected: no diff in the aozora-adapter rows (other adapters unchanged by
construction). Any diff = lift defect; return to Task 3/5, fix, re-run.

- [ ] **Step 3: Repeat for the official-docs seed**

Run the seed scorer the same dual way (pinned vs shim `AB_AOZORA_BIN`; the
seed harness lives at `reports/parser-conformance/author-official-seed.py`
with its scored output next to the 2026-07-09 seed-expansion report — use
the invocation recorded in
`docs/superpowers/reports/2026-07-09-official-docs-seed-expansion.md`).
Expected: identical scores.

- [ ] **Step 4: Write the Gate B report and link it**

Create `docs/superpowers/reports/2026-07-10-fork-parity-conformance.md`
recording: shim rev (git), pinned binary store path, both summary hashes,
and "identical" verdicts for both instruments. Add the report link to the
fork-provenance handoff's Gate B line.

- [ ] **Step 5: Commit**

```bash
git add docs/superpowers/reports/2026-07-10-fork-parity-conformance.md docs/handoffs/2026-07-10-parser-fork-provenance.md
git commit -m "test(parser): Gate B conformance parity for the lifted fork"
```

---

### Task 7: Pin the performance workset

**Files:**
- Create: `ab-validator/data/perf-workset.json`

**Interfaces:**
- Consumes: the 6-work sample used by
  `docs/superpowers/reports/2026-07-08-parser-performance-sample.{md,json}`
  (work ids listed there).
- Produces: `data/perf-workset.json` — the named, hash-pinned workset the
  design's perf protocol references from Phase 1 onward.

- [ ] **Step 1: Extract the 6 work ids and hash-pin them**

Read the work ids from
`docs/superpowers/reports/2026-07-08-parser-performance-sample.json`, then
create `data/perf-workset.json`:

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
    "new_timeout_policy": "unconditional blocker"
  },
  "works": [
    { "work_id": "<id-1>", "source_sha256": "<sha256 of the raw source file>" }
  ]
}
```

with one `works[]` row per sample work (ids from the 2026-07-08 report;
`source_sha256` computed with `sha256sum` over each raw corpus file).

- [ ] **Step 2: Commit**

```bash
git add data/perf-workset.json
git commit -m "chore(parser): hash-pin the perf workset (perf-workset-v1)"
```

---

### Task 8: Gate A — full-corpus AAT parity (hinoki)

**Files:**
- Create: `ab-validator/reports/aat-fidelity/compare-aat-dumps.py`
- Create: `ab-validator/docs/superpowers/reports/2026-07-10-fork-parity-corpus.md`
- Modify: `ab-validator/docs/handoffs/2026-07-10-parser-fork-provenance.md` (Gate A link)

**Interfaces:**
- Consumes: shim binary (Task 5); `reports/aat-fidelity/run-aozora-aat-full.sh`;
  reference dump `/db/ab-validator/aat-corpus/aozora-full-repin-1a4f864`.
- Produces: Gate A evidence: fork AAT ≡ reference AAT for all 17,886 works,
  modulo the metadata allowlist below.

- [ ] **Step 1: Write the dump comparator**

`reports/aat-fidelity/compare-aat-dumps.py`:

```python
#!/usr/bin/env python3
"""Compare two full-corpus AAT dumps for byte-level content parity.

Metadata allowlist: adapter_version may embed a build path / dev-build
marker; everything else must be identical. Exit 0 = parity, 1 = divergence.
"""
import json
import pathlib
import sys

ALLOWLIST = {"adapter_version"}  # top-level metadata keys allowed to differ


def normalize(doc: dict) -> dict:
    return {k: v for k, v in doc.items() if k not in ALLOWLIST}


def main() -> int:
    a_dir, b_dir = pathlib.Path(sys.argv[1]), pathlib.Path(sys.argv[2])
    a_files = {p.name: p for p in a_dir.glob("*.json")}
    b_files = {p.name: p for p in b_dir.glob("*.json")}
    missing = sorted(set(a_files) ^ set(b_files))
    diverged: list[str] = []
    for name in sorted(set(a_files) & set(b_files)):
        a = normalize(json.loads(a_files[name].read_text()))
        b = normalize(json.loads(b_files[name].read_text()))
        if a != b:
            diverged.append(name)
    print(json.dumps({
        "compared": len(set(a_files) & set(b_files)),
        "missing_in_one_side": missing[:20],
        "missing_count": len(missing),
        "diverged": diverged[:20],
        "diverged_count": len(diverged),
    }, indent=2))
    return 0 if not missing and not diverged else 1


if __name__ == "__main__":
    raise SystemExit(main())
```

(Adjust the glob if the dump nests per-adapter subdirectories — mirror the
layout of `/db/ab-validator/aat-corpus/aozora-full-repin-1a4f864`, e.g.
`aat/aozora-adapter/*.json`; check with `ls` first and encode the real
relative path into the script before running.)

- [ ] **Step 2: Run the fork full-corpus dump on hinoki**

```bash
cd ab-validator
nix develop --command bash -c '
  export RUSTC_WRAPPER= SCCACHE_DISABLE=1
  cargo build -p ab-aozora-cli --release
  export AB_AOZORA_BIN="$PWD/target/release/ab-aozora-cli"
  reports/aat-fidelity/run-aozora-aat-full.sh \
    --out-dir /db/ab-validator/aat-corpus/aozora-fork-parity-$(git rev-parse --short HEAD) \
    --jobs 32 --force'
```

Expected: 17,886 AAT files, 0 `fatal_error` in check-reports (the
handoff's trust condition for a dump).

- [ ] **Step 3: Compare against the reference dump**

```bash
python3 reports/aat-fidelity/compare-aat-dumps.py \
  /db/ab-validator/aat-corpus/aozora-full-repin-1a4f864/<aat-subdir> \
  /db/ab-validator/aat-corpus/aozora-fork-parity-<rev>/<aat-subdir>
```

Expected: `"missing_count": 0, "diverged_count": 0`, exit 0. Any divergence
is a lift/shim defect: pick one diverged work, minimize with
`ab-aozora-cli inspect nodes -` vs pinned binary on that work's source,
fix in Task 3/5 scope, re-run from Step 2.

- [ ] **Step 4: Write the Gate A report; complete the provenance handoff**

Create `docs/superpowers/reports/2026-07-10-fork-parity-corpus.md`: dump
paths, comparator output JSON, work count, allowlisted metadata fields
actually differing, verdict `FORK_PARITY_CONFIRMED`. Fill the Gate A line
in `docs/handoffs/2026-07-10-parser-fork-provenance.md`. Delete the scratch
fork dump if `/db` space is a concern (the report records its identity).

- [ ] **Step 5: Commit**

```bash
git add reports/aat-fidelity/compare-aat-dumps.py docs/superpowers/reports/2026-07-10-fork-parity-corpus.md docs/handoffs/2026-07-10-parser-fork-provenance.md
git commit -m "test(parser): Gate A full-corpus AAT parity for the lifted fork"
```

---

## Phase-exit checklist (Phase 1 done means)

- ADR 0031 accepted with reciprocal link; ADR gates green (Task 1).
- Six `ab-aozora-*` crates build with inherited tests green inside
  workspace gates (Task 3), deny-audited (Task 4).
- Shim passes golden byte-equality vs the pinned binary (Task 5).
- Gate B: both conformance instruments identical (Task 6).
- Gate A: full-corpus AAT parity, `FORK_PARITY_CONFIRMED` (Task 8).
- Perf workset pinned (Task 7).
- The pinned upstream lane is untouched and remains the comparison lane
  (design: Legacy Lane Retention) — nothing is deleted in this phase.

Phase 2 (absorption + permanent `ab-aozora` binary) gets its own plan,
written against the post-Phase-1 tree.
