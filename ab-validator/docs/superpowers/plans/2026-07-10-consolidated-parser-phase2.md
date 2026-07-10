# Consolidated Parser Phase 2 (Absorption + `ab-aozora`) Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** `ab-aozora-aat` emits AAT natively in-process, fronted by the
permanent `ab-aozora` stdin→AAT binary; three gates (corpus byte parity,
perf, conformance echo) pass with frozen evidence; the shim dies; identity
rotates to `ab-aozora` unadmitted.

**Architecture:** Per
`docs/superpowers/specs/2026-07-10-consolidated-parser-phase2-absorption-design.md`.
Hazard first (facade `entries` split + converter canonicalization), then the
port (copy — `adapters/aozora` stays frozen), then a real harness lane for
the candidate, then gates, then evidence-checked deletion and rotation.

**Tech Stack:** Rust (root workspace + one excluded canary workspace), Bash
harness (`run-aat-full.sh`), Python harnesses (`reports/**` + pytest), Nix
flake packaging, Clojure/EDN on the abc side (registry row only).

## Global Constraints

- Working root: the `ab-validator/` directory of the phase worktree
  (`.worktrees/parser-fork-phase2`), except Task 12 (abc side) and where a
  step states otherwise. Each task states its working directory in Step 1.
- `adapters/aozora` is **frozen**: no edits of any kind (not even
  formatting). It is the parity comparator; Phase 1 evidence depends on it.
- Ported logic is verbatim-copy discipline: a behavior diff found by a gate
  is a port defect fixed by re-copying from `adapters/aozora/src/lib.rs`,
  never by "improving" the logic.
- Parity allowlist is exactly two pointers: `/meta/adapter` and
  `/meta/adapter_version`. Nothing else may differ, byte-for-byte.
- `ab-aozora` exits `0` success / `1` fatal — never `2` (the conformance
  harness treats nonzero as adapter error; the frozen adapter never exits 2).
- No root-workspace crate may activate `serde_json/preserve_order`
  (`tests/workspace-no-preserve-order.sh` from Task 1 is the check; run it
  whenever workspace membership changes).
- All cargo commands: `export RUSTC_WRAPPER= SCCACHE_DISABLE=1` first.
- Never pipe a gate command into `tail`/`grep` without `set -o pipefail` —
  gates must fail on the command's own exit status.
- Harness runs pass the binary under test explicitly (`--adapter-bin` /
  `--aozora-bin` / justfile `AOZORA_BIN=`); ambient env is not evidence.
- Reference data resolves fail-closed through
  `reports/aat-fidelity/run-sets/current.json` + `AB_DB_ROOT` (never
  hardcode `/db/...` in code; hinoki's data root happens to be
  `/db/ab-validator`).
- Full-corpus and perf runs happen on hinoki
  (`hinoki.hyakutake-barbel.ts.net`, passwordless ssh, 32 cores). hinoki has
  PrivateTmp (nothing under `/tmp` survives across ssh sessions) and a
  ~10-minute ssh command cap: long runs are `nohup … &` detached with a log
  file polled by later ssh calls. Never delete
  `/db/ab-validator/aat-corpus/aozora-full-repin-1a4f864` or any dump a
  run-set references.
- Frozen evidence reports (`docs/superpowers/reports/**`) are never
  rewritten. Tasks 8–10 create new ones; Task 11 verifies them; a re-run
  after a fix produces a NEW report superseding the old by reference.
- **Checkpoint contract (binding for Task 11+):** no deletion, exclude-list,
  or identity-rotation change is committed until the three gate reports
  exist in the tree, each states its passing verdict, and all three cite the
  same candidate commit and `--version` identity.
- Commit message style: conventional commits as in recent history.

---

### Task 1: Facade `entries` feature split + workspace feature-graph guard

**Files:**
- Modify: `crates/ab-aozora-facade/Cargo.toml` (`[features]` table)
- Modify: `crates/ab-aozora-facade/src/lib.rs:100-101` (module gate)
- Modify: `crates/ab-aozora-facade/src/json.rs` (per-item feature gates)
- Create: `tests/workspace-no-preserve-order.sh`

**Interfaces:**
- Consumes: current `json` feature
  (`json = ["dep:serde", "dep:serde_json", "serde_json/preserve_order"]`),
  `json` module gated at `src/lib.rs:100` by `#[cfg(feature = "json")]`.
- Produces: feature `entries` exposing — with **no** serde_json in the
  graph — `json::SCHEMA_VERSION` (u32, currently 2), the entry types
  (`Node`, `Diagnostic`, `Pair`, `ContainerPair`, `GaijiResolution`,
  `Slug`), and the constructors `node_entries(&Tree) -> Vec<Node>`,
  `diagnostic_entries(&[crate::Diagnostic]) -> Vec<Diagnostic>`,
  `pair_entries(&Tree) -> Vec<Pair>`,
  `container_pair_entries(&Tree) -> Vec<ContainerPair>`,
  `gaiji_entries(&str) -> Vec<GaijiResolution>`,
  `slug_entries() -> Vec<Slug>`. Task 3 consumes these.

This is the first semantic edit to a lifted crate — a fork-owned divergence
under ADR 0032, recorded in the provenance handoff in Task 11.

- [ ] **Step 1: Rewrite the feature table** (working directory:
  `ab-validator/`)

In `crates/ab-aozora-facade/Cargo.toml`, replace the `json` feature line
with:

```toml
# Typed wire-projection layer: entry types + *_entries() constructors +
# SCHEMA_VERSION. Builds structs from the parse tree; never serializes, so
# it must not pull serde_json (and above all not serde_json/preserve_order —
# see the feature-unification hazard in
# docs/handoffs/2026-07-10-parser-fork-provenance.md).
entries = ["dep:serde"]
json = ["entries", "dep:serde_json", "serde_json/preserve_order"]
```

Keep the existing comment block above `json` (it documents why
`preserve_order` is pinned there).

- [ ] **Step 2: Re-gate the module**

In `crates/ab-aozora-facade/src/lib.rs:100-101`, change:

```rust
#[cfg(feature = "json")]
#[cfg_attr(docsrs, doc(cfg(feature = "json")))]
pub mod json;
```

to:

```rust
#[cfg(feature = "entries")]
#[cfg_attr(docsrs, doc(cfg(feature = "entries")))]
pub mod json;
```

(`json` implies `entries`, so `--features json` still exposes the module.)

- [ ] **Step 3: Gate serialization-only items inside `src/json.rs`**

Add to every item whose body or signature uses `serde_json` (the
string-emitting functions `diagnostics`, `nodes`, `pairs`,
`container_pairs`, `slugs`, `gaiji`, `gaiji_at`, any private helper that
serializes, and — if not already behind the `schema` feature — the
`schema_*` functions returning `serde_json::Value`):

```rust
#[cfg(feature = "json")]
#[cfg_attr(docsrs, doc(cfg(feature = "json")))]
```

Leave feature-free within the module: `SCHEMA_VERSION`, the entry types and
their derives (`serde::Serialize` is covered by `entries`' `dep:serde`), the
`*_entries` constructors, and any pure-construction helpers they call. If
`gaiji_entries`/`gaiji` share a helper that serializes, split the helper so
the entries path stays serde_json-free. Adjust `use` statements with
`#[cfg(feature = "json")]` as needed so `--features entries` compiles with
zero `unused_imports`/`unexpected_cfgs` warnings.

- [ ] **Step 4: Verify all three feature configurations**

```bash
export RUSTC_WRAPPER= SCCACHE_DISABLE=1
cargo check -p ab-aozora-facade
cargo check -p ab-aozora-facade --features entries
cargo test  -p ab-aozora-facade --features json
cargo tree  -p ab-aozora-facade --features entries -e features | { ! grep -F "preserve_order"; }
cargo tree  -p ab-aozora-facade --features entries | { ! grep -F "serde_json"; }
```

Expected: all pass; the two `grep`s find nothing (inverted with `!`).

- [ ] **Step 5: Verify the excluded shim still builds against the split**

The shim (its own workspace) enables `features = ["json"]` on the facade:

```bash
cargo test --manifest-path crates/ab-aozora-cli/Cargo.toml
```

Expected: PASS (goldens are env-gated; the compile + non-golden tests must
be green).

- [ ] **Step 6: Add the feature-graph guard script**

Create `tests/workspace-no-preserve-order.sh` (mode 755):

```bash
#!/usr/bin/env bash
# Guards the feature-unification hazard recorded in
# docs/handoffs/2026-07-10-parser-fork-provenance.md: no crate in the ROOT
# workspace feature graph may activate serde_json/preserve_order, or every
# canonical-JSON producer (ab-aat-to-parser-ir, ab-aozora-aat) silently
# flips to insertion-order output.
set -euo pipefail
repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
export RUSTC_WRAPPER= SCCACHE_DISABLE=1
graph="$(cargo tree --manifest-path "$repo_root/Cargo.toml" --workspace -e features -f '{p} {f}')"
if grep -F "preserve_order" <<<"$graph"; then
  echo "FAIL: serde_json/preserve_order is active in the root workspace" >&2
  exit 1
fi
echo "OK: no preserve_order in the root workspace feature graph"
```

Run it: `tests/workspace-no-preserve-order.sh` → expected `OK` line, exit 0.

- [ ] **Step 7: Commit**

```bash
git add crates/ab-aozora-facade tests/workspace-no-preserve-order.sh
git commit -m "feat(parser): split facade entries feature from preserve_order json serialization"
```

---

### Task 2: Converter canonicalization + preserve-order canary

**Files:**
- Create: `tools/preserve-order-canary/Cargo.toml`,
  `tools/preserve-order-canary/src/lib.rs`,
  `tools/preserve-order-canary/tests/canonical.rs`
- Modify: `crates/ab-aat-to-parser-ir/src/lib.rs` (or a new
  `canonical_json.rs` module) — explicit key sorting
- Modify: root `Cargo.toml` `exclude` list (add the canary)
- Test: existing `crates/ab-aat-to-parser-ir` integration suite (57 tests —
  the byte-no-op proof)

**Interfaces:**
- Consumes: `ab-aat-to-parser-ir`'s serialization call sites (find with
  `grep -rn "serde_json::to_" crates/ab-aat-to-parser-ir/src`).
- Produces: `pub fn sort_keys_deep(value: serde_json::Value) ->
  serde_json::Value` exported from `ab_aat_to_parser_ir`, applied at every
  call site that serializes `serde_json::Value` (or structs containing
  `serde_json::Map` fields) into persisted/compared output.

The canary is TDD for the hazard: with `preserve_order` ON in its own
workspace, it reproduces the 50/57-failure class, then proves the fix.

- [ ] **Step 1: Create the canary crate** (working directory:
  `ab-validator/`)

`tools/preserve-order-canary/Cargo.toml`:

```toml
# Standalone workspace ON PURPOSE (mirrors the adapters/ pattern): this
# crate exists to turn serde_json/preserve_order ON and prove that
# ab-aat-to-parser-ir's canonical JSON output does not depend on
# serde_json's map ordering. It must NEVER join the root workspace — that
# would leak preserve_order into every root-workspace crate (the exact
# hazard it guards; see docs/handoffs/2026-07-10-parser-fork-provenance.md).
[package]
name = "preserve-order-canary"
version = "0.1.0"
edition = "2021"
publish = false

[workspace]

[dependencies]
ab-aat-to-parser-ir = { path = "../../crates/ab-aat-to-parser-ir" }
serde_json = { version = "1.0", features = ["preserve_order"] }
```

`src/lib.rs`: empty (`// test-only crate; see tests/canonical.rs`).

`tests/canonical.rs`:

```rust
//! Runs with serde_json/preserve_order ACTIVE (this workspace enables it),
//! so `serde_json::Map` preserves insertion order and these tests are a
//! genuine probe of explicit sorting — in the root workspace (BTreeMap)
//! they would pass vacuously.
use serde_json::{json, Map, Value};

#[test]
fn sort_keys_deep_sorts_insertion_ordered_maps() {
    let mut inner = Map::new();
    inner.insert("zeta".into(), json!(1));
    inner.insert("alpha".into(), json!([{"b": 2, "a": 3}]));
    let mut outer = Map::new();
    outer.insert("meta".into(), Value::Object(inner));
    outer.insert("blocks".into(), json!([]));
    let sorted = ab_aat_to_parser_ir::sort_keys_deep(Value::Object(outer));
    let text = serde_json::to_string(&sorted).unwrap();
    assert_eq!(
        text,
        r#"{"blocks":[],"meta":{"alpha":[{"a":3,"b":2}],"zeta":1}}"#
    );
}
```

Add a second test in the same file exercising the real conversion path:
identify the public `ab_aat_to_parser_ir` function the integration suite /
CLI uses to produce serialized parser-IR from an AAT `Value` (read
`crates/ab-aat-to-parser-ir/src/lib.rs` and `tests/`), call it on one small
AAT fixture copied from the existing integration fixtures, and assert the
output equals a golden string generated by running the SAME call in the
root workspace (generate once, paste as the expected constant with a
comment naming the generating command). If no public function covers the
serialization step, export a minimal wrapper from the converter crate
(serialization only — no logic change) and use that from both this test and
the existing call site.

- [ ] **Step 2: Register the canary as excluded and run it — expect FAIL**

In root `Cargo.toml` `exclude`, add (keep the existing hazard comment
intact):

```toml
    # excluded: enables serde_json/preserve_order deliberately, to prove
    # converter canonicalization; must never join the root workspace.
    "tools/preserve-order-canary",
```

```bash
export RUSTC_WRAPPER= SCCACHE_DISABLE=1
cargo test --manifest-path tools/preserve-order-canary/Cargo.toml
```

Expected: FAIL — `sort_keys_deep` does not exist yet (compile error), and
after you stub it as identity, the assertions fail under preserve_order.

- [ ] **Step 3: Implement `sort_keys_deep` and apply it**

In `crates/ab-aat-to-parser-ir` (new module `canonical_json.rs`, re-exported
from `lib.rs`):

```rust
use serde_json::Value;

/// Recursively rebuild `value` inserting object keys in sorted order.
/// Under default serde_json (BTreeMap) this is a byte-level no-op; under
/// preserve_order (IndexMap) sorted insertion restores canonical output.
/// Applied at serialization call sites so canonical bytes never depend on
/// the workspace feature graph.
pub fn sort_keys_deep(value: Value) -> Value {
    match value {
        Value::Object(map) => {
            let mut pairs: Vec<(String, Value)> = map.into_iter().collect();
            pairs.sort_by(|a, b| a.0.cmp(&b.0));
            let mut sorted = serde_json::Map::new();
            for (key, val) in pairs {
                sorted.insert(key, sort_keys_deep(val));
            }
            Value::Object(sorted)
        }
        Value::Array(items) => {
            Value::Array(items.into_iter().map(sort_keys_deep).collect())
        }
        leaf => leaf,
    }
}
```

Then apply it at call sites: `grep -rn "serde_json::to_" crates/ab-aat-to-parser-ir/src`
and, for every site that serializes a `Value` (or a struct with
`serde_json::Map` fields) into output that is persisted or compared against
goldens, route through `sort_keys_deep` first. Do NOT convert
struct-serialization sites to `to_value` (that would alphabetize struct
fields and change current bytes). Iterate with the canary: re-run Step 2's
command until it passes — the failing canary run tells you which paths
still drift.

- [ ] **Step 4: Prove the byte no-op in the root workspace**

```bash
cargo test -p ab-aat-to-parser-ir
```

Expected: full suite PASS with zero fixture updates (57 integration tests
byte-compare against canonical goldens; any golden change means Step 3
touched a struct path — revert that site).

- [ ] **Step 5: Commit**

```bash
git add crates/ab-aat-to-parser-ir tools/preserve-order-canary Cargo.toml
git commit -m "fix(parser-ir): canonical JSON output independent of serde_json map ordering, proven by preserve-order canary"
```

---

### Task 3: `crates/ab-aozora-aat` — the ported library

**Files:**
- Create: `crates/ab-aozora-aat/Cargo.toml`,
  `crates/ab-aozora-aat/src/lib.rs`,
  `crates/ab-aozora-aat/tests/reference_parity.rs`,
  `crates/ab-aozora-aat/tests/data/` (samples copied from
  `crates/ab-aozora-cli/tests/data/` — including the Shift_JIS sample)
- Modify: root `Cargo.toml` `members` (add `crates/ab-aozora-aat`)

**Interfaces:**
- Consumes: Task 1's `entries` feature
  (`ab-aozora-facade = { path = "../ab-aozora-facade", features = ["entries"] }`),
  plus `Document`/`Tree` from the facade root and
  `ab_aozora_facade::encoding::decode_auto`.
- Produces (Task 4 consumes):
  `pub fn aat_json_from_bytes(bytes: &[u8]) -> anyhow::Result<Vec<u8>>` and
  `pub fn adapter_version() -> String`.

- [ ] **Step 1: Scaffold** (working directory: `ab-validator/`)

`crates/ab-aozora-aat/Cargo.toml`: `[package]` name `ab-aozora-aat`,
version `0.1.0`, `edition.workspace = true`, `license = "MIT OR
Apache-2.0"`, `publish = false`, `[lints] workspace = true`. Dependencies:
`ab-aozora-facade = { path = "../ab-aozora-facade", features = ["entries"] }`
— **never** `json` — plus exactly the crates the ported code needs, copied
at the versions in `adapters/aozora/Cargo.toml` (expect `anyhow`, `serde`,
`serde_json` (default features), `sha2`; check the frozen manifest, take
only what the lib code uses — `clap` stays out). Add the crate to root
`Cargo.toml` `members` after `"crates/ab-aozora-corpus"`.

- [ ] **Step 2: Port `adapters/aozora/src/lib.rs` by copy**

Copy `adapters/aozora/src/lib.rs` (946 lines) to
`crates/ab-aozora-aat/src/lib.rs`, then make ONLY these changes:

1. Delete the subprocess plumbing: `fn inspect<T>(…)` (line ~847), `fn
   run_aozora(…)` (line ~870), the `Envelope` type and its `schemaVersion`
   check, and `pub fn html_from_bytes` (the render path rides the upstream
   binary; it is not absorbed).
2. Replace the three `inspect::<…>(…)` calls inside `aat_json_from_bytes`
   with in-process projections. The shim
   (`crates/ab-aozora-cli/src/main.rs:71-79`) is the binding reference for
   the facade call pattern. New code:

```rust
use ab_aozora_facade::{json, Document};

fn projections(
    span_text: &str,
) -> Result<(Vec<AozoraNode>, Vec<AozoraDiagnostic>, Vec<AozoraGaiji>)> {
    // Mirrors the upstream binary's own stdin handling: each `aozora
    // inspect` subprocess ran decode_auto over the bytes the adapter piped
    // in (already-valid UTF-8 passes through unchanged).
    let source = ab_aozora_facade::encoding::decode_auto(span_text.as_bytes())
        .map_err(|err| anyhow::anyhow!("decode_auto: {err:?}"))?;
    let doc = Document::new(source.clone());
    let tree = doc.parse();
    let nodes = from_entries(json::node_entries(&tree))?;
    let diagnostics = from_entries(json::diagnostic_entries(tree.diagnostics()))?;
    let gaiji = from_entries(json::gaiji_entries(&source))?;
    Ok((nodes, diagnostics, gaiji))
}

/// Same data path as the deleted wire hop: the facade's Serialize impls
/// (which produced the inspect JSON) feed the adapter's Deserialize types.
/// Deserialization is key-order-independent, so no preserve_order needed.
fn from_entries<S: serde::Serialize, T: serde::de::DeserializeOwned>(
    entries: Vec<S>,
) -> Result<Vec<T>> {
    Ok(serde_json::from_value(serde_json::to_value(entries)?)?)
}

// The wire envelope's schemaVersion check becomes a compile-time pin: the
// from_entries round-trip is only valid against the wire shape this port
// was written for.
const _: () = assert!(json::SCHEMA_VERSION == 2);
```

Adapt the exact signatures the shim shows if they differ (e.g.
`decode_auto`'s error type, `tree.diagnostics()`); the shim compiles today
against these APIs — match it, don't guess.

3. Replace identity. Delete the `AB_AOZORA_BIN`-shelling
   `adapter_version()` (line ~893) and `VERSION_PREFIX`; add:

```rust
/// Identity fields per the executable-boundary contract: adapter id,
/// adapter version, AAT schema version, build identity.
pub fn adapter_version() -> String {
    let git_rev = option_env!("AB_AOZORA_GIT_REV").unwrap_or("unknown");
    format!(
        "ab-aozora {} aat-schema 1 facade {} wire-schema {} (git {git_rev})",
        env!("CARGO_PKG_VERSION"),
        ab_aozora_facade_version(),
        ab_aozora_facade::json::SCHEMA_VERSION,
    )
}

fn ab_aozora_facade_version() -> &'static str {
    // The facade crate version stands in for the deleted wire
    // schemaVersion check as the compatibility coordinate.
    "0.1.0"
}
```

(`AB_AOZORA_GIT_REV` is optional build env; absent → `unknown`, never a
build failure.)

4. In `build_aat` (line ~211 region), change exactly one string:
   `"adapter": "aozora"` → `"adapter": "ab-aozora"`. `adapter_version` now
   calls the new function. **These are the only two output differences** —
   they are the parity allowlist.
5. Wire `aat_json_from_bytes` to `projections()` (same order: nodes,
   diagnostics, gaiji; same `build_aat` call; same
   `serde_json::to_writer` + trailing `\n`).
6. Keep the `#[cfg(test)]` module (line ~916, source-decoding vectors)
   verbatim, including its `include_str!` fixture — copy the fixture file
   it references into the new crate at the same relative path.

- [ ] **Step 3: Build and run the ported unit tests**

```bash
export RUSTC_WRAPPER= SCCACHE_DISABLE=1
cargo test -p ab-aozora-aat
tests/workspace-no-preserve-order.sh
```

Expected: unit tests PASS; guard prints `OK`.

- [ ] **Step 4: Reference micro-parity test (env-gated)**

Copy the shim's sample source files
(`crates/ab-aozora-cli/tests/data/*`) into
`crates/ab-aozora-aat/tests/data/`. Create
`crates/ab-aozora-aat/tests/reference_parity.rs`:

```rust
//! Byte parity against the frozen adapter, modulo the two identity
//! pointers. Env-gated: set AB_REFERENCE_ADAPTER_BIN (a built
//! adapters/aozora binary) and AB_AOZORA_BIN (the pinned upstream aozora)
//! or the test skips (prints SKIP, passes) — mirrors the shim goldens'
//! env-gating so `cargo test --workspace` stays hermetic.
use std::io::Write;
use std::process::{Command, Stdio};

fn normalize(mut bytes: Vec<u8>) -> Vec<u8> {
    let doc: serde_json::Value = serde_json::from_slice(&bytes).unwrap();
    let meta = doc.get("meta").and_then(|m| m.as_object()).unwrap();
    for key in ["adapter", "adapter_version"] {
        let value = serde_json::to_string(meta.get(key).unwrap()).unwrap();
        let needle = format!("\"{key}\":{value}");
        let hay = String::from_utf8(bytes.clone()).unwrap();
        assert_eq!(
            hay.matches(&needle).count(),
            1,
            "expected exactly one serialized occurrence of {needle}"
        );
        bytes = hay
            .replacen(&needle, &format!("\"{key}\":\"__X__\""), 1)
            .into_bytes();
    }
    bytes
}

#[test]
fn byte_parity_with_frozen_adapter_modulo_identity() {
    let (Ok(reference_bin), Ok(_)) = (
        std::env::var("AB_REFERENCE_ADAPTER_BIN"),
        std::env::var("AB_AOZORA_BIN"),
    ) else {
        eprintln!("SKIP: AB_REFERENCE_ADAPTER_BIN / AB_AOZORA_BIN not set");
        return;
    };
    for entry in std::fs::read_dir(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/tests/data"
    ))
    .unwrap()
    {
        let path = entry.unwrap().path();
        let bytes = std::fs::read(&path).unwrap();
        let mut child = Command::new(&reference_bin)
            .args(["--mode", "aat"])
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .spawn()
            .unwrap();
        child.stdin.take().unwrap().write_all(&bytes).unwrap();
        let reference = child.wait_with_output().unwrap();
        assert!(reference.status.success(), "reference failed on {path:?}");
        let ours = ab_aozora_aat::aat_json_from_bytes(&bytes).unwrap();
        assert_eq!(
            normalize(ours),
            normalize(reference.stdout),
            "byte divergence on {path:?}"
        );
    }
}
```

Run it for real (not skipped):

```bash
cargo build --manifest-path adapters/aozora/Cargo.toml --release
upstream="$(nix build .#upstream-parser-aozora --no-link --print-out-paths)/bin/aozora"
AB_REFERENCE_ADAPTER_BIN="$PWD/adapters/aozora/target/release/aozora-adapter" \
AB_AOZORA_BIN="$upstream" cargo test -p ab-aozora-aat --test reference_parity -- --nocapture
```

Expected: PASS on every sample (any divergence is a port defect — fix by
re-copying the diverging function from the frozen adapter). Note: if the
reference adapter's decode differs on stdin-vs-corpus nuances, the samples
are corpus-representative source files; do not "fix" a divergence by
editing the test.

- [ ] **Step 5: Commit**

```bash
git add crates/ab-aozora-aat Cargo.toml
git commit -m "feat(parser): ab-aozora-aat native AAT emission ported from the frozen adapter"
```

---

### Task 4: `crates/ab-aozora` binary + nix package

**Files:**
- Create: `crates/ab-aozora/Cargo.toml`, `crates/ab-aozora/src/main.rs`,
  `crates/ab-aozora/tests/wire.rs`
- Modify: root `Cargo.toml` `members`; `flake.nix` (package `ab-aozora`)

**Interfaces:**
- Consumes: `ab_aozora_aat::{aat_json_from_bytes, adapter_version}`
  (Task 3).
- Produces: binary `ab-aozora` — stdin bytes → AAT JSON on stdout; flags
  `--version`, `--mode aat` (default aat); exits 0 success / 1 fatal; nix
  attr `.#ab-aozora`. Tasks 6–10 invoke it.

- [ ] **Step 1: Crate** (working directory: `ab-validator/`)

`Cargo.toml`: package `ab-aozora` 0.1.0, `edition.workspace = true`,
`publish = false`, `[lints] workspace = true`; sole dependency
`ab-aozora-aat = { path = "../ab-aozora-aat" }`. Add to root `members`.

`src/main.rs`:

```rust
//! Permanent stdin→AAT harness-edge binary (executable-boundary contract,
//! docs/superpowers/specs/2026-07-10-consolidated-parser-phase2-absorption-design.md).
//! ab-check spawns `<adapter> --mode aat` and probes `<adapter> --version`.
//! Exit 0 = success, 1 = fatal. Exit 2 is reserved by the wire contract but
//! deliberately not emitted: the frozen adapter never exits 2 and the
//! conformance harness treats nonzero as adapter error.
use std::io::{Read, Write};
use std::process::ExitCode;

const USAGE: &str = "usage: ab-aozora [--mode aat] [--version]  \
(source bytes on stdin; one AAT JSON document on stdout)";

fn main() -> ExitCode {
    let mut args = std::env::args().skip(1);
    while let Some(arg) = args.next() {
        match arg.as_str() {
            "--version" => {
                println!("{}", ab_aozora_aat::adapter_version());
                return ExitCode::SUCCESS;
            }
            "--mode" => match args.next().as_deref() {
                Some("aat") => {}
                other => {
                    eprintln!(
                        "ab-aozora: unsupported --mode {:?} (only \"aat\")\n{USAGE}",
                        other.unwrap_or("<missing>")
                    );
                    return ExitCode::FAILURE;
                }
            },
            other => {
                eprintln!("ab-aozora: unknown argument {other:?}\n{USAGE}");
                return ExitCode::FAILURE;
            }
        }
    }
    let mut bytes = Vec::new();
    if let Err(err) = std::io::stdin().read_to_end(&mut bytes) {
        eprintln!("ab-aozora: failed to read stdin: {err}");
        return ExitCode::FAILURE;
    }
    // aat_json_from_bytes builds the full document in memory: on Err
    // nothing has been written to stdout (no-partial-output contract).
    match ab_aozora_aat::aat_json_from_bytes(&bytes) {
        Ok(out) => {
            if let Err(err) = std::io::stdout().write_all(&out) {
                eprintln!("ab-aozora: failed to write stdout: {err}");
                return ExitCode::FAILURE;
            }
            ExitCode::SUCCESS
        }
        Err(err) => {
            eprintln!("ab-aozora: {err:#}");
            ExitCode::FAILURE
        }
    }
}
```

- [ ] **Step 2: Wire tests**

`crates/ab-aozora/tests/wire.rs`:

```rust
use std::io::Write;
use std::process::{Command, Stdio};

fn bin() -> &'static str {
    env!("CARGO_BIN_EXE_ab-aozora")
}

#[test]
fn version_carries_the_identity_fields() {
    let out = Command::new(bin()).arg("--version").output().unwrap();
    assert!(out.status.success());
    let text = String::from_utf8(out.stdout).unwrap();
    for field in ["ab-aozora", "aat-schema 1", "wire-schema 2", "git "] {
        assert!(text.contains(field), "--version missing {field:?}: {text}");
    }
}

#[test]
fn emits_one_aat_document_with_rotated_identity() {
    let mut child = Command::new(bin())
        .args(["--mode", "aat"])
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .spawn()
        .unwrap();
    child
        .stdin
        .take()
        .unwrap()
        .write_all("底本：テスト\n".as_bytes())
        .unwrap();
    let out = child.wait_with_output().unwrap();
    assert!(out.status.success());
    let doc: serde_json::Value = serde_json::from_slice(&out.stdout).unwrap();
    assert_eq!(doc["meta"]["adapter"], "ab-aozora");
}

#[test]
fn usage_errors_exit_1_with_empty_stdout() {
    for argv in [vec!["--mode", "html"], vec!["--frobnicate"]] {
        let out = Command::new(bin()).args(&argv).output().unwrap();
        assert_eq!(out.status.code(), Some(1), "argv {argv:?}");
        assert!(out.stdout.is_empty(), "partial stdout on {argv:?}");
        assert!(!out.stderr.is_empty());
    }
}
```

Add `serde_json` to `[dev-dependencies]`. Run:

```bash
export RUSTC_WRAPPER= SCCACHE_DISABLE=1
cargo test -p ab-aozora
tests/workspace-no-preserve-order.sh
```

Expected: 3/3 PASS; guard `OK`.

- [ ] **Step 3: Nix package**

In `flake.nix`, next to the `abCheck = mkRustBin { … }` definition
(~line 1671), add:

```nix
        # Permanent stdin→AAT fork adapter binary (Phase 2). Packaged so
        # run-aat-full.sh can pin it by content as a first-class lane.
        abAozora = mkRustBin {
          pname = "ab-aozora";
          cargoBuildFlags = [
            "--package"
            "ab-aozora"
          ];
        };
```

(drop the `env` attr `abCheck` has — `ab-aozora` needs no `AB_ABC_ROOT`; if
`mkRustBin` requires the attr, pass an empty set matching its signature).
In the `packages` attrset (~line 1809, next to `ab-check = abCheck;`), add
`ab-aozora = abAozora;`.

```bash
nix build .#ab-aozora --no-link --print-out-paths
"$(nix build .#ab-aozora --no-link --print-out-paths)/bin/ab-aozora" --version
```

Expected: builds; `--version` prints the identity line.

- [ ] **Step 4: Commit**

```bash
git add crates/ab-aozora Cargo.toml flake.nix
git commit -m "feat(parser): permanent ab-aozora stdin->AAT binary with nix package"
```

---

### Task 5: Byte-parity comparator + reports pytest in CI

**Files:**
- Rewrite: `reports/aat-fidelity/compare-aat-dumps.py`
- Modify: `reports/aat-fidelity/tests/test_compare_aat_dumps.py`
- Modify: `flake.nix` (new check `reports-pytest-check`)

**Interfaces:**
- Consumes: dump layout `<dump>/aat/*.json` (one file per work, written by
  ab-check).
- Produces: `compare-aat-dumps.py [--bytes] DIR_A DIR_B` — exit 0 parity /
  1 divergence / 2 usage-or-reference error. With `--bytes`, the exit code
  reflects the BYTE verdict; the JSON summary carries both `semantic` and
  `bytes` blocks. Task 8 runs it with `--bytes`.

- [ ] **Step 1: Rewrite the comparator** (working directory:
  `ab-validator/`)

Replace `reports/aat-fidelity/compare-aat-dumps.py` with:

```python
#!/usr/bin/env python3
"""Parity between two AAT dumps (directories of per-work *.json).

Two instruments over the same allowlist — exactly the identity pointers
/meta/adapter and /meta/adapter_version (Phase 2 rotates both):

  semantic  parsed-JSON equality after REMOVING the two pointers. Key order
            and number formatting are invisible. Localization diagnostic.
  bytes     (--bytes) raw-byte equality after SUBSTITUTING each pointer's
            exact serialized occurrence with a fixed placeholder. The
            occurrence must appear exactly once per document (fail closed:
            exit 2) so drift can never hide inside the substitution.

Without --bytes the exit code reflects semantic parity (Phase 1 Gate A
behavior); with --bytes it reflects byte parity, and the semantic result is
still computed and reported for localization.

Exit 0 = parity; 1 = divergence; 2 = usage/reference error.
"""

import argparse
import json
import pathlib
import sys

POINTERS = ("adapter", "adapter_version")
PLACEHOLDER = "__AB_PARITY_IDENTITY__"


def normalize(doc):
    meta = doc.get("meta")
    if isinstance(meta, dict):
        meta = dict(meta)
        for key in POINTERS:
            meta.pop(key, None)
        doc = dict(doc)
        doc["meta"] = meta
    return doc


def substitute_identity(raw: bytes, path: pathlib.Path) -> bytes:
    doc = json.loads(raw)
    meta = doc.get("meta")
    if not isinstance(meta, dict):
        print(f"ERROR: {path}: no /meta object", file=sys.stderr)
        raise SystemExit(2)
    for key in POINTERS:
        if key not in meta:
            print(f"ERROR: {path}: missing /meta/{key}", file=sys.stderr)
            raise SystemExit(2)
        value = json.dumps(meta[key], ensure_ascii=False).encode("utf-8")
        # serde_json emits compact (`"k":v`); tolerate a single space after
        # the colon in case a producer pretty-prints. Total must be exactly 1.
        needles = [b'"%s":%s' % (key.encode(), value),
                   b'"%s": %s' % (key.encode(), value)]
        counts = [raw.count(n) for n in needles]
        if sum(counts) != 1:
            print(
                f"ERROR: {path}: expected exactly 1 serialized occurrence "
                f"of /meta/{key}, found {sum(counts)}",
                file=sys.stderr,
            )
            raise SystemExit(2)
        needle = needles[0] if counts[0] else needles[1]
        raw = raw.replace(
            needle, b'"%s":"%s"' % (key.encode(), PLACEHOLDER.encode()), 1
        )
    return raw


def load_dir(d: pathlib.Path) -> dict:
    files = {p.name: p for p in sorted(d.glob("*.json"))}
    if not files:
        print(f"ERROR: no *.json under {d}", file=sys.stderr)
        raise SystemExit(2)
    return files


def main() -> int:
    ap = argparse.ArgumentParser()
    ap.add_argument("dir_a")
    ap.add_argument("dir_b")
    ap.add_argument("--bytes", action="store_true")
    args = ap.parse_args()
    a_files = load_dir(pathlib.Path(args.dir_a))
    b_files = load_dir(pathlib.Path(args.dir_b))
    missing = sorted(set(a_files) ^ set(b_files))
    shared = sorted(set(a_files) & set(b_files))
    sem_diverged, byte_diverged = [], []
    for name in shared:
        raw_a = a_files[name].read_bytes()
        raw_b = b_files[name].read_bytes()
        if normalize(json.loads(raw_a)) != normalize(json.loads(raw_b)):
            sem_diverged.append(name)
        if args.bytes and substitute_identity(
            raw_a, a_files[name]
        ) != substitute_identity(raw_b, b_files[name]):
            byte_diverged.append(name)
    summary = {
        "compared": len(shared),
        "missing_count": len(missing),
        "missing_sample": missing[:20],
        "semantic": {
            "diverged_count": len(sem_diverged),
            "diverged_sample": sem_diverged[:20],
        },
    }
    if args.bytes:
        summary["bytes"] = {
            "diverged_count": len(byte_diverged),
            "diverged_sample": byte_diverged[:20],
        }
    print(json.dumps(summary, indent=2))
    gate_diverged = byte_diverged if args.bytes else sem_diverged
    return 0 if not missing and not gate_diverged else 1


if __name__ == "__main__":
    raise SystemExit(main())
```

- [ ] **Step 2: Extend the unit tests — write them, watch new ones fail
  first**

In `reports/aat-fidelity/tests/test_compare_aat_dumps.py`, keep the
existing semantic tests (update any that referenced the old single-pointer
normalize: it now also strips `/meta/adapter`) and add byte-mode tests
(follow the file's existing fixture/helper style for invoking the script
and building dump dirs):

```python
def _doc(adapter="aozora", version="v1", extra=""):
    return (
        '{"blocks":[],"meta":{"adapter":"%s","adapter_version":"%s",'
        '"parse_complete":true%s},"version":1,"work_id":"w"}' % (adapter, version, extra)
    )


def test_bytes_pass_when_only_identity_differs(tmp_path):
    a, b = _mkdumps(tmp_path, _doc("aozora", "old"), _doc("ab-aozora", "new"))
    assert _run(a, b, "--bytes").returncode == 0


def test_bytes_fail_on_key_order_drift_that_semantic_misses(tmp_path):
    reordered = (
        '{"version":1,"work_id":"w","blocks":[],"meta":{"adapter":"aozora",'
        '"adapter_version":"v1","parse_complete":true}}'
    )
    a, b = _mkdumps(tmp_path, _doc(), reordered)
    result = _run(a, b, "--bytes")
    assert result.returncode == 1
    summary = json.loads(result.stdout)
    assert summary["semantic"]["diverged_count"] == 0
    assert summary["bytes"]["diverged_count"] == 1


def test_bytes_fail_closed_when_identity_value_repeats(tmp_path):
    # the adapter_version string also appears in body text -> count 2 -> exit 2
    doc = _doc(extra=',"warnings":[{"message":"\\"adapter_version\\":\\"v1\\""}]')
    a, b = _mkdumps(tmp_path, doc, doc)
    assert _run(a, b, "--bytes").returncode == 2


def test_bytes_fail_closed_on_missing_pointer(tmp_path):
    doc = '{"blocks":[],"meta":{"adapter":"aozora"},"version":1,"work_id":"w"}'
    a, b = _mkdumps(tmp_path, doc, doc)
    assert _run(a, b, "--bytes").returncode == 2
```

(`_mkdumps`/`_run` = the file's existing helpers, or add thin ones writing
`a/x.json`, `b/x.json` and invoking the script via `subprocess`.) Run:

```bash
python -m pytest reports/aat-fidelity/tests/test_compare_aat_dumps.py -q
```

Expected: PASS after Step 1 (write tests first if iterating; the
key-order-drift test is the one that fails against the old semantic-only
script).

- [ ] **Step 3: Wire `reports/**` pytest into the flake checks**

First establish the passing set locally:

```bash
python -m pytest reports/aat-fidelity/tests reports/lib/tests -q
```

Then add a check in `flake.nix`, modeled on the existing pytest oracle
check (~line 1295, the aozora2html mapper one — same python-with-pytest
input pattern):

```nix
        reportsPytestCheck = pkgs.runCommand "reports-pytest-check" {
          nativeBuildInputs = [ (pkgs.python3.withPackages (ps: [ ps.pytest ])) ];
        } ''
          cd ${source}
          python -m pytest \
            ab-validator/reports/aat-fidelity/tests \
            ab-validator/reports/lib/tests \
            -q
          touch "$out"
        '';
```

Adjust the `source` reference and path prefix to match how the neighboring
checks reference the repo tree (copy the working idiom, don't invent one).
If a specific existing test file cannot run hermetically in the nix
sandbox, exclude it via `--ignore=<file>` **with a comment naming the file
and the reason** — no silent narrowing. Register in the `checks` attrset.

```bash
nix build .#checks.$(nix eval --raw --impure --expr builtins.currentSystem).reports-pytest-check
```

Expected: builds (tests pass in sandbox).

- [ ] **Step 4: Commit**

```bash
git add reports/aat-fidelity/compare-aat-dumps.py reports/aat-fidelity/tests/test_compare_aat_dumps.py flake.nix
git commit -m "feat(harness): byte-parity comparator mode over the two identity pointers; reports pytest wired into nix checks"
```

---

### Task 6: `ab-aozora` harness lane (`run-aat-full.sh`, perf runner)

**Files:**
- Modify: `reports/aat-fidelity/run-aat-full.sh`
- Modify: `reports/aat-fidelity/run-perf-workset.py`
- Create: `tests/ab-aozora-lane-smoke.sh`
- Create: `reports/aat-fidelity/tests/test_perf_workset_lanes.py`

**Interfaces:**
- Consumes: `.#ab-aozora` nix attr (Task 4); the binary's `--mode aat` /
  `--version` surface.
- Produces: `run-aat-full.sh --adapter ab-aozora [--adapter-bin PATH]`
  (Task 8 uses it); `run-perf-workset.py --baseline-cmd … --baseline-id-bin
  … --candidate-cmd … --candidate-id-bin …` (Task 9 uses it).

- [ ] **Step 1: `run-aat-full.sh` — new adapter id + `--adapter-bin`**
  (working directory: `ab-validator/`)

Read the script top to bottom once; then:

1. Usage text: `ADAPTER: aozora | ab-aozora | aozora2html | aozora-epub3`;
   document `--adapter-bin PATH: only valid with --adapter ab-aozora` with
   the same identity wording as the existing `--aozora-bin` block
   (lines 29-36) — it is what actually runs AND what is recorded.
2. Arg parsing: accept `--adapter-bin` into `adapter_bin_override`.
3. Validation (near the existing adapter-id validation): `--aozora-bin`
   only with `--adapter aozora` (already the case — keep), `--adapter-bin`
   only with `--adapter ab-aozora`, exit 2 otherwise.
4. Resolution block (next to the `adapter_id == "aozora"` branch,
   ~line 256):

```bash
if [[ "$adapter_id" == "ab-aozora" ]]; then
  if [[ -n "$adapter_bin_override" ]]; then
    if [[ ! -x "$adapter_bin_override" ]]; then
      printf -- '--adapter-bin not found or not executable: %s\n' "$adapter_bin_override" >&2
      exit 2
    fi
    adapter_bin_override="$(cd "$(dirname "$adapter_bin_override")" && pwd)/$(basename "$adapter_bin_override")"
    if ! adapter_bin_override_version="$("$adapter_bin_override" --version)"; then
      printf -- '--adapter-bin --version failed: %s\n' "$adapter_bin_override" >&2
      exit 2
    fi
    adapter_bin_override_sha256="$(sha256sum "$adapter_bin_override" | cut -d' ' -f1)"
    adapter="$adapter_bin_override"
  else
    adapter="$(nix build "$repo_root#ab-aozora" --no-link --print-out-paths)/bin/ab-aozora"
  fi
  adapter_hash_target="$adapter"
fi
```

5. Renderer resolution: the `ab-aozora` lane sets no `renderer_attr` and no
   renderer env export — the adapter binary is the complete generator
   identity. For staleness plumbing, mirror the `--aozora-bin` override's
   trick: when `--adapter-bin` is set, `renderer_dir` = the override's
   containing directory (so a differing override binary changes
   `renderer_content_hash` and a stale dump is never served); when
   nix-resolved, `renderer_dir` = the nix output dir (content-addressed).
6. Metadata: extend the python heredoc (line ~415) argument list with the
   `adapter_bin_override` / `_sha256` / `_version` triple and record them
   under an `"adapter_bin_override"` key exactly parallel to the existing
   `"aozora_bin_override"` block (~line 491); omit/null the renderer
   identity fields for this adapter id if the heredoc otherwise fabricates
   them.
7. Check `mapper_attr`/`renderer_attr` case-analysis: `ab-aozora` must fall
   through with both empty (grep for where they are set per adapter id).

- [ ] **Step 2: Lane smoke test**

Create `tests/ab-aozora-lane-smoke.sh` (755) modeled structurally on
`tests/aozora-bin-override-smoke.sh` (read it fully first — reuse its env
sourcing, tmp/trap, and assertion style):

- Stub: a bash script `stub-ab-aozora` that on `--version` prints
  `stub-ab-aozora 9.9.9-LANE-PROOF`, otherwise reads stdin and prints one
  minimal schema-valid AAT document whose `meta.adapter_version` is
  `"stub-ab-aozora 9.9.9-LANE-PROOF"` and `meta.adapter` is `"ab-aozora"`
  (crib the required fields from `data/aat-schema.json`; start from
  `{"version":1,"work_id":"stdin","blocks":[],"meta":{…}}` and extend until
  ab-check's validator accepts it).
- Run `reports/aat-fidelity/run-aat-full.sh --adapter ab-aozora
  --adapter-bin "$stub" --work-ids <one work id the existing smoke uses>
  --out-dir "$tmp/out" …` (mirror the existing smoke's corpus/limit flags).
- Assert: `metadata.json` records `adapter_bin_override.path/sha256/version`
  with `9.9.9-LANE-PROOF`; the emitted `aat/*.json` contains
  `9.9.9-LANE-PROOF` (the stub really executed); a second stub with a
  different byte content produces a different `input_set_hash` (identity
  moves — copy the assertion pattern from the existing smoke's tail).
- Also assert the relative-path form works (`cd "$tmp" && … --adapter-bin
  ./stub-ab-aozora`), mirroring the existing smoke's relative-path
  regression.

Run: `tests/ab-aozora-lane-smoke.sh` → expected: prints its PASS lines,
exit 0.

- [ ] **Step 3: Perf runner lanes**

Rewrite `run-perf-workset.py`'s lane interface (docstring included — it
currently hardcodes `<bin> inspect nodes -`, which cannot express either
Phase 2 lane):

- Replace `--baseline-bin`/`--candidate-bin` with four required flags:
  `--baseline-cmd` / `--candidate-cmd` (a full argv string, `shlex.split`,
  run with the work's bytes on stdin — `env VAR=x cmd args` works because
  `env` is argv[0]) and `--baseline-id-bin` / `--candidate-id-bin` (the
  adapter executable whose existence is `-x`-checked and whose sha256 +
  verbatim `--version` output are recorded as that lane's identity —
  fail-closed exit 2 on any of those failing).
- `timed_runs` takes the argv list; everything else (workset sha256
  verification, 1 warm-up + N measured, median/spread, BLOCK on new
  timeouts, BLOCK on >threshold regression) is unchanged.
- Record in the report JSON per lane: `argv`, `id_bin`, `id_bin_sha256`,
  `id_bin_version`.

Create `reports/aat-fidelity/tests/test_perf_workset_lanes.py` with two
tests using a stub executable written into `tmp_path` (a bash script that
reads stdin and exits 0):

```python
def test_report_records_lane_identity(tmp_path):
    # build tiny workset json + corpus file with matching sha256, run the
    # script with --baseline-cmd/--candidate-cmd pointing at the stub and
    # --*-id-bin at the stub; assert report json contains argv, sha256 and
    # version fields for both lanes and verdict PASS.

def test_missing_id_bin_fails_closed(tmp_path):
    # --candidate-id-bin pointing at a non-existent path -> exit 2, no report.
```

(Write them concretely against the script's actual flags; keep runtime
under a few seconds — 1 warmup + 1 measured run via the runs flag if one
exists, add `--runs` if not, defaulting to 5.)

Run: `python -m pytest reports/aat-fidelity/tests/test_perf_workset_lanes.py -q`
→ PASS.

- [ ] **Step 4: Commit**

```bash
git add reports/aat-fidelity/run-aat-full.sh reports/aat-fidelity/run-perf-workset.py tests/ab-aozora-lane-smoke.sh reports/aat-fidelity/tests/test_perf_workset_lanes.py
git commit -m "feat(harness): first-class ab-aozora adapter lane with explicit --adapter-bin identity; perf runner per-lane argv"
```

---

### Task 7: Conformance harness drift fix + justfile lane/guard

**Files:**
- Modify: `reports/parser-conformance/run-aozora-notation-spec.py:72`
- Modify: `justfile` (recipe `aozora-notation-spec-comparison`, lines
  91-121)

**Interfaces:**
- Consumes: `.#ab-aozora` (Task 4); facade wire `SCHEMA_VERSION == 2`.
- Produces: justfile lane labels Task 10 depends on: `aozora-adapter`
  (frozen adapter, aat mode), `ab-aozora` (the new binary, aat mode).

- [ ] **Step 1: Fix the schemaVersion drift** (working directory:
  `ab-validator/`)

`run-aozora-notation-spec.py:72` rejects every inspect envelope whose
`schemaVersion != 1`, but the pinned upstream binary (and the shim) emit
`schemaVersion: 2` (`ab-aozora-facade/src/json.rs` `SCHEMA_VERSION = 2`,
bumped upstream in #435). Reproduce first:

```bash
upstream="$(nix build .#upstream-parser-aozora --no-link --print-out-paths)/bin/aozora"
printf 'テスト\n' | "$upstream" inspect nodes - | python -c 'import json,sys; print(json.load(sys.stdin)["schemaVersion"])'
```

Expected: `2`. Then change line 72 to accept exactly the current wire:

```python
    if value.get("schemaVersion") != 2 or not isinstance(value.get("data"), list):
```

Re-run the full recipe (Step 2's command) and compare the upstream lane's
scores against the frozen 2026-07-08 summary
(`docs/superpowers/reports/2026-07-08-aozora-notation-spec-comparison.summary.json`):
if inspect-mode scores CHANGE (they will if the drift was silently zeroing
inspect scoring), do not touch the frozen report — the delta and its cause
get one paragraph in Task 10's echo report. Record what you observed in
your task report either way.

- [ ] **Step 2: justfile — relabel, add the real lane, guard AOZORA_BIN**

In the `aozora-notation-spec-comparison` recipe:

1. Guard (after the existing absolutization at line 98):

```make
	if [ ! -x "$aozora_bin" ]; then echo "AOZORA_BIN not found or not executable: $aozora_bin" >&2; exit 2; fi; \
```

2. The current line 114 labels the FROZEN adapter lane `ab-aozora` (a
   Phase 1 leftover from when the shim stood in for the fork). Relabel it
   `aozora-adapter`:

```make
		--adapter "aozora-adapter=aat:env AB_AOZORA_BIN=$aozora_bin_q {{repo_root}}/adapters/aozora/target/release/aozora-adapter --mode aat" \
```

3. Add the real `ab-aozora` lane. In the build loop section add:

```make
	export RUSTC_WRAPPER= SCCACHE_DISABLE=1; \
	cargo build --manifest-path "{{repo_root}}/Cargo.toml" --package ab-aozora --release; \
```

   and the adapter line:

```make
		--adapter "ab-aozora=aat:{{repo_root}}/target/release/ab-aozora --mode aat" \
```

- [ ] **Step 3: Run the recipe end to end**

```bash
just aozora-notation-spec-comparison REPORT_MD=/tmp-scratch-phase2-conformance.md SUMMARY_JSON=/tmp-scratch-phase2-conformance.summary.json
```

(Use scratch output paths — this is a shakedown, not the gate; the gate run
with recorded identity is Task 10. Adjust paths to a writable scratch dir.)
Expected: completes; summary JSON contains adapters `aozora`,
`aozora-adapter`, `ab-aozora`, `aozora2`, `aozora-rs`, `aozora2html`,
`aozora-epub3`; the `aozora` inspect lane scores non-degenerately (>0
vectors scored).

Also negative-test the guard:
`just aozora-notation-spec-comparison AOZORA_BIN=/nonexistent` → exits 2
with the clear error.

- [ ] **Step 4: Commit**

```bash
git add reports/parser-conformance/run-aozora-notation-spec.py justfile
git commit -m "fix(conformance): accept wire schemaVersion 2; real ab-aozora lane and AOZORA_BIN guard in justfile"
```

---

### Task 8: Absorption parity gate (hinoki, corpus-wide)

**Files:**
- Create: `docs/superpowers/reports/2026-07-10-phase2-absorption-parity.md`
  and `….summary.json`

**Interfaces:**
- Consumes: Tasks 4–6 merged into the branch; hinoki; run-set
  `reports/aat-fidelity/run-sets/current.json`.
- Produces: frozen parity report citing candidate commit + binary sha256 +
  `--version` (checkpoint contract input for Task 11). The candidate dump
  stays on hinoki until Phase 4 (do not delete it).

- [ ] **Step 1: Record the candidate identity** (working directory:
  `ab-validator/`, then hinoki via ssh)

```bash
git rev-parse HEAD   # CANDIDATE_COMMIT — cite this everywhere
git push origin feat/parser-fork-phase2
```

- [ ] **Step 2: Build the candidate on hinoki**

```bash
ssh hinoki.hyakutake-barbel.ts.net 'cd ~/Projects/soranoha && git fetch origin && git worktree add -f ~/Projects/soranoha/.worktrees/parser-fork-phase2 origin/feat/parser-fork-phase2 2>/dev/null || git -C ~/Projects/soranoha/.worktrees/parser-fork-phase2 reset --hard origin/feat/parser-fork-phase2'
ssh hinoki.hyakutake-barbel.ts.net 'cd ~/Projects/soranoha/.worktrees/parser-fork-phase2/ab-validator && export RUSTC_WRAPPER= SCCACHE_DISABLE=1 && cargo build --package ab-aozora --release && ./target/release/ab-aozora --version && sha256sum ./target/release/ab-aozora'
```

Record sha256 + `--version` verbatim (they go in the report).

- [ ] **Step 3: Resolve the reference fail-closed**

On hinoki, resolve `run-sets/current.json` → dump dir, and verify its
content hash with the run-set library
(`reports/lib/aat_hash.py` — invoke it the way Phase 1's Gate A report
documents; see
`docs/superpowers/reports/2026-07-10-fork-parity-corpus.md` for the exact
invocation). The reference is the adapter-lane dump the run-set names. If
verification fails: STOP, report BLOCKED — never substitute an unpinned
directory.

- [ ] **Step 4: Candidate full-corpus run (detached)**

```bash
ssh hinoki.hyakutake-barbel.ts.net 'cd ~/Projects/soranoha/.worktrees/parser-fork-phase2/ab-validator && AB_DB_ROOT=/db/ab-validator nohup reports/aat-fidelity/run-aat-full.sh --adapter ab-aozora --adapter-bin ./target/release/ab-aozora --jobs 32 --report-id ab-aozora-phase2-parity --out-dir /db/ab-validator/aat-corpus/ab-aozora-phase2-<CANDIDATE_COMMIT7> > ~/phase2-parity-run.log 2>&1 &'
```

(Adjust flags to the script's actual corpus defaults — mirror how the
Phase 1 Gate A run was invoked per its report.) Poll with short ssh calls
(`tail -3 ~/phase2-parity-run.log`); the Phase 1 run took ~5–10 min at 32
jobs.

- [ ] **Step 5: Byte comparison**

```bash
ssh hinoki.hyakutake-barbel.ts.net 'cd ~/Projects/soranoha/.worktrees/parser-fork-phase2/ab-validator && python3 reports/aat-fidelity/compare-aat-dumps.py --bytes <REFERENCE_DUMP>/aat /db/ab-validator/aat-corpus/ab-aozora-phase2-<CANDIDATE_COMMIT7>/aat > ~/phase2-parity-summary.json; echo "exit=$?"; cat ~/phase2-parity-summary.json'
```

Required: exit 0; `compared` = 17886 (or the run-set's exact work count);
`missing_count` 0; `bytes.diverged_count` 0. Any divergence: fetch the
named work's two files, diff, fix the port (Task 3 discipline), re-run from
Step 2 (new candidate commit → all three gates re-cite it).

- [ ] **Step 6: Freeze the report**

Write `docs/superpowers/reports/2026-07-10-phase2-absorption-parity.md`
(+ copy the summary JSON alongside as `….summary.json`): candidate commit,
binary sha256, verbatim `--version`, reference run-set id + content hash,
comparator invocation, result block, dump retention path. Model the
structure on `2026-07-10-fork-parity-corpus.md`.

```bash
git add docs/superpowers/reports/2026-07-10-phase2-absorption-parity.*
git commit -m "test(parser): Phase 2 absorption byte-parity gate evidence"
```

---

### Task 9: Performance gate (hinoki)

**Files:**
- Create: `docs/superpowers/reports/2026-07-10-phase2-perf.md` and `….json`

**Interfaces:**
- Consumes: Task 6's perf-runner lanes; the same CANDIDATE_COMMIT build
  from Task 8 Step 2 (same sha256 — verify, don't rebuild from a different
  rev).
- Produces: frozen perf report (checkpoint input for Task 11).

- [ ] **Step 1: Prepare both lanes on hinoki** (working directory: the
  hinoki worktree's `ab-validator/`, via ssh)

```bash
export RUSTC_WRAPPER= SCCACHE_DISABLE=1
cargo build --manifest-path adapters/aozora/Cargo.toml --release
upstream="$(nix build .#upstream-parser-aozora --no-link --print-out-paths)/bin/aozora"
sha256sum target/release/ab-aozora   # must equal Task 8's recorded sha256
```

Derive the perf-workset corpus with the Phase 1 script (fail-closed):
`reports/aat-fidelity/extract-perf-workset-corpus.py` per its `--help`,
into a path under `/db/ab-validator` (PrivateTmp: never under `/tmp`).

- [ ] **Step 2: Run the protocol**

```bash
python3 reports/aat-fidelity/run-perf-workset.py \
  --workset data/perf-workset.json \
  --baseline-cmd "env AB_AOZORA_BIN=$upstream $PWD/adapters/aozora/target/release/aozora-adapter --mode aat" \
  --baseline-id-bin "$PWD/adapters/aozora/target/release/aozora-adapter" \
  --candidate-cmd "$PWD/target/release/ab-aozora --mode aat" \
  --candidate-id-bin "$PWD/target/release/ab-aozora" \
  --corpus <extracted-workset-corpus-dir> \
  --out ~/phase2-perf.json
```

(1 warm-up + ≥5 measured per lane is the script default; both lanes in this
one session on hinoki = same machine identity.) Required verdict: `PASS` —
no new timeouts, no >10% median regression. Additional spec tripwire: if
the candidate median is slower than baseline AT ALL, record it and
investigate before proceeding (expected direction is faster — subprocess
hops removed); a slowdown under threshold is not a formal block but must be
explained in the report.

- [ ] **Step 3: Freeze the report**

`docs/superpowers/reports/2026-07-10-phase2-perf.md` + the JSON: candidate
commit + sha256 + `--version`, baseline argv + adapter sha256 + upstream
store path, machine identity, per-work medians, verdict, and the explicit
note that this is an end-to-end AAT-production comparison (mode change vs
the Phase 1 inner-binary `inspect nodes` measurement — not comparable
across reports). Commit:

```bash
git add docs/superpowers/reports/2026-07-10-phase2-perf.*
git commit -m "test(parser): Phase 2 end-to-end perf gate evidence"
```

---

### Task 10: Conformance echo gate (local)

**Files:**
- Create: `docs/superpowers/reports/2026-07-10-phase2-conformance-echo.md`
  and `….summary.json`

**Interfaces:**
- Consumes: Task 7's justfile lanes; the same CANDIDATE_COMMIT checked out
  locally (verify `git rev-parse HEAD` matches Tasks 8/9; the locally built
  `ab-aozora` sha256 may differ from hinoki's — record BOTH the local
  sha256 and the commit, the commit is the cross-gate join key, and
  `--version` must match verbatim).
- Produces: frozen echo report (checkpoint input for Task 11).

- [ ] **Step 1: Run the recipe with explicit identity** (working
  directory: `ab-validator/`)

```bash
upstream="$(nix build .#upstream-parser-aozora --no-link --print-out-paths)/bin/aozora"
just aozora-notation-spec-comparison \
  AOZORA_BIN="$upstream" \
  REPORT_MD=docs/superpowers/reports/2026-07-10-phase2-conformance-echo.md \
  SUMMARY_JSON=docs/superpowers/reports/2026-07-10-phase2-conformance-echo.summary.json
```

- [ ] **Step 2: Assert the echo**

Compare, vector-for-vector across both suites (127 P4suta + 30
official-docs seed), the `aozora-adapter` lane vs the `ab-aozora` lane in
the summary JSON:

```bash
python3 - docs/superpowers/reports/2026-07-10-phase2-conformance-echo.summary.json <<'PY'
import json, sys
summary = json.load(open(sys.argv[1]))
# The per-adapter/per-vector shape is the same one the frozen
# docs/superpowers/reports/2026-07-08-aozora-notation-spec-comparison.summary.json
# uses — open that file first and write the extraction against it: pull the
# per-vector outcome lists for labels "aozora-adapter" and "ab-aozora",
# diff them, print every differing vector name, exit 1 if any.
PY
```

Required: zero differing vectors (script exits 0 printing a count line). The two lanes run the same ported logic; any difference is a port
defect → fix, new candidate commit, re-run Tasks 8–10.

- [ ] **Step 3: Freeze**

Append to the report MD (or a companion section): candidate commit, local
`ab-aozora` sha256, verbatim `--version` (must equal Task 8/9's), the echo
verdict (identical vector-for-vector), and the schemaVersion-drift
paragraph from Task 7 Step 1 if upstream inspect scores moved vs the frozen
2026-07-08 summary. Commit:

```bash
git add docs/superpowers/reports/2026-07-10-phase2-conformance-echo.*
git commit -m "test(parser): Phase 2 conformance echo gate evidence"
```

---

### Task 11: Checkpoint verification → shim deletion → handoff update

**Files:**
- Delete: `crates/ab-aozora-cli/` (entire crate: sources, goldens, its
  standalone `Cargo.lock`)
- Modify: root `Cargo.toml` (`exclude` entry + hazard comment)
- Modify: `docs/handoffs/2026-07-10-parser-fork-provenance.md`

**Interfaces:**
- Consumes: the three frozen reports (Tasks 8–10) — the checkpoint
  contract.
- Produces: a workspace with no shim; the provenance handoff as the
  narrowed-hazard + facade-divergence record Phase 3 reads.

- [ ] **Step 1: Verify the checkpoint — do not proceed on any failure**
  (working directory: `ab-validator/`)

```bash
set -o pipefail
ls docs/superpowers/reports/2026-07-10-phase2-absorption-parity.md \
   docs/superpowers/reports/2026-07-10-phase2-perf.md \
   docs/superpowers/reports/2026-07-10-phase2-conformance-echo.md
grep -l "CANDIDATE_COMMIT_FULL_SHA" docs/superpowers/reports/2026-07-10-phase2-*.md | wc -l   # must be 3
grep -h "ab-aozora 0" docs/superpowers/reports/2026-07-10-phase2-*.md | sort -u | wc -l       # verbatim --version identical => 1
```

(Substitute the real candidate sha; the three reports must each state
their passing verdict — read them.) Record the verification in the
progress ledger. Any mismatch = a gate ran against a different candidate:
STOP and re-run the stale gate.

- [ ] **Step 2: Delete the shim**

```bash
git rm -r crates/ab-aozora-cli
```

In root `Cargo.toml`: remove the `crates/ab-aozora-cli` (or
`adapters/…`-adjacent) exclude entry for the shim and rewrite its hazard
comment to point forward: the canary (`tools/preserve-order-canary`) is now
the deliberate preserve_order consumer; the narrowed rule lives in the
provenance handoff.

Reference sweep — must return nothing outside frozen evidence and
historical handoff prose:

```bash
grep -rn "ab-aozora-cli" --include="*.toml" --include="*.nix" --include="*.rs" --include="*.sh" --include="*.py" --include="justfile" .
```

Fix any hit (build files, harness code); hits inside
`docs/superpowers/reports/**`, the progress ledger, and past-tense handoff
sections stay.

- [ ] **Step 3: Full verification battery**

```bash
export RUSTC_WRAPPER= SCCACHE_DISABLE=1
cargo test --workspace
tests/workspace-no-preserve-order.sh
cargo test --manifest-path tools/preserve-order-canary/Cargo.toml
cargo clippy --workspace --all-targets -- -D warnings
cargo deny check
```

Expected: all green (workspace count grows by the two new crates' tests;
1755 was the pre-phase baseline).

- [ ] **Step 4: Update the provenance handoff**

In `docs/handoffs/2026-07-10-parser-fork-provenance.md`:

1. LIFT_SET table: annotate `ab-aozora-facade` — no longer rename-only;
   fork-owned divergence: `entries`/`json` feature split (ADR 0032
   discretion), date + commit.
2. Feature-unification hazard section: append the Phase 2 resolution — the
   narrowed rule verbatim from the spec ("any future crate enabling
   `serde_json/preserve_order` must live outside the root workspace unless
   every workspace consumer of canonical JSON is order-independent by
   construction"), pointer to the canary and the guard script, note that
   converter hardening landed.
3. New short section: shim deleted at Phase 2 end per the checkpoint
   contract, superseded by `ab-aozora`; cite the three gate report paths.

- [ ] **Step 5: Commit (message cites the three reports — checkpoint
  contract)**

```bash
git add -A
git commit -m "chore(parser): delete ab-aozora-cli shim after Phase 2 gates

Evidence (checkpoint contract):
- docs/superpowers/reports/2026-07-10-phase2-absorption-parity.md
- docs/superpowers/reports/2026-07-10-phase2-perf.md
- docs/superpowers/reports/2026-07-10-phase2-conformance-echo.md"
```

---

### Task 12: Identity rotation (conversion audit + abc registry row)

**Files:**
- Modify: `abc/data/aat-parser-ir-compatibility.edn` (one new entry)
- Create: the conversion-audit summary artifact the row cites (place per
  existing convention — see how the current `aozora` row's
  `:evidence_scope` numbers were sourced)

**Interfaces:**
- Consumes: the Task 8 candidate dump on hinoki
  (`/db/ab-validator/aat-corpus/ab-aozora-phase2-<COMMIT7>/aat`); the
  `ab-aat-to-parser-ir` audit subcommand
  (`crates/ab-aat-to-parser-ir/src/main.rs:185` `audit::run_audit` /
  `CorpusAuditConfig` — read `main.rs` for the exact CLI flags); the
  existing accepted `aozora` row in the EDN as the mapping-coordinate
  template.
- Produces: an **unadmitted** exact-match registry row for adapter
  `ab-aozora` (registry activation is Phase 4 — this task changes no
  publication behavior; the `aozora-rs` rows are the precedent for inert
  rows).

- [ ] **Step 1: Run the conversion audit on hinoki** (working directory:
  the hinoki worktree's `ab-validator/`, via ssh; detached if >5 min)

Build `ab-aat-to-parser-ir` at the candidate commit
(`cargo build --package ab-aat-to-parser-ir --release`), then run its
audit mode over the candidate dump's `aat/` tree (flags per `main.rs`; the
run prints `audited N AAT files: X succeeded, Y failed`). Capture the full
summary output.

- [ ] **Step 2: Add the registry row** (working directory: `abc/` on the
  branch — the monorepo branch covers both trees)

Copy the current accepted `aozora` entry in
`data/aat-parser-ir-compatibility.edn` as the template; change:

- `:aat_adapter "ab-aozora"`
- `:aat_adapter_version` = the candidate's verbatim `--version` line (the
  same string frozen in the three gate reports)
- `:evidence_scope` — `:adapter "ab-aozora"`, `:adapter_version` as above,
  `:evidence_type :conversion-audit`, `:corpus` naming the Phase 2 parity
  dump, and the fresh audit numbers from Step 1 (no carried-over counts)
- `:compatibility` = what the fresh audit supports (expected: identical to
  the `aozora` row's value, since the AAT is byte-identical modulo the two
  identity pointers — if the audit says otherwise, STOP: that contradicts
  the parity gate; investigate before writing anything)

Keep the mapping coordinates (`:mapping_id`, `:mapping_version`,
`:mapping_hash`, `:mapping_schema_hash`, `:parser_ir_schema_*`) verbatim
from the template row — Phase 2 changes no mapping.

- [ ] **Step 3: abc validation**

Run the abc test suites that own this file (the same ones ADR 0030's
acceptance criteria name):

```bash
clojure -M:test --focus abc.tools.parser-evidence-test 2>/dev/null || clojure -M:test
clojure -M:abc/adr-governance
```

(Use the repo's actual test alias — check `abc/deps.edn`; run the full abc
suite if focused selection isn't available.) Expected: green; the
admission-report test still proves admitted/missing/conflict behavior
(selection-not-admission boundary intact).

- [ ] **Step 4: Commit**

```bash
git add data/aat-parser-ir-compatibility.edn <audit-artifact-path>
git commit -m "feat(registry): unadmitted ab-aozora conversion-audit row (Phase 2 identity rotation)"
```

---

## Final verification (before the branch-finishing flow)

From `ab-validator/`:

```bash
export RUSTC_WRAPPER= SCCACHE_DISABLE=1
cargo test --workspace && cargo clippy --workspace --all-targets -- -D warnings && cargo deny check
tests/workspace-no-preserve-order.sh
cargo test --manifest-path tools/preserve-order-canary/Cargo.toml
tests/ab-aozora-lane-smoke.sh
python -m pytest reports/aat-fidelity/tests reports/lib/tests -q
nix build .#ab-aozora --no-link
```

From `abc/`: the abc suite + `clojure -M:abc/adr-governance`. All green,
then the final whole-branch review (subagent-driven-development) and
superpowers:finishing-a-development-branch.

Out-of-scope reminders for reviewers: `adapters/aozora` diff must be EMPTY;
no AAT schema change; no span-semantics change; no default-lane flip
(`--adapter aozora` remains the measurement default); legacy lane
(flake input `upstream-aozora-src`, `AB_AOZORA_BIN`, the adapter path)
still builds and runs.
