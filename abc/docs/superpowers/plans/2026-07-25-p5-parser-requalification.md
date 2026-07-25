# P5 Parser Re-Qualification (mainline) + Reproducibility Hardening — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Re-qualify the `ab-aozora` parser against current mainline and make the *qualified* binary and the *released* binary one and the same reproducible artifact, so `checks.<system>.publication-build-real-wiring` goes green and **stays** green across unrelated `abc` commits.

**Architecture:** Reuse the existing parser-RQ campaign engine (the runbook's Recipe 2 pipeline: `candidate → seal-readiness → authorize → capture → compose → admit → evaluate → project → promote`) rather than grow new orchestration. First remove the two root causes that make re-qualification non-durable: `ab-aozora`'s binary identity is braided together with (a) a build-time git-rev env that differs between the release derivation and the qualification derivation, and (b) abc governance data (four policy/authority/schema files its parse path never reads) baked in via a cross-repo `include_bytes!`. Task 1 unifies the derivation (deterministic env); Task 2 extracts the capture module into its own `ab-aozora-capture` crate that the release binaries no longer link, so **the installed `ab-aozora` and `ab-aat-to-parser-ir` bytes are independent of the four abc classified-source governance files** (they still depend, as any binary does, on the Cargo lock graph, toolchain, features, and build flags — this plan does not claim otherwise). After the parser is one reproducible artifact whose identity no longer tracks those abc files, wrap the one un-runbooked step (the build-a/build-b provenance capture), run the campaign once against mainline to regenerate evidence, propagate the freshly-emitted identity tuple to every pinned site, record the governance decision, and turn the gate green.

**Tech Stack:** Nix flakes (root `flake.nix`, `ab-validator/flake.nix`); Rust (`ab-validator` crates `ab-aozora`, `ab-aozora-aat`); Clojure 1.12 + Kaocha (abc); Python campaign drivers (`abc/tools/parser_rq_campaign_*.py`, `ab-validator/reports/parser-ir/parser-rq-campaign-provenance.py`); EDN decision records (`abc/docs/adr/decisions.edn`); RFC 8785/JCS SHA-256 content identities.

## Background — why this plan exists

The sole-publication-producer migration (merged `f86c15ee`) added a fail-closed parser
authentication boundary (`abc.tools.parser-release-authority`,
`abc.tools.publication-release/verify-release-root!`). It works correctly and, in doing so,
exposed a pre-existing reproducibility drift: the parser the release flake builds today no
longer matches the accepted **P5** qualification hashes. Running
`nix build .#checks.x86_64-linux.publication-build-real-wiring --print-build-logs` is honestly
RED with two non-rights problems:

- `release-parser-build-hash-mismatch` — actual `sha256:ff31d3036ab6…`, expected
  `sha256:482728cad5bc663c0742ca9e8c6d6fa7031c1a84117d024921cd48628e2eb034`.
- `release-converter-build-hash-mismatch` — actual `sha256:8073c1dc520f…`, expected
  `sha256:a2656fc9404be9a8eb08e5ba16667db814c8a22ad45bdf684d78973befaf5936`.

Full record: `abc/docs/superpowers/reports/2026-07-24-publication-surface-disposition.md` §13.

### Root cause (investigated, file-line confirmed)

1. **Two divergent `ab-aozora` derivations.** The release build binds the *standalone*
   `packages.ab-aozora` (root `flake.nix:109` → `abValidatorPackages."ab-aozora"`), whose Nix
   derivation bakes `AB_AOZORA_GIT_REV = self.rev or "unknown"` (`ab-validator/flake.nix:1900-1908`).
   The qualification campaign captures the **`parser-rq-candidate`** bundle, which builds
   `-p ab-aozora` in one `cargo build` and does **not** set `AB_AOZORA_GIT_REV`
   (`ab-validator/flake.nix:1072-1098`), so it bakes `"unknown"`. `AB_AOZORA_GIT_REV` is read
   through a build script (`ab-validator/crates/ab-aozora-aat/build.rs:11-13`) and embedded via
   `env!` at `ab-validator/crates/ab-aozora-aat/src/lib.rs:1992`. **Different env → different
   bytes → the captured hash can never equal the released hash.**
2. **Unpinned cross-repo `abc` tree baked into the binary.**
   `ab-validator/crates/ab-aozora-aat/src/classified_source.rs:20-27` embeds four `abc` files via
   `include_bytes!("../../../../abc/data/…json")` /
   `include_bytes!("../../../../abc/schemas/…json")`. Those bytes are staged at build time by
   `stageParserRqAbcAuthorities` (`ab-validator/flake.nix:139-167`) from the `abc` flake input,
   which is declared `url = "path:../abc"` and locked with **no narHash**
   (`ab-validator/flake.lock` `abc` node = `{"path":"../abc","type":"path"}`). So `ab-aozora`'s
   bytes are a direct function of the *live* abc tree; any abc edit silently re-drifts the parser
   hash. `ab-aat-to-parser-ir` embeds none of these files — which is exactly why §13 observed it
   reproduces while `ab-aozora` does not.

The parser source itself also legitimately moved: P5 was captured at `parser_git_rev`
`e6bf18518eec…` (`abc/docs/reports/parser-rq/runs/15affdfb…/candidate.edn`), current HEAD is
`f86c15ee`. So this is a real re-capture — `qualification_identity_ref` changes — not a hash
reshuffle.

### Design decision (RATIFIED 2026-07-25)

Make the release-bound and qualification-captured `ab-aozora` **byte-identical and reproducible**
by removing both sources of divergence, rather than "freeze a binary blob". The decomplection path
(D2 below) is ratified; the narHash-pin fallback is recorded for context only and must **not** be
executed. Concretely:

- **D1 — Deterministic, stable parser identity env.** Stop embedding a floating
  `self.rev` into the *release* parser binary. Set `AB_AOZORA_GIT_REV = "unknown"` in the
  standalone `abAozora` derivation so it matches the `parser-rq-candidate` build byte-for-byte.
  Rationale: a release identity that changes on every commit is not an identity — it is a place
  (Hickey: value vs place). The parser is authenticated by binary + mapping + schema, never by a
  mutable git rev.
- **D2 — Extract the capture module so the release parser stops linking it.** `ab-aozora`'s
  `main.rs` has three entry points (`--mode aat` → `aat_json_from_bytes`, `--mode diagnostics` →
  `diagnostics_json_from_bytes`, `--version`) and touches `classified_source` **zero** times. The
  four `abc` files it bakes in via cross-repo `include_bytes!` (`classified_source.rs:20-27`) are
  used only by the qualification *capture* functions the **rq adapters** run — they are dragged into
  the parser binary purely because `classified_source` lives in the `ab-aozora-aat` crate that
  `ab-aozora` (and `ab-aat-to-parser-ir`) link for AAT conversion. So the release parser's identity
  (`sha256(ab-aozora)`) is a function of governance data its parse path never reads. **Fix: move the
  concern to its own crate.** Extract `classified_source` (with its `include_bytes!`) verbatim into a
  new `ab-aozora-capture` crate that only the rq adapters
  (`ab-parser-rq-source-accountability`, `-diagnostic-authorization`) depend on. `ab-aozora` and
  `ab-aat-to-parser-ir` keep depending on `ab-aozora-aat` for conversion but no longer link capture,
  so **the installed `ab-aozora` and `ab-aat-to-parser-ir` bytes are independent of the four abc
  classified-source files** — the exact release identity `publication-build-real-wiring` gates on.
  (Precise invariant: the parser bytes still depend on toolchain/lock graph/flags like any binary —
  the claim is *independence from the four abc files*, not from all inputs.)

  This is a **behavior-preserving structural move** (Hickey/simplification rung 6, split a genuinely
  separate concern), not a logic change: the module, its `include_bytes!` paths, and its golden test
  move verbatim; no function body, ledger shape, or output byte changes. The existing byte-identity
  golden (`classified_source_capture.rs:302`) is the characterization anchor. It **replaces** the
  narHash-pin idea entirely, and is *simpler* than the earlier "runtime-parameter" sketch because it
  invents no runtime authority-loading protocol and no new hash channel — the four files stay baked
  where they are actually used (the rq adapters, which are not release-gated). Their identity keeps
  flowing exactly as today; only the *release binaries* are freed of them.

- **Tactical fallback (F1) — NOT chosen, do not execute.** For the record: a narHash pin on the
  `abc` flake input would make the drift deterministic (parser identity still fused to abc, no
  longer *floating*) and turn the gate green with one lock edit — but it leaves the
  identity-accumulation in place, so every abc governance edit still forces a re-qualification. It
  was considered and rejected in favor of D2. Kept here only so a future reader knows the tradeoff
  was weighed, not missed.

Every downstream task depends on D1/D2 as ratified above. If a genuinely different end-state is ever
proposed (e.g. binding the release to the `parser-rq-candidate` bundle output rather than the
standalone parser), stop and re-scope Tasks 1–2 before writing code.

---

## Global Constraints

- **Governance evidence is append-only and content-addressed.** Never edit an existing
  observation/measurement/provenance file under `abc/docs/reports/parser-rq/runs/**`; a new
  qualification is a new `runs/<new-candidate-ref>/` directory, never a mutation of the P5 run
  (ADR 0040 c3, ADR 0041 c1; runbook §Recipe 2).
- **The gate never edits decisions.** `verify-promotion` and the flake checks read
  `docs/adr/decisions.edn`; the qualification decision is recorded by hand through the governance
  workflow, never written by tooling (runbook step 9).
- **Parser identity = binary + mapping + schema, not invocation argv and not git rev.** Do not
  reintroduce argv or `self.rev` into any authentication comparison (established at
  `abc/src/abc/tools/parser_release_authority.clj`; commit `837b107a`).
- **Reproducibility is a hard gate, by design.** `parser_rq_campaign_orchestrator.py:273` and
  `parser-rq-campaign-provenance.py:447` refuse any provenance whose `status != "reproducible"`.
  Do not weaken these; make the build reproducible instead.
- **The release and qualification binaries MUST be the same bytes.** Success = the `ab-aozora`
  sha256 the release binds (root `flake.nix:109`) equals the `ab-aozora` sha256 recorded in the
  new run's `executable-provenance.json`. If they differ, the plan is not done regardless of any
  green sub-check.
- **Host requirement for capture:** Linux with cgroup v2 (predicate 8 measures process-tree
  memory via cgroups), Nix with flakes, a writable runtime root **outside** the repository.
  Committed manifests never contain runtime locators (ADR 0042).
- **The identity tuple is emitted by the campaign, not chosen.** New `candidate_ref`,
  `qualification_identity_ref`, `ab-aozora` sha256, and `ab-aat-to-parser-ir` sha256 are OUTPUTS
  of the run. Where a task says "the new `<X>`", it means the exact value read from the generated
  evidence file named in that task — never a hand-invented hash.
- **`nix-format` / `clj-kondo` / `cljfmt` must stay green.** Run `nix fmt` on any edited `.nix`
  file; the abc `clj-kondo` check runs `cljfmt check src test`.

### The identity tuple and every site that pins it

The campaign emits a new tuple; these are the exact propagation sites (verified by content
search over the tree). "→ new" = replace with the freshly-emitted value; "regenerated" = the file
is re-emitted wholesale by the campaign, not hand-edited.

| Pinned value (P5, to be replaced) | Sites |
| --- | --- |
| `candidate_ref` `sha256:15affdfb677cc6a9…eddc5ab` | `abc/config/full-corpus-publication-custom-parser-ja.json`; `abc/flake.nix` (`parser-rq-p5-promotion-audit`, ~line 502); `tests/publication-build-real-wiring-smoke.sh`; `abc/test/abc/tools/parser_release_authority_test.clj:16`; `abc/test/abc/tools/soranoha_build_publication_test.clj:90`; `abc/test/abc/tools/parser_rq_resource_test.clj`; `abc/docs/parser-rq-runbook.md:20` |
| `qualification_identity_ref` `sha256:6f365a44b975…bc79edca` | `abc/test/abc/tools/parser_release_authority_test.clj:19`; `abc/docs/adr/custom-parser-release-qualification.md` |
| `ab-aozora` exec `sha256:482728cad5bc663c…e2eb034` | `abc/test/abc/tools/parser_release_authority_test.clj:25`; `abc/test/abc/tools/soranoha_build_publication_test.clj:219` (`p5-parser-build-hash`) |
| `ab-aat-to-parser-ir` exec `sha256:a2656fc9404be9a8…faf5936` | `abc/test/abc/tools/soranoha_build_publication_test.clj:221` (`p5-converter-build-hash`) |
| `mapping_hash` `sha256:9be58ff3fea272c2…fe067142` | UNCHANGED if `data/aat-to-parser-ir-mapping-v2.json` is untouched (it is): `abc/test/abc/tools/parser_release_authority_test.clj:22`, `abc/test/abc/tools/soranoha_build_publication_test.clj`, `abc/data/aat-parser-ir-compatibility.edn`. Verify it is unchanged; do not edit if so. |

Regenerated wholesale by the campaign (do not hand-edit):
`abc/docs/reports/parser-rq/runs/<new-ref>/` (all of `candidate.edn`, `executable-provenance.json`,
`readiness-receipt.json`, `authorizations/`, `captures/`, `evaluations/`);
`abc/docs/reports/parser-release-qualification-measurements.edn`;
`abc/docs/reports/parser-release-qualification-report.json`;
appended row in `abc/data/aat-parser-ir-compatibility.edn`.

Leave the P5 hashes in these historical/frozen documents (they record what was true then):
`abc/docs/superpowers/reports/2026-07-24-publication-surface-disposition.md`,
`abc/docs/superpowers/plans/2026-07-24-sole-publication-producer.md`.

---

### Task 1: Deterministic parser identity env (D1) — one reproducible `ab-aozora`

**Files:**
- Modify: `ab-validator/flake.nix:1900-1908` (the `abAozora` derivation `env` block)
- Test: a temporary two-build byte-comparison (commands below; no committed test file)

**Interfaces:**
- Consumes: nothing from earlier tasks (first task).
- Produces: a standalone `packages.ab-aozora` whose `ab-aozora` binary is byte-identical to the
  `ab-aozora` binary inside `parser-rq-candidate` (given the same `abc` input). Later tasks rely
  on `nix build ./ab-validator#ab-aozora` and the `parser-rq-candidate` build producing the same
  `ab-aozora` sha256.

- [ ] **Step 1: Capture the current divergence (failing baseline)**

Run, from repo root:

```bash
nix build ./ab-validator#ab-aozora --no-link --print-out-paths | \
  xargs -I{} sha256sum {}/bin/ab-aozora
nix build ./ab-validator#parser-rq-candidate --no-link --print-out-paths | \
  xargs -I{} sha256sum {}/bin/ab-aozora
```

Expected NOW: the two `sha256sum` lines **differ** (standalone bakes `self.rev`, the bundle bakes
`"unknown"`). Record both hashes in the task report.

- [ ] **Step 2: Make the standalone derivation bake the same env as the qualification bundle**

In `ab-validator/flake.nix`, change the `abAozora` `env` so it no longer embeds a floating rev:

```nix
        abAozora = mkRustBin {
          pname = "ab-aozora";
          cargoBuildFlags = [
            "--package"
            "ab-aozora"
          ];
          # Parser release identity is authenticated by binary + mapping + schema,
          # never by a mutable git rev. Baking `self.rev` makes the release binary
          # hash change every commit and diverge from the qualification build (which
          # leaves AB_AOZORA_GIT_REV unset -> "unknown"). Pin it to "unknown" so the
          # released and qualified `ab-aozora` are one reproducible artifact.
          env = {
            AB_AOZORA_GIT_REV = "unknown";
          };
          extra.preBuild = stageParserRqAbcAuthorities;
        };
```

- [ ] **Step 3: Format the flake**

Run: `nix fmt ab-validator/flake.nix`
Expected: no diff other than your edit; `git diff --stat` shows only `ab-validator/flake.nix`.

- [ ] **Step 4: Verify the two builds now agree**

Run the same two `nix build … | xargs sha256sum` commands from Step 1.
Expected: the two `ab-aozora` sha256 values are now **identical**. (They are still the *drifted*
mainline hash, not the P5 hash — that is expected; P5 is superseded by this plan.)

- [ ] **Step 5: Verify build-a/build-b reproducibility of the standalone artifact**

Run:

```bash
p1=$(nix build ./ab-validator#ab-aozora --no-link --print-out-paths --rebuild)
p2=$(nix build ./ab-validator#ab-aozora --no-link --print-out-paths --rebuild)
sha256sum "$p1/bin/ab-aozora" "$p2/bin/ab-aozora"
```

Expected: both hashes identical (within one abc checkout — durability across abc edits is Task 2).
If they differ here, STOP: there is a third non-determinism source (see disposition §13 / the
Rust-crate investigation); escalate rather than proceeding.

- [ ] **Step 6: Commit**

```bash
git add ab-validator/flake.nix
git commit -m "fix(ab-validator): pin ab-aozora release identity env to match qualification build

The standalone ab-aozora derivation baked AB_AOZORA_GIT_REV=self.rev while
the parser-rq-candidate qualification build left it unset (\"unknown\"),
producing two different binaries — the release could never match the
qualified hash. Pin both to \"unknown\" so released and qualified ab-aozora
are one reproducible artifact.

🤖 Generated with [Claude Code](https://claude.com/claude-code)"
```

---

### Task 2: Extract the capture module into its own crate so the parser stops linking it (D2)

**Goal of this task:** the release-gated binaries `ab-aozora` and `ab-aat-to-parser-ir` must stop
linking the `classified_source`/capture module (and therefore stop embedding the four abc files via
its `include_bytes!`), while the rq adapters that actually use capture
(`ab-parser-rq-source-accountability`, and `ab-parser-rq-diagnostic-authorization` in tests) keep
identical behavior. This is a **behavior-preserving structural move**, not a logic change: no
function body, `include_bytes!` path, ledger shape, or output byte changes — only *which crate* the
module lives in and *who depends on it*.

**Ground truth (verified):** `classified_source.rs` (`ab-validator/crates/ab-aozora-aat/src/`,
re-exported at `lib.rs:15-19`) is the only place the four consts (`classified_source.rs:20-27`) are
read. The only production caller is `ab-parser-rq-source-accountability`
(`src/main.rs:298` → `capture_generation_from_bytes_for_identity_and_work` + `publish`;
`src/recognition.rs`). `ab-aozora` and `ab-aat-to-parser-ir` depend on `ab-aozora-aat`
(`Cargo.toml:13` / `:30`) for AAT conversion but **never call** any capture function — they only
inherit the embed by linking the crate. `ab-parser-rq-diagnostic-authorization` uses capture only in
`tests/fixture_capture.rs`. `classified_source.rs` *consumes* `lex`/`reconcile_accent_edit_facts`
from `ab-aozora-aat` (e.g. line 992), so the new crate depends on `ab-aozora-aat` (one direction, no
cycle). `ab-check` already does not depend on `ab-aozora-aat` and is unaffected.

**Files:**
- Create: `ab-validator/crates/ab-aozora-capture/Cargo.toml`, `…/src/lib.rs`
- Move (verbatim): `ab-validator/crates/ab-aozora-aat/src/classified_source.rs` →
  `ab-validator/crates/ab-aozora-capture/src/classified_source.rs` (its `include_bytes!("../../../../abc/…")`
  paths resolve identically — the new crate sits at the same `crates/<name>/src` depth)
- Move (verbatim): the golden test `ab-validator/crates/ab-aozora-aat/tests/classified_source_capture.rs`
  → `ab-validator/crates/ab-aozora-capture/tests/classified_source_capture.rs`
- Modify: `ab-validator/crates/ab-aozora-aat/src/lib.rs` (remove `mod classified_source;` and its
  `pub use classified_source::{…}` block at lines 13-19)
- Modify: `ab-validator/crates/ab-parser-rq-source-accountability/Cargo.toml` +
  `src/main.rs`/`src/recognition.rs` imports → depend on and import from `ab-aozora-capture`
- Modify: `ab-validator/crates/ab-parser-rq-diagnostic-authorization/Cargo.toml` +
  `tests/fixture_capture.rs` imports → `ab-aozora-capture`
- Modify: `ab-validator/flake.nix` — add `ab-aozora-capture` to the workspace build where the crate
  set is enumerated (e.g. `parserRqCandidate`'s `cargoBuildFlags` if it must be built explicitly);
  `stageParserRqAbcAuthorities` is still required (the new crate + source-accountability's own
  `recognition.rs`/`recognition_corpus.rs` embeds still resolve `../abc/…`)
- Do **not** touch: the parallel embeds in
  `ab-parser-rq-source-accountability/src/{recognition.rs:9-11,recognition_corpus.rs:17}` — those
  belong to an rq adapter that legitimately embeds these files and is **not** release-gated; leaving
  them is correct. (They do mean `candidate_ref` still moves if these four files are *deliberately*
  edited — an acceptable re-qualification trigger, not the unrelated-abc-edit drift this task kills.)

> **Scope note (precise invariant).** This task makes the **release-gated** `ab-aozora` and
> `ab-aat-to-parser-ir` bytes independent of the four abc classified-source files. It does *not*
> claim the whole campaign is independent of them — `source-accountability` still embeds them
> because it uses them. That is intended: the release identity (what `publication-build-real-wiring`
> gates on) is decoupled; the qualification authority stays where it is used.

- [ ] **Step 1: PROTECT — run the existing golden characterization (green baseline)**

The behavior anchor already exists: `production_fixture_regenerates_byte_identically`
(`ab-validator/crates/ab-aozora-aat/tests/classified_source_capture.rs:302`) byte-compares the
capture generation against goldens at
`abc/test/fixtures/parser-rq/classified-source-capture/{source.txt,decoded.txt,parser-output.json,raw-diagnostics.json,ledger.json,generation.json}`.
Plus the caller tests: `ab-parser-rq-source-accountability/tests/{recognition.rs,recognition_corpus.rs,recognition_fixture.rs,recognition_properties.rs}`
and `ab-parser-rq-diagnostic-authorization/tests/fixture_capture.rs`.

Run and record all green **before** moving anything:

```bash
cd ab-validator
cargo test -p ab-aozora-aat --test classified_source_capture
cargo test -p ab-parser-rq-source-accountability
cargo test -p ab-parser-rq-diagnostic-authorization
```

Expected: PASS. These exact commands (retargeted to the new crate for the first) must stay PASS
after the move — that is the behavior-preservation proof.

- [ ] **Step 2: Record the coupling baseline (no worktree mutation — finding 5)**

Do **not** mutate any tracked file. Just record which release binaries embed the policy string now:

```bash
cd "$(git rev-parse --show-toplevel)"
for pkg in ab-aozora ab-aat-to-parser-ir; do
  p=$(nix build ./ab-validator#$pkg --no-link --print-out-paths)
  bin=$(find "$p/bin" -type f | head -1)
  if grep -aqF "parser-rq-ab-aozora-classified-source-v1" "$bin"; then
    echo "BASELINE: $pkg embeds the policy string (expected NOW)"
  else echo "BASELINE: $pkg already clean"; fi
done
```

Expected NOW: at least `ab-aozora` reports "embeds" (the coupling this task removes). After the
move, Step 5 asserts both are clean. (No `git checkout`/tracked-file edits anywhere in this task.)

- [ ] **Step 3: TRANSFORM — create `ab-aozora-capture`, move the module + golden test**

Create `ab-validator/crates/ab-aozora-capture/Cargo.toml` mirroring `ab-aozora-aat`'s edition and
the dependencies `classified_source.rs` needs (at minimum `ab-aozora-aat`, plus `serde_json`,
`sha2`, and whatever `classified_source.rs`'s `use` block imports — copy them from `ab-aozora-aat`'s
Cargo.toml). `git mv` `classified_source.rs` into `…/ab-aozora-capture/src/`, add
`pub mod classified_source; pub use classified_source::{…the same items lib.rs used to re-export…};`
to the new `src/lib.rs`. `git mv` the golden test into `…/ab-aozora-capture/tests/`. Remove
`mod classified_source;` and the `pub use classified_source::{…}` block from
`ab-aozora-aat/src/lib.rs`.

- [ ] **Step 4: Repoint the two rq-adapter callers**

Add `ab-aozora-capture` to the `[dependencies]` of
`ab-parser-rq-source-accountability/Cargo.toml` and
`ab-parser-rq-diagnostic-authorization/Cargo.toml`, and change their `use ab_aozora_aat::{…capture items…}`
imports to `use ab_aozora_capture::{…}`. Add the crate to `ab-validator/flake.nix` where the crate
set is built. Confirm the whole workspace compiles:

```bash
cd ab-validator && cargo build --workspace
```

Expected: builds. If `ab-aozora-aat` fails to compile because something *inside it* (outside the
moved file) referenced `classified_source`, STOP and report — the dependency direction was not as
mapped; do not paper over it.

- [ ] **Step 5: VERIFY — goldens identical, release binaries decoupled**

```bash
cd ab-validator
cargo test -p ab-aozora-capture --test classified_source_capture    # goldens byte-identical
cargo test -p ab-parser-rq-source-accountability
cargo test -p ab-parser-rq-diagnostic-authorization
```

Expected: PASS (same goldens, moved). Then confirm both release binaries are clean:

```bash
cd "$(git rev-parse --show-toplevel)"
fail=0
for pkg in ab-aozora ab-aat-to-parser-ir; do
  p=$(nix build ./ab-validator#$pkg --no-link --print-out-paths)
  bin=$(find "$p/bin" -type f | head -1)
  if grep -aqF "parser-rq-ab-aozora-classified-source-v1" "$bin"; then
    echo "FAIL: $pkg still embeds abc policy bytes" >&2; fail=1
  else echo "ok: $pkg no longer embeds abc policy bytes"; fi
done
test "$fail" = 0
nix build ./ab-validator#parser-rq-candidate --no-link   # bundle still builds
```

Expected: both `ok:`; the bundle builds. This is the decoupling proof.

- [ ] **Step 6: Commit**

```bash
git add ab-validator/crates ab-validator/flake.nix
git commit -m "refactor(ab-validator): extract ab-aozora-capture crate so parser stops linking it

The classified_source/capture module (which include_bytes! four abc governance
files) lived in ab-aozora-aat, so ab-aozora and ab-aat-to-parser-ir linked it
and embedded those files despite never calling capture — fusing the RELEASE
parser identity to the abc tree and re-drifting it on unrelated abc edits. Move
the module (and its golden test) verbatim into a new ab-aozora-capture crate
depended on only by the rq adapters that actually run capture. Behavior-
preserving (goldens byte-identical); the release binaries now embed no abc bytes.

🤖 Generated with [Claude Code](https://claude.com/claude-code)"
```

---

### Task 3: Wrap the reproducible-build-proof step; make candidate+bind explicit in Recipe 2

**Files:**
- Create: `abc/bin/parser-rq-build-proof.sh` (driver — produces an **unbound** reproducibility proof)
- Modify: `abc/docs/parser-rq-runbook.md` (insert Recipe 2 "Step 0: reproducible build proof", and
  split the currently-implicit candidate/bind into explicit numbered steps)
- Test: `abc/tools/test_parser_rq_build_proof.py` (protocol-shape test — see Step 1; not just help text)

**The real protocol (verified against the CLI and `abc.tools.parser-rq-campaign`).** The provenance
tool `ab-validator/reports/parser-ir/parser-rq-campaign-provenance.py` exposes these exact
subcommands (confirmed via its `main()` dispatch, lines 750-800): `realize-build`, `capture-build`,
`compare-builds`, `bind-provenance`, `resolve-executable`, `verify-installed`, `verify-evidence`.
There is a deliberate **two-phase split of authority**:

1. **Python owns the reproducible build proof.** `realize-build` (×2, `build-a`/`build-b`) →
   `capture-build` (×2, from each realization + the graph + parser-git-rev) → `compare-builds`
   emits an **unbound proof** (`status: reproducible`, carrying `builds` + `executables` with each
   `sha256`/`nar_hash`, but **no** `candidate_ref`/`qualification_identity_ref`).
2. **Clojure owns candidate identity.** `clojure -M:abc/parser-rq-campaign candidate` consumes the
   *unbound proof* (it reads only `:status`/`:builds`/`:executables` + the parser's `parser_git_rev`
   — `build-candidate` in `abc/src/abc/tools/parser_rq_campaign.clj:178`) and derives `candidate.edn`,
   printing `candidate_ref` and carrying `qualification_identity_ref`.
3. **Python then binds.** `bind-provenance --proof <unbound> --candidate-ref <..>
   --qualification-identity-ref <..>` stamps those two derived refs into the final bound
   `executable-provenance.json`.

`resolve-executable --provenance … --name <bin>` returns an **executable filesystem path** — it does
**not** and cannot yield `candidate_ref`/`qualification_identity_ref`. The earlier draft of this task
had that backwards and used non-existent subcommand names; this rewrite corrects both. The wrapper
therefore stops at the **unbound proof** (phase 1); the candidate and bind steps are explicit,
separately-owned Recipe 2 steps — this is the honest boundary (Python = build proof, Clojure =
identity, Python = bind), not a single Python black box that pretends to own identity.

- [ ] **Step 1: Write the failing protocol-shape test**

`abc/tools/test_parser_rq_build_proof.py` — asserts the wrapper exists, documents the real flags,
and (the part that actually matters) that it invokes the real subcommand names in order and does
**not** attempt to bind:

```python
import stat
import subprocess
from pathlib import Path

WRAPPER = Path(__file__).resolve().parents[1] / "bin" / "parser-rq-build-proof.sh"


def test_wrapper_exists_and_is_executable():
    assert WRAPPER.is_file(), "build-proof wrapper missing"
    assert WRAPPER.stat().st_mode & stat.S_IXUSR, "wrapper not executable"


def test_wrapper_documents_real_flags():
    text = subprocess.run([str(WRAPPER), "--help"], capture_output=True, text=True)
    out = text.stdout + text.stderr
    for flag in ("--candidate-tree", "--graph", "--parser-git-rev", "--store-root", "--out"):
        assert flag in out, f"wrapper help must document {flag}"


def test_wrapper_uses_real_subcommands_and_stops_at_unbound_proof():
    body = WRAPPER.read_text()
    for sub in ("realize-build", "capture-build", "compare-builds"):
        assert sub in body, f"wrapper must call {sub}"
    # phase-1 wrapper owns build proof only; identity + bind are Recipe 2 steps.
    assert "bind-provenance" not in body, "wrapper must NOT bind (that follows candidate derivation)"
    assert "resolve-executable" not in body, "resolve-executable returns a path, not candidate_ref"
```

- [ ] **Step 2: Run it to verify it fails**

Run: `cd abc && python3 -m pytest tools/test_parser_rq_build_proof.py -q`
Expected: FAIL (`build-proof wrapper missing`).

- [ ] **Step 3: Write the wrapper (phase 1 only — unbound proof)**

`abc/bin/parser-rq-build-proof.sh`:

```bash
#!/usr/bin/env bash
# Phase 1 of provenance capture: build the parser twice (build-a, build-b),
# capture each, and prove they reproduce -> an UNBOUND reproducibility proof.
# Candidate identity (Clojure) and bind-provenance (Python) are explicit,
# separately-owned Recipe 2 steps AFTER this — see docs/parser-rq-runbook.md.
# Thin wrapper over ab-validator/reports/parser-ir/parser-rq-campaign-provenance.py;
# adds no orchestration logic, only chains realize-build x2 -> capture-build x2
# -> compare-builds.
set -euo pipefail

repo_root=$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)
prov="$repo_root/ab-validator/reports/parser-ir/parser-rq-campaign-provenance.py"

usage() {
  cat >&2 <<'EOF'
usage: parser-rq-build-proof.sh \
  --candidate-tree PATH   parser source tree to build (must be clean; see Task 5) \
  --graph PATH            data/parser-rq-production-graph-v1.json \
  --parser-git-rev REV    monorepo rev the candidate-tree is checked out at \
  --store-root PATH       writable scratch store root (outside the repo) \
  --out PATH              destination unbound reproducibility proof (proof.json)
Emits an UNBOUND proof. Derive the candidate (clojure -M:abc/parser-rq-campaign
candidate --provenance <proof>) and then bind-provenance separately.
EOF
}

candidate_tree="" graph="" parser_git_rev="" store_root="" out=""
while [ $# -gt 0 ]; do
  case "$1" in
    --candidate-tree) candidate_tree="$2"; shift 2 ;;
    --graph) graph="$2"; shift 2 ;;
    --parser-git-rev) parser_git_rev="$2"; shift 2 ;;
    --store-root) store_root="$2"; shift 2 ;;
    --out) out="$2"; shift 2 ;;
    --help|-h) usage; exit 0 ;;
    *) echo "unknown arg: $1" >&2; usage; exit 2 ;;
  esac
done
for req in candidate_tree graph parser_git_rev store_root out; do
  if [ -z "${!req}" ]; then echo "missing --${req//_/-}" >&2; usage; exit 2; fi
done

work=$(mktemp -d)
for bid in build-a build-b; do
  python3 "$prov" realize-build \
    --candidate-tree "$candidate_tree" \
    --build-id "$bid" \
    --store-root "$store_root/$bid" \
    --build-log "$work/$bid.log" \
    --out "$work/$bid.realization.json"
  python3 "$prov" capture-build \
    --realization "$work/$bid.realization.json" \
    --graph "$graph" \
    --parser-git-rev "$parser_git_rev" \
    --out "$work/$bid.build.json"
done
python3 "$prov" compare-builds \
  --first "$work/build-a.build.json" --second "$work/build-b.build.json" \
  --out "$out"
echo "wrote unbound reproducibility proof -> $out" >&2
echo "next: clojure -M:abc/parser-rq-campaign candidate --provenance $out …  then bind-provenance" >&2
```

Then `chmod +x abc/bin/parser-rq-build-proof.sh`.

> **Implementer note:** confirm `realize-build`/`capture-build`/`compare-builds` flag names against
> `parser-rq-campaign-provenance.py`'s `_parser()` (the subparsers are named at lines 706/712/717).
> `compare-builds` raises `ProvenanceUnavailable` (nonzero exit) unless `status == reproducible`, so
> `set -euo pipefail` makes non-reproducibility a hard failure here — correct. The wrapper must not
> reference `bind-provenance`/`resolve-executable` (the Step-1 test enforces this).

- [ ] **Step 4: Run the protocol-shape test to verify it passes**

Run: `cd abc && python3 -m pytest tools/test_parser_rq_build_proof.py -q`
Expected: PASS (3 passed).

- [ ] **Step 5: Make candidate + bind explicit in the runbook**

In `abc/docs/parser-rq-runbook.md`, replace Recipe 2's implicit "assume `executable-provenance.json`
exists" with three explicit steps:

- **Step 0 — reproducible build proof:** one `parser-rq-build-proof.sh` invocation →
  `$work/proof.json` (unbound).
- **Step 0b — derive candidate:**
  `clojure -M:abc/parser-rq-campaign candidate --repo "$(git rev-parse --show-toplevel)"
  --parser-git-rev "$parser_rev" --provenance "$work/proof.json" --out "$work/candidate.edn"`
  (prints `candidate_ref`; `qualification_identity_ref` is inside `candidate.edn`).
- **Step 0c — bind provenance:**
  `python3 …/parser-rq-campaign-provenance.py bind-provenance --proof "$work/proof.json"
  --candidate-ref "$candidate_ref" --qualification-identity-ref "$qual_ref"
  --out "$work/executable-provenance.json"`.

Then the existing Recipe 2 step that consumes `$work/executable-provenance.json` follows unchanged.
Do not alter Recipe 1.

- [ ] **Step 6: Commit**

```bash
git add abc/bin/parser-rq-build-proof.sh abc/tools/test_parser_rq_build_proof.py abc/docs/parser-rq-runbook.md
git commit -m "feat(abc): wrap reproducible build-proof; make candidate+bind explicit in Recipe 2

Phase-1 wrapper owns the unbound reproducibility proof (realize-build x2 ->
capture-build x2 -> compare-builds); candidate identity (Clojure) and
bind-provenance (Python) are explicit, separately-owned runbook steps. Closes
the un-runbooked provenance gap without pretending Python owns identity.

🤖 Generated with [Claude Code](https://claude.com/claude-code)"
```

---

### Task 4: CI byte-equality guard (release-bound == qualification-captured `ab-aozora`)

**Files:**
- Modify: `ab-validator/flake.nix` (add a `checks.<system>.ab-aozora-release-qualification-byte-equality`
  check; and, if Task 2 made it unnecessary, drop `stageParserRqAbcAuthorities` from `abAozora`)
- Test: the check itself is the test (`nix build`)

**What this check does and does NOT prove.** It builds the standalone `packages.ab-aozora` and the
`parser-rq-candidate` bundle in the **same** Nix evaluation and asserts their `ab-aozora` bytes are
identical — i.e. **release↔qualification byte-equality by construction**. It does **not** prove
independent-store build reproducibility; that property is established separately by Task 5's
`realize-build`/`compare-builds` two-store proof. The name reflects the property actually checked;
do not call it "reproducible".

**Interfaces:**
- Consumes: Tasks 1–2's unified `ab-aozora` (D1 deterministic env + D2 decomplection).
- Produces: a standing flake check that fails if the standalone and bundle `ab-aozora` bytes ever
  diverge again — the regression net that keeps D1/D2 from silently rotting.

- [ ] **Step 1: If Task 2 made it dead, remove `stageParserRqAbcAuthorities` from `abAozora`**

After Task 2, the standalone `abAozora` no longer needs the abc governance files staged into its
build tree (the parser stops embedding them). Confirm and remove the `extra.preBuild =
stageParserRqAbcAuthorities;` line from the `abAozora` derivation **iff** `nix build
./ab-validator#ab-aozora` still succeeds and the byte-equality check (Step 2) stays green without
it. If some other part of `abAozora`'s build still needs the staging, leave it and note why in the
task report — do not force removal.

- [ ] **Step 2: Add the byte-equality check**

In `ab-validator/flake.nix`, in the `checks.<system>` set, add:

```nix
          ab-aozora-release-qualification-byte-equality =
            pkgs.runCommand "ab-aozora-release-qualification-byte-equality"
              { nativeBuildInputs = [ pkgs.coreutils ]; }
              ''
                # Release binds the standalone ab-aozora; qualification captures the
                # ab-aozora inside parser-rq-candidate. They MUST be byte-identical,
                # else the release can never match the qualified hash. (Byte-equality
                # by construction; independent-store reproducibility is Task 5's proof.)
                a=$(sha256sum ${abAozora}/bin/ab-aozora | cut -d' ' -f1)
                b=$(sha256sum ${parserRqCandidate}/bin/ab-aozora | cut -d' ' -f1)
                if [ "$a" != "$b" ]; then
                  echo "ab-aozora release/qualification bytes diverged:" >&2
                  echo "  standalone (release-bound): $a" >&2
                  echo "  parser-rq-candidate (qualified): $b" >&2
                  exit 1
                fi
                mkdir -p "$out"
                echo "release-bound ab-aozora == qualified ab-aozora ($a)" > "$out/result.txt"
              '';
```

- [ ] **Step 3: Format and build the check**

```bash
nix fmt ab-validator/flake.nix
nix build ./ab-validator#checks.x86_64-linux.ab-aozora-release-qualification-byte-equality --print-build-logs
```

Expected: GREEN — one shared hash recorded. (Before Task 1 this would have been RED; that ordering
is why it lands now.)

- [ ] **Step 4: Commit**

```bash
git add ab-validator/flake.nix
git commit -m "test(ab-validator): guard release-bound == qualification-captured ab-aozora bytes

🤖 Generated with [Claude Code](https://claude.com/claude-code)"
```

---

### Task 5: Run the campaign against mainline — regenerate qualification evidence (HOST-ONLY)

**Files:**
- Create: `abc/docs/reports/parser-rq/runs/<new-candidate-ref>/` (whole tree, emitted by tooling)
- Modify: `abc/docs/reports/parser-release-qualification-measurements.edn` (regenerated)
- Modify: `abc/docs/reports/parser-release-qualification-report.json` (regenerated)
- Modify: `abc/data/aat-parser-ir-compatibility.edn` (one appended admitted row)

**Interfaces:**
- Consumes: Tasks 1–4 committed (the unified, decomplected, byte-equal parser + the phase-1
  build-proof wrapper `abc/bin/parser-rq-build-proof.sh`); the runbook Recipe 2 pipeline and its
  drivers (`abc/tools/parser_rq_campaign_site.py`, `bin/parser-rq-campaign-capture.sh`,
  `clojure -M:abc/parser-rq-campaign …`, `clojure -M:abc/aat-compat-admission`); the fixed graph
  `abc/data/parser-rq-production-graph-v1.json`; the site template
  `abc/config/parser-rq-site.example.json`.
- Produces: the **new identity tuple** — `candidate_ref`, `qualification_identity_ref`, the
  `ab-aozora` and `ab-aat-to-parser-ir` executable sha256 — recorded in
  `runs/<new-ref>/executable-provenance.json` and `candidate.edn`, plus the regenerated
  measurements/report and the admitted registry row. Task 6 reads these exact values.

> **This task cannot run in CI or a sandbox.** It requires a cgroup-v2 Linux host, `nix develop`,
> and a writable runtime root outside the repo (Global Constraints). Its deliverable is *committed
> evidence*, not code. A subagent without such a host must report BLOCKED with the precise command
> that could not run; **never fabricate a hash**. `parser_rev` must be the merge commit that
> already contains Tasks 1–4 (the fix must be *in* the binary we qualify).

- [ ] **Step 1: Preconditions + a FROZEN, clean candidate build tree (finding 7)**

The binary we qualify must be built from an immutable tree whose revision the evidence will claim.
Build from a detached, clean worktree — never the live checkout — and keep every evidence/registry
mutation in the main checkout so it can never dirty the tree being hashed:

```bash
uname -a && stat -fc %T /sys/fs/cgroup            # expect cgroup2fs
parser_rev=$(git rev-parse HEAD)                  # must include Tasks 1-4
cand_tree=$(mktemp -d)/candidate                  # OUTSIDE the repo
git worktree add --detach "$cand_tree" "$parser_rev"
# Freeze assertions — the tree we build MUST be exactly parser_rev and clean:
test "$(git -C "$cand_tree" rev-parse HEAD)" = "$parser_rev"
test -z "$(git -C "$cand_tree" status --porcelain)"
work=$(mktemp -d)                                 # campaign scratch (values, not the build tree)
```

- [ ] **Step 2: Phase-1 reproducible build proof (unbound) from the frozen tree**

```bash
abc/bin/parser-rq-build-proof.sh \
  --candidate-tree "$cand_tree" \
  --graph abc/data/parser-rq-production-graph-v1.json \
  --parser-git-rev "$parser_rev" \
  --store-root "$RUNTIME_ROOT/store" \
  --out "$work/proof.json"
```

Expected: `wrote unbound reproducibility proof -> …`; `jq -r .status "$work/proof.json"` is
`reproducible`. If not, STOP — Tasks 1–2 did not fully land; do not force past the gate.

- [ ] **Step 2b: Derive the candidate (Clojure owns identity), then bind (Python)**

```bash
candidate_ref=$(clojure -M:abc/parser-rq-campaign candidate \
  --repo "$cand_tree" --parser-git-rev "$parser_rev" \
  --provenance "$work/proof.json" --out "$work/candidate.edn")
qual_ref=$(clojure -M:abc/parser-rq-campaign qualification-identity-ref --candidate "$work/candidate.edn")
python3 ab-validator/reports/parser-ir/parser-rq-campaign-provenance.py bind-provenance \
  --proof "$work/proof.json" --candidate-ref "$candidate_ref" \
  --qualification-identity-ref "$qual_ref" --out "$work/executable-provenance.json"
```

Record `candidate_ref` and `qual_ref` in the task report.

- [ ] **Step 3: Readiness → authorize → capture → compose → admit → evaluate → project**

Execute runbook `docs/parser-rq-runbook.md` Recipe 2 steps 3–8 verbatim, using `$work`,
`$parser_rev`, and `$cand_tree` as the candidate tree. The registry admission
(`aat-compat-admission`, runbook step 6) and all evaluation writes land in the **main** checkout's
tracked files, not in `$cand_tree`. Record every printed ref (`authorization_ref`,
`evaluation_generation_ref`) in the task report.

- [ ] **Step 4: Promote (gate)**

Run runbook Recipe 2 step 9 (`verify-promotion` against the new candidate's coordinates). Expected:
`ok`, exit 0. Any other output lists exact verification failures — investigate, never edit evidence.

- [ ] **Step 5: Publish evidence atomically into the run root (finding 8)**

Do **not** hand-copy files piecemeal. Derive the destination from `candidate_ref`, refuse to
overwrite an existing run, stage into a temp dir, `verify-promotion` the staged shape, then
atomically rename into place:

```bash
run_root="abc/docs/reports/parser-rq/runs/${candidate_ref#sha256:}"
test ! -e "$run_root" || { echo "run root already exists — refusing to overwrite: $run_root" >&2; exit 1; }
stage=$(mktemp -d)/run
mkdir -p "$stage"
cp "$work/executable-provenance.json" "$work/candidate.edn" "$work/readiness-receipt.json" "$stage/"
cp -R "$work/authorizations" "$work/captures" "$work/evaluations" "$stage/"
# verify the staged, committed-SHAPE tree BEFORE publishing it:
clojure -M:abc/parser-rq-campaign verify-promotion \
  --runs-root "$(dirname "$stage")" --candidate-ref "$candidate_ref" \
  --registry data/aat-parser-ir-compatibility.edn \
  --measurements "$work/qualification-measurements.edn" \
  --report "$work/qualification-report.json" \
  --provenance "$stage/executable-provenance.json" \
  --decisions docs/adr/decisions.edn
mkdir -p "$(dirname "$run_root")"
mv "$stage" "$run_root"                    # atomic rename into the repo
cp "$work/qualification-measurements.edn" abc/docs/reports/parser-release-qualification-measurements.edn
cp "$work/qualification-report.json" abc/docs/reports/parser-release-qualification-report.json
```

> **Durable option (recommend filing):** fold this guarded publish (derive-dir-from-ref +
> reject-existing + stage + verify + atomic-rename) into a `parser-rq-campaign publish-run` command
> so no operator hand-assembles a content-addressed run root. Out of scope to build here; the
> guarded shell above is the interim.

- [ ] **Step 6: Verify the release↔qualification identity actually matches (the whole point)**

```bash
new_aozora=$(python3 -c 'import json,sys;d=json.load(open(sys.argv[1]));print(next(e["sha256"] for e in d["executables"] if e["name"]=="ab-aozora"))' \
  "$run_root/executable-provenance.json")
rel=$(nix build ./ab-validator#ab-aozora --no-link --print-out-paths)
echo "qualified=$new_aozora"; sha256sum "$rel/bin/ab-aozora"
```

Expected: the qualified `ab-aozora` sha256 equals the release-bound one (bare hash equal modulo the
`sha256:` prefix). If not, STOP — the derivations are still diverging; return to Task 1/2. Then
clean up the build worktree: `git worktree remove "$cand_tree"`.

- [ ] **Step 7: Commit the regenerated evidence**

```bash
git add abc/docs/reports/parser-rq/runs abc/docs/reports/parser-release-qualification-measurements.edn abc/docs/reports/parser-release-qualification-report.json abc/data/aat-parser-ir-compatibility.edn
git commit -m "evidence(parser-rq): re-qualify ab-aozora against mainline <parser_rev-short>

Regenerated executable-provenance, candidate, measurements, report, and the
admitted registry row for the reproducible, decoupled ab-aozora build (built
from a frozen detached worktree at <parser_rev-short>, clean-tree asserted).
New tuple recorded in the task report; propagation follows in the next task.

🤖 Generated with [Claude Code](https://claude.com/claude-code)"
```

---

### Task 6: Propagate the new tuple to every pinned site

**Files:**
- Modify: `abc/config/full-corpus-publication-custom-parser-ja.json`
- Modify: `abc/flake.nix` (`parser-rq-p5-promotion-audit`, ~line 502 — `candidate_ref` and the
  run-root path; consider renaming the check to `parser-rq-promotion-audit` — see Step 5)
- Modify: `tests/publication-build-real-wiring-smoke.sh`
- Modify: `abc/test/abc/tools/parser_release_authority_test.clj` (lines 16, 19, 22, 25 — the
  `p5-*` def bindings)
- Modify: `abc/test/abc/tools/soranoha_build_publication_test.clj` (lines 90, 219, 221)
- Modify: `abc/test/abc/tools/parser_rq_resource_test.clj` (candidate-ref occurrence)
- Modify: `abc/docs/parser-rq-runbook.md:20` (Recipe 1 `candidate_ref`)

**Interfaces:**
- Consumes: the exact tuple emitted by Task 5 (`candidate_ref`, `qualification_identity_ref`,
  `ab-aozora` sha256, `ab-aat-to-parser-ir` sha256), read from
  `runs/<new-ref>/executable-provenance.json` and `candidate.edn`.
- Produces: a tree in which every pin matches the freshly-qualified build. No later task depends on
  new symbols; this task exists to make the gates in Task 7 pass.

> There is no way to pin these values before Task 5 runs — they are campaign outputs. Treat this
> task as "search-and-replace P5 → new, one value at a time, running the covering test after each".

- [ ] **Step 1: Update the config `parser_candidate_ref`**

In `abc/config/full-corpus-publication-custom-parser-ja.json`, replace `parser_candidate_ref`
(`sha256:15affdfb…`) with the new `candidate_ref`. Run:

```bash
cd abc && bin/kaocha --focus abc.tools.soranoha-build-publication-test
```

Expected: the `checked-in-configs-are-version-0-2-0-test` passes with the new ref (after Step 3
updates its pinned expectation too — run again after Step 3).

- [ ] **Step 2: Update `parser_release_authority_test.clj`**

Replace the four `def` values at lines 16/19/22/25 (`p5-candidate-ref`,
`p5-qualification-identity-ref`, `p5-mapping-hash` — only if the mapping hash changed, which it
does **not** if `aat-to-parser-ir-mapping-v2.json` is untouched; verify — and
`p5-ab-aozora-executable-sha256`) with the new tuple. Run:

```bash
cd abc && bin/kaocha --focus abc.tools.parser-release-authority-test
```

Expected: PASS. (Requires `TEI_SCHEMA_PATH`; run inside the nix check env or `nix develop`.)

- [ ] **Step 3: Update `soranoha_build_publication_test.clj`**

Replace `p5-candidate-ref` (line 90), `p5-parser-build-hash` (line 219, the `ab-aozora` sha256),
and `p5-converter-build-hash` (line 221, the `ab-aat-to-parser-ir` sha256) with the new values.
Run:

```bash
cd abc && bin/kaocha --focus abc.tools.soranoha-build-publication-test
```

Expected: PASS.

- [ ] **Step 4: Update `parser_rq_resource_test.clj` and the two runbook/smoke candidate refs**

Replace the `candidate_ref` occurrence in `abc/test/abc/tools/parser_rq_resource_test.clj`, the
`parser_candidate_ref` in `tests/publication-build-real-wiring-smoke.sh`, and the `candidate_ref`
in `abc/docs/parser-rq-runbook.md:20` (Recipe 1). Run:

```bash
cd abc && bin/kaocha --focus abc.tools.parser-rq-resource-test
```

Expected: PASS.

- [ ] **Step 5: Update the promotion-audit flake check**

In `abc/flake.nix` `parser-rq-p5-promotion-audit` (~line 496-513): set `candidate_ref` to the new
value (the `run_root` is derived from it). The name `…-p5-…` is now historically inaccurate —
rename the attribute to `parser-rq-promotion-audit` and update its references (the runbook
`nix build ./abc#checks…parser-rq-p5-promotion-audit` line and any `flake.nix` self-reference).
Keep a one-line comment noting P5 was superseded on 2026-07-25. Run:

```bash
nix fmt abc/flake.nix
nix build ./abc#checks.x86_64-linux.parser-rq-promotion-audit --print-build-logs
```

Expected: `ok` / GREEN against the new candidate.

- [ ] **Step 5b: Scripted stale-reference assertion (finding 9 — do not trust an enumerated list)**

The "Files" list above is a starting map, not a proof of completeness. After propagating, assert
that **no** old P5 tuple value survives anywhere except the explicitly-frozen historical documents
(the disposition report and the sole-producer plan, which correctly record what was true then):

```bash
cd "$(git rev-parse --show-toplevel)"
frozen='abc/docs/superpowers/reports/2026-07-24-publication-surface-disposition.md|abc/docs/superpowers/plans/2026-07-24-sole-publication-producer.md|abc/docs/superpowers/plans/2026-07-25-p5-parser-requalification.md'
stale=0
for old in 15affdfb677cc6a9 6f365a44b97546 482728cad5bc663c a2656fc9404be9a8; do
  hits=$(rg -l "$old" --glob '!**/target/**' --glob '!**/docs/reports/parser-rq/runs/**' \
    | rg -v "$frozen" || true)
  if [ -n "$hits" ]; then echo "STALE $old still in:"; echo "$hits"; stale=1; fi
done
test "$stale" = 0 || { echo "stale P5 references remain — propagate them or justify as frozen" >&2; exit 1; }
echo "no stale P5 tuple references outside frozen historical docs"
```

Expected: `no stale P5 tuple references …`. Any hit is either a site the map missed (propagate it)
or a genuinely-frozen doc (add it to `$frozen` with a one-line justification in the task report).
Note `mapping_hash` `9be58ff3…` is deliberately excluded from this sweep — it is unchanged if the
mapping file is untouched (Step 2), so it is not a stale value.

- [ ] **Step 6: Commit**

```bash
git add abc/config/full-corpus-publication-custom-parser-ja.json abc/flake.nix tests/publication-build-real-wiring-smoke.sh abc/test/abc/tools/parser_release_authority_test.clj abc/test/abc/tools/soranoha_build_publication_test.clj abc/test/abc/tools/parser_rq_resource_test.clj abc/docs/parser-rq-runbook.md
git commit -m "chore(abc): propagate re-qualified parser tuple to pins, config, tests, checks

🤖 Generated with [Claude Code](https://claude.com/claude-code)"
```

---

### Task 7: Record the governance decision and turn the gate green

**Files:**
- Modify: `abc/docs/adr/decisions.edn` (record the new qualification scope on
  `custom-parser-release-qualification`, slug at line 1594)
- Modify: `abc/docs/adr/custom-parser-release-qualification.md` (update the cited
  `qualification_identity_ref` prose)
- Modify: `abc/docs/superpowers/reports/2026-07-24-publication-surface-disposition.md` (§13:
  mark the follow-up CLOSED with the resolution)
- Modify: `abc/docs/parser-rq-runbook.md` (Recipe 2 status: "exercised on <date>", note residual
  friction fixed)

**Interfaces:**
- Consumes: the green sub-checks from Tasks 4–6 and the new tuple.
- Produces: the closed governance record and a green `publication-build-real-wiring`.

- [ ] **Step 1: Record the qualification decision (governance, by hand)**

Per the governance workflow (the gate never edits decisions), update the Accepted
`custom-parser-release-qualification` record in `abc/docs/adr/decisions.edn` to reflect the new
qualified scope: bump `:accepted` to `2026-07-25`, and add a short claim (c6) stating the release
parser is the reproducible mainline `ab-aozora` whose release-bound and qualification-captured
bytes are proven equal by `checks.<system>.ab-aozora-release-reproducible`, citing
`test/abc/tools/parser_release_authority_test.clj`. Do not delete the P5 claims; append.

- [ ] **Step 2: Validate the decision record**

```bash
cd abc && bin/kaocha --focus abc.tools.decisions-test
nix build ./abc#checks.x86_64-linux.adr-governance --print-build-logs 2>&1 | tail -3
```

Expected: PASS / GREEN (shape-valid, Accepted, dependency decisions intact). Also run the root
`monorepo-adr-governance` check if the decision graph crosses repos.

- [ ] **Step 3: Update the ADR prose and close disposition §13**

Update the `qualification_identity_ref` in `abc/docs/adr/custom-parser-release-qualification.md`
to the new value. In the disposition report §13, add a dated closeout line: the drift is resolved
by unifying the release/qualification `ab-aozora` derivation (D1) and extracting the capture module
into the `ab-aozora-capture` crate so the release binaries (`ab-aozora`, `ab-aat-to-parser-ir`) no
longer embed the four abc classified-source files (D2), re-qualifying against mainline
`<parser_rev-short>`; `publication-build-real-wiring` is now GREEN. Update `parser-rq-runbook.md`
Recipe 2 status from "documented, awaiting host exercise" to "exercised 2026-07-25" and note the
build-proof wrapper + explicit candidate/bind steps (Task 3) closed the Step-0 gap.

- [ ] **Step 4: Turn the target gate green (the acceptance criterion)**

```bash
nix build ./abc#checks.x86_64-linux.publication-build-real-wiring --print-build-logs
```

Expected: **GREEN** — `release_admissible: true`, `build_exit_code: 0`, no
`release-parser-build-hash-mismatch` / `release-converter-build-hash-mismatch`. This is the whole
plan's success condition (Global Constraints).

- [ ] **Step 5: Full abc suite + hygiene**

```bash
cd abc && bin/kaocha            # full suite, 0 failures
nix build ./abc#checks.x86_64-linux.clj-kondo --print-build-logs   # lint + cljfmt
```

Expected: 0 failures; lint/format clean.

- [ ] **Step 6: Commit**

```bash
git add abc/docs/adr/decisions.edn abc/docs/adr/custom-parser-release-qualification.md abc/docs/superpowers/reports/2026-07-24-publication-surface-disposition.md abc/docs/parser-rq-runbook.md
git commit -m "docs(governance): record mainline ab-aozora re-qualification; close real-wiring drift

publication-build-real-wiring is green: release-bound and qualification-captured
ab-aozora are one reproducible artifact, re-qualified against mainline.

🤖 Generated with [Claude Code](https://claude.com/claude-code)"
```

---

## Out of scope (explicit Follow-ups — do not do here)

- **Fully relocate ownership of the four classified-source files.** Task 2 stops the *release
  binaries* (`ab-aozora`, `ab-aat-to-parser-ir`) from embedding them, but they still live in
  `abc/{data,schemas}` and are still `include_bytes!`-embedded at build time into the rq adapters
  (`ab-aozora-capture`, and source-accountability's own parallel embeds) via the
  `../../../../abc/…` cross-repo path. A later question for governance: should these
  parser-qualification authority files be *owned* by `ab-validator` (and abc consume them from
  there), reversing the historical cross-repo direction and removing the last `../abc` reach-up?
  Out of scope here — Task 2 frees the release identity; ownership relocation is a separate design
  decision (this was design option B, deferred).
- **Lane 5:** shrink `abc/src/abc/tools/validate_design_bundle.clj` and its 42 Accepted-claim
  evidence citations (deferred from the sole-producer migration; unrelated to parser identity).
- **The two pre-existing-on-main reds** (`nix-format-check` on `abc/flake.nix`,
  `validate-migration-eval-cache-smoke`) — not introduced by, and not fixed by, this plan.

## Self-review notes (author)

- **Coverage vs disposition §13's 5 steps:** step 1 (regenerate provenance/candidate/qual-identity)
  → Tasks 3+5; step 2 (update `decisions.edn`) → Task 7; step 3 (update pinned test hashes) →
  Task 6; step 4 (update the promotion-audit check) → Task 6 Step 5; step 5 (resolve frozen-vs-
  mainline binding) → the Design Decision + Tasks 1/2/4 (unify on one reproducible mainline
  artifact). All five covered, plus the reproducibility hardening §13's step 5 warned would "keep
  diverging either way."
- **Values-not-knowable-in-advance:** the new hashes are campaign outputs; Tasks 5→6 are
  structured as generate-then-propagate, with each pin's covering test run immediately after its
  edit. This is intentional, not a placeholder.
- **Ordering:** derivation-unify (1) and the capture-crate extraction (2) both precede the CI guard
  (4) and the host campaign (5) — we must qualify the *decoupled* binaries, and the campaign's
  `status: reproducible` gate would refuse before Tasks 1–2 land. Task 2 is a behavior-preserving
  structural move (verbatim module + golden test to a new crate, verify goldens byte-identical); its
  one intended observable effect is the release binaries losing their abc-file dependency.
- **Design depth (Hickey + simplification):** Task 2 is the crate-extraction chosen over both the
  narHash pin (which only made the parser↔abc entanglement *deterministic*) and the runtime-parameter
  sketch (which would have invented an authenticated-input protocol and a hash channel that does not
  exist today — flagged in review as under-specified/risky). Extraction removes the entanglement at
  the crate boundary (the release binaries stop linking a concern they never call), fixes the
  `../../../../abc` dependency direction for those binaries, needs no new protocol, keeps the capture
  goldens byte-identical, and ends the treadmill where an unrelated abc edit re-drifts the release
  parser hash. The narHash pin survives only as an explicitly-labelled, not-to-execute fallback.
