# Release Parser Decoupling + Reproducibility — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make `checks.<system>.publication-build-real-wiring` durably GREEN by (a) making the standalone production `ab-aozora`/`ab-aat-to-parser-ir` reproducible, and (b) **decoupling** the publication release's parser authentication from the `parser-rq` research campaign — the release authenticates the standalone production parser against a small committed *governed record* that a new Accepted decision **binds by exact content identity**, not the 5-parser campaign bundle.

**Architecture:** The sole-publication-producer migration wired the release to authenticate its parser through `abc.tools.parser-rq-campaign` (the 5-parser comparison campaign). Per an explicit user decision, that campaign is a *research artifact* and must have **no bearing** on the publication system. This plan replaces the one coupling seam — `parser-release-authority/authenticate`, which today projects `campaign/promotion-verification` — with a read of a committed `release-parser-identity` record whose exact `candidate_ref` a new Accepted decision binds. The release-time verifier and build-time runtime check consume the identical return shape, so blast radius is small. Two supporting reproducibility fixes (pin the parser's build-time git-rev; move the abc-embedding capture module out of the release binaries' crate) keep the recorded hashes stable across commits and unrelated abc edits.

**Tech Stack:** Nix flakes (`ab-validator/flake.nix`, root `flake.nix`); Rust (`ab-validator` crates); Clojure 1.12 + Kaocha (abc); EDN governed records + RFC 8785/JCS SHA-256 content identities.

## Background — why this plan changed

An execution attempt of the earlier (campaign-based) version of this plan surfaced two facts that reframed it:

1. **Two build topologies can never be byte-equal.** The release binds the *standalone* `packages.ab-aozora` / `packages.ab-aat-to-parser-ir` (root `flake.nix:109`), while the qualification captured the *5-package* `parser-rq-candidate` bundle (`parser-rq-campaign-provenance.py:178`). Even after pinning the git-rev env, the standalone `ab-aozora` (`ff31d303…`, 4,888,440 B) ≠ the bundle's (`63d619aa…`, 4,888,816 B): Cargo feature-unification across the 5 packages plus the bundle's `gaijiEnv`/`gaijiBuildInputs`/`AB_ABC_ROOT` produce different bytes. No env fix reconciles this.
2. **User decision (verbatim):** *"the state of the 5 parsers should have no bearing on this system — the parser comparison is mostly a research artifact."* The release must authenticate the **standalone production** parser, and the campaign must not gate publication.

So the release stops routing through the campaign. The legitimate invariant it enforced — *a publication may only release an approved, reproducible parser* — is preserved by a committed governed record that an Accepted decision binds, plus CI checks that prove the buildable standalone matches it and is reproducible. The `parser-rq` campaign remains, as research, untouched and no longer on the release critical path. **Nothing in this plan requires a host campaign, corpus run, or cgroup measurement** — every hash is `nix build … | sha256sum`, runnable in CI/sandbox.

### The one coupling seam (verified)

`parser-release-authority/authenticate` (`abc/src/abc/tools/parser_release_authority.clj:79,91`) calls `(campaign/promotion-verification opts)` and returns `:executable-provenance (:provenance verification)` verbatim. Every "expected" build hash the release enforces originates there:

- `publication_release.clj` `parser-authority-problems` (`:97-163`): expected `parser_build_hash` = `(executable-hash parser-authority adapter_id)` = campaign provenance's `ab-aozora` sha256 (`:139-145`); expected `converter_build_hash` = campaign provenance's `ab-aat-to-parser-ir` sha256 (`:146-151`); expected `mapping_hash`/`parser_ir_schema_hash` = campaign `:qualification-identity` (`:152-163`). `parser_config_hash` is **already** self-recomputed (JCS over the runtime object, `:83-91`) — campaign-independent, no change needed.
- `soranoha_build_publication.clj` `authenticate-runtime` (`:269-317`): the build-time `parser_build_hash` = sha256 of the actual `AB_AOZORA_BIN` executable (`real-resolve-parser-runtime!:339-340`), checked against the same campaign `:executables[].sha256` (`:289-291,300-303`).
- `parser_candidate_ref` in `config/full-corpus-publication-custom-parser-ja.json:6` flows to `authenticate` as `:candidate_ref` (`soranoha_build_publication.clj:1155-1156` → `:248-260`), selecting the campaign run directory.

**Decoupling target:** change *where the expected `ab-aozora`/`ab-aat-to-parser-ir` sha256, `mapping_hash`, and `parser_ir_schema_hash` come from* — from the campaign to a decision-bound committed record — while keeping `authenticate`'s return shape so `publication_release.clj` and `soranoha_build_publication.clj` consume it unchanged.

---

## Global Constraints

- **The `parser-rq` campaign has no bearing on the publication release.** After this plan, `parser_release_authority.clj` must not `:require` `abc.tools.parser-rq-campaign`. The campaign namespace, its `runs/`, and its checks (`parser-rq-p5-promotion-audit`, `parser-rq-campaign-*`) remain as **research** — do not delete them; just sever the publication dependency.
- **Authorization = integrity AND binding.** `authenticate` requires *both*: (a) the record's `candidate_ref`/`qualification_identity_ref` recompute correctly from its JCS bytes (integrity — never trust an asserted ref), **and** (b) the Accepted `release-parser-identity-approval` decision **binds** that exact `candidate_ref` (+ `qualification_identity_ref`, `adapter_id`, converter name, `schema_version`). A shape-valid, self-consistent record that the decision does not bind is **rejected**. Recompute proves integrity; the decision binding proves *approval*.
- **Supersede, don't amend.** `custom-parser-release-qualification` stays historical research evidence (its claims describe the campaign and remain true). Publication authority moves to the new `release-parser-identity-approval` decision; `sole-publication-release-identity` (dependency + c4) is repointed to it. No claim is appended to the old decision.
- **Release identity = approved reproducible build hash + mapping + schema, bound by the Accepted decision.** Not argv, not git rev, not the campaign bundle.
- **Build-match and reproducibility are separate, separately-named, both-required properties.** `release-parser-build-matches-approved-identity` (one build == the record) and `release-parser-reproducible` (two independent `--rebuild` realizations of **both** `ab-aozora` and `ab-aat-to-parser-ir` are byte-identical) are distinct checks; both are release acceptance criteria. Do not let a single-build match stand in for reproducibility.
- **One parsing protocol at the trust boundary.** The CI build-match check reads `release-parser-identity-v1.edn` through the **same** strict Clojure loader `authenticate` uses (`abc.tools.parser-release-authority/load-shape-valid-record!` via `clojure -M -e` or a tiny `-main`), never a line-shape regex. Two consumers, one interpretation.
- **No committed PENDING-authority state.** The record is committed with **real** reproducible hashes (Task 3); the binding decision + the code that enforces it land together and atomically (Task 4). At no commit is active `authenticate` wiring pointed at a non-authoritative or unbound record: Task 3's record is inert (no reader) until Task 4 wires and binds it in one commit.
- **SUCCESS = both `release-parser-*` checks + `publication-build-real-wiring` GREEN** (`release_admissible: true`), full abc Kaocha suite 0 failures, `clj-kondo`/`cljfmt`/`nix fmt` clean, `monorepo-adr-governance` + `./abc#adr-governance` green.
- **Governance edits by hand.** Decisions in `decisions.edn` are authored/amended through the normal governance workflow, never written by tooling.

### Design decisions (RATIFIED)

- **D1 — reproducible standalone parser (git-rev pin).** Standalone `abAozora` bakes `AB_AOZORA_GIT_REV = self.rev or "unknown"` (`ab-validator/flake.nix:1904`), so its hash changes every commit. Pin it to `"unknown"` so the recorded `parser_build_hash` is stable and reproducible. (The env edit is already applied uncommitted in the working tree from the earlier attempt.)
- **D2 — release binaries stop embedding abc files (extract capture crate).** `ab-aozora`/`ab-aat-to-parser-ir` link `ab-aozora-aat`, which `include_bytes!`s four `abc` files via `classified_source.rs`; they never call it. Extract `classified_source` into a new `ab-aozora-capture` crate that only the rq adapters depend on, so the release binaries embed no abc bytes and their recorded hash stays valid across unrelated abc edits. Behavior-preserving structural move (goldens unchanged).
- **D3 — a *bound* governed record replaces campaign authentication.** Decoupling from the campaign is a genuine **redefinition of release authority**, not just a change of data source (per review). So it takes a **new Accepted decision** that approves *one exact* parser identity, and `authenticate` must reject any record the decision does not bind. Two artifacts:
  1. **The record — `abc/data/release-parser-identity-v1.edn`** (separate data file, per user answer): the approved parser identity (reproducible executable hashes + mapping/schema coordinates). Its `candidate_ref` is a content hash over the record's meaningful contents, so binding `candidate_ref` transitively commits to the executable hashes.
  2. **A new Accepted decision — `release-parser-identity-approval`** in `decisions.edn` — that carries a `:release-parser-identity` binding of the record's **exact** `candidate_ref` (plus `qualification_identity_ref`, `schema_version`, `adapter_id`, converter name, and `record_path`). This decision holds `:release-authority :publication`; it may cite the parser-rq campaign as research justification but derives no authority from it.

  `authenticate` then requires **both** (a) the record's own refs recompute correctly (integrity) **and** (b) the Accepted decision's bound `candidate_ref`/`qualification_identity_ref`/names/schema **equal** the record's (authorization). This closes the review's Blocker 1: swapping the executable hashes changes the record's `candidate_ref`, which no longer matches the decision's bound ref → **rejected** until a governance action updates the decision. Changing parser bytes now requires editing *both* the record and the decision — exactly the intended approval boundary.

  **Supersede, do not amend (Blocker 2).** The existing `custom-parser-release-qualification` decision + ADR assert release qualification happens *only* through the predicate campaign with the immutable P5 capture authoritative — contradictory with binding an exact standalone binary. So this plan does **not** append a claim to it; it stays **historical research evidence**. The new `release-parser-identity-approval` decision holds publication authority, and `sole-publication-release-identity`'s dependency + its c4 claim are repointed from `custom-parser-release-qualification` to the new decision. `parser_release_authority.clj`'s `release-qualification-slug` becomes the new slug.

**The governed record — `abc/data/release-parser-identity-v1.edn`:**

```clojure
{:schema_version "1.0.0"
 :adapter_id "ab-aozora"
 :adapter_version "ab-aozora 0.6.0 aat-schema 2 facade 0.3.0 wire-schema 3 (git unknown)"
 ;; reproducible sha256 of the STANDALONE release binaries (established in Task 3, post D1+D2):
 :executables [{:name "ab-aozora" :sha256 "sha256:<standalone ab-aozora>"}
               {:name "ab-aat-to-parser-ir" :sha256 "sha256:<standalone converter>"}]
 ;; production coordinates computed from the committed mapping + schema files:
 :qualification_identity {:aat_adapter "ab-aozora"
                          :mapping_hash "sha256:9be58ff3fea272c2a94ae16f05e3e362425e8bcdd20c482a4a842c13fe067142"
                          :parser_ir_schema_hash "sha256:43a6a6d86ca5eca062508e6cae633d19bf5248f15c5bb46153a6d8580ea916ec"}
 ;; content-identity refs (authenticate RECOMPUTES these AND checks the decision binds them):
 :qualification_identity_ref "sha256:<JCS(qualification_identity)>"
 :candidate_ref "sha256:<JCS(record minus candidate_ref)>"}
```

The new decision's binding (in `decisions.edn`):

```clojure
{:slug "release-parser-identity-approval"
 :status :accepted
 :accepted "2026-07-25"
 :release-authority :publication
 :title "Release Parser Identity Approval"
 :release-parser-identity {:record_path "data/release-parser-identity-v1.edn"
                           :schema_version "1.0.0"
                           :adapter_id "ab-aozora"
                           :converter_name "ab-aat-to-parser-ir"
                           :candidate_ref "sha256:<record candidate_ref>"
                           :qualification_identity_ref "sha256:<record qual ref>"}
 :relations [{:class :lifecycle :type :depends-on :to "custom-parser-release-qualification"}] ; research citation only
 :claims [ ... c1: publication authority binds exactly this record's candidate_ref;
               c2: the parser-rq campaign is research/justification, not authority;
               c3: replacement requires a new record + updated binding (approval boundary);
               each :evidence a real test file ... ]}
```

`mapping_hash`/`parser_ir_schema_hash` are the *current committed* values (mapping unchanged); verify them, don't invent. The two executable sha256 are the only genuinely-new values, established in Task 3 after D1+D2 land — the record is committed with **real** hashes, never a `PENDING` placeholder (Blocker 5).

---

### Task 1: Reproducible standalone parser — pin `AB_AOZORA_GIT_REV`

**Files:**
- Modify: `ab-validator/flake.nix` (the `abAozora` derivation `env` block, ~`:1900-1908`)

**Interfaces:**
- Produces: a standalone `packages.ab-aozora` that builds reproducibly (build-a == build-b) with `AB_AOZORA_GIT_REV="unknown"`. Task 3 records its sha256 (post D2).

- [ ] **Step 1: Confirm/apply the env edit**

The earlier attempt already changed `abAozora`'s `env` to `{ AB_AOZORA_GIT_REV = "unknown"; }` (uncommitted in the working tree). Confirm it reads exactly:

```nix
        abAozora = mkRustBin {
          pname = "ab-aozora";
          cargoBuildFlags = [ "--package" "ab-aozora" ];
          # Release identity is authenticated by reproducible build hash, not a
          # mutable git rev. Baking self.rev changed the hash every commit; pin
          # "unknown" so the recorded parser_build_hash is stable/reproducible.
          env = { AB_AOZORA_GIT_REV = "unknown"; };
          extra.preBuild = stageParserRqAbcAuthorities;
        };
```

If not present, apply it. Run `nix fmt ab-validator/flake.nix`; ensure only `ab-validator/flake.nix` is staged.

- [ ] **Step 2: Verify reproducibility of the standalone artifact**

```bash
p1=$(nix build ./ab-validator#ab-aozora --no-link --print-out-paths --rebuild)
p2=$(nix build ./ab-validator#ab-aozora --no-link --print-out-paths --rebuild)
sha256sum "$p1/bin/ab-aozora" "$p2/bin/ab-aozora"
```

Expected: the two hashes are identical, and `${p}/bin/ab-aozora --version` prints `(git unknown)`. (The first build compiles a large Rust tree — allow a long timeout; do not abort a progressing build.) Record the hash in the task report — it is the *pre-D2* hash; the final recorded value comes after Task 2.

- [ ] **Step 3: Commit**

```bash
git add ab-validator/flake.nix
git commit -m "fix(ab-validator): pin ab-aozora build-time git-rev for reproducible release hash

$(printf 'AB_AOZORA_GIT_REV=self.rev changed the parser hash every commit. Pin\n\"unknown\" so the standalone release binary builds reproducibly and its\nrecorded parser_build_hash is stable.')

🤖 Generated with [Claude Code](https://claude.com/claude-code)"
```

---

### Task 2: Extract `ab-aozora-capture` so release binaries stop embedding abc files (D2)

**Goal:** the release-gated `ab-aozora` and `ab-aat-to-parser-ir` stop linking the `classified_source`/capture module (and its four `include_bytes!` of abc files), while the rq adapters that actually run capture keep identical behavior. **Behavior-preserving structural move** — no function body, `include_bytes!` path, ledger shape, or output byte changes; only which crate the module lives in and who depends on it.

**Ground truth (verified):** `classified_source.rs` (`ab-validator/crates/ab-aozora-aat/src/`, re-exported `lib.rs:15-19`) is the only reader of the four consts (`:20-27`). The only production caller is `ab-parser-rq-source-accountability` (`src/main.rs:298`; `src/recognition.rs`). `ab-aozora`/`ab-aat-to-parser-ir` depend on `ab-aozora-aat` (`Cargo.toml:13`/`:30`) for AAT conversion but never call capture. `ab-parser-rq-diagnostic-authorization` uses capture only in `tests/fixture_capture.rs`. `classified_source.rs` consumes `lex`/`reconcile_accent_edit_facts` from `ab-aozora-aat` (line 992), so the new crate depends on `ab-aozora-aat` (no cycle). `ab-check` doesn't depend on `ab-aozora-aat`.

**Files:**
- Create: `ab-validator/crates/ab-aozora-capture/Cargo.toml`, `…/src/lib.rs`
- `git mv`: `ab-validator/crates/ab-aozora-aat/src/classified_source.rs` → `…/ab-aozora-capture/src/classified_source.rs` (its `include_bytes!("../../../../abc/…")` resolve identically at the same `crates/<name>/src` depth)
- `git mv`: `ab-validator/crates/ab-aozora-aat/tests/classified_source_capture.rs` → `…/ab-aozora-capture/tests/`
- Modify: `ab-validator/crates/ab-aozora-aat/src/lib.rs` (remove `mod classified_source;` + its `pub use` block, `:13-19`)
- Modify: `ab-parser-rq-source-accountability/Cargo.toml` + `src/{main.rs,recognition.rs}` imports → `ab-aozora-capture`
- Modify: `ab-parser-rq-diagnostic-authorization/Cargo.toml` + `tests/fixture_capture.rs` imports → `ab-aozora-capture`
- Modify: `ab-validator/flake.nix` where the crate set is built (add `ab-aozora-capture`; `stageParserRqAbcAuthorities` still needed for the new crate + source-accountability's own `recognition.rs:9-11`/`recognition_corpus.rs:17` embeds)
- Do **not** touch source-accountability's own parallel embeds — it legitimately embeds these files and is not release-gated.

- [ ] **Step 1: PROTECT — green baseline of the existing golden characterization**

```bash
cd ab-validator
cargo test -p ab-aozora-aat --test classified_source_capture
cargo test -p ab-parser-rq-source-accountability
cargo test -p ab-parser-rq-diagnostic-authorization
```

Expected: PASS. The anchor is `production_fixture_regenerates_byte_identically` (`classified_source_capture.rs:302`, goldens at `abc/test/fixtures/parser-rq/classified-source-capture/*`). These exact commands (first retargeted to the new crate) must stay PASS after the move.

- [ ] **Step 2: Record the coupling baseline (read-only; no tracked-file mutation)**

```bash
cd "$(git rev-parse --show-toplevel)"
for pkg in ab-aozora ab-aat-to-parser-ir; do
  p=$(nix build ./ab-validator#$pkg --no-link --print-out-paths)
  bin=$(find "$p/bin" -type f | head -1)
  grep -aqF "parser-rq-ab-aozora-classified-source-v1" "$bin" \
    && echo "BASELINE: $pkg embeds policy string (expected NOW)" \
    || echo "BASELINE: $pkg already clean"
done
```

Expected NOW: at least `ab-aozora` reports "embeds". After the move, Step 5 asserts both clean.

- [ ] **Step 3: TRANSFORM — create `ab-aozora-capture`, move module + golden test**

Create `ab-validator/crates/ab-aozora-capture/Cargo.toml` mirroring `ab-aozora-aat`'s edition and the deps `classified_source.rs` uses (at least `ab-aozora-aat`, plus whatever its `use` block imports — copy from `ab-aozora-aat`'s Cargo.toml). `git mv` the module into `…/ab-aozora-capture/src/`; add `pub mod classified_source; pub use classified_source::{…the items lib.rs used to re-export…};` to the new `src/lib.rs`. `git mv` the golden test into `…/ab-aozora-capture/tests/`. Remove `mod classified_source;` + the `pub use classified_source::{…}` block from `ab-aozora-aat/src/lib.rs`.

- [ ] **Step 4: Repoint the two rq-adapter callers + flake**

Add `ab-aozora-capture` to `[dependencies]` of `ab-parser-rq-source-accountability/Cargo.toml` and `ab-parser-rq-diagnostic-authorization/Cargo.toml`; change their `use ab_aozora_aat::{…capture items…}` to `use ab_aozora_capture::{…}`. Add the crate to `ab-validator/flake.nix` where the crate set is built. Then:

```bash
cd ab-validator && cargo build --workspace
```

Expected: builds. If `ab-aozora-aat` fails because something *inside it* (outside the moved file) referenced `classified_source`, STOP and report — the dependency direction was not as mapped; do not paper over it.

- [ ] **Step 5: VERIFY — goldens identical, release binaries decoupled**

```bash
cd ab-validator
cargo test -p ab-aozora-capture --test classified_source_capture
cargo test -p ab-parser-rq-source-accountability
cargo test -p ab-parser-rq-diagnostic-authorization
cd "$(git rev-parse --show-toplevel)"
fail=0
for pkg in ab-aozora ab-aat-to-parser-ir; do
  p=$(nix build ./ab-validator#$pkg --no-link --print-out-paths)
  bin=$(find "$p/bin" -type f | head -1)
  grep -aqF "parser-rq-ab-aozora-classified-source-v1" "$bin" \
    && { echo "FAIL: $pkg still embeds abc policy bytes" >&2; fail=1; } \
    || echo "ok: $pkg no longer embeds abc policy bytes"
done
test "$fail" = 0
nix build ./ab-validator#parser-rq-candidate --no-link
```

Expected: goldens PASS; both `ok:`; the bundle still builds.

- [ ] **Step 6: Commit**

```bash
git add ab-validator/crates ab-validator/flake.nix
git commit -m "refactor(ab-validator): extract ab-aozora-capture so release binaries drop abc embed

Move classified_source (+ its four abc include_bytes! and golden test)
verbatim into a new ab-aozora-capture crate depended on only by the rq
adapters that run capture. ab-aozora and ab-aat-to-parser-ir no longer link
it, so the release binaries embed no abc bytes — their recorded hash stays
valid across unrelated abc edits. Behavior-preserving (goldens byte-identical).

🤖 Generated with [Claude Code](https://claude.com/claude-code)"
```

---

### Task 3: Establish the approved record with real reproducible hashes (inert)

**Files:**
- Create: `abc/data/release-parser-identity-v1.edn` — with **real** executable sha256 and correctly-computed refs

**Interfaces:**
- Consumes: Tasks 1–2 (reproducible, abc-decoupled standalone binaries).
- Produces: the committed governed record. It is **inert** — no code reads it yet (Task 4 wires and binds it atomically). No PENDING placeholder is ever committed.

- [ ] **Step 1: Build the standalone binaries reproducibly; capture real hashes**

```bash
cd "$(git rev-parse --show-toplevel)"
a1=$(nix build ./ab-validator#ab-aozora --no-link --print-out-paths --rebuild)
a2=$(nix build ./ab-validator#ab-aozora --no-link --print-out-paths --rebuild)
ha=$(sha256sum "$a1/bin/ab-aozora"|cut -d' ' -f1); test "$ha" = "$(sha256sum "$a2/bin/ab-aozora"|cut -d' ' -f1)" || { echo "ab-aozora NOT reproducible" >&2; exit 1; }
c1=$(nix build ./ab-validator#ab-aat-to-parser-ir --no-link --print-out-paths --rebuild)
c2=$(nix build ./ab-validator#ab-aat-to-parser-ir --no-link --print-out-paths --rebuild)
hc=$(sha256sum "$c1/bin/ab-aat-to-parser-ir"|cut -d' ' -f1); test "$hc" = "$(sha256sum "$c2/bin/ab-aat-to-parser-ir"|cut -d' ' -f1)" || { echo "converter NOT reproducible" >&2; exit 1; }
echo "ab-aozora=$ha  ab-aat-to-parser-ir=$hc"
```

Expected: both reproducible (each pair identical). Record `ha`/`hc`. If either is not reproducible, STOP — Task 1/2 incomplete.

- [ ] **Step 2: Verify the mapping/schema coordinates are the current committed values**

```bash
cd abc && clojure -M -e '(require (quote [abc.tools.hash :as h])) (println (h/format-sha256 (h/sha256-file "../ab-validator/data/aat-to-parser-ir-mapping-v2.json")))'
```

Confirm `mapping_hash`/`parser_ir_schema_hash` in the record shape equal the current committed values (the mapping is unchanged; use the repo's own hashing, matching how `build-candidate` computed them). If they differ, investigate — do not invent a value.

- [ ] **Step 3: Write the record with real executable hashes**

Create `abc/data/release-parser-identity-v1.edn` per the D3 shape with `:executables` sha256 = `sha256:$ha` / `sha256:$hc`, real `:qualification_identity`, and placeholder refs to be filled in Step 4.

- [ ] **Step 4: Compute the record's identity refs with the repo's own hashing**

Compute `qualification_identity_ref = SHA-256(JCS(:qualification_identity))` and `candidate_ref = SHA-256(JCS(record without :candidate_ref))` using `abc.tools.hash`/`abc.tools.jcs` — the exact functions `authenticate` will recompute with (Task 4) — via `clojure -M -e`, and write them into the record:

```bash
cd abc && clojure -M -e '(require (quote [abc.tools.hash :as h]) (quote [abc.tools.jcs :as jcs]) (quote [clojure.edn :as edn]))
  (let [r (edn/read-string (slurp "data/release-parser-identity-v1.edn"))
        qi (:qualification_identity r)]
    (println :qi (h/format-sha256 (h/sha256-bytes (jcs/canonical-json-bytes qi))))
    (println :cand (h/format-sha256 (h/sha256-bytes (jcs/canonical-json-bytes (dissoc r :candidate_ref))))))'
```

Write the two printed values into `:qualification_identity_ref`/`:candidate_ref`. (Confirm the exact canonical-bytes helper matches what `authenticate` uses — align on one function; the campaign used `hash/sha256-json-jcs`.)

- [ ] **Step 5: Commit the inert record**

```bash
git add abc/data/release-parser-identity-v1.edn
git commit -m "feat(abc): add governed release-parser-identity record (real reproducible hashes)

The approved standalone ab-aozora/ab-aat-to-parser-ir identity, with content
refs computed by the repo's own JCS+sha256. Inert here (nothing reads it yet);
Task 4 wires authenticate to it and binds it in a governance decision atomically.

🤖 Generated with [Claude Code](https://claude.com/claude-code)"
```

---

### Task 4: Atomic decouple — bind the record, rewrite `authenticate`, add both CI checks

This is one coherent trust-boundary transition: the binding decision, the rewritten `authenticate` (integrity + binding), the two consumers, the config, the tests, and the two CI checks land **together**. Before this commit the record is inert; after it, the release authenticates the bound record and no longer touches the campaign.

**Files:**
- Modify: `abc/docs/adr/decisions.edn` — add the Accepted `release-parser-identity-approval` decision binding the record's exact `candidate_ref`/`qualification_identity_ref`/`adapter_id`/`converter_name`/`schema_version` (values from Task 3's record)
- Modify: `abc/src/abc/tools/parser_release_authority.clj` — rewrite `authenticate`; add `load-shape-valid-record!`; drop `[abc.tools.parser-rq-campaign]`; `release-qualification-slug` → `"release-parser-identity-approval"`
- Modify: `abc/src/abc/tools/publication_release.clj` — `authenticate-parser-authority` (`:251-276`) passes `{:release_parser_identity_path … :decisions_path …}`; no campaign provenance path
- Modify: `abc/src/abc/tools/soranoha_build_publication.clj` — `runtime-authenticate-options` (`:248-260`) / `release-authority-sources` (`:1107-1112`) supply the record path; drop `:provenance_path`/`:runs_root`/`:candidate_ref`/`:measurements_path`/`:report_path`
- Modify: `abc/config/full-corpus-publication-custom-parser-ja.json` — replace `parser_candidate_ref` with `"release_parser_identity": "data/release-parser-identity-v1.edn"`; update the schema `abc/schemas/soranoha-publication-build-config.schema.json`; update `build-publication!` read (`:1155-1156`)
- Modify: `abc/flake.nix` — add checks `release-parser-build-matches-approved-identity` + `release-parser-reproducible`
- Test: `abc/test/abc/tools/parser_release_authority_test.clj`, `publication_release_test.clj`, `soranoha_build_publication_test.clj`

**Interfaces:**
- Produces: `authenticate` returns the SAME keys as today (`:candidate-ref :qualification-identity-ref :qualification-identity :executable-provenance :decision :authority-hashes`), sourced from the record + decision. `:executable-provenance` = `{:executables (:executables record)}`. Downstream consumers read the same fields.

- [ ] **Step 1: Write failing authenticate tests (integrity + binding)**

Replace the campaign-coupled tests in `parser_release_authority_test.clj` with:

```clojure
(deftest authenticate-projects-bound-record-test
  (testing "returns the record's coordinates when the decision binds it"
    (let [r (authority/authenticate {:release_parser_identity_path record-path
                                     :decisions_path decisions-path})]
      (is (= "ab-aozora" (get-in r [:qualification-identity :aat_adapter])))
      (is (= recorded-ab-aozora-sha
             (:sha256 (first (filter #(= "ab-aozora" (:name %))
                                     (get-in r [:executable-provenance :executables])))))))))

(deftest authenticate-recomputes-and-rejects-tampered-ref-test ...)   ; asserted ref != recomputed -> problem
(deftest authenticate-rejects-record-the-decision-does-not-bind-test  ; Blocker-1 guard
  (testing "a shape-valid record whose candidate_ref the decision does not bind is rejected"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"authentication failed"
          (authority/authenticate {:release_parser_identity_path tampered-record  ; new hashes, new self-refs, decision unchanged
                                   :decisions_path decisions-path})))))
(deftest authenticate-rejects-non-accepted-or-non-publication-decision-test ...)  ; authority-problems retained
```

Delete `authenticates-the-committed-p5-candidate-test` and the `promotion-verification`/`:candidate_ref`/`:registry_path`/`:measurements_path` breakage tests (`:78-98,138-154`) — they exercise campaign coupling that no longer exists.

- [ ] **Step 2: Run to verify failure**

Run: `cd abc && bin/kaocha --focus abc.tools.parser-release-authority-test`
Expected: FAIL (record/binding path not implemented).

- [ ] **Step 3: Add the binding decision**

In `decisions.edn`, add the `release-parser-identity-approval` Accepted decision (D3 shape) with `:release-parser-identity` bound to Task 3's record `candidate_ref`/`qualification_identity_ref` and names/schema. Claims c1–c3 as in D3; each `:evidence` cites a real test file (`test/abc/tools/parser_release_authority_test.clj`, `test/abc/tools/publication_release_test.clj`) that this task updates to assert the binding.

- [ ] **Step 4: Rewrite `authenticate`**

Drop `[abc.tools.parser-rq-campaign :as campaign]`; add `[abc.tools.hash :as hash]` + `[abc.tools.jcs :as jcs]` (or reuse `hash/sha256-json-jcs`). Implement `load-shape-valid-record!` (require `:schema_version :adapter_id :adapter_version :executables :qualification_identity :qualification_identity_ref :candidate_ref`; each executable `{:name :sha256}` sha256 pattern-valid; report `:invalid-release-parser-identity` problems, mirroring the decisions loader). Rewrite `authenticate`:

```clojure
(def release-qualification-slug "release-parser-identity-approval")

(defn- binding-problems [decision record]
  (let [b (:release-parser-identity decision)]
    (cond-> []
      (nil? b) (conj (problem :decision-has-no-release-parser-identity-binding "…"))
      (and b (not= (:candidate_ref b) (:candidate_ref record)))
      (conj (problem :decision-does-not-bind-record-candidate-ref "…"
                     :bound (:candidate_ref b) :record (:candidate_ref record)))
      (and b (not= (:qualification_identity_ref b) (:qualification_identity_ref record)))
      (conj (problem :decision-qualification-identity-ref-mismatch "…"))
      (and b (not= (:adapter_id b) (:adapter_id record)))
      (conj (problem :decision-adapter-id-mismatch "…"))
      (and b (not= (:schema_version b) (:schema_version record)))
      (conj (problem :decision-schema-version-mismatch "…")))))

(defn authenticate [{:keys [release_parser_identity_path decisions_path]}]
  (let [record (load-shape-valid-record! release_parser_identity_path)
        {:keys [decision content-hash problems]} (decision-resolution decisions_path)
        qi (:qualification_identity record)
        recompute-qi (hash/format-sha256 (hash/sha256-json-jcs qi))
        recompute-cand (hash/format-sha256 (hash/sha256-json-jcs (dissoc record :candidate_ref)))
        integrity (cond-> []
                    (not= recompute-qi (:qualification_identity_ref record))
                    (conj (problem :qualification-identity-ref-mismatch "…"))
                    (not= recompute-cand (:candidate_ref record))
                    (conj (problem :candidate-ref-mismatch "…")))
        authority (if decision (into (authority-problems decision) (binding-problems decision record)) [])
        all (vec (concat (record-problems record) problems integrity authority))]
    (if (seq all)
      (throw (ex-info "parser release authority authentication failed" {:problems all}))
      {:candidate-ref (:candidate_ref record)
       :qualification-identity-ref (:qualification_identity_ref record)
       :qualification-identity qi
       :executable-provenance {:executables (:executables record)}
       :decision decision
       :authority-hashes {:decisions-file content-hash
                          :record-file (hash/format-sha256 (hash/sha256-file release_parser_identity_path))}})))
```

- [ ] **Step 5: Repoint consumers + config + schema**

`publication_release.clj` `authenticate-parser-authority` (`:251-276`): pass `{:release_parser_identity_path … :decisions_path …}`; drop the campaign-provenance-path derivation. `soranoha_build_publication.clj` `runtime-authenticate-options`/`release-authority-sources`: supply the record path; drop `:provenance_path`/`:runs_root`/`:candidate_ref`/`:measurements_path`/`:report_path`. Only the opts *construction* changes; the fields read off the result are unchanged. Update the config field + its schema + the `build-publication!` read.

- [ ] **Step 6: Implement + pass**

Run: `cd abc && bin/kaocha --focus abc.tools.parser-release-authority-test`
Expected: PASS. Then update `publication_release_test.clj` (`matching-authority` fixture `:29-38` → sourced from a record fixture; mismatch tests `:151-172` stay) and `soranoha_build_publication_test.clj` (replace `p5-candidate-ref` plumbing with the record path; `p5-parser-build-hash`/`p5-converter-build-hash` become the record's real executable sha256 from Task 3). Run all three focuses; expected PASS.

- [ ] **Step 7: Add the two CI checks (build-match via strict loader; reproducible)**

In `abc/flake.nix`:

```nix
          release-parser-build-matches-approved-identity =
            pkgs.runCommand "abc-release-parser-build-matches-approved-identity"
              { nativeBuildInputs = [ pkgs.clojure pkgs.coreutils ]; }
              ''
                ${copyWritableSource}
                ${cljSandboxEnv}
                # read recorded hashes through the SAME strict loader authenticate uses:
                read want_a want_c < <(clojure -M -e '(require (quote [abc.tools.parser-release-authority :as a]))
                  (let [r (a/load-shape-valid-record! "data/release-parser-identity-v1.edn")
                        h (fn [n] (:sha256 (first (filter #(= n (:name %)) (:executables r)))))]
                    (println (subs (h "ab-aozora") 7) (subs (h "ab-aat-to-parser-ir") 7)))')
                a=$(sha256sum ${abValidatorPackages."ab-aozora"}/bin/ab-aozora | cut -d' ' -f1)
                c=$(sha256sum ${abValidatorPackages."ab-aat-to-parser-ir"}/bin/ab-aat-to-parser-ir | cut -d' ' -f1)
                [ "$a" = "$want_a" ] || { echo "ab-aozora $a != recorded $want_a" >&2; exit 1; }
                [ "$c" = "$want_c" ] || { echo "converter $c != recorded $want_c" >&2; exit 1; }
                mkdir -p "$out"; echo "release parser build matches approved record" > "$out/result.txt"
              '';

          release-parser-reproducible =
            pkgs.runCommand "abc-release-parser-reproducible"
              { nativeBuildInputs = [ pkgs.coreutils ]; }
              ''
                # two independently-realized store paths per binary must be byte-identical.
                a1=$(sha256sum ${abAozoraA}/bin/ab-aozora|cut -d' ' -f1); a2=$(sha256sum ${abAozoraB}/bin/ab-aozora|cut -d' ' -f1)
                c1=$(sha256sum ${abConvA}/bin/ab-aat-to-parser-ir|cut -d' ' -f1); c2=$(sha256sum ${abConvB}/bin/ab-aat-to-parser-ir|cut -d' ' -f1)
                [ "$a1" = "$a2" ] && [ "$c1" = "$c2" ] || { echo "not reproducible" >&2; exit 1; }
                mkdir -p "$out"; echo "ab-aozora=$a1 converter=$c1 reproducible" > "$out/result.txt"
              '';
```

> **Implementer note:** for `release-parser-reproducible`, realize two independent builds of each binary. If the flake's fixed-output/`--rebuild` model makes two in-eval realizations collapse to one store path, use the provenance script's `realize-build`×2 mechanism or `nix build --rebuild` in a `runCommand` with the daemon — pick whichever genuinely produces two independent realizations, and document in the task report exactly what independence the check establishes. Confirm how `abc/flake.nix` accesses the standalone `ab-validator` packages (`abValidatorPackages` per root `flake.nix:109`).

- [ ] **Step 8: Format, build both checks GREEN**

```bash
nix fmt abc/flake.nix
cd abc && bin/kaocha --focus abc.tools.parser-release-authority-test --focus abc.tools.publication-release-test --focus abc.tools.soranoha-build-publication-test
nix build ./abc#checks.x86_64-linux.release-parser-build-matches-approved-identity --print-build-logs
nix build ./abc#checks.x86_64-linux.release-parser-reproducible --print-build-logs
```

Expected: suites PASS; both checks GREEN (the record's hashes match the real reproducible builds).

- [ ] **Step 9: Commit (atomic trust-boundary transition)**

```bash
git add abc/docs/adr/decisions.edn abc/src/abc/tools/parser_release_authority.clj abc/src/abc/tools/publication_release.clj abc/src/abc/tools/soranoha_build_publication.clj abc/config/full-corpus-publication-custom-parser-ja.json abc/schemas/soranoha-publication-build-config.schema.json abc/flake.nix abc/test/abc/tools/parser_release_authority_test.clj abc/test/abc/tools/publication_release_test.clj abc/test/abc/tools/soranoha_build_publication_test.clj
git commit -m "refactor(abc): authenticate release parser from a decision-bound governed record

parser-release-authority/authenticate now reads data/release-parser-identity-v1.edn
and requires BOTH its content refs to recompute AND the Accepted
release-parser-identity-approval decision to bind that exact candidate_ref —
integrity plus approval. Drops the parser-rq-campaign dependency; return shape
unchanged so consumers are untouched. Adds build-match + reproducibility checks.

🤖 Generated with [Claude Code](https://claude.com/claude-code)"
```

---

### Task 5: Green the release gate; repoint governance; full verification

**Files:**
- Modify: `abc/docs/adr/decisions.edn` — repoint `sole-publication-release-identity` (its `:depends-on` relation + c4 claim) from `custom-parser-release-qualification` to `release-parser-identity-approval`
- Modify: `abc/docs/adr/custom-parser-release-qualification.md` — note it remains historical research evidence; publication release authority moved to `release-parser-identity-approval`
- Modify: `abc/docs/superpowers/reports/2026-07-24-publication-surface-disposition.md` §13 — close the follow-up

**Interfaces:**
- Consumes: Task 4 (bound record + rewired authenticate + checks).
- Produces: `publication-build-real-wiring` GREEN and a consistent governance narrative.

- [ ] **Step 1: Repoint `sole-publication-release-identity` to the new authority**

In `decisions.edn`, change its `:depends-on` relation and c4 claim text from `custom-parser-release-qualification` to `release-parser-identity-approval` (the release now depends on the exact Accepted identity-approval decision). Validate:

```bash
cd abc && bin/kaocha --focus abc.tools.decisions-test
nix build ./abc#checks.x86_64-linux.adr-governance --print-build-logs
nix build .#checks.x86_64-linux.monorepo-adr-governance --print-build-logs
```

Expected: PASS/GREEN.

- [ ] **Step 2: The release gate (acceptance criterion)**

```bash
nix build ./abc#checks.x86_64-linux.publication-build-real-wiring --print-build-logs
```

Expected: **GREEN** — `release_admissible: true`, no `release-parser-build-hash-mismatch`/`release-converter-build-hash-mismatch`. (This likely already passed after Task 4's authenticate flip; confirm here as the final gate.)

- [ ] **Step 3: Governance prose + close §13**

Update `custom-parser-release-qualification.md` (historical/research note). In disposition §13, add a dated closeout: the real-wiring RED is resolved by decoupling the release from the research campaign onto a decision-bound reproducible standalone parser (D1+D2+D3); the parser-rq campaign is research only.

- [ ] **Step 4: Full suite + hygiene**

```bash
cd abc && bin/kaocha
nix build ./abc#checks.x86_64-linux.clj-kondo --print-build-logs
```

Expected: 0 failures; lint/format clean.

- [ ] **Step 5: Commit**

```bash
git add abc/docs
git commit -m "docs(governance): move publication authority to release-parser-identity-approval

Repoint sole-publication-release-identity to the exact bound identity decision;
mark custom-parser-release-qualification historical research; close disposition
§13. publication-build-real-wiring is green — the release authenticates the
reproducible standalone parser, fully decoupled from the parser-rq campaign.

🤖 Generated with [Claude Code](https://claude.com/claude-code)"
```

---

## Out of scope (explicit follow-ups)

- **Delete/retire the campaign-coupling remnants** now unused by the release (any residual `parser_candidate_ref` plumbing, `parser_phase5_frozen_tuple.clj` if fully unreferenced). Only after confirming zero references — a separate cleanup, not this plan.
- **Fully relocate the four classified-source files' ownership** to `ab-validator` (remove the last `../abc` `include_bytes!` reach-up in the rq adapters). D2 frees the release binaries; ownership relocation is a separate design decision.
- **Lane 5** (`validate_design_bundle.clj` shrink) and the two pre-existing-on-main reds (`nix-format-check` on `abc/flake.nix`, `validate-migration-eval-cache-smoke`) — unrelated.

## Self-review

- **Blocker 1 (approval, not just integrity):** `authenticate`'s `binding-problems` requires the Accepted decision to bind the record's exact `candidate_ref` (+ qual-ref/adapter/schema); a self-consistent-but-unbound record is rejected (`authenticate-rejects-record-the-decision-does-not-bind-test`). ✔
- **Blocker 2 (supersede, not amend):** a new `release-parser-identity-approval` decision holds authority; `custom-parser-release-qualification` is left historical; `sole-publication-release-identity` is repointed (Task 5). No contradictory claim appended. ✔
- **Strong 3 (build-match ≠ reproducible):** two separately-named checks, both required; `release-parser-reproducible` realizes both binaries twice. ✔
- **Strong 4 (parse EDN as EDN):** the build-match check reads the record via `load-shape-valid-record!` — the same loader `authenticate` uses — not a regex. ✔
- **Strong 5 (no PENDING authority):** Task 3 commits the record with real hashes but inert (no reader); Task 4 lands the binding decision + wiring + checks atomically. No commit has active wiring to an unbound/placeholder record. ✔
- **Decouple pivot:** `parser_release_authority.clj` drops the `parser-rq-campaign` require (Global Constraint); campaign left intact as research. ✔
- **No host dependency:** every hash is `nix build … | sha256sum`; all tasks run in CI/sandbox. ✔
- **Values-not-knowable-in-advance:** the two executable sha256 are outputs of D1+D2, established with real values in Task 3 (never placeholders); the record's refs and the decision binding derive from them. Intentional, not a placeholder-defect. ✔
