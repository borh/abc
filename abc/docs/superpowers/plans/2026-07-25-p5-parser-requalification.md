# Release Parser Decoupling + Reproducibility — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make `checks.<system>.publication-build-real-wiring` durably GREEN by (a) making the standalone production `ab-aozora`/`ab-aat-to-parser-ir` reproducible, and (b) **decoupling** the publication release's parser authentication from the `parser-rq` research campaign — the release authenticates the standalone production parser against a small committed *governed record*, not the 5-parser campaign bundle.

**Architecture:** The sole-publication-producer migration wired the release to authenticate its parser through `abc.tools.parser-rq-campaign` (the 5-parser comparison campaign). Per an explicit user decision, that campaign is a *research artifact* and must have **no bearing** on the publication system. This plan replaces the one coupling seam — `parser-release-authority/authenticate`, which today projects `campaign/promotion-verification` — with a read of a committed `release-parser-identity` record carrying the approved production parser's reproducible build hashes. The release-time verifier and build-time runtime check consume the identical return shape, so blast radius is small. Two supporting reproducibility fixes (pin the parser's build-time git-rev; move the abc-embedding capture module out of the release binaries' crate) keep the recorded hashes stable across commits and unrelated abc edits.

**Tech Stack:** Nix flakes (`ab-validator/flake.nix`, root `flake.nix`); Rust (`ab-validator` crates); Clojure 1.12 + Kaocha (abc); EDN governed records + RFC 8785/JCS SHA-256 content identities.

## Background — why this plan changed

An execution attempt of the earlier (campaign-based) version of this plan surfaced two facts that reframed it:

1. **Two build topologies can never be byte-equal.** The release binds the *standalone* `packages.ab-aozora` / `packages.ab-aat-to-parser-ir` (root `flake.nix:109`), while the qualification captured the *5-package* `parser-rq-candidate` bundle (`parser-rq-campaign-provenance.py:178`). Even after pinning the git-rev env, the standalone `ab-aozora` (`ff31d303…`, 4,888,440 B) ≠ the bundle's (`63d619aa…`, 4,888,816 B): Cargo feature-unification across the 5 packages plus the bundle's `gaijiEnv`/`gaijiBuildInputs`/`AB_ABC_ROOT` produce different bytes. No env fix reconciles this.
2. **User decision (verbatim):** *"the state of the 5 parsers should have no bearing on this system — the parser comparison is mostly a research artifact."* The release must authenticate the **standalone production** parser, and the campaign must not gate publication.

So the release stops routing through the campaign. The legitimate invariant it enforced — *a publication may only release an approved, reproducible parser* — is preserved by a committed governed record + a CI check that proves the buildable standalone matches it. The `parser-rq` campaign remains, as research, untouched and no longer on the release critical path. **Nothing in this plan requires a host campaign, corpus run, or cgroup measurement** — every hash is `nix build … | sha256sum`, runnable in CI/sandbox.

### The one coupling seam (verified)

`parser-release-authority/authenticate` (`abc/src/abc/tools/parser_release_authority.clj:79,91`) calls `(campaign/promotion-verification opts)` and returns `:executable-provenance (:provenance verification)` verbatim. Every "expected" build hash the release enforces originates there:

- `publication_release.clj` `parser-authority-problems` (`:97-163`): expected `parser_build_hash` = `(executable-hash parser-authority adapter_id)` = campaign provenance's `ab-aozora` sha256 (`:139-145`); expected `converter_build_hash` = campaign provenance's `ab-aat-to-parser-ir` sha256 (`:146-151`); expected `mapping_hash`/`parser_ir_schema_hash` = campaign `:qualification-identity` (`:152-163`). `parser_config_hash` is **already** self-recomputed (JCS over the runtime object, `:83-91`) — campaign-independent, no change needed.
- `soranoha_build_publication.clj` `authenticate-runtime` (`:269-317`): the build-time `parser_build_hash` = sha256 of the actual `AB_AOZORA_BIN` executable (`real-resolve-parser-runtime!:339-340`), checked against the same campaign `:executables[].sha256` (`:289-291,300-303`).
- `parser_candidate_ref` in `config/full-corpus-publication-custom-parser-ja.json:6` flows to `authenticate` as `:candidate_ref` (`soranoha_build_publication.clj:1155-1156` → `:248-260`), selecting the campaign run directory.

**Decoupling target:** change *where the expected `ab-aozora`/`ab-aat-to-parser-ir` sha256, `mapping_hash`, and `parser_ir_schema_hash` come from* — from the campaign to a committed record — while keeping `authenticate`'s return shape so `publication_release.clj` and `soranoha_build_publication.clj` consume it unchanged.

---

## Global Constraints

- **The `parser-rq` campaign has no bearing on the publication release.** After this plan, `parser_release_authority.clj` must not `:require` `abc.tools.parser-rq-campaign`. The campaign namespace, its `runs/`, and its checks (`parser-rq-p5-promotion-audit`, `parser-rq-campaign-*`) remain as **research** — do not delete them; just sever the publication dependency.
- **Recompute, never trust.** The governed record's identity refs (`candidate_ref`, `qualification_identity_ref`) are content hashes `authenticate` **recomputes** from the record's canonical (JCS) bytes and requires to match the asserted values — same discipline the campaign used. Never trust an asserted ref.
- **Release identity = approved reproducible build hash + mapping + schema, authorized by the Accepted decision.** Not argv, not git rev, not the campaign bundle.
- **Reproducibility is the trust basis.** The recorded `parser_build_hash`/`converter_build_hash` MUST equal what `nix build ./ab-validator#ab-aozora` / `#ab-aat-to-parser-ir` produce, proven by the CI check in Task 4. Build the parser reproducibly; never record a hash you cannot reproduce.
- **SUCCESS = `nix build ./abc#checks.x86_64-linux.publication-build-real-wiring` GREEN** (`release_admissible: true`), full abc Kaocha suite 0 failures, `clj-kondo`/`cljfmt`/`nix fmt` clean, `monorepo-adr-governance` + `./abc#adr-governance` green.
- **Governance edits by hand.** The `custom-parser-release-qualification` decision is amended through the normal workflow, never by tooling.

### Design decisions (RATIFIED direction; record-location noted for review)

- **D1 — reproducible standalone parser (git-rev pin).** Standalone `abAozora` bakes `AB_AOZORA_GIT_REV = self.rev or "unknown"` (`ab-validator/flake.nix:1904`), so its hash changes every commit. Pin it to `"unknown"` so the recorded `parser_build_hash` is stable and reproducible. (The env edit is already applied uncommitted in the working tree from the earlier attempt.)
- **D2 — release binaries stop embedding abc files (extract capture crate).** `ab-aozora`/`ab-aat-to-parser-ir` link `ab-aozora-aat`, which `include_bytes!`s four `abc` files via `classified_source.rs`; they never call it. Extract `classified_source` into a new `ab-aozora-capture` crate that only the rq adapters depend on, so the release binaries embed no abc bytes and their recorded hash stays valid across unrelated abc edits. Behavior-preserving structural move (goldens unchanged). *(Full rationale/ground-truth retained from the prior revision; unchanged.)*
- **D3 — governed record replaces campaign authentication.** Add `abc/data/release-parser-identity-v1.edn` (see shape below). `authenticate` reads it + the Accepted decision and returns the same-shaped map. **Record-location sub-choice (please confirm at review):** a **separate data file** (chosen here — keeps reproducible hashes out of the ADR prose corpus, content-addressed as data) vs. embedding the hashes inside the `custom-parser-release-qualification` decision in `decisions.edn` (one fewer file, hashes live in the ADR corpus). The plan below is written for the separate-file choice; switching to in-decision changes only Task 3's read source.

**The governed record — `abc/data/release-parser-identity-v1.edn`:**

```clojure
{:schema_version "1.0.0"
 :adapter_id "ab-aozora"
 :adapter_version "ab-aozora 0.6.0 aat-schema 2 facade 0.3.0 wire-schema 3 (git unknown)"
 ;; reproducible sha256 of the STANDALONE release binaries (established in Task 5):
 :executables [{:name "ab-aozora" :sha256 "sha256:<standalone ab-aozora, Task 5>"}
               {:name "ab-aat-to-parser-ir" :sha256 "sha256:<standalone converter, Task 5>"}]
 ;; production coordinates computed from the committed mapping + schema files:
 :qualification_identity {:aat_adapter "ab-aozora"
                          :mapping_hash "sha256:9be58ff3fea272c2a94ae16f05e3e362425e8bcdd20c482a4a842c13fe067142"
                          :parser_ir_schema_hash "sha256:43a6a6d86ca5eca062508e6cae633d19bf5248f15c5bb46153a6d8580ea916ec"}
 ;; self-referential identity refs (recomputed + verified by authenticate):
 :qualification_identity_ref "sha256:<JCS(qualification_identity), Task 3>"
 :candidate_ref "sha256:<JCS(record minus candidate_ref), Task 3>"}
```

`mapping_hash`/`parser_ir_schema_hash` are the *current committed* values (mapping unchanged from P5); verify them in Task 3, don't invent. The two executable sha256 are the only genuinely-new values and are filled in Task 5 after D1+D2 land.

---

### Task 1: Reproducible standalone parser — pin `AB_AOZORA_GIT_REV`

**Files:**
- Modify: `ab-validator/flake.nix` (the `abAozora` derivation `env` block, ~`:1900-1908`)

**Interfaces:**
- Produces: a standalone `packages.ab-aozora` that builds reproducibly (build-a == build-b) with `AB_AOZORA_GIT_REV="unknown"`. Task 5 records its sha256.

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

### Task 3: Decouple `authenticate` onto the governed record (D3)

**Files:**
- Create: `abc/data/release-parser-identity-v1.edn` (the governed record; executable sha256 as placeholders `"sha256:PENDING-ab-aozora"`/`"sha256:PENDING-converter"` — Task 5 fills them and recomputes the refs)
- Modify: `abc/src/abc/tools/parser_release_authority.clj` — rewrite `authenticate` to read the record + decision; drop the `abc.tools.parser-rq-campaign` require
- Modify: `abc/src/abc/tools/publication_release.clj` — `authenticate-parser-authority` (`:251-276`) stops deriving a campaign provenance path; passes the record path instead
- Modify: `abc/src/abc/tools/soranoha_build_publication.clj` — `runtime-authenticate-options`/`release-authority-sources` pass the record path, not campaign paths; drop `:provenance_path` derivation
- Modify: `abc/config/full-corpus-publication-custom-parser-ja.json` — replace `parser_candidate_ref` with `release_parser_identity: "data/release-parser-identity-v1.edn"` (or keep `parser_candidate_ref` carrying the record's `candidate_ref` — pick one and use consistently)
- Modify: `abc/docs/adr/decisions.edn` `custom-parser-release-qualification` — Step 5 records that it now authorizes `data/release-parser-identity-v1.edn` (governance edit); this task only wires the code
- Test: `abc/test/abc/tools/parser_release_authority_test.clj`, `publication_release_test.clj`, `soranoha_build_publication_test.clj`

**Interfaces:**
- Produces: `authenticate` returns the SAME map keys as today (`:candidate-ref :qualification-identity-ref :qualification-identity :executable-provenance :decision :authority-hashes`), sourced from the record. `:executable-provenance` is `{:executables [{:name "ab-aozora" :sha256 …} {:name "ab-aat-to-parser-ir" :sha256 …}]}`. Downstream consumers are unchanged.

- [ ] **Step 1: Write the failing authenticate test against the record**

In `parser_release_authority_test.clj`, replace the campaign-coupled tests with record-based ones. Add a fixture record (a temp EDN mirroring `release-parser-identity-v1.edn` with two known executable sha256) and assert:

```clojure
(deftest authenticate-projects-governed-record-test
  (testing "authenticate returns the record's coordinates, decision-authorized, campaign-free"
    (let [result (authority/authenticate
                  {:release_parser_identity_path <fixture-record>
                   :decisions_path decisions-path})]
      (is (= "ab-aozora" (get-in result [:qualification-identity :aat_adapter])))
      (is (= <fixture-ab-aozora-sha> (:sha256 (first (filter #(= "ab-aozora" (:name %))
                                        (get-in result [:executable-provenance :executables]))))))
      ;; refs are RECOMPUTED, not trusted:
      (is (= (:qualification-identity-ref result)
             (hash/format-sha256 (hash/sha256-json-jcs (:qualification-identity result))))))))

(deftest authenticate-rejects-non-accepted-decision-test ...)   ; authority-problems unchanged
(deftest authenticate-recomputes-and-rejects-tampered-ref-test ...) ; asserted ref != recomputed -> problem
```

Also delete the `promotion-verification`/`:registry-ref`/`:candidate_ref`-breakage tests (`:78-98,138-154`) — they exercise campaign coupling that no longer exists.

- [ ] **Step 2: Run to verify it fails**

Run: `cd abc && bin/kaocha --focus abc.tools.parser-release-authority-test`
Expected: FAIL (new keys/behavior not implemented).

- [ ] **Step 3: Rewrite `authenticate`**

Rewrite `abc/src/abc/tools/parser_release_authority.clj`: drop `[abc.tools.parser-rq-campaign :as campaign]`; add `[abc.tools.hash :as hash]` + a strict record loader. `authenticate` now:

```clojure
(defn authenticate
  [{:keys [release_parser_identity_path decisions_path]}]
  (let [record (load-shape-valid-record! release_parser_identity_path)   ; throws->problems on bad shape
        {:keys [decision content-hash problems]} (decision-resolution decisions_path)
        authority (if decision (authority-problems decision) [])
        qi (:qualification_identity record)
        recomputed-qi-ref (hash/format-sha256 (hash/sha256-json-jcs qi))
        recomputed-cand-ref (hash/format-sha256 (hash/sha256-json-jcs (dissoc record :candidate_ref)))
        ref-problems (cond-> []
                       (not= recomputed-qi-ref (:qualification_identity_ref record))
                       (conj (problem :qualification-identity-ref-mismatch "…"))
                       (not= recomputed-cand-ref (:candidate_ref record))
                       (conj (problem :candidate-ref-mismatch "…")))
        all (vec (concat (record-problems record) problems authority ref-problems))]
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

Keep `decision-resolution`/`authority-problems`/`release-qualification-slug` unchanged. Add `load-shape-valid-record!` (require `:schema_version :adapter_id :adapter_version :executables :qualification_identity :qualification_identity_ref :candidate_ref`; each executable `{:name :sha256}`; sha256 pattern-valid) reporting `:invalid-release-parser-identity` problems, mirroring the decisions loader's strictness.

- [ ] **Step 4: Repoint the two consumers**

`publication_release.clj` `authenticate-parser-authority` (`:251-276`): pass `{:release_parser_identity_path … :decisions_path …}` instead of the campaign path bundle + `candidate-ref`→provenance-path derivation. `soranoha_build_publication.clj` `runtime-authenticate-options` (`:248-260`)/`release-authority-sources` (`:1107-1112`): supply the record path; drop `:provenance_path`/`:runs_root`/`:candidate_ref`/`:measurements_path`/`:report_path`. Update `parser-authority-problems`/`authenticate-runtime` only where the opts map is built — the fields they *read* off the result are unchanged.

- [ ] **Step 5: Update the config**

In `config/full-corpus-publication-custom-parser-ja.json`, replace `parser_candidate_ref` with `"release_parser_identity": "data/release-parser-identity-v1.edn"`; update `build-publication!` (`:1155-1156`) to read it. Grep the schema `soranoha-publication-build-config.schema.json` and update the field there too if `parser_candidate_ref` is declared.

- [ ] **Step 6: Update the two downstream test files**

`publication_release_test.clj`: the `matching-authority` fixture (`:29-38`) already hard-codes an `:executable-provenance {:executables […]}` shape — keep that shape but source it from a record fixture; the mismatch tests (`:151-172`) stay (they test the same coordinate check). `soranoha_build_publication_test.clj`: replace `p5-candidate-ref` plumbing (`:90-91,241-278`) with the record path; the `p5-parser-build-hash`/`p5-converter-build-hash` values become the record's executable sha256 (final values in Task 5 — use `"sha256:PENDING-*"` placeholders now, or point the test at the fixture record).

- [ ] **Step 7: Run all three suites**

```bash
cd abc && bin/kaocha --focus abc.tools.parser-release-authority-test \
  --focus abc.tools.publication-release-test \
  --focus abc.tools.soranoha-build-publication-test
```

Expected: PASS (against fixture/placeholder hashes; real hashes land in Task 5). Requires `TEI_SCHEMA_PATH` (run in `nix develop` or the nix check env).

- [ ] **Step 8: Commit**

```bash
git add abc/data/release-parser-identity-v1.edn abc/src/abc/tools/parser_release_authority.clj abc/src/abc/tools/publication_release.clj abc/src/abc/tools/soranoha_build_publication.clj abc/config/full-corpus-publication-custom-parser-ja.json abc/schemas/soranoha-publication-build-config.schema.json abc/test/abc/tools/parser_release_authority_test.clj abc/test/abc/tools/publication_release_test.clj abc/test/abc/tools/soranoha_build_publication_test.clj
git commit -m "refactor(abc): authenticate release parser from governed record, not the campaign

parser-release-authority/authenticate now reads a committed
data/release-parser-identity-v1.edn (approved reproducible parser hashes +
mapping/schema), recomputing its identity refs, instead of projecting the
parser-rq campaign. The publication release no longer depends on the 5-parser
research campaign. Return shape unchanged; consumers untouched.

🤖 Generated with [Claude Code](https://claude.com/claude-code)"
```

---

### Task 4: CI check — buildable standalone parser matches the governed record

**Files:**
- Modify: `abc/flake.nix` (add `checks.<system>.ab-aozora-release-identity`)

**Interfaces:**
- Produces: a check that fails if `nix build`'s standalone `ab-aozora`/`ab-aat-to-parser-ir` sha256 ≠ the record's `:executables[].sha256`. This is the trust link: the governed record is only valid if the buildable release parser reproduces it.

- [ ] **Step 1: Add the check**

In `abc/flake.nix` `checks.<system>`, add a check that reads `data/release-parser-identity-v1.edn`, builds the two standalone `ab-validator` packages, and byte-compares:

```nix
          ab-aozora-release-identity =
            pkgs.runCommand "abc-ab-aozora-release-identity"
              { nativeBuildInputs = [ pkgs.coreutils pkgs.jq ]; }
              ''
                ${copyWritableSource}
                a=$(sha256sum ${abValidator.ab-aozora}/bin/ab-aozora | cut -d' ' -f1)
                c=$(sha256sum ${abValidator.ab-aat-to-parser-ir}/bin/ab-aat-to-parser-ir | cut -d' ' -f1)
                # extract recorded hashes (strip sha256: prefix) from the EDN record:
                want_a=$(grep -oE 'ab-aozora" :sha256 "sha256:[0-9a-f]{64}' data/release-parser-identity-v1.edn | grep -oE '[0-9a-f]{64}$')
                want_c=$(grep -oE 'ab-aat-to-parser-ir" :sha256 "sha256:[0-9a-f]{64}' data/release-parser-identity-v1.edn | grep -oE '[0-9a-f]{64}$')
                [ "$a" = "$want_a" ] || { echo "ab-aozora build $a != recorded $want_a" >&2; exit 1; }
                [ "$c" = "$want_c" ] || { echo "ab-aat-to-parser-ir build $c != recorded $want_c" >&2; exit 1; }
                mkdir -p "$out"; echo "release parser build matches governed record" > "$out/result.txt"
              '';
```

> **Implementer note:** confirm how the root/abc flake exposes the standalone `ab-validator` packages to `abc/flake.nix` (the migration already references `abValidatorPackages."ab-aozora"` at root `flake.nix:109`; reuse the same accessor here). If a robust EDN read is preferred over `grep`, use `clojure -M -e` to read the record — but keep the check dependency-light.

- [ ] **Step 2: Format + note it will be RED until Task 5**

`nix fmt abc/flake.nix`. This check is expected RED now (record holds `PENDING` placeholders); Task 5 fills the real hashes and turns it green. Do **not** build-verify green here.

- [ ] **Step 3: Commit**

```bash
git add abc/flake.nix
git commit -m "test(abc): add ab-aozora-release-identity check (standalone build == governed record)

🤖 Generated with [Claude Code](https://claude.com/claude-code)"
```

---

### Task 5: Establish the reproducible hashes; turn the gate green; record governance

**Files:**
- Modify: `abc/data/release-parser-identity-v1.edn` (fill the two executable sha256 + recompute refs)
- Modify: `abc/test/abc/tools/soranoha_build_publication_test.clj`, `parser_release_authority_test.clj` (any remaining pinned hashes → the recorded values)
- Modify: `abc/docs/adr/decisions.edn` + `abc/docs/adr/custom-parser-release-qualification.md` (governance: record authorizes the production identity; may cite research)
- Modify: `abc/docs/superpowers/reports/2026-07-24-publication-surface-disposition.md` §13 (close: decoupled from campaign)

**Interfaces:**
- Consumes: Tasks 1–4 (reproducible, decoupled binaries + the record + the check).
- Produces: `publication-build-real-wiring` GREEN.

- [ ] **Step 1: Record the reproducible standalone hashes**

```bash
cd "$(git rev-parse --show-toplevel)"
a=$(nix build ./ab-validator#ab-aozora --no-link --print-out-paths --rebuild)
a2=$(nix build ./ab-validator#ab-aozora --no-link --print-out-paths --rebuild)
test "$(sha256sum "$a/bin/ab-aozora"|cut -d' ' -f1)" = "$(sha256sum "$a2/bin/ab-aozora"|cut -d' ' -f1)" || { echo "ab-aozora not reproducible" >&2; exit 1; }
c=$(nix build ./ab-validator#ab-aat-to-parser-ir --no-link --print-out-paths)
echo "ab-aozora    sha256: $(sha256sum "$a/bin/ab-aozora"|cut -d' ' -f1)"
echo "ab-aat-to-ir sha256: $(sha256sum "$c/bin/ab-aat-to-parser-ir"|cut -d' ' -f1)"
```

Write both into `release-parser-identity-v1.edn` as `sha256:<value>` (replacing the `PENDING` placeholders). If ab-aozora is not reproducible, STOP — Task 1/2 incomplete.

- [ ] **Step 2: Recompute the record's identity refs**

Compute `qualification_identity_ref = SHA-256(JCS(:qualification_identity))` and `candidate_ref = SHA-256(JCS(record without :candidate_ref))` using the repo's own hashing (`clojure -M -e` calling `abc.tools.hash`/`abc.tools.jcs`, matching `authenticate`), and write them into the record. Verify by running the authenticate test against the real record:

```bash
cd abc && bin/kaocha --focus abc.tools.parser-release-authority-test
```

Expected: PASS (recomputed == asserted).

- [ ] **Step 3: Propagate the recorded hashes into the remaining tests**

Set the recorded `ab-aozora`/`ab-aat-to-parser-ir` sha256 into `soranoha_build_publication_test.clj` (`p5-parser-build-hash`/`p5-converter-build-hash`, now the standalone values) and any fixture in `parser_release_authority_test.clj`. Run both focuses; expected PASS.

- [ ] **Step 4: Green the CI checks (the acceptance criteria)**

```bash
nix build ./abc#checks.x86_64-linux.ab-aozora-release-identity --print-build-logs
nix build ./abc#checks.x86_64-linux.publication-build-real-wiring --print-build-logs
```

Expected: BOTH GREEN. `publication-build-real-wiring` shows `release_admissible: true`, no `release-parser-build-hash-mismatch`/`release-converter-build-hash-mismatch`. This is the plan's success condition.

- [ ] **Step 5: Record governance + close §13**

Amend `custom-parser-release-qualification` in `decisions.edn` (by hand): bump `:accepted "2026-07-25"`; add a claim that the release parser is the reproducible standalone `ab-aozora`/`ab-aat-to-parser-ir` recorded in `data/release-parser-identity-v1.edn` and proven buildable by `checks.<system>.ab-aozora-release-identity`; the parser-rq campaign is cited as research justification only, with no release authority. Update the ADR `.md` prose. In disposition §13, close the follow-up: the real-wiring RED is resolved by decoupling the release from the research campaign (D3) atop the reproducible, abc-decoupled standalone parser (D1+D2). Validate:

```bash
cd abc && bin/kaocha --focus abc.tools.decisions-test
nix build ./abc#checks.x86_64-linux.adr-governance --print-build-logs
nix build .#checks.x86_64-linux.monorepo-adr-governance --print-build-logs
```

Expected: PASS/GREEN.

- [ ] **Step 6: Full suite + hygiene**

```bash
cd abc && bin/kaocha
nix build ./abc#checks.x86_64-linux.clj-kondo --print-build-logs
```

Expected: 0 failures; lint/format clean.

- [ ] **Step 7: Commit**

```bash
git add abc/data/release-parser-identity-v1.edn abc/test abc/docs
git commit -m "feat(abc): record reproducible release parser; publication-build-real-wiring green

Filled the governed record with the reproducible standalone ab-aozora/
ab-aat-to-parser-ir hashes, recomputed its identity refs, and turned the
real-wiring gate green — the release now authenticates the production parser,
fully decoupled from the parser-rq research campaign.

🤖 Generated with [Claude Code](https://claude.com/claude-code)"
```

---

## Out of scope (explicit follow-ups)

- **Delete/retire the campaign-coupling remnants** now unused by the release (e.g. any `parser_candidate_ref` plumbing, `parser_phase5_frozen_tuple.clj` if fully unreferenced). Only after confirming zero references — a separate cleanup, not this plan.
- **Fully relocate the four classified-source files' ownership** to `ab-validator` (remove the last `../abc` `include_bytes!` reach-up in the rq adapters). D2 frees the release binaries; ownership relocation is a separate design decision.
- **Lane 5** (`validate_design_bundle.clj` shrink) and the two pre-existing-on-main reds (`nix-format-check` on `abc/flake.nix`, `validate-migration-eval-cache-smoke`) — unrelated.

## Self-review

- **Coverage of the user pivot:** release no longer requires the campaign (Task 3 drops the `parser-rq-campaign` require; Global Constraints forbid re-adding it). Campaign left intact as research (not deleted). ✔
- **No host dependency:** every hash is `nix build … | sha256sum`; no corpus/cgroup/auth-window. Tasks 1–5 all run in CI/sandbox. ✔
- **Invariant preserved:** "release only an approved, reproducible parser" → governed record + Accepted decision + `ab-aozora-release-identity` build-match check. ✔
- **Blast radius:** `authenticate` keeps its return shape, so `publication_release.clj`/`soranoha_build_publication.clj` consumers are near-unchanged; the churn is the auth source + config + tests. ✔
- **Values-not-knowable-in-advance:** the two standalone executable sha256 are outputs of D1+D2; Task 3 uses `PENDING` placeholders, Task 5 fills the reproducible values and greens the gate. Intentional, not a placeholder-defect. ✔
- **Open sub-choice for review:** record-as-separate-file (chosen) vs record-in-decision (D3 note) — only Task 3's read source differs.
