# Wrapper-Adapter Identity Completeness Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Complete the input-set identity of the two wrapper-based AAT adapters (`aozora2html`, `aozora-epub3`) by pinning their nix-packaged renderers (Ruby aozora2html gem / AozoraEpub3.jar), so the active skip can safely fire for them too.

**Architecture:** Follows the merged initiative (`2026-07-09-generator-identity-completeness-nix.md`). Today `aozora` is identity-complete and skip-eligible; the two wrapper adapters are marked `adapter_identity_complete=0` because their identity pinned only the Rust mapper, not the external renderer, AND because the mapper is built *after* the skip gate. Both renderers are already nix-packaged as content-addressed store dirs (`upstream-parser-aozora2html`, `upstream-parser-aozora-epub3`) and invoked as arm's-length subprocesses (Ruby / `java -jar`) — so pinning them is license-safe (no derived work; the GPL JAR is run unmodified as a separate process). This plan: (1) adds a `renderer_content_hash` to the generator identity; (2) rewires `run-aat-full.sh` to resolve the renderer store dir and build the wrapper mapper *before* the gate, thread the renderer into both the gate hash and the recorded metadata, and drop the now-obsolete `adapter_identity_complete` flag so skip is uniformly eligible for all three adapters.

**Tech Stack:** Python 3 (`generator_identity`, `unittest`), Bash (`run-aat-full.sh`), Nix (existing `upstream-parser-*` packages).

**Status:** IMPLEMENTED + MERGED to main (merge commit `9dc1e59a`, 2026-07-09). Commits `4d48ac41` (Task 1), `8a42381a` (Task 2). Final whole-branch review: READY, zero findings, all invariants HOLD.

## Global Constraints

- **No stale-as-fresh, ever.** Every output-determining input must be in the identity; the renderer determines the `aat/` output, so a renderer change MUST change `input_set_hash`. Fail toward recompute on any anomaly.
- **Recorded == checked.** The `input_set_hash` computed by the skip gate (via the `generator_identity.py` CLI) must be byte-identical to the one `provenance_fields(...)` writes into `metadata.json`. Both must receive the SAME renderer input. The recorded `input_identity` object (`identity_fields`) must be the object that hashes to the recorded `input_set_hash` — one shared kwargs dict feeds both.
- **Content-based, never mtime.** The renderer is hashed by content of its whole nix store dir via `tree_hash.tree_hash` (uniform for both the Ruby package dir and the JAR package dir). Locally-built binaries stay `file_sha256` (they are not nix store paths); the renderer is a nix package dir, so `tree_hash` of the dir is the content id.
- **Pinned bytes must not move.** `aat_hash.hash_aat_dir` is untouched. Adding `renderer_content_hash` to the identity keyset changes all recorded `input_set_hash` values (including aozora's, whose value is `None`) — acceptable and correct: no real gated 45 GB dump exists yet, so invalidation cost is zero, and a changed keyset correctly forces recompute.
- **Renderer is license-clean to pin:** `aozora2html` gem = BSD-2; `AozoraEpub3.jar` = GPL, run unmodified as a subprocess (no linking/derivation). Pin by hashing the already-redistributed nix package dir.
- **`--print-plan` stays build-free:** all nix builds / cargo builds happen only past the `--print-plan` early-exit.
- Tests run via `python -m unittest discover` (pytest not in the default devShell).

---

### Task 1: Add `renderer_content_hash` to the generator identity

Add an optional renderer input to `build_identity_object`, hashed by `tree_hash` of the renderer's nix store dir. `None` for self-contained adapters (aozora), set for wrapper adapters. Pure Python + unittest, fully sandbox-testable.

**Files:**
- Modify: `ab-validator/reports/aat-fidelity/generator_identity.py`
- Modify: `ab-validator/reports/aat-fidelity/tests/test_generator_identity.py`

**Interfaces:**
- Consumes: `tree_hash.tree_hash(root, *, pattern, exclude_names)` (already imported in `generator_identity.py`).
- Produces: `build_identity_object` gains keyword-only `renderer_dir: str | Path | None = None`, adding key `renderer_content_hash` (= `tree_hash.tree_hash(renderer_dir)` when set, else `None`). `generator_input_set_hash`, `provenance_fields`, `identity_fields`, and `main()` thread it (they use `**kwargs`, so only `main()`'s CLI needs a new flag). Task 2 supplies `renderer_dir` for wrapper adapters.

- [ ] **Step 1: Write the failing tests.** Add to `test_generator_identity.py`. In `setUp`, add a renderer fixture dir:

```python
        self.renderer = self.d / "renderer"
        (self.renderer / "lib").mkdir(parents=True)
        (self.renderer / "lib" / "engine.rb").write_text("render v1", encoding="utf-8")
```

Then add these tests (the helper `self.h(**ov)` calls `generator_input_set_hash`; `self.base` does NOT include a renderer, so absent-renderer stays the default):

```python
    def test_renderer_absent_by_default_is_none(self) -> None:
        obj = gi.build_identity_object(**self.base)
        self.assertIsNone(obj["renderer_content_hash"])

    def test_renderer_presence_changes_hash(self) -> None:
        self.assertNotEqual(self.h(), self.h(renderer_dir=self.renderer))

    def test_renderer_content_change_changes_hash(self) -> None:
        before = self.h(renderer_dir=self.renderer)
        (self.renderer / "lib" / "engine.rb").write_text("render v2", encoding="utf-8")
        self.assertNotEqual(before, self.h(renderer_dir=self.renderer))
```

Extend the existing keyset assertion test (the one calling `build_identity_object(**self.base)` and asserting `set(obj) == {...}`) to include `"renderer_content_hash"`.

- [ ] **Step 2: Run tests to verify they fail**

Run: `cd ab-validator && python -m unittest discover -s reports/aat-fidelity/tests -p 'test_generator_identity*.py' -v`
Expected: FAIL — `KeyError: 'renderer_content_hash'` / `unexpected keyword argument 'renderer_dir'` / keyset mismatch.

- [ ] **Step 3: Add the key to `build_identity_object`.** New keyword-only param and key (place `renderer_dir` after `ab_check_binary`, and `renderer_content_hash` after `ab_check_binary_hash` in the returned dict):

```python
def build_identity_object(
    *,
    corpus_dir: str | Path,
    adapter_version: str,
    adapter_binary: str | Path,
    ab_index_binary: str | Path,
    ab_check_binary: str | Path,
    feature_patterns_file: str | Path,
    renderer_dir: str | Path | None = None,
    timeout: str | None = None,
    features: str | None = None,
    work_ids: str | None = None,
) -> dict[str, Any]:
```

Add to the returned dict (after the `ab_check_binary_hash` entry):

```python
        "renderer_content_hash": (
            tree_hash.tree_hash(renderer_dir) if renderer_dir is not None else None
        ),
```

Extend the docstring: the renderer is the external parser (Ruby aozora2html gem / AozoraEpub3.jar) that wrapper adapters invoke as a subprocess; it determines the `aat/` output, so its nix package dir is hashed by content (`tree_hash`). `None` for self-contained adapters that have no external renderer.

- [ ] **Step 4: Thread `--renderer-dir` through `main()`'s CLI.** Add an optional arg and pass it:

```python
    ap.add_argument("--renderer-dir", default=None)
```

and in the `generator_input_set_hash(...)` call add `renderer_dir=a.renderer_dir,`.

- [ ] **Step 5: Run tests to verify they pass**

Run: `cd ab-validator && python -m unittest discover -s reports/aat-fidelity/tests -v`
Expected: PASS (all aat-fidelity tests, including the three new renderer tests, the extended keyset test, and the untouched skip tests).

- [ ] **Step 6: Commit**

```bash
git add ab-validator/reports/aat-fidelity/generator_identity.py ab-validator/reports/aat-fidelity/tests/test_generator_identity.py
git commit -m "feat(fidelity): add renderer_content_hash to AAT-dump identity"
```

---

### Task 2: Resolve + pin renderers in `run-aat-full.sh`; enable skip for wrapper adapters

Rewire the script so the wrapper adapters' renderer and Rust mapper are both resolved/built *before* the skip gate, thread the renderer into the gate hash and the recorded metadata, and drop the `adapter_identity_complete` flag (all three adapters are now identity-complete). Unsandboxable; verify with `bash -n`, `--print-plan`, and a hash-alignment proof.

**Files:**
- Modify: `ab-validator/reports/aat-fidelity/run-aat-full.sh`

**Interfaces:**
- Consumes: nix packages `upstream-parser-aozora2html` (→ `bin/aozora2html`, store dir is the renderer content) and `upstream-parser-aozora-epub3` (→ `lib/AozoraEpub3.jar`, store dir is the renderer content); `generator_identity`'s new `renderer_dir` kwarg / `--renderer-dir` CLI (Task 1).
- Produces: `metadata.json` whose `input_set_hash` now covers the renderer for wrapper adapters; skip fires for all three adapters when fresh.

- [ ] **Step 1: In the `case` block, set a renderer attr per adapter and drop `adapter_identity_complete`.** For `aozora2html` set `renderer_attr="upstream-parser-aozora2html"`; for `aozora-epub3` set `renderer_attr="upstream-parser-aozora-epub3"`; for `aozora` set `renderer_attr=""`. Remove all three `adapter_identity_complete=...` lines and update the block comment at the top of the `case` to reflect that renderers are now pinned so all adapters are identity-complete. Keep the wrapper adapters' `adapter=`/`build_step=`/`adapter_hash_target=` lines and aozora's nix resolution unchanged.

- [ ] **Step 2: After the ab-index/ab-check resolution and after aozora's nix block, resolve the renderer store dir and build the wrapper mapper — all before the gate.** Add:

```bash
# Resolve the external renderer (wrapper adapters) from its nix store dir so it
# is pinned by content, and build the Rust mapper NOW (before the skip gate) so
# its hash is available to the gate. aozora has no external renderer.
renderer_dir=""
if [[ -n "$renderer_attr" ]]; then
  renderer_dir="$(nix build "$repo_root#$renderer_attr" --no-link --print-out-paths)"
fi
if [[ "$adapter_id" == "aozora2html" ]]; then
  export AB_AOZORA2HTML_BIN="$renderer_dir/bin/aozora2html"
elif [[ "$adapter_id" == "aozora-epub3" ]]; then
  export AB_AOZORAEPUB3_JAR="$renderer_dir/lib/AozoraEpub3.jar"
fi
# Wrapper adapters build their Rust mapper up front so the gate can hash it
# (adapter_hash_target points at the mapper binary). aozora comes prebuilt from
# nix (build_step empty). This runs pre-workflow_init, so use a plain build (not
# run_step); the post-workflow build-adapter step below re-verifies incrementally.
if [[ ${#build_step[@]} -gt 0 ]]; then
  "${build_step[@]}"
fi
```

- [ ] **Step 3: Make the gate compute the hash for ALL adapters, threading the renderer.** Replace the `if [[ "$adapter_identity_complete" == "1" ]]; then … fi` conditional so the hash + skip check run unconditionally (every adapter is now identity-complete), adding `--renderer-dir` when set. The gate block becomes:

```bash
if [[ -e "$out_dir" && "$force" != "1" ]]; then
  # Active skip: if a prior dump at $out_dir is provably fresh for the current
  # inputs, exit 0 without recomputing. Every adapter is identity-complete now
  # (aozora is a self-contained nix binary; wrapper adapters pin their Rust
  # mapper AND their nix-packaged renderer, both resolved above), so the check
  # runs for all. The input_set_hash here MUST be byte-identical to the one
  # provenance_fields(...) writes into metadata.json below.
  renderer_arg=()
  if [[ -n "$renderer_dir" ]]; then
    renderer_arg=(--renderer-dir "$renderer_dir")
  fi
  current_hash="$(python "$repo_root/reports/aat-fidelity/generator_identity.py" \
    --corpus-dir "$corpus/cards" \
    --adapter-version "$(cd "$repo_root" && "$adapter" --version)" \
    --adapter-binary "$adapter_hash_target" \
    --ab-index-binary "$ab_index_bin" \
    --ab-check-binary "$ab_check_bin" \
    --feature-patterns "$repo_root/data/feature-patterns.toml" \
    "${renderer_arg[@]}" \
    ${timeout:+--timeout "$timeout"} \
    ${features:+--features "$features"} \
    ${work_ids:+--work-ids "$work_ids"})"
  if python "$repo_root/reports/aat-fidelity/generator_skip.py" \
       --out-dir "$out_dir" --input-set-hash "$current_hash"; then
    printf '%s AAT dump already fresh, skipping: %s\n' "$adapter_id" "$out_dir"
    exit 0
  fi
  printf 'output directory exists (stale or unverifiable): %s\n' "$out_dir" >&2
  printf 'pass --force to replace it\n' >&2
  exit 2
fi
```

- [ ] **Step 4: Remove the now-redundant post-gate epub3 JAR block.** Delete the `if [[ "$adapter_id" == "aozora-epub3" && -z "${AB_AOZORAEPUB3_JAR:-}" ]]; then … fi` block — the JAR is now resolved and exported up front in Step 2. Keep the post-`workflow_init` `build-adapter` `run_step` as an incremental re-verify + workflow log (cargo is incremental, near-instant since Step 2 already built it).

- [ ] **Step 5: Thread the renderer into the metadata heredoc's identity.** Add `"$renderer_dir"` to the heredoc's positional args and to the `= sys.argv[1:]` unpacking. In the shared `identity_kwargs` dict, add:

```python
    renderer_dir=(renderer_dir or None),
```

(so a wrapper adapter records its renderer hash; aozora, with `renderer_dir=""`, records `None`). This keeps the gate hash and the recorded hash aligned, and — because `identity_kwargs` feeds both `provenance_fields` and `identity_fields` — keeps the recorded object consistent with the recorded hash.

- [ ] **Step 6: Verify (unsandboxable — no 45 GB run).**

Run: `cd ab-validator && bash -n reports/aat-fidelity/run-aat-full.sh` → clean.
Run: `reports/aat-fidelity/run-aat-full.sh --adapter aozora2html --corpus <dir-with-cards/> --print-plan` → fast, ZERO nix/cargo builds.

**Hash-alignment proof (the key check):** in a Python snippet, build one shared kwargs dict including `renderer_dir=<the aozora2html renderer store dir>`, and assert the gate CLI path and the metadata path agree:
`run_identity.input_set_hash(generator_identity.identity_fields(**shared)) == generator_identity.provenance_fields(aat_dir=<fixture-aat>, **shared)["input_set_hash"]`, and that the `generator_identity.py` CLI invoked with `--renderer-dir <dir>` (+ the same other args) prints that same hash. Then mutate a file in the renderer dir and assert the hash changes (renderer change defeats skip).

- [ ] **Step 7: Commit**

```bash
git add ab-validator/reports/aat-fidelity/run-aat-full.sh
git commit -m "feat(fidelity): pin wrapper-adapter renderers in identity; enable skip for all adapters"
```

---

## Self-Review

- **Spec coverage:** the follow-up ("fold renderer store-path hashes into wrapper-adapter identity → enable skip") = Task 1 (identity key) + Task 2 (resolve/pin/thread + enable). Covered.
- **No stale-as-fresh:** the renderer is now in the identity (Task 1) and is resolved *before* the gate and threaded into both the gate hash and the recorded hash (Task 2). A renderer change → different `input_set_hash` → no skip. The mapper is built before the gate, so its hash is current at gate time.
- **Recorded == checked:** one `identity_kwargs` dict (incl. `renderer_dir`) feeds both `provenance_fields` and `identity_fields`; the gate CLI mirrors the same args; the hash-alignment proof (Step 6) verifies byte-equality.
- **Type consistency:** `build_identity_object` gains `renderer_dir`; `generator_input_set_hash`/`provenance_fields`/`identity_fields` pass it via `**kwargs`; `main()` adds `--renderer-dir`; the script passes it in both the gate and the heredoc.
- **Pinned bytes:** `aat_hash.hash_aat_dir` untouched; `renderer_content_hash` uses `tree_hash` (the general fold), not the pinned AAT hash.
- **Flag removal safe:** `adapter_identity_complete` is referenced only in `run-aat-full.sh` (verified) — removing it affects nothing else.
