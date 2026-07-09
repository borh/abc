# Skip-Gate Hardening Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Close two gaps in the AAT-generator skip feature found by a pattern sweep: (1) three top-level justfile recipes hardcode `--force`, silently defeating the skip gate when an output dir is reused; (2) the input identity is computed up to three times per run, with the "gate hash == recorded hash" invariant hand-maintained across two call sites instead of structurally.

**Architecture:** Both are hardening of the merged staleness feature. Part 1 is a justfile-only change: make `--force` conditional on an explicit `FORCE=` arg so `just <adapter>-aat-full DIR=<existing dump>` actually consults freshness (the reuse case the feature exists for) instead of always recomputing. Part 2 makes `run-aat-full.sh` compute the input identity ONCE up front (emitting `{input_set_hash, identity_object}` to a temp file), and have both the skip gate and the metadata writer read from it — so `tree_hash(corpus)` runs once instead of three times, and recorded==checked becomes structural (same value) rather than a hand-checked byte-identity between two argument lists.

**Tech Stack:** Bash (justfile, `run-aat-full.sh`), Python 3 (`generator_identity.py`, `unittest`).

## Global Constraints

- **No stale-as-fresh, ever.** Part 1 must keep the fail-closed path intact: a reused dir that is STALE must still refuse (exit 2 "pass --force") rather than silently overwrite or skip. Part 2 must keep the recorded `input_set_hash`, `output_content_hash`, and `input_identity` byte-identical to today's output for identical inputs (characterization tests before/after), and the gate's hash must equal the recorded hash by construction (same emitted payload).
- **Recorded == checked, structurally.** After Part 2, the gate hash and the metadata `input_set_hash` must derive from the SAME computed payload — not two independent `generator_identity` invocations that happen to match. This is the whole point; do not leave a second recompute path.
- **`--print-plan` stays build-free.** The identity emission (which reads binaries + hashes the corpus) must sit PAST the `--print-plan` early-exit, like the existing binary resolutions.
- **`adapter --version` parity.** The value fed into identity and the value recorded in `metadata["adapter_version"]` must be the same string, both produced from `$repo_root` (today the gate uses `cd "$repo_root" && "$adapter" --version`, the heredoc uses `run([adapter,"--version"], cwd=repo)`). Compute it once and reuse.
- **Pinned bytes:** `aat_hash.hash_aat_dir` (output_content_hash source) is untouched. `build_identity_object`'s construction is unchanged — Part 2 only changes HOW MANY TIMES and WHERE it is called, never its output bytes.
- Tests run via `python -m unittest discover` (pytest not in the default devShell).

---

### Task 1: Make `--force` conditional in the three `*-aat-full` recipes

The recipes `aozora2html-aat-full`, `aozora-aat-full`, `aozora-epub3-aat-full` (justfile ~564/573/582) hardcode `--force` in their `args=(...)`, so `run-aat-full.sh`'s freshness gate (`if [[ -e "$out_dir" && "$force" != "1" ]]`) never runs. Add a `FORCE=""` param and pass `--force` only when set.

**Files:**
- Modify: `ab-validator/justfile` (the three `*-aat-full` recipes)

**Interfaces:**
- `run-aat-full.sh` already treats `--force` → `force=1` → bypass gate; absent → gate consults `generator_skip`. No script change needed for Task 1.

- [ ] **Step 1: Read the three recipes and check for callers that rely on force-overwrite.** Read `aozora2html-aat-full`, `aozora-aat-full`, `aozora-epub3-aat-full` in `ab-validator/justfile`. Then grep for any test/script that invokes these recipes with a fixed/reused `DIR=` and would now break if the second run skips or refuses instead of overwriting: `grep -rn 'aat-full' tests/ reports/ | grep -iE 'DIR=|just '`. Also check `aozora-epub3-aat-full-smoke` (justfile ~591) and `tests/aozora-epub3-aat-full-smoke.sh`. If any caller depends on unconditional overwrite, note it — those callers should pass `FORCE=1` explicitly. Report findings before editing.

- [ ] **Step 2: Add `FORCE=""` param + conditional flag to `aozora2html-aat-full`.** Change the signature to end with `FORCE=""` and replace the hardcoded `--force` in the `args=(...)` line with a conditional append. The recipe becomes (preserving all other lines exactly):

```
aozora2html-aat-full DIR="" JOBS="0" TIMEOUT="180s" REPORT_ID="" WORK_IDS="" FEATURES="" FORCE="":
	@run_dir="{{DIR}}"; if [ -z "$run_dir" ]; then run_dir="{{ab_db_root}}/aat-corpus/aozora2html-full-$(date -u +%Y%m%dT%H%M%SZ)"; fi; \
	jobs="{{JOBS}}"; if [ "$jobs" = "0" ]; then jobs="$(nproc)"; fi; \
	report_id="{{REPORT_ID}}"; if [ -z "$report_id" ]; then report_id="aozora2html-full-$(date -u +%F)"; fi; \
	args=(--out-dir "$run_dir" --jobs "$jobs" --timeout "{{TIMEOUT}}" --report-id "$report_id"); \
	if [ -n "{{FORCE}}" ]; then args+=(--force); fi; \
	if [ -n "{{WORK_IDS}}" ]; then args+=(--work-ids "{{WORK_IDS}}"); fi; \
	if [ -n "{{FEATURES}}" ]; then args+=(--features "{{FEATURES}}"); fi; \
	"{{repo_root}}/reports/aat-fidelity/run-aozora2html-aat-full.sh" "${args[@]}"
```

- [ ] **Step 3: Apply the identical change to `aozora-aat-full`** (default `TIMEOUT="300s"`, run_dir prefix `aozora-full-`, report_id `aozora-full-`, script `run-aozora-aat-full.sh`): add `FORCE=""` param, drop `--force` from `args`, add the `if [ -n "{{FORCE}}" ]; then args+=(--force); fi;` line in the same position.

- [ ] **Step 4: Apply the identical change to `aozora-epub3-aat-full`** (default `TIMEOUT="300s"`, prefix `aozora-epub3-full-`, script `run-aozora-epub3-aat-full.sh`): same transformation.

- [ ] **Step 5: Verify recipes parse and expand correctly.**

Run: `cd ab-validator && just --list 2>/dev/null | grep -E '(aozora2html|aozora|aozora-epub3)-aat-full'` → all three listed with the new `FORCE` param.
Run: `just -n aozora2html-aat-full DIR=/tmp/x 2>&1 | tail -5` → the invoked command has NO `--force`. Then `just -n aozora2html-aat-full DIR=/tmp/x FORCE=1 2>&1 | tail -5` → the command DOES include `--force`. Paste both into the report.

- [ ] **Step 6: Commit**

```bash
git add ab-validator/justfile
git commit -m "fix(fidelity): make --force opt-in in *-aat-full recipes so the skip gate applies on DIR reuse"
```

---

### Task 2: Emit the input-identity payload from `generator_identity.py`

Add a way to compute the identity object and its hash ONCE and serialize both, so the shell can compute identity a single time and reuse it for the gate and the metadata record. Pure Python + unittest.

**Files:**
- Modify: `ab-validator/reports/aat-fidelity/generator_identity.py`
- Modify: `ab-validator/reports/aat-fidelity/tests/test_generator_identity.py`

**Interfaces:**
- Consumes: `build_identity_object(**kwargs)`, `run_identity.input_set_hash(obj)` (both exist).
- Produces: `identity_payload(**kwargs) -> {"input_set_hash": str, "identity_object": dict}`; a `main()` option `--emit-identity PATH` that writes `json.dumps(identity_payload(**kwargs))` to PATH while STILL printing the input_set_hash to stdout (backward compatible with the gate's current capture).

- [ ] **Step 1: Write the failing tests.** Add to `test_generator_identity.py`:

```python
    def test_identity_payload_hash_matches_input_set_hash(self) -> None:
        pay = gi.identity_payload(**self.base)
        self.assertEqual(pay["input_set_hash"], gi.generator_input_set_hash(**self.base))

    def test_identity_payload_object_matches_build(self) -> None:
        pay = gi.identity_payload(**self.base)
        self.assertEqual(pay["identity_object"], gi.build_identity_object(**self.base))

    def test_identity_payload_hash_is_of_its_own_object(self) -> None:
        import run_identity
        pay = gi.identity_payload(**self.base)
        self.assertEqual(pay["input_set_hash"], run_identity.input_set_hash(pay["identity_object"]))

    def test_emit_identity_writes_payload_and_prints_hash(self) -> None:
        import io, json as _json
        from contextlib import redirect_stdout
        out = self.d / "id.json"
        argv = ["--corpus-dir", str(self.corpus), "--adapter-version", "1.2.3",
                "--adapter-binary", str(self.adapter), "--ab-index-binary", str(self.ab_index),
                "--ab-check-binary", str(self.ab_check),
                "--feature-patterns", str(self.feature_patterns),
                "--emit-identity", str(out)]
        buf = io.StringIO()
        with redirect_stdout(buf):
            gi.main(argv)
        printed = buf.getvalue().strip()
        payload = _json.loads(out.read_text())
        self.assertEqual(printed, payload["input_set_hash"])
        self.assertEqual(payload["input_set_hash"], gi.generator_input_set_hash(
            corpus_dir=self.corpus, adapter_version="1.2.3", adapter_binary=self.adapter,
            ab_index_binary=self.ab_index, ab_check_binary=self.ab_check,
            feature_patterns_file=self.feature_patterns))
```

(Match the real `setUp` fixture names — the test file already defines `self.corpus`, `self.adapter`, `self.ab_index`, `self.ab_check`, and a feature-patterns fixture; use whatever the existing helper calls them. If the feature-patterns fixture has a different attr name, use that.)

- [ ] **Step 2: Run tests to verify they fail**

Run: `cd ab-validator && python -m unittest discover -s reports/aat-fidelity/tests -p 'test_generator_identity*.py' -v`
Expected: FAIL — `module 'generator_identity' has no attribute 'identity_payload'` / no `--emit-identity`.

- [ ] **Step 3: Add `identity_payload` and wire `--emit-identity`.** Add the function (near `provenance_fields`):

```python
def identity_payload(**identity_kwargs: Any) -> dict[str, Any]:
    """Compute the identity object and its input_set_hash together, so a caller
    can compute identity ONCE and reuse it for both the skip check and the
    recorded metadata (instead of recomputing build_identity_object per use)."""
    obj = build_identity_object(**identity_kwargs)
    return {"input_set_hash": run_identity.input_set_hash(obj), "identity_object": obj}
```

In `main()`, add the option and, after computing the hash, optionally write the payload:

```python
    ap.add_argument("--emit-identity", default=None,
                    help="write {input_set_hash, identity_object} JSON to this path")
    ...
    pay = identity_payload(
        corpus_dir=a.corpus_dir, adapter_version=a.adapter_version,
        adapter_binary=a.adapter_binary, ab_index_binary=a.ab_index_binary,
        ab_check_binary=a.ab_check_binary, feature_patterns_file=a.feature_patterns,
        renderer_dir=a.renderer_dir, timeout=a.timeout, features=a.features,
        work_ids=a.work_ids,
    )
    if a.emit_identity:
        import json as _json
        pathlib.Path(a.emit_identity).write_text(_json.dumps(pay) + "\n", encoding="utf-8")
    print(pay["input_set_hash"])
```

(Adjust to the ACTUAL current `main()` — it currently calls `generator_input_set_hash(...)` and prints it; replace that with the `identity_payload(...)` computation above so the printed hash is unchanged AND the payload is available. Keep every existing `--` arg. Import `pathlib`/`json` as the file already does.)

- [ ] **Step 4: Run tests to verify they pass**

Run: `cd ab-validator && python -m unittest discover -s reports/aat-fidelity/tests -v`
Expected: PASS (all aat-fidelity tests, incl. the 4 new payload tests and the untouched skip/identity tests).

- [ ] **Step 5: Commit**

```bash
git add ab-validator/reports/aat-fidelity/generator_identity.py ab-validator/reports/aat-fidelity/tests/test_generator_identity.py
git commit -m "feat(fidelity): add identity_payload + --emit-identity to compute AAT identity once"
```

---

### Task 3: Compute the AAT identity once in `run-aat-full.sh`; thread it to gate + metadata

Rewire the script so the identity is computed a single time (up front, into a temp file), the gate reads its hash, and the metadata writer reads its object + hash — removing the two extra `build_identity_object` recomputes and making recorded==checked structural. SAFETY-CRITICAL: the recorded identity bytes must not change.

**Files:**
- Modify: `ab-validator/reports/aat-fidelity/run-aat-full.sh`

**Interfaces:**
- Consumes: `generator_identity.py --emit-identity` (Task 2); `generator_skip.py` (unchanged); `aat_hash.hash_aat_dir` for output_content_hash.

- [ ] **Step 1: Compute `adapter_version` and emit the identity payload ONCE, before the gate.** In the region after all binaries/renderer are resolved and PAST the `--print-plan` early-exit, but before the `if [[ -e "$out_dir" && "$force" != "1" ]]` gate (line ~270), add:

```bash
# Compute the input identity ONCE (tree_hash over the corpus is the expensive
# part) and reuse it for both the skip gate and the recorded metadata, so the
# gate hash and the recorded hash are the SAME value by construction rather than
# two argument lists that must be kept byte-identical by hand.
adapter_version="$(cd "$repo_root" && "$adapter" --version)"
identity_file="$(mktemp)"
cleanup_identity_file() { rm -f "$identity_file"; }
trap cleanup_identity_file EXIT
renderer_arg=()
if [[ -n "$renderer_dir" ]]; then
  renderer_arg=(--renderer-dir "$renderer_dir")
fi
current_hash="$(python "$repo_root/reports/aat-fidelity/generator_identity.py" \
  --emit-identity "$identity_file" \
  --corpus-dir "$corpus/cards" \
  --adapter-version "$adapter_version" \
  --adapter-binary "$adapter_hash_target" \
  --ab-index-binary "$ab_index_bin" \
  --ab-check-binary "$ab_check_bin" \
  --feature-patterns "$repo_root/data/feature-patterns.toml" \
  "${renderer_arg[@]}" \
  ${timeout:+--timeout "$timeout"} \
  ${features:+--features "$features"} \
  ${work_ids:+--work-ids "$work_ids"})"
```

If the script already installs an `EXIT` trap elsewhere, fold `rm -f "$identity_file"` into it instead of adding a competing trap (bash keeps only the last `EXIT` trap). Confirm there is no pre-existing `trap ... EXIT` in this script (`grep -n 'trap ' run-aat-full.sh`); if there is, compose rather than overwrite.

- [ ] **Step 2: Slim the gate to reuse `current_hash`.** The `if [[ -e "$out_dir" && "$force" != "1" ]]` block currently recomputes `current_hash` and builds `renderer_arg`. Remove that recomputation (now done up front) and the now-redundant long comment about "byte-identical to the heredoc"; the block becomes:

```bash
if [[ -e "$out_dir" && "$force" != "1" ]]; then
  # A prior dump exists: skip recompute iff it is provably fresh for the current
  # inputs. current_hash was computed once above and is the same value the
  # metadata below records, so the gate and the record cannot drift.
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

- [ ] **Step 3: Feed the payload + adapter_version into the metadata heredoc.** Pass `"$identity_file"` and `"$adapter_version"` as positional args to the metadata `python - ... <<'PY'` block and unpack them. Replace the `metadata["adapter_version"] = run([adapter, "--version"])` computation with the passed-in value (no second `--version` subprocess), and replace the `identity_kwargs` + `provenance_fields` + `identity_fields` block (lines ~415-437) with reading the emitted payload and computing only `output_content_hash` fresh:

```python
# input identity was computed once up front (identity_file); reuse it verbatim so
# the recorded input_set_hash equals the one the skip gate checked, by construction.
import json as _json
payload = _json.loads(pathlib.Path(identity_file).read_text())
sys.path.insert(0, str(repo / "reports" / "lib"))
import aat_hash  # noqa: E402
metadata["input_set_hash"] = payload["input_set_hash"]
metadata["output_content_hash"] = aat_hash.hash_aat_dir(out / "aat")
metadata["input_identity"] = payload["identity_object"]
```

Keep `metadata["adapter_version"] = adapter_version` (from the passed arg). Ensure the heredoc's argv list and `sys.argv[1:]` unpacking add `identity_file` and `adapter_version` in matching positions. The `generator_identity` import may no longer be needed in the heredoc — remove it if unused (grep the heredoc body).

- [ ] **Step 4: Characterization — the recorded metadata identity fields must be UNCHANGED.** Because there is no fixture corpus in-sandbox, prove byte-equality at the unit level instead: in a Python snippet, for one set of fixture inputs, assert that the NEW path and the OLD path produce identical values:
  - `input_set_hash`: `generator_identity.identity_payload(**k)["input_set_hash"]` == `generator_identity.provenance_fields(aat_dir=FX, **k)["input_set_hash"]` (old).
  - `input_identity`: `identity_payload(**k)["identity_object"]` == `generator_identity.identity_fields(**k)` (old).
  - `output_content_hash`: `aat_hash.hash_aat_dir(FX)` == `generator_identity.provenance_fields(aat_dir=FX, **k)["output_content_hash"]` (old).
  Capture all three equalities in the report. This proves the metadata bytes are unchanged and that the gate hash (== `identity_payload` hash) equals the recorded hash.

- [ ] **Step 5: Verify the script.**

Run: `cd ab-validator && bash -n reports/aat-fidelity/run-aat-full.sh` → clean.
Run: `grep -n 'provenance_fields\|identity_fields\|adapter --version\|generator_identity.py' reports/aat-fidelity/run-aat-full.sh` → the only `generator_identity.py` reference is the single up-front emission; no `provenance_fields`/`identity_fields` remain; `--version` is computed once.
Run: `reports/aat-fidelity/run-aat-full.sh --adapter aozora2html --corpus <dir-with-cards/> --print-plan` → fast, build-free (the emission sits past the early-exit).

- [ ] **Step 6: Commit**

```bash
git add ab-validator/reports/aat-fidelity/run-aat-full.sh
git commit -m "refactor(fidelity): compute AAT input identity once; gate and metadata share it"
```

---

## Self-Review

- **Spec coverage:** Finding #1 = Task 1 (conditional `--force`). Finding #2 = Task 2 (`identity_payload`/`--emit-identity`) + Task 3 (compute-once wiring). Covered.
- **No stale-as-fresh:** Task 1 preserves the fail-closed `exit 2` for a stale reused dir and keeps `FORCE=1` as the explicit overwrite. Task 3 keeps the gate's `generator_skip` verdict logic identical; only the hash's provenance changes (now shared).
- **Recorded == checked becomes structural:** Task 3's gate and metadata both read the single emitted payload; the characterization in Step 4 proves the recorded fields are byte-unchanged and the gate hash equals the recorded hash.
- **Type consistency:** `identity_payload` returns `{input_set_hash, identity_object}`; `--emit-identity` writes it; the heredoc reads exactly those keys. `provenance_fields`/`identity_fields` remain defined (tests/back-compat) but unused by the script.
- **Independence:** Task 1 (justfile) is independent of Tasks 2-3 (Python + script). Task 3 depends on Task 2's `--emit-identity`.
- **Pinned/print-plan:** `aat_hash.hash_aat_dir` and `build_identity_object` construction untouched; the emission sits past the `--print-plan` early-exit.
