# TEI-EAJ Probe Production Follow-Up Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Promote alignment probes from a fixture-only characterization into regular TEI-EAJ comparison output, remove stale ignored-paper Melos assumptions, and add the first non-TEI-EAJ consumer of the reusable alignment kernel.

**Architecture:** Keep Python as the TEI-EAJ report orchestrator and Rust as the alignment probe producer. The Python workset export remains the join point: it discovers ABC TEI counterparts from explicit files or generated publication directories, and the Rust CLI attaches bounded alignment probes to mismatched compared rows. Add the second consumer in `ab-diff-utils`/`ab-validator` as a normalized-visible comparison CLI so schema or renderer version diffs reuse the same probe vocabulary without TEI-specific code.

**Tech Stack:** Python 3 stdlib report tools, Rust `ab-diff-utils` alignment kernel, `ab-aat-to-parser-ir` TEI-EAJ probe CLI, Nix flake apps/checks, JSON Schema fixtures.

## Global Constraints

- Do not reference or use the ignored paper-demo Melos TEI path.
- Do not restore `paper` as a default ABC TEI input directory.
- Generated ABC TEI inputs must come from explicit `--abc-tei`, explicit `--abc-tei-dir`, `ABC_TEI_EAJ_ABC_TEI_DIRS`, or an existing generated publication root under `target/soranoha/full-corpus-publication-basic-ja/artifacts`.
- The Rust alignment probe remains token-sequence evidence, not source-span evidence.
- The TEI-EAJ comparison schema keeps `alignment_probe` optional on each file row.
- Keep the first production Nix check bounded; full-corpus generation is an operator command, not a flake-check requirement.

---

### Task 1: Remove Stale Melos Defaults And Resolve Melos From Real ABC Inputs

**Files:**
- Modify: `abc/tools/tei_eaj_compare.py`
- Modify: `abc/tools/tei_eaj_aozora_reports.py`
- Modify: `abc/tools/test_tei_eaj_compare.py`

**Interfaces:**
- Produces: `resolve_melos_abc_path(abc, abc_tei_specs, abc_tei_dirs) -> str`
- Produces: wrapper behavior where `--abc-melos` is optional and has no stale default.
- Consumes: existing `discover_abc_counterparts`, `abc_counterpart_from_spec`, and `discover_abc_counterparts`.

- [ ] **Step 1: Write failing Python tests**

Add tests that assert:

```python
def test_melos_report_resolves_abc_from_counterpart_directory_when_abc_omitted(self):
    with tempfile.TemporaryDirectory() as td:
        root = pathlib.Path(td)
        abc_dir = root / "generated" / "artifacts"
        abc_dir.mkdir(parents=True)
        abc = self.write_xml(abc_dir, "work/tei/tei.xml", "走れメロス", "<p>メロス</p>")
        abc.write_text(TEI_WITH_WORK_ID_1567, encoding="utf-8")
        self.write_xml(root, "data/complete/tei_lib_lv4/1567_tei.xml", "走れメロス", "<p>メロス</p>")

        report = probe.build_report(None, root, source_rev="probe-rev", abc_tei_dirs=[abc_dir])

    self.assertEqual(abc.as_posix(), report["abc_path"])
```

Add a wrapper test that parses defaults and asserts no ignored paper-demo TEI path is present in `abc/tools/tei_eaj_aozora_reports.py`.

- [ ] **Step 2: Verify tests fail**

Run:

```bash
python -m unittest abc.tools.test_tei_eaj_compare.TeiEajCompareTest.test_melos_report_resolves_abc_from_counterpart_directory_when_abc_omitted
```

Expected: failure because `build_report` currently requires a concrete `abc_tei` path.

- [ ] **Step 3: Implement Melos input resolution**

Change `build_report` to accept `abc_tei=None`, `abc_tei_specs=()`, `abc_tei_dirs=()`. If `abc_tei` is absent, discover counterparts and use work id `1567`. If it is missing, raise `SystemExit` with a message naming `--abc`, `--abc-tei`, and `--abc-tei-dir`.

- [ ] **Step 4: Remove stale wrapper defaults**

Change `--abc-melos` default from the ignored paper-demo TEI path to `None`. Change `abc_all_work_inputs` so no input means `ABC_TEI_EAJ_ABC_TEI_DIRS` or an existing `target/soranoha/full-corpus-publication-basic-ja/artifacts`; otherwise no ABC counterpart directory is supplied.

- [ ] **Step 5: Verify and commit**

Run:

```bash
python -m unittest abc.tools.test_tei_eaj_compare
```

Commit:

```bash
git add abc/tools/tei_eaj_compare.py abc/tools/tei_eaj_aozora_reports.py abc/tools/test_tei_eaj_compare.py
git commit -m "fix(tei-eaj): resolve melos comparison from generated inputs"
```

### Task 2: Attach Rust Alignment Probes To TEI-EAJ Workset Exports

**Files:**
- Modify: `abc/tools/tei_eaj_aozora_reports.py`
- Modify: `abc/tools/test_tei_eaj_compare.py`
- Modify: `ab-validator/crates/ab-aat-to-parser-ir/src/tei_eaj_alignment_probe.rs`
- Modify: `ab-validator/crates/ab-aat-to-parser-ir/tests/integration.rs`

**Interfaces:**
- Produces: wrapper subcommand `alignment-probe`.
- Produces: wrapper `all-with-probes` command that writes Melos MD, all-work MD, workset JSON, alignment-probe JSON, and alignment-probe MD.
- Consumes: `ab-aat-to-parser-ir tei-eaj-alignment-probe --workset ... --summary-json ... --report-md ...`.

- [ ] **Step 1: Write failing wrapper tests**

Add a Python unit test for command construction using a fake runner:

```python
def test_alignment_probe_command_consumes_generated_workset_without_paper_default(self):
    commands = []
    context = reports.ReportContext(..., abc_melos=None, abc_tei=[], abc_tei_dir=[abc_dir], alignment_probe_bin="probe-bin")
    reports.command_alignment_probe(context, workset_json, summary_json, report_md, run=commands.append)
    self.assertNotIn("paper", " ".join(commands[0]))
    self.assertEqual("probe-bin", commands[0][0])
```

- [ ] **Step 2: Verify tests fail**

Run:

```bash
python -m unittest abc.tools.test_tei_eaj_compare
```

Expected: failure because the wrapper has no alignment-probe command.

- [ ] **Step 3: Implement wrapper commands**

Add `--alignment-probe-bin` to `tei_eaj_aozora_reports.py`. Add `alignment-probe` and `all-with-probes` subcommands. `all-with-probes` first regenerates the workset JSON, then invokes the Rust probe over that workset.

- [ ] **Step 4: Keep Rust path handling explicit**

If needed, update the Rust probe error when `abc_tei` is missing or unreadable so it names the workset row and path. Do not add fallback to `paper`.

- [ ] **Step 5: Verify and commit**

Run:

```bash
python -m unittest abc.tools.test_tei_eaj_compare
cargo test -p ab-aat-to-parser-ir tei_eaj_alignment_probe -- --nocapture
```

Commit:

```bash
git add abc/tools/tei_eaj_aozora_reports.py abc/tools/test_tei_eaj_compare.py ab-validator/crates/ab-aat-to-parser-ir/src/tei_eaj_alignment_probe.rs ab-validator/crates/ab-aat-to-parser-ir/tests/integration.rs
git commit -m "feat(tei-eaj): run alignment probes from report wrapper"
```

### Task 3: Promote Bounded Probe Report Generation Into Nix

**Files:**
- Modify: `abc/flake.nix`
- Modify: `abc/docs/handoffs/tei-eaj-aozora-comparison.md`
- Regenerate: `abc/docs/handoffs/tei-eaj-aozora-workset-export.json`
- Regenerate: `abc/docs/handoffs/tei-eaj-aozora-all-work-comparison-report.md`
- Regenerate: `abc/docs/handoffs/tei-eaj-aozora-melos-comparison-report.md`
- Regenerate: `abc/docs/handoffs/tei-eaj-alignment-probe-melos.json`
- Regenerate: `abc/docs/handoffs/tei-eaj-alignment-probe-melos.md`

**Interfaces:**
- Produces: flake app `tei-eaj-aozora-alignment-probe`.
- Produces: flake check `abc-tei-eaj-aozora-alignment-probe-generation`.

- [ ] **Step 1: Write failing Nix check expectation**

Add a check that builds a tiny generated ABC TEI directory inside the derivation, regenerates workset JSON, runs the Rust alignment probe with `--max-rows 4`, and asserts:

```bash
grep -q "tail_addition" alignment-probe.md
python - <<'PY'
import json
with open("alignment-probe.json", encoding="utf-8") as fh:
    report = json.load(fh)
assert report["schema_version"] == "tei-eaj-alignment-probe-report-v1"
assert report["rows"]
PY
```

- [ ] **Step 2: Verify check fails**

Run:

```bash
nix build .#checks.x86_64-linux.abc-tei-eaj-aozora-alignment-probe-generation --print-build-logs
```

Expected: failure until the app/check wiring exists.

- [ ] **Step 3: Implement app/check wiring**

Add `alignment-probe` and `all-with-probes` launchers to the ABC flake. Pass `${self'.packages.ab-validator-ab-aat-to-parser-ir}/bin/ab-aat-to-parser-ir` if available from the top-level flake; otherwise keep the bounded check in the top-level flake where both ABC and ab-validator packages are in scope.

- [ ] **Step 4: Regenerate handoff reports without stale paths**

Run the bounded generation path. Checked-in handoffs must not contain the ignored paper-demo TEI path.

- [ ] **Step 5: Verify and commit**

Run:

```bash
rg "ignored paper-demo TEI path literal" abc/tools abc/docs/handoffs ab-validator/crates/ab-aat-to-parser-ir/tests
nix build .#checks.x86_64-linux.abc-tei-eaj-aozora-report-generation --print-build-logs
nix build .#checks.x86_64-linux.abc-tei-eaj-aozora-alignment-probe-generation --print-build-logs
```

Commit:

```bash
git add abc/flake.nix abc/docs/handoffs
git commit -m "feat(tei-eaj): promote bounded alignment probe reports"
```

### Task 4: Add The First Non-TEI-EAJ Alignment Consumer

**Files:**
- Modify: `ab-validator/crates/ab-diff-utils/src/lib.rs`
- Create: `ab-validator/crates/ab-validator/src/bin/ab-visible-diff.rs` or add an existing `ab-validator` subcommand if that is the local pattern.
- Add tests in the owning Rust crate.

**Interfaces:**
- Produces: CLI that compares two UTF-8 text files after whitespace removal and emits `alignment-probe-v1` JSON with `left_witness`, `right_witness`, and `evidence_level = "token_sequence_aligned"`.
- Consumes: `ab_diff_utils::align_pair`.

- [ ] **Step 1: Write failing CLI test**

Create two temporary files with the same text plus a tail addition and assert the CLI emits an insertion sample rather than only a first-difference offset.

- [ ] **Step 2: Verify test fails**

Run the focused Rust test for the new CLI.

- [ ] **Step 3: Implement minimal visible-text adapter**

Tokenize by sentence-like runs using the same punctuation heuristic as the TEI-EAJ adapter, normalize with Unicode whitespace removal, and emit the standalone probe object.

- [ ] **Step 4: Verify and commit**

Run:

```bash
cargo test -p ab-validator visible_diff -- --nocapture
cargo test -p ab-diff-utils
```

Commit:

```bash
git add ab-validator/crates/ab-validator ab-validator/crates/ab-diff-utils
git commit -m "feat(diff): add normalized visible text alignment probe"
```

### Task 5: Final Verification, Merge, And Push

**Files:**
- No planned source edits.

- [ ] **Step 1: Run targeted checks**

```bash
python -m unittest abc.tools.test_tei_eaj_compare
cargo test -p ab-aat-to-parser-ir tei_eaj_alignment_probe -- --nocapture
cargo test -p ab-diff-utils
nix build .#checks.x86_64-linux.abc-tei-eaj-aozora-report-generation --print-build-logs
nix build .#checks.x86_64-linux.abc-tei-eaj-aozora-alignment-probe-generation --print-build-logs
```

- [ ] **Step 2: Run full gate**

```bash
nix flake check --print-build-logs
```

- [ ] **Step 3: Merge and push**

Fast-forward `main`, rerun `nix flake check --print-build-logs` on the merged commit, and push `main`. Preserve unrelated dirty files in the original checkout.

## Self-Review

- Spec coverage: covers stale-path removal, regular report integration, bounded Nix promotion, and a second non-TEI-EAJ consumer.
- Scope check: this is a larger follow-up, but each task is independently testable and commit-sized.
- Ambiguity check: full-corpus publication generation is not a flake check; it remains an operator command because it is expected to be expensive.
