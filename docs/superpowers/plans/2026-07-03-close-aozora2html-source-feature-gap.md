# Close Aozora2html Source-Feature Gap Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use `superpowers:executing-plans` or `superpowers:subagent-driven-development` to execute this plan task-by-task. Track progress by updating the checkbox list.

**Goal:** Convert the remaining clean aozora2html policy residuals into either emitted AAT observations or explicitly accepted source-index-only evidence, then regenerate the post-measurement gate. This plan does not start the owned AAT-to-parser-IR CLI.

**Current blocker:** `docs/superpowers/reports/2026-07-03-post-measurement-abc-sync.md` still reports `MEASUREMENT_BLOCKED`. The clean policy-specific bucket is 65 warigaki works and 3 kunten works in `source_feature_without_aat_observation`: reports present, AAT present, check reports clean, but no family observation counted.

**Architecture:** Keep the measurement boundary file-based. Add a small classifier over the existing residual worksets and source index, patch only adapter/measurement behavior proven by that classifier, run a targeted workset retry at `JOBS=24`, then regenerate the existing audit/sample/residual/ABC-sync reports. Full corpus artifacts stay under `/db`; only scripts, tests, and compact reports are committed.

**Tech Stack:** Rust adapter tests, Python report scripts, Bash smoke tests, existing `just aozora2html-aat-full`, existing `/db/ab-validator/aat-corpus/aozora2html-full-20260703T020301Z` baseline plus retry `/db/ab-validator/aat-corpus/aozora2html-policy-retry-20260703T055526Z`.

---

## Global Constraints

- Do not change `data/aat-schema.json`, ABC parser-IR schemas, or AAT v2 vocabulary in this plan.
- Do not implement `crates/ab-aat-to-parser-ir` in this plan.
- Do not edit `../abc`; emit sync material here for the ABC repo to consume.
- Keep `/db/ab-validator/aat-corpus/...` generated output out of git.
- Workset JSON files must be flat arrays of work-ID strings, matching `ab-check --work-ids`.
- Use `JOBS=24` for targeted aozora2html reruns.
- The existing `ab-index` detector is the source-feature authority. Do not narrow its prevalence detector merely to make adapter numbers look better.

---

## Hammock Synthesis

The previous plan restored trustworthy measurement and found that aozora2html does emit warigaki and kunten at corpus scale. The unresolved question is narrower: why do some source-indexed works with clean AAT/check reports have no counted family observation?

Three facts shape the next move:

- The `ab-index` source detector intentionally recognizes real corpus spellings: warigaki `［＃割り注］` / `［＃ここから割り注］`, compact kaeriten such as `［＃レ］` and `［＃一］`, and parenthesized okurigana such as `［＃（ノ）］`.
- `adapters/aozora2html/src/source_derived.rs` currently supports narrower source-derived forms: warigaki `［＃割書］...［＃割書終わり］`, kaeriten `［＃返り点...］`, and okurigana `［＃訓点送り仮名「...」］`.
- The current audit counts kunten AAT observations via `style_type in {"kaeriten", "okurigana"}` or `meta.semantic_summary.syntax` keys beginning with `kunten.`, but the adapter's existing okurigana representation is a ruby node with `x-annotation-type = "okurigana"`. That is a measurement predicate gap before it is an adapter gap.

Sampled clean residuals show two classes that must be separated before patching:

- Adapter-obligation candidates: body text containing real marker spellings the adapter should emit, especially `［＃ここから割り注］...［＃ここで割り注終わり］`, compact `［＃割り注］...［＃割り注終わり］`, compact return marks, and parenthesized okurigana.
- Source-index-only candidates: title notes, bottom notes, notation examples, or other source lines that prove prevalence but may not be part of the adapter body contract.

Chosen direction: classify the clean bucket first, then fix measured adapter/observation gaps. Do not start the parser-IR CLI until regenerated reports show zero unknown clean emission gaps, or the remaining items are explicitly documented as accepted source-index-only caveats.

---

## Task 1: Fix The Kunten Observation Predicate

**Files:**
- Modify: `reports/aat-fidelity/audit-aozora2html-measurement.py`
- Modify: `reports/aat-fidelity/extract-aozora2html-policy-samples.py`
- Modify: `tests/aozora2html-measurement-audit-smoke.sh`
- Modify: `tests/aozora2html-policy-samples-smoke.sh`

**Contract:**
- Count AAT v1 okurigana observations when a node has `kind == "ruby"` and `x-annotation-type == "okurigana"`.
- Continue counting kaeriten through `style_type == "kaeriten"` and semantic summary keys beginning with `kunten.`.
- Fix the displayed `kunten_observations` metric so it includes semantic-only observations instead of duplicating only `kunten_nodes`.
- Do not add a new AAT node kind.

- [ ] **Step 1:** Add a shared local predicate in both Python scripts for kunten observations:

  ```python
  def is_kunten_node(node: dict[str, Any]) -> bool:
      return (
          node.get("style_type") in {"kaeriten", "okurigana"}
          or (
              node.get("kind") == "ruby"
              and node.get("x-annotation-type") == "okurigana"
          )
      )
  ```

  Use it anywhere the scripts currently test only `style_type in {"kaeriten", "okurigana"}`.

- [ ] **Step 2:** Correct `summarize_observations()` in the audit script.

  `kunten_observations` must be the sum of node-level kunten observations plus semantic summary observations. It must not duplicate `kunten_nodes` while omitting semantic-only `kunten.*` observations.

- [ ] **Step 3:** Extend the measurement audit smoke fixture with one source-feature `okurigana` work whose AAT has a ruby node with `x-annotation-type: "okurigana"`, and another work whose only kunten evidence is `meta.semantic_summary.syntax.kunten.okurigana`. Assert both land in `observed_in_aat`, not `source_feature_without_aat_observation`, and assert `kunten_observations` includes both.

- [ ] **Step 4:** Extend the policy samples smoke fixture so the same ruby okurigana representation is displayed as an observed kunten sample.

- [ ] **Step 5:** Verify:

  ```bash
  bash tests/aozora2html-measurement-audit-smoke.sh
  bash tests/aozora2html-policy-samples-smoke.sh
  python3 -m compileall reports/aat-fidelity
  ```

- [ ] **Step 6:** Commit:

  ```bash
  git add reports/aat-fidelity/audit-aozora2html-measurement.py \
    reports/aat-fidelity/extract-aozora2html-policy-samples.py \
    tests/aozora2html-measurement-audit-smoke.sh \
    tests/aozora2html-policy-samples-smoke.sh
  git commit -m "fix: count okurigana ruby as kunten observation"
  ```

---

## Task 2: Add A Source-Feature Gap Classifier

**Files:**
- Create: `reports/aat-fidelity/classify-aozora2html-source-feature-gaps.py`
- Create: `tests/aozora2html-source-feature-gap-classifier-smoke.sh`
- Create: `docs/superpowers/reports/2026-07-03-aozora2html-source-feature-gap-classification.md`
- Create: `docs/superpowers/reports/2026-07-03-aozora2html-source-feature-gap-classification.summary.json`
- Read: `docs/superpowers/reports/2026-07-03-aozora2html-policy-residual-triage.summary.json`
- Read: `/db/ab-validator/aat-corpus/aozora2html-full-20260703T020301Z/index.json`
- Read: `/db/ab-validator/aat-corpus/aozora2html-full-20260703T020301Z/residual-worksets/*source_feature_without_aat_observation.json`

**CLI:**

```bash
python3 reports/aat-fidelity/classify-aozora2html-source-feature-gaps.py \
  --run-dir /db/ab-validator/aat-corpus/aozora2html-full-20260703T020301Z \
  --residual-summary docs/superpowers/reports/2026-07-03-aozora2html-policy-residual-triage.summary.json \
  --out-md docs/superpowers/reports/2026-07-03-aozora2html-source-feature-gap-classification.md \
  --summary-json docs/superpowers/reports/2026-07-03-aozora2html-source-feature-gap-classification.summary.json \
  --worksets-dir /db/ab-validator/aat-corpus/aozora2html-full-20260703T020301Z/source-feature-gap-worksets
```

**Output summary JSON keys:**
- `run_dir`
- `residual_summary`
- `families`
- `marker_class_counts`
- `context_hint_counts`
- `works`
- `worksets`
- `verdict_inputs`

**Worksets to generate under `/db`, not git:**
- `warigaki-adapter-obligation-candidates.json`
- `kunten-adapter-obligation-candidates.json`
- `source-feature-gap-adapter-obligation-union.json`
- `source-feature-gap-unknown-union.json`
- `source-feature-gap-source-index-only-candidates.json`

- [ ] **Step 1:** Implement source reading that resolves `index.json.corpus_root` plus `txt_path`.

  `txt_path` can be a plain path or `archive.zip::member`. Decode with the same order as the adapter: UTF-8 BOM, UTF-8, then Windows-31J/CP932 with replacement recorded as `windows-31j-lossy` if needed.

- [ ] **Step 2:** For each work in the two `source_feature_without_aat_observation` worksets, load only the indexed feature lines from `work.feature_lines`.

  Do not commit source excerpts. Store only line numbers, marker-class labels, short marker strings, and context hints.

- [ ] **Step 3:** Classify marker forms deterministically.

  Required marker classes:

  - `warigaki.koko_start_end`: line contains `［＃ここから割り注］` or `［＃ここで割り注終わり］`.
  - `warigaki.compact_start_end`: line contains `［＃割り注］` or `［＃割り注終わり］`.
  - `warigaki.legacy_warigaki`: line contains `［＃割書］` or `［＃割書終わり］`.
  - `warigaki.with_source_line_break`: warigaki line also contains `［＃改行］`.
  - `kunten.kaeriten.compact`: line contains compact return marks matched by `data/feature-patterns.toml`, such as `［＃レ］`, `［＃一］`, `［＃二］`, `［＃上］`, `［＃中］`, or `［＃下］`.
  - `kunten.kaeriten.named`: line contains `［＃返り点...］`.
  - `kunten.okurigana.parenthesized`: line contains parenthesized okurigana/reread markers such as `［＃（ノ）］`.
  - `kunten.okurigana.named`: line contains `［＃訓点送り仮名「...」］`.
  - `unknown`: detector says the feature is present but the classifier cannot assign a marker class.

- [ ] **Step 4:** Add context hints without making policy decisions automatically.

  Required context hints:

  - `notation_example`: line contains `（例）` or `：返り点`.
  - `base_text_note`: line contains `題は底本では` or `底本では`.
  - `publication_or_editor_note`: line contains `初出`, `ファイル末`, or `入力`.
  - `body_candidate`: none of the above hints matched.

  A work can have multiple marker classes and multiple context hints.

- [ ] **Step 5:** Define candidate worksets.

  - `adapter-obligation-candidates`: works with at least one `body_candidate` line and no `unknown` marker class.
  - `source-index-only-candidates`: works with only non-body context hints and no `unknown` marker class.
  - `unknown`: any work with an `unknown` marker class or unreadable source line.

  These names are classifier outputs, not final product policy. The executor must not relabel source-index-only candidates as accepted caveats until Task 4 updates the ABC sync report with the explicit scope decision: body-only adapter obligation versus full-source loss accounting.

  The script exits non-zero if any generated workset is not a sorted unique JSON array of strings.

- [ ] **Step 6:** Add a smoke test.

  The smoke fixture must include:

  - a zipped source path with `［＃ここから割り注］...［＃改行］...［＃ここで割り注終わり］`,
  - a zipped source path with `［＃割り注］...［＃割り注終わり］`,
  - a compact kunten line with `［＃レ］` and `［＃一］`,
  - a parenthesized okurigana line with `［＃（ノ）］`,
  - a base-text note line containing `題は底本では`,
  - an intentionally unknown detector hit.

  Assert marker counts, context-hint counts, and all five workset paths.

- [ ] **Step 7:** Run the classifier on the real run and inspect the report.

  ```bash
  bash tests/aozora2html-source-feature-gap-classifier-smoke.sh
  python3 reports/aat-fidelity/classify-aozora2html-source-feature-gaps.py \
    --run-dir /db/ab-validator/aat-corpus/aozora2html-full-20260703T020301Z \
    --residual-summary docs/superpowers/reports/2026-07-03-aozora2html-policy-residual-triage.summary.json \
    --out-md docs/superpowers/reports/2026-07-03-aozora2html-source-feature-gap-classification.md \
    --summary-json docs/superpowers/reports/2026-07-03-aozora2html-source-feature-gap-classification.summary.json \
    --worksets-dir /db/ab-validator/aat-corpus/aozora2html-full-20260703T020301Z/source-feature-gap-worksets
  jq '.marker_class_counts, .context_hint_counts, .verdict_inputs' \
    docs/superpowers/reports/2026-07-03-aozora2html-source-feature-gap-classification.summary.json
  ```

- [ ] **Step 8:** Commit:

  ```bash
  git add reports/aat-fidelity/classify-aozora2html-source-feature-gaps.py \
    tests/aozora2html-source-feature-gap-classifier-smoke.sh \
    docs/superpowers/reports/2026-07-03-aozora2html-source-feature-gap-classification.md \
    docs/superpowers/reports/2026-07-03-aozora2html-source-feature-gap-classification.summary.json
  git commit -m "test: classify aozora2html source feature gaps"
  ```

---

## Task 3: Patch Adapter Marker Support Proven By The Classifier

**Files:**
- Modify: `adapters/aozora2html/src/source_derived.rs`
- Modify: `adapters/aozora2html/src/xhtml_mapper.rs`
- Modify: `adapters/aozora2html/tests/test_mapper.py`
- Possibly modify: `adapters/aozora2html/README.md`

**Gate before editing:** Only implement marker support for classes present in `adapter-obligation-candidates`. If the classifier says all clean residuals are source-index-only or unknown, skip adapter edits and go to Task 4.

- [ ] **Step 1:** Add failing adapter tests for the observed warigaki classes if present.

  Required cases when the classifier reports them as adapter-obligation candidates:

  - `米《べー》リンスキー［＃ここから割り注］魯国の批評家［＃ここで割り注終わり］` maps to a `kind: "warigaki"` node and records a warigaki semantic observation.
  - `［＃ここから割り注］上［＃改行］下［＃ここで割り注終わり］` maps to `kind: "warigaki"` with upper text `上`, lower text `下`, and `x-provenance: "source-derived"`.
  - `［＃割り注］注［＃割り注終わり］` maps to `kind: "warigaki"` with upper text `注`, empty lower, and `x-provenance: "source-derived"`.
  - Existing `［＃割書］...［＃割書終わり］` behavior remains unchanged.

- [ ] **Step 2:** Implement a single source-derived warigaki parser helper.

  Replace the narrow `warigaki_inline_re()` path with a helper that recognizes:

  - `［＃ここから割り注］...［＃ここで割り注終わり］`
  - `［＃割り注］...［＃割り注終わり］`
  - existing `［＃割書］...［＃割書終わり］`

  Split the inner content on the first `［＃改行］`, `／`, or `/` into upper/lower. Preserve remaining text before and after the marker as text nodes.

- [ ] **Step 3:** Add failing adapter tests for compact kunten classes if present.

  Required cases when the classifier reports them as adapter-obligation candidates:

  - `漢［＃レ］文` maps to a source-derived `style_type: "kaeriten"` node with `x-marker: "レ"` and semantic summary `kunten.kaeriten`.
  - `漢［＃二］文［＃一］` maps both compact return marks.
  - `一朶［＃（ノ）］妖紅` maps to the existing AAT v1 okurigana representation: a ruby node with `base: ""`, `reading: "ノ"`, `x-annotation-type: "okurigana"`, and provenance `source-derived`; also add `kunten.okurigana` semantic summary so future measurements do not depend only on node walking.
  - If the upstream XHTML renders parenthesized okurigana as `<sup>...</sup>`, that XHTML path must map to the same AAT v1 okurigana representation or at least the same `kunten.okurigana` semantic summary; it must not remain an `unmapped-sup` warning.

- [ ] **Step 4:** Implement compact kunten parser helpers without adding a new AAT node kind.

  - Extend kaeriten matching from `［＃返り点...］` to the compact forms already recognized by `data/feature-patterns.toml`.
  - Extend okurigana matching from `［＃訓点送り仮名「...」］` to parenthesized kana forms such as `［＃（ノ）］`.
  - In `xhtml_mapper.rs`, handle `<sup>` okurigana output if the classifier samples or targeted XHTML inspection show that aozora2html emits compact okurigana that way.
  - Add semantic summary entries:
    - `kunten.kaeriten` for return marks.
    - `kunten.okurigana` for okurigana/reread markers.

- [ ] **Step 5:** Verify adapter behavior.

  ```bash
  cd adapters/aozora2html
  cargo test
  python3 -m pytest tests/test_mapper.py
  cd ../..
  ```

- [ ] **Step 6:** Commit:

  ```bash
  git add adapters/aozora2html/src/source_derived.rs \
    adapters/aozora2html/src/xhtml_mapper.rs \
    adapters/aozora2html/tests/test_mapper.py \
    adapters/aozora2html/README.md
  git commit -m "fix: map real aozora2html warigaki and kunten markers"
  ```

---

## Task 4: Targeted Remeasurement And Report Regeneration

**Files:**
- Modify: `docs/superpowers/reports/2026-07-03-aozora2html-measurement-trust.md`
- Modify: `docs/superpowers/reports/2026-07-03-aozora2html-measurement-trust.summary.json`
- Modify: `docs/superpowers/reports/2026-07-03-aozora2html-policy-samples.md`
- Modify: `docs/superpowers/reports/2026-07-03-aozora2html-policy-samples.summary.json`
- Modify: `docs/superpowers/reports/2026-07-03-aozora2html-policy-residual-triage.md`
- Modify: `docs/superpowers/reports/2026-07-03-aozora2html-policy-residual-triage.summary.json`
- Modify: `docs/superpowers/reports/2026-07-03-post-measurement-abc-sync.md`

- [ ] **Step 1:** Build the targeted workset.

  Use the classifier's `source-feature-gap-adapter-obligation-union.json` if it is non-empty. If Task 3 was skipped, use `source-feature-gap-unknown-union.json` only when there are unknowns that need measurement confirmation. Do not rerun the full corpus here.

  If both candidate worksets are empty after Task 1 and Task 2, skip Step 2 and regenerate reports against the baseline plus the existing retry run `/db/ab-validator/aat-corpus/aozora2html-policy-retry-20260703T055526Z`. This branch is valid only when the classifier report explains that every clean gap is source-index-only and the ABC sync report records that scope decision.

- [ ] **Step 2:** Run a targeted aozora2html retry at 24 jobs.

  Run this only when the chosen workset is non-empty.

  ```bash
  retry_dir="/db/ab-validator/aat-corpus/aozora2html-source-feature-gap-$(date -u +%Y%m%dT%H%M%SZ)"
  workset="/db/ab-validator/aat-corpus/aozora2html-full-20260703T020301Z/source-feature-gap-worksets/source-feature-gap-adapter-obligation-union.json"
  just aozora2html-aat-full \
    DIR="$retry_dir" \
    JOBS=24 \
    TIMEOUT=600s \
    REPORT_ID=aozora2html-source-feature-gap-2026-07-03 \
    WORK_IDS="$workset" \
    FEATURES=""
  ```

- [ ] **Step 3:** Regenerate the measurement audit with the new retry run.

  Set `retry_dir` to the directory created in Step 2. If Step 2 was skipped because there was no non-empty targeted workset, set it to `/db/ab-validator/aat-corpus/aozora2html-policy-retry-20260703T055526Z`.

  ```bash
  python3 reports/aat-fidelity/audit-aozora2html-measurement.py \
    --run-dir /db/ab-validator/aat-corpus/aozora2html-full-20260703T020301Z \
    --retry-run-dir "$retry_dir" \
    --out-md docs/superpowers/reports/2026-07-03-aozora2html-measurement-trust.md \
    --summary-json docs/superpowers/reports/2026-07-03-aozora2html-measurement-trust.summary.json \
    --worksets-dir /db/ab-validator/aat-corpus/aozora2html-full-20260703T020301Z/worksets
  ```

- [ ] **Step 4:** Regenerate policy samples and residual triage.

  ```bash
  python3 reports/aat-fidelity/extract-aozora2html-policy-samples.py \
    --run-dir /db/ab-validator/aat-corpus/aozora2html-full-20260703T020301Z \
    --audit-summary-json docs/superpowers/reports/2026-07-03-aozora2html-measurement-trust.summary.json \
    --out-md docs/superpowers/reports/2026-07-03-aozora2html-policy-samples.md \
    --summary-json docs/superpowers/reports/2026-07-03-aozora2html-policy-samples.summary.json \
    --worksets-dir /db/ab-validator/aat-corpus/aozora2html-full-20260703T020301Z/worksets \
    --limit-per-bucket 10

  python3 reports/aat-fidelity/triage-aozora2html-policy-residuals.py \
    --run-dir /db/ab-validator/aat-corpus/aozora2html-full-20260703T020301Z \
    --audit-summary-json docs/superpowers/reports/2026-07-03-aozora2html-measurement-trust.summary.json \
    --out-md docs/superpowers/reports/2026-07-03-aozora2html-policy-residual-triage.md \
    --summary-json docs/superpowers/reports/2026-07-03-aozora2html-policy-residual-triage.summary.json \
    --worksets-dir /db/ab-validator/aat-corpus/aozora2html-full-20260703T020301Z/residual-worksets \
    --limit-per-bucket 15
  ```

- [ ] **Step 5:** Update the ABC sync report.

  `docs/superpowers/reports/2026-07-03-post-measurement-abc-sync.md` must include:

  - the new targeted retry directory and workset path,
  - before/after counts for `source_feature_without_aat_observation`,
  - marker-classification counts from Task 2,
  - any accepted source-index-only caveat counts,
  - whether unknown clean gaps remain,
  - the unchanged status of `report_failed_other_property`, `parse_incomplete`, and timeout/protocol buckets.
  - the explicit scope decision for source-index-only candidates: either they are accepted as outside the body adapter obligation, or they become full-source loss-accounting work before the CLI can proceed.

- [ ] **Step 6:** Apply the gate.

  Set the sync verdict to:

  - `CLI_READY_WITH_LOWER_BOUND_CAVEAT` only if all clean adapter-obligation candidates now emit observations, all source-index-only candidates are explicitly listed, and there are zero unknown clean gaps.
  - `MEASUREMENT_BLOCKED` if any clean adapter-obligation or unknown clean gap remains.

  This plan does not automatically override the still-open `report_failed_other_property` and incomplete buckets. If they remain policy-blocking, name them as the next plan's first tasks.

- [ ] **Step 7:** Verify:

  ```bash
  bash tests/aozora2html-measurement-audit-smoke.sh
  bash tests/aozora2html-policy-samples-smoke.sh
  bash tests/aozora2html-policy-residual-triage-smoke.sh
  bash tests/aozora2html-source-feature-gap-classifier-smoke.sh
  python3 -m compileall reports/aat-fidelity
  jq '.bucket_counts.warigaki.source_feature_without_aat_observation, .bucket_counts.kunten.source_feature_without_aat_observation' \
    docs/superpowers/reports/2026-07-03-aozora2html-policy-residual-triage.summary.json
  rg -n "Verdict|source_feature_without_aat_observation|source-index-only|unknown clean gaps" \
    docs/superpowers/reports/2026-07-03-post-measurement-abc-sync.md
  ```

- [ ] **Step 8:** Commit:

  ```bash
  git add docs/superpowers/reports/2026-07-03-aozora2html-measurement-trust.md \
    docs/superpowers/reports/2026-07-03-aozora2html-measurement-trust.summary.json \
    docs/superpowers/reports/2026-07-03-aozora2html-policy-samples.md \
    docs/superpowers/reports/2026-07-03-aozora2html-policy-samples.summary.json \
    docs/superpowers/reports/2026-07-03-aozora2html-policy-residual-triage.md \
    docs/superpowers/reports/2026-07-03-aozora2html-policy-residual-triage.summary.json \
    docs/superpowers/reports/2026-07-03-post-measurement-abc-sync.md
  git commit -m "docs: update aozora2html source feature gate"
  ```

---

## Task 5: Final Verification And Integration

- [ ] **Step 1:** Run focused verification.

  ```bash
  cargo test -p ab-index
  cargo test
  bash tests/aozora2html-measurement-audit-smoke.sh
  bash tests/aozora2html-policy-samples-smoke.sh
  bash tests/aozora2html-policy-residual-triage-smoke.sh
  bash tests/aozora2html-source-feature-gap-classifier-smoke.sh
  python3 -m compileall reports/aat-fidelity
  nix build .#checks.$(nix eval --impure --raw --expr builtins.currentSystem).taxonomy-drift
  git diff --check
  ```

- [ ] **Step 2:** Run broad verification if time allows.

  ```bash
  nix flake check --print-build-logs
  ```

  If this still fails on the known `/db` sandbox access issue in `aat-oracle-data-schema-smoke`, record it as existing residual risk rather than hiding it.

- [ ] **Step 3:** Review the final gate.

  Do not claim the parser-IR CLI is unblocked unless the regenerated ABC sync report says so. If the verdict remains `MEASUREMENT_BLOCKED`, the next overall plan is:

  1. characterize `report_failed_other_property` against visible-text oracle behavior,
  2. triage timeout/protocol and parse-incomplete buckets,
  3. regenerate the ABC sync gate again,
  4. only then start the owned AAT-to-parser-IR CLI.

- [ ] **Step 4:** Merge if requested after verification passes.

  ```bash
  git status --short
  git log --oneline --decorate -5
  ```

---

## Success Criteria

- The clean `source_feature_without_aat_observation` bucket is no longer an uncharacterized blocker.
- The measurement scripts count all current AAT v1 kunten representations, including ruby okurigana.
- Real marker spellings confirmed as adapter obligations have regression tests and targeted corpus evidence.
- Source-index-only candidates are listed as caveats, not silently counted as adapter failures.
- The ABC sync report gives a clear verdict and names the remaining blockers, if any.
