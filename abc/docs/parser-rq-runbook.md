# Parser-RQ Operator Runbook

Two executable recipes for the retained qualification capability
(ADRs 0039–0042; see
`docs/superpowers/reports/2026-07-24-parser-rq-disposition.md`):
the historical audit of the accepted P5 campaign, and a clean-host
requalification. Neither recipe edits evidence; the campaign store is
append-only and content-addressed.

## Recipe 1 — Historical audit (minutes, CI-pinned)

Verifies the accepted P5 promotion end to end — closed capture membership,
blob hashes, canonical projections, registry binding, and the `:accepted`
status of the dependency decisions in `docs/adr/decisions.edn` — without
re-running any measurement.

From `abc/`:

```bash
candidate_ref=sha256:15affdfb677cc6a94a4a5364da68ca2d11441f899737651e727dbac90eddc5ab
run_root="docs/reports/parser-rq/runs/${candidate_ref#sha256:}"
nix develop -c clojure -M:abc/parser-rq-campaign verify-promotion \
  --runs-root docs/reports/parser-rq/runs \
  --candidate-ref "$candidate_ref" \
  --registry data/aat-parser-ir-compatibility.edn \
  --measurements docs/reports/parser-release-qualification-measurements.edn \
  --report docs/reports/parser-release-qualification-report.json \
  --provenance "$run_root/executable-provenance.json" \
  --decisions docs/adr/decisions.edn
```

Expected output: `ok`, exit 0. Any other output lists the exact
verification failures; do not alter evidence in response — investigate.

The same invocation is pinned in CI as the
`parser-rq-p5-promotion-audit` flake check:

```bash
nix build ./abc#checks.x86_64-linux.parser-rq-p5-promotion-audit --no-link
```

## Recipe 2 — Clean-host requalification

Status: **host-exercised 2026-07-25** (32-core Linux, cgroup v2). The
sequence below is the corrected one: it ran end to end to
`verify-promotion` → `ok` against the pinned qualification corpus,
producing `candidate_ref`
`sha256:ae9a97bf7584581e56d89954686c088132f364fd783a37fcc39de2c457c55d4e`
with `gate_status: release-qualified` (9/9 predicates `pass`). The
pre-exercise draft of this recipe could not be followed literally; the
corrections it required are marked **[corrected]** below and are the
reason the step list is longer than the draft's nine steps. No new
orchestration was added — only the missing steps were written down.

Prerequisites: Linux with cgroup v2 (predicate 8 measures process-tree
memory via cgroups), Nix with flakes, a **clean** checkout of this
monorepo (a `git worktree` is the easy way — the readiness receipt records
`candidate_tree_clean: true` as a fact, so asserting it on a dirty tree
falsifies the receipt), and a writable runtime root outside the
repository. Run everything from `abc/` inside `nix develop`.

`$work` is a directory holding the campaign value files. **[corrected]**
It must be the directory you later pass as `--evidence-tree`: the
orchestrator requires `candidate.edn`, `authorization.edn`,
`executable-provenance.json`, and `readiness-receipt.json` to resolve
below that root (`_below` in `tools/parser_rq_campaign_orchestrator.py`).
`--evidence-tree` is **not** the repository checkout; pointing it at the
checkout fails with `candidate is not below its authenticated tree`.

**[corrected]** Two site-descriptor fields do not do what their names
suggest on this path. `capture-core` resolves the corpus as
`{candidate_tree}/ab-validator/crates/ab-index/tests/fixtures/corpus`
(hardcoded), and the capture writes its output under `--staging-root`.
The descriptor's `corpus_root` is authenticated by preflight only as "an
absolute, existing directory" — an *empty* directory passes — and
`evidence_store_root` is left untouched by the capture. Do not expect
either to select or receive evidence. Consequently a full-corpus
requalification is **not** reachable by repointing the descriptor; it
requires changing that hardcoded corpus root.

1. **Describe and preflight the site.** Copy
   `config/parser-rq-site.example.json` to `$work/site.json` and point
   `corpus_root`, `evidence_store_root`, `scratch_root`, and
   `campaign_lock_path` at the runtime root (committed manifests never
   contain these locators — ADR 0042):

   ```bash
   python3 tools/parser_rq_campaign_site.py preflight-site \
     --site-descriptor "$work/site.json" \
     --graph data/parser-rq-production-graph-v1.json \
     --evidence-tree-clean true
   ```

2. **Realize the candidate twice and prove reproducibility.**
   **[corrected — this step was absent.]** Nothing else produces the
   provenance that step 3 consumes. `realize-build` defaults to
   `ab-validator#packages.x86_64-linux.parser-rq-candidate`. Note that
   `--store-root` realizes into an *isolated local* store, which is why
   the extra daemon-store build below is needed before `verify-installed`
   (otherwise it reports `realized output is absent`).

   ```bash
   prov=../ab-validator/reports/parser-ir/parser-rq-campaign-provenance.py
   parser_rev=$(git rev-parse HEAD)      # must be a full 40-hex sha

   for b in a b; do
     python3 "$prov" realize-build \
       --candidate-tree "$candidate_tree" --build-id "build-$b" \
       --store-root "$work/store-$b" --build-log "$work/build-$b.log" \
       --out "$work/realization-$b.json"
     python3 "$prov" capture-build \
       --realization "$work/realization-$b.json" \
       --graph data/parser-rq-production-graph-v1.json \
       --parser-git-rev "$parser_rev" --out "$work/build-$b.json"
   done

   # The UNBOUND proof. compare-builds fails unless both builds agree.
   python3 "$prov" compare-builds \
     --first "$work/build-a.json" --second "$work/build-b.json" \
     --out "$work/provenance-proof.json"

   # verify-installed queries the daemon store, so realize there too.
   nix build ./../ab-validator#packages.x86_64-linux.parser-rq-candidate --no-link
   python3 "$prov" verify-installed --proof "$work/provenance-proof.json"
   ```

   **[corrected]** There are two distinct provenance artifacts and the
   draft recipe gave both the same filename. `provenance-proof.json` is
   the *unbound* `compare-builds` output and is the input to step 3.
   `executable-provenance.json` is the *bound* envelope produced in step 4
   and is what every later step wants. Feeding the unbound proof to
   `verify-authorization-record` fails with `bound provenance envelope is
   invalid`.

3. **Build the candidate.** Derives identity from the pinned corpus and
   predicate data plus the candidate executable's authenticated
   provenance. Rotating candidate/corpus/predicate/instrument identity is
   a data change plus a new candidate — never an edit to old observations
   (ADR 0040 c3, ADR 0041 c1):

   ```bash
   clojure -M:abc/parser-rq-campaign candidate \
     --repo .. --parser-git-rev "$parser_rev" \
     --provenance "$work/provenance-proof.json" \
     --out "$work/candidate.edn"          # prints candidate_ref
   ```

   Read `qualification_identity_ref` from `$work/candidate.edn` and
   `provenance_core_ref` from `$work/provenance-proof.json`; step 5 needs
   both. `corpus_snapshot_hash` / `corpus_list_hash` come from
   `data/parser-release-qualification-corpus.edn`.

4. **Bind the provenance to the candidate.** **[corrected — this step was
   absent.]** It must run *after* step 3, because it consumes the two refs
   step 3 derives.

   ```bash
   python3 "$prov" bind-provenance \
     --proof "$work/provenance-proof.json" \
     --candidate-ref "$candidate_ref" \
     --qualification-identity-ref "$qual_ref" \
     --out "$work/executable-provenance.json"
   ```

5. **Seal readiness and authorize a capture window.** The authorization's
   `repetitions` is fixed at 3 and `reduction` at `maximum`
   (`build-authorization`); `authorization-record-errors` rejects any
   other value, so there is no single-repetition option. Three
   repetitions exist for the *environmental* observations — wall time,
   timeouts, and peak cgroup memory are not byte-determined and are
   reduced by `max` as a conservative bound — not for the hermetic ones.

   ```bash
   python3 tools/parser_rq_campaign_site.py seal-readiness \
     --site-descriptor "$work/site.json" \
     --graph data/parser-rq-production-graph-v1.json \
     --candidate-ref "$candidate_ref" \
     --qualification-identity-ref "$qual_ref" \
     --provenance-core-ref "$prov_core_ref" \
     --candidate-git-rev "$parser_rev" \
     --evidence-base-git-rev "$(git rev-parse HEAD)" \
     --corpus-snapshot-hash "$corpus_snapshot_hash" \
     --corpus-list-hash "$corpus_list_hash" \
     --candidate-tree-clean true --evidence-tree-clean true \
     --out "$work/readiness-receipt.json"

   clojure -M:abc/parser-rq-campaign authorize \
     --candidate "$work/candidate.edn" \
     --receipt "$work/readiness-receipt.json" \
     --ordinal 1 \
     --not-before "$not_before_utc" --not-after "$not_after_utc" \
     --out "$work/authorization.edn"      # prints authorization_ref

   clojure -M:abc/parser-rq-campaign verify-authorization-record \
     --candidate "$work/candidate.edn" \
     --provenance "$work/executable-provenance.json" \
     --graph data/parser-rq-production-graph-v1.json \
     --receipt "$work/readiness-receipt.json" \
     --authorization "$work/authorization.edn"

   clojure -M:abc/parser-rq-campaign runtime-inputs \
     --candidate "$work/candidate.edn" \
     --authorization "$work/authorization.edn" \
     --out "$work/runtime-inputs.json"
   ```

6. **Capture.** The orchestrator authenticates the fixed production graph
   and executes capture under the site descriptor. Wall time is dominated
   by this step (the authorization's `repetitions` over the pinned
   corpus); on the 3-work pinned corpus the 2026-07-25 exercise took 51
   seconds.

   ```bash
   bin/parser-rq-campaign-capture.sh \
     --candidate "$work/candidate.edn" \
     --authorization "$work/authorization.edn" \
     --provenance "$work/executable-provenance.json" \
     --readiness-receipt "$work/readiness-receipt.json" \
     --site-descriptor "$work/site.json" \
     --candidate-tree "$candidate_tree" \
     --evidence-tree "$work" \
     --staging-root "$staging_root" \
     --production
   ```

   **[corrected]** `--evidence-tree` is `$work` (see the preamble), not
   the checkout. The capture writes its whole generation under
   `--staging-root`: the seven member files (`core_attempt.json`,
   `source_recognition.json`, `diagnostic_gap.json`,
   `diagnostic_completeness.json`, `parser_ir_conformance.json`,
   `publication_structure.json`, `resource.json`), `measurements.json`,
   and `capture-start.json`. So `$capture_root` in the next step **is**
   `$staging_root`, and `$capture_started_at_utc` is the
   `capture_started_at_utc` field of `$staging_root/capture-start.json`.

7. **Compose the capture generation and verify it.** For a freshly
   captured (index-less) root, `compose` requires the authorization and
   the capture start time and writes the canonical measurements:

   ```bash
   clojure -M:abc/parser-rq-campaign compose \
     --candidate "$work/candidate.edn" \
     --capture-root "$capture_root" \
     --authorization "$work/authorization.edn" \
     --capture-started-at "$capture_started_at_utc" \
     --out "$work/measurements.json"

   clojure -M:abc/parser-rq-campaign verify-capture \
     --candidate "$work/candidate.edn" \
     --authorization "$work/authorization.edn" \
     --capture-root "$capture_root"
   ```

8. **Authenticate every logical blob and publish the receipt.**
   **[corrected — this step was absent, and `verify-promotion` fails
   without it]** (`evidence-integrity-receipt.json (No such file or
   directory)`). `blobs.json` is just the capture index's member values:

   ```bash
   capture_ref=$(clojure -M:abc/parser-rq-campaign capture-ref \
     --capture-index "$capture_root/capture-index.edn")

   CAPTURE_INDEX="$capture_root/capture-index.edn" \
   BLOBS_OUT="$capture_root/blobs.json" \
   clojure -M -e \
     '(require (quote [clojure.edn :as edn]) (quote [abc.tools.json :as json]))
      (json/write-deterministic-json-file!
       (System/getenv "BLOBS_OUT")
       (vec (vals (:members (edn/read-string (slurp (System/getenv "CAPTURE_INDEX")))))))'

   python3 "$prov" verify-evidence \
     --blobs "$capture_root/blobs.json" --evidence-root "$capture_root" \
     --candidate-ref "$candidate_ref" --capture-generation-ref "$capture_ref" \
     --out "$capture_root/evidence-integrity-receipt.json"
   ```

9. **Produce the admission candidate and admit into the registry.**
   **[corrected]** `--candidates` is read as **EDN**, not JSON, and
   nothing in the repository produces an `admission-candidates` file — the
   draft recipe's `$work/admission-candidates.json` had no producer. Build
   it as `{:entries [<entry>]}` where the entry carries the candidate's
   identity fields plus an `:evidence_scope` of
   `:evidence_type :conversion-audit`. If the identity tuple
   (`aat_adapter_version` + `mapping_hash` + `parser_ir_schema_hash`) is
   already in the registry, that admitted entry *is* the candidate and no
   append is needed — which was the case in the 2026-07-25 exercise, so
   `--append-out` went unused:

   ```bash
   clojure -M:abc/aat-compat-admission -- \
     --candidates "$work/admission-candidates.edn" \
     --registry data/aat-parser-ir-compatibility.edn \
     --append-out data/aat-parser-ir-compatibility.edn
   ```

10. **Evaluate and publish the evaluation generation.** **[corrected]**
    `--out` must **not** already exist (`evaluation output root already
    exists`), and `--candidate-root` is `$runs_root/${candidate_ref#sha256:}`.

    ```bash
    clojure -M:abc/parser-rq-campaign evaluate \
      --candidate "$work/candidate.edn" \
      --capture-root "$capture_root" \
      --registry data/aat-parser-ir-compatibility.edn \
      --admission-candidate "$work/admission-candidates.edn" \
      --out "$evaluation_root"             # prints evaluation_generation_ref

    clojure -M:abc/parser-rq-campaign publish-evaluation \
      --candidate-root "$candidate_root" \
      --evaluation-root "$evaluation_root"
    ```

11. **Assemble the run root.** **[corrected — this step was absent.]**
    `publish-evaluation` places only the evaluation. `project` and
    `verify-promotion` read a run root laid out exactly as the committed
    P5 run under `docs/reports/parser-rq/runs/<ref>/`, so place the rest
    by hand (`project` otherwise fails on a missing `candidate.edn`):

    ```bash
    run_root="$runs_root/${candidate_ref#sha256:}"
    cp "$work/candidate.edn" "$work/readiness-receipt.json" \
       "$work/executable-provenance.json" "$run_root/"
    mkdir -p "$run_root/authorizations" "$run_root/captures"
    cp "$work/authorization.edn" \
       "$run_root/authorizations/${authorization_ref#sha256:}.edn"
    cp -r "$capture_root" "$run_root/captures/${capture_ref#sha256:}"
    ```

12. **Project the canonical measurements and report.**

    ```bash
    clojure -M:abc/parser-rq-campaign project \
      --runs-root "$runs_root" --candidate-ref "$candidate_ref" \
      --registry data/aat-parser-ir-compatibility.edn \
      --measurements-out "$work/qualification-measurements.edn" \
      --report-out "$work/qualification-report.json"
    ```

13. **Promote.** Run Recipe 1's `verify-promotion` against the new
    candidate's coordinates (same flags, new paths, plus
    `--decisions docs/adr/decisions.edn`). Promotion requires every
    predicate verdict `pass`, an admitted build, the closed
    evidence-integrity receipt, and `:accepted`, shape-valid dependency
    decisions. Note it authenticates that the *dependency* decisions are
    accepted; it does not require a decision binding this new
    `candidate_ref`. A qualification decision for the new scope is then
    recorded in `docs/adr/decisions.edn` per the governance workflow —
    the gate never edits decisions itself.

### What the 2026-07-25 exercise measured

All nine predicates passed (`gate_status: release-qualified`,
`verdict_tally {"pass": 9}`): `fatal-failures` 0.0, `source-span-coverage`
1.0, `silent-drops` 0, `diagnostic-completeness` 1.0,
`parser-ir-schema-validation` 1.0, `publication-structure` 1.0,
`wall-time` 0.06s (≤300), `memory` 10,764,288 B (≤2 GiB),
`timeout-policy` 0.0.

Two honest caveats about what that verdict does and does not establish:

- **`diagnostic-completeness` passed vacuously.** Its own details record
  `"vacuous": true, "diagnostic_count": 0` — the 3-work pinned corpus
  emits no diagnostics at all, so the predicate is not exercised. Any
  claim that this dimension is *verified* needs a corpus that produces
  diagnostics.
- **The `parser-rq-candidate` bundle reproduces.** `build-a`, `build-b`,
  and an independent daemon-store build all produced `output_ref`
  `sha256:8794b0dfbaa22fd73b4167d2fbb8dab898978fe9c3a95e4ff26d41b38a3849e6`.
  This retires the drift recorded in
  `docs/superpowers/reports/2026-07-24-publication-surface-disposition.md`
  §13. The bundle's `ab-aat-to-parser-ir`
  (`sha256:a2656fc9…`) equals the hash §13 recorded as P5's expected
  value; its `ab-aozora` (`sha256:55b9c5bf…`) legitimately differs from
  P5's `sha256:482728ca…` because the parser's bytes changed after P5
  (git-rev pin, dropped `abc` embed), which is why this run mints a new
  candidate identity instead of relabelling the old one.

Bounded end-to-end coverage of this sequence (candidate → authorize →
capture → compose → evaluate → promote, on a small fixture) runs in CI as
the `parser-rq-campaign-site`, `parser-rq-campaign-orchestrator`, and
`parser-rq-admission-promotion-smoke` checks and the root
`parser-rq-production-wiring` check.
