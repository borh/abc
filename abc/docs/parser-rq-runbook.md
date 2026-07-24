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

Status: **documented, awaiting host exercise.** Every command below is
exact (flags per `parser_rq_campaign.clj` and
`tools/parser_rq_campaign_site.py`); the sequence mirrors the CI-proven
orchestrator smoke. First host exercise should correct any residual
friction here rather than grow new orchestration.

Prerequisites: Linux with cgroup v2 (predicate 8 measures process-tree
memory via cgroups), Nix with flakes, a checkout of this monorepo, and a
writable runtime root outside the repository. Run everything from `abc/`
inside `nix develop`. `$work` below is a scratch directory for campaign
values; the capture/evaluation stores live under the site's
`evidence_store_root`.

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

2. **Build the candidate.** Derives identity from the pinned corpus and
   predicate data plus the candidate executable's authenticated
   provenance. Rotating candidate/corpus/predicate/instrument identity is
   a data change plus a new candidate — never an edit to old observations
   (ADR 0040 c3, ADR 0041 c1):

   ```bash
   clojure -M:abc/parser-rq-campaign candidate \
     --repo .. --parser-git-rev "$parser_rev" \
     --provenance "$work/executable-provenance.json" \
     --out "$work/candidate.edn"          # prints candidate_ref
   ```

3. **Seal readiness and authorize a capture window.**

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

4. **Capture.** The orchestrator authenticates the fixed production graph
   and executes capture under the site descriptor. Wall time is dominated
   by this step (the authorization's `repetitions` over the pinned
   corpus):

   ```bash
   bin/parser-rq-campaign-capture.sh \
     --candidate "$work/candidate.edn" \
     --authorization "$work/authorization.edn" \
     --provenance "$work/executable-provenance.json" \
     --readiness-receipt "$work/readiness-receipt.json" \
     --site-descriptor "$work/site.json" \
     --candidate-tree "$candidate_tree" \
     --evidence-tree "$evidence_tree" \
     --staging-root "$staging_root" \
     --production
   ```

5. **Compose the capture generation and verify it.** For a freshly
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

6. **Produce the admission candidate and admit into the registry.**

   ```bash
   clojure -M:abc/aat-compat-admission -- \
     --candidates "$work/admission-candidates.json" \
     --registry data/aat-parser-ir-compatibility.edn \
     --append-out data/aat-parser-ir-compatibility.edn
   ```

7. **Evaluate and publish the evaluation generation.**

   ```bash
   clojure -M:abc/parser-rq-campaign evaluate \
     --candidate "$work/candidate.edn" \
     --capture-root "$capture_root" \
     --registry data/aat-parser-ir-compatibility.edn \
     --admission-candidate "$work/admission-candidates.json" \
     --out "$evaluation_root"             # prints evaluation_generation_ref

   clojure -M:abc/parser-rq-campaign publish-evaluation \
     --candidate-root "$candidate_root" \
     --evaluation-root "$evaluation_root"
   ```

8. **Project the canonical measurements and report.**

   ```bash
   clojure -M:abc/parser-rq-campaign project \
     --runs-root "$runs_root" --candidate-ref "$candidate_ref" \
     --registry data/aat-parser-ir-compatibility.edn \
     --measurements-out "$work/qualification-measurements.edn" \
     --report-out "$work/qualification-report.json"
   ```

9. **Promote.** Run Recipe 1's `verify-promotion` against the new
   candidate's coordinates (same flags, new paths, plus
   `--decisions docs/adr/decisions.edn`). Promotion requires every
   predicate verdict `pass`, an admitted build, the closed
   evidence-integrity receipt, and `:accepted`, shape-valid dependency
   decisions. A qualification decision for the new scope is then recorded
   in `docs/adr/decisions.edn` per the governance workflow — the gate
   never edits decisions itself.

Bounded end-to-end coverage of this sequence (candidate → authorize →
capture → compose → evaluate → promote, on a small fixture) runs in CI as
the `parser-rq-campaign-site`, `parser-rq-campaign-orchestrator`, and
`parser-rq-admission-promotion-smoke` checks and the root
`parser-rq-production-wiring` check.
