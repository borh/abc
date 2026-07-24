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

Prerequisites: Linux with cgroup v2 (predicate 8 measures process-tree
memory via cgroups), Nix with flakes, a checkout of this monorepo, and a
writable runtime root outside the repository.

1. **Describe the site.** Copy `config/parser-rq-site.example.json` and
   point `corpus_root`, `evidence_store_root`, `scratch_root`, and
   `campaign_lock_path` at the runtime root. Committed manifests never
   contain these locators (ADR 0042 portability).

2. **Build the candidate identity.** The campaign CLI derives the
   candidate from the pinned corpus, predicate set, and the candidate
   executable's authenticated provenance:

   ```bash
   clojure -M:abc/parser-rq-campaign <candidate-ref|qualification-identity-ref|authorization-ref|capture-ref|evaluate-ref> --<kind> PATH
   ```

   Predicate thresholds come from
   `data/parser-release-qualification-predicates.edn` and the corpus from
   `data/parser-release-qualification-corpus.edn`. Rotating candidate,
   corpus, predicate, or instrument identity is a data change to those
   files plus a new candidate — never an edit to old observations
   (ADR 0040 c3, ADR 0041 c1).

3. **Authorize and capture.** The orchestrator authenticates the fixed
   production graph and executes capture under the site descriptor:

   ```bash
   bin/parser-rq-campaign-capture.sh \
     --candidate CANDIDATE --authorization AUTHORIZATION \
     --provenance PROVENANCE --readiness-receipt RECEIPT \
     --site-descriptor SITE.json \
     --candidate-tree DIR --evidence-tree DIR --staging-root DIR \
     --production
   ```

   Wall time is dominated by this step: three repetitions of the full
   pinned corpus per the authorization's `repetitions`.

4. **Compose, verify, evaluate, project.**

   ```bash
   clojure -M:abc/parser-rq-campaign compose  --candidate PATH --capture-root DIR [--out PATH]
   clojure -M:abc/parser-rq-campaign verify-capture --candidate PATH --capture-root DIR
   clojure -M:abc/parser-rq-campaign project --runs-root DIR --candidate-ref HASH --registry PATH \
     [--measurements-out PATH] [--report-out PATH]
   ```

5. **Promote.** Run Recipe 1's `verify-promotion` against the new
   candidate's coordinates. Promotion requires every predicate verdict
   `pass`, an admitted build, the closed evidence-integrity receipt, and
   `:accepted` dependency decisions. A qualification decision for the new
   scope is then recorded in `docs/adr/decisions.edn` per the governance
   workflow — the gate never edits decisions itself.

Bounded end-to-end coverage of this sequence (candidate → authorize →
capture → compose → evaluate → promote, on a small fixture) runs in CI as
the `parser-rq-campaign-site`, `parser-rq-campaign-orchestrator`, and
`parser-rq-admission-promotion-smoke` checks and the root
`parser-rq-production-wiring` check.
