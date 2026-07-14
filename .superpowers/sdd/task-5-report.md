# Task 5 report — Measure the custom parser on shared instruments

Status: **DONE**

Real ab-aozora measurement was folded into the report generator's appendix.
The corpus + vector runs were executed on hinoki over the identical pinned
instruments; nothing was fabricated. Every existing-parser row and metric
definition stays byte-identical, and the frozen preregistration and
`run-manifests.json` are untouched.

## Commits (branch `feat/parser-release-qualification`)

- `5677815f` `data(parser-study): freeze ab-aozora shared-instrument appendix run manifest`
- `8f2e852d` `docs(parser): add custom-parser comparison baselines` (main)
- (this report committed separately: `docs(sdd): record task 5 report`)

## What was measured, and at which revision

- Custom parser = `ab-aozora`, **native-only** (`--mode aat` emits parser-IR/AAT
  on stdin→stdout; no adapter lane). CLI confirmed identical at HEAD and at the
  pinned baseline.
- Measured the **pinned baseline revision `ac2be926`**, not HEAD: the parser
  crates changed 1600+ lines since `ac2be926`, so building HEAD and labelling it
  `ac2be926` would misattribute behavior. `.#ab-aozora` exists at `ac2be926` and
  `AB_AOZORA_GIT_REV = self.rev` bakes the rev into `--version`, so the built
  binary self-identifies as the baseline.

Build (hinoki):
```
nix build --no-link --print-out-paths \
  "git+file:///home/bor/Projects/soranoha?dir=ab-validator&rev=ac2be926738f919faf44300e2999b3548d724297#ab-aozora"
-> /nix/store/p5wzii1dv99dngz3jlrrh6147i2j9cqa-ab-aozora-0.1.0
$ printf 'test\n' | .../ab-aozora --version
ab-aozora 0.5.0 aat-schema 2 facade 0.3.0 wire-schema 3 (git ac2be926738f919faf44300e2999b3548d724297)
program_sha256 = sha256:42a7095d5488827f579ce1e9fc73925047222842be6ea03c1d2bd4db7bbaa980
```

Command binding (mirrors how adapters emit AAT via `--mode aat`):
program basename `ab-aozora`, argv `["--mode","aat"]`, environment `{}`,
execution_sha256 `sha256:b97400ab08ca37a5613df9a90b796c28eb42a023adb0d4b4072ba51f18dad706`
(recomputed from the `{command, environment}` preimage; matches the run manifest).

## Materialization + run (hinoki)

Reused the committed Rust corpus materializer (`ab-check`'s
`ab-materialize-study-inventory`), `ab-index`, and the committed
`neutral_executor.py`. Both inventories reproduce the committed identities
**byte-identically**, proving the identical pinned corpus/vectors:

```
ab-index --corpus <aozorabunko-corpus 0e9ea3e> --output index.json        -> 17886 works
ab-materialize-study-inventory --index index.json --corpus <corpus> --output corpus-mat
  corpus inventory_sha256 = sha256:5573de6f174b66cd1e7e30e62ebb7003984c79657fc5f81394ee5116e0d2a07c
  (== committed run-manifests corpus inventory_sha256)   items = 17886
neutral_executor.py --materialize-vectors <upstream-aozora-notation-spec>/conformance/vectors --output vectors-mat
  vectors inventory_sha256 = sha256:cdfb40a49f5d52dd8274efb71aa6f512f9305f034e502b6d9c44c98c4779c787
  (== committed run-manifests vectors inventory_sha256)  items = 127
```

Runs (frozen 300 s per-work timeout, jobs 32):
```
neutral_executor.py --inventory vectors-mat/inventory.json --source-root vectors-mat/sources \
  --output run-vectors --timeout 300 --jobs 32 <ab-aozora> --mode aat
  -> outcomes {success: 127} / 127            manifest_sha256 sha256:db265b0e...f9d690d6
neutral_executor.py --inventory corpus-mat/inventory.json  --source-root corpus-mat/sources \
  --output run-corpus  --timeout 300 --jobs 32 <ab-aozora> --mode aat   (13.8 s wall)
  -> outcomes {success: 17886} / 17886        manifest_sha256 sha256:42d96af3...126457a95f
```

Zero failures, zero timeouts on both instruments.

## Appendix result folded into the report IR

The nine ab-aozora native appendix rows (Task 4 reserved them as `missing`) now:

- **robustness (native): `measured`** — 17886/17886 from the appendix corpus run,
  SAME preregistered 17,886 denominator (failures/timeouts retained) and 95%
  Wilson interval as the existing-parser robustness lane: rate 1.000000,
  CI [0.999785, 1.000000].
- **spans, diagnostics (native): `non_comparable`** (missingness `non-comparable`)
  — owned-contract axes ab-aozora emits natively (source spans in the AAT IR;
  structured diagnostics via `--mode diagnostics`) for which the existing parsers
  have no native analogue (they reach AAT only through their adapter lane, which
  the native-only baseline lacks). Never a competitor zero, never a blocker.
- **construct_coverage, fidelity, performance, maintenance, packaging, license
  (native): `missing`** (`unavailable`) — no committed instrument for any parser;
  they carry the same blocker text as the existing-parser rows.

No competitor was assigned zero; nothing imputed.

### Falsifiable sensitivity analysis (added)

Claim under test: ab-aozora native parse-completion sits at the corpus ceiling
and is *matched but not exceeded* by the strongest existing native lanes
(`aozora2`, `aozora-rs`, `aozora2html`, each also 17886/17886), so
parse-completion does not separate the baseline from the strongest parsers.
Adversarial failure/timeout reweighting (reclassify k worst works as failures,
k ∈ {1, 12, 50, 113} drawn from observed competitor corpus failure/timeout
counts) yields rates 0.999944 / 0.999329 / 0.997205 / 0.993682 with Wilson
intervals whose upper bound falls below 1 for any k≥1. Falsifier: the
exact-ceiling reading is fragile to a single adversarial reclassification, so the
result reads as "ceiling under the frozen 300 s timeout on the measurement host,"
never as fidelity/diagnostics/span/performance superiority. Only this measured,
comparative claim is asserted; unmeasured axes assert nothing.

## Verification (real gates)

Report crate (run locally cargo 1.96.2 and on hinoki; identical results):
```
cargo fmt -p ab-parser-study-report -- --check       -> exit 0 (clean)
cargo clippy -p ab-parser-study-report --all-targets  -> Finished, no warnings
cargo test -p ab-parser-study-report                 -> 16 passed (report_contract)
                                                        6 passed (report_generation)
```
`report_generation` includes the byte-identical regeneration/drift test
(`committed_reports_match_regeneration_from_raw_manifests`) — green with the new
appendix data — and a new
`ab_aozora_appendix_is_measured_noncomparable_or_missing_never_zero`.
Confirmed independently: the 99 existing-parser machine rows are byte-identical
before/after regeneration.

Appendix run verifier (hinoki, `verify_appendix.py` + committed
`neutral_executor.py --verify`): program_sha256, both inventory identities, both
run manifest+execution hashes, ordered per-item completeness, every
outcome/stdout/stderr hash, and outcome counts all match the committed appendix
manifest.
```
program_sha256 OK: sha256:42a7095d5488827f579ce1e9fc73925047222842be6ea03c1d2bd4db7bbaa980
inventory aozorabunko-source-snapshot: items=17886 sha OK
inventory official-notation-vectors: items=127 sha OK
run official-notation-vectors: manifest+exec sha OK, ordered-verify OK, outcomes {'success': 127} match committed
run aozorabunko-source-snapshot: manifest+exec sha OK, ordered-verify OK, outcomes {'success': 17886} match committed
APPENDIX VERIFIER: ALL CHECKS PASS
```

## Constraints honored

- Existing-parser results and metric/denominator definitions unchanged
  (99 rows byte-identical).
- Frozen contracts (preregistration `.md/.json`, result schema, run-manifests)
  not mutated; extended only via a new committed appendix manifest.
- Custom parser native-only; no adapter_normalized lane fabricated.
- `non_comparable` used precisely (≠ zero, ≠ blocker-missing).
- Reports regenerate byte-identically from committed manifests.
- Corpus-scale raw bytes never committed (appendix manifest holds counts +
  provenance hashes only).

## Concerns

- The appendix corpus/vector raw artifacts live only under `/home/bor/task5` on
  hinoki (not committed, by design). They are regenerable deterministically from
  the pinned corpus + baseline binary; the committed manifest hashes pin their
  identity. If those scratch artifacts are deleted, re-running the documented
  build+materialize+run reproduces the same hashes.
