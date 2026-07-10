# Rust Quality, Performance, and Hermetic Tests Design

## Goal

Turn the `ab-validator/{crates,adapters}` audit into a staged set of behavior-preserving improvements, beginning with reproducible test and benchmark evidence and then addressing the highest-impact corpus-scale costs.

## Constraints

- Keep the root flake and root `justfile` as the primary entry points.
- Preserve the boundary that ABC owns publication schemas and `ab-validator` owns parser/adaptor measurement and corpus reports.
- Do not introduce literal `/nix/store/...` paths, `result`-symlink dependencies, or machine-local output paths.
- Refer to the pinned Aozora parser through the Nix value `${upstreamParserAozora}/bin/aozora`; Nix must realize and retain the dependency.
- Do not change AAT, parser-IR, warehouse, hash, or report formats during performance or structural refactors.
- Establish a failing regression test or benchmark assertion before each semantic implementation change.
- Keep generated and corpus-scale artifacts out of source.

## Confirmed Test Failure

Bare `cargo test` in `adapters/aozora` fails because the adapter falls back to an `aozora` executable on `PATH`. With `AB_AOZORA_BIN` set to the pinned `upstreamParserAozora` package, all 15 integration tests pass. The Nix `aozora-smoke` check already exports the correct package-derived value, but it runs only three shell cases. The adapter package sets `doCheck = false`, so no current Nix check runs the Rust integration suite.

The fix is a dedicated hermetic integration check built from the full `ab-validator` source. It exports `AB_AOZORA_BIN = "${upstreamParserAozora}/bin/aozora"`, configures Cargo to use `aozoraCargoDeps`, and runs the integration target offline and locked. The default dev shell exports the same package-derived value, and `just aozora-test` enters that shell rather than assuming a host executable.

## Delivery Order

1. Make `aozora` integration tests hermetic and visible as an explicit flake check.
2. Add strict formatter and Clippy checks for all excluded adapter workspaces.
3. Add benchmarks and counters that expose detector construction and coverage traversal behavior.
4. Reuse orthographic detectors across warehouse batches.
5. Replace shared prevalence locks with thread-local reduction and consolidate AAT traversal.
6. Reduce `ab-compare` peak memory by summarizing parsed roots immediately.
7. Apply measured allocation and regex improvements.
8. Add decoder parity protection without forcing every self-contained adapter into the root workspace.
9. Preserve the newly merged DuckDB/Parquet compatibility matrix and defer broad reporting-module moves until that protocol stabilizes.

## Nix Shape

Checks consume derivations directly:

```nix
extraEnv = {
  AB_AOZORA_BIN = "${upstreamParserAozora}/bin/aozora";
};
```

Cargo dependencies remain offline through `rustPlatform.importCargoLock` outputs and a generated `$CARGO_HOME/config.toml`. No test script discovers package paths dynamically.

## Performance Shape

Orthographic detectors become prepared run resources. Serial execution may construct one detector for a standalone run; warehouse-parallel execution constructs one shared `Arc<dyn OrthoDetector>` before workers start and clones the `Arc` into batches. Detector identity and normalization provenance remain unchanged.

Coverage measurement gains a compiled detector plan. Node-local AAT predicates share one traversal, while the small number of genuinely structural predicates remain explicit whole-tree rules. Rayon workers accumulate private counters and merge bounded top-five samples at reduction time.

## DuckDB Compatibility Boundary

Newly merged work made the reporting boundary materially richer than the original audit observed. `nway_feature_diffs` readers now support pre-v3 scalar `analyzer_id` and v3 collapsed `analyzers` shapes, detect shape from Parquet footers, adapt both DuckDB and in-memory engines, and stream materialization when DuckDB is unavailable. Some public SQL-preview wrappers deliberately fall back to the v3 shape when files are absent so DuckDB retains ownership of the missing-file error, while execution paths require successful shape detection.

This revision was reconciled against commits `03229ebb`, `033221a0`, `a95a3218`, and `e3901854`.

The broad `summary_body.rs` split is therefore deferred. This plan treats shape detection, SQL expansion, in-memory reading, engine equivalence, and fallback behavior as one compatibility protocol. A future module-boundary proposal must begin from that protocol and its characterization matrix rather than moving functions by implementation category.

## Acceptance

- The dedicated Aozora integration Nix check runs and passes all 15 tests with the pinned parser.
- `just aozora-test` enters the pinned dev shell and requires no host-installed `aozora`.
- All five adapter workspaces pass format and strict Clippy checks.
- A warehouse benchmark crossing the 32-document boundary proves one detector construction per run.
- Coverage results are byte-for-byte equal before and after traversal/reduction changes.
- AAT comparison results are byte-for-byte equal while full parsed roots are no longer retained corpus-wide.
- DuckDB and in-memory summaries remain equal for scalar and collapsed feature-diff shapes, and the streaming materialization fallback remains covered.
- Root workspace tests, adapter tests, focused Nix checks, and `just validate-migration` pass.
