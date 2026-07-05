# Aozora Parser Adapter and Notation Comparator Handoff

## Implemented

- Added parser id `aozora` as a fifth adapter lane.
- Added pinned references for `P4suta/aozora` and `P4suta/aozora-notation-spec`.
- Added `aozora-adapter` with schema-valid AAT smoke coverage.
- Added notation-spec comparator reports kept separate from local AAT oracle.
- Added flake checks for the adapter smoke and notation-spec comparator smoke.

## Verification

- `cargo test --manifest-path adapters/aozora/Cargo.toml`
- `cargo test -p ab-coverage --jobs 24`
- `just aozora-smoke`
- `just aozora-notation-spec-comparator-smoke`
- `nix --option post-build-hook "" build .#checks.$system.aozora-smoke --print-build-logs`
- `nix --option post-build-hook "" build .#checks.$system.aozora-notation-spec-comparator-smoke --print-build-logs`

Plain `just aozora-flake-smoke` and `just aozora-notation-spec-comparator-flake-smoke` are wired, but local verification used `--option post-build-hook ""` because this host's post-build hook can stall on a remote cache copy or GC lock. The flake derivations themselves do not write to `/db`.

## First Comparator Evidence

`docs/superpowers/reports/2026-07-05-aozora-notation-spec-comparison.summary.json` measured 127 external notation-spec vectors across six lanes:

- upstream `aozora` inspect lane: 124 warnings, 3 failures.
- local AAT lanes `ab-aozora`, `aozora2`, `aozora2html`, `aozora-rs`, and `aozora-epub3`: 127 structural-warning rows each because AAT does not expose the external `aozora inspect` projection surface.

The three upstream `aozora` failures are diagnostic-shape mismatches for `pua_collision`, `tate_chu_yoko`, and `unclosed_bracket`; they are comparison evidence, not local policy.

## Next Operator Measurements

1. Run full `aozora` AAT corpus:
   `just aozora-aat-full "" 24 300s`
2. Add the resulting AAT dir to `AB_AOZORA_AAT_DIR`.
3. Run `just aat-to-parser-ir-full-audit JOBS=24`.
4. Run `just tei-eaj-structural-expansion JOBS=24`.
5. Run parser performance measurement with all five parser lanes.

## Trust Boundary

`aozora-notation-spec` is useful comparison evidence, not the authority for local source representability, parser-IR vocabulary, or ABC TEI admission.
