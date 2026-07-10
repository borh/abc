# Gate A: full-corpus AAT parity (fork shim vs run-set-pinned reference)

**Date:** 2026-07-10
**Authority:** ADR 0031 (hard detach), `.superpowers/sdd/task-9-brief.md`,
`docs/handoffs/2026-07-10-parser-fork-provenance.md`.
**Verdict:** `FORK_PARITY_CONFIRMED`

## Gate definition

Semantic JSON equality of every per-work AAT document between the fork
dump and the reference dump, with a **single allowlisted pointer**:
`/meta/adapter_version`, which embeds the producing binary's identity
(upstream store path vs fork shim) and is the only sanctioned
difference. Comparison is over parsed JSON documents (not bytes), via
`reports/aat-fidelity/compare-aat-dumps.py` (unit-tested: allowlisted
pointer normalized; any other difference and any missing/extra file
diverge).

## Reference dump (resolved fail-closed via the run-set)

Resolved on hinoki through `reports/aat-fidelity/run-sets/current.json`
(`run_set_id: current-aat-fidelity-2026-07-09`) with
`AB_DB_ROOT=/db/ab-validator`, hashing the on-disk tree with the
repository's own recipe (`reports/lib/aat_hash.py::hash_aat_dir` — the
same function `resolve-run-set.py` gates locks with; the full
`resolve-run-set.py` was not usable here because the other four
adapters' pinned dumps are not materialized on hinoki, and it fails
closed on all adapters at once):

```json
{
  "run_set_id": "current-aat-fidelity-2026-07-09",
  "aat_dir": "/db/ab-validator/aat-corpus/aozora-full-repin-1a4f864/aat/aozora-adapter",
  "files": 17886,
  "content_hash": "sha256:9860a7b64bce8b0af2f09f587df239a95ae7ffe9afd97961447306b022cecfa0",
  "expected": "sha256:9860a7b64bce8b0af2f09f587df239a95ae7ffe9afd97961447306b022cecfa0",
  "match": true
}
```

## Fork dump (generated under explicit `--aozora-bin` override)

Command (hinoki, 32 jobs, inside `nix develop`, sccache disabled):

```bash
export RUSTC_WRAPPER= SCCACHE_DISABLE=1 AB_DB_ROOT=/db/ab-validator
cargo build -p ab-aozora-cli --release
reports/aat-fidelity/run-aozora-aat-full.sh \
  --aozora-bin "$PWD/target/release/ab-aozora-cli" \
  --out-dir "$AB_DB_ROOT/aat-corpus/aozora-fork-parity-2263b92a" \
  --jobs 32 --force
```

- Git rev: `2263b92a` (branch `feat/parser-fork-phase1`)
- Out dir: `/db/ab-validator/aat-corpus/aozora-fork-parity-2263b92a`
  (scratch dump; may be deleted — this report records its identity)
- Workflow: `status: passed`, 4/4 steps, started 2026-07-10T07:05:34Z,
  ended 2026-07-10T07:08:54Z (3m20s; ~4m50s including the warm cargo
  build check and one-off nix builds of `ab-index`/schema derivations)
- AAT files: **17,886**; check-reports: 17,886; `fatal_error`
  occurrences in check-reports: **0**
- `metadata.json` override identity (Task 6 contract), recorded verbatim:

```json
"aozora_bin_override": {
  "path": "/home/bor/Projects/soranoha/.worktrees/parser-fork-phase1/ab-validator/target/release/ab-aozora-cli",
  "sha256": "c071042c00464253eb066c6f812abb1610614066d8b0bd508100fa444e0973a6",
  "version": "ab-aozora-cli 0.1.0 (fork of P4suta/aozora @ 1a4f864, ADR 0031)"
}
```

- Fork dump content hash (same recipe as the reference verification,
  `hash_aat_dir`):

```json
{
  "aat_dir": "/db/ab-validator/aat-corpus/aozora-fork-parity-2263b92a/aat/aozora-adapter",
  "file_count": 17886,
  "content_hash": "sha256:51d77b6958c909ae18c23197ea43a1291efe7411063288af0b1f47eb5e3ffecb"
}
```

Note the fork dump's content hash is expected to differ from the
reference's — every file's `/meta/adapter_version` embeds the producing
binary identity, so byte-level tree hashes differ by design. The gate
is the semantic comparison below.

## Comparator output

```bash
python3 reports/aat-fidelity/compare-aat-dumps.py \
  /db/ab-validator/aat-corpus/aozora-full-repin-1a4f864/aat/aozora-adapter \
  /db/ab-validator/aat-corpus/aozora-fork-parity-2263b92a/aat/aozora-adapter
```

```json
{
  "compared": 17886,
  "missing_count": 0,
  "missing_sample": [],
  "diverged_count": 0,
  "diverged_sample": []
}
```

Exit code 0.

## Verdict

**`FORK_PARITY_CONFIRMED`** — all 17,886 works compared; 0 missing,
0 diverged under the single-pointer allowlist. The lifted fork
(`ab-aozora-*` crates behind the `ab-aozora-cli` shim) reproduces the
pinned upstream binary's AAT output over the full corpus.
