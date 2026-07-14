# Existing-parser raw runs

`run-manifests.json` freezes the identities and exact outcome counts of the
preregistered native and adapter-normalized executions. Corpus-scale stdout,
stderr, and per-item outcome files remain in external evidence storage; the
logical `artifact_root` values deliberately contain no host-local path.

Each recorded manifest was accepted only after `neutral_executor.py --verify`
rechecked inventory completeness and every outcome, stdout, and stderr hash.
The manifest SHA-256 identifies the external manifest that orders those
per-item outcome hashes.

Verification requires an untracked resolver JSON with `inventories`,
`artifact_roots`, and `executions` maps. Artifact keys are the logical roots in
the checked summary; execution keys map each execution SHA-256 to its exact
`{"command": [...], "environment": {...}}` preimage. The verifier recomputes
that hash before inspecting raw artifacts, so local paths stay in explicit
configuration rather than checked evidence.

The vector and corpus adapter execution hashes differ for `aozora2` and
`aozora-rs` because separate Nix builds produced different store paths in the
exact command vectors. Parser and adapter source revisions are identical; the
execution identity intentionally hashes the concrete program path. The
original `aozora-rs` native command preimage was not recoverable, so only that
fast lane was rerun into preserved replacement roots with immediate host
capture. All other host captures remain explicitly unavailable under the
frozen missing-data rule.
