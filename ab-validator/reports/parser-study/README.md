# Parser study — evidence tooling

This directory holds the neutral parser-comparison study's run evidence and the
Python tooling that freezes and verifies it. The committed manifests
(`runs/*/run-manifests.json`, `runs/*/appendix-run-manifests.json`) record only
identities and exact outcome **counts**; the corpus-scale stdout/stderr/per-item
outcome bytes stay in external evidence storage, and the logical `artifact_root`
values deliberately contain no host-local path. Nothing here re-derives per-work
bytes into the repository.

## `verify_external` — the count↔real-run authenticity gate

`freeze_run_evidence.py::verify_external` is the out-of-repo gate that binds the
committed counts to the actual runs. For each lane it:

- recomputes `execution_sha256` from the resolved `{"command", "environment"}`
  preimage (local paths stay in configuration, never in checked evidence);
- binds the recorded `program_sha256` to the **real on-disk binary** through the
  materialization projection (candidate/mode, frozen parser/adapter revisions,
  executable byte hash, derivation identity → `materialization_binding_sha256`);
- checks the external run `manifest.json` against the recorded `manifest_sha256`;
- re-hashes every per-item outcome and **re-derives each recorded outcome count**
  from the real artifacts, rejecting any drift.

It is not a substitute for the lane-matrix shape check: `verify_external` first
runs `validate_summary` (the exact 20-lane matrix), then the binding core,
`verify_external_bindings`. The binding core is separated so it can be exercised
on a synthetic single run — see `tests/test_freeze_run_evidence.py`
(`test_verify_external_accepts_an_authentic_run`, plus tampered-count and
tampered-program-hash rejections).

## Running it

The gate needs an **untracked resolver JSON** that maps the summary's logical
keys to local evidence and command/environment preimages. Its shape (`inventories`,
`artifact_roots`, `executions`, `materializations`, `inventory_identities`,
`host_capture`) is documented in
`runs/aozora-parser-neutral-comparison-2026-07/README.md`.

Verify committed counts against external artifact roots (on hinoki, where the
external evidence and Nix-built binaries live):

```sh
cd ab-validator/reports/parser-study
python freeze_run_evidence.py \
  --verify-external \
  --summary runs/aozora-parser-neutral-comparison-2026-07/run-manifests.json \
  --resolver /path/to/untracked-resolver.json \
  --executor neutral_executor.py
```

The command exits non-zero on any mismatch (execution hash, command binding,
materialization/program-hash binding, manifest hash, or an outcome count that
does not re-derive from the real run). `--verify-external` runs only the gate; it
does not need `--preregistration` and does not rewrite the committed summary.

> Note: these Python tests are not yet wired into a Nix check (the
> `reports-pytest-check` in `ab-validator/flake.nix` covers `aat-fidelity`,
> `lib`, `parser-conformance`, and `source-regions`, not `parser-study`). Run
> them with `pytest reports/parser-study/tests` in a Python env that provides
> `pytest`.
