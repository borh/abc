# Existing-parser raw runs

`run-manifests.json` freezes the identities and exact outcome counts of the
preregistered native and adapter-normalized executions. Corpus-scale stdout,
stderr, and per-item outcome files remain in external evidence storage; the
logical `artifact_root` values deliberately contain no host-local path.

Each recorded manifest was accepted only after `neutral_executor.py --verify`
rechecked inventory completeness and every outcome, stdout, and stderr hash.
The manifest SHA-256 identifies the external manifest that orders those
per-item outcome hashes.
