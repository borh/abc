# ABC Schema Contracts

ABC owns the JSON Schemas in this directory. The schemas listed in
`schema-contracts.json` are cross-repo protocol contracts consumed by
ab-validator.

## Version Discipline

Each cross-repo schema must carry:

- `$id`: stable schema identity.
- `title`: human-readable contract name.
- `version`: semver string for the schema contract.

Any behaviorally relevant schema edit must update the schema `version` and
regenerate `schema-contracts.json`:

```sh
python3 tools/schema_contracts.py --write
```

The manifest records the ABC schema hash for each contract using
`abc-legacy-json-c14n-v0`, the same canonicalization used by ABC manifest and
AAT parser-IR compatibility checks. The drift check fails if a schema's
version/hash pair or identity metadata differs from the committed manifest.

Historical registry entries remain valid under their historical schema hashes.
New producer evidence must use the current schema hash and matching contract
version.
