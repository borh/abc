# Parser-RQ Evidence Containment

Parser-RQ must not depend on an untracked place or service to reconstruct its
release decision.

- Git contains the candidate description, policies, manifests, receipts,
  observations, reports, and small witnesses.
- Runtime configuration supplies the corpus, evidence-store, scratch, and lock
  paths without making those places part of qualification identity.
- Every required corpus-scale blob is named by logical SHA-256, byte count,
  media type, and a locator below the configured evidence store.
- Promotion re-hashes every manifest-referenced blob and rejects an absent,
  escaping, truncated, or mismatched value.
- Deterministic projections regenerate from the committed values and the closed
  captured generation. Volatile resource observations remain authenticated
  captured facts rather than falsely reproducible executions.

No parser-RQ result may require an undeclared path, an operator's workstation,
or a third-party storage service. Storage durability remains an ordinary
property of the configured, controlled evidence store; parser-RQ neither models
nor attests it.
