# ADR 0003: Nix Materialization Policy

Status: Draft
Date: 2026-04-26
Supersedes: none
Source: `docs/high-level-architecture-note.md` v0.5

## Context

Nix is attractive for deterministic builds, pinned toolchains, and cacheable
recipes. A naive derivation matrix over all works, parsers, TEI profiles,
tokenizers, and analysis recipes would be expensive to evaluate and may copy
large corpus snapshots into the Nix store unnecessarily.

## Decision

Nix is a recipe/materialization backend, not the source of corpus-delta logic.
The default v0 bias is:

- Use an external manifest/index to identify the requested work set.
- Ask Nix to build bounded subsets, not the full Cartesian product.
- Avoid eager `works x parsers x profiles x tokenizers` attrsets.
- Use local external corpus paths for development when copying the corpus into
  the store would dominate iteration cost.
- Use fixed-output or content-addressed snapshots for publication builds.
- Pin corpus snapshots, parser source, tokenizer packages, dictionary archives,
  schema/profile sources, and analysis code in `flake.lock` or an equivalent
  release lock where exact rebuilds matter.

## Granularity

Default unit: one work artifact or a small explicit batch. Batching is allowed
when evaluation overhead is higher than rebuild waste. The chosen batch rule
must be recorded in manifest or run metadata so failures remain attributable
to individual works.

## Cache Tiers

- Hot: local development outputs.
- Warm: shared cache for common public artifacts.
- Cold: recipes and manifests, outputs rebuilt on demand.
- Archived: public release manifests, schemas, source locks, and archive IDs.

## Acceptance Criteria

- v0 example bundle can be built without evaluating the full corpus.
- Nix evaluation for the smoke corpus completes in under 30 seconds and uses
  under 2 GB peak memory on baseline CI hardware, or the ADR must be revised
  before implementation proceeds.
- The corpus input policy names development and release modes separately.
- Publication inputs record hashes and archive IDs where available.
- A first cost-envelope report records corpus input size, manifest index size,
  parser IR size per work, TEI size per work, tokenized output size per work,
  parse time, render time, validation time, Nix evaluation time, and peak
  memory for the example and smoke corpus.

## Rollback

If Nix evaluation or closure size blocks v0, keep the manifest/schema design
and use a simple script runner while retaining Nix as a future materializer.
