# Build Publication Command Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add a smoke-scoped `soranoha build-publication` command that turns an official-Aozora-shaped checkout fixture into a materialized root, then delegates to `publication-rehearsal!`.

**Architecture:** Keep `abc.tools.soranoha` as the CLI dispatcher. Put config parsing, Aozora root validation, official catalog ZIP reading, source selection, smoke materialization, atomic output handling, source-selection reporting, and rehearsal delegation in `abc.tools.soranoha-build-publication`. The first implementation writes the minimal materialized work-root shape directly from catalog rows and work ZIP identity, then exercises the canonical Soranoha publication path without pretending the full Rust adapter protocol is done.

**Tech Stack:** Clojure, existing `abc.tools.manifest` deterministic JSON writer, existing `abc.tools.soranoha/publication-rehearsal!`, existing Nix `abc-clj-nix-focused-tests`.

## Global Constraints

- Do not reimplement source-snapshot, request-set resolution, reproduction, reports, staging, or validation already owned by `publication-rehearsal!`.
- Non-smoke scopes require explicit `--snapshot-date`.
- Dirty Aozora check is limited to `index_pages` and `cards`.
- The first implementation is smoke/demo-scoped; full-corpus adapter materialization remains gated on measured concurrency and adapter protocol work.
- Every known skipped source must appear in `source-selection-report.json`.

---

### Task 1: Build-Publication Config, Selection, and Materialization

**Files:**
- Create: `abc/src/abc/tools/soranoha_build_publication.clj`
- Create: `abc/schemas/soranoha-publication-build-config.schema.json`
- Create: `abc/config/publication-basic-ja.json`
- Modify: `abc/test/abc/tools/soranoha_test.clj`
- Modify: `abc/src/abc/tools/soranoha.clj`

**Interfaces:**
- Produces: `abc.tools.soranoha-build-publication/build-publication! [args] -> exit-code`.
- Consumes: `abc.tools.soranoha/publication-rehearsal! [materialized-root output-root request-set-label snapshot-scope snapshot-date]`.

- [x] **Step 1: Write failing CLI tests**

Add tests to `abc/test/abc/tools/soranoha_test.clj` that create an official-Aozora-shaped fixture root with:

```text
index_pages/list_person_all_extended_utf8.zip
cards/000879/files/000001_ruby_fixture.zip
cards/000879/files/000001_images.zip
support/tools.zip
```

The first test runs:

```clojure
(soranoha/run! ["build-publication"
                "--aozora-root" (str aozora-root)
                "--config" "abc/config/publication-basic-ja.json"
                "--snapshot-date" "2026-07-08"
                "--output-root" (str output-root)])
```

It asserts:

- exit code is zero;
- `materialized-root/works/000001_000879_000001_ruby_fixture/official-source.json` exists;
- `source-selection-report.json` records one selected source and at least one rejected source;
- `rehearsal/rehearsal-report.json` exists;
- `rehearsal/snapshot-root/artifacts/works/000001_000879_000001_ruby_fixture/tei/tei.xml` exists;
- `soranoha validate <output-root>/rehearsal/publication` exits zero.

The second test runs without `--snapshot-date` against the non-smoke config and asserts exit code `1` with an error containing `snapshot-date is required`.

- [x] **Step 2: Run tests to verify red**

Run:

```bash
nix build .#checks.x86_64-linux.abc-clj-nix-focused-tests --print-build-logs
```

Expected: failure because `build-publication` is not a known command.

- [x] **Step 3: Add schema and config**

Create `abc/schemas/soranoha-publication-build-config.schema.json` with required keys:

```json
{
  "$schema": "https://json-schema.org/draft/2020-12/schema",
  "$id": "https://w3id.org/abc/schemas/soranoha-publication-build-config.schema.json",
  "type": "object",
  "additionalProperties": false,
  "required": [
    "config_schema_id",
    "request_set_label",
    "snapshot_scope",
    "parser_profile",
    "publication_profile",
    "continue_on_failure",
    "materialization_scope"
  ],
  "properties": {
    "config_schema_id": {"const": "https://w3id.org/abc/schemas/soranoha-publication-build-config.schema.json"},
    "request_set_label": {"type": "string", "minLength": 1},
    "snapshot_scope": {"type": "string", "minLength": 1},
    "parser_profile": {"type": "string", "minLength": 1},
    "publication_profile": {"type": "string", "minLength": 1},
    "continue_on_failure": {"type": "boolean"},
    "materialization_scope": {"enum": ["smoke", "demo", "full-corpus"]}
  }
}
```

Create `abc/config/publication-basic-ja.json` using the design's default labels.

- [x] **Step 4: Implement build-publication namespace**

Implement `abc.tools.soranoha-build-publication` with:

- CLI option parser for `--aozora-root`, `--config`, `--snapshot-date`, `--output-root`, `--replace`.
- config schema validation.
- relevant dirty check using `git status --porcelain -- index_pages cards`.
- source selection over `cards/[0-9]{6}/files/*.zip`.
- selected text ZIPs are catalog rows whose `テキストファイルURL` basename matches an official work ZIP.
- materialization writes the minimal work root from source ZIP metadata, source ZIP SHA-256, and `aozora-ingest/run-from-rows!`.
- deterministic `source-selection-report.json`.
- `build-plan.json` with config hash, Aozora commit, dirty status, selected count, rejected count, and rehearsal root.
- atomic in-progress directory, then promote to `--output-root`.
- delegation to `soranoha/publication-rehearsal!`.

- [x] **Step 5: Wire command dispatch**

Modify `abc.tools.soranoha`:

- require `abc.tools.soranoha-build-publication`;
- add usage line;
- add command `"build-publication" {:args :variadic :run ...}`;
- update arity handling so `:variadic` commands accept arbitrary remaining args.

- [x] **Step 6: Run focused tests**

Run:

```bash
nix build .#checks.x86_64-linux.abc-clj-nix-focused-tests --print-build-logs
```

Expected: pass.

- [ ] **Step 7: Commit**

```bash
git add abc/src/abc/tools/soranoha.clj \
        abc/src/abc/tools/soranoha_build_publication.clj \
        abc/test/abc/tools/soranoha_test.clj \
        abc/schemas/soranoha-publication-build-config.schema.json \
        abc/config/publication-basic-ja.json
git commit -m "feat(abc): add build-publication command"
```

### Task 2: Full Verification and Integration

**Files:**
- Modify: no new files expected.

**Interfaces:**
- Consumes: completed Task 1 command and tests.
- Produces: merged, pushed main branch after full verification.

- [ ] **Step 1: Run full flake check**

Run:

```bash
nix flake check --print-build-logs
```

Expected: pass.

- [ ] **Step 2: Merge and push**

Fast-forward `main`, run `nix flake check --print-build-logs` again on `main`, push, remove worktree, delete branch.
