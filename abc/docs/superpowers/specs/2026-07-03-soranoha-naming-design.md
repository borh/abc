# Spec: Soranoha Project Renaming and Namespace Migration

Date: 2026-07-03  
Status: Draft RFC  
Author: Antigravity  

This document details the transition plan to rename the repository and codebase from the generic/overloaded `abc` to **`Soranoha` (空の葉 — "Leaves of the Sky")**. This branding establishes the project as a distinct, third-party digital humanities curation pipeline and database ecosystem for the Aozora Bunko corpus.

---

## 1. Brand & Component Architecture

The **Soranoha** brand uses a hybrid naming model to balance standard engineering conventions with literary Japanese register:

* **Umbrella Brand:** **Soranoha** (`soranoha.org`)
* **LOD Namespace:** `https://w3id.org/soranoha/` (Prefix `snh:`)
* **Schematron Prefix:** `snh-` (e.g., `snh-ruby-base-non-empty`)

### Component Systems (Model B):

| Package / Module | Role | Suffix Meaning | Namespace / Crate |
| :--- | :--- | :--- | :--- |
| **`soranoha-core`** | Common types, schema definitions, logging, and config | Core (English standard) | `soranoha.core` |
| **`soranoha-yomi`** | Parser adapters, text loading, and CSV ingestion | 読み (Reading) | `soranoha.yomi` |
| **`soranoha-ori`** | Curation pipeline, TEI generation, and LOD expansion | 織り (Weaving) | `soranoha.ori` |
| **`soranoha-kura`** | Database integration, storage, and caching | 蔵 (Storehouse) | `soranoha.kura` |
| **`soranoha-za`** | Platform, API presentation, and web components | 座 (Platform/Constellation) | `soranoha.za` |

---

## 2. Directory and Namespace Mapping

### Source Code (`src/`)

All files currently located in `src/abc/` will migrate to nested directories under `src/soranoha/` reflecting their architectural component:

```text
src/
└── soranoha/
    ├── core/
    │   ├── annotation_schema.clj
    │   ├── config.clj
    │   ├── logging.clj
    │   ├── ndc.clj
    │   ├── rdf_prefixes.clj
    │   ├── schema.clj
    │   └── text.clj
    ├── yomi/
    │   ├── diff.clj
    │   ├── git.clj
    │   ├── load.clj
    │   └── tools/
    │       ├── aat_parser_ir_compat.clj
    │       ├── aozora_csv.clj
    │       ├── aozora_history_audit.clj
    │       ├── aozora_ingest.clj
    │       ├── materialize_import.clj
    │       ├── metadata_record.clj
    │       ├── person_drift.clj
    │       ├── person_drift_history.clj
    │       └── person_record.clj
    ├── ori/
    │   ├── aozora.clj
    │   ├── core.clj
    │   ├── owl.clj
    │   ├── rdf.clj
    │   ├── relaxng.clj
    │   ├── stats.clj
    │   ├── tei.clj
    │   └── tools/
    │       ├── linked_art.clj
    │       ├── manifest.clj
    │       ├── manifest_index.clj
    │       ├── manifest_to_rdf.clj
    │       ├── schematron.clj
    │       ├── shacl.clj
    │       ├── tei.clj
    │       ├── tei_header.clj
    │       └── validate_corpus.clj
    ├── kura/
    │   ├── db.clj
    │   └── xtdb.clj
    └── za/
        ├── web.cljc
        └── tools/
            └── iiif.clj
```

### Test Code (`test/`)

Test files under `test/abc/` will move to mirror the source namespace layout under `test/soranoha/`:
* `test/abc/core_test.clj` $\rightarrow$ `test/soranoha/ori/core_test.clj`
* `test/abc/aozora_test.clj` $\rightarrow$ `test/soranoha/ori/aozora_test.clj`
* `test/abc/db_test.clj` $\rightarrow$ `test/soranoha/kura/db_test.clj`
* `test/abc/tools/aozora_csv_test.clj` $\rightarrow$ `test/soranoha/yomi/tools/aozora_csv_test.clj`
* ... (remaining test files mapped accordingly)

---

## 3. Linked Open Data (LOD) & Vocabulary Updates

1. **Vocabulary Namespace Change:**
   * Replace all occurrences of `https://w3id.org/abc/` with `https://w3id.org/soranoha/`.
2. **RDF Prefix Shift:**
   * Replace `@prefix abc: <https://w3id.org/abc/>` with `@prefix snh: <https://w3id.org/soranoha/>`.
   * Update SHACL definitions (`schemas/manifest.shacl.ttl`) and serialization code to output properties like `snh:artifactId`, `snh:contentHash`, and `snh:validationStatus`.
3. **JSON-LD Context:**
   * Rename `contexts/abc-v0.jsonld` to `contexts/soranoha-v0.jsonld`.
   * Update the internal mappings to bind the `snh` prefix to the new URI.

---

## 4. Schematron Constraint Renaming

All custom XML constraints in the TEI ODD profile (`schemas/tei-profile.odd`) will be renamed to use the `snh-` prefix:
* `abc-ruby-base-non-empty` $\rightarrow$ `snh-ruby-base-non-empty`
* `abc-figure-accessibility` $\rightarrow$ `snh-figure-accessibility`
* `abc-transcription-vs-annotation` $\rightarrow$ `snh-transcription-vs-annotation`

The Schematron validator (`soranoha.ori.schematron`) will enforce these updated rule patterns.

---

## 5. Nix Integration & Configuration Updates

1. **`flake.nix` updates:**
   * Update metadata descriptions to read "Soranoha development environment".
   * Rename Nix-exposed app targets:
     * `validate-design-bundle`
     * `materialize-import`
     * `manifest-to-rdf`
     * `aozora-ingest`
     * `aozora-upstream-audit`
   * Update the shell script wrappers in `flake.nix` to call the new Clojure entry points: `soranoha.ori.core` with aliases like `:soranoha/validate-design-bundle`.
2. **`deps.edn` aliases:**
   * Replace `:abc/*` main-opts aliases with `:soranoha/*` or `:snh/*` coordinates (e.g., `:snh/validate-design-bundle`).

---

## 6. Migration Steps & Verification Checklist

1. **Step 1: Code Base Relocation:** Move files to their new `soranoha` directories and sub-folders.
2. **Step 2: Namespace Declaration Updates:** Refactor `(ns abc.x ...)` declarations to `(ns soranoha.component.x ...)` across all source and test files.
3. **Step 3: Update Requirements/Imports:** Update `(:require [abc.x :as x])` references to `(:require [soranoha.component.x :as x])` globally.
4. **Step 5: Vocabulary and XML Rule Renaming:** Update SHACL schemas, JSON-LD contexts, ODD Schematron rules, and test fixtures.
5. **Step 6: Build Configuration Updates:** Refactor `deps.edn`, `tests.edn`, and `flake.nix` aliases.
6. **Step 7: Verification:**
   * Run local compilation checks.
   * Run tests via `clojure -Atest:runner` (or updated alias).
   * Run the Nix design bundle validation: `nix run .#validate-design-bundle` to verify schema and fixture parity.
